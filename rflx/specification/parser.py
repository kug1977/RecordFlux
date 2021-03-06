import logging
from collections import OrderedDict
from multiprocessing import Pool
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Set, Tuple

from pyparsing import ParseException, ParseFatalException

from rflx import common, expression as expr
from rflx.error import (
    RecordFluxError,
    Severity,
    Subsystem,
    fail,
    parser_location,
    pop_source,
    push_source,
)
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    INTERNAL_TYPES,
    Array,
    Field,
    Link,
    Message,
    Model,
    Refinement,
    Scalar,
    Session,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
    is_builtin_type,
    qualified_type_identifier,
)

from . import grammar
from .ast import (
    ArraySpec,
    Component,
    DerivationSpec,
    MessageSpec,
    PackageSpec,
    RefinementSpec,
    Specification,
)
from .cache import Cache

log = logging.getLogger(__name__)


class Parser:
    def __init__(self, skip_verification: bool = False, cached: bool = False) -> None:
        self.skip_verification = skip_verification
        self.__specifications: OrderedDict[Path, Optional[Specification]] = OrderedDict()
        self.__evaluated_specifications: Set[ID] = set()
        self.__types: List[Type] = [*BUILTIN_TYPES.values(), *INTERNAL_TYPES.values()]
        self.__sessions: List[Session] = []
        self.__cache = Cache(cached)

    def parse(self, *specfiles: Path) -> None:
        error = RecordFluxError()

        for f in specfiles:
            error.extend(self.__determine_dependencies(f))

        with Pool() as p:
            for f, s, e in p.map(
                self._parse, [f for f, s in self.__specifications.items() if s is None]
            ):
                self.__specifications[f] = s
                error.extend(e)

            p.close()
            p.join()

        error.propagate()

    def __determine_dependencies(
        self, specfile: Path, transitions: List[Tuple[str, ID]] = None
    ) -> RecordFluxError:
        if specfile in self.__specifications:
            self.__specifications.move_to_end(specfile)
        else:
            self.__specifications[specfile] = None

        if not transitions:
            transitions = []

        error = RecordFluxError()

        with open(specfile, "r") as filehandle:
            push_source(specfile)
            try:
                for context in grammar.context_clause().parseFile(filehandle):
                    for item in context.items:
                        transition = (specfile.name, item)
                        if transition in transitions:
                            error.append(
                                f'dependency cycle when including "{transitions[0][1]}"',
                                Subsystem.PARSER,
                                Severity.ERROR,
                                transitions[0][1].location,
                            )
                            error.extend(
                                [
                                    (
                                        f'when including "{i}"',
                                        Subsystem.PARSER,
                                        Severity.INFO,
                                        i.location,
                                    )
                                    for _, i in transitions[1:]
                                ]
                            )
                            continue
                        transitions.append(transition)
                        error.extend(
                            self.__determine_dependencies(
                                specfile.parent / f"{str(item).lower()}.rflx", transitions
                            )
                        )
            except (ParseException, ParseFatalException) as e:
                error.append(
                    e.msg,
                    Subsystem.PARSER,
                    Severity.ERROR,
                    parser_location(e.loc, e.loc, e.pstr, specfile),
                )
                error.propagate()
            finally:
                pop_source()

        return error

    @staticmethod
    def _parse(specfile: Path) -> Tuple[Path, Optional[Specification], RecordFluxError]:
        error = RecordFluxError()
        log.info("Parsing %s", specfile)

        specification = None

        with open(specfile, "r") as filehandle:
            push_source(specfile)
            try:
                specifications = grammar.unit().parseFile(filehandle)
                if specifications:
                    assert len(specifications) == 1
                    specification = specifications[0]
                    check_naming(error, specification.package, specfile.name)
            except (ParseException, ParseFatalException) as e:
                error.append(
                    e.msg,
                    Subsystem.PARSER,
                    Severity.ERROR,
                    parser_location(e.loc, e.loc, e.pstr, specfile),
                )
            finally:
                pop_source()

        return (specfile, specification, error)

    def parse_string(self, string: str) -> None:
        error = RecordFluxError()
        try:
            for specification in grammar.unit().parseString(string):
                check_naming(error, specification.package)
                self.__specifications[
                    Path(common.file_name(str(specification.package.identifier)))
                ] = specification
        except (ParseException, ParseFatalException) as e:
            error.append(
                e.msg,
                Subsystem.PARSER,
                Severity.ERROR,
                parser_location(e.loc, e.loc, e.pstr),
            )
        error.propagate()

    def create_model(self) -> Model:
        error = RecordFluxError()
        for specification in reversed(self.__specifications.values()):
            if (
                specification
                and specification.package.identifier not in self.__evaluated_specifications
            ):
                self.__evaluated_specifications.add(specification.package.identifier)
                try:
                    self.__evaluate_specification(specification)
                except RecordFluxError as e:
                    error.extend(e)
        try:
            result = Model(self.__types, self.__sessions)
        except RecordFluxError as e:
            error.extend(e)

        error.propagate()
        return result

    @property
    def specifications(self) -> Dict[str, Specification]:
        return {str(s.package.identifier): s for s in self.__specifications.values() if s}

    def __evaluate_specification(self, specification: Specification) -> None:
        log.info("Processing %s", specification.package.identifier)

        error = RecordFluxError()
        self.__evaluate_types(specification, error)
        self.__evaluate_sessions(specification)
        error.propagate()

    def __evaluate_types(self, spec: Specification, error: RecordFluxError) -> None:
        for t in spec.package.types:
            t.identifier = ID(spec.package.identifier, t.identifier.location) * t.name

            new_type: Type

            try:
                if isinstance(t, Scalar):
                    new_type = t

                elif isinstance(t, ArraySpec):
                    new_type = create_array(t, self.__types)

                elif isinstance(t, MessageSpec):
                    new_type = create_message(t, self.__types, self.skip_verification, self.__cache)

                elif isinstance(t, DerivationSpec):
                    new_type = create_derived_message(
                        t, self.__types, self.skip_verification, self.__cache
                    )

                elif isinstance(t, RefinementSpec):
                    new_type = create_refinement(t, self.__types)

                else:
                    raise NotImplementedError(f'unsupported type "{type(t).__name__}"')

                self.__types.append(new_type)
                error.extend(new_type.error)

            except RecordFluxError as e:
                error.extend(e)

    def __evaluate_sessions(self, spec: Specification) -> None:
        for s in spec.package.sessions:
            self.__sessions.append(
                Session(
                    ID(spec.package.identifier, s.identifier.location) * s.identifier.name,
                    s.initial,
                    s.final,
                    s.states,
                    s.declarations,
                    s.parameters,
                    self.__types,
                    s.location,
                )
            )


def create_array(array: ArraySpec, types: Sequence[Type]) -> Array:
    array.element_type.identifier = ID(
        array.element_type.full_name.replace("__PACKAGE__", str(array.package)), array.location
    )

    try:
        element_type = [t for t in types if array.element_type.identifier == t.identifier][0]
    except IndexError:
        fail(
            f'undefined element type "{array.element_type.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            array.element_type.location,
        )

    return Array(array.identifier, element_type, array.location)


def create_message(
    message: MessageSpec,
    types: Sequence[Type],
    skip_verification: bool,
    cache: Cache,
) -> Message:

    components = list(message.components)

    if components and components[0].identifier:
        components.insert(0, Component())

    error = RecordFluxError()

    field_types = create_message_types(message, types, components)
    structure = create_message_structure(components, error)

    return create_proven_message(
        UnprovenMessage(
            message.identifier, structure, field_types, message.aspects, message.location, error
        ).merged(),
        skip_verification,
        cache,
    )


def create_message_types(
    message: MessageSpec,
    types: Sequence[Type],
    components: Sequence[Component],
) -> Dict[Field, Type]:

    field_types: Dict[Field, Type] = {}

    for component in components:
        if component.identifier and component.type_identifier:
            type_identifier = qualified_type_identifier(component.type_identifier, message.package)
            field_type = [t for t in types if t.identifier == type_identifier]
            if field_type:
                field_types[Field(component.identifier)] = field_type[0]

    return field_types


def create_message_structure(components: Sequence[Component], error: RecordFluxError) -> List[Link]:
    # pylint: disable=too-many-branches

    structure: List[Link] = []

    for i, component in enumerate(components):
        source_node = Field(component.identifier) if component.identifier else INITIAL

        if not component.thens:
            identifier = components[i + 1].identifier if i + 1 < len(components) else None
            target_node = Field(identifier) if identifier else FINAL
            structure.append(Link(source_node, target_node))

        if (
            component.first != expr.UNDEFINED
            or component.size != expr.UNDEFINED
            or component.condition != expr.TRUE
        ):
            for l in (l for l in structure if l.target.identifier == component.identifier):
                if component.first != expr.UNDEFINED:
                    if l.first == expr.UNDEFINED:
                        l.first = component.first
                    else:
                        error.append(
                            f'first aspect of field "{component.identifier}"'
                            " conflicts with previous"
                            " specification",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            component.first.location,
                        )
                        error.append(
                            "previous specification of first",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.first.location,
                        )

                if component.size != expr.UNDEFINED:
                    if l.size == expr.UNDEFINED:
                        l.size = component.size
                    else:
                        error.append(
                            f'size aspect of field "{component.identifier}" conflicts with previous'
                            " specification",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            component.size.location,
                        )
                        error.append(
                            "previous specification of size",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.size.location,
                        )

                if component.condition != expr.TRUE:
                    l.condition = (
                        expr.And(component.condition, l.condition, location=l.condition.location)
                        if l.condition != expr.TRUE
                        else component.condition
                    )

        for then in component.thens:
            if then.identifier and not any(then.identifier == c.identifier for c in components):
                error.append(
                    f'undefined field "{then.identifier}"',
                    Subsystem.PARSER,
                    Severity.ERROR,
                    then.identifier.location if then.identifier else None,
                )
                continue
            target_node = Field(then.identifier) if then.identifier else FINAL
            structure.append(
                Link(source_node, target_node, then.condition, then.size, then.first, then.location)
            )

    return structure


def create_derived_message(
    derivation: DerivationSpec,
    types: Sequence[Type],
    skip_verification: bool,
    cache: Cache,
) -> Message:
    base_name = qualified_type_identifier(derivation.base, derivation.package)
    error = RecordFluxError()

    base_types = [t for t in types if t.identifier == base_name]

    if not base_types:
        fail(
            f'undefined base message "{base_name}" in derived message',
            Subsystem.PARSER,
            Severity.ERROR,
            derivation.location,
        )

    base_messages = [t for t in base_types if isinstance(t, Message)]

    if not base_messages:
        error.append(
            f'illegal derivation "{derivation.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            derivation.location,
        )
        error.append(
            f'invalid base message type "{base_name}"',
            Subsystem.PARSER,
            Severity.INFO,
            base_types[0].location,
        )
        error.propagate()

    return create_proven_message(
        UnprovenDerivedMessage(
            derivation.identifier, base_messages[0], location=derivation.location
        ).merged(),
        skip_verification,
        cache,
    )


def create_proven_message(
    unproven_message: UnprovenMessage, skip_verification: bool, cache: Cache
) -> Message:
    proven_message = unproven_message.proven(
        skip_verification or cache.is_verified(unproven_message)
    )

    cache.add_verified(unproven_message)

    return proven_message


def create_refinement(refinement: RefinementSpec, types: Sequence[Type]) -> Refinement:
    messages = {t.identifier: t for t in types if isinstance(t, Message)}

    refinement.pdu = qualified_type_identifier(refinement.pdu, refinement.package)
    if refinement.pdu not in messages:
        fail(
            f'undefined type "{refinement.pdu}" in refinement',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.location,
        )

    refinement.sdu = qualified_type_identifier(refinement.sdu, refinement.package)
    if refinement.sdu not in messages:
        fail(
            f'undefined type "{refinement.sdu}" in refinement of "{refinement.pdu}"',
            Subsystem.PARSER,
            Severity.ERROR,
            refinement.sdu.location,
        )

    return Refinement(
        refinement.package,
        messages[refinement.pdu],
        Field(refinement.field),
        messages[refinement.sdu],
        refinement.condition,
        refinement.location,
    )


def check_naming(error: RecordFluxError, package: PackageSpec, filename: str = None) -> None:
    if str(package.identifier).startswith("RFLX"):
        error.append(
            f'illegal prefix "RFLX" in package identifier "{package.identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            package.identifier.location,
        )
    if package.identifier != package.end_identifier:
        error.append(
            f'inconsistent package identifier "{package.end_identifier}"',
            Subsystem.PARSER,
            Severity.ERROR,
            package.end_identifier.location,
        )
        error.append(
            f'previous identifier was "{package.identifier}"',
            Subsystem.PARSER,
            Severity.INFO,
            package.identifier.location,
        )
    if filename:
        expected_filename = f"{str(package.identifier).lower()}.rflx"
        if filename != expected_filename:
            error.append(
                f'file name does not match unit name "{package.identifier}",'
                f' should be "{expected_filename}"',
                Subsystem.PARSER,
                Severity.ERROR,
                package.identifier.location,
            )
    for t in package.types:
        if is_builtin_type(t.identifier.name):
            error.append(
                f'illegal redefinition of built-in type "{t.identifier.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                t.location,
            )
