# pylint: disable=too-many-lines
import itertools
from abc import abstractmethod
from collections import defaultdict
from copy import copy
from dataclasses import dataclass
from typing import Dict, List, Mapping, Optional, Sequence, Set, Tuple

import rflx.typing_ as rty
from rflx import expression as expr
from rflx.common import Base, flat_name, indent, indent_next, verbose_repr
from rflx.contract import ensure, invariant
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID, StrID

from . import const, type_ as mty


class Field(Base):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __repr__(self) -> str:
        return f'Field("{self.identifier}")'

    def __lt__(self, other: "Field") -> int:
        return self.identifier < other.identifier

    @property
    def name(self) -> str:
        return str(self.identifier)

    @property
    def affixed_name(self) -> str:
        return f"F_{self.name}"


INITIAL = Field("Initial")
FINAL = Field("Final")


@dataclass(order=True)
class Link(Base):
    source: Field
    target: Field
    condition: expr.Expr = expr.TRUE
    length: expr.Expr = expr.UNDEFINED
    first: expr.Expr = expr.UNDEFINED
    location: Optional[Location] = None

    def __str__(self) -> str:
        condition = indent_next(
            f"\nif {indent_next(str(self.condition), 3)}" if self.condition != expr.TRUE else "", 3
        )
        aspects = []
        if self.length != expr.UNDEFINED:
            aspects.append(f"Length => {self.length}")
        if self.first != expr.UNDEFINED:
            aspects.append(f"First => {self.first}")
        with_clause = indent_next("\nwith " + ", ".join(aspects) if aspects else "", 3)
        target_name = self.target.name if self.target != FINAL else "null"
        return f"then {target_name}{with_clause}{condition}"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.source == other.source
                and self.target == other.target
                and self.condition == other.condition
                and self.length == other.length
                and self.first == other.first
            )
        return NotImplemented

    def __hash__(self) -> int:
        return 0


def valid_message_field_types(message: "AbstractMessage") -> bool:
    for t in message.types.values():
        if not isinstance(t, (mty.Scalar, mty.Composite, AbstractMessage)):
            return False
    return True


class MessageState(Base):
    fields: Optional[Tuple[Field, ...]] = ()
    definite_predecessors: Optional[Mapping[Field, Tuple[Field, ...]]] = None
    field_condition: Optional[Mapping[Field, expr.Expr]] = None
    checksums: Mapping[ID, Sequence[expr.Expr]] = {}


@invariant(lambda self: valid_message_field_types(self))
@invariant(lambda self: not self.types if not self.structure else True)
class AbstractMessage(mty.Type):
    # pylint: disable=too-many-arguments,too-many-public-methods
    def __init__(
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, mty.Type],
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
        state: MessageState = None,
    ) -> None:
        assert not (state and aspects)

        super().__init__(identifier, location, error)

        self.structure = sorted(structure)
        self.__types = types
        self.__aspects = aspects or {}
        self.__has_unreachable = False
        self._state = state or MessageState()
        self.__paths_cache: Dict[Field, Set[Tuple[Link, ...]]] = {}

        if not state and (structure or types):
            try:
                self.__verify()
                self._state.fields = self.__compute_topological_sorting()
                if self._state.fields:
                    self.__types = {f: self.__types[f] for f in self._state.fields}
                if ID("Checksum") in self.__aspects:
                    self._state.checksums = self.__aspects[ID("Checksum")]
            except RecordFluxError:
                pass

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.identifier == other.identifier
                and self.structure == other.structure
                and self.types == other.types
                and self.aspects == other.aspects
            )
        return NotImplemented

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "structure", "types"])

    def __str__(self) -> str:
        if not self.structure or not self.types:
            return f"type {self.name} is null message"

        fields = ""
        field_list = [INITIAL, *self.fields]
        for i, f in enumerate(field_list):
            if f != INITIAL:
                fields += "\n" if fields else ""
                fields += f"{f.name} : {self.types[f].name}"
            outgoing = self.outgoing(f)
            if not (
                len(outgoing) == 1
                and outgoing[0].condition == expr.TRUE
                and outgoing[0].length == expr.UNDEFINED
                and outgoing[0].first == expr.UNDEFINED
                and (i >= len(field_list) - 1 or field_list[i + 1] == outgoing[0].target)
            ):
                if f == INITIAL:
                    fields += "null"
                fields += "\n" + indent("\n".join(str(o) for o in outgoing), 3)
            if fields:
                fields += ";"
        return f"type {self.name} is\n   message\n{indent(fields, 6)}\n   end message"

    @property
    def type_(self) -> rty.Type:
        return rty.Message(self.full_name)

    @abstractmethod
    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "AbstractMessage":
        raise NotImplementedError

    @abstractmethod
    def proven(self, skip_proof: bool = False) -> "Message":
        raise NotImplementedError

    @property
    def fields(self) -> Tuple[Field, ...]:
        """Return fields topologically sorted."""
        return self._state.fields or ()

    @property
    def all_fields(self) -> Tuple[Field, ...]:
        return (INITIAL, *self.fields, FINAL)

    @property
    def types(self) -> Mapping[Field, mty.Type]:
        """Return fields and corresponding types topologically sorted."""
        return self.__types

    @property
    def aspects(self) -> Mapping[ID, Mapping[ID, Sequence[expr.Expr]]]:
        return self.__aspects

    @property
    def checksums(self) -> Mapping[ID, Sequence[expr.Expr]]:
        return self._state.checksums or {}

    def incoming(self, field: Field) -> Sequence[Link]:
        return [l for l in self.structure if l.target == field]

    def outgoing(self, field: Field) -> Sequence[Link]:
        return [l for l in self.structure if l.source == field]

    def predecessors(self, field: Field) -> Tuple[Field, ...]:
        if field == INITIAL:
            return ()
        if field == FINAL:
            return self.fields
        return self.fields[: self.fields.index(field)]

    def successors(self, field: Field) -> Tuple[Field, ...]:
        if field == INITIAL:
            return self.fields
        if field == FINAL:
            return ()
        return self.fields[self.fields.index(field) + 1 :]

    def direct_predecessors(self, field: Field) -> Sequence[Field]:
        return list(dict.fromkeys([l.source for l in self.incoming(field)]))

    def direct_successors(self, field: Field) -> Sequence[Field]:
        return list(dict.fromkeys([l.target for l in self.outgoing(field)]))

    def definite_predecessors(self, field: Field) -> Tuple[Field, ...]:
        """Return preceding fields which are part of all possible paths."""
        if self._state.definite_predecessors is None:
            self._state.definite_predecessors = {
                f: self.__compute_definite_predecessors(f) for f in self.all_fields
            }
        return self._state.definite_predecessors[field]

    def field_condition(self, field: Field) -> expr.Expr:
        if self._state.field_condition is None:
            self._state.field_condition = {
                f: self.__compute_field_condition(f).simplified() for f in self.all_fields
            }
        return self._state.field_condition[field]

    def field_size(self, field: Field) -> expr.Expr:
        if field == FINAL:
            return expr.Number(0)

        assert field in self.fields, f'field "{field.name}" not found'

        field_type = self.types[field]
        if isinstance(field_type, mty.Scalar):
            return field_type.size

        raise NotImplementedError

    def paths(self, field: Field) -> Set[Tuple[Link, ...]]:
        if field == INITIAL:
            return set()
        if field in self.__paths_cache:
            return self.__paths_cache[field]

        result = set()
        for l in self.incoming(field):
            source = self.paths(l.source)
            for s in source:
                result.add(s + (l,))
            if not source:
                result.add((l,))

        self.__paths_cache[field] = result
        return result

    def prefixed(self, prefix: str) -> "AbstractMessage":
        fields = {f.identifier for f in self.fields}

        def prefixed_expression(expression: expr.Expr) -> expr.Expr:
            variables = {v.identifier for v in expression.variables()}
            literals = {l for l in variables - fields if len(l.parts) == 1}

            return expression.substituted(
                mapping={
                    **{
                        v: v.__class__(ID(prefix) + v.name)
                        for v in expression.variables()
                        if v.identifier in fields
                    },
                    **{
                        v: v.__class__(self.package * v.name)
                        for v in expression.variables()
                        if v.identifier in literals
                        and v.identifier not in mty.BUILTIN_LITERALS
                        and v.identifier != ID("Message")
                    },
                }
            ).simplified()

        structure = []

        for l in self.structure:
            source = Field(prefix + l.source.identifier) if l.source != INITIAL else INITIAL
            target = Field(prefix + l.target.identifier) if l.target != FINAL else FINAL
            condition = prefixed_expression(l.condition)
            length = prefixed_expression(l.length)
            first = prefixed_expression(l.first)
            structure.append(Link(source, target, condition, length, first, l.location))

        types = {Field(prefix + f.identifier): t for f, t in self.types.items()}

        return self.copy(structure=structure, types=types)

    def is_possibly_empty(self, field: Field) -> bool:
        if isinstance(self.types[field], mty.Scalar):
            return False

        for p in self.paths(FINAL):
            if not any(l.target == field for l in p):
                continue
            conditions = [l.condition for l in p if l.condition != expr.TRUE]
            lengths = [
                expr.Equal(expr.Length(l.target.name), l.length)
                for l in p
                if l.length != expr.UNDEFINED
            ]
            empty_field = expr.Equal(expr.Length(field.name), expr.Number(0))
            proof = empty_field.check([*self.type_constraints(empty_field), *conditions, *lengths])
            if proof.result() == expr.ProofResult.sat:
                return True

        return False

    # pylint: disable=too-many-branches
    def __verify(self) -> None:
        type_fields = self.__types.keys() | {INITIAL, FINAL}
        structure_fields = {l.source for l in self.structure} | {l.target for l in self.structure}

        for f in structure_fields - type_fields:
            self.error.append(
                f'missing type for field "{f.name}" in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                f.identifier.location,
            )

        for f in type_fields - structure_fields - {FINAL}:
            self.error.append(
                f'unused field "{f.name}" in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                f.identifier.location,
            )

        if len(self.outgoing(INITIAL)) != 1:
            self.error.append(
                f'ambiguous first field in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            self.error.extend(
                [
                    ("duplicate", Subsystem.MODEL, Severity.INFO, l.target.identifier.location)
                    for l in self.outgoing(INITIAL)
                    if l.target.identifier.location
                ]
            )

        name_conflicts = [
            (f, l)
            for f in type_fields
            for l in qualified_literals(self.types, self.package)
            if f.identifier == l
        ]

        if name_conflicts:
            conflicting_field, conflicting_literal = name_conflicts.pop(0)
            self.error.append(
                f'name conflict for field "{conflicting_field.name}" in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                conflicting_field.identifier.location,
            )
            self.error.append(
                "conflicting enumeration literal",
                Subsystem.MODEL,
                Severity.INFO,
                conflicting_literal.location,
            )

        self.error.propagate()

        for f in structure_fields:
            for l in self.structure:
                if f in (INITIAL, l.target):
                    break
            else:
                self.__has_unreachable = True
                self.error.append(
                    f'unreachable field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )

        duplicate_links = defaultdict(list)
        for link in self.structure:
            duplicate_links[(link.source, link.target, link.condition)].append(link)

        for links in duplicate_links.values():
            if len(links) > 1:
                self.error.append(
                    f'duplicate link from "{links[0].source.identifier}"'
                    f' to "{links[0].target.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    links[0].source.identifier.location,
                )
                self.error.extend(
                    [
                        (
                            "duplicate link",
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.location,
                        )
                        for l in links
                    ]
                )

        for l in self.structure:
            exponentiations = itertools.chain.from_iterable(
                e.findall(lambda x: isinstance(x, expr.Pow))
                for e in [l.condition, l.first, l.length]
            )
            for e in exponentiations:
                assert isinstance(e, expr.Pow)
                variables = e.right.findall(lambda x: isinstance(x, expr.Variable))
                if variables:
                    self.error.append(
                        f'unsupported expression in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        e.location,
                    )
                    for v in variables:
                        self.error.append(
                            f'variable "{v}" in exponent',
                            Subsystem.MODEL,
                            Severity.INFO,
                            v.location,
                        )

    def _verify_expression_types(self) -> None:
        literals = qualified_literals(self.types, self.package)
        types: Dict[ID, mty.Type] = {}

        def typed_variable(expression: expr.Expr) -> expr.Expr:
            if isinstance(expression, expr.Variable):
                if expression.name.lower() == "message":
                    expression.type_ = rty.UniversalInteger()
                if ID(expression.name) in types:
                    expression.type_ = types[ID(expression.name)].type_
                if ID(expression.name) in literals:
                    expression.type_ = literals[ID(expression.name)].type_
            return expression

        for p in self.paths(FINAL):
            types = {}
            path = []
            for l in p:
                path.append(l.target)

                if l.source in self.types:
                    types[l.source.identifier] = self.types[l.source]

                l.condition = l.condition.substituted(typed_variable)
                l.length = l.length.substituted(typed_variable)
                l.first = l.first.substituted(typed_variable)

                for t in [l.condition, l.length, l.first]:
                    if t != expr.UNDEFINED and not t.error.check():
                        expr.check_type(t.error, t, rty.Any())

                    self.error.extend(t.error)

                    if t.error.check():
                        self.error.append(
                            "on path " + " -> ".join(f.name for f in path),
                            Subsystem.MODEL,
                            Severity.INFO,
                            t.location,
                        )

    def _verify_expressions(self) -> None:
        for f in (INITIAL, *self.fields):
            for l in self.outgoing(f):
                self.__check_attributes(l.condition, l.condition.location)
                self.__check_first_expression(l, l.first.location)
                self.__check_length_expression(l)

    def __check_attributes(self, expression: expr.Expr, location: Location = None) -> None:
        for a in expression.findall(lambda x: isinstance(x, expr.Attribute)):
            if isinstance(a, expr.Length) and not (
                isinstance(a.prefix, expr.Variable)
                and (
                    a.prefix.name == "Message"
                    or (
                        Field(a.prefix.name) in self.fields
                        and isinstance(self.types[Field(a.prefix.name)], mty.Composite)
                    )
                )
            ):
                self.error.append(
                    f'invalid use of length attribute for "{a.prefix}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    location,
                )

    def __check_first_expression(self, link: Link, location: Location = None) -> None:
        if link.first != expr.UNDEFINED and not isinstance(link.first, expr.First):
            self.error.append(
                f'invalid First for field "{link.target.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

    def __check_length_expression(self, link: Link) -> None:
        if link.target == FINAL and link.length != expr.UNDEFINED:
            self.error.append(
                f'length attribute for final field in "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                link.length.location,
            )
        if link.target != FINAL and link.target in self.types:
            t = self.types[link.target]
            unconstrained = isinstance(t, (mty.Opaque, mty.Array))
            if not unconstrained and link.length != expr.UNDEFINED:
                self.error.append(
                    f'fixed size field "{link.target.name}" with length expression',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    link.target.identifier.location,
                )
            if unconstrained and link.length == expr.UNDEFINED:
                self.error.append(
                    f'unconstrained field "{link.target.name}" without length expression',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    link.target.identifier.location,
                )

    def type_constraints(self, expression: expr.Expr) -> Sequence[expr.Expr]:
        def get_constraints(aggregate: expr.Aggregate, field: expr.Variable) -> Sequence[expr.Expr]:
            comp = self.types[Field(field.name)]
            assert isinstance(comp, mty.Composite)
            result = expr.Equal(
                expr.Mul(aggregate.length, comp.element_size),
                expr.Length(field),
                location=expression.location,
            )
            if isinstance(comp, mty.Array) and isinstance(comp.element_type, mty.Scalar):
                return [
                    result,
                    *comp.element_type.constraints(name=comp.element_type.name, proof=True),
                ]
            return [result]

        literals = qualified_literals(self.types, self.package)
        scalar_types = [
            (f.name, t)
            for f, t in self.types.items()
            if isinstance(t, mty.Scalar)
            and ID(f.name) not in literals
            and f.name not in ["Message", "Final"]
        ]

        aggregate_constraints: List[expr.Expr] = []
        for r in expression.findall(lambda x: isinstance(x, (expr.Equal, expr.NotEqual))):
            assert isinstance(r, (expr.Equal, expr.NotEqual))
            if isinstance(r.left, expr.Aggregate) and isinstance(r.right, expr.Variable):
                aggregate_constraints.extend(get_constraints(r.left, r.right))
            if isinstance(r.left, expr.Variable) and isinstance(r.right, expr.Aggregate):
                aggregate_constraints.extend(get_constraints(r.right, r.left))

        message_constraints: List[expr.Expr] = [
            expr.Equal(expr.Mod(expr.First("Message"), expr.Number(8)), expr.Number(1)),
            expr.Equal(expr.Mod(expr.Length("Message"), expr.Number(8)), expr.Number(0)),
        ]

        scalar_constraints = [
            c
            for n, t in scalar_types
            for c in t.constraints(name=n, proof=True, same_package=self.package == t.package)
        ]

        return [*message_constraints, *aggregate_constraints, *scalar_constraints]

    def _verify_checksums(self) -> None:
        def valid_lower(expression: expr.Expr) -> bool:
            return isinstance(expression, expr.First) or (
                isinstance(expression, expr.Add)
                and len(expression.terms) == 2
                and isinstance(expression.terms[0], expr.Last)
                and expression.terms[1] == expr.Number(1)
            )

        def valid_upper(expression: expr.Expr) -> bool:
            return isinstance(expression, expr.Last) or (
                isinstance(expression, expr.Sub)
                and isinstance(expression.left, expr.First)
                and expression.right == expr.Number(1)
            )

        for name, expressions in self.checksums.items():  # pylint: disable=too-many-nested-blocks
            if Field(name) not in self.fields:
                self.error.append(
                    f'checksum definition for unknown field "{name}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    name.location,
                )

            for e in expressions:
                if not (
                    isinstance(e, (expr.Variable, expr.Length))
                    or (
                        isinstance(e, expr.ValueRange)
                        and valid_lower(e.lower)
                        and valid_upper(e.upper)
                    )
                ):
                    self.error.append(
                        f'unsupported expression "{e}" in definition of checksum "{name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        e.location,
                    )

                for v in e.findall(lambda x: isinstance(x, expr.Variable)):
                    assert isinstance(v, expr.Variable)

                    if Field(v.name) not in self.fields:
                        self.error.append(
                            f'unknown field "{v.name}" referenced'
                            f' in definition of checksum "{name}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            v.location,
                        )

                if isinstance(e, expr.ValueRange):
                    lower = e.lower.findall(lambda x: isinstance(x, expr.Variable))[0]
                    upper = e.upper.findall(lambda x: isinstance(x, expr.Variable))[0]

                    assert isinstance(lower, expr.Variable)
                    assert isinstance(upper, expr.Variable)

                    if lower != upper:
                        upper_field = (
                            Field(upper.name) if upper.name.lower() != "message" else FINAL
                        )
                        lower_field = (
                            Field(lower.name) if lower.name.lower() != "message" else INITIAL
                        )
                        for p in self.paths(upper_field):
                            if not any(lower_field == l.source for l in p):
                                self.error.append(
                                    f'invalid range "{e}" in definition of checksum "{name}"',
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    e.location,
                                )

        checked = {
            e.prefix.identifier
            for e in self.field_condition(FINAL).findall(
                lambda x: isinstance(x, expr.ValidChecksum)
            )
            if isinstance(e, expr.ValidChecksum) and isinstance(e.prefix, expr.Variable)
        }
        for name in set(self.checksums) - checked:
            self.error.append(
                f'no validity check of checksum "{name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                name.location,
            )
        for name in checked - set(self.checksums):
            self.error.append(
                f'validity check for undefined checksum "{name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                name.location,
            )

    def __prove_conflicting_conditions(self) -> None:
        for f in (INITIAL, *self.fields):
            for i1, c1 in enumerate(self.outgoing(f)):
                for i2, c2 in enumerate(self.outgoing(f)):
                    if i1 < i2:
                        conflict = expr.And(c1.condition, c2.condition)
                        proof = conflict.check(self.type_constraints(conflict))
                        if proof.result() == expr.ProofResult.sat:
                            c1_message = str(c1.condition).replace("\n", " ")
                            c2_message = str(c2.condition).replace("\n", " ")
                            self.error.append(
                                f'conflicting conditions for field "{f.name}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                f.identifier.location,
                            )
                            self.error.append(
                                f"condition {i1} ({f.identifier} -> {c1.target.identifier}):"
                                f" {c1_message}",
                                Subsystem.MODEL,
                                Severity.INFO,
                                c1.condition.location,
                            )
                            self.error.append(
                                f"condition {i2} ({f.identifier} -> {c2.target.identifier}):"
                                f" {c2_message}",
                                Subsystem.MODEL,
                                Severity.INFO,
                                c2.condition.location,
                            )

    def __prove_reachability(self) -> None:
        def has_final(field: Field) -> bool:
            if field == FINAL:
                return True
            for o in self.outgoing(field):
                if has_final(o.target):
                    return True
            return False

        for f in (INITIAL, *self.fields):
            if not has_final(f):
                self.error.append(
                    f'no path to FINAL for field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )

        for f in (*self.fields, FINAL):
            paths = []
            for path in self.paths(f):
                facts = [fact for link in path for fact in self.__link_expression(link)]
                outgoing = self.outgoing(f)
                if f != FINAL and outgoing:
                    facts.append(
                        expr.Or(*[o.condition for o in outgoing], location=f.identifier.location)
                    )
                proof = expr.TRUE.check(facts)
                if proof.result() == expr.ProofResult.sat:
                    break

                paths.append((path, proof.error))
            else:
                self.error.append(
                    f'unreachable field "{f.name}" in "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    f.identifier.location,
                )
                for index, (path, errors) in enumerate(sorted(paths)):
                    self.error.append(
                        f"path {index} (" + " -> ".join([l.target.name for l in path]) + "):",
                        Subsystem.MODEL,
                        Severity.INFO,
                        f.identifier.location,
                    )
                    self.error.extend(
                        [
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                            for m, l in errors
                        ]
                    )

    def __prove_contradictions(self) -> None:
        for f in (INITIAL, *self.fields):
            contradictions = []
            paths = 0
            for path in self.paths(f):
                facts = [fact for link in path for fact in self.__link_expression(link)]
                for c in self.outgoing(f):
                    paths += 1
                    contradiction = c.condition
                    constraints = self.type_constraints(contradiction)
                    proof = contradiction.check([*constraints, *facts])
                    if proof.result() == expr.ProofResult.sat:
                        continue

                    contradictions.append((path, c.condition, proof.error))

            if paths == len(contradictions):
                for path, cond, errors in sorted(contradictions):
                    self.error.append(
                        f'contradicting condition in "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        cond.location,
                    )
                    self.error.extend(
                        [
                            (
                                f'on path: "{l.target.identifier}"',
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.target.identifier.location,
                            )
                            for l in path
                        ]
                    )
                    self.error.extend(
                        [
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                            for m, l in errors
                        ]
                    )

    @staticmethod
    def __target_first(link: Link) -> expr.Expr:
        if link.source == INITIAL:
            return expr.First("Message")
        if link.first != expr.UNDEFINED:
            return link.first
        return expr.Add(expr.Last(link.source.name), expr.Number(1), location=link.location)

    def __target_length(self, link: Link) -> expr.Expr:
        if link.length != expr.UNDEFINED:
            return link.length
        return self.field_size(link.target)

    def __target_last(self, link: Link) -> expr.Expr:
        return expr.Sub(
            expr.Add(self.__target_first(link), self.__target_length(link)),
            expr.Number(1),
            link.target.identifier.location,
        )

    def __link_expression(self, link: Link) -> Sequence[expr.Expr]:
        name = link.target.name
        target_first = self.__target_first(link)
        target_length = self.__target_length(link)
        target_last = self.__target_last(link)
        return [
            expr.Equal(expr.First(name), target_first, target_first.location or self.location),
            expr.Equal(expr.Length(name), target_length, target_length.location or self.location),
            expr.Equal(expr.Last(name), target_last, target_last.location or self.location),
            expr.GreaterEqual(expr.First("Message"), expr.Number(0), self.location),
            expr.GreaterEqual(expr.Last("Message"), expr.Last(name), self.location),
            expr.GreaterEqual(expr.Last("Message"), expr.First("Message"), self.location),
            expr.Equal(
                expr.Length("Message"),
                expr.Add(expr.Sub(expr.Last("Message"), expr.First("Message")), expr.Number(1)),
                self.location,
            ),
            *expression_list(link.condition),
        ]

    def __prove_field_positions(self) -> None:
        for f in (*self.fields, FINAL):
            for path in self.paths(f):

                last = path[-1]
                negative = expr.Less(
                    self.__target_length(last), expr.Number(0), last.length.location
                )
                start = expr.GreaterEqual(
                    self.__target_first(last), expr.First("Message"), last.location
                )

                facts = [fact for link in path for fact in self.__link_expression(link)]

                outgoing = self.outgoing(f)
                if f != FINAL and outgoing:
                    facts.append(
                        expr.Or(*[o.condition for o in outgoing], location=f.identifier.location)
                    )

                facts.extend(self.type_constraints(negative))
                facts.extend(self.type_constraints(start))

                proof = expr.TRUE.check(facts)

                # Only check positions of reachable paths
                if proof.result() != expr.ProofResult.sat:
                    continue

                proof = negative.check(facts)
                if proof.result() != expr.ProofResult.unsat:
                    path_message = " -> ".join([l.target.name for l in path])
                    self.error.append(
                        f'negative length for field "{f.name}" ({path_message})',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        f.identifier.location,
                    )
                    return

                proof = start.check(facts)
                if proof.result() != expr.ProofResult.sat:
                    path_message = " -> ".join([last.target.name for last in path])
                    self.error.append(
                        f'negative start for field "{f.name}" ({path_message})',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.identifier.location,
                    )
                    self.error.extend(
                        [
                            (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, locn)
                            for m, locn in proof.error
                        ]
                    )
                    return

                if f in self.__types:
                    t = self.__types[f]
                    if not isinstance(t, mty.Opaque):
                        continue
                    element_size = t.element_size
                    start_aligned = expr.Not(
                        expr.Equal(
                            expr.Mod(self.__target_first(last), element_size),
                            expr.Number(1),
                            last.location,
                        )
                    )
                    proof = start_aligned.check([*facts, *self.type_constraints(start_aligned)])
                    if proof.result() != expr.ProofResult.unsat:
                        path_message = " -> ".join([p.target.name for p in path])
                        self.error.append(
                            f'opaque field "{f.name}" not aligned to {element_size} bit boundary'
                            f" ({path_message})",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        )
                        return

                    length_multiple_element_size = expr.Not(
                        expr.Equal(
                            expr.Mod(self.__target_length(last), element_size),
                            expr.Number(0),
                            last.location,
                        )
                    )
                    proof = length_multiple_element_size.check(
                        [*facts, *self.type_constraints(length_multiple_element_size)]
                    )
                    if proof.result() != expr.ProofResult.unsat:
                        path_message = " -> ".join([p.target.name for p in path])
                        self.error.append(
                            f'length of opaque field "{f.name}" not multiple of {element_size} bit'
                            f" ({path_message})",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            f.identifier.location,
                        )
                        return

    def __prove_coverage(self) -> None:
        """
        Prove that the fields of a message cover all message bits, i.e. there are no holes in the
        message definition.

        Idea: Let f be the bits covered by the message. By definition
            (1) f >= Message'First and f <= Message'Last
        holds. For every field add a conjunction of the form
            (2) Not(f >= Field'First and f <= Field'Last),
        effectively pruning the range that this field covers from the bit range of the message. For
        the overall expression, prove that it is false for all f, i.e. no bits are left.
        """
        for path in [p[:-1] for p in self.paths(FINAL) if p]:

            facts: Sequence[expr.Expr]

            # Calculate (1)
            facts = [
                expr.GreaterEqual(expr.Variable("f"), expr.First("Message")),
                expr.LessEqual(expr.Variable("f"), expr.Last("Message")),
            ]
            # Calculate (2) for all fields
            facts.extend(
                [
                    expr.Not(
                        expr.And(
                            expr.GreaterEqual(expr.Variable("f"), self.__target_first(l)),
                            expr.LessEqual(expr.Variable("f"), self.__target_last(l)),
                            location=l.location,
                        )
                    )
                    for l in path
                ]
            )

            # Define that the end of the last field of a path is the end of the message
            facts.append(
                expr.Equal(self.__target_last(path[-1]), expr.Last("Message"), self.location)
            )

            # Constraints for links and types
            facts.extend([f for l in path for f in self.__link_expression(l)])

            # Coverage expression must be False, i.e. no bits left
            proof = expr.TRUE.check(facts)
            if proof.result() == expr.ProofResult.sat:
                self.error.append(
                    "path does not cover whole message",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.identifier.location,
                )
                self.error.extend(
                    [
                        (
                            f'on path: "{l.target.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.target.identifier.location,
                        )
                        for l in path
                    ]
                )
                return

    def __prove_overlays(self) -> None:
        for f in (INITIAL, *self.fields):
            for p, l in [(p, p[-1]) for p in self.paths(f) if p]:
                if l.first != expr.UNDEFINED and isinstance(l.first, expr.First):
                    facts = [f for l in p for f in self.__link_expression(l)]
                    overlaid = expr.Equal(
                        self.__target_last(l), expr.Last(l.first.prefix), l.location
                    )
                    proof = overlaid.check(facts)
                    if proof.result() != expr.ProofResult.sat:
                        self.error.append(
                            f'field "{f.name}" not congruent with'
                            f' overlaid field "{l.first.prefix}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.identifier.location,
                        )
                        self.error.extend(
                            [
                                (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, l)
                                for m, l in proof.error
                            ]
                        )

    def _prove(self) -> None:
        self.__prove_conflicting_conditions()
        self.__prove_reachability()
        self.__prove_contradictions()
        self.__prove_coverage()
        self.__prove_overlays()
        self.__prove_field_positions()

    def __compute_topological_sorting(self) -> Optional[Tuple[Field, ...]]:
        """Return fields topologically sorted (Kahn's algorithm)."""
        result: Tuple[Field, ...] = ()
        fields = [INITIAL]
        visited = set()
        while fields:
            n = fields.pop(0)
            result += (n,)
            for e in self.outgoing(n):
                visited.add(e)
                if set(self.incoming(e.target)) <= visited:
                    fields.append(e.target)
        if not self.__has_unreachable and set(self.structure) - visited:
            self.error.append(
                f'structure of "{self.identifier}" contains cycle',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            # ISSUE: Componolit/RecordFlux#256
            return None
        return tuple(f for f in result if f not in [INITIAL, FINAL])

    def __compute_definite_predecessors(self, final: Field) -> Tuple[Field, ...]:
        return tuple(
            f
            for f in self.fields
            if all(any(f == pf.source for pf in p) for p in self.paths(final))
        )

    def __compute_field_condition(self, final: Field) -> expr.Expr:
        if final == INITIAL:
            return expr.TRUE
        return expr.Or(
            *[
                expr.And(self.__compute_field_condition(l.source), l.condition)
                for l in self.incoming(final)
            ],
            location=final.identifier.location,
        )


class Message(AbstractMessage):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        structure: Sequence[Link],
        types: Mapping[Field, mty.Type],
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
        state: MessageState = None,
        skip_proof: bool = False,
    ) -> None:
        super().__init__(identifier, structure, types, aspects, location, error, state)

        if not self.error.check() and (structure or types) and not skip_proof:
            self._verify_expression_types()
            self._verify_expressions()
            self._verify_checksums()
            self.error.propagate()
            self._prove()

        self.error.propagate()

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "Message":
        return Message(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            aspects if aspects else copy(self.aspects),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self, skip_proof: bool = False) -> "Message":
        return copy(self)


class DerivedMessage(Message):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        base: AbstractMessage,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> None:

        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            aspects if aspects else copy(base.aspects),
            location if location else base.location,
            error if error else base.error,
        )
        self.base = base

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "DerivedMessage":
        return DerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            aspects if aspects else copy(self.aspects),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self, skip_proof: bool = False) -> "DerivedMessage":
        return copy(self)


class UnprovenMessage(AbstractMessage):
    # pylint: disable=too-many-arguments
    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "UnprovenMessage":
        return UnprovenMessage(
            identifier if identifier else self.identifier,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            aspects if aspects else copy(self.aspects),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self, skip_proof: bool = False) -> Message:
        return Message(
            identifier=self.identifier,
            structure=self.structure,
            types=self.types,
            location=self.location,
            error=self.error,
            state=self._state,
            skip_proof=skip_proof,
        )

    @ensure(lambda result: valid_message_field_types(result))
    def merged(self) -> "UnprovenMessage":
        def prune_dangling_fields(
            structure: List[Link], types: Dict[Field, mty.Type]
        ) -> Tuple[List[Link], Dict[Field, mty.Type]]:
            dangling = []
            progress = True
            while progress:
                progress = False
                fields = {x for l in structure for x in (l.source, l.target) if x != FINAL}
                for s in fields:
                    if all(l.source != s for l in structure):
                        dangling.append(s)
                        progress = True
                structure = [l for l in structure if l.target not in dangling]

            return (
                structure,
                {k: v for k, v in types.items() if k not in dangling},
            )

        message = self

        while True:
            inner_messages = [
                (f, t) for f, t in message.types.items() if isinstance(t, AbstractMessage)
            ]

            if not inner_messages:
                break

            field, inner_message = inner_messages.pop(0)
            inner_message = inner_message.prefixed(f"{field.name}_")

            name_conflicts = [
                f for f in message.fields for g in inner_message.fields if f.name == g.name
            ]

            if name_conflicts:
                conflicting = name_conflicts.pop(0)
                self.error.append(
                    f'name conflict for "{conflicting.identifier}" in "{message.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    conflicting.identifier.location,
                )
                self.error.append(
                    f'when merging message "{inner_message.identifier}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    inner_message.location,
                )
                self.error.append(
                    f'into field "{field.name}"',
                    Subsystem.MODEL,
                    Severity.INFO,
                    field.identifier.location,
                )

            structure = []

            for link in message.structure:
                if link.target == field:
                    initial_link = inner_message.outgoing(INITIAL)[0]
                    structure.append(
                        Link(
                            link.source,
                            initial_link.target,
                            link.condition,
                            initial_link.length,
                            link.first,
                            link.location,
                        )
                    )
                elif link.source == field:
                    for final_link in inner_message.incoming(FINAL):
                        merged_condition = expr.And(link.condition, final_link.condition)
                        proof = merged_condition.check(
                            [
                                *inner_message.type_constraints(merged_condition),
                                inner_message.field_condition(final_link.source),
                            ]
                        )
                        if proof.result() != expr.ProofResult.unsat:
                            structure.append(
                                Link(
                                    final_link.source,
                                    link.target,
                                    merged_condition.simplified(),
                                    link.length,
                                    link.first,
                                    link.location,
                                )
                            )
                else:
                    structure.append(link)

            structure.extend(
                l for l in inner_message.structure if l.target != FINAL and l.source != INITIAL
            )

            types = {
                **{f: t for f, t in message.types.items() if f != field},
                **inner_message.types,
            }

            structure, types = prune_dangling_fields(structure, types)
            if not structure or not types:
                fail(
                    f'empty message type when merging field "{field.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    field.identifier.location,
                )

            message = message.copy(structure=structure, types=types)

        return message


class UnprovenDerivedMessage(UnprovenMessage):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        identifier: StrID,
        base: AbstractMessage,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> None:

        super().__init__(
            identifier,
            structure if structure else copy(base.structure),
            types if types else copy(base.types),
            aspects if aspects else copy(base.aspects),
            location if location else base.location,
            error if error else base.error,
        )
        self.error.extend(base.error)
        self.base = base

    def copy(
        self,
        identifier: StrID = None,
        structure: Sequence[Link] = None,
        types: Mapping[Field, mty.Type] = None,
        aspects: Mapping[ID, Mapping[ID, Sequence[expr.Expr]]] = None,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> "UnprovenDerivedMessage":
        return UnprovenDerivedMessage(
            identifier if identifier else self.identifier,
            self.base,
            structure if structure else copy(self.structure),
            types if types else copy(self.types),
            aspects if aspects else copy(self.aspects),
            location if location else self.location,
            error if error else self.error,
        )

    def proven(self, skip_proof: bool = False) -> DerivedMessage:
        return DerivedMessage(
            self.identifier,
            self.base,
            self.structure,
            self.types,
            self.aspects,
            self.location,
            self.error,
        )


class Refinement(mty.Type):
    # pylint: disable=too-many-arguments
    def __init__(
        self,
        package: StrID,
        pdu: Message,
        field: Field,
        sdu: Message,
        condition: expr.Expr = expr.TRUE,
        location: Location = None,
        error: RecordFluxError = None,
    ) -> None:
        package = ID(package)

        super().__init__(
            package * "__REFINEMENT__"
            f"{flat_name(sdu.full_name)}__{flat_name(pdu.full_name)}__{field.name}__",
            location,
            error,
        )

        self.error = error or RecordFluxError()
        if len(package.parts) != 1:
            self.error.append(
                f'unexpected format of package name "{package}"',
                Subsystem.MODEL,
                Severity.ERROR,
                package.location,
            )

        for f, t in pdu.types.items():
            if f == field:
                if not isinstance(t, mty.Opaque):
                    self.error.append(
                        f'invalid type of field "{field.name}" in refinement of "{pdu.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        field.identifier.location,
                    )
                    self.error.append(
                        "expected field of type Opaque",
                        Subsystem.MODEL,
                        Severity.INFO,
                        f.identifier.location,
                    )
                break
        else:
            self.error.append(
                f'invalid field "{field.name}" in refinement of "{pdu.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                field.identifier.location,
            )

        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition

    def __str__(self) -> str:
        condition = f"\n   if {self.condition}" if self.condition != expr.TRUE else ""
        return f"for {self.pdu.name} use ({self.field.name} => {self.sdu.name}){condition}"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.package == other.package
                and self.pdu == other.pdu
                and self.field == other.field
                and self.sdu == other.sdu
            )
        return NotImplemented


def qualified_literals(types: Mapping[Field, mty.Type], package: ID) -> Dict[ID, mty.Enumeration]:
    literals = {}

    for t in types.values():
        if isinstance(t, mty.Enumeration):
            for l in t.literals:
                if t.package == const.BUILTINS_PACKAGE or t.package == package:
                    literals[l] = t
                if t.package != const.BUILTINS_PACKAGE:
                    literals[t.package * l] = t

    return literals


def expression_list(expression: expr.Expr) -> Sequence[expr.Expr]:
    if isinstance(expression, expr.And):
        return expression.terms
    return [expression]
