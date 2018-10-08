#!/usr/bin/env python

from abc import ABC, abstractmethod
from copy import copy
from math import log
from pprint import pformat
from typing import Dict, List, Optional, Tuple


class Expr(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented


class LogExpr(Expr):
    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> 'LogExpr':
        raise NotImplementedError


class TrueExpr(LogExpr):
    def __repr__(self) -> str:
        return 'TRUE'

    def __str__(self) -> str:
        return self.__repr__()

    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        return self


TRUE = TrueExpr()


class BinLogExpr(LogExpr):
    def __init__(self, left: LogExpr, right: LogExpr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '({} {} {})'.format(self.left, self.__class__.__name__, self.right)

    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        raise NotImplementedError


class And(BinLogExpr):
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if left is TRUE and right is TRUE:
            return TRUE
        if left is TRUE:
            return right
        if right is TRUE:
            return left
        return And(left, right)


class Or(BinLogExpr):
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if left is TRUE or right is TRUE:
            return TRUE
        return Or(left, right)


class MathExpr(Expr):
    def __lt__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return self == other
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return self == other
        return NotImplemented

    @abstractmethod
    def __neg__(self) -> 'MathExpr':
        raise NotImplementedError

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> 'MathExpr':
        raise NotImplementedError

    @abstractmethod
    def to_bytes(self) -> 'MathExpr':
        raise NotImplementedError


class UndefinedExpr(MathExpr):
    def __init__(self) -> None:
        pass

    def __repr__(self) -> str:
        return 'UNDEFINED'

    def __str__(self) -> str:
        return self.__repr__()

    def __neg__(self) -> MathExpr:
        return self

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        return self

    def to_bytes(self) -> MathExpr:
        return self


UNDEFINED = UndefinedExpr()


class Number(MathExpr):
    def __init__(self, value: int) -> None:
        self.value = value

    def __repr__(self) -> str:
        return 'Number({})'.format(self.value)

    def __str__(self) -> str:
        if self.value < 0:
            return '({})'.format(self.value)
        return str(self.value)

    def __int__(self) -> int:
        return self.value

    def __neg__(self) -> 'Number':
        return Number(-self.value)

    def __add__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value + other.value)
        return NotImplemented

    def __sub__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value - other.value)
        return NotImplemented

    def __mul__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value * other.value)
        return NotImplemented

    def __floordiv__(self, other: 'Number') -> 'MathExpr':
        if isinstance(other, Number):
            if self.value % other.value == 0:
                return Number(self.value // other.value)
            return Div(Number(self.value), Number(other.value))
        return NotImplemented

    def __lt__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value < other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value <= other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value > other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value >= other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        return self

    def to_bytes(self) -> MathExpr:
        if self.value % 8 != 0 and (self.value + 1) % 8 != 0:
            raise RuntimeError('value should point to first or last bit of byte')
        return Number(self.value // 8)


class AssMathExpr(MathExpr):
    def __init__(self, *terms: MathExpr) -> None:
        self.terms = list(terms)

    def __repr__(self) -> str:
        return '({})'.format(' {} '.format(self.symbol()).join(map(str, self.terms)))

    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def __neg__(self) -> MathExpr:
        raise NotImplementedError

    def __lt__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                lt = [x < y for x, y in zip(self.terms, other.terms)]
                eq = [x == y for x, y in zip(self.terms, other.terms)]
                return any(lt) and all(map((lambda x: x[0] or x[1]), zip(lt, eq)))
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                return all([x <= y for x, y in zip(self.terms, other.terms)])
            return False
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                gt = [x > y for x, y in zip(self.terms, other.terms)]
                eq = [x == y for x, y in zip(self.terms, other.terms)]
                return any(gt) and all(map((lambda x: x[0] or x[1]), zip(gt, eq)))
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                return all([x >= y for x, y in zip(self.terms, other.terms)])
            return False
        return NotImplemented

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        terms: List[MathExpr] = []
        all_terms = list(self.terms)
        total = self.neutral_element()
        for term in all_terms:
            t = term.simplified(facts)
            if isinstance(t, Number):
                total = self.operation(total, t.value)
            elif isinstance(t, type(self)):
                all_terms += t.terms
            else:
                terms.append(t)
        if not terms:
            return Number(total)
        if total != self.neutral_element():
            terms.append(Number(total))
        if len(terms) == 1:
            return terms[0]
        return self.__class__(*terms)

    @abstractmethod
    def operation(self, left: int, right: int) -> int:
        raise NotImplementedError

    @abstractmethod
    def neutral_element(self) -> int:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError

    def to_bytes(self) -> MathExpr:
        return self.__class__(*[term.to_bytes() for term in self.terms])


class Add(AssMathExpr):
    def __neg__(self) -> MathExpr:
        return Add(*[-term for term in self.terms])

    def operation(self, left: int, right: int) -> int:
        return left + right

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        expr = super().simplified(facts)
        if not isinstance(expr, Add):
            return expr
        terms: List[MathExpr] = []
        for term in expr.terms:
            complement = False
            for other in terms:
                if other == -term:
                    terms.remove(other)
                    complement = True
            if not complement:
                terms.append(term)
        if len(terms) == 1:
            return terms[0]
        return Add(*terms)

    def neutral_element(self) -> int:
        return 0

    def symbol(self) -> str:
        return '+'


class Mul(AssMathExpr):
    def __neg__(self) -> MathExpr:
        return Mul(*list(self.terms) + [Number(-1)])

    def operation(self, left: int, right: int) -> int:
        return left * right

    def neutral_element(self) -> int:
        return 1

    def symbol(self) -> str:
        return '*'


class BinMathExpr(MathExpr):
    def __init__(self, left: 'MathExpr', right: 'MathExpr') -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '({} {} {})'.format(self.left, self.symbol(), self.right)

    def __str__(self) -> str:
        return self.__repr__()

    def __neg__(self) -> MathExpr:
        return self.__class__(-self.left, self.right)

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError

    def to_bytes(self) -> MathExpr:
        left = self.left.to_bytes()
        right = self.right.to_bytes()
        return self.__class__(left, right)


class Sub(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left - right
        if isinstance(left, Number):
            return Add(right, -left)
        if isinstance(right, Number):
            return Add(left, -right)
        return Add(left, -right)

    def symbol(self) -> str:
        return '-'


class Div(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left // right
        if isinstance(left, Add) and isinstance(right, Number):
            return Add(*[Div(term, right) for term in left.terms]).simplified(facts)
        if isinstance(left, Mul) and isinstance(right, Number):
            terms: List[MathExpr] = []
            for term in left.terms:
                if isinstance(term, Number):
                    terms.append((term // right).simplified(facts))
                else:
                    terms.append(term)
            return Mul(*terms).simplified(facts)
        return Div(left, right)

    def symbol(self) -> str:
        return '/'


class Attribute(MathExpr):
    def __init__(self, name: str, negative: bool = False) -> None:
        self.name = name
        self.negative = negative

    def __repr__(self) -> str:
        result = '{}\'{}'.format(self.name, self.__class__.__name__)
        if self.negative:
            return '(-{})'.format(result)
        return result

    def __hash__(self) -> int:
        return hash(self.name + self.__class__.__name__)

    def __neg__(self) -> 'Attribute':
        return self.__class__(self.name, not self.negative)

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        if facts and self in facts:
            return facts[self]
        return self

    def to_bytes(self) -> MathExpr:
        return self


class Value(Attribute):
    def __str__(self) -> str:
        if self.negative:
            return '(-{})'.format(self.name)
        return self.name


class Length(Attribute):
    pass


class First(Attribute):
    pass


class Last(Attribute):
    pass


class Relation(LogExpr):
    def __init__(self, left: MathExpr, right: MathExpr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '{}({}, {})'.format(self.__class__.__name__, self.left, self.right)

    def __str__(self) -> str:
        return self.__repr__()

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> 'Relation':
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        return self.__class__(left, right)

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class Less(Relation):
    def symbol(self) -> str:
        return '<'


class LessEqual(Relation):
    def symbol(self) -> str:
        return '<='


class Equal(Relation):
    def symbol(self) -> str:
        return '='


class GreaterEqual(Relation):
    def symbol(self) -> str:
        return '>='


class Greater(Relation):
    def symbol(self) -> str:
        return '>'


class NotEqual(Relation):
    def symbol(self) -> str:
        return '!='


class Type(ABC):
    def __init__(self, name: str) -> None:
        self.name = name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'Type({})'.format(self.name)

    @abstractmethod
    def size(self) -> Number:
        raise NotImplementedError


class ModularInteger(Type):
    def __init__(self, name: str, modulus: int) -> None:
        if modulus == 0 or (modulus & (modulus - 1)) != 0:
            raise ModelError('invalid type {}: {} is not a power of two'.format(name, modulus))
        super().__init__(name)
        self.modulus = modulus
        self.__size = int(log(self.modulus) / log(2))

    def size(self) -> Number:
        return Number(self.__size)


class RangeInteger(Type):
    def __init__(self, name: str, first: int, last: int, size: int) -> None:
        if first < 0:
            raise ModelError('invalid type {}: negative first'.format(name))
        if first > last:
            raise ModelError('invalid type {}: negative range'.format(name))
        if log(last + 1) / log(2) > size:
            raise ModelError('invalid type {}: size too small for given range'.format(name))
        super().__init__(name)
        self.first = first
        self.last = last
        self.__size = size

    def size(self) -> Number:
        return Number(self.__size)


class Array(Type):
    def size(self) -> Number:
        raise RuntimeError('array type has no fixed size')


class Node:
    def __init__(self, name: str, data_type: Type, edges: List['Edge'] = None) -> None:
        self.name = name
        self.type = data_type
        self.edges = edges or []

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'Node(\n\t\'{}\',\n\t{},\n\t{}\n\t)'.format(
            self.name, self.type, self.edges).replace('\t', '\t  ')


FINAL = Node('', Array(''))


class Edge:
    def __init__(self, target: Node, condition: LogExpr = TRUE, length: MathExpr = UNDEFINED,
                 first: MathExpr = UNDEFINED) -> None:
        self.target = target
        self.condition = condition
        self.length = length
        self.first = first

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'Edge(\n\t{},\n\t{},\n\t{},\n\t{}\n\t)'.format(
            self.target, self.condition, self.length, self.first).replace('\t', '\t  ')


class Field:
    def __init__(self, name: str, data_type: Type,
                 variants: List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]) -> None:
        self.name = name
        self.type = data_type
        self.variants = variants

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'Field(\n\t{},\n\t{},\n\t{}\n)'.format(
            self.name, self.type, pformat(self.variants, indent=2))


class PDU:
    def __init__(self, name: str, node: Node) -> None:
        self.name = name
        self.initial_node = node

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'PDU(\n\t{},\n\t{}\n)'.format(
            self.name, self.initial_node)

    def fields(self, facts: Dict[Attribute, MathExpr] = None,
               first: MathExpr = UNDEFINED) -> List[Field]:
        if facts is None:
            facts = {}
        return evaluate(facts, TRUE, Edge(self.initial_node, TRUE, first=first))


class ModelError(Exception):
    pass


def evaluate(facts: Dict[Attribute, MathExpr], all_cond: LogExpr, in_edge: Edge,
             visited: List[Edge] = None) -> List[Field]:
    node = in_edge.target

    if in_edge.length is UNDEFINED:
        in_edge.length = node.type.size()
    if in_edge.first is UNDEFINED:
        in_edge.first = Number(0)

    facts = create_facts(facts, in_edge)

    fields = [Field(node.name,
                    node.type,
                    [(combine_conditions(all_cond,
                                         in_edge.condition,
                                         [e.condition for e in node.edges]).simplified(),
                      facts)])]

    for out_edge in node.edges:
        if out_edge.target is FINAL:
            continue

        visited = create_visited_edges(visited, out_edge)

        edge = copy(out_edge)
        if edge.first is UNDEFINED:
            edge.first = Add(in_edge.first, in_edge.length)

        extend_fields(fields,
                      evaluate(facts,
                               combine_conditions(all_cond, in_edge.condition, []),
                               edge,
                               visited))

    return fields


def create_facts(facts: Dict[Attribute, MathExpr], edge: Edge) -> Dict[Attribute, MathExpr]:
    facts = dict(facts)
    facts[First(edge.target.name)] = edge.first.simplified(facts)
    facts[Last(edge.target.name)] = Add(edge.first, edge.length, Number(-1)).simplified(facts)
    return facts


def combine_conditions(all_cond: LogExpr, in_cond: LogExpr, out_cond: List[LogExpr]) -> LogExpr:
    if out_cond:
        res = out_cond.pop()
        for c in out_cond:
            res = Or(res, c)
    else:
        res = TRUE
    return And(And(all_cond, in_cond), res)


def create_visited_edges(visited: Optional[List[Edge]], edge: Edge) -> List[Edge]:
    if not visited:
        visited = []
    if edge in visited:
        raise ModelError('cyclic')
    return list(visited + [edge])


def extend_fields(fields: List[Field], new_fields: List[Field]) -> None:
    for new_field in new_fields:
        found = False
        for field in fields:
            if field.name == new_field.name:
                if field.type != new_field.type:
                    raise ModelError('duplicate node {} with different types ({} != {})'.format(
                        field.name, field.type.name, new_field.type.name))
                field.variants += new_field.variants
                found = True
        if not found:
            fields.append(new_field)


def filter_fields(fields: Dict[str, List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]]
                  ) -> Dict[str, List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]]:
    return {
        field:
        [
            (
                condition,
                {attr: expr for attr, expr in expressions.items() if attr.name == field}
            )
            for condition, expressions in variants
        ]
        for field, variants in fields.items()
    }