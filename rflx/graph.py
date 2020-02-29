from copy import copy
from typing import BinaryIO, Set, Union

from pydotplus import Dot, Edge, Node

from rflx.expression import TRUE, UNDEFINED
from rflx.fsm import State, StateMachine
from rflx.model import FINAL, INITIAL, Link, Message
from rflx.statement import Assignment


class Graph:
    def __init__(self, data: Union[StateMachine, Message]) -> None:
        self.__data = copy(data)

    def __target_size(self, link: Link) -> str:
        if not isinstance(self.__data, Message):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")
        return str(self.__data.field_size(link.target))

    def __edge_label(self, link: Link) -> str:
        return "({cond},{sep1}{length},{sep2}{first})".format(
            cond=str(link.condition) if link.condition != TRUE else "⊤",
            sep1=" " if link.condition == TRUE or link.length == UNDEFINED else "\n",
            length=str(link.length) if link.length != UNDEFINED else self.__target_size(link),
            sep2=" " if link.first == UNDEFINED else "\n",
            first=str(link.first) if link.first != UNDEFINED else "⋆",
        )

    @property
    def get(self) -> Dot:
        if isinstance(self.__data, Message):
            return self.__get_message
        if isinstance(self.__data, StateMachine):
            return self.__get_session
        raise NotImplementedError(f"Unsupported data format {type(self.__data).__name__}")

    @classmethod
    def __graph_with_defaults(cls, name: str) -> Dot:
        """Return default pydot graph."""

        result = Dot(graph_name=name)
        result.set_graph_defaults(splines="ortho", ranksep="0.8 equally")
        result.set_edge_defaults(
            fontname="Fira Code", fontcolor="#6f6f6f", color="#6f6f6f", penwidth="2.5"
        )
        result.set_node_defaults(
            fontname="Arimo",
            fontcolor="#ffffff",
            color="#6f6f6f",
            fillcolor="#009641",
            width="1.5",
            style='"rounded,filled"',
            shape="box",
        )
        return result

    def __add_state(self, state: State, result: Dot, variables: Set[str]) -> None:

        if not isinstance(self.__data, StateMachine):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")

        variables_read: Set[str] = set()
        variables_write: Set[str] = set()
        if state.name == self.__data.initial:
            result.add_node(Node(name=state.name.name, fillcolor="#ffffff", fontcolor="black"))
        elif state.name == self.__data.final:
            result.add_node(Node(name=state.name.name, fillcolor="#6f6f6f"))
        else:
            result.add_node(Node(name=state.name.name))

        if not isinstance(state.name.name, str):
            raise TypeError

        for index, t in enumerate(state.transitions):
            label = (
                f"{state.name.name} → {t.target.name}\n\n[{index}] {t.condition}"
                if t.condition != TRUE
                else ""
            )
            result.add_edge(Edge(src=state.name.name, dst=t.target.name, tooltip=label))
            variables_read.update(
                [
                    v.name
                    for v in t.condition.variables()
                    if isinstance(v.name, str) and v.name not in state.declarations
                ]
            )

        for index, a in enumerate(state.actions):
            if a.name not in state.declarations:
                variables_write.update([a.name])
            if isinstance(a, Assignment):
                variables_read.update(
                    [
                        v.name
                        for v in a.expression.variables()
                        if isinstance(v.name, str) and v.name not in state.declarations
                    ]
                )

        for v in variables_read:
            result.add_edge(
                Edge(src=v, dst=state.name.name, tooltip=f"{state.name.name}: read {v}")
            )
        for v in variables_write:
            result.add_edge(
                Edge(src=state.name.name, dst=v, tooltip=f"{state.name.name}: write {v}")
            )

        variables.update(variables_read)
        variables.update(variables_write)

    @property
    def __get_session(self) -> Dot:
        """Return pydot graph representation of session."""

        if not isinstance(self.__data, StateMachine):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")

        variables: Set[str] = set()
        result = self.__graph_with_defaults("StateMachine")
        for s in self.__data.states:
            self.__add_state(s, result, variables)

        for v in variables:
            result.add_node(Node(name=v, fillcolor="#7e8ab8"))

        return result

    @property
    def __get_message(self) -> Dot:
        """Return pydot graph representation of message."""

        if not isinstance(self.__data, Message):
            raise TypeError(f"Invalid data format {type(self.__data).__name__}")

        if not self.__data.structure:
            self.__data.structure = [Link(INITIAL, FINAL)]

        result = self.__graph_with_defaults(self.__data.full_name)
        result.add_node(
            Node(name="Initial", fillcolor="#ffffff", shape="circle", width="0.5", label="")
        )
        for f in self.__data.fields:
            result.add_node(Node(name=f.name))
        for l in self.__data.structure:
            result.add_edge(Edge(src=l.source.name, dst=l.target.name, xlabel=self.__edge_label(l)))
        result.add_node(
            Node(name="Final", fillcolor="#6f6f6f", shape="circle", width="0.5", label="")
        )
        return result

    def write(self, handle: BinaryIO, fmt: str = "svg") -> None:
        self.get.write(handle, format=fmt)
