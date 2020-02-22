from typing import List

from rflx.error import Location, RecordFluxError
from rflx.expression import Expr
from rflx.identifier import ID, StrID
from rflx.model import Base


class Declaration(Base):
    def __init__(self, location: Location = None, error: RecordFluxError = None) -> None:
        self.location = location
        self.error = error or RecordFluxError()


class Argument(Declaration):
    def __init__(self, name: StrID, typ: StrID, location: Location = None):
        super().__init__(location)
        self.__name = ID(name)
        self.__type = ID(typ)


class VariableDeclaration(Declaration):
    def __init__(self, typ: StrID, init: Expr = None, location: Location = None):
        super().__init__(location)
        self.__type = ID(typ)
        self.__init = init


class Subprogram(Declaration):
    def __init__(self, arguments: List[Argument], return_type: StrID, location: Location = None):
        super().__init__(location)
        self.__arguments = arguments
        self.__return_type = ID(return_type)