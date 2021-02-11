#!/usr/bin/env python3

"""
This tool checks that each requirement is referenced at least once in the given directories.
"""

import argparse
import re
import sys
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

ID_REGEX = r"(?P<id>§(?P<prefix>\w+)(?P<number>(?:\d+\.)*\d+))"


class ID:
    def __init__(self, identifier: str):
        match = re.match(ID_REGEX, identifier)

        if not match:
            raise ValueError(f'invalid identifier "{identifier}"')

        self._prefix = match.group("prefix")
        self._number = tuple(int(p) for p in match.group("number").split("."))

    def __str__(self) -> str:
        return f"§{self._prefix}" + ".".join(str(p) for p in self._number)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return NotImplemented
        return self.prefix < other.prefix or (
            self.prefix == other.prefix and self.number < other.number[: len(self.number)]
        )

    @property
    def prefix(self) -> str:
        return self._prefix

    @property
    def number(self) -> Tuple[int, ...]:
        return self._number


class Requirement:
    def __init__(
        self,
        identifier: str,
        description: str,
        referenced: bool = False,
        requirements: List["Requirement"] = None,
    ):
        self._identifier = identifier
        self._description = description
        self._referenced = referenced
        self.requirements = requirements or []

    def __contains__(self, value: str) -> bool:
        return value == self.identifier or any(value in req for req in self.requirements)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return NotImplemented
        return ID(self.identifier) < ID(other.identifier)

    @property
    def identifier(self) -> str:
        return str(self._identifier)

    @property
    def description(self) -> str:
        return self._description

    @property
    def referenced(self) -> bool:
        if self.requirements:
            return all(req.referenced for req in self.requirements)
        return self._referenced

    @referenced.setter
    def referenced(self, value: bool) -> None:
        if self.requirements:
            raise ValueError('"referenced" attribute of meta requirement must not be set')
        self._referenced = value


def main(argv: Sequence[str]) -> bool:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("requirements", metavar="REQUIREMENTS_FILE", type=Path)
    arg_parser.add_argument("directories", metavar="DIRECTORY", type=Path, nargs="+")
    arg_parser.add_argument("--checkbox-list", help="print checkbox list", action="store_true")
    args = arg_parser.parse_args(argv[1:])

    requirements = parse_requirements(args.requirements)
    if requirements is None:
        return False

    references = search_references(args.directories)

    error = check_references(requirements, references)

    if args.checkbox_list:
        print_checkbox_list(requirements)

    return error


def parse_requirements(requirements_file: Path) -> Optional[List[Requirement]]:
    requirements: Dict[str, Requirement] = {}
    error = False

    with open(requirements_file) as f:
        for l in f:
            if "§" not in l:
                continue

            match = re.search(r"(?:^|[-#.!?] )([^-#.!?]*)? \[(§[^\]]+)\]", l)
            if match:
                for identifier, _, _ in re.findall(ID_REGEX, match.group(2)):
                    req = Requirement(identifier, match.group(1))
                    if req.identifier in requirements or any(
                        identifier == req.identifier for identifier in requirements
                    ):
                        print(f'duplicate requirement "{req.identifier}"')
                        error = True
                    requirements[req.identifier] = req

                    if "." in req.identifier:
                        parent = req.identifier.rsplit(".", 1)[0]
                        if parent in requirements:
                            requirements[parent].requirements.append(req)
                        else:
                            print(f'missing requirement "{parent}" for "{req.identifier}"')
                            error = True
            else:
                print(f'ignored "{l}"')

    if error:
        return None

    return list(requirements.values())


def search_references(directories: Sequence[Path]) -> List[str]:
    references: List[str] = []

    for d in directories:
        for f in d.glob("**/*.py"):
            with open(f) as fd:
                content = fd.read()
                references.extend(r for r, _, _ in re.findall(ID_REGEX, content))

    return references


def check_references(requirements: List[Requirement], references: Sequence[str]) -> bool:
    error = False

    for req in requirements:
        if req.identifier in references:
            if not req.requirements:
                req.referenced = True
            else:
                print(f'meta requirement "{req.identifier}" must not be referenced')
                error = True

    undefined = [
        ref
        for ref in references
        if all(ref != req.identifier and ref not in req for req in requirements)
    ]
    if undefined:
        for ref in undefined:
            print(f'reference to undefined requirement "{ref}"')
        error = True

    for r in [r for r in requirements if not r.referenced]:
        print(f'unreferenced requirement "{r.identifier} {r.description}"')
        error = True

    return error


def print_checkbox_list(requirements: Sequence[Requirement]) -> None:
    print()
    for r in sorted(requirements):
        indentation = "    " * r.identifier.count(".")
        status = "x" if r.referenced else " "
        print(f"{indentation}- [{status}] {r.description}")


if __name__ == "__main__":
    sys.exit(main(sys.argv))
