#! /usr/bin/env python3

import re
import unittest
import argparse
import sys

def split_outside_brackets(s):
    """
    Splits a string by whitespace except parts enclosed in square brackets.
    Example:
        'A B [one two] C [three four]'
        -> ['A', 'B', '[one two]', 'C', '[three four]']
    """
    pattern = r'\[[^\]]*\]|[^\s]+'
    return re.findall(pattern, s)

def split_by_statement_terminator(s):
    """
    Splits a string by the statement terminator: a comma or semicolon.
    Example:
        'logic  a ; // abcd'
        -> ('logic a ', ' // abcd')
    """
    x = s.split(";",  1)
    if len(x) == 2: return (x[0], ";", x[1])
    x = s.split(",", 1)
    if len(x) == 2: return (x[0], ";", x[1])
    return (s, "", "")

def split_by_column(s):
    if not s:
        return ("", "", "", "")

    unpacked = ""
    identifier = ""
    packed = ""
    data_type = ""

    dimension = True
    x = ""
    y = split_outside_brackets(s)
    for i in reversed(y):
        if dimension and i[0] != "[":
            if identifier:
                packed = x
            else:
                unpacked = x
            x = ""
            dimension = False
        if not dimension and not identifier:
            identifier = i
            x = ""
            dimension = True
            continue
        if dimension:
            x = f"{i}{x}"
        elif x:
            x = f"{i} {x}"
        else:
            x = i

    data_type = x

    return (data_type, packed, identifier, unpacked)

class SplitStatement():

    def __init__(self, s):
        self.statement = s
        self.data_type = ""
        self.packed = ""
        self.identifier = ""
        self.unpacked = ""
        self.terminator = ""
        self.comment = ""
        self._parse_statement()

    def _parse_statement(self):
        x, y, z = split_by_statement_terminator(self.statement)
        self.comment = z
        self.terminator = y
        pieces = split_by_column(x)
        self.data_type = pieces[0]
        self.packed = pieces[1]
        self.identifier = pieces[2]
        self.unpacked = pieces[3]

class TestColumnSplit(unittest.TestCase):

    def test_basic(self):
        example = "Type1 Type2 [dim [one ]two][abc] Type3 [dim three] var;"
        self.assertEqual(split_by_column(example),
                       ('Type1 Type2 [dim [one ] two][abc] Type3', '[dim three]', 'var;', ''))

        example = "logic abc_t [123][324:32 4] [243] abc def; [abd][ghi]"
        self.assertEqual(split_by_column(example),
                         ('logic abc_t [123] [324:32 4] [243] abc', '', 'def;', '[abd][ghi]'))

        example = "logic abc_t [123][324:32 4] [243] def; [abd][ghi]"
        self.assertEqual(split_by_column(example),
                       ('logic abc_t', '[123][324:32 4][243]', 'def;', '[abd][ghi]'))

        example = "logic abc_t [1  23][324:32 4] de_f [abd] [ghi]"
        self.assertEqual(split_by_column(example),
                         ('logic abc_t', '[1  23][324:32 4]', 'de_f', '[abd][ghi]'))


def main():
    argparser = argparse.ArgumentParser()
    argparser.add_argument('--test', action='store_true', help="Run tests")
    args = argparser.parse_args()
    if args.test:
        unittest.main(argv=[''])
        return

    statements = []
    first_line = True
    indentation = ""

    for line in sys.stdin:
        space = re.match(r'^\s*', line)
        if first_line and space:
            indentation = space.group(0)
        line = line.strip()
        if not line:
            continue
        if len(line) >= 2 and line[0:2] == "//":
            s = SplitStatement("")
            s.comment = line
            statements.append(s)
        else:
            statements.append(SplitStatement(line))
        first_line = False

    max_t_and_p = max(len(s.data_type + s.packed) for s in statements)
    max_id = max(len(s.identifier) for s in statements)

    for s in statements:
        a = s.data_type
        b_width = max_t_and_p - len(a)
        b = f"{s.packed:>{b_width + 1}}"
        c = f" {s.identifier:<{max_id + 1}}"
        d = s.unpacked
        x = (a + b + c + d).strip()
        print(f"{indentation}{x}{s.terminator}{s.comment}")

if __name__ == "__main__":
    main()
