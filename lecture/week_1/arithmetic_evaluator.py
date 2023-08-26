#!/usr/bin/env python3
#
# ---AST Definition---
#
# There are four kinds of expressions:
#
# 1.) Numbers, which represent a numeric literal.  These are
#     represented with the `Number` class, which has a `value`
#     field holding the value of the number.
#
# 2.) Arithmetic addition, which represents the idea of adding
#     two subexpressions.  This is represented with the `Plus`
#     class, which has `left` and `right` fields for the
#     subexpressions.
#
# 3.) Arithmetic subtraction, which represents the idea of subtracting
#     one subexpression from another.  This is represented with the
#     `Minus` class, which has `left` and `right` fields for the
#     subexpressions.
#
# 4.) Arithmetic multiplication, which represents the idea of multiplying
#     two subexpressions together.  This is represented with the
#     `Multiply` class, which has `left` and `right` fields for the
#     subexpressions.
#
# 5.) Arithmetic negation, which represents the idea of negating a
#     subexpression.  This is represented with the `Negate` class,
#     which has a `sub_expr` field for the subexpression
#
# A more compact representation of all the above information is shown
# below in a variant of a BNF grammar:
#
# n ∈ PythonNumber
# e ∈ Expression ::= Number(n) | Plus(e1, e2) | Minus(e1, e2) |
#                    Multiply(e1, e2) | Negate(e)

class Number:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

class Binop:
    def __init__(self, left, right, op_string):
        self.left = left
        self.right = right
        self.op_string = op_string
        
    def __str__(self):
        return "({} {} {})".format(
            str(self.left),
            self.op_string,
            str(self.right))

class Plus(Binop):
    def __init__(self, left, right):
        super().__init__(left, right, "+")
    
class Minus(Binop):
    def __init__(self, left, right):
        super().__init__(left, right, "-")

class Multiply(Binop):
    def __init__(self, left, right):
        super().__init__(left, right, "*")

class Negate:
    def __init__(self, sub_expr):
        self.sub_expr = sub_expr

    def __str__(self):
        return "(-{})".format(str(self.sub_expr))


def eval_expr(e):
    if isinstance(e, Number):
        return e.value
    elif isinstance(e, Plus):
        return eval_expr(e.left) + eval_expr(e.right)
    elif isinstance(e, Minus):
        return eval_expr(e.left) - eval_expr(e.right)
    elif isinstance(e, Multiply):
        return eval_expr(e.left) * eval_expr(e.right)
    elif isinstance(e, Negate):
        return -eval_expr(e.sub_expr)
    else:
        raise Exception("Unknown expression: " + e)

tests = [(Number(1), 1),                                             # 1
         (Plus(Number(1), Number(2)), 3),                            # 1 + 2
         (Minus(Number(3), Number(1)), 2),                           # 3 - 1
         (Multiply(Number(3), Number(4)), 12),                       # 3 * 4
         (Negate(Number(1)), -1),                                    # -1
         (Negate(Plus(Number(2), Minus(Number(3), Number(4)))), -1)] # -(2 + (3 - 4))

def test_result_ok(test, expected_result):
    return eval_expr(test) == expected_result

def run_tests():
    tests_failed = False
    for (test, expected_result) in tests:
        received_result = eval_expr(test)
        if expected_result != received_result:
            print("Failed: {}".format(test))
            print("\tExpected: {}".format(expected_result))
            print("\tReceived: {}".format(received_result))
            tests_failed = True
    if not tests_failed:
        print("All tests passed")

if __name__ == "__main__":
    run_tests()



