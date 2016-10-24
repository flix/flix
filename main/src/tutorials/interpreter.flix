///
/// In this tutorial we demonstrate the functional language in Flix.
///
/// We will write some interpreters and compilers for a small toy programming language.
///
/// We begin by defining the data types we will be working with.
///

///
/// We define an enum to capture the syntax of arithmetic expressions.
///
enum AExp {
    // a literal integer constant.
    case Cst(Int),

    // a binary addition expression: e1 + e2.
    case Plus(AExp, AExp),

    // a binary substraction expression: e1 - e2.
    case Minus(AExp, AExp),

    // a binary multiplication expression: e1 * e2.
    case Times(AExp, AExp),

    // a binary division expression: e1 / e2.
    case Divide(AExp, AExp),

    // a if-then-else expression: if (e1) e2 else e3.
    case IfThenElse(BExp, AExp, AExp)
}

///
/// We then define an enum to capture the syntax of boolean expressions.
///
enum BExp {
    // the true boolean literal.
    case True,

    // the false boolean literal.
    case False,

    // a logical negation expression: !e.
    case Not(BExp),

    // a logical conjunction expression: e1 && e2.
    case Conj(BExp, BExp),

    // a logical disjunction expression: e1 || e2.
    case Disj(BExp, BExp),

    // an equality of expression: e1 == e2.
    case Eq(AExp, AExp),

    // an inequality of expression: e1 != e2.
    case NEq(AExp, AExp)
}

///
/// We can now define a small interpreter for arithmetic expressions.
///
def evalAExp(e: AExp): Int = match e with {
    case Cst(i)                 => i
    case Plus(e1, e2)           => evalAExp(e1) + evalAExp(e2)
    case Minus(e1, e2)          => evalAExp(e1) - evalAExp(e2)
    case Times(e1, e2)          => evalAExp(e1) * evalAExp(e2)
    case Divide(e1, e2)         => evalAExp(e1) / evalAExp(e2)
    case IfThenElse(e1, e2, e3) => ??? // TODO
}

///
/// And here is the small interpreter for boolean expressions.
///


///
/// We test each interpreter by writing some small test.
///
/// You can run these functions by passing the `--main` argument to Flix, e.g.
///   $ flix --main testEvalAExp1
///

def testEvalAExp1: Int = evalAExp(Cst(42))
def testEvalAExp2: Int = evalAExp(Cst(42))
def testEvalAExp3: Int = evalAExp(Cst(42))
def testEvalAExp4: Int = evalAExp(Cst(42))
def testEvalAExp5: Int = evalAExp(Cst(42))

