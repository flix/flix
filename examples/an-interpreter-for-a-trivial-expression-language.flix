//
// We define an adt to capture the syntax of arithmetic expressions.
//
enum AExp {
    // a literal integer constant.
    case Cst(Int),

    // a binary addition expression: e1 + e2.
    case Plus(AExp, AExp),

    // a binary subtraction expression: e1 - e2.
    case Minus(AExp, AExp),

    // a binary multiplication expression: e1 * e2.
    case Times(AExp, AExp),

    // a binary division expression: e1 / e2.
    case Divide(AExp, AExp),

    //n a if-then-else expression: if (e1) e2 else e3.
    case IfThenElse(BExp, AExp, AExp)
}

//
// We then define an adt to capture the syntax of boolean expressions.
//
enum BExp {
    // the true boolean literal.
    case True,

    // the false boolean literal.
    case False,

    // a logical negation expression: !e.
    case Not(BExp),

    // a logical conjunction expression: e1 and e2.
    case Conj(BExp, BExp),

    // a logical disjunction expression: e1 or e2.
    case Disj(BExp, BExp),

    // an equality of expression: e1 == e2.
    case Eq(AExp, AExp),

    // an inequality of expression: e1 != e2.
    case Neq(AExp, AExp)
}

//
// We now define a small interpreter for arithmetic expressions.
//
def evalAExp(e: AExp): Int = match e {
    case Cst(i)                 => i
    case Plus(e1, e2)           => evalAExp(e1) + evalAExp(e2)
    case Minus(e1, e2)          => evalAExp(e1) - evalAExp(e2)
    case Times(e1, e2)          => evalAExp(e1) * evalAExp(e2)
    case Divide(e1, e2)         => evalAExp(e1) / evalAExp(e2)
    case IfThenElse(e1, e2, e3) =>
        let cond = evalBExp(e1);
            if (cond) evalAExp(e2) else evalAExp(e3)
}

//
// and here is the small interpreter for boolean expressions.
//
def evalBExp(e: BExp): Bool = match e {
    case True           => true
    case False          => false
    case Not(e1)        => not evalBExp(e1)
    case Conj(e1, e2)   => evalBExp(e1) and evalBExp(e2)
    case Disj(e1, e2)   => evalBExp(e1) or evalBExp(e2)
    case Eq(e1, e2)     => evalAExp(e1) == evalAExp(e2)
    case Neq(e1,e2)     => evalAExp(e1) != evalAExp(e2)
}


// We can now run it!
def main(_args: Array[String]): Int32 & Impure =
    let r = evalAExp(IfThenElse(Neq(Cst(1), Cst(2)), Cst(42), Cst(21)));
    r |> println;
    0 // exit code
