///
/// We define the syntax of arithmetic expression.
///
enum AExp {
    /// a literal integer constant.
    case Cst(Int32),

    /// a binary addition expression: e1 + e2.
    case Plus(AExp, AExp),

    /// a binary subtraction expression: e1 - e2.
    case Minus(AExp, AExp),

    /// a binary multiplication expression: e1 * e2.
    case Times(AExp, AExp),

    /// a binary division expression: e1 / e2.
    case Divide(AExp, AExp),

    //n a if-then-else expression: if (e1) e2 else e3.
    case IfThenElse(BExp, AExp, AExp)
}

///
/// We then define the syntax of Boolean expressions.
///
enum BExp {
    /// the true boolean literal.
    case True,

    /// the false boolean literal.
    case False,

    /// a logical negation expression: !e.
    case Not(BExp),

    /// a logical conjunction expression: e1 and e2.
    case Conj(BExp, BExp),

    /// a logical disjunction expression: e1 or e2.
    case Disj(BExp, BExp),

    /// an equality of expression: e1 == e2.
    case Eq(AExp, AExp),

    /// an inequality of expression: e1 != e2.
    case Neq(AExp, AExp)
}

///
/// We now define a small interpreter for arithmetic expressions.
///
def evalAExp(e: AExp): Int32 = match e {
    case AExp.Cst(i)                 => i
    case AExp.Plus(e1, e2)           => evalAExp(e1) + evalAExp(e2)
    case AExp.Minus(e1, e2)          => evalAExp(e1) - evalAExp(e2)
    case AExp.Times(e1, e2)          => evalAExp(e1) * evalAExp(e2)
    case AExp.Divide(e1, e2)         => evalAExp(e1) / evalAExp(e2)
    case AExp.IfThenElse(e1, e2, e3) =>
        let cond = evalBExp(e1);
            if (cond) evalAExp(e2) else evalAExp(e3)
}

///
/// and here is the small interpreter for Boolean expressions.
///
def evalBExp(e: BExp): Bool = match e {
    case BExp.True           => true
    case BExp.False          => false
    case BExp.Not(e1)        => not evalBExp(e1)
    case BExp.Conj(e1, e2)   => evalBExp(e1) and evalBExp(e2)
    case BExp.Disj(e1, e2)   => evalBExp(e1) or evalBExp(e2)
    case BExp.Eq(e1, e2)     => evalAExp(e1) == evalAExp(e2)
    case BExp.Neq(e1,e2)     => evalAExp(e1) != evalAExp(e2)
}

/// We can now run it!
def main(): Unit \ IO =
    let r = evalAExp(
        AExp.IfThenElse(BExp.Neq(AExp.Cst(1), AExp.Cst(2)), AExp.Cst(42), AExp.Cst(21))
    );
    r |> println
