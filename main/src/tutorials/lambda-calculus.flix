//
// In this tutorial we will write a interpreter for the lambda calculus!
//

///
/// The expressions of the lambda calculus are: variables, lambda abstractions, and applications.
///
enum Expression {
    // a variable expression.
    case Var(Int),

    // a lambda abstraction expression.
    case Abs(Int, Expression),

    // a function application expression.
    case App(Expression, Expression)
}

///
/// The evaluation contexts of lambda calculus describe the location of the next redex.
///
enum EvaluationContext {
    // represents the redex in the context.
    case Hole,

    // represents an application e1 e2 where evaluation should continue in e1.
    case App1(EvaluationContext, Expression),

    // represents an application v e where evaluation should continue in e2.
    case App2(Expression, EvaluationContext)
}

///
/// Returns `true` if the given expression `e` is a value.
///
def isValue(e: Expression): Bool = match e with {
    case Abs(x, e) => true
    case _         => false
}

///
/// Returns `true` if the given expression `e` is not a value.
///
def nonValue(e: Expression): Bool = !isValue(e)

///
/// Optionally returns a pair of the next redex: the expression to reduce and its evaluation context.
///
def redex(e: Expression): Option[(Expression, EvaluationContext)] = match e with {
    case Var(x) => None
    case Abs(x, e) => None
    // TODO: Add if guards to case statements?
    case App(e1, e2) =>
        // TODO: Add syntactic sugar for paired functions?
        if (nonValue(e1))
            match redex(e1) with {
                case None => None
                case Some((e, ec)) => Some((e, App1(ec, e2)))
            }
        else {
            if (isValue(e2))
                Some(e, Hole)
            else {
                match redex(e2) with {
                    case None => None
                    case Some((e, ec)) => Some((e, App2(e1, ec)))
                }
            }
        }
}

///
/// Returns an expression reconstructed from the given evaluation context `ec0`
/// with the given expression `e0` replacing the hole in the context.
///
def recompose(ec0: EvaluationContext, e0: Expression): Expression = match ec0 with {
    case Hole => e0
    case App1(ec, e) => App(recompose(ec, e0), e)
    case App2(v, ec) => App(v, recompose(ec, e0))
}

///
/// Reduces a single contractum.
///
def reduce(e: Expression): Expression = match e with {
    case App(Abs(x, e1), e2) => subst(e1, x, alphaRename(e2, 0))
    case _ => e
}

///
/// Performs a one-step reduction of the given expression `e0`
/// by finding a redex, reducing it, and recomposing the expression.
///
def step(e0: Expression): Expression = match redex(e0) with {
    case None => e0
    case Some((e, ec)) => recompose(ec, reduce(e0))
}


def subst(e: Expression, x: Int, v: Expression): Expression = match e with {
    case Var(y) =>  ???
    case App(e1, e2) => ???
//    case Lam(y, e) =>  ???
}

def alphaRename(e: Expression, c: Int): Expression = match e with {
    case Var(x) => ???
}


def main: Expression = {
    step(App(
         Abs(0, Var(0)),
         Abs(1, Var(1))
        ))
}

// TODO: Variable naming with e and e0