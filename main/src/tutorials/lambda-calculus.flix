//
// In this tutorial we will write a simple interpreter for the lambda calculus!
//
// The interpreter will be expressed as a reduction semantics using evaluation contexts.
//

///
/// The type of expressions:
///
enum Expr {
    case Var(Int),
    case App(Expr, Expr),
    case Lam(Int, Expr)
}

///
/// The type of evaluation contexts:
///
enum EC {
    case Hole,
    case App1(EC, Expr),
    case App2(Expr, EC)
}

///
/// Returns `true` if the given expression `e` is a value.
///
/// The values of the calculus are lambda abstractions.
///
def isValue(e: Expr): Bool = match e with {
    case Lam(x, e) => true
    case _ => false
}



def reduce(e: Expr): Expr = match e with {
    case App(Lam(x, e1), e2) => if (isValue(e2)) subst(e1, x, e2) else e
    case _ => e
}

def subst(e: Expr, x: Str, v: Expr): Expr = match e with {
    case Var(y) =>  ???
    case App(e1, e2) => ???
    case Lam(y, e) =>  ???
}

def alphaRename(e: Expr, c: Int): Expr = match e with {
    case Var(x) =>
}
