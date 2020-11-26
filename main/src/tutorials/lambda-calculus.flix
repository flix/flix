//
// In this tutorial we will write a interpreter for the lambda calculus!
//
// The interpreter will be based on evaluation contexts and operate in a redex-reduce-recompose loop.
//

///
/// The expressions of the lambda calculus are: variables, lambda abstractions, and applications.
///
enum Expression {
    // A variable expression. A variable is represented by an integer.
    case Var(Int),

    // A lambda abstraction expression. A variable is represented by an integer.
    case Abs(Int, Expression),

    // A function application expression.
    case App(Expression, Expression)
}

///
/// The evaluation contexts of lambda calculus describe the location of the next redex,
/// i.e. the next sub-expression which we can reduce in one-step.
///
enum EvaluationContext {
    // Represents the hole in the context.
    case Hole,

    // Represents the context: E e.
    case EApp1(EvaluationContext, Expression),

    // Represents the context: v E.
    case EApp2(Expression, EvaluationContext)
}

///
/// Returns `true` if the given expression `e0` is a value.
///
def isValue(e0: Expression): Bool = match e0 {
    case Abs(x, e) => true
    case _         => false
}

///
/// Returns `true` if the given expression `e0` is not a value.
///
def nonValue(e0: Expression): Bool = not isValue(e0)

///
/// Returns a pair of a redex and its evaluation context for the given expression `e0`.
///
/// The evaluation context is a representation of the expression "inside-out" allowing us to recompose
/// the expression after the redex has been reduced (contracted).
///
/// Returns the expression itself (and the empty evaluation context) if the expression is irreducible.
///
def redex(e0: Expression): (Expression, EvaluationContext) = match e0 {
    case Var(x) =>
        // A variable cannot be reduced.
        (Var(x), Hole)
    case Abs(x, e) =>
        // An abstraction cannot be reduced.
        (Abs(x, e), Hole)
    case App(e1, e2) => {
        if (isValue(e1) and isValue(e2))
            // An application where both arguments are values can be reduced.
            // Return the expression and the hole evaluation context.
            (e0, Hole)
        else if (nonValue(e1))
            // An application where the first argument is *NOT* a value.
            // Reduction should continue in the first argument.
            let (rdx, ec) = redex(e1);
                (rdx, EApp1(ec, e2))
        else
            // An application where the first argument is a value.
            // Reduction should continue in the second argument.
            let (rdx, ec) = redex(e1);
                (rdx, EApp2(e1, ec))
    }
}

///
/// Returns an expression reconstructed from the given evaluation context `ec0`
/// with the given expression `e0` replacing the hole in the context.
///
def recompose(e0: Expression, ec0: EvaluationContext): Expression = match ec0 {
    case Hole         => e0
    case EApp1(ec, e) => App(recompose(e0, ec), e)
    case EApp2(v, ec) => App(v, recompose(e0, ec))
}

///
/// Performs a single reduction step.
///
def step(e0: Expression): Expression =
    let (rdx, ec) = redex(e0);
    match rdx {
        case App(Abs(x, e1), e2) =>
            // Perform beta-reduction: Use alpha-renaming to avoid capture.
            recompose(substitute(e1, x, alpha(e2, Map.empty())), ec)
         case _ => recompose(rdx, ec)

    }

///
/// Replaces every occurrence of the variable `x` in the expression `e0` with the expression `r0`.
///
def substitute(e0: Expression, x: Int, r0: Expression): Expression = match e0 {
    case Var(y)      => if (x == y) r0 else Var(y)
    case Abs(y, e)   => if (x == y) Abs(y, e) else Abs(y, substitute(e, x, r0))
    case App(e1, e2) => App(substitute(e1, x, r0), substitute(e2, x, r0))
}

///
/// Performs alpha conversion by introducing fresh variables for all variables in the given expression `e0`.
///
def alpha(e0: Expression, m: Map[Int, Int]): Expression = match e0 {
    case Var(x) =>
        // Check if we need to rename the variable.
        match Map.get(x, m) {
            case None    => Var(x)
            case Some(y) => Var(y)
        }
    case Abs(x, e) =>
        // Generate a fresh variable name for `x`.
        let y = freshVar();
        Abs(y, alpha(e, Map.insert(x, y, m)))

    case App(e1, e2) =>
        // Recursively perform alpha conversion on each expression.
        App(alpha(e1, m), alpha(e2, m))
}

///
/// Repeatedly performs one-step reductions until the expression `e0` has been reduced to a value.
///
def eval(e0: Expression): Expression =
    let e = step(e0);
        if (isValue(e)) e else eval(e)


//
// We can now test our lambda interpreter!
//
def testEval1(): Expression = eval(Abs(0, Var(0)))                                // λx. x
def testEval2(): Expression = eval(Abs(0, Abs(1, Var(0))))                        // λx. λy. x
def testEval3(): Expression = eval(App(Abs(0, Var(0)), Abs(1, Var(1))))           // (λx. x) (λy. y)
def testEval4(): Expression = eval(App(Abs(0, Abs(1, Var(0))), Abs(2, Var(2))))   // (λx. λy. x) (λz. z)

//
// Here is the omega-combinator: (λx. x x) (λx. x x)
//
// If you run this, Flix will loop forever.
//
def testOmega(): Expression = eval(
    App(
        Abs(0, App(Var(0), Var(0))),
        Abs(0, App(Var(0), Var(0)))))


///
/// Returns a fresh variable name.
/// (Relies on an unsafe and impure built-in Flix function!).
///
def freshVar(): Int = ??? // TODO
