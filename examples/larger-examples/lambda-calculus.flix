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
    case Var(Int32),

    // A lambda abstraction expression. A variable is represented by an integer.
    case Abs(Int32, Expression),

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
    case Expression.Abs(_, _) => true
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
    case Expression.Var(x) =>
        // A variable cannot be reduced.
        (Expression.Var(x), EvaluationContext.Hole)
    case Expression.Abs(x, e) =>
        // An abstraction cannot be reduced.
        (Expression.Abs(x, e), EvaluationContext.Hole)
    case Expression.App(e1, e2) => {
        if (isValue(e1) and isValue(e2))
            // An application where both arguments are values can be reduced.
            // Return the expression and the hole evaluation context.
            (e0, EvaluationContext.Hole)
        else if (nonValue(e1))
            // An application where the first argument is *NOT* a value.
            // Reduction should continue in the first argument.
            let (rdx, ec) = redex(e1);
                (rdx, EvaluationContext.EApp1(ec, e2))
        else
            // An application where the first argument is a value.
            // Reduction should continue in the second argument.
            let (rdx, ec) = redex(e1);
                (rdx, EvaluationContext.EApp2(e1, ec))
    }
}

///
/// Returns an expression reconstructed from the given evaluation context `ec0`
/// with the given expression `e0` replacing the hole in the context.
///
def recompose(e0: Expression, ec0: EvaluationContext): Expression = match ec0 {
    case EvaluationContext.Hole         => e0
    case EvaluationContext.EApp1(ec, e) => Expression.App(recompose(e0, ec), e)
    case EvaluationContext.EApp2(v, ec) => Expression.App(v, recompose(e0, ec))
}

///
/// Performs a single reduction step.
///
def step(e0: Expression): Expression =
    let (rdx, ec) = redex(e0);
    match rdx {
        case Expression.App(Expression.Abs(x, e1), e2) =>
            // Perform beta-reduction: Use alpha-renaming to avoid capture.
            recompose(substitute(e1, x, alpha(e2, Map.empty())), ec)
         case _ => recompose(rdx, ec)

    }

///
/// Replaces every occurrence of the variable `x` in the expression `e0` with the expression `r0`.
///
def substitute(e0: Expression, x: Int32, r0: Expression): Expression = match e0 {
    case Expression.Var(y)      => if (x == y) r0 else Expression.Var(y)
    case Expression.Abs(y, e)   => if (x == y) Expression.Abs(y, e) else Expression.Abs(y, substitute(e, x, r0))
    case Expression.App(e1, e2) => Expression.App(substitute(e1, x, r0), substitute(e2, x, r0))
}

///
/// Performs alpha conversion by introducing fresh variables for all variables in the given expression `e0`.
///
def alpha(e0: Expression, m: Map[Int32, Int32]): Expression = match e0 {
    case Expression.Var(x) =>
        // Check if we need to rename the variable.
        match Map.get(x, m) {
            case None    => Expression.Var(x)
            case Some(y) => Expression.Var(y)
        }
    case Expression.Abs(x, e) =>
        // Generate a fresh variable name for `x`.
        let y = freshVar();
        Expression.Abs(y, alpha(e, Map.insert(x, y, m)))

    case Expression.App(e1, e2) =>
        // Recursively perform alpha conversion on each expression.
        Expression.App(alpha(e1, m), alpha(e2, m))
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
def _testEval1(): Expression = eval(Expression.Abs(0, Expression.Var(0)))                                // λx. x
def _testEval2(): Expression = eval(Expression.Abs(0, Expression.Abs(1, Expression.Var(0))))                        // λx. λy. x
def _testEval3(): Expression = eval(Expression.App(Expression.Abs(0, Expression.Var(0)), Expression.Abs(1, Expression.Var(1))))           // (λx. x) (λy. y)
def _testEval4(): Expression = eval(Expression.App(Expression.Abs(0, Expression.Abs(1, Expression.Var(0))), Expression.Abs(2, Expression.Var(2))))   // (λx. λy. x) (λz. z)

//
// Here is the omega-combinator: (λx. x x) (λx. x x)
//
// If you run this, Flix will loop forever.
//
def _testOmega(): Expression = eval(
    Expression.App(
        Expression.Abs(0, Expression.App(Expression.Var(0), Expression.Var(0))),
        Expression.Abs(0, Expression.App(Expression.Var(0), Expression.Var(0)))))


///
/// Returns a fresh variable name.
/// (Relies on an unsafe and impure built-in Flix function!).
///
def freshVar(): Int32 = ??? // TODO
