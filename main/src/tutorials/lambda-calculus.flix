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
    // Represents the redex in the context.
    case Hole,

    // Represents an application e1 e2 where evaluation should continue in e1.
    case App1(EvaluationContext, Expression),

    // Represents an application v e where evaluation should continue in e2.
    case App2(Expression, EvaluationContext)
}

///
/// Returns `true` if the given expression `e0` is a value.
///
def isValue(e0: Expression): Bool = match e0 with {
    case Abs(x, e) => true
    case _         => false
}

///
/// Returns `true` if the given expression `e0` is not a value.
///
def nonValue(e0: Expression): Bool = !isValue(e0)

///
/// Optionally returns a pair of a redex and its evaluation context for the given expression `e0`.
///
/// The evaluation context is a representation of the expression "inside-out" allowing us to recompose
/// the expression after the redex has been reduced (contracted).
///
def redex(e0: Expression): Option[(Expression, EvaluationContext)] = match e0 with {
    case Var(x) =>
        // A variable cannot be reduced.
        None
    case Abs(x, e) =>
        // An abstraction cannot be reduced.
        None
    case App(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // An application where both arguments are values can be reduced.
            // Return the expression and the hole evaluation context.
            Some((e0, Hole))
        case nonValue(e1) =>
            // An application where the first argument is *NOT* a value.
            // Reduction should continue in the first argument.
            match redex(e1) with {
                case None => None
                case Some((e, ec)) =>
                    // The first argument has a redex `e` in evaluation context `ec`.
                    // Extend the evaluation context with `e2`.
                    Some((e, App1(ec, e2)))
            }
        case isValue(e1) =>
            // An application where the first argument is a value.
            // Reduction should continue in the second argument.
            match redex(e2) with {
                case None => None
                case Some((e, ec)) =>
                    // The second argument has a redex `e` in evaluation context `ec`.
                    // Extend the evaluation context with `e1`.
                    Some((e, App2(e1, ec)))
            }
    }
}

///
/// Returns an expression reconstructed from the given evaluation context `ec0`
/// with the given expression `e0` replacing the hole in the context.
///
def recompose(ec0: EvaluationContext, e0: Expression): Expression = match ec0 with {
    case Hole        => e0
    case App1(ec, e) => App(recompose(ec, e0), e)
    case App2(v, ec) => App(v, recompose(ec, e0))
}

///
/// Reduces the redex `e0` with a single application of beta-reduction.
///
def reduce(e: Expression): Expression = match e with {
    case App(Abs(x, e1), e2) =>
        // Perform beta-reduction: Use alpha-renaming to avoid capture.
        substitute(e1, x, alpha(e2, Map/empty()))
    case _ => e
}

///
/// Replaces every occurrence of the variable `x` in the expression `e0` with the expression `r0`.
///
def substitute(e0: Expression, x: Int, r0: Expression): Expression = match e0 with {
    case Var(y)      => if (x == y) r0 else Var(y)
    case Abs(y, e)   => if (x == y) Abs(y, e) else Abs(y, substitute(e, x, r0))
    case App(e1, e2) => App(substitute(e1, x, r0), substitute(e2, x, r0))
}

///
/// Performs alpha conversion by introducing fresh variables for all variables in the given expression `e0`.
///
def alpha(e0: Expression, m: Map2[Int, Int]): Expression = match e0 with {
    case Var(x) =>
        // Check if we need to rename the variable.
        match Map/get(x, m) with {
            case None    => Var(x)
            case Some(y) => Var(y)
        }
    case Abs(x, e) =>
        // Generate a fresh variable name for `x`.
        let y = freshVar();
        Abs(y, alpha(e, Map/insert(y, x, m)))

    case App(e1, e2) =>
        // Recursively perform alpha conversion on each expression.
        App(alpha(e1, m), alpha(e2, m))
}

///
/// Performs a single step reduction of the given expression `e0`,
/// i.e. performs a single redex-reduce-recompose step.
///
def step(e0: Expression): Expression = match redex(e0) with {
    case None => e0
    case Some((e, ec)) => recompose(ec, reduce(e0))
}

///
/// Repeatedly performs one-step reductions until a fixed point.
///
def loop(e0: Expression): Expression =
    let e = step(e0);
        if (e == e0) e else loop(e)


//
// We can now test our lambda interpreter!
//
def testLoop1: Expression = loop(Var(0))                                        // x
def testLoop2: Expression = loop(Abs(0, Var(0)))                                // λx. x
def testLoop3: Expression = loop(Abs(0, Abs(1, Var(0))))                        // λx. λy. x
def testLoop4: Expression = loop(App(Abs(0, Var(0)), Abs(1, Var(1))))           // (λx. x) (λy. y)
def testLoop5: Expression = loop(App(Abs(0, Abs(1, Var(0))), Abs(2, Var(2))))   // (λx. λy. x) (λz. z)

//
// Here is the omega-combinator: (λx. x x) (λx. x x)
//
// If you run this, Flix will loop forever.
//
def testOmega: Expression = loop(
    App(
        Abs(0, App(Var(0), Var(0))),
        Abs(0, App(Var(0), Var(0)))))



///
/// Returns a fresh variable name.
/// (Relies on an unsafe and impure built-in Flix function!).
///
def freshVar: Int = ???


// TODO: replace by library maps.
enum Map2[k, v] {
    case Map2(List[(k, v)])
}
namespace Map {
    def empty: Map2[Int, Int] = Map2(Nil)

    def insert(k: Int, v: Int, m: Map2[Int, Int]): Map2[Int, Int] = ???

    def get[k, v](k: k, m: Map2[k, v]): Option[v] =  ???

    def getHelper[k, v](xs: List[k, v]): Option[v] = ???
}