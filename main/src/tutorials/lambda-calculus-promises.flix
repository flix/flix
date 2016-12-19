//
// An extension of the lambda-calculus with promises.
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
    case App(Expression, Expression),

    //
    // General Extensions:
    //

    // A heap address.
    case Addr(Int),

    // A heap reference expression: ref e.
    case Ref(Expression),

    // A heap de-reference expression: deref e.
    case Deref(Expression),

    // A sequence expression: e1; e2.
    case Seq(Expression, Expression),

    //
    // Promises Extensions:
    //

    // A promisify expression: promisify(e).
    case Promisify(Expression),

    // A resolve expression: e1.resolve(e2).
    case Resolve(Expression, Expression),

    // A reject expression: e1.reject(e2).
    case Reject(Expression, Expression),

    // An onResolve expression: e1.onResolve(e2).
    case OnResolve(Expression, Expression),

    // An onReject expression: e1.onReject(e2).
    case OnReject(Expression, Expression),

    // A link expression: e1.link(e2).
    case Link(Expression, Expression)

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
    case EApp2(Expression, EvaluationContext),

    // Represents the context: ref E.
    case ERef(EvaluationContext),

    // Represents the context: deref E.
    case EDeref(EvaluationContext),

    // Represents the context: E ; e
    case ESeq1(EvaluationContext, Expression),

    // Represents the context: v ; E
    case ESeq2(Expression, EvaluationContext),

    // Represents the context: promisify(E).
    case EPromisify(EvaluationContext),

    // Represents the context: E.resolve(e).
    case EResolve1(EvaluationContext, Expression),

    // Represents the context: v.resolve(E).
    case EResolve2(Expression, EvaluationContext),

    // Represents the context: E.reject(e).
    case EReject1(EvaluationContext, Expression),

    // Represents the context: v.reject(E).
    case EReject2(Expression, EvaluationContext),

    // Represents the context: E.onResolve(e).
    case EOnResolve1(EvaluationContext, Expression),

    // Represents the context: v.onResolve(E).
    case EOnResolve2(Expression, EvaluationContext),

    // Represents the context: E.onReject(e).
    case EOnReject1(EvaluationContext, Expression),

    // Represents the context: v.onReject(E).
    case EOnReject2(Expression, EvaluationContext),

    // Represents the context: E.link(e).
    case ELink1(EvaluationContext, Expression),

    // Represents the context: v.link(E).
    case ELink2(Expression, EvaluationContext)
}


///
/// Returns `true` if the given expression `e0` is a value.
///
def isValue(e0: Expression): Bool = match e0 with {
    case Abs(x, e) => true
    case Addr(a)   => true
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

    // TODO: Data type like: Both(V, V), One(V), Zilch ?
    case App(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, EApp1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, EApp2(e1, ec)), redex(e2))
    }

    case Addr(a) =>
        // An address cannot be reduced.
        None

    case Ref(e) =>
        if (isValue(e))
            // The expression is a redex.
            Some((e0, Hole))
        else
            // The redex is in the expression e.
            Option/map(match (rdx, ec) -> (rdx, ERef(ec)), redex(e))

    case Deref(e) =>
        if (isValue(e))
            // The expression is a redex.
            Some((e0, Hole))
        else
            // The redex is in the expression e.
            Option/map(match (rdx, ec) -> (rdx, EDeref(ec)), redex(e))

    case Seq(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, ESeq1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, ESeq2(e1, ec)), redex(e2))
    }

    case Promisify(e) =>
        if (isValue(e))
            // The expression is a redex.
            Some((e0), Hole)
        else
            // The redex is in the expression e.
            Option/map(match (rdx, ec) -> (rdx, EPromisify(ec)), redex(e))

    case Resolve(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, EResolve1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, EResolve2(e1, ec)), redex(e2))
    }

    case Reject(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, EReject1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, EReject2(e1, ec)), redex(e2))
    }

    case OnResolve(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, EOnResolve1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, EOnResolve2(e1, ec)), redex(e2))
    }

    case OnReject(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, EOnReject1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, EOnReject2(e1, ec)), redex(e2))
    }

    case Link(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            Some((e0, Hole))
        case nonValue(e1) =>
            // Continue reduction in e1.
            Option/map(match (rdx, ec) -> (rdx, ELink1(ec, e2)), redex(e1))
        case isValue(e1) =>
            // Continue reduction in e2.
            Option/map(match (rdx, ec) -> (rdx, ELink2(e1, ec)), redex(e2))
    }

}

///
/// Returns an expression reconstructed from the given evaluation context `ec0`
/// with the given expression `e0` replacing the hole in the context.
///
def recompose(ec0: EvaluationContext, e0: Expression): Expression = match ec0 with {
    case Hole               => e0
    case EApp1(ec, e)       => App(recompose(ec, e0), e)
    case EApp2(v, ec)       => App(v, recompose(ec, e0))
    case ERef(ec)           => Ref(recompose(ec, e0))
    case EDeref(ec)         => Deref(recompose(ec, e0))
    case ESeq1(ec, e)       => Seq(recompose(ec, e0), e)
    case ESeq2(e, ec)       => Seq(e, recompose(ec, e0))
    case EPromisify(ec)     => Promisify(recompose(ec, e0))
    case EResolve1(ec, e)   => Resolve(recompose(ec, e0), e)
    case EResolve2(e, ec)   => Resolve(e, recompose(ec, e0))
    case EReject1(ec, e)    => Reject(recompose(ec, e0), e)
    case EReject2(e, ec)    => Reject(e, recompose(ec, e0))
    case EOnResolve1(ec, e) => OnResolve(recompose(ec, e0), e)
    case EOnResolve2(e, ec) => OnResolve(e, recompose(ec, e0))
    case EOnReject1(ec, e)  => OnReject(recompose(ec, e0), e)
    case EOnReject2(e, ec)  => OnReject(e, recompose(ec, e0))
    case ELink1(ec, e)      => Link(recompose(ec, e0), e)
    case ELink2(e, ec)      => Link(e, recompose(ec, e0))
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
    case Var(y)             => if (x == y) r0 else Var(y)
    case Abs(y, e)          => if (x == y) Abs(y, e) else Abs(y, substitute(e, x, r0))
    case App(e1, e2)        => App(substitute(e1, x, r0), substitute(e2, x, r0))

    case Addr(a)            => Addr(a)
    case Ref(e)             => Ref(substitute(e, x, r0))
    case Deref(e)           => Deref(substitute(e, x, r0))
    case Seq(e1, e2)        => Seq(substitute(e1, x, r0), substitute(e2, x, r0))

    case Promisify(e)       => Promisify(substitute(e, x, r0))
    case Resolve(e1, e2)    => Resolve(substitute(e1, x, r0), substitute(e2, x, r0))
    case Reject(e1, e2)     => Reject(substitute(e1, x, r0), substitute(e2, x, r0))
    case OnResolve(e1, e2)  => OnResolve(substitute(e1, x, r0), substitute(e2, x, r0))
    case OnReject(e1, e2)   => OnReject(substitute(e1, x, r0), substitute(e2, x, r0))
    case Link(e1, e2)       => Link(substitute(e1, x, r0), substitute(e2, x, r0))
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
        Abs(y, alpha(e, Map/insert(x, y, m)))
    case App(e1, e2) => App(alpha(e1, m), alpha(e2, m))

    case Addr(a)            => Addr(a)
    case Ref(e)             => Ref(alpha(e, m))
    case Deref(e)           => Deref(alpha(e, m))
    case Seq(e1, e2)        => Seq(alpha(e1, m), alpha(e2, m))

    case Promisify(e)       => Promisify(alpha(e, m))
    case Resolve(e1, e2)    => Resolve(alpha(e1, m), alpha(e2, m))
    case Reject(e1, e2)     => Reject(alpha(e1, m), alpha(e2, m))
    case OnResolve(e1, e2)  => OnResolve(alpha(e1, m), alpha(e2, m))
    case OnReject(e1, e2)   => OnReject(alpha(e1, m), alpha(e2, m))
    case Link(e1, e2)       => Link(alpha(e1, m), alpha(e2, m))
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
/// Repeatedly performs one-step reductions until the expression `e0` has been reduced to a value.
///
def eval(e0: Expression): Expression =
    let e = step(e0);
        if (isValue(e)) e else eval(e)


//
// We can now test our lambda interpreter!
//
def testEval1: Expression = eval(Abs(0, Var(0)))                                // λx. x
def testEval2: Expression = eval(Abs(0, Abs(1, Var(0))))                        // λx. λy. x
def testEval3: Expression = eval(App(Abs(0, Var(0)), Abs(1, Var(1))))           // (λx. x) (λy. y)
def testEval4: Expression = eval(App(Abs(0, Abs(1, Var(0))), Abs(2, Var(2))))   // (λx. λy. x) (λz. z)

///
/// Returns a fresh variable name.
/// (Relies on an unsafe and impure built-in Flix function!).
///
def freshVar: Int = genSym!()






//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
enum Map2[k, v] {
    case Map2(List[(k, v)])
}
namespace Map {
    def empty: Map2[Int, Int] = Map2(Nil)

    def insert(k: Int, v: Int, m: Map2[Int, Int]): Map2[Int, Int] =
        let Map2(xs) = m;
            Map2((k, v) :: xs)

    def get[k, v](k: k, m: Map2[k, v]): Option[v] =
        let Map2(xs) = m;
            getHelper(k, xs)

    def getHelper[k, v](k: k, xs: List[(k, v)]): Option[v] = match xs with {
        case Nil => None
        case (y, v) :: ys => if (k == y) Some(v) else getHelper(k, ys)
    }

}
