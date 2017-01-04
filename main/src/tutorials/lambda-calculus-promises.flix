//
// An extension of the lambda-calculus with promises.
//

// TODO: Introduce Address constructor.

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

    // A constant string value.
    case Cst(Str),

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
/// The store maps addresses to values.
///
enum Heap {
    case Heap(Map2[Int, Expression])
}

///
/// The promise state maps addresses to promise values.
///
enum PromiseState {
    case PromiseState(Map2[Int, PromiseValue])
}

///
/// The fulfill reactions map addresses to a list of (Lam, Addr)-pairs.
///
enum FulfillReactions {
    case FulfillReactions(Map2[Int, List[(Expression, Int)]])
}

///
/// The reject reactions map addresses to a list of (Lam, Addr)-pairs.
///
enum RejectReactions {
    case RejectReactions(Map2[Int, List[(Expression, Int)]])
}

///
/// The promise links map addresses to a list of addresses.
///
enum PromiseLinks {
    case PromiseLinks(Map2[Int, List[Int]])
}

///
/// The queue is a list of (PromiseValue, Lam, Addr)-triples.
///
enum Queue {
    case Queue(List[(PromiseValue, Expression, Int)])
}

///
/// A promise is in one of three states: Pending, fulfilled (with a value), or rejected (with a value).
///
enum PromiseValue {
    case Pending,
    case Fulfilled(Expression),
    case Rejected(Expression)
}

///
/// A machine configuration.
///
enum State {
    case State(Expression, Heap, PromiseState, FulfillReactions, RejectReactions, PromiseLinks, Queue)
}

///
/// Returns `true` if the given expression `e0` is a value.
///
def isValue(e0: Expression): Bool = match e0 with {
    case Abs(x, e)  => true
    case Cst(s)     => true
    case Addr(a)    => true
    case _          => false
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

    case Cst(s) =>
        // A string cannot be reduced.
        None

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
/// Performs a single reduction step in the given machine `m0`.
///
def reduce(m0: State): State = match m0 with {
    case State(App(Abs(x, e1), e2), h0, s0, f0, r0, l0, q0) =>
        // Apply beta-reduction: Use alpha-renaming to avoid capture.
        let e = substitute(e1, x, alpha(e2, Map/empty()));
            State(e, h0, s0, f0, r0, l0, q0)

    case State(Seq(v, e), h0, s0, f0, r0, l0, q0) =>
        // Discard the value in the sequence.
        State(e, h0, s0, f0, r0, l0, q0)

    case State(Ref(v), Heap(h0), s0, f0, r0, l0, q0) =>
        // Allocate a fresh address in the heap and store the value at that address.
        let a = freshAddr();
        let h1 = Map/insert(a, v, h0);
        let e1 = Addr(a);
            State(e1, Heap(h1), s0, f0, r0, l0, q0)

    case State(Deref(Addr(a)), Heap(h0), s0, f0, r0, l0, q0) =>
        // Dereference the address and return its value.
        match Map/lookup(a, h0) with {
            case None => m0
            case Some(v) => State(v, Heap(h0), s0, f0, r0, l0, q0)
        }

    case State(Promisify(Addr(a)), h0, PromiseState(s0), f0, r0, l0, q0) =>
        // Sets the state of an uninitialized promise to pending.
        match Map/lookup(a, s0) with {
            case None =>
                let s1 = Map/insert(a, Pending, s0);
                    State(Cst("Undef"), h0, PromiseState(s1), f0, r0, l0, q0)
            case Some(_) => m0
        }

    case State(Resolve(Addr(a), v), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0)) =>
        // Lookup the state of the promise.
        match Map/lookup(a, s0) with {
            case None =>
                // Case 1: The address is not a promise. The evaluation is stuck.
                m0
            case Some(Pending) =>
                // Case 2: The promise is pending.

                // Set the promise state to fulfilled.
                let s1 = Map/insert(a, Fulfilled(v), s0);

                // Clear out the fulfill reactions.
                let f1 = Map/insert(a, Nil, f0);

                // Clear out the rejected reactions.
                let r1 = Map/insert(a, Nil, r0);

                // Clear out the promise links.
                let l1 = Map/insert(a, Nil, l0);

                // Retrieve the fulfill reactions and append them to the queue.
                let rs = Map/getWithDefault(a, Nil, f0);
                let q1 = q0 ::: List/map(match (lam, addr) -> (Fulfilled(v), lam, addr), rs);
                    State(Cst("Undef"), Heap(h0), PromiseState(s1), FulfillReactions(f1), RejectReactions(r1), PromiseLinks(l1), Queue(q0))
            case Some(Fulfilled(v)) =>
                // Case 3: The promise has already been fulfilled with the value `v`. The resolve has no effect.
                State(Cst("Undef"), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0))
            case Some(Rejected(v)) =>
                // Case 4: The promise has already been rejected with the value `v`. The resolve has no effect.
                State(Cst("Undef"), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0))
        }

    case State(Reject(e1, e2), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0)) =>
        ??? // TODO: Similar to Resolve.

    case State(OnResolve(Addr(a), lam), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0)) =>
        // Lookup the state of the promise.
        match Map/lookup(a, s0) with {
            case None =>
                // Case 1: The address is not a promise. The evaluation is stuck.
                m0
            case Some(Pending) =>
                // Case 2: The promise is in the pending state.

                // Allocate a child promise.
                let childAddr = freshAddr();

                // TODO: Store something at that address.
                let h1 = Map/insert(childAddr, ???, h0);

                // Set the promise state to pending.
                let s1 = Map/insert(a, Pending, s0);

                // Update the fulfill reactions.
                let fs = Map/getWithDefault(a, Nil, f0);
                let f1 = Map/insert(a, fs ::: (lam, childAddr) :: Nil, f0);
                    State(Addr(childAddr), Heap(h1), PromiseState(s1), FulfillReactions(f1), RejectReactions(r0), PromiseLinks(l0), Queue(q0))
            case Some(Fulfilled(v)) =>
                // Case 3: The promise has been fulfilled.
                // Allocate a child promise and enqueue the promise reaction `lam` with the resolved value `v`.
                let childAddr = freshAddr();
                let job = (Fulfilled(v), lam, childAddr);
                    State(Addr(childAddr), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(job :: q0))
            case Some(Rejected(v)) =>
                // Case 4: The promise has been rejected.. The onResolve has no effect.
                State(Cst("Undef"), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0))
        }

    case State(OnReject(e1, e2), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0)) =>
        ??? // TODO: Similar to OnResolve.

    case State(Link(Addr(a1), Addr(a2)), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), Queue(q0)) =>
        // Lookup the list of registered links for a1.
        let xs = Map/getWithDefault(a1, Nil, l0);
        // Append a2 onto the list of registered links.
        let l1 = Map/insert(a1, xs ::: a2 :: Nil, l0);
            State(Cst("Undef"), Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l1), Queue(q0))

    // TODO: Need case for the event loop.

}

///
/// Replaces every occurrence of the variable `x` in the expression `e0` with the expression `r0`.
///
def substitute(e0: Expression, x: Int, r0: Expression): Expression = match e0 with {
    case Var(y)             => if (x == y) r0 else Var(y)
    case Abs(y, e)          => if (x == y) Abs(y, e) else Abs(y, substitute(e, x, r0))
    case App(e1, e2)        => App(substitute(e1, x, r0), substitute(e2, x, r0))

    case Cst(s)             => Cst(s)
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
        match Map/lookup(x, m) with {
            case None    => Var(x)
            case Some(y) => Var(y)
        }
    case Abs(x, e) =>
        // Generate a fresh variable name for `x`.
        let y = freshVar();
        Abs(y, alpha(e, Map/insert(x, y, m)))
    case App(e1, e2) => App(alpha(e1, m), alpha(e2, m))

    case Cst(s)             => Cst(s)
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
/// Performs a single step reduction of the given state `m0`,
/// i.e. performs a single redex-reduce-recompose step.
///
def step(m0: State): State = match m0 with {
    case State(e0, h0, s0, f0, r0, l0, q0) =>
        match redex(e0) with {
            case None =>
                // Case 1: No redex found. Return the state unchanged.
                m0
            case Some((e0, ec)) =>
                // Case 2: A redex has been found. Reduce and recompose.
                let State(e1, h1, s, f1, r1, l1, q1) = reduce(State(e0, h0, s0, f0, r0, l0, q0));
                let e2 = recompose(ec, e1);
                    State(e2, h1, s, f1, r1, l1, q1)
    }
}

///
/// Repeatedly performs one-step reductions until the state `s0` reaches a fixed-point.
///
def eval(m0: State): State =
    let State(e, h, s, f, r, l, q) = step(m0);
        if (isValue(e))
            State(e, h, s, f, r, l, q)
        else
            eval(State(e, h, s, f, r, l, q))


///
/// Injects the given expression `e0` into the initial state.
///
def inject(e0: Expression): State =
    let h0 = Heap(Map/empty());
    let s0 = PromiseState(Map/empty());
    let f0 = FulfillReactions(Map/empty());
    let r0 = RejectReactions(Map/empty());
    let l0 = PromiseLinks(Map/empty());
    let q0 = Queue(Nil);
        State(e0, h0, s0, f0, r0, l0, q0)

//
// We can now test our lambda interpreter!
//
def testEval1: State = eval(inject(Abs(0, Var(0))))                                // λx. x
def testEval2: State = eval(inject(Abs(0, Abs(1, Var(0)))))                        // λx. λy. x
def testEval3: State = eval(inject(App(Abs(0, Var(0)), Abs(1, Var(1)))))           // (λx. x) (λy. y)
def testEval4: State = eval(inject(App(Abs(0, Abs(1, Var(0))), Abs(2, Var(2)))))   // (λx. λy. x) (λz. z)

def testEval5: State = eval(inject(
    Deref(Ref(Cst("hello")))
))

def testEval6: State = eval(inject(
    App(Abs(0,
        Seq(Promisify(Var(0)),
            OnResolve(Var(0), Abs(42, Var(42))))
        ),
        Ref(Cst("Promise1")))
))


///
/// Returns a fresh address.
///
def freshAddr: Int = genSym!()

///
/// Returns a fresh variable.
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
    def empty[k, v]: Map2[k, v] = Map2(Nil)

    def insert[k, v](k: k, v: v, m: Map2[k, v]): Map2[k, v] =
        let Map2(xs) = m;
            Map2((k, v) :: xs)

    def lookup[k, v](k: k, m: Map2[k, v]): Option[v] =
        let Map2(xs) = m;
            getHelper(k, xs)

    def getWithDefault[k, v](k: k, d: v, m: Map2[k, v]): v = ???

    def getHelper[k, v](k: k, xs: List[(k, v)]): Option[v] = match xs with {
        case Nil => None
        case (y, v) :: ys => if (k == y) Some(v) else getHelper(k, ys)
    }

}

namespace List {

    ///
    /// Returns `ys` appended to `xs`.
    ///
    def append[a](xs: List[a], ys: List[a]): List[a] = match xs with {
        case Nil => ys
        case x :: rs => x :: append(rs, ys)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    def map[a,b](f: a -> b, xs: List[a]): List[b] = match xs with {
        case Nil => Nil
        case x :: rs => f(x) :: map(f, rs)
    }

}