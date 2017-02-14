//
// An extension of the lambda calculus with references, sequencing, and promises.
//

///
/// The type of heap addresses.
///
enum Address {
    case A(Int)
}

///
/// The type of variables.
///
enum Variable {
    case V(Int)
}

///
/// The expressions of the lambda calculus extended with references, sequencing, and promises.
///
enum Expression {
    // A variable expression: x.
    case Var(Variable),

    // A lambda abstraction expression : x. e.
    case Abs(Variable, Expression),

    // A function application expression: e1 e2.
    case App(Expression, Expression),

    //
    // General Extensions:
    //

    // A string value.
    case Cst(Str),

    // A heap address.
    case Ptr(Address),

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
    case ESeq(EvaluationContext, Expression),

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
    case Heap(Map[Address, Expression])
}

///
/// The promise state maps addresses to promise values.
///
enum PromiseState {
    case PromiseState(Map[Address, PromiseValue])
}

///
/// The fulfill reactions map addresses to a list of (Lam, Address)-pairs where the address points-to the dependent promise.
///
enum FulfillReactions {
    case FulfillReactions(Map[Address, List[(Expression, Address)]])
}

///
/// The reject reactions map addresses to a list of (Lam, Address)-pairs where the address points-to the dependent promise.
///
enum RejectReactions {
    case RejectReactions(Map[Address, List[(Expression, Address)]])
}

///
/// The reaction queue is a list of (PromiseValue, Lam, Address)-triples where the address points to the dependent promise.
///
enum Queue {
    case Queue(List[(PromiseValue, Expression, Address)])
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
/// A state configuration.
///
enum State {
    case State(Heap, PromiseState, FulfillReactions, RejectReactions, Queue)
}

///
/// Returns `true` if the given expression `e0` is a value.
///
def isValue(e0: Expression): Bool = match e0 with {
    case Abs(x, e)  => true
    case Cst(s)     => true
    case Ptr(a)     => true
    case _          => false
}

///
/// Returns `true` if the given expression `e0` is not a value.
///
def nonValue(e0: Expression): Bool = !isValue(e0)

///
/// Returns a pair of a redex and its evaluation context for the given expression `e0`.
///
/// The evaluation context is a representation of the expression "inside-out" allowing us to recompose
/// the expression after the redex has been reduced (contracted).
///
/// Returns the expression itself (and the empty evaluation context) if the expression is irreducible.
///
def redex(e0: Expression): (Expression, EvaluationContext) = match e0 with {
    case Var(x) =>
        // A variable cannot be reduced.
        (Var(x), Hole)

    case Abs(x, e) =>
        // An abstraction cannot be reduced.
        (Abs(x, e), Hole)

    case App(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            (e0, Hole)
        case nonValue(e1) =>
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, EApp1(ec, e2))
        case isValue(e1) =>
            // Continue reduction in e2.
            let (rdx, ec) = redex(e2);
                (rdx, EApp2(e1, ec))
    }

    case Cst(s) =>
        // A string cannot be reduced.
        (Cst(s), Hole)

    case Ptr(a) =>
        // A pointer cannot be reduced.
        (Ptr(a), Hole)

    case Ref(e) =>
        if (isValue(e))
            // The expression is a redex.
            (e0, Hole)
        else
            // The redex is in the expression e.
            let (rdx, ec) = redex(e);
                (rdx, ERef(ec))

    case Deref(e) =>
        if (isValue(e))
            // The expression is a redex.
            (e0, Hole)
        else
            // The redex is in the expression e.
            let (rdx, ec) = redex(e);
                (rdx, EDeref(ec))

    case Seq(e1, e2) =>
        if (isValue(e1))
            // The expression is a redex.
            (e0, Hole)
        else
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, ESeq(ec, e2))

    case Promisify(e) =>
        if (isValue(e))
            // The expression is a redex.
            (e0, Hole)
        else
            // The redex is in the expression e.
            let (rdx, ec) = redex(e);
                (rdx, EPromisify(ec))

    case Resolve(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            (e0, Hole)
        case nonValue(e1) =>
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, EResolve1(ec, e2))
        case isValue(e1) =>
            // Continue reduction in e2.
            let (rdx, ec) = redex(e2);
                (rdx, EResolve2(e1, ec))
    }

    case Reject(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            (e0, Hole)
        case nonValue(e1) =>
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, EReject1(ec, e2))
        case isValue(e1) =>
            // Continue reduction in e2.
            let (rdx, ec) = redex(e2);
                (rdx, EReject2(e1, ec))
    }

    case OnResolve(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            (e0, Hole)
        case nonValue(e1) =>
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, EOnResolve1(ec, e2))
        case isValue(e1) =>
            // Continue reduction in e2.
            let (rdx, ec) = redex(e2);
                (rdx, EOnResolve2(e1, ec))
    }

    case OnReject(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            (e0, Hole)
        case nonValue(e1) =>
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, EOnReject1(ec, e2))
        case isValue(e1) =>
            // Continue reduction in e2.
            let (rdx, ec) = redex(e2);
                (rdx, EOnReject2(e1, ec))
    }

    case Link(e1, e2) => switch {
        case isValue(e1) && isValue(e2) =>
            // The expression is a redex.
            (e0, Hole)
        case nonValue(e1) =>
            // Continue reduction in e1.
            let (rdx, ec) = redex(e1);
                (rdx, ELink1(ec, e2))
        case isValue(e1) =>
            // Continue reduction in e2.
            let (rdx, ec) = redex(e2);
                (rdx, ELink2(e1, ec))
    }

}

///
/// Returns an expression reconstructed from the given evaluation context `ec0`
/// with the given expression `e0` replacing the hole in the context.
///
def recompose(e0: Expression, ec0: EvaluationContext): Expression = match ec0 with {
    case Hole               => e0
    case EApp1(ec, e)       => App(recompose(e0, ec), e)
    case EApp2(v, ec)       => App(v, recompose(e0, ec))
    case ERef(ec)           => Ref(recompose(e0, ec))
    case EDeref(ec)         => Deref(recompose(e0, ec))
    case ESeq(ec, e)        => Seq(recompose(e0, ec), e)
    case EPromisify(ec)     => Promisify(recompose(e0, ec))
    case EResolve1(ec, e)   => Resolve(recompose(e0, ec), e)
    case EResolve2(e, ec)   => Resolve(e, recompose(e0, ec))
    case EReject1(ec, e)    => Reject(recompose(e0, ec), e)
    case EReject2(e, ec)    => Reject(e, recompose(e0, ec))
    case EOnResolve1(ec, e) => OnResolve(recompose(e0, ec), e)
    case EOnResolve2(e, ec) => OnResolve(e, recompose(e0, ec))
    case EOnReject1(ec, e)  => OnReject(recompose(e0, ec), e)
    case EOnReject2(e, ec)  => OnReject(e, recompose(e0, ec))
    case ELink1(ec, e)      => Link(recompose(e0, ec), e)
    case ELink2(e, ec)      => Link(e, recompose(e0, ec))
}

///
/// Represents the outcome of a single reduction step.
///
enum Outcome {
    /// Represents the next state.
    case Next(Expression, State),

    /// Represents that the reduction is stuck.
    case Done(Expression, State),

    /// Represents that the reduction failed.
    case Abort(Str)
}

///
/// Performs a single reduction step.
///
def step(e0: Expression, m0: State): Outcome =
    let (rdx, ec) = redex(e0);
    match (rdx, m0) with {

        //
        // E-App
        //
        case (App(Abs(x, e1), e2), State(h0, s0, f0, r0, q0)) =>
            // Apply beta-reduction: Use alpha-renaming to avoid capture.
            let e = substitute(e1, x, alpha(e2, Map.empty()));
                Next(recompose(e, ec), State(h0, s0, f0, r0, q0))

        //
        // E-Seq
        //
        case (Seq(v, e), State(h0, s0, f0, r0, q0)) =>
            // Discard the value in the sequence.
            Next(recompose(e, ec), State(h0, s0, f0, r0, q0))

        //
        //  E-Ref
        //
        case (Ref(v), State(Heap(h0), s0, f0, r0, q0)) =>
            // Allocate a fresh address in the heap and store the value at that address.
            let a = freshAddr();
            let h1 = Map.insert(a, v, h0);
            let e1 = Ptr(a);
                Next(recompose(e1, ec), State(Heap(h1), s0, f0, r0, q0))

        //
        //  E-Deref
        //
        case (Deref(Ptr(a)), State(Heap(h0), s0, f0, r0, q0)) =>
            // Dereference the address and return its value.
            match Map.get(a, h0) with {
                case None => Abort("Unable to dereference unallocated address!")
                case Some(v) => Next(recompose(v, ec), State(Heap(h0), s0, f0, r0, q0))
            }

        //
        //  E-Promisify
        //
        case (Promisify(Ptr(a)), State(h0, PromiseState(s0), f0, r0, q0)) =>
            // Sets the state of an uninitialized promise to pending.
            match Map.get(a, s0) with {
                case None =>
                    // Case 1: The address has not yet been promisified. Set the promise state to pending.
                    let s1 = Map.insert(a, Pending, s0);
                        Next(recompose(Cst("Undef"), ec), State(h0, PromiseState(s1), f0, r0, q0))
                case Some(_) =>
                    // Case 2: The address has *already* been promisified. Simply return undef.
                    Next(recompose(Cst("Undef"), ec), m0)
            }

        //
        //  E-Resolve-Pending, E-Resolve-Fulfilled, and E-Resolve-Rejected
        //
        case (Resolve(Ptr(a), v), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0))) =>
            // Lookup the state of the promise.
            match Map.get(a, s0) with {
                case None =>
                    // Case 1: The address is not a promise. The evaluation is stuck.
                    Abort("Unable to resolve non-promise address!")
                case Some(Pending) =>
                    // Case 2: The promise is pending.

                    // Set the promise state to fulfilled.
                    let s1 = Map.insert(a, Fulfilled(v), s0);

                    // Clear out the fulfill reactions.
                    let f1 = Map.delete(a, f0);

                    // Clear out the rejected reactions.
                    let r1 = Map.delete(a, r0);

                    // Retrieve the fulfill reactions and append them to the reaction queue.
                    let rs = Map.getWithDefault(a, Nil, f0);
                    let q1 = q0 ::: List.map(match (lam, addr) -> (Fulfilled(v), lam, addr), rs);
                        Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s1), FulfillReactions(f1), RejectReactions(r1), Queue(q1)))
                case Some(Fulfilled(v)) =>
                    // Case 3: The promise has already been fulfilled with the value `v`. The resolve has no effect.
                    Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0)))
                case Some(Rejected(v)) =>
                    // Case 4: The promise has already been rejected with the value `v`. The resolve has no effect.
                    Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0)))
            }

        //
        //  E-Reject-Pending, E-Reject-Fulfilled, and E-Reject-Rejected
        //
        case (Reject(e1, e2), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0))) =>
            ??? // NB: Similiar to Resolve.

        //
        //  E-OnResolve-Pending, E-OnResolve-Fulfilled, and E-OnResolve-Rejected.
        //
        case (OnResolve(Ptr(a), lam), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0))) =>
            // Lookup the state of the promise.
            match Map.get(a, s0) with {
                case None =>
                    // Case 1: The address is not a promise. The evaluation is stuck.
                    Abort("Unable to register reaction on non-promise address!")
                case Some(Pending) =>
                    // Case 2: The promise is in the pending state.

                    // Allocate a child promise.
                    let childAddr = freshAddr();

                    // Allocate a fresh "object" in the heap. We use a string instead.
                    let h1 = Map.insert(childAddr, Cst("child"), h0);

                    // Set the child promise state to pending.
                    let s1 = Map.insert(childAddr, Pending, s0);

                    // Update the fulfill reactions.
                    let fs = Map.getWithDefault(a, Nil, f0);
                    let f1 = Map.insert(a, fs ::: (lam, childAddr) :: Nil, f0);
                        Next(recompose(Ptr(childAddr), ec), State(Heap(h1), PromiseState(s1), FulfillReactions(f1), RejectReactions(r0), Queue(q0)))
                case Some(Fulfilled(v)) =>
                    // Case 3: The promise has been fulfilled.
                    // Allocate a child promise and enqueue the promise reaction `lam` with the resolved value `v`.
                    let childAddr = freshAddr();
                    let job = (Fulfilled(v), lam, childAddr);
                        Next(recompose(Ptr(childAddr), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(job :: q0)))
                case Some(Rejected(v)) =>
                    // Case 4: The promise has been rejected.. The onResolve has no effect.
                    Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0)))
            }

        //
        //  E-OnReject-Pending, E-OnReject-Fulfilled, and E-OnReject-Rejected.
        //
        case (OnReject(e1, e2), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0))) =>
            ??? // NB: Similiar to OnResolve.

        //
        //  E-Link
        //
        case (Link(Ptr(a1), Ptr(a2)), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0))) =>
            // Add identity fulfill and reject reactions where `a2` is the dependent promise.
            let lam = Abs(V(0), Var(V(0)));
            let fs = Map.getWithDefault(a1, Nil, f0);
            let f1 = Map.insert(a1, fs ::: (lam, a2) :: Nil, f0);
            let rs = Map.getWithDefault(a1, Nil, f0);
            let r1 = Map.insert(a1, rs ::: (lam, a2) :: Nil, r0);
                Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f1), RejectReactions(r1), Queue(q0)))

        //
        //  E-Reaction-Loop
        //
        case (e, State(h0, s0, f0, r0, Queue((Fulfilled(v), lambda, dependentPromise) :: q1))) =>
            // The reaction queue contains an item. Check if the expression is a value.
            if (isValue(e))
                // The expression is a value. Discard it and evaluate the item from the reaction queue.
                let e = Resolve(Ptr(dependentPromise), App(lambda, v));
                    Next(e, State(h0, s0, f0, r0, Queue(q1)))
             else
                // The expression is a non-value. Evaluation is stuck.
                Done(e, m0)

        case (e, m) => Done(e, m)

}

///
/// Replaces every occurrence of the variable `x` in the expression `e0` with the expression `r0`.
///
def substitute(e0: Expression, x: Variable, r0: Expression): Expression = match e0 with {
    case Var(y)             => if (x == y) r0 else Var(y)
    case Abs(y, e)          => if (x == y) Abs(y, e) else Abs(y, substitute(e, x, r0))
    case App(e1, e2)        => App(substitute(e1, x, r0), substitute(e2, x, r0))

    case Cst(s)             => Cst(s)
    case Ptr(a)             => Ptr(a)
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
def alpha(e0: Expression, m: Map[Variable, Variable]): Expression = match e0 with {
    case Var(x) =>
        // Check if we need to rename the variable.
        match Map.get(x, m) with {
            case None    => Var(x)
            case Some(y) => Var(y)
        }
    case Abs(x, e) =>
        // Generate a fresh variable name for `x`.
        let y = freshVar();
        Abs(y, alpha(e, Map.insert(x, y, m)))
    case App(e1, e2) => App(alpha(e1, m), alpha(e2, m))

    case Cst(s)             => Cst(s)
    case Ptr(a)             => Ptr(a)
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
/// Repeatedly performs one-step reductions until the state `s0` reaches a fixed-point.
///
def eval(e0: Expression, m0: State): Result[(Expression, State), Str] = match println!(step(e0, m0)) with {
    case Next(e, m) => eval(e, m)
    case Done(e, m) => Ok((e, m))
    case Abort(e)   => Err(e)
}

///
/// Injects the given expression `e0` into the initial state.
///
def initialState: State =
    let h0 = Heap(Map.empty());
    let s0 = PromiseState(Map.empty());
    let f0 = FulfillReactions(Map.empty());
    let r0 = RejectReactions(Map.empty());
    let q0 = Queue(Nil);
        State(h0, s0, f0, r0, q0)

//
// Example 1:
//
// var x = Ref("Promise1")
// promisify(x)
// x.onResolve(y => Ref(y))
// x.resolve("hello")
//
def testEval1: Result[(Expression, State), Str] = eval(
    App(Abs(V(0),
        Seq(Promisify(Var(V(0))),
            Seq(OnResolve(Var(V(0)), Abs(V(42), Ref(Var(V(42))))),
                Resolve(Var(V(0)), Cst("hello"))
            ))
        ),
        Ref(Cst("Promise1"))),
    initialState()
)

//
// Example 2:
//
// var x = Ref("Promise1")
// var y = Ref("Promise2")
// promisify(x)
// promisify(y)
// x.link(y)
// y.onResolve(z => Ref(z))
// x.resolve("hello")
//
def testEval2: Result[(Expression, State), Str] = eval(
    App(App(Abs(V(0), Abs(V(1),
        Seq(Promisify(Var(V(0))),
            Seq(Promisify(Var(V(1))),
                Seq(Link(Var(V(0)), Var(V(1))),
                    Seq(OnResolve(Var(V(1)), Abs(V(42), Ref(Var(V(42))))),
                        Resolve(Var(V(0)), Cst("hello"))
                    )
                )
            )
        ))),
        Ref(Cst("Promise1"))), Ref(Cst("Promise2"))),
    initialState()
)


///
/// Returns a fresh address.
///
def freshAddr: Address = A(genSym!())

///
/// Returns a fresh variable.
///
def freshVar: Variable = V(1000 + genSym!())

