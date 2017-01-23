//
// Abstract Interpreter for the lambda calculus extended with references, sequencing, and promises.
//

///
/// A variable name is simply an integer.
///
type Variable = V(Int)

///
/// An address is simply an integer.
///
type Address = A(Int)

///
/// The expressions of the lambda calculus are: variables, lambda abstractions, and applications.
///
enum Expression {
    // A variable expression.
    case Var(Variable),

    // A lambda abstraction expression.
    case Abs(Variable, Expression),

    // A function application expression.
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
/// An environment is a map from variables to addresses.
///
type Environment = Environment(Map2[Variable, Address])

///
/// The type of storable values.
///
enum Storable {
    /// Represents a stored continuation.
    case StoredKont(Kontinuation),

    /// Represents a stored value, i.e. an expression and its closing environment.
    case StoredValue(Expression, Environment)
}

// TODO: Introduce: Introduce Value as the pair of an expression closed by an environment?

///
/// A store is a map from addresses to a set of storables.
///
type Store = Store(Map2[Address, Set2[Storable]])

///
/// The CEK-machine operates based on continuations derived
//  from the evaluation contexts of the language.
///
enum Kontinuation {
    // A continuation that represents the empty evaluation context.
    case Empty,

    // A continuation that represents the `[] e` context where the expression `e` is closed by the environment.
    case KApp1(Expression, Environment, Address),

    // A continuation that represents the `v []` context where the value `v` is closed by the environment.
    case KApp2(Expression, Environment, Address),

    // A continuation that represents the `Ref []` context.
    case KRef(Address),

    // A continuation that represents the `Deref []` context.
    case KDeref(Address),

    // A continuation that represents the `[] ; e` context where the expression `e` is closed by the environment.
    case KSeq(Expression, Environment, Address),

    // A continuation that represents the `promisify([])` context.
    case KPromisify(Address),

    // A continuation that represents the `[].resolve(e)` context where the expression `e` is closed by the environment.
    case KResolve1(Expression, Environment, Address),

    // A continuation that represents the `v.resolve([])` context where the value `e` is closed by the environment.
    case KResolve2(Expression, Environment, Address),

    // NB: KReject1 and KReject2 are conceptually similar to KResolve1 and KResolve2.

    // A continuation that represents the `[].onResolve(e)` context where the expression `e` is closed by the environment.
    case KOnResolve1(Expression, Environment, Address),

    // A continuation that represents the `v.onResolve([])` context where the value `e` is closed by the environment.
    case KOnResolve2(Expression, Environment, Address), // TODO: This does not require an environment, since it must be a pointer...

    // NB: KOnReject1 and KOnReject2 are conceptually similar to KOnResolve1 and KOnResolve2.

    // A continuation that represents the `[].link(e)` context where the expression `e` is closed by the environment.
    case KLink1(Expression, Environment, Address),

    // A continuation that represents the `v.link([])` context where the value `e` is closed by the environment.
    case KLink2(Expression, Environment, Address)
}
// TODO: Rename these constructors in other machines, e.g. cek,cesk
// TODO: Add these and other machines to unit tests.

///
/// The promise state maps addresses to a set of promise values.
///
type PromiseState = PromiseState(Map2[Address, Set2[PromiseValue]])

///
/// The fulfill reactions map addresses to a list of (Lam, Address)-pairs where the address points-to the child promise.
///
type FulfillReactions = FulfillReactions(Map2[Address, Set2[(Expression, Environment, Address)]]) // TODO: Should this be a set? // TODO: Does expression need to be paired with an environment?

///
/// The reject reactions map addresses to a list of (Lam, Address)-pairs where the address points-to the child promise.
///
type RejectReactions = RejectReactions(Map2[Address, List[(Expression, Address)]]) // TODO: Should this be a set? // TODO: Does expression need to be paired with an environment?

///
/// The promise links map addresses to a list of child promise addresses.
///
type PromiseLinks = PromiseLinks(Map2[Address, List[Address]]) // TODO: Should this be a set?

///
/// The link queue is a list of (PromiseValue, Address)-pairs where the address points-to the linked promise.
///
type LinkQueue = LinkQueue(List[(PromiseValue, Address)]) // TODO: Should this be a set?

///
/// The reaction queue is a set of (PromiseValue, Lam, Environment, Address)-tuples where the address points to the child promise.
///
type ReactionQueue = ReactionQueue(Set2[(PromiseValue, Expression, Environment, Address)]) // TODO: Should this be a set? // TODO: Does expression need to be paired with an environment?

///
/// A promise is in one of three states: Pending, fulfilled (with a value), or rejected (with a value).
///
enum PromiseValue {
    case Pending,
    case Fulfilled(Expression), // TODO: Need environment to close the expression?
    case Rejected(Expression)   // TODO: Need environment to close the expression?
}

///
/// The timestamp associated with a machine configuration.
///
type Time = Time(Int)

///
/// A machine state configuration.
///
/// NB: Note that the Address component is a pointer to continuation in the store.
///
type State = State(Expression, Environment, Store, PromiseState, FulfillReactions, RejectReactions, PromiseLinks, LinkQueue, ReactionQueue, Address, Time)

///
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): State =
    // Save the empty continuation into the store.
    let kont = Kontinuation.Empty;
    let kptr = A(0);
    let env0 = Map2.empty();
    let sto0 = Map2.insert(kptr, Set2.singleton(StoredKont(kont)), Map2.empty());
    let s0 = Map2.empty();
    let f0 = Map2.empty();
    let r0 = Map2.empty();
    let l0 = Map2.empty();
    let w0 = Nil;
    let q0 = Set2.empty();
    let time = Time(1);
        State(e0, Environment(env0), Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), kptr, time)

///
/// Returns the next timestamp for the machine in the given state `m0` and continuation `k0`.
///
def tick(m0: State, k0: Kontinuation): Time = match m0 with {
    case State(_, _, _, _, _, _, _, _, _, _, Time(t)) => Time(t + 1)
}

///
/// Returns the next address for the machine in the given state `m0` and continuation `c0`.
///
def alloc(m0: State, k0: Kontinuation): Address = match m0 with {
    case State(_, _, _, _, _, _, _, _, _, _, Time(t)) => A(t)
}

///
/// Represents the outcome of a single reduction step.
///
enum Outcome {
    /// Represents the next state.
    case Next(State),

    /// Represents that the reduction is stuck.
    case Done(State),

    /// Represents that the reduction failed.
    case Abort(Str)
}

///
/// Returns all possible transitions (outcomes) from the given state `m0` and continuation `k0`.
///
// TODO: Rename Store to Heap and h0.
def step(m0: State, k0: Kontinuation): Set2[Outcome] = match (m0, k0) with {

//        //
//        // E-Link-Loop
//        //
//        case (e, State(h0, s0, f0, r0, l0, LinkQueue((Fulfilled(v), a) :: lq), q0)) =>
//            // The link queue is non-empty. Extract an item and evaluate it.
//            let e = Seq(Resolve(Ptr(a), v), e);
//                Next(recompose(e, ec), State(h0, s0, f0, r0, l0, LinkQueue(lq), q0))


    //
    // Variable Expression.
    //
    case (State(Var(x), Environment(env0), Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), kptr, time), _) =>
        // The expression is a variable. Look it up in the environment.
         match Map2.lookup(x, env0) with {
            case None => Set2(Abort("Unbound variable.") :: Nil)
            case Some(ptr) =>
                let values = Option.getWithDefault(Map2.lookup(ptr, sto0), Set2.empty());
                let f = storable -> match storable with {
                    case StoredKont(_) => Abort("Non-value storable.")
                    case StoredValue(v, env1) => Next(State(v, env1, Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), kptr, tick(m0, k0)))
                };
                Set2.map(f, values)
         }

    //
    // Application Expression.
    //
    case (State(App(e1, e2), Environment(env0), Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), kptr, time), _) =>
        // Generate a fresh address where to store the continuation.
        let a = alloc(m0, k0);
        // Continue evaluation of `e1` and store `e2` in a continuation.
        let k = KApp1(e2, Environment(env0), kptr);
        // Store the continuation.
        let newValue = StoredKont(k);
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
        let sto1 = Map2.insert(a, lubValue, sto0);
            Set2.singleton(Next(State(e1, Environment(env0), Store(sto1), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), a, tick(m0, k0))))

    //
    // Application1 Kontinuation.
    //
    case (State(v1, Environment(env0), Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), _, time), KApp1(e2, env2, kont)) if isValue(v1) =>
        // Generate a fresh address where to store the continuation.
        let a = alloc(m0, k0);
        // Continue evaluation of `e2` and store `v1` in a continuation.
        let k = KApp2(v1, Environment(env0), kont);
        // Store the continuation.
        let newValue = StoredKont(k);
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
        let sto1 = Map2.insert(a, lubValue, sto0);
            Set2.singleton(Next(State(e2, env2, Store(sto1), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), a, tick(m0, k0))))

    //
    // Application2 Kontinuation.
    //
    case (State(v2, Environment(env0), Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), _, time), KApp2(v1, Environment(env1), kont)) if isValue(v2) =>
        // Evaluate the body of the lambda `v1` with argument `v2`.
        let Abs(x, body) = v1;
        let a = alloc(m0, k0);
        let env2 = Map2.insert(x, a, env1);
        let newValue = StoredValue(v2, Environment(env0));
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
        let sto1 = Map2.insert(a, lubValue, sto0);
            Set2.singleton(Next(State(body, Environment(env2), Store(sto1), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), kont, tick(m0, k0))))


    //
    // Reference Expression.
    //
    case (State(Ref(e), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KRef` continuation in the store.
        let a = alloc(m0, k0);
        let k = StoredKont(KRef(kptr));
        let sto1 = Store.weakUpdate(a, k, sto0);
            Set2.singleton(Next(State(e, env0, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    // Reference Kontinuation.
    //
    case (State(e, env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KRef(kont)) if isValue(e) =>
        // Allocate an address in the heap and store the value at that address.
        let a = alloc(m0, k0);
        let v = StoredValue(e, env0);
        let sto1 = Store.weakUpdate(a, v, sto0);
            Set2.singleton(Next(State(Ptr(a), env0, sto1, s0, f0, r0, l0, w0, q0, kont, tick(m0, k0)))) // TODO: What happens to the environment?

    //
    // Dereference Expression.
    //
    case (State(Deref(e), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KDeref` continuation in the store.
        let a = alloc(m0, k0);
        let k = StoredKont(KDeref(kptr));
        let sto1 = Store.weakUpdate(a, k, sto0);
            Set2.singleton(Next(State(e, env0, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    // Dereference Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KDeref(kont)) =>
        // Lookup the address `a` in the store.
        let vs = Store.lookup(a, sto0);
        let f = s -> match s with {
            case StoredKont(k)          => Abort("Non-value storable.")
            case StoredValue(e, env1)   => Next(State(e, env1, sto0, s0, f0, r0, l0, w0, q0, a, tick(m0, k0)))
        };
            Set2.map(f, vs)

    //
    // Sequence Expression.
    //
    case (State(Seq(e1, e2), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KSeq` continuation in the store.
        let a = alloc(m0, k0);
        let k = StoredKont(KSeq(e2, env0, kptr));
        let sto1 = Store.weakUpdate(a, k, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    // Sequence Kontinuation.
    //
    case (State(e1, env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KSeq(e2, env2, kont)) if isValue(e1) =>
        // Discard the value `e1`.
        Set2.singleton(Next(State(e2, env2, sto0, s0, f0, r0, l0, w0, q0, kont, tick(m0, k0))))

    //
    // Promisify Expression.
    //
    case (State(Promisify(e), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KPromisify` continuation in the store.
        let a = alloc(m0, k0);
        let k = StoredKont(KPromisify(kptr));
        let sto1 = Store.weakUpdate(a, k, sto0);
            Set2.singleton(Next(State(e, env0, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    // Promisify Continuation.
    //
    case (State(Ptr(a), env0, sto0, PromiseState(s0), f0, r0, l0, w0, q0, kptr, time), KPromisify(kont)) =>
        // Weakly update the state of the promise `a` to pending.
        let promiseStates = Option.getWithDefault(Map2.lookup(a, s0), Set2.empty());
        let s1 = Map2.insert(a, Set2.insert(Pending, promiseStates), s0);
            Set2.singleton(Next(State(Cst("Undef"), env0, sto0, PromiseState(s1), f0, r0, l0, w0, q0, kont, tick(m0, k0))))



//        //
//        //  E-Resolve-Pending, E-Resolve-Fulfilled, and E-Resolve-Rejected
//        //
//        case (Resolve(Ptr(a), v), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0))) =>
//            // Lookup the state of the promise.
//            match Map2.lookup(a, s0) with {
//                case None =>
//                    // Case 1: The address is not a promise. The evaluation is stuck.
//                    Abort("Unable to resolve non-promise address!")
//                case Some(Pending) =>
//                    // Case 2: The promise is pending.
//
//                    // Set the promise state to fulfilled.
//                    let s1 = Map2.insert(a, Fulfilled(v), s0);
//
//                    // Clear out the fulfill reactions.
//                    let f1 = Map2.remove(a, f0);
//
//                    // Clear out the rejected reactions.
//                    let r1 = Map2.remove(a, r0);
//
//                    // Clear out the promise links.
//                    let l1 = Map2.remove(a, l0);
//
//                    // Retrieve the promise links and append them to the link queue.
//                    let lq = Map2.getWithDefault(a, Nil, l0);
//                    let w1 = w0 ::: List.map(l -> (Fulfilled(v), l), lq);
//
//                    // Retrieve the fulfill reactions and append them to the reaction queue.
//                    let rs = Map2.getWithDefault(a, Nil, f0);
//                    let q1 = q0 ::: List.map(match (lam, addr) -> (Fulfilled(v), lam, addr), rs);
//                        Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s1), FulfillReactions(f1), RejectReactions(r1), PromiseLinks(l1), LinkQueue(w1), ReactionQueue(q1)))
//                case Some(Fulfilled(v)) =>
//                    // Case 3: The promise has already been fulfilled with the value `v`. The resolve has no effect.
//                    Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0)))
//                case Some(Rejected(v)) =>
//                    // Case 4: The promise has already been rejected with the value `v`. The resolve has no effect.
//                    Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0)))
//            }
//
    //
    //  E-Reject-Pending, E-Reject-Fulfilled, and E-Reject-Rejected
    //
    //  NB: Omitted


    // TODO: Many of these cases seem to use kptr incorrectly? Check up on that.
    // Essentially kptr should only be used when there is not a specific kontinuation in play.

    //
    //  OnResolve Expression.
    //
    case (State(OnResolve(e1, e2), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KOnResolve1` continuation in the store.
        let a = alloc(m0, k0);
        let k = StoredKont(KOnResolve1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(a, k, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    //  OnResolve1 Kontinuation.
    //
    case (State(e1, env0, sto0, s0, f0, r0, l0, w0, q0, _, time), KOnResolve1(e2, env2, kont)) if isValue(e1) =>
        // Allocate the `KOnResolve2` continuation in the store.
        let a = alloc(m0, k0);
        let k = StoredKont(KOnResolve2(e1, env0, kont));
        let sto1 = Store.weakUpdate(a, k, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    //  OnResolve2 Kontinuation.
    //
    case (State(e2, env2, sto0, s0, f0, r0, l0, w0, ReactionQueue(q0), _, time), KOnResolve2(Ptr(a), env1, kont)) if isValue(e2) => // TODO: Enforce that e2 is a lambda?
        let f: PromiseValue -> Set2[Outcome] = pv -> match pv with {
            case Pending =>
                // Allocate a child promise.
                let childAddr = alloc(m0, k0);

                // Allocate a fresh "object" in the heap. We use a string instead.
                let value = StoredValue(Cst("child"), Environment(Map2.empty()));
                let sto1 = Store.weakUpdate(childAddr, value, sto0);

                // Set the child promise state to pending.
                let s1 = PromiseState.weakUpdate(childAddr, Pending, s0);
//
//                    // Update the fulfill reactions.
//                    let fs = Map2.getWithDefault(a, Nil, f0);
//                    let f1 = Map2.insert(a, fs ::: (lam, childAddr) :: Nil, f0);
                ???
            case Fulfilled(v) =>
                // Allocate a child promise and enqueue the promise reaction `lam` with the resolved value `v`.
                let childAddr = alloc(m0, k0);
                let job = (Fulfilled(v), e2, env2, childAddr);
                let q1 = Set2.insert(job, q0);
                    Set2.singleton(Next(State(Ptr(childAddr), Environment(Map2.empty()), sto0, s0, f0, r0, l0, w0, ReactionQueue(q1), kont, tick(m0, k0))))
            case Rejected(v) =>
                // TODO: Conceptually similar to fulfilled.
                ???
        };
        Set2.flatMap(f, PromiseState.lookup(a, s0))


//        case (OnResolve(Ptr(a), lam), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0))) =>
//            // Lookup the state of the promise.
//            match Map2.lookup(a, s0) with {
//                case None =>
//                    // Case 1: The address is not a promise. The evaluation is stuck.
//                    Abort("Unable to register reaction on non-promise address!")
//                case Some(Pending) =>
//                    // Case 2: The promise is in the pending state.
//
//                    // Allocate a child promise.
//                    let childAddr = freshAddr();
//
//                    // Allocate a fresh "object" in the heap. We use a string instead.
//                    let h1 = Map2.insert(childAddr, Cst("child"), h0);
//
//                    // Set the child promise state to pending.
//                    let s1 = Map2.insert(childAddr, Pending, s0);
//
//                    // Update the fulfill reactions.
//                    let fs = Map2.getWithDefault(a, Nil, f0);
//                    let f1 = Map2.insert(a, fs ::: (lam, childAddr) :: Nil, f0);
//                        Next(recompose(Ptr(childAddr), ec), State(Heap(h1), PromiseState(s1), FulfillReactions(f1), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0)))
//                case Some(Fulfilled(v)) =>
//                    // Case 3: The promise has been fulfilled.
//                    // Allocate a child promise and enqueue the promise reaction `lam` with the resolved value `v`.
//                    let childAddr = freshAddr();
//                    let job = (Fulfilled(v), lam, childAddr);
//                        Next(recompose(Ptr(childAddr), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(job :: q0)))
//                case Some(Rejected(v)) =>
//                    // Case 4: The promise has been rejected. The onResolve has no effect.
//                    Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0)))
//            }
//


    //
    //  E-OnReject-Pending, E-OnReject-Fulfilled, and E-OnReject-Rejected.
    //
    //  NB: Omitted



    //
    //  E-Link
    //

//        case (Link(Ptr(a1), Ptr(a2)), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0))) =>
//            // Lookup the list of registered links for a1.
//            let xs = Map2.getWithDefault(a1, Nil, l0);
//            // Append a2 onto the list of registered links.
//            let l1 = Map2.insert(a1, xs ::: a2 :: Nil, l0);
//                Next(recompose(Cst("Undef"), ec), State(Heap(h0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l1), LinkQueue(w0), ReactionQueue(q0)))
//
//        //
//        //  E-Reaction-Loop
//        //
//        case (e, State(h0, s0, f0, r0, l0, w0, ReactionQueue((Fulfilled(v), lambda, childPromise) :: q1))) =>
//            // The reaction queue contains an item. Check if the expression is a value.
//            if (isValue(e))
//                // The expression is a value. Discard it and evaluate the item from the reaction queue.
//                let e = Resolve(Ptr(childPromise), App(lambda, v));
//                    Next(e, State(h0, s0, f0, r0, l0, w0, ReactionQueue(q1)))
//             else
//                // The expression is a non-value. Evaluation is stuck.
//                Done(e, m0)

    //
    // Stuck.
    //
    case _ => let foo = println!(k0); Set2.singleton(Done(m0))
}

///
/// Returns all possible transitions from the given state `m0`.
///
def stepAll(m0: State): Set2[Outcome] = match m0 with {
    case State(exp0, env0, Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), PromiseLinks(l0), LinkQueue(w0), ReactionQueue(q0), kptr, time) =>
        let values = Option.getWithDefault(Map2.lookup(kptr, sto0), Set2.empty());
        let f = value -> match value with {
            case StoredKont(k) => step(m0, k)
            case _             => Set2.empty()
        };
        Set2.flatMap(f, values)
}

///
/// Computes the fixed-point of the transition relation defined by `stepAll`
/// starting from the given machine configuration `m0`.
///
def eval(m0: State): Set2[Outcome] =
    let outcomes = println!(stepAll(m0));
    let f = outcome -> match outcome with {
        case Next(m)        => eval(m)
        case Done(m)        => Set2.singleton(Done(m))
        case Abort(e)       => Set2.singleton(Abort(e))
    };
    Set2.union(outcomes, Set2.flatMap(f, outcomes))

///
/// Evaluates the given expression to a value by injecting it into the initial state of the CEK machine
/// and repeatedly performing a transition until the machine halts with a value (or aborts).
///
def run(e0: Expression): Set2[Outcome] = eval(inject(e0))

//
// Example 1:
//
// var x = Ref("Promise1")
// promisify(x)
// x.onResolve(y => Ref(y))
// x.resolve("hello")
//
def testEval1: Set2[Outcome] = eval(inject(
    App(Abs(V(0),
        Seq(Promisify(Var(V(0))),
            Seq(OnResolve(Var(V(0)), Abs(V(42), Ref(Var(V(42))))),
                Resolve(Var(V(0)), Cst("hello"))
            ))
        ),
        Ref(Cst("Promise1"))))
)


// TODO: Cleanup

namespace PromiseState {

    // TODO: Cleanup
    def lookup(a: Address, s: PromiseState): Set2[PromiseValue] =
        let PromiseState(m) = s;
            Option.getWithDefault(Map2.lookup(a, m), Set2.empty())

    // TODO: Cleanup
    def weakUpdate(a: Address, v: PromiseValue, s: PromiseState): PromiseState =
        let PromiseState(m) = s;
        let newValue = v;
        let oldValue = Option.getWithDefault(Map2.lookup(a, m), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
            PromiseState(Map2.insert(a, lubValue, m))

}

namespace Store {

    // TODO: Cleanup
    def lookup(a: Address, sto: Store): Set2[Storable] =
        let Store(sto0) = sto;
            Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty())

    // TODO: Cleanup
    def weakUpdate(a: Address, newValue: Storable, sto: Store): Store =
        let Store(sto0) = sto;
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
            Store(Map2.insert(a, lubValue, sto0))

}



//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
//

enum Set2[a] {
    case Set2(List[a])
}

namespace Set2 {

    def empty[a]: Set2[a] = Set2(Nil)

    def singleton[a](a: a): Set2[a] = Set2(a :: Nil)

    ///
     /// Adds `x` to `xs`.
     ///
     def insert[a](x: a, xs: Set2[a]): Set2[a] =
         let Set2(s) = xs;
             if (List.memberOf(x, s)) xs else Set2(x :: s)

    def union[a](xs: Set2[a], ys: Set2[a]): Set2[a] =
         let Set2(s) = xs;
             List.foldRight(insert, ys, s)

    ///
     /// Returns the result of applying `f` to every element in `xs`.
     /// Note: The returned set may be smaller than `xs`.
     ///
     def map[a, b](f: a -> b, xs: Set2[a]): Set2[b] = match xs with {
         case Set2(Nil) => Set2(Nil)
         case Set2(x :: rs) => insert(f(x), map(f, Set2(rs)))
     }

    ///
    /// Returns the result of applying `f` to every element in `xs` and taking the union.
    ///
    def flatMap[a,b](f: a -> Set2[b], xs: Set2[a]): Set2[b] = match xs with {
        case Set2(Nil) => Set2(Nil)
        case Set2(x :: rs) => union(f(x), flatMap(f, Set2(rs)))
    }

}

enum Map2[k, v] {
    case Map2(List[(k, v)])
}

namespace Map2 {

    def empty[k, v]: Map2[k, v] = Map2(Nil)

    def insert[k, v](k: k, v: v, m: Map2[k, v]): Map2[k, v] =
        let Map2(xs) = m;
            Map2((k, v) :: xs)

    def lookup[k, v](k: k, m: Map2[k, v]): Option[v] =
        let Map2(xs) = m;
            getHelper(k, xs)

    def getHelper[k, v](k: k, xs: List[(k, v)]): Option[v] = match xs with {
        case Nil => None
        case (y, v) :: ys => if (k == y) Some(v) else getHelper(k, ys)
    }

    def remove[k, v](k: k, m: Map2[k, v]): Map2[k, v] =
        let Map2(xs) = m;
            Map2(removeHelper(k, xs))

    def removeHelper[k, v](k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (y, v) :: ys => if (k == y) removeHelper(k,  ys) else (y, v) :: removeHelper(k, ys)
    }

}


// TODO: need/want?

enum MultiMap[k, v] {
    case MultiMap(List[(k, Set[v])])
}

namespace MultiMap {

    def empty[k, v]: MultiMap[k, v] = MultiMap(Nil)

}
