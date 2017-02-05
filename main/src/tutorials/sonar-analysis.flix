// TODO: Add to unit test (along with other machines).

// TODO: Ondrej: Do we want a closure expression or not?

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
/// A (source) location is simply a string label.
///
type Label = L(Str)

///
/// The expressions of the lambda calculus are: variables, lambda abstractions, and applications.
///
enum Expression {
    // A variable expression.
    case Var(Variable, Label),

    // A lambda abstraction expression.
    case Abs(Variable, Expression, Label),

    // A function application expression.
    case App(Expression, Expression, Label),

    //
    // General Extensions:
    //

    // A string value.
    case Cst(Str, Label),

    // A heap address.
    case Ptr(Address), // TODO: Ondrej: I don't think it makes sense for Ptr to have an address?

    // A heap reference expression: ref e.
    case Ref(Expression, Label),

    // A heap de-reference expression: deref e.
    case Deref(Expression, Label),

    // A sequence expression: e1; e2.
    case Seq(Expression, Expression, Label),

    //
    // Promises Extensions:
    //

    // A promisify expression: promisify(e).
    case Promisify(Expression, Label),

    // A resolve expression: e1.resolve(e2).
    case Resolve(Expression, Expression, Label),

    // A reject expression: e1.reject(e2).
    case Reject(Expression, Expression, Label),

    // An onResolve expression: e1.onResolve(e2).
    case OnResolve(Expression, Expression, Label),

    // An onReject expression: e1.onReject(e2).
    case OnReject(Expression, Expression, Label),

    // A link expression: e1.link(e2).
    case Link(Expression, Expression, Label)
}

///
/// Returns `true` if the given expression `e0` is a value.
///
def isValue(e0: Expression): Bool = match e0 with {
    case Abs(x, e, _)   => true
    case Cst(s)         => true
    case Ptr(a)         => true
    case _              => false
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

///
/// A store is a map from addresses to a set of storables.
///
type Store = Store(Map2[Address, Set2[Storable]])

///
/// The CEK-machine operates based on continuations derived from the evaluation contexts of the language.
///
/// NB: The last Address component is the contination pointer.
///
enum Kontinuation {
    // A continuation that represents the empty evaluation context.
    case KEmpty,

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

    // A continuation that represents the `a.resolve([])` context.
    case KResolve2(Address, Address),

    // NB: KReject1 and KReject2 are conceptually similar to KResolve1 and KResolve2.

    // A continuation that represents the `[].onResolve(e)` context where the expression `e` is closed by the environment.
    case KOnResolve1(Expression, Environment, Address),

    // A continuation that represents the `a.onResolve([])` context.
    case KOnResolve2(Address, Address),

    // NB: KOnReject1 and KOnReject2 are conceptually similar to KOnResolve1 and KOnResolve2.

    // A continuation that represents the `[].link(e)` context where the expression `e` is closed by the environment.
    case KLink1(Expression, Environment, Address),

    // A continuation that represents the `a.link([])` context.
    case KLink2(Address, Address)
}

///
/// The promise state maps addresses to a set of promise values.
///
type PromiseState = PromiseState(Map2[Address, Set2[PromiseValue]])

///
/// The fulfill reactions map addresses to a set of (Lambda, Environment, Address)-triples where the address points-to the child promise.
///
type FulfillReactions = FulfillReactions(Map2[Address, Set2[(Expression, Environment, Address)]])

///
/// The reject reactions map addresses to a set of (Lambda, Environment, Address)-triples where the address points-to the child promise.
///
type RejectReactions = RejectReactions(Map2[Address, Set2[(Expression, Address)]])

///
/// The promise links map addresses to a set of child promise addresses.
///
type PromiseLinks = PromiseLinks(Map2[Address, Set2[Address]])

///
/// The link queue is a set of (PromiseValue, Address)-pairs where the address points-to the linked promise.
///
type LinkQueue = LinkQueue(Set2[(PromiseValue, Address)])

///
/// The reaction queue is a set of (PromiseValue, Lambda, Environment, Address)-tuples where the address points to the child promise.
///
type ReactionQueue = ReactionQueue(Set2[(PromiseValue, Expression, Environment, Address)])

///
/// A promise is in one of three states: Pending, fulfilled (with a value), or rejected (with a value).
///
enum PromiseValue {
    // The promise is pending.
    case Pending,

    // The promise has been fulfilled with a value. NB: We assume the value is not a lambda, hence the lack of an environment.
    case Fulfilled(Expression),

    // The promise has been rejected with a value. NB: We assume the value is not a lambda, hence the lack of an environment.
    case Rejected(Expression)
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
/// The edges of the promise graph.
///
enum Edge {

    // A resolve edge from a value allocation site (label) to a promise allocation site (label).
    case ResolveEdge(Label, Label),

    // A reject edge from a value allocation site (label) to a promise allocation site (label).
    case RejectEdge(Label, Label),

    // A resolve registration edge from a promise allocation site (label) to lambda (label).
    case ResolveRegistration(Label, Label),

    // A reject registration edge from a promise allocation site (label) to lambda (label).
    case RejectRegistration(Label, Label),

    // A link edge from a promise allocation site (label) to a promise allocation site (label).
    case LinkEdge(Label, Label)

}

///
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): State =
    // Save the empty continuation into the store.
    let kont = Kontinuation.KEmpty;
    let kptr = A(0);
    let env0 = Map2.empty();
    let sto0 = Map2.insert(kptr, Set2.singleton(StoredKont(kont)), Map2.empty());
    let s0 = Map2.empty();
    let f0 = Map2.empty();
    let r0 = Map2.empty();
    let l0 = Map2.empty();
    let w0 = Set2.empty();
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
def step(m0: State, k0: Kontinuation): Set2[Outcome] = match (m0, k0) with {

    //
    // Link-Loop.
    //
    // NB: This *must* appear here before any other rule has a chance to fire.
    case (State(exp, env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) if LinkQueue.nonEmpty(w0) =>
        let f: ((PromiseValue, Address)) -> Outcome = match (pv, a) -> {
            match pv with {
                case Pending => Abort("Non-settled promise in LinkQueue.")
                case Fulfilled(v) =>
                    let loc = L("LinkLoop");
                    let e = Seq(Resolve(Ptr(a), v, loc), exp, loc);
                    let w1 = LinkQueue.strongDelete(pv, a, w0);
                        Next(State(e, env0, sto0, s0, f0, r0, l0, w1, q0, kptr, tick(m0, k0)))
                case Rejected(v) =>
                    ??? // NB: Conceptually similar to the fulfilled case.
            }
        };
        LinkQueue.map(f, w0)

    //
    // Variable Expression.
    //
    case (State(Var(x, loc), Environment(env0), sto0, s0, f0, r0, l0, w1, q0, kptr, time), _) =>
        // The expression is a variable. Look it up in the environment.
         match Map2.lookup(x, env0) with {
            case None => Set2(Abort("Unbound variable.") :: Nil)
            case Some(a) =>
                let values = Store.lookup(a, sto0);
                let f = storable -> match storable with {
                    case StoredKont(_) => Abort("Non-value storable.")
                    case StoredValue(v, env1) => Next(State(v, env1, sto0, s0, f0, r0, l0, w1, q0, kptr, tick(m0, k0)))
                };
                Set2.map(f, values)
         }

    //
    // Application Expression.
    //
    case (State(App(e1, e2, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), _) =>
        // Allocate the `KApp1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KApp1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    // Application1 Kontinuation.
    //
    case (State(v1, env0, sto0, s0, f0, r0, l0, w0, q0, _, time), KApp1(e2, env2, kont)) if isValue(v1) =>
        // NB: `v1` should be a lambda abstraction.
        // Allocate the `KApp2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KApp2(v1, env0, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e2, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    // Application2 Kontinuation.
    //
    case (State(v2, Environment(env2), sto0, s0, f0, r0, l0, w0, q0, _, time), KApp2(v1, Environment(env1), kont)) if isValue(v2) =>
        // NB: `v1` should be an lambda abstraction and `v2` is the argument of the application (closed by env2).
        // Evaluate the body of the lambda `v1` with argument `v2`.
        let Abs(x, body, loc) = v1;
        let argValue = StoredValue(v2, Environment(env2));
        let argEnvAddr = alloc(m0, k0);
        let bodyEnv = Map2.insert(x, argEnvAddr, env1);
        let sto1 = Store.weakUpdate(argEnvAddr, argValue, sto0);
            Set2.singleton(Next(State(body, Environment(bodyEnv), sto1, s0, f0, r0, l0, w0, q0, kont, tick(m0, k0))))

    //
    // Reference Expression.
    //
    case (State(Ref(e, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), _) =>
        // Allocate the `KRef` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KRef(kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    // Reference Kontinuation.
    //
    case (State(e, env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KRef(kont)) if isValue(e) =>
        // Allocate an address in the heap and store the value at that address.
        let freshAddr = alloc(m0, k0);
        let value = StoredValue(e, env0);
        let sto1 = Store.weakUpdate(freshAddr, value, sto0);
            Set2.singleton(Next(State(Ptr(freshAddr), Environment.empty(), sto1, s0, f0, r0, l0, w0, q0, kont, tick(m0, k0))))

    //
    // Dereference Expression.
    //
    case (State(Deref(e, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KDeref` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KDeref(kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    // Dereference Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KDeref(kont)) =>
        // Lookup the address `a` in the store.
        let storables = Store.lookup(a, sto0);
        let f = s -> match s with {
            case StoredKont(k)          => Abort("Non-value storable.")
            case StoredValue(e, env1)   => Next(State(e, env1, sto0, s0, f0, r0, l0, w0, q0, a, tick(m0, k0)))
        };
        Set2.map(f, storables)

    //
    // Sequence Expression.
    //
    case (State(Seq(e1, e2, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KSeq` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KSeq(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    // Sequence Kontinuation.
    //
    case (State(e1, env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KSeq(e2, env2, kont)) if isValue(e1) =>
        // Discard the value `e1`.
        Set2.singleton(Next(State(e2, env2, sto0, s0, f0, r0, l0, w0, q0, kont, tick(m0, k0))))

    //
    // Promisify Expression.
    //
    case (State(Promisify(e, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), kont) =>
        // Allocate the `KPromisify` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KPromisify(kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    // Promisify Continuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, l0, w0, q0, _, time), KPromisify(kont)) =>
        // Weakly update the state of the promise `a` to pending.
        let s1 = PromiseState.weakUpdate(a, Pending, s0);
            Set2.singleton(Next(State(Cst("Undef", L("-")), env0, sto0, s1, f0, r0, l0, w0, q0, kont, tick(m0, k0))))

    //
    // Resolve Expression.
    //
    case (State(Resolve(e1, e2, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), _) =>
        // Allocate the `KResolve1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KResolve1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    //  Resolve1 Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, l0, w0, q0, _, time), KResolve1(e2, env2, kont)) =>
        // Allocate the `KResolve2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KResolve2(a, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e2, env2, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    //  Resolve2 Kontinuation.
    //
    case (State(e2, env2, sto0, s0, f0, r0, l0, w0, q0, _, time), KResolve2(a, kont)) if isValue(e2) =>
        let f: PromiseValue -> Set2[Outcome] = pv -> match pv with {
            case Pending =>
                // The new promise value.
                let pv = Fulfilled(e2);

                // Set the promise state to fulfilled.
                let s1 = PromiseState.weakUpdate(a, pv, s0);

                // Clear out the fulfill reactions.
                // NB: Ignored - requires strong update analysis.

                // Clear out the rejected reactions.
                // NB: Ignored - requires strong update analysis.

                // Clear out the promise links.
                // NB: Ignored - requires strong update analysis.

               // Retrieve the promise links and append them to the link queue.
               let ls = PromiseLinks.lookup(a, l0);
               let w1 = LinkQueue.weakUpdate(Set2.map(l -> (pv, l), ls), w0);

               // Retrieve the fulfill reactions and append them to the reaction queue.
               let fr = FulfillReactions.lookup(a, f0);
               let q1 = ReactionQueue.weakUpdate(Set2.map(match (lam, env, addr) -> (pv, lam, env, addr), fr), q0);

               Set2.singleton(Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s1, f0, r0, l0, w1, q1, kont, tick(m0, k0))))

            case Fulfilled(v) =>
                // The promise has already been resolved with the value `v`. The resolve has no effect.
                Set2.singleton(Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

            case Rejected(v) =>
                // The promise has already been rejected with the value `v`. The resolve has no effect.
                Set2.singleton(Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

        };
        Set2.flatMap(f, PromiseState.lookup(a, s0))


    //
    //  OnResolve Expression.
    //
    case (State(OnResolve(e1, e2, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), _) =>
        // Allocate the `KOnResolve1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KOnResolve1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    //  OnResolve1 Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, l0, w0, q0, _, time), KOnResolve1(e2, env2, kont)) =>
        // Allocate the `KOnResolve2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KOnResolve2(a, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e2, env2, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    //  OnResolve2 Kontinuation.
    //
    case (State(e2, env2, sto0, s0, f0, r0, l0, w0, q0, _, time), KOnResolve2(a, kont)) if isValue(e2) =>
        let f: PromiseValue -> Set2[Outcome] = pv -> match pv with {
            case Pending =>
                // Allocate a child promise.
                let childAddr = alloc(m0, k0);

                // Allocate a fresh "object" in the heap. We use a string instead.
                let value = StoredValue(Cst("child", L("-")), Environment.empty());
                let sto1 = Store.weakUpdate(childAddr, value, sto0);

                // Set the child promise state to pending.
                let s1 = PromiseState.weakUpdate(childAddr, Pending, s0);

                // Update the fulfill reactions.
                let f1 = FulfillReactions.weakUpdate(a, e2, env2, childAddr, f0);

                Set2.singleton(Next(State(Ptr(childAddr), Environment.empty(), sto0, s1, f1, r0, l0, w0, q0, kont, tick(m0, k0))))

            case Fulfilled(v) =>
                // Allocate a child promise and enqueue the promise reaction `lam` with the resolved value `v`.
                let childAddr = alloc(m0, k0);
                let job = Set2.singleton((Fulfilled(v), e2, env2, childAddr));
                let q1 = ReactionQueue.weakUpdate(job, q0);
                    Set2.singleton(Next(State(Ptr(childAddr), Environment.empty(), sto0, s0, f0, r0, l0, w0, q1, kont, tick(m0, k0))))
            case Rejected(v) =>
                // NB: Conceptually similar to fulfilled.
                ???
        };
        Set2.flatMap(f, PromiseState.lookup(a, s0))

    //
    //  Link Expression.
    //
    case (State(Link(e1, e2, loc), env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), _) =>
        // Allocate the `KLink1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KLink1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e1, env0, sto1, s0, f0, r0, l0, w0, q0, kontAddr, tick(m0, k0))))

    //
    //  Link1 Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, l0, w0, q0, _, time), KLink1(e2, env2, kont)) =>
        // Allocate the `KLink2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KLink2(a, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            Set2.singleton(Next(State(e2, env2, sto1, s0, f0, r0, l0, w0, q0, a, tick(m0, k0))))

    //
    //  Link2 Kontinuation.
    //
    case (State(Ptr(a2), _, sto0, s0, f0, r0, l0, w0, q0, _, time), KLink2(a1, kont)) =>
        let l1 = PromiseLinks.weakUpdate(a1, a2, l0);
            Set2.singleton(Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s0, f0, r0, l1, w0, q0, kont, tick(m0, k0))))

    //
    // Reaction-Loop
    //
    case (State(v, env0, sto0, s0, f0, r0, l0, w0, q0, kptr, time), KEmpty) if isValue(v) =>
        let f: ((PromiseValue, Expression, Environment, Address)) -> Outcome = match (pv, exp, env, childAddr) -> {
            match pv with {
                case Fulfilled(v) =>
                        // Dequeue the item from the queue.
                        let q1 = ReactionQueue.strongDelete(pv, exp, env, childAddr, q0);

                        // Construct the expression: childAddr.resolve(exp(v))
                        let loc = L("ReactionLoop");
                        let e = Resolve(Ptr(childAddr), App(exp, v, loc), loc);
                            Next(State(e, Environment.empty(), sto0, s0, f0, r0, l0, w0, q1, kptr, tick(m0, k0)))
                case Rejected(v) => ??? // NB: Conceptually similar to the fulfilled case.
            }
         };

        ReactionQueue.map(f, q0)

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


namespace State {

    ///
    /// Returns `true` if the two given states `s1` and `s2` are equal.
    ///
    def eq(s1: State, s2: State): Bool =
        let State(e1, env1, sto1, s1, f1, r1, l1, w1, q1, kptr1, t1) = s1;
        let State(e2, env2, sto2, s2, f2, r2, l2, w2, q2, kptr2, t2) = s2;
            e1 == e2 &&
            Environment.eq(env1, env2) &&
            Store.eq(sto1, sto2) &&
            PromiseState.eq(s1, s2) &&
            FulfillReactions.eq(f1, f2) &&
            // RejectReactions.eq(r1, r2) && // NB: Not currently in use.
            PromiseLinks.eq(l1, l2) &&
            LinkQueue.eq(w1, w2) &&
            ReactionQueue.eq(q1, q2) &&
            kptr1 == kptr2 &&
            t1 == t2

}

namespace Environment {

    ///
    /// Returns the empty environment.
    ///
    def empty: Environment = Environment(Map2.empty())

    ///
    /// Returns `true` if the two given environments `env1` and `env2` are equal.
    ///
    def eq(env1: Environment, env2: Environment): Bool = ???

}

namespace Store {

    ///
    /// Returns `true` if the two given stores `sto1` and `sto2` are equal.
    ///
    def eq(sto1: Store, sto2: Store): Bool = ???

    ///
    /// Returns the set of storable values pointed-to by the (abstract) address `a`.
    ///
    def lookup(a: Address, sto: Store): Set2[Storable] =
        let Store(m) = sto;
            Option.getWithDefault(Map2.lookup(a, m), Set2.empty())

    ///
    /// Weakly updates the store `sto` with the storable value `v` associated with the (abstract) address `a`.
    ///
    def weakUpdate(a: Address, v: Storable, sto: Store): Store =
        let Store(sto0) = sto;
        let newValue = v;
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
            Store(Map2.insert(a, lubValue, sto0))

}


namespace PromiseState {

    ///
    /// Returns `true` if the two given promise states `s1` and `s2` are equal.
    ///
    def eq(s1: PromiseState, s2: PromiseState): Bool = ???

    ///
    /// Returns the set of promise values for the given (abstract) address `a`.
    ///
    def lookup(a: Address, s: PromiseState): Set2[PromiseValue] =
        let PromiseState(m) = s;
            Option.getWithDefault(Map2.lookup(a, m), Set2.empty())

    ///
    /// Weakly updates the promise states `s` with the promise value `v` for the (abstract) address `a`.
    ///
    def weakUpdate(a: Address, v: PromiseValue, s: PromiseState): PromiseState =
        let PromiseState(m) = s;
        let newValue = v;
        let oldValue = Option.getWithDefault(Map2.lookup(a, m), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
            PromiseState(Map2.insert(a, lubValue, m))

}

namespace FulfillReactions {

    ///
    /// Returns `true` if the two given fullfill reactions `f1` and `f2` are equal.
    ///
    def eq(f1: FulfillReactions, f2: FulfillReactions): Bool = ???

    ///
    /// Returns the (lambda, environment, childAddr)-triples associated with the given (abstract) address `a`.
    ///
    def lookup(a: Address, f: FulfillReactions): Set2[(Expression, Environment, Address)] =
        let FulfillReactions(m) = f;
            Option.getWithDefault(Map2.lookup(a, m), Set2.empty())

    ///
    /// Weaky updates the fulfillreactions `r` with the (lambda, environment, childAddr)-triple for the given (abstract) address `a`.
    ///
    def weakUpdate(a: Address, lam: Expression, env: Environment, childAddr: Address, r: FulfillReactions): FulfillReactions =
        let FulfillReactions(m) = r;
        let newValue = (lam, env, childAddr);
        let oldValue = Option.getWithDefault(Map2.lookup(a, m), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
            FulfillReactions(Map2.insert(a, lubValue, m))

}

namespace PromiseLinks {

    ///
    /// Returns `true` if the two given promise links `l1` and `l2` are equal.
    ///
    def eq(l1: PromiseLinks, l2: PromiseLinks): Bool = ???

    ///
    /// Returns the set of promise links associated with the given (abstract) address `a`.
    ///
    def lookup(a: Address, l: PromiseLinks): Set2[Address] =
        let PromiseLinks(m) = l;
            Option.getWithDefault(Map2.lookup(a, m), Set2.empty())

    ///
    /// Weakly updates the promise links `l` linking the (abstract) address `a1` to the (abstract) address `a2`.
    ///
    def weakUpdate(a1: Address, a2: Address, l: PromiseLinks): PromiseLinks =
        let PromiseLinks(m) = l;
        let newValue = Set2.singleton(a2);
        let oldValue = Option.getWithDefault(Map2.lookup(a1, m), Set2.empty());
        let lubValue = Set2.union(newValue, oldValue);
            PromiseLinks(Map2.insert(a1, lubValue, m))

}

namespace LinkQueue {

    ///
    /// Returns `true` if the two given link queues `w1` and `w2` are equal.
    ///
    def eq(w1: LinkQueue, w2: LinkQueue): Bool = ???

    ///
    /// Returns `true` if the link queue `w` is non empty.
    ///
    def nonEmpty(w: LinkQueue): Bool = match w with {
        case LinkQueue(Set2(Nil)) => false
        case _                    => true
    }

    ///
    /// Applies the given function `f` to all (promiseValue, promiseAddress)-pairs in the link queue `w`.
    ///
    def map(f: ((PromiseValue, Address)) -> Outcome, w: LinkQueue): Set2[Outcome] =
        let LinkQueue(xs) = w;
            Set2.map(f, xs)

    ///
    /// Weakly updates the link queue `w` with the given (promiseValue, promiseAddress)-pair.
    ///
    def weakUpdate(ls: Set2[(PromiseValue, Address)], w: LinkQueue): LinkQueue =
        let LinkQueue(w0) = w;
            LinkQueue(Set2.union(w0, ls))

    // TODO: Unsound, remove.
    def strongDelete(pv: PromiseValue, a: Address, w: LinkQueue): LinkQueue =
        let LinkQueue(Set2(xs)) = w;
            LinkQueue(Set2(List.remove((pv, a), xs)))

}

namespace ReactionQueue {

    ///
    /// Returns `true` if the two given reaction queues `q1` and `q2` are equal.
    ///
    def eq(q1: ReactionQueue, q2: ReactionQueue): Bool = ???

    ///
    /// Applies the given function `f` to the set (PromiseValue, Lambda, Environment, childAddress)-tuples in the reaction queue `q`.
    ///
    def map(f: ((PromiseValue, Expression, Environment, Address)) -> Outcome, q: ReactionQueue): Set2[Outcome] =
        let ReactionQueue(xs) = q;
            Set2.map(f, xs)

    ///
    /// Weakly updates the reaction queue `q` with the given (PromiseValue, Lambda, Environment, childAddress)-tuple.
    ///
    def weakUpdate(rs: Set2[(PromiseValue, Expression, Environment, Address)], q: ReactionQueue): ReactionQueue =
        let ReactionQueue(xs) = q;
            ReactionQueue(Set2.union(rs, xs))

    // TODO: Unsound, remove.
    def strongDelete(pv: PromiseValue, e: Expression, env: Environment, a: Address, q: ReactionQueue): ReactionQueue =
        let ReactionQueue(Set2(xs)) = q;
            ReactionQueue(Set2(List.remove((pv, e, env, a), xs)))

}


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
        Seq(Promisify(Var(V(0), L("l2")), L("l2")),
            Seq(OnResolve(Var(V(0), L("l3")), Abs(V(42), Ref(Var(V(42), L("l3")), L("l3")), L("l3")), L("l3")),
                Resolve(Var(V(0), L("l4")), Cst("hello", L("l4")), L("l4"))
            , L("l5")), L("l5"))
        , L("l6")),
        Ref(Cst("Promise1", L("l7")), L("l7")), L("l7")))
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
def testEval2: Set2[Outcome] = eval(inject(
    App(App(Abs(V(0), Abs(V(1),
        Seq(Promisify(Var(V(0), L("l2")), L("l2")),
            Seq(Promisify(Var(V(1), L("l3")), L("l3")),
                Seq(Link(Var(V(0), L("l4")), Var(V(1), L("l4")), L("l4")),
                    Seq(OnResolve(Var(V(1), L("l2")), Abs(V(42), Ref(Var(V(42), L("l5")), L("l5")), L("l5")), L("l5")),
                        Resolve(Var(V(0), L("l6")), Cst("hello", L("l6")), L("l6"))
                    , L("l7"))
                , L("l8"))
            , L("l9"))
        , L("l10")), L("l10")), L("l10")),
        Ref(Cst("Promise1", L("l11")), L("l11")), L("l11")), Ref(Cst("Promise2", L("l11")), L("l11")), L("l11"))))







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

     def eq[a](xs: Set2[a], ys: Set2[a]): Bool = ???

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
