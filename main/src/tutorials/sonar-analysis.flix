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
    case Ptr(Address),

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
/// Returns the label (source location) of the given expression `e`.
///
def getLabel(e: Expression): Label = match e with {
    case Var(_, l)          => l
    case Abs(_, _, l)       => l
    case App(_, _, l)       => l
    case Cst(_, l)          => l
    case Ref(_, l)          => l
    case Deref(_, l)        => l
    case Seq(_, _, l)       => l
    case Promisify(_, l)    => l
    case Resolve(_, _, l)   => l
    case Reject(_, _, l)    => l
    case OnResolve(_, _, l) => l
    case OnReject(_, _, l)  => l
    case Link(_, _, l)      => l
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
type Environment = Environment(Map[Variable, Address])

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
type Store = Store(Map[Address, Set[Storable]])

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
type PromiseState = PromiseState(Map[Address, Set[PromiseValue]])

///
/// The fulfill reactions map addresses to a set of (Lambda, Environment, Address)-triples where the address points-to the child promise.
///
type FulfillReactions = FulfillReactions(Map[Address, Set[(Expression, Environment, Address)]])

///
/// The reject reactions map addresses to a set of (Lambda, Environment, Address)-triples where the address points-to the child promise.
///
type RejectReactions = RejectReactions(Map[Address, Set[(Expression, Address)]])

///
/// The reaction queue is a set of (PromiseValue, Lambda, Environment, Address)-tuples where the address points to the child promise.
///
type Queue = Queue(Set[(PromiseValue, Expression, Environment, Address)])

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
/// The edges of the promise graph.
///
enum Edge {

    // A resolve edge from a value allocation site (label) to a promise allocation site (label).
    case ResolveEdge(/* Value */ Expression, Address),

    // A reject edge from a value allocation site (label) to a promise allocation site (label).
    case RejectEdge(/* Value */ Expression, Address), // NB: Currently unused.

    // A resolve registration edge from a promise allocation site (label) to lambda (label).
    case ResolveRegistration(Address, Label),

    // A reject registration edge from a promise allocation site (label) to lambda (label).
    case RejectRegistration(Address, Label), // NB: Currently unused.

    // A link edge from a promise allocation site (label) to a promise allocation site (label).
    case LinkEdge(Address, Address)

}

///
/// The Promise Graph.
///
type PromiseGraph = PromiseGraph(Set[Edge])

///
/// The timestamp associated with a machine configuration.
///
type Time = Time(Int)

///
/// A machine state configuration.
///
/// NB: Note that the Address component is a pointer to continuation in the store.
///
type State = State(Expression, Environment, Store, PromiseState, FulfillReactions, RejectReactions, Queue, PromiseGraph, Address, Time)

///
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): State =
    // Save the empty continuation into the store.
    let kont = Kontinuation.KEmpty;
    let kptr = A(0);
    let env0 = Map.empty();
    let sto0 = Map.insert(kptr, #{StoredKont(kont)}, Map.empty());
    let s0 = Map.empty();
    let f0 = Map.empty();
    let r0 = Map.empty();
    let l0 = Map.empty();
    let w0 = Set.empty();
    let q0 = Set.empty();
    let time = Time(1);
    let g0 = PromiseGraph.empty();
        State(e0, Environment(env0), Store(sto0), PromiseState(s0), FulfillReactions(f0), RejectReactions(r0), Queue(q0), g0, kptr, time)

///
/// Returns the next timestamp for the machine in the given state `m0` and continuation `k0`.
///
def tick(m0: State, k0: Kontinuation): Time = match m0 with {
    case State(_, _, _, _, _, _, _, _, _, Time(t)) => Time(t + 1)
}

///
/// Returns the next address for the machine in the given state `m0` and continuation `c0`.
///
def alloc(m0: State, k0: Kontinuation): Address = match m0 with {
    case State(_, _, _, _, _, _, _, _, _, Time(t)) => A(t)
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
def step(m0: State, k0: Kontinuation): Set[Outcome] = match (m0, k0) with {

    //
    // Variable Expression.
    //
    case (State(Var(x, loc), Environment(env0), sto0, s0, f0, r0, q0, g0, kptr, time), _) =>
        // The expression is a variable. Look it up in the environment.
         match Map.get(x, env0) with {
            case None => #{ Abort("Unbound variable.") }
            case Some(a) =>
                let values = Store.lookup(a, sto0);
                let f = storable -> match storable with {
                    case StoredKont(_) => Abort("Non-value storable.")
                    case StoredValue(v, env1) => Next(State(v, env1, sto0, s0, f0, r0, q0, g0, kptr, tick(m0, k0)))
                };
                Set.map(f, values)
         }

    //
    // Application Expression.
    //
    case (State(App(e1, e2, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), _) =>
        // Allocate the `KApp1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KApp1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e1, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    // Application1 Kontinuation.
    //
    case (State(v1, env0, sto0, s0, f0, r0, q0, g0, _, time), KApp1(e2, env2, kont)) if isValue(v1) =>
        // NB: `v1` should be a lambda abstraction.
        // Allocate the `KApp2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KApp2(v1, env0, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e2, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    // Application2 Kontinuation.
    //
    case (State(v2, Environment(env2), sto0, s0, f0, r0, q0, g0, _, time), KApp2(v1, Environment(env1), kont)) if isValue(v2) =>
        // NB: `v1` should be an lambda abstraction and `v2` is the argument of the application (closed by env2).
        // Evaluate the body of the lambda `v1` with argument `v2`.
        let Abs(x, body, loc) = v1;
        let argValue = StoredValue(v2, Environment(env2));
        let argEnvAddr = alloc(m0, k0);
        let bodyEnv = Map.insert(x, argEnvAddr, env1);
        let sto1 = Store.weakUpdate(argEnvAddr, argValue, sto0);
            #{ Next(State(body, Environment(bodyEnv), sto1, s0, f0, r0, q0, g0, kont, tick(m0, k0))) }

    //
    // Reference Expression.
    //
    case (State(Ref(e, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), _) =>
        // Allocate the `KRef` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KRef(kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    // Reference Kontinuation.
    //
    case (State(e, env0, sto0, s0, f0, r0, q0, g0, kptr, time), KRef(kont)) if isValue(e) =>
        // Allocate an address in the heap and store the value at that address.
        let freshAddr = alloc(m0, k0);
        let value = StoredValue(e, env0);
        let sto1 = Store.weakUpdate(freshAddr, value, sto0);
            #{ Next(State(Ptr(freshAddr), Environment.empty(), sto1, s0, f0, r0, q0, g0, kont, tick(m0, k0)))}

    //
    // Dereference Expression.
    //
    case (State(Deref(e, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), kont) =>
        // Allocate the `KDeref` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KDeref(kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    // Dereference Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, q0, g0, kptr, time), KDeref(kont)) =>
        // Lookup the address `a` in the store.
        let storables = Store.lookup(a, sto0);
        let f = s -> match s with {
            case StoredKont(k)          => Abort("Non-value storable.")
            case StoredValue(e, env1)   => Next(State(e, env1, sto0, s0, f0, r0, q0, g0, kont, tick(m0, k0)))
        };
        Set.map(f, storables)

    //
    // Sequence Expression.
    //
    case (State(Seq(e1, e2, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), kont) =>
        // Allocate the `KSeq` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KSeq(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e1, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    // Sequence Kontinuation.
    //
    case (State(e1, env0, sto0, s0, f0, r0, q0, g0, kptr, time), KSeq(e2, env2, kont)) if isValue(e1) =>
        // Discard the value `e1`.
        #{ Next(State(e2, env2, sto0, s0, f0, r0, q0, g0, kont, tick(m0, k0))) }

    //
    // Promisify Expression.
    //
    case (State(Promisify(e, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), kont) =>
        // Allocate the `KPromisify` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KPromisify(kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    // Promisify Continuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, q0, g0, _, time), KPromisify(kont)) =>
        // Weakly update the state of the promise `a` to pending.
        let s1 = PromiseState.weakUpdate(a, Pending, s0);
            #{ Next(State(Cst("Undef", L("-")), env0, sto0, s1, f0, r0, q0, g0, kont, tick(m0, k0))) }

    //
    // Resolve Expression.
    //
    case (State(Resolve(e1, e2, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), _) =>
        // Allocate the `KResolve1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KResolve1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e1, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    //  Resolve1 Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, q0, g0, _, time), KResolve1(e2, env2, kont)) =>
        // Allocate the `KResolve2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KResolve2(a, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e2, env2, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    //  Resolve2 Kontinuation.
    //
    case (State(e2, env2, sto0, s0, f0, r0, q0, g0, _, time), KResolve2(a, kont)) if isValue(e2) =>
        let f: PromiseValue -> Set[Outcome] = pv -> match pv with {
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

               // Retrieve the fulfill reactions and append them to the reaction queue.
               let fr = FulfillReactions.lookup(a, f0);
               let q1 = Queue.weakUpdate(Set.map(match (lam, env, addr) -> (pv, lam, env, addr), fr), q0);

                // Update the promise graph.
                let g1 = PromiseGraph.addEdge(ResolveEdge(e2, a), g0);

               #{ Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s1, f0, r0, q1, g1, kont, tick(m0, k0))) }

            case Fulfilled(v) =>
                // The promise has already been resolved with the value `v`. The resolve has no effect.
                #{ Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s0, f0, r0, q0, g0, kont, tick(m0, k0))) }

            case Rejected(v) =>
                // The promise has already been rejected with the value `v`. The resolve has no effect.
                #{ Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s0, f0, r0, q0, g0, kont, tick(m0, k0))) }

        };
        Set.flatMap(f, PromiseState.lookup(a, s0))


    //
    //  OnResolve Expression.
    //
    case (State(OnResolve(e1, e2, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), _) =>
        // Allocate the `KOnResolve1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KOnResolve1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e1, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    //  OnResolve1 Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, q0, g0, _, time), KOnResolve1(e2, env2, kont)) =>
        // Allocate the `KOnResolve2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KOnResolve2(a, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e2, env2, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    //  OnResolve2 Kontinuation.
    //
    case (State(e2, env2, sto0, s0, f0, r0, q0, g0, _, time), KOnResolve2(a, kont)) if isValue(e2) =>
        let f: PromiseValue -> Set[Outcome] = pv -> match pv with {
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

                // Update the promise graph.
                let g1 = PromiseGraph.addEdge(ResolveRegistration(a, getLabel(e2)), g0);

                #{ Next(State(Ptr(childAddr), Environment.empty(), sto0, s1, f1, r0, q0, g1, kont, tick(m0, k0))) }

            case Fulfilled(v) =>
                // Allocate a child promise and enqueue the promise reaction `lam` with the resolved value `v`.
                let childAddr = alloc(m0, k0);
                let job = #{ (Fulfilled(v), e2, env2, childAddr) };
                let q1 = Queue.weakUpdate(job, q0);
                    #{ Next(State(Ptr(childAddr), Environment.empty(), sto0, s0, f0, r0, q1, g0, kont, tick(m0, k0))) }
            case Rejected(v) =>
                ??? // NB: Conceptually similar to fulfilled.
        };
        Set.flatMap(f, PromiseState.lookup(a, s0))

    //
    //  Link Expression.
    //
    case (State(Link(e1, e2, loc), env0, sto0, s0, f0, r0, q0, g0, kptr, time), _) =>
        // Allocate the `KLink1` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KLink1(e2, env0, kptr));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e1, env0, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    //  Link1 Kontinuation.
    //
    case (State(Ptr(a), env0, sto0, s0, f0, r0, q0, g0, _, time), KLink1(e2, env2, kont)) =>
        // Allocate the `KLink2` continuation in the store.
        let kontAddr = alloc(m0, k0);
        let nextKont = StoredKont(KLink2(a, kont));
        let sto1 = Store.weakUpdate(kontAddr, nextKont, sto0);
            #{ Next(State(e2, env2, sto1, s0, f0, r0, q0, g0, kontAddr, tick(m0, k0))) }

    //
    //  Link2 Kontinuation.
    //
    case (State(Ptr(a2), _, sto0, s0, f0, r0, q0, g0, _, time), KLink2(a1, kont)) =>
        // Add identity fulfill and reject reactions where `a2` is the dependent promise.
        let lam = Abs(V(0), Var(V(0), L("id")), L("id"));
        let env = Environment.empty();
        let f1 = FulfillReactions.weakUpdate(a1, lam, env, a2, f0);
        let g1 = PromiseGraph.addEdge(LinkEdge(a1, a2), g0);
            #{ Next(State(Cst("Undef", L("-")), Environment.empty(), sto0, s0, f1, r0, q0, g1, kont, tick(m0, k0))) }

    //
    // Loop
    //
    case (State(v, env0, sto0, s0, f0, r0, q0, g0, kptr, time), KEmpty) if isValue(v) =>
        let f: ((PromiseValue, Expression, Environment, Address)) -> Outcome = match (pv, exp, env, childAddr) -> {
            match pv with {
                case Fulfilled(v) =>
                        // Dequeue the item from the queue.
                        let q1 = Queue.strongDelete(pv, exp, env, childAddr, q0);

                        // Construct the expression: childAddr.resolve(exp(v))
                        let loc = L("ReactionLoop");
                        let e = Resolve(Ptr(childAddr), App(exp, v, loc), loc);
                            Next(State(e, Environment.empty(), sto0, s0, f0, r0, q1, g0, kptr, tick(m0, k0)))
                case Rejected(v) => ??? // NB: Conceptually similar to the fulfilled case.
            }
         };

        Queue.map(f, q0)

    //
    // Stuck.
    //
    case _ => let foo = println!(k0); #{ Done(m0) }
}

///
/// Returns all possible transitions from the given state `m0`.
///
def stepAll(m0: State): Set[Outcome] = match m0 with {
    case State(_, _, Store(sto0), _, _, _, _, _, kptr, time) =>
        let values = Map.getWithDefault(kptr, Set.empty(), sto0);
        let f = value -> match value with {
            case StoredKont(k) => step(m0, k)
            case _             => #{}
        };
        Set.flatMap(f, values)
}

///
/// Computes the fixed-point of the transition relation defined by `stepAll` starting from the given outcomes `xs`.
///
def eval(xs: Set[Outcome]): Set[Outcome] =
    let f = outcome -> match outcome with {
        case Next(m)        => stepAll(m)
        case Done(m)        => #{Done(m)}
        case Abort(e)       => #{Abort(e)}
    };
    let ys = Set.flatMap(f, xs);
    if (ys `Outcome.isSubsetOf` xs) // NB: Uses custom isSubsetOf due to custom equality.
        xs
    else
        eval(xs `Set.union` ys)

///
/// Evaluates the given expression to a value by injecting it into the initial state of the CEK machine
/// and repeatedly performing a transition until the machine halts with a value (or aborts).
///
def run(e0: Expression): Set[Outcome] = eval(#{Next(inject(e0))})

///
/// Returns the edges of the promise graph of the given state `m0`.
///
def getPromiseGraphEdges(m0: State): Set[Edge] =
    let State(_, _, _, _, _, _, _, g0, _, _) = m0;
    let PromiseGraph(edges) = g0;
        edges

///
/// Returns the promise graph of the given set of outcomes `xs`.
///
def getPromiseGraph(xs: Set[Outcome]): Set[Edge] =
    let f = outcome -> match outcome with {
        case Next(s)  => getPromiseGraphEdges(s)
        case Done(s)  => getPromiseGraphEdges(s)
        case Abort(e) => Set.empty()
    };
    Set.flatMap(f, xs)


namespace Outcome {

    ///
    /// Returns `true` if the two given outcomes `o1` and `o2` are equal.
    ///
    def eq(o1: Outcome, o2: Outcome): Bool = match (o1, o2) with {
        case (Next(s1), Next(s2))   => State.eq(s1, s2)
        case (Done(s1), Done(s2))   => State.eq(s1, s2)
        case (Abort(r1), Abort(r2)) => true
        case _                      => false
    }

    ///
    /// Returns `true` if `xs` is a subset of the set `ys`.
    ///
    def isSubsetOf(xs: Set[Outcome], ys: Set[Outcome]): Bool =
        Set.forall(x -> Set.exists(y -> eq(x, y), ys), xs)

}

namespace State {

    ///
    /// Returns `true` if the two given states `s1` and `s2` are equal.
    ///
    def eq(s1: State, s2: State): Bool =
        let State(e1, env1, sto1, s1, f1, r1, q1, g1, kptr1, t1) = s1;
        let State(e2, env2, sto2, s2, f2, r2, q2, g2, kptr2, t2) = s2;
            e1 == e2 &&
            Environment.eq(env1, env2) &&
            Store.eq(sto1, sto2) &&
            PromiseState.eq(s1, s2) &&
            FulfillReactions.eq(f1, f2) &&
            // RejectReactions.eq(r1, r2) && // NB: Not currently in use.
            Queue.eq(q1, q2) &&
            kptr1 == kptr2 &&
            t1 == t2

}

namespace Environment {

    ///
    /// Returns the empty environment.
    ///
    def empty: Environment = Environment(Map.empty())

    ///
    /// Returns `true` if the two given environments `env1` and `env2` are equal.
    ///
    def eq(env1: Environment, env2: Environment): Bool =
        let Environment(x) = env1;
        let Environment(y) = env2;
            Map.eq(x, y)

}

namespace Store {

    ///
    /// Returns the set of storable values pointed-to by the (abstract) address `a`.
    ///
    // TODO: Naming of this function
    def lookup(a: Address, sto: Store): Set[Storable] =
        let Store(m) = sto;
            Map.getWithDefault(a, Set.empty(), m)

    ///
    /// Weakly updates the store `sto` with the storable value `v` associated with the (abstract) address `a`.
    ///
    def weakUpdate(a: Address, v: Storable, sto: Store): Store =
        let Store(sto0) = sto;
        let newValue = v;
        let oldValue = Map.getWithDefault(a, Set.empty(), sto0);
        let lubValue = Set.union(#{newValue}, oldValue);
            Store(Map.insert(a, lubValue, sto0))

    ///
    /// Returns `true` if the two given stores `sto1` and `sto2` are equal.
    ///
    def eq(sto1: Store, sto2: Store): Bool =
        let Store(x) = sto1;
        let Store(y) = sto2;
            Map.eq(x, y)

}


namespace PromiseState {

    ///
    /// Returns the set of promise values for the given (abstract) address `a`.
    ///
    def lookup(a: Address, s: PromiseState): Set[PromiseValue] =
        let PromiseState(m) = s;
            Map.getWithDefault(a, Set.empty(), m)

    ///
    /// Weakly updates the promise states `s` with the promise value `v` for the (abstract) address `a`.
    ///
    def weakUpdate(a: Address, v: PromiseValue, s: PromiseState): PromiseState =
        let PromiseState(m) = s;
        let newValue = v;
        let oldValue = Map.getWithDefault(a, Set.empty(), m);
        let lubValue = Set.union(#{newValue}, oldValue);
            PromiseState(Map.insert(a, lubValue, m))

    ///
    /// Returns `true` if the two given promise states `s1` and `s2` are equal.
    ///
    def eq(s1: PromiseState, s2: PromiseState): Bool =
        let PromiseState(x) = s1;
        let PromiseState(y) = s2;
            Map.eq(x, y)
}

namespace FulfillReactions {

    ///
    /// Returns the (lambda, environment, childAddr)-triples associated with the given (abstract) address `a`.
    ///
    def lookup(a: Address, f: FulfillReactions): Set[(Expression, Environment, Address)] =
        let FulfillReactions(m) = f;
            Map.getWithDefault(a, Set.empty(), m)

    ///
    /// Weaky updates the fulfillreactions `r` with the (lambda, environment, childAddr)-triple for the given (abstract) address `a`.
    ///
    def weakUpdate(a: Address, lam: Expression, env: Environment, childAddr: Address, r: FulfillReactions): FulfillReactions =
        let FulfillReactions(m) = r;
        let newValue = (lam, env, childAddr);
        let oldValue = Map.getWithDefault(a, Set.empty(), m);
        let lubValue = Set.union(#{newValue}, oldValue);
            FulfillReactions(Map.insert(a, lubValue, m))

    ///
    /// Returns `true` if the two given fullfill reactions `f1` and `f2` are equal.
    ///
    def eq(f1: FulfillReactions, f2: FulfillReactions): Bool =
        let FulfillReactions(x) = f1;
        let FulfillReactions(y) = f2;
            Map.eq(x, y)

}

namespace Queue {

    ///
    /// Applies the given function `f` to the set (PromiseValue, Lambda, Environment, childAddress)-tuples in the reaction queue `q`.
    ///
    def map(f: ((PromiseValue, Expression, Environment, Address)) -> Outcome, q: Queue): Set[Outcome] =
        let Queue(xs) = q;
            Set.map(f, xs)

    ///
    /// Weakly updates the reaction queue `q` with the given (PromiseValue, Lambda, Environment, childAddress)-tuple.
    ///
    def weakUpdate(rs: Set[(PromiseValue, Expression, Environment, Address)], q: Queue): Queue =
        let Queue(xs) = q;
            Queue(Set.union(rs, xs))

    // TODO: Unsound, remove.
    def strongDelete(pv: PromiseValue, e: Expression, env: Environment, a: Address, q: Queue): Queue =
        let Queue(Set(xs)) = q;
            Queue(Set(List.filter(x -> x != (pv, e, env, a), xs)))

    ///
    /// Returns `true` if the two given reaction queues `q1` and `q2` are equal.
    ///
    def eq(q1: Queue, q2: Queue): Bool =
        let Queue(x) = q1;
        let Queue(y) = q2;
            Set.eq(x, y)

}

namespace PromiseGraph {

    ///
    /// Returns the empty promise graph.
    ///
    def empty: PromiseGraph = PromiseGraph(Set.empty())

    ///
    /// Adds the given edge to the given promise graph.
    ///
    def addEdge(e: Edge, g: PromiseGraph): PromiseGraph =
        let PromiseGraph(xs) = g;
            PromiseGraph(Set.insert(e, xs))

}

//
// Example 1:
//
// var x = Ref("Promise1")
// promisify(x)
// x.onResolve(y => Ref(y))
// x.resolve("hello")
//
def testEval1: Set[Edge] = getPromiseGraph(run(
    App(Abs(V(0),
        Seq(Promisify(Var(V(0), L("l2")), L("l2")),
            Seq(OnResolve(Var(V(0), L("l3")), Abs(V(42), Ref(Var(V(42), L("l3")), L("l3")), L("l3")), L("l3")),
                Resolve(Var(V(0), L("l4")), Cst("hello", L("l4")), L("l4"))
            , L("l5")), L("l5"))
        , L("l6")),
        Ref(Cst("Promise1", L("l7")), L("l7")), L("l7"))))


//
// Example 2:
//
// var x = Ref("Promise1")
// var y = Ref("Promise2")
// promisify(x)
// promisify(y)
// x.link(y)
// var z = y.onResolve(z => Ref(z))
// x.resolve("hello")
//
def testEval2: Set[Edge] = getPromiseGraph(run(
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
