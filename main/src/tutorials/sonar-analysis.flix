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
    case KOnResolve2(Expression, Environment, Address),

    // NB: KOnReject1 and KOnReject2 are conceptually similar to KOnResolve1 and KOnResolve2.

    // A continuation that represents the `[].link(e)` context where the expression `e` is closed by the environment.
    case KLink1(Expression, Environment, Address),

    // A continuation that represents the `v.link([])` context where the value `e` is closed by the environment.
    case KLink2(Expression, Environment, Address)
}

///
/// The promise state maps addresses to promise values.
///
type PromiseState = PromiseState(Map2[Address, PromiseValue])

///
/// The fulfill reactions map addresses to a list of (Lam, Address)-pairs where the address points-to the child promise.
///
type FulfillReactions = FulfillReactions(Map2[Address, List[(Expression, Address)]]) // TODO: Should this be a set? // TODO: Does expression need to be paired with an environment?

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
/// The reaction queue is a list of (PromiseValue, Lam, Address)-triples where the address points to the child promise.
///
type ReactionQueue = ReactionQueue(List[(PromiseValue, Expression, Address)]) // TODO: Should this be a set? // TODO: Does expression need to be paired with an environment?

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
type State = State(Expression, Environment, Store, Address, Time)

///
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): State =
    // Save the empty continuation into the store.
    let kont = Kontinuation.Empty;
    let kptr = A(0);
    let env0 = Map2.empty();
    let sto0 = Map2.insert(kptr, Set2.singleton(StoredKont(kont)), Map2.empty());
    let time = Time(1);
        State(e0, Environment(env0), Store(sto0), kptr, time)

///
/// Returns the next timestamp for the machine in the given state `m0` and continuation `k0`.
///
def tick(m0: State, k0: Kontinuation): Time = match m0 with {
    case State(_, _, _, _, Time(t)) => Time(t + 1)
}

///
/// Returns the next address for the machine in the given state `m0` and continuation `c0`.
///
def alloc(m0: State, k0: Kontinuation): Address = match m0 with {
    case State(_, _, _, _, Time(t)) => A(t)
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
    // Variable Expression.
    //
    case (State(Var(x), Environment(env0), Store(sto0), kptr, time), _) =>
        // The expression is a variable. Look it up in the environment.
         match Map2.lookup(x, env0) with {
            case None => Set2(Abort("Unbound variable.") :: Nil)
            case Some(ptr) =>
                let values = Option.getWithDefault(Map2.lookup(ptr, sto0), Set2.empty());
                let f = storable -> match storable with {
                    case StoredKont(_) => Abort("Non-value storable.")
                    case StoredValue(v, env1) => Next(State(v, env1, Store(sto0), kptr, tick(m0, k0)))
                };
                Set2.map(f, values)
         }

    //
    // Application Expression.
    //
    case (State(App(e1, e2), Environment(env0), Store(sto0), kptr, time), _) =>
        // Continue evaluation of `e1` and store `e2` in a continuation.
        let k = KApp1(e2, Environment(env0), kptr);
        // Generate a fresh address where to store the continuation.
        let a = alloc(m0, k0);
        // Store the continuation.
        let newValue = StoredKont(k);
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
        let sto1 = Map2.insert(a, lubValue, sto0);
            Set2.singleton(Next(State(e1, Environment(env0), Store(sto1), a, tick(m0, k0))))

    //
    // Application1 Kontinuation.
    //
    case (State(v1, Environment(env0), Store(sto0), _, time), KApp1(e2, env2, kont)) if isValue(v1) =>
        // Continue evaluation of `e2` and store `v1` in a continuation.
        let k = KApp2(v1, Environment(env0), kont);
        // Generate a fresh address where to store the continuation.
        let a = alloc(m0, k0);
        // Store the continuation.
        let newValue = StoredKont(k);
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
        let sto1 = Map2.insert(a, lubValue, sto0);
            Set2.singleton(Next(State(e2, env2, Store(sto1), a, tick(m0, k0))))

    //
    // Application2 Kontinuation.
    //
    case (State(v2, Environment(env0), Store(sto0), _, time), KApp2(v1, Environment(env1), kont)) if isValue(v2) =>
        // Evaluate the body of the lambda `v1` with argument `v2`.
        let Abs(x, body) = v1;
        let a = alloc(m0, k0);
        let env2 = Map2.insert(x, a, env1);
        let newValue = StoredValue(v2, Environment(env0));
        let oldValue = Option.getWithDefault(Map2.lookup(a, sto0), Set2.empty());
        let lubValue = Set2.union(Set2.singleton(newValue), oldValue);
        let sto1 = Map2.insert(a, lubValue, sto0);
            Set2.singleton(Next(State(body, Environment(env2), Store(sto1), kont, tick(m0, k0))))

    //
    // Stuck.
    //
    case _ => Set2.singleton(Done(m0))
}

///
/// Returns all possible transitions from the given state `m0`.
///
def stepAll(m0: State): Set2[Outcome] = match m0 with {
    case State(exp0, env0, Store(sto0), kptr, time) =>
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
// We can now test our CEK machine!
//
def testEval1: Set2[Outcome] = run(Var(V(0)))                                                      // x
def testEval2: Set2[Outcome] = run(Abs(V(0), Var(V(0))))                                           // λx. x
def testEval3: Set2[Outcome] = run(Abs(V(0), Abs(V(1), Var(V(0)))))                                // λx. λy. x
def testEval4: Set2[Outcome] = run(App(Abs(V(0), Var(V(0))), Abs(V(1), Var(V(1)))))                // (λx. x) (λy. y)
def testEval5: Set2[Outcome] = run(App(Abs(V(0), Abs(V(1), Var(V(0)))), Abs(V(2), Var(V(2)))))     // (λx. λy. x) (λz. z)
def testEval6: Set2[Outcome] = run(App(App(Abs(V(0), Abs(V(1), App(Var(V(0)), Var(V(1))))), Abs(V(2), Var(V(2)))), Abs(V(3), Var(V(3)))))     // ((λx. λy. x y) (λz. z)) (λw. w)

//
// Here is the omega-combinator: (λx. x x) (λx. x x)
//
def testOmega: Set2[Outcome] = run(
    App(
        Abs(V(0), App(Var(V(0)), Var(V(0)))),
        Abs(V(0), App(Var(V(0)), Var(V(0))))))





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
