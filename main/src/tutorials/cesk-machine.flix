//
// In this tutorial we will implement the Control-Environment-Store-Kontinuation (CESK) machine.
//
// Be sure to check out the Lambda calculus tutorial before starting this tutorial.
//
// The ideas developed here are based on the paper "Abstracting Abstract Machines" by Horn and Might.
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
    case App(Expression, Expression)
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
/// An environment is a map from variables to addresses.
///
type Environment = Environment(Map2[Variable, Address])

///
/// A store is a map from addresses to (value, environment)-pairs.
///
type Store = Store(Map2[Address, (Expression, Environment)])

///
/// The CEK-machine operates based on continuations derived
//  from the evaluation contexts of the language.
///
enum Kontinuation {
    // Represents the empty evaluation context (named `mt` in the paper).
    case KEmpty,

    // Represents the evaluation context ([] e) with continuation κ
    // where the environment ρ closes the expression e.
    // NB: In the paper this constructor is named `ar`.
    case KApp1(Expression, Environment, Kontinuation),

    // Represents the evaluation context (v []) with continuation κ
    // where the environment ρ closes the the value v.
    // NB: In the paper this constructor is named `fn`.
    case KApp2(Expression, Environment, Kontinuation)
}

///
/// A machine state configuration.
///
type State = State(Expression, Environment, Store, Kontinuation)

///
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): State =
    State(e0, Environment(Map2.empty()), Store(Map2.empty()), Kontinuation.KEmpty)

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
/// Returns the outcome of a single transition for the machine in state `m`.
///
/// (The transition rules are described in Figure 1 in the paper.)
///
def step(m0: State): Outcome = match m0 with {

    //
    // Variable Expression.
    //
    case State(Var(x), Environment(env0), Store(sto0), kont) =>
        // The expression is a variable. Look it up in the environment.
         match Map2.lookup(x, env0) with {
            case None => Abort("Unbound variable.")
            case Some(ptr) => match Map2.lookup(ptr, sto0) with {
                case None => Abort("Unbound address.")
                case Some((v, env1)) => Next(State(v, env1, Store(sto0), kont))
            }
         }

    //
    // Application Expression.
    //
    case State(App(e1, e2), Environment(env0), Store(sto0), kont) =>
        // Continue evaluation of `e1` and store `e2` in a kontinuation.
        let k = KApp1(e2, Environment(env0), kont);
            Next(State(e1, Environment(env0), Store(sto0), k))

    //
    // Application1 Kontinuation.
    //
    case State(v1, Environment(env0), Store(sto0), KApp1(e2, env2, kont)) if isValue(v1) =>
        // Continue evaluation of `e2` and store `v1` in a kontinuation.
        let k = KApp2(v1, Environment(env0), kont);
            Next(State(e2, env2, Store(sto0), k))

    //
    // Application2 Kontinuation.
    //
    case State(v2, Environment(env0), Store(sto0), KApp2(v1, Environment(env1), kont)) if isValue(v2) =>
        // Evaluate the body of the lambda `v1` with argument `v2`.
        let Abs(x, body) = v1;
        let a = freshAddr();
        let env2 = Map2.insert(x, a, env1);
        let sto1 = Map2.insert(a, (v2, Environment(env0)), sto0);
            Next(State(body, Environment(env2), Store(sto1), kont))

    //
    // Stuck.
    //
    case _ => Done(m0)
}


///
/// Repeatedly performs single-step transitions in the given machine `m0` until the machine halts with a value.
///
def eval(m0: State): Result[State, Str] =
    match println!(step(m0)) with {
        case Next(m)        => eval(m)
        case Done(m)        => Ok(m)
        case Abort(e)       => Err(e)
    }

///
/// Evaluates the given expression to a value by injecting it into the initial state of the CEK machine
/// and repeatedly performing a transition until the machine halts with a value (or aborts).
///
def run(e0: Expression): Result[Expression, Str] =
    let result = eval(inject(e0));
        Result.map(match State(exp, env, sto, kont) -> substitute(exp, env, sto), result)

///
/// Substitutes every free variable in the given expression `e0` for the value in the given environment `env0`.
///
def substitute(e0: Expression, env0: Environment, sto0: Store): Expression = match (e0, env0, sto0) with {
    case (Var(x), Environment(env), Store(sto)) => match Map2.lookup(x, env) with {
        case None               => Var(x)
        case Some(a)            => match Map2.lookup(a, sto) with {
            case None => Var(x)
            case Some((e, env1)) => substitute(e, env1, sto0)
        }
    }

    case (Abs(x, e), Environment(env), Store(sto)) =>
        Abs(x, substitute(e, Environment(Map2.remove(x, env)), sto0))

    case (App(e1, e2), _, _) =>
        App(substitute(e1, env0, sto0), substitute(e2, env0, sto0))
}

//
// We can now test our CEK machine!
//
def testEval1: Result[Expression, Str] = run(Var(V(0)))                                                      // x
def testEval2: Result[Expression, Str] = run(Abs(V(0), Var(V(0))))                                           // λx. x
def testEval3: Result[Expression, Str] = run(Abs(V(0), Abs(V(1), Var(V(0)))))                                // λx. λy. x
def testEval4: Result[Expression, Str] = run(App(Abs(V(0), Var(V(0))), Abs(V(1), Var(V(1)))))                // (λx. x) (λy. y)
def testEval5: Result[Expression, Str] = run(App(Abs(V(0), Abs(V(1), Var(V(0)))), Abs(V(2), Var(V(2)))))     // (λx. λy. x) (λz. z)

//
// Here is the omega-combinator: (λx. x x) (λx. x x)
//
def testOmega: Result[Expression, Str] = run(
    App(
        Abs(V(0), App(Var(V(0)), Var(V(0)))),
        Abs(V(0), App(Var(V(0)), Var(V(0))))))




///
/// Returns a fresh address.
///
def freshAddr: Address = A(genSym!())


//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
//

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