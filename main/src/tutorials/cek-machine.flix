//
// In this tutorial we will implement the Control-Environment-Kontinuation (CEK) machine.
//
// Be sure to check out the Lambda calculus tutorial before starting this tutorial.
//
// The ideas developed here are based on the paper "Abstracting Abstract Machines" by Horn and Might.
//

///
/// We introduce a type for variable names.
///
type Variable = V(Int)

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
/// An environment is a map from variables to a (value, environment)-pair.
///
type Environment = Environment(Map2[Variable, (Expression, Environment)])

///
/// The CEK-machine operates based on continuations derived
//  from the evaluation contexts of the language.
///
enum Kontinuation {
    // Represents the empty evaluation context (named `mt` in the paper).
    case Empty,

    // Represents the evaluation context ([] e) with continuation κ
    // where the environment ρ closes the expression e.
    // NB: In the paper this constructor is named `ar`.
    case App1(Expression, Environment, Kontinuation),

    // Represents the evaluation context (v []) with continuation κ
    // where the environment ρ closes the the value v.
    // NB: In the paper this constructor is named `fn`.
    case App2(Expression, Environment, Kontinuation)
}

///
/// A machine state configuration.
///
type State = State(Expression, Environment, Kontinuation)

///
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): State =
    State(e0, Environment(Map2.empty()), Kontinuation.Empty)

///
/// Represents the outcome of a single reduction step.
///
enum Outcome {
    /// Represents the next state.
    case Next(State),

    /// Represents that the reduction is stuck.
    case Done(Expression, Environment),

    /// Represents that the reduction failed.
    case Abort(Str)
}

///
/// Returns the outcome of a single transition for the machine in state `m`.
///
/// (The transition rules are described in Figure 1 in the paper.)
///
def step(m: State): Outcome = match m with {

    //
    // Variable Expression.
    //
    case State(Var(x), Environment(env0), kont) =>
        // The expression is a variable. Look it up in the environment.
         match Map2.lookup(x, env0) with {
            case None => Abort("Unbound variable.")
            case Some((v, env1)) => Next(State(v, env1, kont))
         }

    //
    // Application Expression.
    //
    case State(App(e1, e2), Environment(env0), kont) =>
        // Continue evaluation of `e1` and store `e2` in a kontinuation.
        let k = App1(e2, Environment(env0), kont);
            Next(State(e1, Environment(env0), k))

    //
    // Application1 Kontinuation.
    //
    case State(v1, Environment(env0), App1(e2, env2, kont)) if isValue(v1) =>
        // Continue evaluation of `e2` and store `v1` in a kontinuation.
        let k = App2(v1, Environment(env0), kont);
            Next(State(e2, env2, k))

    //
    // Application2 Kontinuation.
    //
    case State(v2, Environment(env0), App2(v1, Environment(env1), kont)) if isValue(v2) =>
        // Evaluate the body of the lambda `v1` with argument `v2`.
        let Abs(x, body) = v1;
        let env2 = Map2.insert(x, (v2, Environment(env0)), env1);
            Next(State(body, Environment(env2), kont))

    //
    // Stuck.
    //
    case State(e, env0, k) => Done(e, env0)
}


///
/// Repeatedly performs single-step transitions in the given machine `m0` until the machine halts with a value.
///
def eval(m0: State): Result[(Expression, Environment), Str] =
    match println!(step(m0)) with {
        case Next(m)        => eval(m)
        case Done(exp, env) => Ok((exp, env))
        case Abort(e)       => Err(e)
    }

///
/// Evaluates the given expression to a value by injecting it into the initial state of the CEK machine
/// and repeatedly performing a transition until the machine halts with a value (or aborts).
///
def run(e0: Expression): Result[Expression, Str] =
    let result = eval(inject(e0));
        Result.map(match (exp, env) -> substitute(exp, env), result)

///
/// Substitutes every free variable in the given expression `e0` for the value in the given environment `env0`.
///
def substitute(e0: Expression, env0: Environment): Expression = match (e0, env0) with {
    case (Var(x), Environment(env))     => match Map2.lookup(x, env) with {
        case None               => Var(x)
        case Some((e, env1))    => substitute(e, env1)
    }
    case (Abs(x, e), Environment(env))  => Abs(x, substitute(e, Environment(Map2.remove(x, env))))
    case (App(e1, e2), _)               => App(substitute(e1, env0), substitute(e2, env0))
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











//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
//

enum Map2[k, v] {
    case Map2(List[(k, v)])
}

namespace Map2 {

    def empty: Map2[Variable, (Expression, Environment)] = Map2(Nil)

    def insert(k: Variable, v: (Expression, Environment), m: Map2[Variable, (Expression, Environment)]): Map2[Variable, (Expression, Environment)] =
        let Map2(xs) = m;
            Map2((k, v) :: xs)

    def lookup[k, v](k: k, m: Map2[k, v]): Option[v] =
        let Map2(xs) = m;
            getHelper(k, xs)

    def getHelper[k, v](k: k, xs: List[(k, v)]): Option[v] = match xs with {
        case Nil => None
        case (y, v) :: ys => if (k == y) Some(v) else getHelper(k, ys)
    }

    def remove(k: Variable, m: Map2[Variable, (Expression, Environment)]): Map2[Variable, (Expression, Environment)] =
        let Map2(xs) = m;
            Map2(removeHelper(k, xs))

    def removeHelper[k, v](k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (y, v) :: ys => if (k == y) removeHelper(k,  ys) else (y, v) :: removeHelper(k, ys)
    }

}