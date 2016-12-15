//
// In this tutorial we will implement the Control-Environment-Kontinuation (CEK) machine.
//
// Be sure to check out the Lambda calculus tutorial before starting this tutorial.
//
// The ideas developed here are based on the paper "Abstracting Abstract Machines" by Horn and Might.
//

///
/// We begin with the usual expressions of the lambda calculus:
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
/// An environment is a map from variables (integers) to a (value, environment)-pair.
///
enum Environment {
    // NB: We use Expression here since there is no explicit syntactic category for values.
    case Environment(Map2[Int, (Expression, Environment)])
}

///
/// The CEK-machine operates based on continuations derived
//  from the evaluation contexts of the language.
///
enum Kontinuation {
    // Represents the empty evaluation context (named `mt` in the paper).
    case Halt,

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
/// Returns the initial state of the CEK-machine for the given expression `e0`.
///
def inject(e0: Expression): (Expression, Environment, Kontinuation) =
    (e0, Env/empty(), Halt)


///
/// Returns the next state of the given machine `m`.
///
/// NB: Returns the same state if no further transition is possible.
///
/// (The transition rules are described in Figure 1 in the paper.)
///
def step(m: (Expression, Environment, Kontinuation)): (Expression, Environment, Kontinuation) = match m with {
    case (Var(x), env0, kont) =>
        // Case 1: The expression is a variable. Look it up in the environment.
        // TODO: What should happen if the variable  does not exist? Probably the machine should be stuck.
        let (v, env1) = Env/lookup(x, env0);
            (v, env1, kont)
    case (App(e1, e2), env0, kont) =>
        // Case 2: The expression is an application.
        // Use the lambda as the expression and move the argument into the continuation.
        (e1, env0, App1(e2, env0, kont))
    case (e, env0, App1(e1, env1, kont)) =>
        // Check if the expression is a value. Otherwise the machine is stuck.
        if (nonValue(e)) m else {
            (e1, env1, App2(e, env0, kont))
        }
    case (e, env0, App2(e1, env1, kont)) =>
        // Check if the expression is a value. Otherwise the machine is stuck.
        if (nonValue(e)) m else {
            // We know that e1 is a value (and in particular a lambda abstration).
            let Abs(x, body) = e1;
                (body, Env/insert(x, (e, env0), env1), kont)
        }
     case _ => m
}

// TODO: Use greek letters?


///
/// Repeteatedly performs single-step transitions in the given machine `m0` until the machine halts with a value.
///
def eval(m0: (Expression, Environment, Kontinuation)): (Expression, Environment) =
    // Perform a single transition step.
    let m1 = step(m0);
    // Unpack the components of the machine.
    let (e, env, kont) = m1;
        // Check if the machine is halted with a value.
        // If so, return the value and environment, otherwise take another transition.
        if (isValue(e) && kont == Halt)
            (e, env)
        else
            eval(m1)


///
/// Evaluates the given expression to a (value, environment)-pair by injecting it into the initial state
/// of the CEK machine and repeatedly performing a transition until the machine halts with a value.
///
def run(e0: Expression): Expression =
    let (v, env) = eval(inject(e0));
        substitute(v, env)

///
/// Substitutes every free variable in the given expression `e0` for the value in the given environment `env0`.
///
def substitute(e0: Expression, env0: Environment): Expression = match e0 with {
    case Var(x)         =>
        let Environment(m) = env0;
            match Map/get(x, m) with {
                case None => Var(x)
                case Some((e, env)) => substitute(e, env)
            }
    case Abs(x, e)      => Abs(x, substitute(e, Env/remove(x, env0)))
    case App(e1, e2)    => App(substitute(e1, env0), substitute(e2, env0))
}


// TODO: Where is the use of gensym in the machine?

//
// We can now test our CEK machine!
//
def testEval1: Expression = run(Var(0))                                         // x
def testEval2: Expression = run(Abs(0, Var(0)))                                 // λx. x
def testEval3: Expression = run(Abs(0, Abs(1, Var(0))))                         // λx. λy. x
def testEval4: Expression = run(App(Abs(0, Var(0)), Abs(1, Var(1))))            // (λx. x) (λy. y)
def testEval5: Expression = run(App(Abs(0, Abs(1, Var(0))), Abs(2, Var(2))))    // (λx. λy. x) (λz. z)

//
// Here is the omega-combinator: (λx. x x) (λx. x x)
//
def testOmega: Expression = run(
    App(
        Abs(0, App(Var(0), Var(0))),
        Abs(0, App(Var(0), Var(0)))))
















namespace Env {

    def empty: Environment = Environment(Map2(Nil): Map2[Int, (Expression, Environment)])

    // TODO: Signature
    def lookup(k: Int, env: Environment): (Expression, Environment) = match env with {
        case Environment(m) =>
            // Assume that the value exists
            let Some(v) = Map/get(k, m);
                v
    }

    def insert(k: Int, v: (Expression, Environment), env: Environment): Environment = match env with {
        case Environment(m) =>
            Environment(Map/insert(k, v, m))
    }

    def remove(k: Int, env: Environment): Environment = match env with {
        case Environment(m) =>
            Environment(Map/remove(k, m))
    }

}


// TODO: Allow greek letters?

//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
//
enum Map2[k, v] {
    case Map2(List[(k, v)])
}
namespace Map {

    def insert(k: Int, v: (Expression, Environment), m: Map2[Int, (Expression, Environment)]): Map2[Int, (Expression, Environment)] =
        let Map2(xs) = m;
            Map2((k, v) :: xs)

    // TODO: Signature and name
    def get[k, v](k: k, m: Map2[k, v]): Option[v] =
        let Map2(xs) = m;
            getHelper(k, xs)

    def getHelper[k, v](k: k, xs: List[(k, v)]): Option[v] = match xs with {
        case Nil => None
        case (y, v) :: ys => if (k == y) Some(v) else getHelper(k, ys)
    }

    def remove(k: Int, m: Map2[Int, (Expression, Environment)]): Map2[Int, (Expression, Environment)] =
        let Map2(xs) = m;
            Map2(removeHelper(k, xs))

    def removeHelper[k, v](k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (y, v) :: ys => if (k == y) removeHelper(k,  ys) else (y, v) :: removeHelper(k, ys)
    }

}