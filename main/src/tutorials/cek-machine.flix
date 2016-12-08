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
/// Returns the same state if no further transition is possible.
///
/// (See Figure 1 in the paper)
///
def step(m: (Expression, Environment, Kontinuation)): (Expression, Environment, Kontinuation) = match m with {
    case (Var(x), env, kont) => ???
}


def eval(e0: Expression): Option[Expression] = ???




namespace Env {

    def empty: Environment = Environment(Map2(Nil): Map2[Int, (Expression, Environment)])

}


//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
//
enum Map2[k, v] {
    case Map2(List[(k, v)])
}
namespace Map {

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