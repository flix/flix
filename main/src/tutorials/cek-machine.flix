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
/// The CEK-machine operates based on continuations derived
//  from the evaluation contexts of the language.
///
enum Kontinuation {
    // Represents the empty evaluation context.
    case Empty,

    // Represents the evaluation context ([] e).
    case App1(Expression),

    //
    case App2(Expression)
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




//
// ----------------------------------------------------------------------------
// TODO: Replace by functions from the standard library.
// ----------------------------------------------------------------------------
//
enum Map2[k, v] {
    case Map2(List[(k, v)])
}
namespace Map {
    def empty: Map2[Int, Int] = Map2(Nil)

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