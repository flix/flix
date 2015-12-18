package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

object FOption {

  /**
    * A common super-type for all option operations.
    */
  sealed trait OptOperator

  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (Opt[A], A => B) => Opt[B]` function.
    */
  object Map extends OptOperator {
    val tpe = (Opt(A), A ~> B) ~> Opt(B)
  }

  /**
    * The `flatMap : (Opt[A], A => Opt[B]) => Opt[B]` function.
    */
  object FlatMap extends OptOperator {
    val tpe = (Opt(A), A ~> Opt(B)) ~> Opt(B)
  }

}
