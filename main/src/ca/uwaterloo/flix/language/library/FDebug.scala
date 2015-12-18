package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

object FDebug {

  /**
    * A common super-type for all debug operations.
    */
  sealed trait DebugOperator

  val A = Type.Var("A")
  val B = Type.Var("B")

  /**
    * The `abort : Str => Unit` function.
    */
  object Abort extends DebugOperator {
    val tpe = Str ~> Unit
  }

  /**
    * The `print : A => A` function.
    */
  object Print extends DebugOperator {
    val tpe = A ~> A
  }

  /**
    * The `time : A => A` function.
    */
  object Time extends DebugOperator {
    val tpe = A ~> A
  }

  /**
    * The `trace : A => A` function.
    */
  object Trace extends DebugOperator {
    val tpe = A ~> A
  }

}
