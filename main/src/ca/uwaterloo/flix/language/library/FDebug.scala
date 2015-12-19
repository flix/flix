package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

object FDebug {

  /**
    * All debug operations.
    */
  val Ops = List(
    "Debug::abort" -> Abort,
    "Debug::print" -> Print,
    "Debug::time" -> Time,
    "Debug::trace" -> Trace
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * A common super-type for all debug operations.
    */
  sealed trait DebugOperator

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")

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
