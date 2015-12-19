package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FDebug {

  /**
    * All debug operations.
    */
  val Ops: immutable.Map[Name.Resolved, DebugOperator] = List(
    "Debug::abort" -> abort,
    "Debug::print" -> print,
    "Debug::time" -> time,
    "Debug::trace" -> trace
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * A common super-type for all debug operations.
    */
  sealed trait DebugOperator extends LibraryOperator

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")

  /**
    * The `abort : Str => Unit` function.
    */
  object abort extends DebugOperator {
    val tpe = Str ~> Unit
  }

  /**
    * The `print : A => A` function.
    */
  object print extends DebugOperator {
    val tpe = A ~> A
  }

  /**
    * The `time : A => A` function.
    */
  object time extends DebugOperator {
    val tpe = A ~> A
  }

  /**
    * The `trace : A => A` function.
    */
  object trace extends DebugOperator {
    val tpe = A ~> A
  }

}
