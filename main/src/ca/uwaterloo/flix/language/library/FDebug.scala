package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Type._

import scala.collection.immutable

object FDebug {

  /**
    * A common super-type for all debug operations.
    */
  sealed trait DebugOperator extends LibraryOperator

  /**
    * All debug operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, DebugOperator] = List(
    "Debug/abort" -> abort,
    "Debug/print" -> print,
    "Debug/time" -> time
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")

  object abort extends DebugOperator {
    val tpe = Str ~> Unit
  }

  object print extends DebugOperator {
    val tpe = A ~> A
  }

  object time extends DebugOperator {
    val tpe = (() ~> A) ~> A
  }

}
