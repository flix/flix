package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type

import scala.collection.immutable

object FInt {

  /**
    * A common super-type for all int operations.
    */
  sealed trait IntOperator extends LibraryOperator

  /**
    * All int operations.
    */
  val Ops: immutable.Map[Name.Resolved, IntOperator] = List(
    // Int Constants.
    "Int/minValue" -> minValue,
    "Int/maxValue" -> maxValue,

    // Int Operations.
    "Int/min" -> min,
    "Int/max" -> max,

    // Int Conversions.
    "Int/toChar" -> toChar
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /////////////////////////////////////////////////////////////////////////////
  // Int Constants                                                           //
  /////////////////////////////////////////////////////////////////////////////
  object minValue extends IntOperator {
    val tpe = () ~> Type.Int
  }

  object maxValue extends IntOperator {
    val tpe = () ~> Type.Int
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int Operations                                                          //
  /////////////////////////////////////////////////////////////////////////////
  object abs extends IntOperator {
    val tpe = (Type.Int, Type.Int) ~> Type.Int
  }

  object min extends IntOperator {
    val tpe = (Type.Int, Type.Int) ~> Type.Int
  }

  object max extends IntOperator {
    val tpe = (Type.Int, Type.Int) ~> Type.Int
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int Conversions                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object toChar extends IntOperator {
    val tpe = Type.Int ~> Type.Char
  }

}
