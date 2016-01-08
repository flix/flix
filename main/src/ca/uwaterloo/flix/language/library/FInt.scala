package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type

import scala.collection.immutable

object FInt {

  /**
    * A common super-type for all integer operations.
    */
  sealed trait IntOperator extends LibraryOperator

  /**
    * All integer operations.
    */
  val Ops: immutable.Map[Name.Resolved, IntOperator] = List(
    "Int/minValue" -> minValue,
    "Int/maxValue" -> maxValue
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /////////////////////////////////////////////////////////////////////////////
  // Integer Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////

  // TODO:
  // isPositive/isNegative?
  // https://doc.rust-lang.org/std/primitive.i64.html
  // https://docs.oracle.com/javase/7/docs/api/java/lang/Integer.html

  object abs extends IntOperator {

  }

  object min extends IntOperator {

  }

  object max extends IntOperator {

  }

  object minValue extends IntOperator {
    val tpe = () ~> Type.Int
  }

  object maxValue extends IntOperator {
    val tpe = () ~> Type.Int
  }

}
