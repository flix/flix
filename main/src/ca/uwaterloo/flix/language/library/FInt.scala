package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.runtime.Value

import scala.collection.immutable

object FInt {

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

  /**
    * A common super-type for all int operations.
    */
  sealed trait IntOperator extends LibraryOperator {
    def eval(args: Array[AnyRef]): AnyRef = this match {
      // Int Constants.
      case `minValue` => Value.mkInt32(minValue())
      case `maxValue` => Value.mkInt32(maxValue())
      // Int Operations.
      case `abs` => Value.mkInt32(abs(Value.cast2int32(args(0))))
      case `min` => Value.mkInt32(min(Value.cast2int32(args(0)), Value.cast2int32(args(1))))
      case `max` => Value.mkInt32(min(Value.cast2int32(args(0)), Value.cast2int32(args(1))))
      // Int Conversions.
      case `toChar` => ???
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int Constants                                                           //
  /////////////////////////////////////////////////////////////////////////////
  object minValue extends IntOperator {
    val tpe = () ~> Type.Int

    def apply(): Int = Int.MinValue
  }

  object maxValue extends IntOperator {
    val tpe = () ~> Type.Int

    def apply(): Int = Int.MaxValue
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int Operations                                                          //
  /////////////////////////////////////////////////////////////////////////////
  object abs extends IntOperator {
    val tpe = (Type.Int, Type.Int) ~> Type.Int

    def apply(x: Int): Int = math.abs(x)
  }

  object min extends IntOperator {
    val tpe = (Type.Int, Type.Int) ~> Type.Int

    def apply(x: Int, y: Int): Int = math.min(x, y)
  }

  object max extends IntOperator {
    val tpe = (Type.Int, Type.Int) ~> Type.Int

    def apply(x: Int, y: Int): Int = math.max(x, y)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Int Conversions                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object toChar extends IntOperator {
    val tpe = Type.Int ~> Type.Char

    def apply(x: Int): Char = ???
  }

}
