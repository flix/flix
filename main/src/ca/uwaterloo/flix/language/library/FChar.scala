package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type

import scala.collection.immutable

object FChar {

  /**
    * A common super-type for all char operations.
    */
  sealed trait CharOperator extends LibraryOperator

  // isAscii
  // isChar: Char ~> Bool
  // isControl: Char ~> Bool
  // isLower
  // isUpper
  // isWhiteSpace
  // isLetter: Char ~> Bool
  // isLetterOrDigit: Char ~> Bool
  // isOctDigit
  // isHexDigit
  // isNumber

  /**
    * All char operations.
    */
  val Ops: immutable.Map[Name.Resolved, CharOperator] = List(
    // Char Predicates.

    // Char Conversions.
    "Char/toLower" -> toLower,
    "Char/toUpper" -> toUpper,
    "Char/toInt" -> toInt
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  // TODO: Replace Int by Char

  /////////////////////////////////////////////////////////////////////////////
  // Char Predicates                                                         //
  /////////////////////////////////////////////////////////////////////////////



  /////////////////////////////////////////////////////////////////////////////
  // Char Conversions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object toInt extends CharOperator {
    val tpe = Type.Int ~> Type.Int
  }

  object toLower extends CharOperator {
    val tpe = Type.Int ~> Type.Int
  }

  object toUpper extends CharOperator {
    val tpe = Type.Int ~> Type.Int
  }

}
