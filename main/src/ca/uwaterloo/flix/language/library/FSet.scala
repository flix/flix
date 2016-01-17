package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Type._

import scala.collection.immutable

object FSet {

  /**
    * A common super-type for all set operations.
    */
  sealed trait SetOperator extends LibraryOperator

  /**
    * All set operations.
    */
  val Ops: immutable.Map[Name.Resolved, SetOperator] = List(
    // Set Construction.
    "Set/empty" -> empty,
    "Set/singleton" -> singleton,
    "Set/insert" -> insert,
    "Set/delete" -> delete,

    // Set Predicates.
    "Set/null" -> nul,
    "Set/memberOf" -> memberOf,
    "Set/isSubsetOf" -> isSubsetOf,
    "Set/isProperSubsetOf" -> isProperSubsetOf,

    // Elementary Set Operations.
    "Set/union" -> union,
    "Set/intersection" -> intersection,
    "Set/difference" -> difference,
    "Set/subsets" -> subsets,

    // Set Transformation.
    "Set/filter" -> filter,
    "Set/map" -> map,
    "Set/flatMap" -> flatMap,

    // Set Conversions.
    "Set/toList" -> toAscList,
    "Set/toAscList" -> toAscList,
    "Set/toDescList" -> toDescList,
    "Set/toMap" -> toMap,

    // Order and Lattice Operations.
    "Set/isAntiChain" -> isAntiChain,
    "Set/join" -> join,
    "Set/meet" -> meet,
    "Set/widen" -> widen,
    "Set/narrow" -> narrow
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Set Construction                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object empty extends SetOperator {
    val tpe = () ~> Bool
  }

  object singleton extends SetOperator {
    val tpe = A ~> Bool
  }

  object insert extends SetOperator {
    val tpe = (A, Set(A)) ~> Set(A)
  }

  object delete extends SetOperator {
    val tpe = (A, Set(A)) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Set Predicates                                                          //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends SetOperator {
    val tpe = Set(A) ~> Bool
  }

  object memberOf extends SetOperator {
    val tpe = (A, Set(A)) ~> Bool
  }

  object isSubsetOf extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Bool
  }

  object isProperSubsetOf extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Two Set Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object union extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  object intersection extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  object difference extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  object subsets extends SetOperator {
    val tpe = Set(A) ~> Set(Set(A))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Set Transformation                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object filter extends SetOperator {
    val tpe = (A ~> Bool, Set(A)) ~> Set(A)
  }

  object map extends SetOperator {
    val tpe = (A ~> B, Set(A)) ~> Set(B)
  }

  object flatMap extends SetOperator {
    val tpe = (A ~> Set(B), Set(A)) ~> Set(B)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Set Conversions                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object toAscList extends SetOperator {
    val tpe = Set(A) ~> Lst(A)
  }

  object toDescList extends SetOperator {
    val tpe = Set(A) ~> Lst(A)
  }

  object toMap extends SetOperator {
    val tpe = Set((A, B)) ~> Type.Map(A, B)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Order and Lattice Operations                                            //
  /////////////////////////////////////////////////////////////////////////////
  object isAntiChain extends SetOperator {
    val tpe = Lst(A) ~> Bool
  }

  object join extends SetOperator {
    val tpe = Lst(A) ~> A
  }

  object meet extends SetOperator {
    val tpe = Lst(A) ~> A
  }

  object widen extends SetOperator {
    val tpe = Lst(A) ~> A
  }

  object narrow extends SetOperator {
    val tpe = Lst(A) ~> A
  }

}
