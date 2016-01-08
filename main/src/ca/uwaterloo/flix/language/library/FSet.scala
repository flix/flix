package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

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
    // Basic Operations.
    "Set:null" -> nul,
    "Set:memberOf" -> memberOf,
    "Set:singleton" -> singleton,
    "Set:insert" -> insert,
    "Set:delete" -> delete,

    // Set Predicates.
    "Set:isSubsetOf" -> isSubsetOf,
    "Set:isProperSubsetOf" -> isProperSubsetOf,

    // Two Set Operations.
    "Set:union" -> union,
    "Set:intersection" -> intersection,
    "Set:difference" -> difference,


    "Set:filter" -> filter,
    "Set:map" -> map,
    "Set:flatMap" -> flatMap,
    "Set:foldLeft" -> foldLeft,
    "Set:foldRight" -> foldRight,

    // Set Conversions.
    "Set:toList" -> toAscList,
    "Set:toAscList" -> toAscList,
    "Set:toDescList" -> toDescList,
    "Set:toMap" -> toMap
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap


  // TODO: collect? collectFirst
  // TODO: count?
  // TODO: find and other list like things, or not?
  // TODO: flatten
  // TODO: minimum, maximum, minimumBy, maximumBy
  // TODO: partition/split?
  // TODO: sum/product?
  // TODO: reduceLeft, reduceRight, reduceLeftOpt, reduceRightOpt
  // TODO: scanLeft, scanRight
  // TODO: size
  // TODO: subsets
  // TODO: zip?
  // TODO: min, max, minBy, maxBy
  // TODO: scala's aggregate[B](z: ⇒ B)(seqop: (B, (A, B)) ⇒ B, combop: (B, B) ⇒ B): B

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends SetOperator {
    val tpe = Set(A) ~> Bool
  }

  object memberOf extends SetOperator {
    val tpe = (A, Set(A)) ~> Bool
  }

  object singleton extends SetOperator {
    val tpe = (A) ~> Bool
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

  /////////////////////////////////////////////////////////////////////////////
  // Filter                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `filter : (A => Bool, Set[A]) => Set[A]` function.
    */
  object filter extends SetOperator {
    val tpe = (A ~> Bool, Set(A)) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Maps                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (A => B, Set[A]) => Set[B]` function.
    */
  object map extends SetOperator {
    val tpe = (A ~> B, Set(A)) ~> Set(B)
  }

  /**
    * The `flatMap : (Set[A], A => Set[B]) => Set[B]` function.
    */
  object flatMap extends SetOperator {
    val tpe = (A ~> Set(B), Set(A)) ~> Set(B)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Folds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `foldLeft : (Set[A], B, (B, A) => B) => B` function.
    */
  object foldLeft extends SetOperator {
    val tpe = ((B, A) ~> B, B, Set(A)) ~> B
  }

  /**
    * The `foldRight : (Set[A], B, (B, A) => B) => B` function.
    */
  object foldRight extends SetOperator {
    val tpe = ((A, B) ~> B, B, Set(A)) ~> B
  }

  /**
    * Returns `true` iff the list is an anti-chain according to the partial order.
    *
    * The function has type `isAntiChain: List[A] => Bool`.
    */
  // TODO
  object isAntiChain extends SetOperator {
    val tpe = Lst(A) ~> Bool
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

}
