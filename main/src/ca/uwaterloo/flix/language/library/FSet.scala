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
    "Set:null" -> nul,
    "Set:memberOf" -> memberOf,
    "Set:isSubsetOf" -> isSubsetOf,
    "Set:isProperSubsetOf" -> isProperSubsetOf,
    "Set:insert" -> insert,
    "Set:delete" -> delete,
    "Set:union" -> union,
    "Set:intersection" -> intersection,
    "Set:difference" -> difference,
    "Set:filter" -> filter,
    "Set:map" -> map,
    "Set:flatMap" -> flatMap,
    "Set:foldLeft" -> foldLeft,
    "Set:foldRight" -> foldRight,
    "Set:toAscList" -> toAscList,
    "Set:toDescList" -> toDescList,
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



    "Set:toMap" -> toMap
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `null : Set[A] => Bool` function.
    */
  object nul extends SetOperator {
    val tpe = Set(A) ~> Bool
  }

  /**
    * The `memberOf : (A, Set[A]) => Bool` function.
    */
  object memberOf extends SetOperator {
    val tpe = (A, Set(A)) ~> Bool
  }

  /**
    * The `isSubsetOf : (Set[A], Set[A]) => Bool` function.
    */
  object isSubsetOf extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Bool
  }

  /**
    * The `isProperSubsetOf : (Set[A], Set[A]) => Bool` function.
    */
  object isProperSubsetOf extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Construction                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `insert : (A, Set[A]) => Set[A]` function.
    */
  object insert extends SetOperator {
    val tpe = (A, Set(A)) ~> Set(A)
  }

  /**
    * The `delete : (A, Set[A]) => Set[A]` function.
    */
  object delete extends SetOperator {
    val tpe = (A, Set(A)) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `union : (Set[A], Set[A]) => Set[A]` function.
    */
  object union extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  /**
    * The `intersection : (Set[A], Set[A]) => Set[A]` function.
    */
  object intersection extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  /**
    * The `difference : (Set[A], Set[A]) => Set[A]` function.
    */
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

  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `toAscList : Set[A] => List[A]` function.
    */
  object toAscList extends SetOperator {
    val tpe = Set(A) ~> Lst(A)
  }

  /**
    * The `toDescList : Set[A] => List[A]` function.
    */
  object toDescList extends SetOperator {
    val tpe = Set(A) ~> Lst(A)
  }

  /**
    * The `toMap : Set[(A, B)] => Map[A, B]` function.
    */
  object toMap extends SetOperator {
    val tpe = Set((A, B)) ~> Type.Map(A, B)
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

}
