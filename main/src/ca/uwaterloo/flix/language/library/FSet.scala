package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FSet {

  /**
    * All set operations.
    */
  val Ops: immutable.Map[Name.Resolved, SetOperator] = List(
    "Set:isEmpty" -> nul,
    "Set:memberOf" -> memberOf,
    "Set:isSubsetOf" -> isSubsetOf,
    "Set:isProperSubsetOf" -> isProperSubsetOf,
    "Set:empty" -> empty,
    "Set:singleton" -> singleton,
    "Set:insert" -> insert,
    "Set:delete" -> delete,
    "Set:union" -> union,
    "Set:intersection" -> intersection,
    "Set:difference" -> difference,
    "Set:filter" -> filter,
    "Set:map" -> map,
    "Set:flatMap" -> FlatMap,
    "Set:foldLeft" -> FoldLeft,
    "Set:foldRight" -> FoldRight,
    "Set:toAscList" -> ToAscList,
    "Set:toDescList" -> ToDescList,
    "Set:toMap" -> ToMap
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * A common super-type for all set operations.
    */
  sealed trait SetOperator extends LibraryOperator

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
    * The `empty : Unit => Set[A]` function.
    */
  object empty extends SetOperator {
    val tpe = Type.Unit ~> Set(A)
  }

  /**
    * The `singleton : A => Set[A]` function.
    */
  object singleton extends SetOperator {
    val tpe = A ~> Set(A)
  }

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
  object FlatMap extends SetOperator {
    val tpe = (Set(A), A ~> Set(B)) ~> Set(B)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Folds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `foldLeft : (Set[A], B, (B, A) => B) => B` function.
    */
  object FoldLeft extends SetOperator {
    val tpe = (Set(A), B, (B, A) ~> B) ~> B
  }

  /**
    * The `foldRight : (Set[A], B, (B, A) => B) => B` function.
    */
  object FoldRight extends SetOperator {
    val tpe = (Set(A), B, (A, B) ~> B) ~> B
  }

  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `toAscList : Set[A] => List[A]` function.
    */
  object ToAscList extends SetOperator {
    val tpe = Set(A) ~> Lst(A)
  }

  /**
    * The `toDescList : Set[A] => List[A]` function.
    */
  object ToDescList extends SetOperator {
    val tpe = Set(A) ~> Lst(A)
  }

  /**
    * The `toMap : Set[(A, B)] => Map[A, B]` function.
    */
  object ToMap extends SetOperator {
    val tpe = Set((A, B)) ~> Type.Map(A, B)
  }

}
