package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

object FSet {

  /**
    * All set operations.
    */
  val Ops = List(
    "Set:isEmpty" -> IsEmpty,
    "Set:memberOf" -> MemberOf,
    "Set:isSubsetOf" -> IsSubsetOf,
    "Set:isProperSubsetOf" -> IsProperSubsetOf,
    "Set:empty" -> Empty,
    "Set:singleton" -> Singleton,
    "Set:insert" -> Insert,
    "Set:delete" -> Delete,
    "Set:union" -> Union,
    "Set:intersection" -> Intersection,
    "Set:difference" -> Difference,
    "Set:filter" -> Filter,
    "Set:map" -> Map,
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
  sealed trait SetOperator

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `isEmpty : Set[A] => Bool` function.
    */
  object IsEmpty extends SetOperator {
    val tpe = Set(A) ~> Bool
  }

  /**
    * The `memberOf : (A, Set[A]) => Bool` function.
    */
  object MemberOf extends SetOperator {
    val tpe = (A, Set(A)) ~> Bool
  }

  /**
    * The `isSubsetOf : (Set[A], Set[A]) => Bool` function.
    */
  object IsSubsetOf extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Bool
  }

  /**
    * The `isProperSubsetOf : (Set[A], Set[A]) => Bool` function.
    */
  object IsProperSubsetOf extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Construction                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `empty : Unit => Set[A]` function.
    */
  object Empty extends SetOperator {
    val tpe = Type.Unit ~> Set(A)
  }

  /**
    * The `singleton : A => Set[A]` function.
    */
  object Singleton extends SetOperator {
    val tpe = A ~> Set(A)
  }

  /**
    * The `insert : (A, Set[A]) => Set[A]` function.
    */
  object Insert extends SetOperator {
    val tpe = (A, Set(A)) ~> Set(A)
  }

  /**
    * The `delete : (A, Set[A]) => Set[A]` function.
    */
  object Delete extends SetOperator {
    val tpe = (A, Set(A)) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `union : (Set[A], Set[A]) => Set[A]` function.
    */
  object Union extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  /**
    * The `intersection : (Set[A], Set[A]) => Set[A]` function.
    */
  object Intersection extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  /**
    * The `difference : (Set[A], Set[A]) => Set[A]` function.
    */
  object Difference extends SetOperator {
    val tpe = (Set(A), Set(A)) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Filter                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `filter : (Set[A], A => Bool) => Set[A]` function.
    */
  object Filter extends SetOperator {
    val tpe = (Set(A), A ~> Bool) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Maps                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (Set[A], A => B) => Set[B]` function.
    */
  object Map extends SetOperator {
    val tpe = (Set(A), A ~> B) ~> Set(B)
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
  object FoldRight {
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
