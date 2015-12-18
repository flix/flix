package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

object FSet {

  // TODO: Allow syntax for: variableName.length() --> length(variableName). "postfix call"
  // aSet.has(athing) --> has(aSet, athing).
  // aSet.getOrElse(xyz) --> getOrElse(aSet, xyz).
  //
  // foo::bar::baz(qux) <--> quux.foo::bar::baz.
  // aSet.Set::has(athing) --> Set::has(aSet, athing).
  // TODO: Allow postfix calls without ()?
  //    // TODO: Pattern matching can simplify this, e.g.:
  //
  //    match xs with {
  //      case #Set{} => // empty set
  //      case #Set{x} => // singleton with variable x
  //      case #Set{42} => singleton with literao 42
  //      case #Set{x, 42, y, rest...} => // set with two elements x and y, and 42, and rest...
  //    }

  /**
    * All set operations.
    */
  val Ops = List(
    "Set:memberOf" -> MemberOf
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * A common super-type for all set operations.
    */
  sealed trait SetOperator

  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `isEmpty : Set[A] => Bool` function.
    */
  object isEmpty extends SetOperator {
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


  //    fn isSingleton[A](xs: Set[A]): Bool = ...
  //    fn nonEmpty[A](xs: Set[A]): Bool = ...
  //
  //    fn head(xs: Set[A]): A
  //    fn tail(xs: Set[A]): A
  //
  //    fn conj(xs: Set[A]): (A, Set[A])
  //
  //    fn size[A](xs: Set[A]): Int = ...
  //    fn in[A](a: A, Set[A]): Bool = ...
  //    fn memberOf[A](a: A, Set[A]): Bool = ...
  //    fn notMemberOf[A](a: A, Set[A]): Bool = ...
  //
  //
  //    // construction
  //    fn empty(): Set[A] = ???
  //    fn singleton(a: A): Set[A] = ???
  //    fn insert(a: A, xs: Set[A]): Set[A] = ???
  //    fn delete(a: A, xs: Set[A]): Set[A] = ???

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


  //    // filter/select/where?
  //    fn filter[A](xs: Set[A], f: A => Bool): Set[A] = ???
  //
  //    // map
  //    fn map[A, B](xs: Set[A], f: A => B): Set[B] = ???
  //
  // }

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
  object Map extends SetOperator {
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
    val tpe = Set((A, B)) ~> Map(A, B)
  }

}
