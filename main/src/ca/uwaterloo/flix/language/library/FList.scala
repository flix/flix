package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FList {

  // TODO: Which of these should have special syntax?
  // TODO: Need empty and Cons.

  /**
    * All list operations.
    */
  val Ops: immutable.Map[Name.Resolved, ListOperator] = List(
    "List::null" -> nul,
    "List::head" -> head,
    "List::tail" -> tail,
    // TODO: length
    // TODO: init
    // TODO: last
    // TODO: mapPartial/collect.
    // TODO: partition, or better, splitWith
  // TODO: splitAt
  // TODO: span
    // TODO: sortBy
    // TODO: append
    // TODO: intersperse.
    // TODO: intercalate :: [a] -> [[a]] -> [a]
    // TODO: transpose :: [[a]] -> [[a]]
    // TODO: subsequences :: [a] -> [[a]]
    // TODO: permutations :: [a] -> [[a]]

    "List::find" -> find, // TODO: findLeft
  // TODO at(index)
  // TODO: indexOf
  // TODO: findIndex


    // TODO: Slice
    // TODO: repeat
  // TODO: mapWithIndex.
    "List::memberOf" -> memberOf,
    "List::isPrefixOf" -> isPrefixOf, // TODO: or startsWith
    "List::isInfixOf" -> isInfixOf,
    "List::isSuffixOf" -> isSuffixOf, // TODO: endsWith
  // TODO: isSubsequenceOf
    "List::map" -> map,
    "List::flatMap" -> flatMap,
    "List::reverse" -> reverse,
    "List::foldLeft" -> foldLeft,
    "List::foldRight" -> foldRight,
    "List::concatenate" -> concatenate,
  // TODO: reduceLeft
  // TODO: reduceLeftOpt
    // TODO: reduceRight
  // TODO: reduceRightOpt

    "List::exists" -> exists,
    "List::forall" -> forall,
    "List::and" -> and,
    "List::or" -> or,
    "List::reduceLeft" -> reduceLeft,
    "List::reduceRight" -> reduceRight,
    "List::filter" -> filter,
    "List::take" -> take,
    "List::takeWhile" -> takeWhile,
    "List::drop" -> drop,
    "List::dropWhile" -> dropWhile,
    "List::zip" -> zip,
  // TODO: zipWith
  // TODO: unzip
  // TODO: zip
  // TODO: count

    "List::toMap" -> toMap,
    "List::toSet" -> toSet,
    "List::groupBy" -> groupBy
    // TODO: sum, product, minimum, maximum?
  // TODO: MaximumBy, minimumBy

  // TODO: partial order and lattice ops:
  // List:leq xs ys
    // List::lub
  // List::meet

  // TODO: Paper idea, combinator library for lattices:
  // - flatLatticeOf(xs)
  // - lift(l)
  // - upsidedown
  // - product (two kinds?)

  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

  /**
    * A common super-type for all list operations.
    */
  sealed trait ListOperator extends LibraryOperator

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `null : List[A] => Bool` function.
    */
  object nul extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  /**
    * The `head : List[A] => A` function.
    */
  object head extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * The `tail : List[A] => List[A]` function.
    */
  object tail extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  /**
    * The `find : (A => Bool, List[A]) => Opt[A]` function.
    */
  object find extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Opt(A)
  }

  /**
    * The `memberOf : (A, List[A]) => Bool` function.
    */
  object memberOf extends ListOperator {
    val tpe = (A, Lst(A)) ~> Bool
  }

  /**
    * The `isPrefixOf : (List[A], List[A]) => Bool` function.
    */
  object isPrefixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /**
    * The `isInfixOf : (List[A], List[A]) => Bool` function.
    */
  object isInfixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /**
    * The `isSuffixOf : (List[A], List[A]) => Bool` function.
    */
  object isSuffixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Transformations                                                         //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (A => B, List[A]) => List[B]` function.
    */
  object map extends ListOperator {
    val tpe = (A ~> B, Lst(A)) ~> Lst(B)
  }

  /**
    * The `flatMap : (A => List[B], List[A]) => List[B]` function.
    */
  object flatMap extends ListOperator {
    val tpe = (A ~> Lst(B), Lst(A)) ~> Lst(B)
  }

  /**
    * The `reverse : List[A] => List[A]` function.
    */
  object reverse extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Folds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `foldLeft : ((B, A) => B, B, List[A]) => B` function.
    */
  object foldLeft extends ListOperator {
    val tpe = ((B, A) ~> B, B, Lst(A)) ~> B
  }

  /**
    * The `foldRight : ((A, B) => B, B, List[A]) => B` function.
    */
  object foldRight extends ListOperator {
    val tpe = ((A, B) ~> B, B, Lst(A)) ~> B
  }

  /////////////////////////////////////////////////////////////////////////////
  // Special Folds                                                           //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `concatenate : List[List[A]] => List[A]` function.
    */
  object concatenate extends ListOperator {
    val tpe = Lst(Lst(A)) ~> Lst(A)
  }

  /**
    * The `exists : (A => Bool, List[A]) => Bool` function.
    */
  object exists extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Bool
  }

  /**
    * The `forall : (A => Bool, List[A]) => Bool` function.
    */
  object forall extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Bool
  }

  /**
    * The `and : List[Bool] => Bool` function.
    */
  object and extends ListOperator {
    val tpe = Lst(Bool) ~> Bool
  }

  /**
    * The `or : List[Bool] => Bool` function.
    */
  object or extends ListOperator {
    val tpe = Lst(Bool) ~> Bool
  }

  /**
    * The `reduceLeft : ((A, A) => A, List[A]) => A` function.
    */
  object reduceLeft extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> A
  }

  /**
    * The `reduceRight : ((A, A) => A, List[A]) => A` function.
    */
  object reduceRight extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> A
  }

  /////////////////////////////////////////////////////////////////////////////
  // Sub Lists                                                               //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `filter : (A => Bool, List[A]) => List[A]` function.
    */
  object filter extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Lst(A)
  }

  /**
    * The `take : (Int, List[A]) => List[A]` function.
    */
  object take extends ListOperator {
    val tpe = (Int, Lst(A)) ~> Lst(A)
  }

  /**
    * The `takeWhile : (A => Bool, List[A]) => List[A]` function.
    */
  object takeWhile extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Lst(A)
  }

  /**
    * The `drop : (Int, List[A]) => List[A]` function.
    */
  object drop extends ListOperator {
    val tpe = (Int, Lst(A)) ~> Lst(A)
  }

  /**
    * The `dropWhile : (A => Bool, List[A]) => List[A]` function.
    */
  object dropWhile extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Lst(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Zipping                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `zip : (List[A], List[B]) => List[(A, B)]` function.
    */
  object zip extends ListOperator {
    val tpe = (Lst(A), Lst(B)) ~> Lst((A, B))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `toMap : List[(A, B)] => Map[A, B]` function.
    */
  object toMap extends ListOperator {
    val tpe = Lst((A, B)) ~> Type.Map(A, B)
  }

  /**
    * The `toSet : List[A] => Set[A]` function.
    */
  object toSet extends ListOperator {
    val tpe = Lst(A) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Grouping                                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `groupBy : ((A, A) => Bool, List[A]) => List[List[A]]` function.
    */
  object groupBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Lst(A)) ~> Lst(Lst(A))
  }

}
