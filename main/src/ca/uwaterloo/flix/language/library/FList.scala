package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FList {

  // TODO: check that every operation has a corresponding index op.
  // TODO: check that every operation has a "2" op?

  // TODO: last vs. right

  /**
    * All list operations.
    */
  val Ops: immutable.Map[Name.Resolved, ListOperator] = List(
    // basic operations.
    "List::nil" -> nil,
    "List::cons" -> cons,
    "List::null" -> nul,
    "List::head" -> head,
    "List::tail" -> tail,
    "List::init" -> init,
    "List::last" -> last,
    "List::length" -> length,
    "List::append" -> append,
    "List::at" -> at,

    "List::intersperse" -> intersperse,
    "List::intercalate" -> intercalate,
    "List::transpose" -> transpose,

    // TODO: splitAt
    //splitWith
    // TODO: span

    "List::range" -> range,
    "List::repeat" -> repeat,
    "List::permutations" -> permutations,
    "List::subsequences" -> subsequences,

    "List::indexOf" -> indexOf, // TODO: or indexWhere? lastIndexOf???
    "List::findLeft" -> findLeft,
    "List::findRight" -> findRight,

    "List::memberOf" -> memberOf,
    "List::isPrefixOf" -> isPrefixOf,
    "List::isInfixOf" -> isInfixOf,
    "List::isSuffixOf" -> isSuffixOf,
    "List::map" -> map,
    "List::mapWithIndex" -> mapWithIndex,
    "List::flatMap" -> flatMap,

    "List::reverse" -> reverse,
    "List::rotateLeft" -> rotateLeft,
    "List::rotateRight" -> rotateRight,

    // fold operations.
    "List::foldLeft" -> foldLeft,
    "List::foldRight" -> foldRight,

    // special fold operations.
    "List::count" -> count,
    "List::concatenate" -> concatenate,
    "List::reduceLeft" -> reduceLeft,
    "List::reduceLeftOpt" -> reduceLeftOpt,
    "List::reduceRight" -> reduceRight,
    "List::reduceRightOpt" -> reduceRightOpt,

    "List::exists" -> exists,
    "List::forall" -> forall,
    "List::and" -> and,
    "List::or" -> or,

    "List::filter" -> filter,
    "List::slice" -> slice,
    "List::take" -> take,
    "List::takeWhile" -> takeWhile,
    "List::drop" -> drop,
    "List::dropWhile" -> dropWhile,

    "List::replace" -> replace,
    "List::patch" -> patch,

    // zipping operations.
    "List::zip" -> zip,
    "List::zipWith" -> zipWith,
    "List::unzip" -> unzip,

    "List::groupBy" -> groupBy,

    // aggregation operations.
    "List::sum" -> sum,
    "List::product" -> product,
    "List::min" -> min,
    "List::max" -> max,
    "List::minBy" -> minBy,
    "List::maxBy" -> maxBy,

    "List::sort" -> sort,
    "List::sortBy" -> sortBy,

    "List::scanLeft" -> scanLeft,
    "List::scanRight" -> scanRight,

    // conversion operations.
    "List::toMap" -> toMap,
    "List::toSet" -> toSet,

    "List::oneOf" -> oneOf,

    "List::concatMap" -> concatMap,
    "List::filterMap" -> filterMap,
    "List::findMap" -> findMap,

    // operations on two lists.
    "List::map2" -> map2,
    "List::flatMap2" -> flatMap2,
    "List::foldLeft2" -> foldLeft2,
    "List::foldRight2" -> foldRight2,

    // order and lattice operations.
    "List::isAscChain" -> isAscChain,
    "List::isDescChain" -> isDescChain,
    "List::join" -> join,
    "List::meet" -> meet,
    "List::widen" -> widen,
    "List::narrow" -> widen,
    "List::zipWithJoin" -> zipWithJoin,
    "List::zipWithMeet" -> zipWithMeet
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
  val C = Type.Var("C")

  /////////////////////////////////////////////////////////////////////////////
  // Construction                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The empty list.
    */
  object nil extends ListOperator {
    val tpe = () ~> Lst(A)
  }

  /**
    * A cons cell.
    */
  object cons extends ListOperator {
    val tpe = (A, Lst(A)) ~> Lst(A)
  }

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
    * The `init : List[A] => List[A]` function.
    */
  object init extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  /**
    * The `last : List[A] => A` function.
    */
  object last extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * Returns the length of the list.
    *
    * The `length : List[A] => Int` function.
    */
  object length extends ListOperator {
    val tpe = Lst(A) ~> Int
  }

  /**
    * Returns the first list with the second list appended.
    *
    * The `append : (List[A], List[A]) => List[A]` function.
    */
  object append extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
  }

  /**
    * Returns the element at the given position in the list.
    *
    * The `at : (List[A], Int) => A` function.
    */
  object at extends ListOperator {
    val tpe = (Lst(A), A) ~> A
  }

  /**
    * The `indexOf : (A => Bool, List[A]) => Int` function.
    */
  object indexOf extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Int
  }

  /**
    * The `findLeft : (A => Bool, List[A]) => Opt[A]` function.
    */
  object findLeft extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Opt(A)
  }

  /**
    * The `findRight : (A => Bool, List[A]) => Opt[A]` function.
    */
  object findRight extends ListOperator {
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
    * The `mapWithIndex : ((A, Int) => B, List[A]) => List[B]` function.
    */
  object mapWithIndex extends ListOperator {
    val tpe = ((A, Int) ~> B, Lst(A)) ~> Lst(B)
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
    * Returns the number of elements satisfying the predicate.
    *
    * The `count : (A => Bool, List[A]) => Int` function.
    */
  object count extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Int
  }

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
    * The `reduceLeft : ((A, A) => A, List[A]) => Opt[A]` function.
    *
    * Optionally returns the result of applying the binary operator going from left to right.
    *
    * Returns None if the list is empty.
    */
  object reduceLeftOpt extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> Opt(A)
  }

  /**
    * The `reduceRight : ((A, A) => A, List[A]) => A` function.
    */
  object reduceRight extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> A
  }

  /**
    * The `reduceRight : ((A, A) => A, List[A]) => Opt[A]` function.
    *
    * Optionally returns the result of applying the binary operator going from right to left.
    *
    * Returns None if the list is empty.
    */
  object reduceRightOpt extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> Opt(A)
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
    * The `slice : (Int, Int, List[A]) => List[A]` function.
    */
  object slice extends ListOperator {
    val tpe = (Int, Int, Lst(A)) ~> Lst(A)
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

  /**
    * The `zipWith : ((A, B) => C, List[A], List[B]) => List[C]` function.
    */
  object zipWith extends ListOperator {
    val tpe = ((A, B) ~> C, Lst(A), Lst(B)) ~> Lst(C)
  }

  /**
    * The `unzip : List[(A, B)] => (List[A], List[B])` function.
    */
  object unzip extends ListOperator {
    val tpe = Lst((A, B)) ~>(Lst(A), Lst(B))
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

  /////////////////////////////////////////////////////////////////////////////
  // Aggregation                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * fn sum(xs: List[Int): Int
    *
    * Computes the sum of the elements in the list `xs`. Returns zero if the list is empty.
    */
  object sum extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  /**
    * fn product(xs: List[Int): Int
    *
    * Computes the product of the elements in the list `xs`. Returns one if the list is empty.
    */
  object product extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  /**
    * fn minimum(xs: List[Int): Int
    *
    * Selects the minimum element in the list. Returns Int::MaxValue if the list is empty.
    */
  object min extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  /**
    * fn maximum(xs: List[Int): Int
    *
    * Selects the maximum element in the list. Returns Int::MinValue if the list is empty.
    */
  object max extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  /**
    * fn minBy[A](ord: (A, A) => Ord, xs: List[A): A
    *
    * Selects the minimum element in the list according to the total order induced by the `cmp` function.
    *
    * TODO Aborts if the list is empty?
    */
  // TODO: Kind of need type class instance?
  object minBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Lst(A)) ~> A
  }

  /**
    * fn maxBy[A](ord: (A, A) => Ord, xs: List[A): A
    *
    * Selects the maximum element in the list according to the total order induced by the `cmp` function.
    *
    * TODO Aborts if the list is empty?
    */
  // TODO: Kind of need type class instance?
  object maxBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Lst(A)) ~> A
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
  // Order and Lattice Operations                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Pairwise compares elements from the two lists according to the partial order.
    *
    * Returns `true` if for every pair the first component is less than or equal to the second component.
    *
    * The function has type: `leq: (List[A], List[A]) => Bool`.
    */
  object leq extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  /**
    * Returns `true` iff the list is an ascending chain according to the partial order.
    *
    * The function has type `isAscChain: List[A] => Bool`.
    */
  object isAscChain extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  /**
    * Returns `true` iff the list is a descending chain according to the partial order.
    *
    * The function has type `isDescChain: List[A] => Bool`.
    */
  object isDescChain extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  /**
    * Returns all non-bottom elements in the list.
    *
    * The function has type `strict: List[A] => List[A]`.
    */
  object strict extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  /**
    * Returns the least upper bound of all elements in the list. Returns bottom if the list is empty.
    *
    * The function has type: `join: List[A: JoinLattice] => A`.
    */
  object join extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * Returns the greatest lower bound of all elements in the list. Returns top if the list is empty.
    *
    * The function has type `meet: List[A: MeetLattice] => A`.
    */
  object meet extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * Returns the widening of all elements in the list. Returns bottom if the list is empty.
    *
    * The function has type `widen: List[A: WidenOp] => A`.
    */
  object widen extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * Returns the narrowing of all elements in the list. Returns top if the list is empty.
    *
    * The function has type `narrow: List[A: NarrowOp] => A`.
    */
  object narrow extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  /**
    * Pairwise computes the least upper bound of the elements of the two lists.
    *
    * If the lists are of unequal length the missing element(s) are assumed to be bottom. // TODO: or just drop them?
    *
    * The function has type: `(List[A], List[A]) => List[A]`.
    */
  object zipWithJoin extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
  }

  /**
    * Pairwise computes the greatest lower bound of the elements of the two lists.
    *
    * If the lists are of unequal length the missing element(s) are assumed to be top. // TODO: or just drop them?
    *
    * The function has type `(List[A], List[A]) => List[A]`.
    */
  object zipWithMeet extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
  }

  // TODO: sort .....~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  /**
    * Returns a list of all numbers in the range (including the smallest number and excluding the largest number).
    *
    * The function has type `(Int, Int) => List[Int]`.
    */
  object range extends ListOperator {
    val tpe = (Int, Int) ~> Lst(Int)
  }

  /**
    * Returns a list with the element repeated a given number of times.
    *
    * The function has type `(A, Int) => List[A]`.
    */
  object repeat extends ListOperator {
    val tpe = (A, Int) ~> Lst(A)
  }

  /**
    * Returns all permutations of the list.
    *
    * The function has type `List[A] => List[List[A]]`.
    */
  object permutations extends ListOperator {
    val tpe = Lst(A) ~> Lst(Lst(A))
  }

  /**
    * Returns all subsequences of the list.
    *
    * The function has type `List[A] => List[List[A]]`.
    */
  object subsequences extends ListOperator {
    val tpe = Lst(A) ~> Lst(Lst(A))
  }

  /**
    * fn intersperse(x: A, xs: List[A]): List[A]
    *
    * TODO: doc
    */
  object intersperse extends ListOperator {
    val tpe = (A, Lst(A)) ~> Lst(A)
  }

  /**
    * fn intercalate(x: List[A], xs: List[List[A]]): List[A]
    *
    * TODO: doc
    */
  object intercalate extends ListOperator {
    val tpe = (Lst(A), Lst(Lst(A))) ~> Lst(A)
  }

  /**
    * fn transpose(xs: List[List[A]]): List[List[A]]
    *
    * TODO: doc
    */
  object transpose extends ListOperator {
    val tpe = Lst(Lst(A)) ~> Lst(Lst(A))
  }

  /**
    * fn rotateLeft(k: Int, xs: List[A]): List[A]
    *
    * TODO: doc
    */
  object rotateLeft extends ListOperator {
    val tpe = (Int, Lst(A)) ~> Lst(A)
  }

  /**
    * fn rotateRight(k: Int, xs: List[A]): List[A]
    *
    * TODO: doc
    */
  object rotateRight extends ListOperator {
    val tpe = (Int, Lst(A)) ~> Lst(A)
  }

  /**
    * fn scanLeft(f: (B, A) => B, x: B, xs: List[A]): List[B]
    *
    * TODO: doc
    */
  object scanLeft extends ListOperator {
    val tpe = ((B, A) ~> B, B, Lst(A)) ~> Lst(B)
  }

  /**
    * fn scanRight(f: (A, B) => B, x: B, xs: List[A]): List[B]
    *
    * TODO: doc
    */
  object scanRight extends ListOperator {
    val tpe = ((B, A) ~> B, B, Lst(A)) ~> Lst(B)
  }

  /**
    * fn sort(xs: List[A]): List[A]
    *
    * TODO: doc
    */
  object sort extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  /**
    * fn sortBy(f: (A, A) => Ord, xs: List[A]): List[A]
    *
    * TODO: doc
    */
  object sortBy extends ListOperator {
    val tpe = ((A, A) ~> Int, Lst(A)) ~> Lst(A)
  }

  /**
    * fn patch(from: Int, xs: List[A], replaced: Int): List[A]
    *
    * TODO: doc
    */
  object patch extends ListOperator {
    val tpe = ((A, A) ~> Int, Lst(A)) ~> Lst(A)
  }

  /**
    * fn replace(index: Int, x: A, xs: List[A]): List[A]
    *
    * TODO: doc
    */
  object replace extends ListOperator {
    val tpe = (Int, A, Lst(A)) ~> Lst(A)
  }

  /**
    * fn oneOf(xs: List[Opt[A]]): Opt[A]
    *
    * TODO: doc
    */
  object oneOf extends ListOperator {
    val tpe = Lst(Opt(A)) ~> Opt(A)
  }

  /**
    * fn concatMap(f: A => List[B], xs: List[A]): List[B]
    *
    * TODO: doc
    */
  object concatMap extends ListOperator {
    val tpe = (A ~> Lst(B), Lst(A)) ~> Lst(B)
  }

  /**
    * fn filterMap(f: A => Opt[B], xs: List[A]): List[B]
    *
    * TODO: doc
    */
  object filterMap extends ListOperator {
    val tpe = (A ~> Opt(B), Lst(A)) ~> Lst(B)
  }

  /**
    * fn findMap(f: A => Opt[B], xs: List[A]): Opt[B]
    *
    * TODO: doc
    */
  object findMap extends ListOperator {
    val tpe = (A ~> Opt(B), Lst(A)) ~> B
  }

  /**
    * fn map2(f: (A, B) => C, xs: List[A], ys: List[B]): List[C]
    *
    * TODO: doc
    */
  object map2 extends ListOperator {
    val tpe = ((A, B) ~> C, Lst(A), Lst(B)) ~> Lst(C)
  }

  /**
    * fn flatMap2(f: (A, B) => List[C], xs: List[A], ys: List[B]): List[C]
    *
    * TODO: doc
    */
  object flatMap2 extends ListOperator {
    val tpe = ((A, B) ~> Lst(C), Lst(A), Lst(B)) ~> Lst(C)
  }

  /**
    * fn foldLeft2(f: (C, A, B) => C, c: C, xs: List[A], ys: List[B]): C
    *
    * TODO: doc
    */
  object foldLeft2 extends ListOperator {
    val tpe = ((C, A, B) ~> C, C, Lst(A), Lst(B)) ~> C
  }

  /**
    * fn foldRight2(f: (A, B, C) => C, c: C, xs: List[A], ys: List[B]): C
    *
    * TODO: doc
    */
  object foldRight2 extends ListOperator {
    val tpe = ((A, B, C) ~> C, C, Lst(A), Lst(B)) ~> C
  }

  // TODO: remember to put generic types.

}
