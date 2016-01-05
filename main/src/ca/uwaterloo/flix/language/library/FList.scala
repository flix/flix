package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FList {

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
    "List::partition" -> partition,
    "List::range" -> range,
    "List::repeat" -> repeat,
    "List::permutations" -> permutations,
    "List::subsequences" -> subsequences,
    "List::indexWhere" -> indexWhere,
    "List::findLeft" -> findLeft,
    "List::findRight" -> findRight,
    "List::memberOf" -> memberOf,
    "List::map" -> map,
    "List::mapWithIndex" -> mapWithIndex,
    "List::flatMap" -> flatMap,

    "List::scanLeft" -> scanLeft,
    "List::scanRight" -> scanRight,
    "List::oneOf" -> oneOf,
    "List::replace" -> replace,
    "List::patch" -> patch,

    "List::groupBy" -> groupBy,
    "List::reverse" -> reverse,
    "List::rotateLeft" -> rotateLeft,
    "List::rotateRight" -> rotateRight,


    // Predicates.
    "List::isPrefixOf" -> isPrefixOf,
    "List::isInfixOf" -> isInfixOf,
    "List::isSuffixOf" -> isSuffixOf,

    // Fold Operations.
    "List::fold" -> foldLeft,
    "List::foldLeft" -> foldLeft,
    "List::foldRight" -> foldRight,
    "List::reduceLeft" -> reduceLeft,
    "List::reduceLeftOpt" -> reduceLeftOpt,
    "List::reduceRight" -> reduceRight,
    "List::reduceRightOpt" -> reduceRightOpt,

    // Special Fold Operations.
    "List::count" -> count,
    "List::concat" -> concat,
    "List::exists" -> exists,
    "List::forall" -> forall,
    "List::and" -> and,
    "List::or" -> or,

    // Sub List Operations.
    "List::filter" -> filter,
    "List::span" -> span,
    "List::slice" -> slice,
    "List::drop" -> drop,
    "List::dropWhile" -> dropWhile,
    "List::take" -> take,
    "List::takeWhile" -> takeWhile,

    // Aggregation And Sorting Operations.
    "List::sum" -> sum,
    "List::product" -> product,
    "List::min" -> min,
    "List::max" -> max,
    "List::minBy" -> minBy,
    "List::maxBy" -> maxBy,
    "List::sort" -> sort,
    "List::sortBy" -> sortBy,

    // Zipping and Unzipping
    "List::zip" -> zip,
    "List::zipWith" -> zipWith,
    "List::unzip" -> unzip,

    // Two List Operations.
    "List::map2" -> map2,
    "List::flatMap2" -> flatMap2,
    "List::foldLeft2" -> foldLeft2,
    "List::foldRight2" -> foldRight2,

    // Combined Operations.
    "List::concatMap" -> concatMap,
    "List::filterMap" -> filterMap,
    "List::findMap" -> findMap,

    // Conversion Operations.
    "List::toMap" -> toMap,
    "List::toSet" -> toSet,

    // Order and Lattice Operations.
    "List::leq" -> leq,
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
  object nil extends ListOperator {
    val tpe = () ~> Lst(A)
  }

  object cons extends ListOperator {
    val tpe = (A, Lst(A)) ~> Lst(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  object head extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  object tail extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  object init extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  object last extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  object length extends ListOperator {
    val tpe = Lst(A) ~> Int
  }

  object append extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
  }

  object at extends ListOperator {
    val tpe = (Int, Lst(A)) ~> A
  }

  /**
    * The `indexWhere : (A => Bool, List[A]) => Int` function.
    */
  object indexWhere extends ListOperator {
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

  /////////////////////////////////////////////////////////////////////////////
  // Predicates                                                              //
  /////////////////////////////////////////////////////////////////////////////
  object isPrefixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  object isInfixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  object isSuffixOf extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Fold Operations                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object foldLeft extends ListOperator {
    val tpe = ((B, A) ~> B, B, Lst(A)) ~> B
  }

  object foldRight extends ListOperator {
    val tpe = ((A, B) ~> B, B, Lst(A)) ~> B
  }

  object reduceLeft extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> A
  }

  object reduceLeftOpt extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> Opt(A)
  }

  object reduceRight extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> A
  }

  object reduceRightOpt extends ListOperator {
    val tpe = ((A, A) ~> A, Lst(A)) ~> Opt(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Special Fold Operations                                                 //
  /////////////////////////////////////////////////////////////////////////////
  object count extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Int
  }

  object concat extends ListOperator {
    val tpe = Lst(Lst(A)) ~> Lst(A)
  }

  object exists extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Bool
  }

  object forall extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Bool
  }

  object and extends ListOperator {
    val tpe = Lst(Bool) ~> Bool
  }

  object or extends ListOperator {
    val tpe = Lst(Bool) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Sub Lists Operations                                                    //
  /////////////////////////////////////////////////////////////////////////////
  object filter extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Lst(A)
  }

  object slice extends ListOperator {
    val tpe = (Int, Int, Lst(A)) ~> Lst(A)
  }

  object span extends ListOperator {
    val tpe = (A ~> Bool, Lst(A), Lst(B)) ~>(Lst(A), Lst(A))
  }

  object drop extends ListOperator {
    val tpe = (Int, Lst(A)) ~> Lst(A)
  }

  object dropWhile extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Lst(A)
  }

  object take extends ListOperator {
    val tpe = (Int, Lst(A)) ~> Lst(A)
  }

  object takeWhile extends ListOperator {
    val tpe = (A ~> Bool, Lst(A)) ~> Lst(A)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Aggregation and Sorting Operations                                      //
  /////////////////////////////////////////////////////////////////////////////
  object sum extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  object product extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  object min extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  object max extends ListOperator {
    val tpe = Lst(Int) ~> Int
  }

  object minBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Lst(A)) ~> A
  }

  object maxBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Lst(A)) ~> A
  }

  object sort extends ListOperator {
    val tpe = Lst(A) ~> Lst(A)
  }

  object sortBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Lst(A)) ~> Lst(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Zipping and Unzipping Operations                                        //
  /////////////////////////////////////////////////////////////////////////////
  object zip extends ListOperator {
    val tpe = (Lst(A), Lst(B)) ~> Lst((A, B))
  }

  object zipWith extends ListOperator {
    val tpe = ((A, B) ~> C, Lst(A), Lst(B)) ~> Lst(C)
  }

  object unzip extends ListOperator {
    val tpe = Lst((A, B)) ~>(Lst(A), Lst(B))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combined Operations                                                     //
  /////////////////////////////////////////////////////////////////////////////
  object concatMap extends ListOperator {
    val tpe = (A ~> Lst(B), Lst(A)) ~> Lst(B)
  }

  object filterMap extends ListOperator {
    val tpe = (A ~> Opt(B), Lst(A)) ~> Lst(B)
  }

  object findMap extends ListOperator {
    val tpe = (A ~> Opt(B), Lst(A)) ~> B
  }

  /////////////////////////////////////////////////////////////////////////////
  // Two List Operations                                                     //
  /////////////////////////////////////////////////////////////////////////////
  object map2 extends ListOperator {
    val tpe = ((A, B) ~> C, Lst(A), Lst(B)) ~> Lst(C)
  }

  object flatMap2 extends ListOperator {
    val tpe = ((A, B) ~> Lst(C), Lst(A), Lst(B)) ~> Lst(C)
  }

  object foldLeft2 extends ListOperator {
    val tpe = ((C, A, B) ~> C, C, Lst(A), Lst(B)) ~> C
  }

  object foldRight2 extends ListOperator {
    val tpe = ((A, B, C) ~> C, C, Lst(A), Lst(B)) ~> C
  }

  /////////////////////////////////////////////////////////////////////////////
  // Conversion Operations                                                   //
  /////////////////////////////////////////////////////////////////////////////
  object toMap extends ListOperator {
    val tpe = Lst((A, B)) ~> Type.Map(A, B)
  }

  object toSet extends ListOperator {
    val tpe = Lst(A) ~> Set(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Order and Lattice Operations                                            //
  /////////////////////////////////////////////////////////////////////////////
  object leq extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Bool
  }

  object isAscChain extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  object isDescChain extends ListOperator {
    val tpe = Lst(A) ~> Bool
  }

  object join extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  object meet extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  object widen extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  object narrow extends ListOperator {
    val tpe = Lst(A) ~> A
  }

  object zipWithJoin extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
  }

  object zipWithMeet extends ListOperator {
    val tpe = (Lst(A), Lst(A)) ~> Lst(A)
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
    * fn partition(f: A => Bool, xs: List[A]): (List[A], List[A])
    *
    * TODO: doc
    */
  object partition extends ListOperator {
    val tpe = (A ~> Bool, Lst(A), Lst(B)) ~>(Lst(A), Lst(A))
  }

}
