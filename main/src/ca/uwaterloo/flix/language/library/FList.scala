package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Type._

import scala.collection.immutable

object FList {

  /**
    * A common super-type for all list operations.
    */
  sealed trait ListOperator extends LibraryOperator

  /**
    * All list operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, ListOperator] = List(

    // Basic Operations.
    "List/nil" -> nil,
    "List/cons" -> cons,
    "List/null" -> nul,
    "List/head" -> head,
    "List/tail" -> tail,
    "List/init" -> init,
    "List/last" -> last,
    "List/length" -> length,
    "List/append" -> append,
    "List/at" -> at,
    "List/memberOf" -> memberOf,
    "List/indexOf" -> indexOf,
    "List/find" -> findLeft,
    "List/findLeft" -> findLeft,
    "List/findRight" -> findRight,

    // List Building.
    "List/range" -> range,
    "List/repeat" -> repeat,
    "List/scan" -> scanLeft,
    "List/scanLeft" -> scanLeft,
    "List/scanRight" -> scanRight,

    // List Transformations.
    "List/map" -> map,
    "List/mapWithIndex" -> mapWithIndex,
    "List/flatMap" -> flatMap,
    "List/reverse" -> reverse,
    "List/rotateLeft" -> rotateLeft,
    "List/rotateRight" -> rotateRight,
    "List/replace" -> replace,
    "List/patch" -> patch,
    "List/intersperse" -> intersperse,
    "List/intercalate" -> intercalate,
    "List/permutations" -> permutations,
    "List/subsequences" -> subsequences,
    "List/transpose" -> transpose,

    // List Predicates.
    "List/isPrefixOf" -> isPrefixOf,
    "List/isInfixOf" -> isInfixOf,
    "List/isSuffixOf" -> isSuffixOf,

    // Fold and Reduce.
    "List/fold" -> foldLeft,
    "List/foldLeft" -> foldLeft,
    "List/foldRight" -> foldRight,
    "List/reduce" -> reduceLeft,
    "List/reduceLeft" -> reduceLeft,
    "List/reduceOpt" -> reduceLeftOpt,
    "List/reduceLeftOpt" -> reduceLeftOpt,
    "List/reduceRight" -> reduceRight,
    "List/reduceRightOpt" -> reduceRightOpt,

    // Special Folds.
    "List/count" -> count,
    "List/concat" -> concat,
    "List/exists" -> exists,
    "List/forall" -> forall,
    "List/and" -> and,
    "List/or" -> or,

    // Sub Lists.
    "List/filter" -> filter,
    "List/span" -> span,
    "List/partition" -> partition,
    "List/slice" -> slice,
    "List/drop" -> drop,
    "List/dropWhile" -> dropWhile,
    "List/take" -> take,
    "List/takeWhile" -> takeWhile,

    // Aggregation and Sorting.
    "List/sum" -> sum,
    "List/product" -> product,
    "List/min" -> min,
    "List/max" -> max,
    "List/minBy" -> minBy,
    "List/maxBy" -> maxBy,
    "List/sort" -> sort,
    "List/sortBy" -> sortBy,
    "List/groupBy" -> groupBy,

    // Zipping and Unzipping.
    "List/zip" -> zip,
    "List/zipWith" -> zipWith,
    "List/unzip" -> unzip,

    // Two List Operations.
    "List/map2" -> map2,
    "List/flatMap2" -> flatMap2,
    "List/fold2" -> foldLeft2,
    "List/foldLeft2" -> foldLeft2,
    "List/foldRight2" -> foldRight2,

    // Combined Operations.
    "List/concatMap" -> concatMap,
    "List/filterMap" -> filterMap,
    "List/findMap" -> findMap,

    // List Conversions.
    "List/toMap" -> toMap,
    "List/toSet" -> toSet,

    // Order and Lattice Operations.
    "List/leq" -> leq,
    "List/isAscChain" -> isAscChain,
    "List/isDescChain" -> isDescChain,
    "List/join" -> join,
    "List/meet" -> meet,
    "List/widen" -> widen,
    "List/narrow" -> widen,
    "List/zipWithJoin" -> zipWithJoin,
    "List/zipWithMeet" -> zipWithMeet
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

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
    val tpe = () ~> Type.FList(A)
  }

  object cons extends ListOperator {
    val tpe = (A, Type.FList(A)) ~> Type.FList(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Basic List Operations                                                   //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends ListOperator {
    val tpe = Type.FList(A) ~> Bool
  }

  object head extends ListOperator {
    val tpe = Type.FList(A) ~> A
  }

  object tail extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FList(A)
  }

  object init extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FList(A)
  }

  object last extends ListOperator {
    val tpe = Type.FList(A) ~> A
  }

  object length extends ListOperator {
    val tpe = Type.FList(A) ~> Int32
  }

  object append extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Type.FList(A)
  }

  object at extends ListOperator {
    val tpe = (Int32, Type.FList(A)) ~> A
  }

  object memberOf extends ListOperator {
    val tpe = (A, Type.FList(A)) ~> Bool
  }

  object indexOf extends ListOperator {
    val tpe = (A, Type.FList(A)) ~> Int32
  }

  object findLeft extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Type.FOpt(A)
  }

  object findRight extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Type.FOpt(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // List Building                                                           //
  /////////////////////////////////////////////////////////////////////////////
  object range extends ListOperator {
    val tpe = (Int32, Int32) ~> Type.FList(Int32)
  }

  object repeat extends ListOperator {
    val tpe = (A, Int32) ~> Type.FList(A)
  }

  object scanLeft extends ListOperator {
    val tpe = ((B, A) ~> B, B, Type.FList(A)) ~> Type.FList(B)
  }

  object scanRight extends ListOperator {
    val tpe = ((B, A) ~> B, B, Type.FList(A)) ~> Type.FList(B)
  }

  /////////////////////////////////////////////////////////////////////////////
  // List Transformations                                                    //
  /////////////////////////////////////////////////////////////////////////////
  object map extends ListOperator {
    val tpe = (A ~> B, Type.FList(A)) ~> Type.FList(B)
  }

  object mapWithIndex extends ListOperator {
    val tpe = ((A, Int32) ~> B, Type.FList(A)) ~> Type.FList(B)
  }

  object flatMap extends ListOperator {
    val tpe = (A ~> Type.FList(B), Type.FList(A)) ~> Type.FList(B)
  }

  object reverse extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FList(A)
  }

  object rotateLeft extends ListOperator {
    val tpe = (Int32, Type.FList(A)) ~> Type.FList(A)
  }

  object rotateRight extends ListOperator {
    val tpe = (Int32, Type.FList(A)) ~> Type.FList(A)
  }

  object replace extends ListOperator {
    val tpe = (Int32, A, Type.FList(A)) ~> Type.FList(A)
  }

  object patch extends ListOperator {
    val tpe = (Int32, Int32, Type.FList(A), Type.FList(A)) ~> Type.FList(A)
  }

  object permutations extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FList(Type.FList(A))
  }

  object subsequences extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FList(Type.FList(A))
  }

  object intersperse extends ListOperator {
    val tpe = (A, Type.FList(A)) ~> Type.FList(A)
  }

  object intercalate extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(Type.FList(A))) ~> Type.FList(A)
  }

  object transpose extends ListOperator {
    val tpe = Type.FList(Type.FList(A)) ~> Type.FList(Type.FList(A))
  }

  /////////////////////////////////////////////////////////////////////////////
  // List Predicates                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object isPrefixOf extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Bool
  }

  object isInfixOf extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Bool
  }

  object isSuffixOf extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Fold and Reduce                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object foldLeft extends ListOperator {
    val tpe = ((B, A) ~> B, B, Type.FList(A)) ~> B
  }

  object foldRight extends ListOperator {
    val tpe = ((A, B) ~> B, B, Type.FList(A)) ~> B
  }

  object reduceLeft extends ListOperator {
    val tpe = ((A, A) ~> A, Type.FList(A)) ~> A
  }

  object reduceLeftOpt extends ListOperator {
    val tpe = ((A, A) ~> A, Type.FList(A)) ~> Type.FOpt(A)
  }

  object reduceRight extends ListOperator {
    val tpe = ((A, A) ~> A, Type.FList(A)) ~> A
  }

  object reduceRightOpt extends ListOperator {
    val tpe = ((A, A) ~> A, Type.FList(A)) ~> Type.FOpt(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Special Folds                                                           //
  /////////////////////////////////////////////////////////////////////////////
  object count extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Int32
  }

  object concat extends ListOperator {
    val tpe = Type.FList(Type.FList(A)) ~> Type.FList(A)
  }

  object exists extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Bool
  }

  object forall extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Bool
  }

  object and extends ListOperator {
    val tpe = Type.FList(Bool) ~> Bool
  }

  object or extends ListOperator {
    val tpe = Type.FList(Bool) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Sub Lists                                                               //
  /////////////////////////////////////////////////////////////////////////////
  object filter extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Type.FList(A)
  }

  object slice extends ListOperator {
    val tpe = (Int32, Int32, Type.FList(A)) ~> Type.FList(A)
  }

  object partition extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A), Type.FList(B)) ~>(Type.FList(A), Type.FList(A))
  }

  object span extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A), Type.FList(B)) ~>(Type.FList(A), Type.FList(A))
  }

  object drop extends ListOperator {
    val tpe = (Int32, Type.FList(A)) ~> Type.FList(A)
  }

  object dropWhile extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Type.FList(A)
  }

  object take extends ListOperator {
    val tpe = (Int32, Type.FList(A)) ~> Type.FList(A)
  }

  object takeWhile extends ListOperator {
    val tpe = (A ~> Bool, Type.FList(A)) ~> Type.FList(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Aggregation and Sorting                                                 //
  /////////////////////////////////////////////////////////////////////////////
  object sum extends ListOperator {
    val tpe = Type.FList(Int32) ~> Int32
  }

  object product extends ListOperator {
    val tpe = Type.FList(Int32) ~> Int32
  }

  object min extends ListOperator {
    val tpe = Type.FList(Int32) ~> Int32
  }

  object max extends ListOperator {
    val tpe = Type.FList(Int32) ~> Int32
  }

  object minBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Type.FList(A)) ~> A
  }

  object maxBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Type.FList(A)) ~> A
  }

  object sort extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FList(A)
  }

  object sortBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Type.FList(A)) ~> Type.FList(A)
  }

  object groupBy extends ListOperator {
    val tpe = ((A, A) ~> Bool, Type.FList(A)) ~> Type.FList(Type.FList(A))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Zipping and Unzipping                                                   //
  /////////////////////////////////////////////////////////////////////////////
  object zip extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(B)) ~> Type.FList((A, B))
  }

  object zipWith extends ListOperator {
    val tpe = ((A, B) ~> C, Type.FList(A), Type.FList(B)) ~> Type.FList(C)
  }

  object unzip extends ListOperator {
    val tpe = Type.FList((A, B)) ~>(Type.FList(A), Type.FList(B))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combined Operations                                                     //
  /////////////////////////////////////////////////////////////////////////////
  object concatMap extends ListOperator {
    val tpe = (A ~> Type.FList(B), Type.FList(A)) ~> Type.FList(B)
  }

  object filterMap extends ListOperator {
    val tpe = (A ~> Type.FOpt(B), Type.FList(A)) ~> Type.FList(B)
  }

  object findMap extends ListOperator {
    val tpe = (A ~> Type.FOpt(B), Type.FList(A)) ~> B
  }

  /////////////////////////////////////////////////////////////////////////////
  // Two List Operations                                                     //
  /////////////////////////////////////////////////////////////////////////////
  object map2 extends ListOperator {
    val tpe = ((A, B) ~> C, Type.FList(A), Type.FList(B)) ~> Type.FList(C)
  }

  object flatMap2 extends ListOperator {
    val tpe = ((A, B) ~> Type.FList(C), Type.FList(A), Type.FList(B)) ~> Type.FList(C)
  }

  object foldLeft2 extends ListOperator {
    val tpe = ((C, A, B) ~> C, C, Type.FList(A), Type.FList(B)) ~> C
  }

  object foldRight2 extends ListOperator {
    val tpe = ((A, B, C) ~> C, C, Type.FList(A), Type.FList(B)) ~> C
  }

  /////////////////////////////////////////////////////////////////////////////
  // List Conversions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object toMap extends ListOperator {
    val tpe = Type.FList((A, B)) ~> Type.FMap(A, B)
  }

  object toSet extends ListOperator {
    val tpe = Type.FList(A) ~> Type.FSet(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Order and Lattice Operations                                            //
  /////////////////////////////////////////////////////////////////////////////
  object leq extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Bool
  }

  object isAscChain extends ListOperator {
    val tpe = Type.FList(A) ~> Bool
  }

  object isDescChain extends ListOperator {
    val tpe = Type.FList(A) ~> Bool
  }

  object join extends ListOperator {
    val tpe = Type.FList(A) ~> A
  }

  object meet extends ListOperator {
    val tpe = Type.FList(A) ~> A
  }

  object widen extends ListOperator {
    val tpe = Type.FList(A) ~> A
  }

  object narrow extends ListOperator {
    val tpe = Type.FList(A) ~> A
  }

  object zipWithJoin extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Type.FList(A)
  }

  object zipWithMeet extends ListOperator {
    val tpe = (Type.FList(A), Type.FList(A)) ~> Type.FList(A)
  }

}
