package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Type._

import scala.collection.immutable

object FMap {

  /**
    * A common super-type for all map operations.
    */
  sealed trait MapOperator extends LibraryOperator

  /**
    * All map operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, MapOperator] = List(
    // Map Construction.
    "Map/empty" -> empty,
    "Map/singleton" -> singleton,

    // Basic Operations.
    "Map/null" -> nul,
    "Map/get" -> get,
    "Map/getWithDefault" -> getWithDefault,
    "Map/memberOf" -> memberOf,
    "Map/keysOf" -> keysOf,
    "Map/valuesOf" -> valuesOf,

    // Insert.
    "Map/insert" -> insert,
    "Map/insertWith" -> insertWith,
    "Map/insertWithKey" -> insertWithKey,

    // Adjust.
    "Map/adjust" -> adjust,
    "Map/adjustWithKey" -> adjustWithKey,

    // Update.
    "Map/update" -> update,
    "Map/updateWithKey" -> updateWithKey,

    // Delete.
    "Map/delete" -> delete,

    // Map Predicates.
    "Map/isSubmapOf" -> isSubmapOf,
    "Map/isProperSubmapOf" -> isProperSubmapOf,

    // Map Transformation.
    "Map/filter" -> filter,
    "Map/filterWithKey" -> filterWithKey,
    "Map/map" -> map,
    "Map/mapWithKey" -> mapWithKey,

    // Folds.
    "Map/fold" -> foldLeft,
    "Map/foldWithKey" -> foldLeftWithKey,
    "Map/foldLeft" -> foldLeft,
    "Map/foldLeftWithKey" -> foldLeftWithKey,
    "Map/foldRight" -> foldRight,
    "Map/foldRightWithKey" -> foldRightWithKey,

    // Combine Operations.
    "Map/union" -> union,
    "Map/unionWith" -> unionWith,
    "Map/unionWithKey" -> unionWithKey,
    "Map/intersection" -> intersection,
    "Map/intersectionWith" -> intersectionWith,
    "Map/intersectionWithKey" -> intersectionWithKey,
    "Map/difference" -> difference,
    "Map/differenceWith" -> differenceWith,
    "Map/differenceWithKey" -> differenceWithKey,

    // Conversions.
    "Map/toList" -> toAscList,
    "Map/toAscList" -> toAscList,
    "Map/toDescList" -> toDescList,
    "Map/toSet" -> toSet,

    // Order and Lattice Operations.
    "Map/join" -> join,
    "Map/meet" -> meet
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val K = Type.Var("K")
  val V = Type.Var("V")
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Map Construction                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object empty extends MapOperator {
    val tpe = () ~> Type.FMap(K, V)
  }

  object singleton extends MapOperator {
    val tpe = (K, V) ~> Type.FMap(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends MapOperator {
    val tpe = Type.FMap(K, V) ~> Bool
  }

  object get extends MapOperator {
    val tpe = (K, Type.FMap(K, V)) ~> Type.FOpt(V)
  }

  object getWithDefault extends MapOperator {
    val tpe = (K, V, Type.FMap(K, V)) ~> V
  }

  object memberOf extends MapOperator {
    val tpe = (K, Type.FMap(K, V)) ~> Bool
  }

  object keysOf extends MapOperator {
    val tpe = Type.FMap(K, V) ~> Type.FSet(K)
  }

  object valuesOf extends MapOperator {
    val tpe = Type.FMap(K, V) ~> Type.FList(V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Insert                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object insert extends MapOperator {
    val tpe = (K, V, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object insertWith extends MapOperator {
    val tpe = ((V, V) ~> V, K, V, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object insertWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, K, V, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Adjust                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object adjust extends MapOperator {
    val tpe = (V ~> V, K, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object adjustWithKey extends MapOperator {
    val tpe = ((K, V) ~> V, K, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Update                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object update extends MapOperator {
    val tpe = (V ~> Type.FOpt(V), K, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object updateWithKey extends MapOperator {
    val tpe = ((K, V) ~> Type.FOpt(V), K, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Delete                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object delete extends MapOperator {
    val tpe = (K, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Map Predicates                                                          //
  /////////////////////////////////////////////////////////////////////////////
  object isSubmapOf extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Bool
  }

  object isProperSubmapOf extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Map Transformations                                                     //
  /////////////////////////////////////////////////////////////////////////////
  object filter extends MapOperator {
    val tpe = (V ~> Bool, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object filterWithKey extends MapOperator {
    val tpe = ((K, V) ~> Bool, Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object map extends MapOperator {
    val tpe = (A ~> B, Type.FMap(K, A)) ~> Type.FMap(K, B)
  }

  object mapWithKey extends MapOperator {
    val tpe = ((K, A) ~> B, Type.FMap(K, A)) ~> Type.FMap(K, B)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object union extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object unionWith extends MapOperator {
    val tpe = ((V, V) ~> V, Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object unionWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object intersection extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object intersectionWith extends MapOperator {
    val tpe = ((V, V) ~> V, Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object intersectionWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object difference extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object differenceWith extends MapOperator {
    val tpe = ((V, V) ~> V, Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object differenceWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Map                                                                     //
  /////////////////////////////////////////////////////////////////////////////
  object foldLeft extends MapOperator {
    val tpe = ((B, A) ~> B, B, Type.FMap(K, A)) ~> B
  }

  object foldLeftWithKey extends MapOperator {
    val tpe = ((B, K, A) ~> B, B, Type.FMap(K, A)) ~> B
  }

  object foldRight extends MapOperator {
    val tpe = ((A, B) ~> B, B, Type.FMap(K, A)) ~> B
  }

  object foldRightWithKey extends MapOperator {
    val tpe = ((K, A, B) ~> B, B, Type.FMap(K, A)) ~> B
  }

  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  object toAscList extends MapOperator {
    val tpe = Type.FMap(K, V) ~> Type.FList((K, V))
  }

  object toDescList extends MapOperator {
    val tpe = Type.FMap(K, V) ~> Type.FList((K, V))
  }

  object toSet extends MapOperator {
    val tpe = Type.FMap(K, V) ~> Type.FSet((K, V))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Order and Lattice Operations                                            //
  /////////////////////////////////////////////////////////////////////////////
  object join extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

  object meet extends MapOperator {
    val tpe = (Type.FMap(K, V), Type.FMap(K, V)) ~> Type.FMap(K, V)
  }

}
