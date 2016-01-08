package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FMap {

  /**
    * A common super-type for all map operations.
    */
  sealed trait MapOperator extends LibraryOperator

  /**
    * All map operations.
    */
  val Ops: immutable.Map[Name.Resolved, MapOperator] = List(
    // Map Construction
    "Map/empty" -> empty,
    "Map/singleton" -> singleton,

    // TODO:
    "Map/null" -> nul,
    "Map/get" -> get,
    "Map/getWithDefault" -> getWithDefault,
    "Map/memberOf" -> memberOf,
    //  TODO: keys
    // TODO: values

    // Insert
    "Map/insert" -> insert,
    "Map/insertWith" -> insertWith,
    "Map/insertWithKey" -> insertWithKey,

    "Map/update" -> update,
    // TODO: adjust
    // TODO: alter  // TODO: adjustWithKey


    // Delete
    "Map/delete" -> delete,

    // TODO: differenceWith
    // TODO: differenceWithKey
    "Map/map" -> map,
    "Map/mapWithKey" -> mapWithKey,
    // TODO mapKeys

    // Combine Operations.
    "Map/union" -> union,
    "Map/unionWith" -> unionWith,
    "Map/unionWithKey" -> unionWithKey,

    "Map/intersection" -> intersection,
    // TODO: intersectionWith
    // TODO: intersectionWithKey

    "Map/difference" -> difference,
    "Map/differenceWith" -> differenceWith,
    "Map/differenceWithKey" -> differenceWithKey,

    // Conversions.
    "Map/toList" -> toAscList,
    "Map/toAscList" -> toAscList,
    "Map/toDescList" -> toDescList,
    "Map/toSet" -> toSet
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap


  // TODO - removeKey
  //TODO  - foldValues
  // TODO filterKeys
  // TODO: filter
  // TODO: filterWithKey
  // /filterValues
  // TODO  foldLeft/foldRight,
  // TODO foldLeftWithKey, foldRightWithKey
  // TODO  - keySet // TODO: keys or keySet?
  // TODO - values
  // TODO  filter/filterWithKey
  // TODO  paritition, partitionWithKey
  // TODO  isSubmapOf
  // TODO  isProperSubmapOf
  // TODO: minimum, maximum,

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
    val tpe = () ~> Map(K, V)
  }

  object singleton extends MapOperator {
    val tpe = (K, V) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends MapOperator {
    val tpe = Map(K, V) ~> Bool
  }

  object memberOf extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Bool
  }

  object get extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Opt(V)
  }

  object getWithDefault extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> V
  }

  /////////////////////////////////////////////////////////////////////////////
  // Insert                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object insert extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  object insertWith extends MapOperator {
    val tpe = ((V, V) ~> V, K, V, Map(K, V)) ~> Map(K, V)
  }

  object insertWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, K, V, Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Update                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object update extends MapOperator {
    val tpe = (K, V ~> V, Map(K, V)) ~> Map(K, V)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Delete                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  object delete extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object union extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  object unionWith extends MapOperator {
    val tpe = ((V, V) ~> V, Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  object unionWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `intersection : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object intersection extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }


  object difference extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  object differenceWith extends MapOperator {
    val tpe = ((V, V) ~> V, Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  object differenceWithKey extends MapOperator {
    val tpe = ((K, V, V) ~> V, Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Map                                                                     //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (A => B, Map[K, A]) => Map[K, B]` function.
    */
  object map extends MapOperator {
    val tpe = (A ~> B, Map(K, A)) ~> Map(K, B)
  }

  /**
    * The `mapWithKey : ((K, A) => B, Map[K, A]) => Map[K, B]` function.
    */
  object mapWithKey extends MapOperator {
    val tpe = ((K, A) ~> B, Map(K, A)) ~> Map(K, B)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  object toAscList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  object toDescList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  object toSet extends MapOperator {
    val tpe = Map(K, V) ~> Set((K, V))
  }


}
