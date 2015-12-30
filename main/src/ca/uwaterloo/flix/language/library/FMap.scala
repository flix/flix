package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FMap {

  /**
    * All map operations.
    */
  val Ops: immutable.Map[Name.Resolved, MapOperator] = List(
    "Map::null" -> nul,
    "Map::get" -> get,
    "Map::getWithDefault" -> getWithDefault,
    "Map::memberOf" -> memberOf,

    "Map::insert" -> insert,
  // TODOL insertWith
  // TODO: insertWithKey

    "Map::update" -> update,
// TODO: Update with key
    "Map::delete" -> delete,
// TODO: alter
    "Map::union" -> union,

    // TODO: adjust
    // TODO: adjustWithKey
  // TODO: unionWith
  // TODO: unionWithKey
    // TODO: Empty, singleton

    "Map::intersection" -> intersection,
  // TODO: intersectionWith
  // TODO: intersectionWithKey
    "Map::difference" -> difference,
  // TODO: differenceWith
  // TODO: differenceWithKey
    "Map::map" -> map,
    "Map::mapWithKey" -> mapWithKey,
  // TODO mapKeys
    "Map::toAscList" -> toAscList,
    "Map::toDescList" -> toDescList,
    "Map::toSet" -> toSet
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

  // TODO: mapEither??? then we need either.

  // Indexed:
  // TODO: lookupIndex, deleteAt, updateAt - wait do we need these?

  // TODO: minimum, maximum,

  /**
    * A common super-type for all map operations.
    */
  sealed trait MapOperator extends LibraryOperator

  /**
    * Generic type variables.
    */
  val K = Type.Var("K")
  val V = Type.Var("V")
  val A = Type.Var("A")
  val B = Type.Var("B")

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
  // Insert / Update / Delete                                                //
  /////////////////////////////////////////////////////////////////////////////
  object insert extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  object insertWith extends MapOperator {
    val tpe = ((V, V) ~> V, K, V, Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `update : (K, V => V, Map[K, V]) => Map[K, V]` function.
    */
  object update extends MapOperator {
    val tpe = (K, V ~> V, Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `delete : (K, V, Map[K, V]) => Map[K, V]` function.
    */
  object delete extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `union : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object union extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `intersection : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object intersection extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `difference : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object difference extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
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
  /**
    * The `toAscList : Map[K, V] => Lst[(K, V)]` function.
    */
  object toAscList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  /**
    * The `toDescList : Map[K, V] => Lst[(K, V)]` function.
    */
  object toDescList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  /**
    * The `toSet : Map[K, V] => Set[(K, V)]` function.
    */
  object toSet extends MapOperator {
    val tpe = Map(K, V) ~> Set((K, V))
  }


}
