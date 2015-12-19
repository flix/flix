package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FMap {

  // TODO: Check naming, escp w.r.t. llist, set, etc.

  /**
    * All map operations.
    */
  val Ops: immutable.Map[Name.Resolved, MapOperator] = List(
    "Map::isEmpty" -> IsEmpty,
    "Map::isMember" -> IsMember,
    "Map::lookup" -> Lookup,
    "Map::empty" -> Empty,
    "Map::singleton" -> Singleton,
    "Map::insert" -> Insert,
    "Map::update" -> Update,
    "Map::delete" -> Delete,
    "Map::union" -> Union,
    "Map::intersection" -> Intersection,
    "Map::difference" -> Difference,
    "Map::toList" -> ToList,
    "Map::toSet" -> ToSet
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap

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
  /**
    * The `isEmpty : Map[K, V] => Bool` function.
    */
  object IsEmpty extends MapOperator {
    val tpe = Map(K, V) ~> Bool
  }

  /**
    * The `isMember : (K, Map[K, V]) => Bool` function.
    */
  object IsMember extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Bool
  }

  /**
    * The `lookup : (K, Map[K, V]) => Opt[V]` function.
    */
  object Lookup extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Opt(V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Construction                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `empty : Unit => Map[K, V]` function.
    */
  object Empty extends MapOperator {
    val tpe = Unit ~> Map(K, V)
  }

  /**
    * The `singleton : (K, V) => Map[K, V]` function.
    */
  object Singleton extends MapOperator {
    val tpe = (K, V) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Insert / Update / Delete                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `insert : (K, V, Map[K, V]) => Map[K, V]` function.
    */
  object Insert extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `delete : (K, V => V, Map[K, V]) => Map[K, V]` function.
    */
  object Update extends MapOperator {
    val tpe = (K, V ~> V, Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `delete : (K, V, Map[K, V]) => Map[K, V]` function.
    */
  object Delete extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `union : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object Union extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `intersection : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object Intersection extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `difference : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object Difference extends MapOperator {
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


  // TODO: Map
  // - removeKey
  // - foldValues
  // - filterKeys/filterValues/exists/find/fold
  // - map
  // - mapWithKey
  // - mapKeys
  // foldLeft/foldRigyht, foldLeftWithKey, foldRightWithKey
  // - elms
  // - keys
  // filter/filterWithKey
  // paritition, partitionWithKey
  // isSubmapOf
  // isProperSubmapOf


  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `toList : Map[K, V] => Lst[(K, V)]` function.
    */
  object ToList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  /**
    * The `toSet : Map[K, V] => Set[(K, V)]` function.
    */
  object ToSet extends MapOperator {
    val tpe = Map(K, V) ~> Set((K, V))
  }


}
