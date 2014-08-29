package impl.datastore

import impl.logic.{Predicate, Symbol, Value}
import impl.runtime.Interpreter
import syntax.Symbols._
import syntax.Values._

import util.collection.mutable

/**
 * A datastore which is indexed left-to-right.
 */
class IndexedStore {

  val map1 = mutable.Map1.empty[Symbol.PredicateSymbol, Value]
  val map2 = mutable.Map2.empty[Symbol.PredicateSymbol, Value, Value]
  val map3 = mutable.Map3.empty[Symbol.PredicateSymbol, Value, Value, Value]
  val map4 = mutable.Map4.empty[Symbol.PredicateSymbol, Value, Value, Value, Value]
  val map5 = mutable.Map5.empty[Symbol.PredicateSymbol, Value, Value, Value, Value, Value]

  def store(p: Predicate.GroundPredicate): Unit = p.values match {
    case List(v1) =>                    map1.put(p.name, v1)
    case List(v1, v2) =>                map2.put(p.name, v1, v2)
    case List(v1, v2, v3) =>            map3.put(p.name, v1, v2, v3)
    case List(v1, v2, v3, v4) =>        map4.put(p.name, v1, v2, v3, v4)
    case List(v1, v2, v3, v4, v5) =>    map5.put(p.name, v1, v2, v3, v4, v5)
    case _ => throw new UnsupportedOperationException()
  }

  def lookup(p: Predicate.GroundPredicate): Option[Value] = p.values match {
    case List(_) =>                     map1.get(p.name)
    case List(v1, _) =>                 map2.get(p.name, v1)
    case List(v1, v2, _) =>             map3.get(p.name, v1, v2)
    case List(v1, v2, v3, _) =>         map4.get(p.name, v1, v2, v3)
    case List(v1, v2, v3, v4, _) =>     map5.get(p.name, v1, v2, v3, v4)
    case _ => throw new UnsupportedOperationException()
  }

  def query(p: Predicate): List[List[Value]] = p.terms.map(t => Interpreter.evaluateOpt(t)) match {
    case List(_) =>                     map1.get(p.name).map(x => List(x)).toList

    case List(Some(v1), _) =>           map2.get(p.name, v1).map(x => List(v1, x)).toList
    case List(_, _) =>                  map2.get(p.name).map({case (v1, v2) => List(v1, v2)}).toList

    case List(Some(v1), Some(v2), _) => map3.get(p.name, v1, v2).map(x => List(v1, v2, x)).toList
    case List(_, _, _) =>               map3.get(p.name).map({case (v1, v2, v3) => List(v1, v2, v3)}).toList

    // TODO: Rest

    case _ => throw new UnsupportedOperationException()
  }

  def output(): Unit = {
    for ((p, v1) <- map1) {
      println(p.fmt + "(" + v1.fmt + ").")
    }
    for ((p, v1, v2) <- map2) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ").")
    }
    for ((p, v1, v2, v3) <- map3) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ").")
    }
    for ((p, v1, v2, v3, v4) <- map4) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ").")
    }
    for ((p, v1, v2, v3, v4, v5) <- map5) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ", " + v5.fmt + ").")
    }
  }
}
