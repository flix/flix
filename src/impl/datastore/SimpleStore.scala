package impl.datastore

import impl.logic.{Predicate, Symbol, Value}
import syntax.Symbols._
import syntax.Values._

import scala.collection.mutable

/**
 * A datastore which is not indexed.
 */
class SimpleStore extends DataStore {

  val map1 = mutable.Map.empty[ Symbol.PredicateSymbol, Value]
  val map2 = mutable.Map.empty[(Symbol.PredicateSymbol, Value), Value]
  val map3 = mutable.Map.empty[(Symbol.PredicateSymbol, Value, Value), Value]
  val map4 = mutable.Map.empty[(Symbol.PredicateSymbol, Value, Value, Value), Value]
  val map5 = mutable.Map.empty[(Symbol.PredicateSymbol, Value, Value, Value, Value), Value]

  def store(p: Predicate.GroundPredicate): Unit = p.values match {
    case List(v1) =>                    map1.put(p.name, v1)
    case List(v1, v2) =>                map2.put((p.name, v1), v2)
    case List(v1, v2, v3) =>            map3.put((p.name, v1, v2), v3)
    case List(v1, v2, v3, v4) =>        map4.put((p.name, v1, v2, v3), v4)
    case List(v1, v2, v3, v4, v5) =>    map5.put((p.name, v1, v2, v3, v4), v5)
    case _ => throw new UnsupportedOperationException()
  }

  def lookup(p: Predicate.GroundPredicate): Option[Value] = p.values match {
    case List(_) =>                     map1.get(p.name)
    case List(v1, _) =>                 map2.get((p.name, v1))
    case List(v1, v2, _) =>             map3.get((p.name, v1, v2))
    case List(v1, v2, v3, _) =>         map4.get((p.name, v1, v2, v3))
    case List(v1, v2, v3, v4, _) =>     map5.get((p.name, v1, v2, v3, v4))
    case _ => throw new UnsupportedOperationException()
  }

  def query(p: Predicate): List[List[Value]] = p.terms match {
    case List(_) =>                     map1.toList.collect({case (s, v1)                   if p.name == s => List(v1)})
    case List(t1, _) =>                 map2.toList.collect({case ((s, v1), v2)             if p.name == s => List(v1, v2)})
    case List(t1, t2, _) =>             map3.toList.collect({case ((s, v1, v2), v3)         if p.name == s => List(v1, v2, v3)})
    case List(t1, t2, t3, _) =>         map4.toList.collect({case ((s, v1, v2, v3), v4)     if p.name == s => List(v1, v2, v3, v4)})
    case List(t1, t2, t3, t4, _) =>     map5.toList.collect({case ((s, v1, v2, v3, v4), v5) if p.name == s => List(v1, v2, v3, v4, v5)})
    case _ => throw new UnsupportedOperationException()
  }

  def output(): Unit = {
    for ((p, v1) <- map1) {
      println(p.fmt + "(" + v1.fmt + ").")
    }
    for (((p, v1), v2) <- map2) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ").")
    }
    for (((p, v1, v2), v3) <- map3) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ").")
    }
    for (((p, v1, v2, v3), v4) <- map4) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ").")
    }
    for (((p, v1, v2, v3, v4), v5) <- map5) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ", " + v5.fmt + ").")
    }
  }
}
