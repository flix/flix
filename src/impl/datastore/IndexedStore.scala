package impl.datastore

import impl.logic.{Predicate, Symbol, Value}
import impl.runtime.Interpreter
import syntax.Symbols._
import syntax.Values._
import util.ascii.AsciiTable

import util.collection.mutable

/**
 * A datastore which is indexed left-to-right.
 */
class IndexedStore extends DataStore {

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
    case List(_) =>                                           map1.get(p.name).map(x => List(x)).toList

    case List(Some(v1), _) =>                                 map2.get(p.name, v1).map(x => List(v1, x)).toList
    case List(_, _) =>                                        map2.get(p.name).map({case (v1, v2) => List(v1, v2)}).toList

    case List(Some(v1), Some(v2), _) =>                       map3.get(p.name, v1, v2).map(x => List(v1, v2, x)).toList
    case List(_, _, _) =>                                     map3.get(p.name).map({case (v1, v2, v3) => List(v1, v2, v3)}).toList

    case List(Some(v1), Some(v2), Some(v3), _) =>             map4.get(p.name, v1, v2, v3).map(x => List(v1, v2, v3, x)).toList
    case List(_, _, _, _) =>                                  map4.get(p.name).map({case (v1, v2, v3, v4) => List(v1, v2, v3, v4)}).toList

    case List(Some(v1), Some(v2), Some(v3), Some(v4), _) =>   map5.get(p.name, v1, v2, v3, v4).map(x => List(v1, v2, v3, v4, x)).toList
    case List(_, _, _, _, _) =>                               map5.get(p.name).map({case (v1, v2, v3, v4, v5) => List(v1, v2, v3, v4, v5)}).toList

    case _ => throw new UnsupportedOperationException()
  }

  def output(): Unit = {
    if (map1.nonEmpty) {
      val t = new AsciiTable().mkCol("Predicate").mkCol("Value").mkRows(map1.toSeq.map {
        case (p, v1) => List(p.fmt, v1.fmt)
      })
      println(t.output)
    }

    if (map2.nonEmpty) {
      val t = new AsciiTable()
        .mkCol("Predicate").mkCol("Key1").mkCol("Value").mkRows(map2.toSeq.map {
        case (p, v1, v2) => List(p.fmt, v1.fmt, v2.fmt)
      })
      println(t.output)
    }

    if (map3.nonEmpty) {
      val t = new AsciiTable()
        .mkCol("Predicate").mkCol("Key1").mkCol("Key2").mkCol("Value").mkRows(map3.toSeq.map {
        case (p, v1, v2, v3) => List(p.fmt, v1.fmt, v2.fmt, v3.fmt)
      })
      println(t.output)
    }

    if (map4.nonEmpty) {
      val t = new AsciiTable()
        .mkCol("Predicate").mkCol("Key1").mkCol("Key2").mkCol("Key3").mkCol("Value")
        .mkRows(map4.toSeq.map {
        case (p, v1, v2, v3, v4) => List(p.fmt, v1.fmt, v2.fmt, v3.fmt, v4.fmt)
      })
      println(t.output)
    }

    if (map5.nonEmpty) {
      val t = new AsciiTable()
        .mkCol("Predicate").mkCol("Key1").mkCol("Key2").mkCol("Key3").mkCol("Key4").mkCol("Value")
        .mkRows(map5.toSeq.map {
        case (p, v1, v2, v3, v4, v5) => List(p.fmt, v1.fmt, v2.fmt, v3.fmt, v4.fmt, v5.fmt)
      })
      println(t.output)
    }
  }
}
