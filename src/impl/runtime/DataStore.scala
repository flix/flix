package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym}
import impl.logic.Value

import scala.collection.mutable

class DataStore {

  val M = mutable.Map.empty[PSym, Relation]

  def get(p: PSym, q: List[Option[Value]]): Set[List[Value]] = ???

  def put(p: PSym, v: List[Value]): Boolean = ???


  trait Relation {
    def get(q: List[Option[Value]]): Set[List[Value]]

    def put(k: List[Value], v: List[Value]): Boolean

    def tuples: Set[List[Value]]
  }


  class Relation1 extends Relation {
    val v0 = mutable.Set.empty[List[Value]] // TODO: Actually needs to be a map? from a single key to the value?

    def get(q: List[Option[Value]]): Set[List[Value]] = q match {
      case Nil => v0.toSet
      case None :: Nil => v0.toSet
      case Some(x) :: Nil if v0 contains List(x) => Set(List(x))
      case xs => throw new RuntimeException(s"Illegal query: '$xs'.")
    }

    def put(k: List[Value], v: List[Value]): Boolean = k match {
      case Nil if v0 contains v => false
      case Nil => v0 += v; true
      case xs => throw new RuntimeException(s"Illegal key: '$xs'.")
    }

    def tuples: Set[List[Value]] = v0.toSet
  }

  class Relation2 extends Relation {
    val m0: mutable.Map[Value, Relation1] = mutable.Map.empty
    val m1: Option[mutable.Map[Value, Relation1]] = None

    def get(q: List[Option[Value]]): Set[List[Value]] = q match {
      case Some(v) :: xs => m0.get(v).map(_.get(xs)).getOrElse(Set.empty)
      case None :: Some(v) :: xs => m1 match {
        case None => ???
        case Some(i1) => ???
      }
      case xs => throw new RuntimeException(s"Illegal query: '$xs'.")
    }

    def put(k: List[Value], v: List[Value]): Boolean = ???

    def tuples: Set[List[Value]] = ???
  }

}
