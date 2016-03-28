package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.runtime.Model

import scala.collection.JavaConverters._

final class WrappedModel(val model: Model) extends IModel {

  def getConstant(name: String): IValue = ??? // TODO

  def getRelation(name: String): java.lang.Iterable[Array[IValue]] = {
    model.getRelationOpt(name) match {
      case None => throw new IllegalArgumentException(s"Unknown relation: '$name'.")
      case Some(iterable) => iterable.map {
        case row => row.map(v => new WrappedValue(v): IValue).toArray
      }.asJava
    }
  }

  def getLattice(name: String): java.lang.Iterable[Array[IValue]] = {
    model.getLatticeOpt(name) match {
      case None => throw new IllegalArgumentException(s"Unknown relation: '$name'.")
      case Some(iterable) => iterable.map {
        case (keys, values) => (keys ::: values).map(e => new WrappedValue(e): IValue).toArray
      }.asJava
    }
  }

}
