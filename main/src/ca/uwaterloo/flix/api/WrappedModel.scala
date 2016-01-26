package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.runtime.Model

import scala.collection.JavaConverters._

protected class WrappedModel(private val model: Model) extends IModel {

  def getConstants: java.lang.Iterable[String] =
    model.constants.keys.map(_.fqn).asJava

  def getRelations: java.lang.Iterable[String] =
    model.relations.keys.map(_.fqn).asJava

  def getLattices: java.lang.Iterable[String] =
    model.lattices.keys.map(_.fqn).asJava

  def getConstant(name: String): IValue = {
    val rname = Name.Resolved.mk(name)
    model.constants.get(rname) match {
      case None => throw new IllegalArgumentException(s"Unknown constant: '$name'.")
      case Some(v) => new WrappedValue(v)
    }
  }

  def getRelation(name: String): java.lang.Iterable[Array[IValue]] = {
    val rname = Name.Resolved.mk(name)
    model.relations.get(rname) match {
      case None => throw new IllegalArgumentException(s"Unknown relation: '$name'.")
      case Some(iterable) => iterable.map(row => row.map(v => new WrappedValue(v): IValue).toArray).asJava
    }
  }

  def getLattice(name: String): java.lang.Iterable[Array[IValue]] = {
    throw new UnsupportedOperationException("Not Implemented Yet. Sorry.")
  }

  def isFact(name: String, fact: Array[IValue]): Boolean = {
    throw new UnsupportedOperationException("Not Implemented Yet. Sorry.")
  }

}
