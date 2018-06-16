package ca.uwaterloo.flix.runtime.solver.datastore

import ca.uwaterloo.flix.util.InternalRuntimeException

/**
  * A proxy object wraps a raw Flix object with appropriate methods for equality, hashCode, and toString.
  */
class ProxyObject(v: AnyRef, eq: (AnyRef, AnyRef) => Boolean, hash: AnyRef => Int, toStr: AnyRef => String) {

  /**
    * Returns the wrapped value.
    */
  def getValue: AnyRef = v

  /**
    * Returns `true` if the two wrapped object are considered equal.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ProxyObject =>
      if (eq == null) v == that.getValue else eq(v, that.getValue)
    case _ => throw InternalRuntimeException(s"Unexpected value: '$obj'.")
  }

  /**
    * Returns the hashCode of the wrapped value.
    */
  override def hashCode(): Int =
    if (hash == null) v.hashCode() else hash(v)

  /**
    * Returns the string representation of the wrapped value.
    */
  override def toString: String =
    if (toStr == null) v.toString else toStr(v)

}
