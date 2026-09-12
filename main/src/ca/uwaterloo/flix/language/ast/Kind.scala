/*
 * Copyright 2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.fmt.FormatKind
import ca.uwaterloo.flix.util.ConcurrentCache

import scala.annotation.tailrec

/**
  * A kind represents the "type" of a type expression.
  */
sealed trait Kind {

  /**
    * Constructs an arrow kind.
    *
    * This is a right-associative operator, i.e., the following two kinds are equivalent:
    *
    *   - `Kind.Star ->: Kind.Star ->: Kind.Star`
    *   - `Kind.Star ->: (Kind.Star ->: Kind.Star)`
    */
  def ->:(left: Kind): Kind = Kind.mkArrow(left, this)

  /**
    * Returns a human readable representation of `this` kind.
    */
  override def toString: String = FormatKind.formatKind(this)


}

object Kind {

  /**
    * A cache to reuse kinds.
    */
  private val Cache: ConcurrentCache[Kind] = new ConcurrentCache

  /**
    * Represents a wild kind.
    * A wild kind exists during the kinding phase, but should be eliminated before the following phase,
    * unless the kind is deemed irrelevant (e.g. the kind of a wildcard type).
    */
  case object Wild extends Kind

  /**
    * Represents the wildcard kind only matching Case Sets.
    * A wild kind exists during the kinding phase, but should be eliminated before the following phase,
    * unless the kind is deemed irrelevant (e.g. the kind of a wildcard type).
    */
  case object WildCaseSet extends Kind

  /**
    * Represents the kind of types.
    */
  case object Star extends Kind

  /**
    * Represents the kind of effect sets.
    */
  case object Eff extends Kind

  /**
    * Represents the kind of Boolean formulas
    */
  case object Bool extends Kind

  /**
    * Represents the kind of record rows.
    */
  case object RecordRow extends Kind

  /**
    * Represents the kind of schema rows.
    */
  case object SchemaRow extends Kind

  /**
    * Represents the kind of predicates.
    */
  case object Predicate extends Kind

  /**
   * Represents the kind of a Java constructor, method, or field.
   */
  case object Jvm extends Kind

  /**
    * Represents the kind of sets of restrictable enum cases.
    */
  case class CaseSet(sym: Symbol.RestrictableEnumSym) extends Kind

  /**
    * Represents the kind of type expressions `k1 -> k2`.
    */
  case class Arrow(k1: Kind, k2: Kind) extends Kind

  /**
    * Represents an error kind.
    */
  case object Error extends Kind

  /**
    * A smart constructor for the [[Arrow]] kind.
    */
  def mkArrow(k1: Kind, k2: Kind): Kind = Cache.getCanonicalValue(Arrow(k1, k2))

  /**
    * Returns the kind: * -> (* ... -> *).
    */
  def mkArrow(numArgs: Int): Kind = mkArrowTo(numArgs, Kind.Star)

  /**
    * Returns the kind * -> (* -> ... ret).
    */
  def mkArrowTo(numArgs: Int, ret: Kind): Kind = {
    if (numArgs == 0) {
      ret
    } else {
      mkArrowTo(numArgs - 1, Star ->: ret)
    }
  }

  /**
    * Returns the kind: k1 -> (k2 ... -> (kn -> *)) for the given list of kinds `ks`.
    */
  def mkArrow(ks: List[Kind]): Kind = mkArrowTo(ks, Star)

  /**
    * Returns the kind: k1 -> (k2 ... -> (kn -> ret)) for the given list of kinds `ks`.
    */
  def mkArrowTo(ks: List[Kind], ret: Kind): Kind = ks match {
    case Nil => ret
    case x :: xs => mkArrow(x, mkArrowTo(xs, ret))
  }

  /**
    * Returns the base of an arrow kind.
    */
  @tailrec
  def base(k: Kind): Kind = k match {
    case Arrow(k1, _) => base(k1)
    case _ => k
  }

  /**
    * Returns the result of an arrow kind.
    */
  @tailrec
  def resultKind(k: Kind): Kind = k match {
    case Kind.Arrow(_, k2) => resultKind(k2)
    case _ => k
  }

  /**
    * Returns the arguments of an arrow kind.
    */
  def kindArgs(k: Kind): List[Kind] = k match {
    case Arrow(k1, k2) => k1 :: kindArgs(k2)
    case _ => Nil
  }

  /**
    * Traverses the kind and returns `true` if `k` contains [[Kind.Error]].
    * Returns `false` otherwise.
    */
  def hasError(k: Kind): Boolean = k match {
    case Wild => false
    case WildCaseSet => false
    case Star => false
    case Eff => false
    case Bool => false
    case RecordRow => false
    case SchemaRow => false
    case Predicate => false
    case Jvm => false
    case CaseSet(_) => false
    case Arrow(k1, k2) => hasError(k1) || hasError(k2)
    case Error => true
  }
}
