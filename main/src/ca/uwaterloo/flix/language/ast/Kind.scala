/*
 * Copyright 2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
  def mkArrow(ks: List[Kind]): Kind = ks match {
    case Nil => Star
    case x :: xs => mkArrow(x, mkArrow(xs))
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
