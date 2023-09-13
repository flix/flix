/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.util.collection

/**
  * A linear data structure that allows fast concatenation.
  */
sealed trait Chain[+A] extends Iterable[A] {

  /**
    * Returns an iterator over the chain, from left to right.
    */
  override def iterator: Iterator[A] = this match {
    case Chain.Empty => Iterator.empty
    case Chain.One(x) => Iterator.single(x)
    case Chain.Link(l, r) => Iterator.concat(l, r)
    case Chain.Many(cs) => Iterator.concat(cs: _*)
    case Chain.Proxy(xs) => xs.iterator
  }

  /**
    * Concatenates `this` chain and `that` chain.
    */
  def ++[B >: A](that: Chain[B]): Chain[B] = {
    if (this == Chain.Empty) {
      that
    } else if (that == Chain.Empty) {
      this
    } else {
      Chain.Link(this, that)
    }
  }

  /**
    * The empty chain.
    */
  override val empty: Chain[A] = Chain.Empty
}

object Chain {

  /**
    * The empty chain.
    */
  case object Empty extends Chain[Nothing]

  /**
    * A singleton chain.
    */
  case class One[A](x: A) extends Chain[A]

  /**
    * A concatenation of two chains.
    */
  case class Link[A](l: Chain[A], r: Chain[A]) extends Chain[A]

  /**
    * A concatenation of many chains.
    */
  case class Many[A](cs: Seq[Chain[A]]) extends Chain[A]

  /**
    * A chain wrapping a sequence.
    */
  case class Proxy[A](cs: Seq[A]) extends Chain[A]

  /**
    * Returns a chain containing the given elements.
    */
  def apply[A](xs: A*): Chain[A] = Chain.Proxy(xs)
}
