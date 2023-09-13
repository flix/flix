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
    case Chain.Many(l, r) => Iterator.concat(l, r)
  }

  /**
    * Concatenates `this` chain and `that` chain.
    */
  def ++[B >: A](that: Chain[B]) : Chain[B] = (this, that) match {
    case (c1, Chain.Empty) => c1
    case (Chain.Empty, c2) => c2
    case (c1, c2) => Chain.Many(c1, c2)
  }

  /**
    * The empty chain.
    */
  override val empty: Chain[A] = Chain.Empty
}

object Chain {
  case object Empty extends Chain[Nothing]
  case class One[A](x: A) extends Chain[A]

  case class Many[A](l: Chain[A], r: Chain[A]) extends Chain[A]
}
