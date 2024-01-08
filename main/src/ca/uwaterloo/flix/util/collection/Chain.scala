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

import scala.collection.mutable.ListBuffer

/**
  * A linear data structure that allows fast concatenation.
  */
sealed trait Chain[+A] {

  /**
    * Returns an iterator over the chain, from left to right.
    */
  final def iterator: Iterator[A] = this match {
    case Chain.Empty => Iterator.empty
    case Chain.Link(l, r) => l.iterator ++ r.iterator
    case Chain.Many(cs) => cs.flatMap(_.iterator).iterator
    case Chain.Proxy(xs) => xs.iterator
  }

  /**
    * Concatenates `this` chain and `that` chain.
    */
  final def ++[B >: A](that: Chain[B]): Chain[B] = {
    if (this == Chain.Empty) {
      that
    } else if (that == Chain.Empty) {
      this
    } else {
      Chain.Link(this, that)
    }
  }

  final def isEmpty: Boolean = this match {
    case Chain.Empty => true
    case Chain.Link(l, r) => l.isEmpty && r.isEmpty
    case Chain.Many(cs) => cs.forall(_.isEmpty)
    case Chain.Proxy(xs) => xs.isEmpty
  }

  final def head: Option[A] = this match {
    case Chain.Empty => None
    case Chain.Link(l, _) => l.head
    case Chain.Many(cs) => cs.find(_.head.isDefined).flatMap(_.head)
    case Chain.Proxy(xs) => xs.headOption
  }

  /**
    * Returns `this` as a [[List]].
    */
  def toList: List[A]

  final def toSeq: Seq[A] = this match {
    case Chain.Empty => Seq.empty
    case Chain.Link(l, r) => l.toSeq ++ r.toSeq
    case Chain.Many(cs) => cs.flatMap(_.toSeq)
    case Chain.Proxy(xs) => xs
  }

  final def map[B](f: A => B): Chain[B] = this match {
    case Chain.Empty => Chain.empty
    case Chain.Link(l, r) => Chain.Link(l.map(f), r.map(f))
    case Chain.Many(cs) => Chain.Many(cs.map(_.map(f)))
    case Chain.Proxy(xs) => Chain.Proxy(xs.map(f))
  }

  final def foreach(f: A => Unit): Unit = this match {
    case Chain.Empty => ()
    case Chain.Link(l, r) => l.foreach(f); r.foreach(f)
    case Chain.Many(cs) => cs.foreach(_.foreach(f))
    case Chain.Proxy(xs) => xs.foreach(f)
  }

  final def exists(f: A => Boolean): Boolean = this match {
    case Chain.Empty => false
    case Chain.Link(l, r) => l.exists(f) || r.exists(f)
    case Chain.Many(cs) => cs.exists(_.exists(f))
    case Chain.Proxy(xs) => xs.exists(f)
  }

  final def mkString(sep: String): String = this.toList.mkString(sep)

  /**
    * The empty chain.
    */
  final val empty: Chain[A] = Chain.Empty

  /**
    * Returns the amount of elements in the chain.
    */
  final def length: Int = this match {
    case Chain.Empty => 0
    case Chain.Link(l, r) => l.length + r.length
    case Chain.Many(cs) => cs.map(_.length).sum
    case Chain.Proxy(xs) => xs.length
  }

  final override def hashCode(): Int = this.toList.hashCode()

  final override def equals(obj: Any): Boolean = obj match {
    case that: Chain[_] => this.toList == that.toList
    case _ => false
  }
}

object Chain {

  /**
    * The empty chain.
    */
  private case object Empty extends Chain[Nothing] {

    /**
      * Returns `this` as a [[List]].
      */
    override def toList: List[Nothing] = List.empty
  }

  /**
    * A concatenation of two chains.
    */
  private case class Link[A](l: Chain[A], r: Chain[A]) extends Chain[A] {

    /**
      * Returns `this` as a [[List]].
      */
    override def toList: List[A] = l.toList ++ r.toList
  }

  /**
    * A concatenation of many chains.
    */
  private case class Many[A](cs: Seq[Chain[A]]) extends Chain[A] {

    /**
      * Returns `this` as a [[List]].
      */
    override def toList: List[A] = {
      val buf = ListBuffer.empty[A]
      cs.foreach(c => buf.addAll(c.iterator))
      buf.toList
    }
  }

  /**
    * A chain wrapping a sequence.
    */
  private case class Proxy[A](xs: Seq[A]) extends Chain[A] {

    /**
      * Returns `this` as a [[List]].
      */
    override def toList: List[A] = xs.toList
  }

  /**
    * Returns a chain containing the given elements.
    */
  def apply[A](xs: A*): Chain[A] = from(xs)

  /**
    * Returns a chain containing the given elements.
    */
  def from[A](xs: Seq[A]): Chain[A] = if (xs.isEmpty) Chain.empty else Chain.Proxy(xs)

  /**
    * Returns a chain containing the given elements.
    */
  def apply[A](xs: Iterable[A]): Chain[A] = from(xs)

  /**
    * Returns a chain containing the given elements.
    */
  def from[A](xs: Iterable[A]): Chain[A] = if (xs.isEmpty) Chain.empty else Chain.Proxy(xs.toSeq)

  /**
    * The empty chain.
    */
  val empty: Chain[Nothing] = Chain.Empty

  /**
    * Concatenates the given sequence of chains, in order.
    */
  def concat[A](cs: Seq[Chain[A]]): Chain[A] = Chain.Many(cs)

}
