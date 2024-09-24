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

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** A linear data structure that allows fast concatenation. */
sealed trait Chain[+A] {

  /** Concatenates `this` chain and `that` chain. */
  final def ++[B >: A](that: Chain[B]): Chain[B] = {
    if (this == Chain.Empty) {
      that
    } else if (that == Chain.Empty) {
      this
    } else {
      Chain.Link(this, that)
    }
  }

  /** The empty chain. */
  final val empty: Chain[A] = Chain.Empty

  /** Returns `true` if and only if `this` contains no elements. */
  final def isEmpty: Boolean = this match {
    case Chain.Empty => true
    case Chain.Link(l, r) => l.isEmpty && r.isEmpty
    case Chain.Proxy(xs) => xs.isEmpty
  }

  /** Returns the leftmost element if any exists. */
  @tailrec
  final def head: Option[A] = this match {
    case Chain.Empty => None
    case Chain.Link(Chain.empty, r) => r.head
    case Chain.Link(l, _) => l.head
    case Chain.Proxy(xs) => xs.headOption
  }

  /** Returns the amount of elements in the chain. */
  final def length: Int = this match {
    case Chain.Empty => 0
    case Chain.Link(l, r) => l.length + r.length
    case Chain.Proxy(xs) => xs.length
  }

  /** Returns `true` if and only if an element in `this` satisfies the predicate `f`. */
  final def exists(f: A => Boolean): Boolean = this match {
    case Chain.Empty => false
    case Chain.Link(l, r) => l.exists(f) || r.exists(f)
    case Chain.Proxy(xs) => xs.exists(f)
  }

  /** Returns a new [[Chain]] with `f` applied to every element in `this`. */
  final def map[B](f: A => B): Chain[B] = this match {
    case Chain.Empty => Chain.empty
    case Chain.Link(l, r) => Chain.Link(l.map(f), r.map(f))
    case Chain.Proxy(xs) => Chain.Proxy(xs.map(f))
  }

  /** Applies `f` this every element in `this`. */
  final def foreach(f: A => Unit): Unit = this match {
    // N.B.: We must match on the type to avoid
    // infinite recursion when pattern matching
    // since it calls the equals method which
    // depends on foreach.
    case _: Chain.Empty.type => ()
    case c: Chain.Link[_] => c.l.foreach(f); c.r.foreach(f)
    case c: Chain.Proxy[_] => c.xs.foreach(f)
  }

  /** Returns `this` as a [[Seq]]. */
  final def toSeq: Seq[A] = this match {
    case Chain.Empty => Seq.empty
    case Chain.Link(l, r) => l.toSeq ++ r.toSeq
    case Chain.Proxy(xs) => xs
  }

  /** Returns `this` as a [[List]]. */
  final def toList: List[A] = {
    val buf = new ListBuffer[A]
    this.foreach(buf.addOne)
    buf.toList
  }

  /** Displays all elements of this collection in a string using a separator string. */
  final def mkString(sep: String): String = {
    val sb = new StringBuilder()
    this.foreach { x =>
      sb.addAll(x.toString); sb.addAll(sep)
    }
    sb.dropRight(sep.length)
    sb.mkString
  }

  final override def hashCode(): Int = this.toList.hashCode()

  final override def equals(obj: Any): Boolean = obj match {
    case that: Chain[_] => this.toList == that.toList
    case _ => false
  }

  final override def toString: String = {
    s"Chain(${this.mkString(", ")})"
  }
}

object Chain {

  /** The empty chain. */
  private case object Empty extends Chain[Nothing]

  /** A concatenation of two chains. */
  private case class Link[A](l: Chain[A], r: Chain[A]) extends Chain[A]

  /** A chain wrapping a sequence. */
  private case class Proxy[A](xs: Seq[A]) extends Chain[A]

  /** The empty chain. */
  val empty: Chain[Nothing] = Chain.Empty

  /** Returns a chain containing the given elements. */
  def apply[A](xs: A*): Chain[A] = from(xs)

  /** Returns a chain containing the given elements. */
  def from[A](xs: Seq[A]): Chain[A] = if (xs.isEmpty) Chain.empty else Chain.Proxy(xs)

  /** Returns a chain containing the given elements. */
  def from[A](xs: Iterable[A]): Chain[A] = if (xs.isEmpty) Chain.empty else Chain.Proxy(xs.toSeq)

}
