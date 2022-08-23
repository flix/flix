/*
 * Copyright 2022 Matthew Lutze
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

import scala.collection.{SeqFactory, SeqOps, mutable}

/**
  * Nonempty list type
  */
case class Nel[A](override val head: A, override val tail: List[A]) extends Seq[A] with SeqOps[A, Nel, Seq[A]] {
  override def length: Int = 1 + tail.length

  override def iterator: Iterator[A] = Iterator(head) ++ tail

  override def iterableFactory: SeqFactory[Nel] = Nel.Factory

  override def apply(i: Int): A = if (i == 0) head else tail(i - 1)
}

object Nel {
  object Factory extends SeqFactory[Nel] {
    override def from[A](source: IterableOnce[A]): Nel[A] = {
      val it = source.iterator
      val head = it.next()
      val tail = it.toList
      Nel(head, tail)
    }

    override def empty[A]: Nel[A] = throw new UnsupportedOperationException

    override def newBuilder[A]: mutable.Builder[A, Nel[A]] = throw new UnsupportedOperationException
  }
}
