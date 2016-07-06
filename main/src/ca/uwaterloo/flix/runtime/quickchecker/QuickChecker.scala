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

package ca.uwaterloo.flix.runtime.quickchecker

import ca.uwaterloo.flix.runtime.verifier.SymVal
import ca.uwaterloo.flix.runtime.verifier.SymVal.Tuple

import scala.language.implicitConversions
import scala.util.Random

object QuickChecker {


  /////////////////////////////////////////////////////////////////////////////
  // Generator Streams                                                       //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A global random number generator.
    */
  val Random = new Random()

  /**
    * A common super-type for infinite generator streams.
    */
  trait Stream[A] {
    def head: A

    def tail: Stream[A]

    def #::(h: A): Stream[A] = Hd(h, this)
  }

  /**
    * A single stream element.
    */
  case class Hd[A](head: A, tail: Stream[A]) extends Stream[A]

  /**
    * Generates a stream of boolean values.
    */
  class GenBool extends Stream[SymVal] {
    val stream = SymVal.True #:: SymVal.False #:: this

    def head: SymVal = stream.head

    def tail: Stream[SymVal] = stream.tail
  }

  /**
    * Generates a stream of tuple values.
    */
  class GenTuple[A](gs: List[Stream[A]]) extends Stream[SymVal.Tuple] {
    def head: Tuple = ???

    def tail: Stream[Tuple] = ???
  }

  /**
    * Generates a stream of Int32 values.
    */
  class GenInt32 extends Stream[Int] {
    val stream = 0 #:: 1 #:: -1 #:: 2 #:: -2 #:: 3 #:: -3 #:: GenRandomInt32

    def head: Int = stream.head

    def tail: Stream[Int] = stream.tail
  }

  /**
    * Generates Random Int32 values.
    */
  object GenRandomInt32 extends Stream[Int] {
    def head: Int = Random.nextInt()

    def tail: Stream[Int] = GenRandomInt32
  }

  /**
    * Generates Random Int64 values.
    */
  object GenRandomInt64 extends Stream[SymVal] {
    def head: SymVal = SymVal.Int64(Random.nextLong())

    def tail: Stream[SymVal] = GenRandomInt64
  }

}
