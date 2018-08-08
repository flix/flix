/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.util

sealed trait Validation[+Value, +Error] {

  import Validation._

  /**
    * Returns `true` iff this is a [[Success]] object.
    */
  final def isSuccess: Boolean = this match {
    case v: Success[Value, Error] => true
    case _ => false
  }

  /**
    * Returns `true` iff this is a [[Failure]] object.
    */
  final def isFailure: Boolean = !isSuccess

  /**
    * Returns the value inside `this` [[Success]] object.
    *
    * Throws an exception if `this` is a [[Failure]] object.
    */
  final def get: Value = this match {
    case Success(value) => value
    case Failure(errors) => throw new RuntimeException(s"Attempt to retrieve value from Failure. The errors are: ${errors.mkString(", ")}")
  }

  /**
    * Returns a [[Success]] containing the result of applying `f` to the value in this validation (if it exists).
    *
    * Preserves the errors.
    */
  final def map[Output](f: Value => Output): Validation[Output, Error] = this match {
    case Success(value) => Success(f(value))
    case Failure(errors) => Failure(errors)
  }

  /**
    * Similar to `map` but does not wrap the result in a [[Success]].
    *
    * Preserves the errors.
    */
  final def flatMap[Output, A >: Error](f: Value => Validation[Output, A]): Validation[Output, A] = this match {
    case Success(input) => f(input) match {
      case Success(value) => Success(value)
      case Failure(thatErrors) => Failure(errors #::: thatErrors)
    }
    case Failure(errors) => Failure(errors)
  }

  /**
    * Returns the errors in this [[Success]] or [[Failure]] object.
    */
  def errors: Stream[Error]

}

object Validation {

  /**
    * Represents a success `value`.
    */
  case class Success[T, E](t: T) extends Validation[T, E] {
    def errors: Stream[E] = Stream.empty
  }

  /**
    * Represents a failure with no value and `errors`.
    */
  case class Failure[T, E](errors: Stream[E]) extends Validation[T, E]

  /**
    * Sequences the given list of validations `xs`.
    */
  // TODO: Rename to sequence.
  def seqM[T, E](xs: Traversable[Validation[T, E]]): Validation[List[T], E] = {
    val zero = Success(List.empty[T]): Validation[List[T], E]
    xs.foldRight(zero) {
      case (Success(curValue), Success(accValue)) =>
        Success(curValue :: accValue)
      case (Success(_), Failure(accErrors)) =>
        Failure(accErrors)
      case (Failure(curErrors), Success(_)) =>
        Failure(curErrors)
      case (Failure(curErrors), Failure(accErrors)) =>
        Failure(curErrors #::: accErrors)
    }
  }

  /**
    * Sequences the validation T1 and T2 into a validation pair.
    */
  // TODO: Rename to sequence.
  // TODO: Tests
  def seqM[T1, T2, E](a: Validation[T1, E], b: Validation[T2, E]): Validation[(T1, T2), E] =
    (a, b) match {
      case (Success(valueA), Success(valueB)) =>
        Success((valueA, valueB))
      case _ => Failure(b.errors #::: a.errors)
    }

  /**
    * Traverses `xs` while applying the function `f`.
    */
  // TODO: Use traverse moe often.
  // TODO: Performance.
  def traverse[T, S, E](xs: Traversable[T])(f: T => Validation[S, E]): Validation[List[S], E] = xs.toList match {
    case Nil => Success(Nil)
    case y :: ys =>
      val s = f(y)
      traverse(ys)(f) flatMap {
        case rs => s map (r => r :: rs)
      }
  }

  /**
    * Maps over t1 and t2.
    */
  def mapN[T1, T2, U, E](t1: Validation[T1, E], t2: Validation[T2, E])
                        (f: (T1, T2) => U): Validation[U, E] =
    (t1, t2) match {
      case (Success(v1), Success(v2)) => Success(f(v1, v2))
      case _ => Failure(t1.errors #::: t2.errors)
    }

  /**
    * Maps over t1, t2, and t3.
    */
  def mapN[T1, T2, T3, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E])
                            (f: (T1, T2, T3) => U): Validation[U, E] =
    (t1, t2, t3) match {
      case (Success(v1), Success(v2), Success(v3)) => Success(f(v1, v2, v3))
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors)
    }

  /**
    * Maps over t1, t2, t3, and t4.
    */
  def mapN[T1, T2, T3, T4, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                 t4: Validation[T4, E])
                                (f: (T1, T2, T3, T4) => U): Validation[U, E] =
    (t1, t2, t3, t4) match {
      case (Success(v1), Success(v2), Success(v3), Success(v4)) => Success(f(v1, v2, v3, v4))
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors)
    }

  /**
    * Maps over t1, t2, t3, t4, and t5.
    */
  def mapN[T1, T2, T3, T4, T5, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                     t4: Validation[T4, E], t5: Validation[T5, E])
                                    (f: (T1, T2, T3, T4, T5) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5) match {
      case (Success(v1), Success(v2), Success(v3), Success(v4), Success(v5)) => Success(f(v1, v2, v3, v4, v5))
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors)
    }

  /**
    * Maps over t1, t2, t3, t4, t5, and t6.
    */
  def mapN[T1, T2, T3, T4, T5, T6, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                         t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E])
                                        (f: (T1, T2, T3, T4, T5, T6) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6) match {
      case (Success(v1), Success(v2), Success(v3), Success(v4), Success(v5), Success(v6)) => Success(f(v1, v2, v3, v4, v5, v6))
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors)

    }

  /**
    * FlatMaps over t1 and t2.
    */
  def flatMapN[T1, T2, U, E](t1: Validation[T1, E], t2: Validation[T2, E])
                            (f: (T1, T2) => Validation[U, E]): Validation[U, E] =
    (t1, t2) match {
      case (Success(v1), Success(v2)) => f(v1, v2)
      case _ => Failure(t1.errors #::: t2.errors)
    }

  /**
    * FlatMaps over t1, t2, and t3.
    */
  def flatMapN[T1, T2, T3, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E])
                                (f: (T1, T2, T3) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3) match {
      case (Success(v1), Success(v2), Success(v3)) => f(v1, v2, v3)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors)
    }

  /**
    * Adds an implicit `toSuccess` method.
    */
  implicit class ToSuccess[+T](val t: T) {
    def toSuccess[U >: T, E]: Validation[U, E] = Success(t)
  }

  /**
    * Adds an implicit `toFailure` method.
    */
  implicit class ToFailure[+E](val e: E) {
    def toFailure[V, F >: E]: Validation[V, F] = Failure(e #:: Stream.empty)
  }

  // TODO: Everything below this line is deprecated.


  /**
    * Folds the given function `f` over all elements `xs`.
    *
    * Returns a sequence of successful elements wrapped in [[Success]].
    */
  def fold[In, Out, Error](xs: Seq[In], zero: Out)(f: (Out, In) => Validation[Out, Error]): Validation[Out, Error] = {
    xs.foldLeft(Success(zero): Validation[Out, Error]) {
      case (acc, a) => acc flatMap {
        case value => f(value, a)
      }
    }
  }

  /**
    * Flattens a sequence of validations into one validation. Errors are concatenated.
    *
    * Returns [[Success]] if every element in `xs` is a [[Success]].
    */
  // TODO: Deprecated, replace by mapN
  def @@[Value, Error](xs: Traversable[Validation[Value, Error]]): Validation[List[Value], Error] = seqM(xs)

}

