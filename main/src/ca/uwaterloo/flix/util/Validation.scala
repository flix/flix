/*
 * Copyright 2018 Magnus Madsen
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

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

sealed trait Validation[+T, +E] {

  /**
    * Returns the value inside `this` [[Validation.Success]] object.
    *
    * Throws an exception if `this` is a [[Validation.Failure]] object.
    */
  final def get: T = this match {
    case Validation.Success(value) => value
    case Validation.Failure(errors) => throw new RuntimeException(s"Attempt to retrieve value from Failure. The errors are: ${errors.mkString(", ")}")
  }

  /**
    * Returns a [[Validation.Success]] containing the result of applying `f` to the value in this validation (if it exists).
    *
    * Preserves the errors.
    */
  final def map[U](f: T => U): Validation[U, E] = this match {
    case Validation.Success(value) => Validation.Success(f(value))
    case Validation.Failure(errors) => Validation.Failure(errors)
  }

  /**
    * Similar to `map` but does not wrap the result in a [[Validation.Success]].
    *
    * Preserves the errors.
    */
  final def flatMap[U, A >: E](f: T => Validation[U, A]): Validation[U, A] = this match {
    case Validation.Success(input) => f(input) match {
      case Validation.Success(value) => Validation.Success(value)
      case Validation.Failure(thatErrors) => Validation.Failure(errors #::: thatErrors)
    }
    case Validation.Failure(errors) => Validation.Failure(errors)
  }

  /**
    * Returns the errors in this [[Validation.Success]] or [[Validation.Failure]] object.
    */
  protected def errors: LazyList[E]

}

object Validation {

  /**
    * Represents a successful validation with the empty list.
    */
  final val SuccessNil = Success(Nil)

  /**
    * Represents a success `value`.
    */
  case class Success[T, E](t: T) extends Validation[T, E] {
    def errors: LazyList[E] = LazyList.empty
  }

  /**
    * Represents a failure with no value and `errors`.
    */
  case class Failure[T, E](errors: LazyList[E]) extends Validation[T, E]

  /**
    * Sequences the given list of validations `xs`.
    */
  def sequence[T, E](xs: Iterable[Validation[T, E]]): Validation[List[T], E] = {
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
    * Traverses `xs` applying the function `f` to each element.
    */
  def traverse[T, S, E](xs: Iterable[T])(f: T => Validation[S, E]): Validation[List[S], E] = fastTraverse(xs)(f)

  /**
    * A fast implementation of traverse.
    */
  private def fastTraverse[T, S, E](xs: Iterable[T])(f: T => Validation[S, E]): Validation[List[S], E] = {
    // Check if the sequence is empty.
    if (xs.isEmpty)
      return Validation.SuccessNil

    // Two mutable arrays to hold the intermediate results.
    val successValues = mutable.ArrayBuffer.empty[S]
    val failureStream = mutable.ArrayBuffer.empty[LazyList[E]]

    // Apply f to each element and collect the results.
    for (x <- xs) {
      f(x) match {
        case Success(v) => successValues += v
        case Failure(e) => failureStream += e
      }
    }

    // Check whether we were successful or not.
    if (failureStream.isEmpty) {
      Success(successValues.toList)
    } else {
      Failure(failureStream.foldLeft(LazyList.empty[E])(_ #::: _))
    }
  }

  /**
    * Maps over t1.
    */
  def mapN[T1, U, E](t1: Validation[T1, E])
                    (f: T1 => U): Validation[U, E] =
    t1 match {
      case Success(v1) => Success(f(v1))
      case _ => Failure(t1.errors)
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
    * Maps over t1, t2, t3, t4, t5, t6, and t7.
    */
  def mapN[T1, T2, T3, T4, T5, T6, T7, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                             t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E],
                                             t7: Validation[T7, E])
                                            (f: (T1, T2, T3, T4, T5, T6, T7) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6, t7) match {
      case (Success(v1), Success(v2), Success(v3), Success(v4), Success(v5), Success(v6), Success(v7)) => Success(f(v1, v2, v3, v4, v5, v6, v7))
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors #::: t7.errors)
    }

  /**
    * FlatMaps over t1.
    */
  def flatMapN[T1, U, E](t1: Validation[T1, E])(f: T1 => Validation[U, E]): Validation[U, E] =
    t1 match {
      case Success(v1) => f(v1)
      case _ => Failure(t1.errors)
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
    * FlatMaps over t1, t2, t3, and t4.
    */
  def flatMapN[T1, T2, T3, T4, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                     t4: Validation[T4, E])
                                     (f: (T1, T2, T3, T4) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3, t4) match {
      case (Success(v1), Success(v2), Success(v3), Success(v4)) => f(v1, v2, v3, v4)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors)
    }

  /**
    * FlatMaps over t1, t2, t3, t4, and t5.
    */
  def flatMapN[T1, T2, T3, T4, T5, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                         t4: Validation[T4, E], t5: Validation[T5, E])
                                         (f: (T1, T2, T3, T4, T5) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3, t4, t5) match {
      case (Success(v1), Success(v2), Success(v3), Success(v4), Success(v5)) => f(v1, v2, v3, v4, v5)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors)
    }

  /**
    * Folds Right over `xs` using the function `f` with the initial value `zero`.
    */
  def foldRight[T, U, E](xs: Seq[T])(zero: Validation[U, E])(f: (T, U) => Validation[U, E]): Validation[U, E] = {
    xs.foldRight(zero) {
      case (a, acc) => acc flatMap {
        case v => f(a, v)
      }
    }
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
    def toFailure[V, F >: E]: Validation[V, F] = Failure(e #:: LazyList.empty)
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

}

