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
    * Returns a [[Success]] containing the result of applying `f` to the value in this validation (if it exists).
    *
    * Preserves the errors.
    */
  final def map[Output](f: Value => Output): Validation[Output, Error] = this match {
    case Success(value, errors) => Success(f(value), errors)
    case Failure(errors) => Failure(errors)
  }

  /**
    * Similar to `map` but does not wrap the result in a [[Success]].
    *
    * Preserves the errors.
    */
  final def flatMap[Output, A >: Error](f: Value => Validation[Output, A]): Validation[Output, A] = this match {
    case Success(input, errors) => f(input) match {
      case Success(value, thatErrors) => Success(value, errors #::: thatErrors)
      case Failure(thatErrors) => Failure(errors #::: thatErrors)
    }
    case Failure(errors) => Failure(errors)
  }

  /**
    * Returns the errors in this [[Success]] or [[Failure]] object.
    */
  def errors: Stream[Error]

  /**
    * Returns `true` iff this is a [[Success]] object.
    */
  def isSuccess: Boolean = this match {
    case v: Success[Value, Error] => true
    case _ => false
  }

  /**
    * Returns `true` iff this is a [[Failure]] object.
    */
  def isFailure: Boolean = !isSuccess

  /**
    * Returns the value inside `this` [[Success]] object.
    *
    * Throws an exception if `this` is a [[Failure]] object.
    */
  def get: Value = this match {
    case Success(value, errors) => value
    case Failure(errors) => throw new RuntimeException(s"Attempt to retrieve value from Failure. The errors are: ${errors.mkString(", ")}")
  }


}

object Validation {

  /**
    * Folds the given function `f` over all elements `xs`.
    *
    * Returns a sequence of successful elements wrapped in [[Success]].
    */
  def fold[In, Out, Error](xs: Seq[In], zero: Out)(f: (Out, In) => Validation[Out, Error]): Validation[Out, Error] = {
    xs.foldLeft(Success(zero, Stream.empty[Error]): Validation[Out, Error]) {
      case (acc, a) => acc flatMap {
        case value => f(value, a)
      }
    }
  }

  /**
    * TODO: DOC
    */
  // TODO: need foldMap, foldMapKeys, foldMapValues
  def fold[K, V, K2, V2, Error](m: Map[K, V])(f: (K, V) => Validation[(K2, V2), Error]): Validation[Map[K2, V2], Error] =
  m.foldLeft(Success(Map.empty[K2, V2], Stream.empty[Error]): Validation[Map[K2, V2], Error]) {
    case (macc, (k, v)) => macc flatMap {
      case ma => f(k, v) map {
        case ko => ma + ko
      }
    }
  }

  /**
    * TODO: DOC
    */
  def @@[Value, Error](o: Option[Validation[Value, Error]]): Validation[Option[Value], Error] = o match {
    case None => Success(None, Stream.empty)
    case Some(Success(v, errors)) => Success(Some(v), errors)
    case Some(Failure(errors)) => Failure(errors)
  }

  /**
    * Flattens a sequence of validations into one validation. Errors are concatenated.
    *
    * Returns [[Success]] if every element in `xs` is a [[Success]].
    */
  def @@[Value, Error](xs: Traversable[Validation[Value, Error]]): Validation[List[Value], Error] = {
    val zero = Success(List.empty[Value], Stream.empty[Error]): Validation[List[Value], Error]
    xs.foldRight(zero) {
      case (Success(curValue, curErrors), Success(accValue, accErrors)) =>
        Success(curValue :: accValue, curErrors #::: accErrors)
      case (Success(_, curErrors), Failure(accErrors)) =>
        Failure(curErrors #::: accErrors)
      case (Failure(curErrors), Success(_, accErrors)) =>
        Failure(curErrors #::: accErrors)
      case (Failure(curErrors), Failure(accErrors)) =>
        Failure(curErrors #::: accErrors)
    }
  }

  /**
    * TODO: DOC
    */
  def seqM[Value, Error](xs: Traversable[Validation[Value, Error]]): Validation[List[Value], Error] = @@(xs)

  /**
    * Returns a sequence of values wrapped in a [[Success]] for every [[Success]] in `xs`. Errors are concatenated.
    */
  def collect[Value, Error](xs: Seq[Validation[Value, Error]]): Validation[Seq[Value], Error] = {
    val zero = Success(List.empty[Value], Stream.empty[Error]): Validation[List[Value], Error]
    xs.foldRight(zero) {
      case (Success(value, errors), Success(accValue, accErrors)) =>
        Success(value :: accValue, errors #::: accErrors)
      case (Success(value, errors), Failure(accErrors)) =>
        Success(value :: Nil, errors #::: accErrors)
      case (Failure(errors), Success(accValue, accErrors)) =>
        Success(accValue, errors #::: accErrors)
      case (Failure(errors), Failure(accErrors)) =>
        Success(List.empty, errors #::: accErrors)
    }
  }


  /**
    * Merges 2 validations.
    */
  def @@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[(A, B), X] =
    (a, b) match {
      case (Success(valueA, altA), Success(valueB, altB)) =>
        Success((valueA, valueB), altB #::: altA)
      case _ => Failure(b.errors #::: a.errors)
    }

  /**
    * Merges 3 validations.
    */
  def @@[A, B, C, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X]): Validation[(A, B, C), X] =
    (@@(a, b), c) match {
      case (Success((valueA, valueB), altAB), Success(valueC, altC)) =>
        Success((valueA, valueB, valueC), altC #::: altAB)
      case (that, _) => Failure(c.errors #::: that.errors)
    }

  /**
    * Merges 4 validations.
    */
  def @@[A, B, C, D, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                        d: Validation[D, X]): Validation[(A, B, C, D), X] =
    (@@(a, b, c), d) match {
      case (Success((valueA, valueB, valueC), altABC), Success(valueD, altD)) =>
        Success((valueA, valueB, valueC, valueD), altD #::: altABC)
      case (that, _) => Failure(d.errors #::: that.errors)
    }

  /**
    * Merges 5 validations.
    */
  def @@[A, B, C, D, E, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                           d: Validation[D, X], e: Validation[E, X]): Validation[(A, B, C, D, E), X] =
    (@@(a, b, c, d), e) match {
      case (Success((valueA, valueB, valueC, valueD), altABCD), Success(valueE, altE)) =>
        Success((valueA, valueB, valueC, valueD, valueE), altE #::: altABCD)
      case (that, _) => Failure(e.errors #::: that.errors)
    }

  /**
    * Merges 6 validations.
    */
  def @@[A, B, C, D, E, F, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                              d: Validation[D, X], e: Validation[E, X], f: Validation[F, X]): Validation[(A, B, C, D, E, F), X] =
    (@@(a, b, c, d, e), f) match {
      case (Success((valueA, valueB, valueC, valueD, valueE), altABCDE), Success(valueF, altF)) =>
        Success((valueA, valueB, valueC, valueD, valueE, valueF), altF #::: altABCDE)
      case (that, _) => Failure(e.errors #::: that.errors)
    }

  /**
    * Add implicit `toSuccess` method.
    */
  implicit class ToSuccess[+Value](val value: Value) {
    def toSuccess[V >: Value, Error]: Validation[V, Error] = Success(value, Stream.empty)
  }

  /**
    * Add implicit `toFailure` method.
    */
  implicit class ToFailure[+Error](val failure: Error) {
    def toFailure[Value, E >: Error]: Validation[Value, E] = Failure(failure #:: Stream.empty)
  }

  /**
    * Represents a success `value` and `errors`.
    */
  case class Success[Value, Error](value: Value, errors: Stream[Error]) extends Validation[Value, Error]

  /**
    * Represents a failure with no value and `errors`.
    */
  case class Failure[Value, Error](errors: Stream[Error]) extends Validation[Value, Error]

}

