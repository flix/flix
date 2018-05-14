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
  case class Success[Value, Error](value: Value) extends Validation[Value, Error] {
    def errors: Stream[Error] = Stream.empty
  }

  /**
    * Represents a failure with no value and `errors`.
    */
  case class Failure[Value, Error](errors: Stream[Error]) extends Validation[Value, Error]




  // TODO: DOC
  def seqM[Value, Error](xs: Traversable[Validation[Value, Error]]): Validation[List[Value], Error] = @@(xs)





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
    * TODO: DOC
    */
  def @@[Value, Error](o: Option[Validation[Value, Error]]): Validation[Option[Value], Error] = o match {
    case None => Success(None)
    case Some(Success(v)) => Success(Some(v))
    case Some(Failure(errors)) => Failure(errors)
  }

  /**
    * Flattens a sequence of validations into one validation. Errors are concatenated.
    *
    * Returns [[Success]] if every element in `xs` is a [[Success]].
    */
  def @@[Value, Error](xs: Traversable[Validation[Value, Error]]): Validation[List[Value], Error] = {
    val zero = Success(List.empty[Value]): Validation[List[Value], Error]
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
    * Merges 2 validations.
    */
  def @@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[(A, B), X] =
    (a, b) match {
      case (Success(valueA), Success(valueB)) =>
        Success((valueA, valueB))
      case _ => Failure(b.errors #::: a.errors)
    }

  /**
    * Merges 3 validations.
    */
  def @@[A, B, C, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X]): Validation[(A, B, C), X] =
    (@@(a, b), c) match {
      case (Success((valueA, valueB)), Success(valueC)) =>
        Success((valueA, valueB, valueC))
      case (that, _) => Failure(c.errors #::: that.errors)
    }

  /**
    * Merges 4 validations.
    */
  def @@[A, B, C, D, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                        d: Validation[D, X]): Validation[(A, B, C, D), X] =
    (@@(a, b, c), d) match {
      case (Success((valueA, valueB, valueC)), Success(valueD)) =>
        Success((valueA, valueB, valueC, valueD))
      case (that, _) => Failure(d.errors #::: that.errors)
    }

  /**
    * Merges 5 validations.
    */
  def @@[A, B, C, D, E, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                           d: Validation[D, X], e: Validation[E, X]): Validation[(A, B, C, D, E), X] =
    (@@(a, b, c, d), e) match {
      case (Success((valueA, valueB, valueC, valueD)), Success(valueE)) =>
        Success((valueA, valueB, valueC, valueD, valueE))
      case (that, _) => Failure(e.errors #::: that.errors)
    }

  /**
    * Merges 6 validations.
    */
  def @@[A, B, C, D, E, F, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                              d: Validation[D, X], e: Validation[E, X], f: Validation[F, X]): Validation[(A, B, C, D, E, F), X] =
    (@@(a, b, c, d, e), f) match {
      case (Success((valueA, valueB, valueC, valueD, valueE)), Success(valueF)) =>
        Success((valueA, valueB, valueC, valueD, valueE, valueF))
      case (that, _) => Failure(e.errors #::: that.errors)
    }

  /**
    * Adds an implicit `toSuccess` method.
    */
  implicit class ToSuccess[+Value](val value: Value) {
    def toSuccess[V >: Value, Error]: Validation[V, Error] = Success(value)
  }

  /**
    * Adds an implicit `toFailure` method.
    */
  implicit class ToFailure[+Error](val failure: Error) {
    def toFailure[Value, E >: Error]: Validation[Value, E] = Failure(failure #:: Stream.empty)
  }

}

