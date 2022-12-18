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

sealed trait Validation[+T, +E] {

  /**
    * Returns the value inside `this` [[Validation.Success]] object.
    *
    * Throws an exception if `this` is a [[Validation.Failure]] object.
    */
  final def get: T = this match {
    case Validation.Success(value, _) => value
    case Validation.Failure(errors) => throw new RuntimeException(s"Attempt to retrieve value from Failure. The errors are: ${errors.mkString(", ")}")
  }

  /**
    * Returns a [[Validation.Success]] containing the result of applying `f` to the value in this validation (if it exists).
    *
    * Preserves the errors.
    */
  final def map[U](f: T => U): Validation[U, E] = this match {
    case Validation.Success(value, errors) => Validation.Success(f(value), errors)
    case Validation.Failure(errors) => Validation.Failure(errors)
  }

  /**
    * Similar to `map` but does not wrap the result in a [[Validation.Success]].
    *
    * Preserves the errors.
    */
  // Deprecated. Use flatMapN instead.
  final def flatMap[U, A >: E](f: T => Validation[U, A]): Validation[U, A] = this match {
    case Validation.Success(input, errors) => f(input) match {
      case Validation.Success(value, thatErrors) => Validation.Success(value, errors #::: thatErrors)
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
  final val SuccessNil = Success(Nil, LazyList.empty)

  /**
    * Represents a successful validation with the empty option.
    */
  final val SuccessNone = Success(None, LazyList.empty)

  /**
    * Represents a success `value`.
    */
  case class Success[T, E](t: T, errors: LazyList[E]) extends Validation[T, E]

  /**
    * Represents a failure with no value and `errors`.
    */
  case class Failure[T, E](errors: LazyList[E]) extends Validation[T, E]

  /**
    * Sequences the given list of validations `xs`.
    */
  def sequence[T, E](xs: Iterable[Validation[T, E]]): Validation[List[T], E] = {
    val zero = Validation.SuccessNil: Validation[List[T], E]
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
    * Sequences the given list of validations `xs`, ignoring non-error results.
    */
  def sequenceX[T, E](xs: Iterable[Validation[T, E]]): Validation[Unit, E] = {
    sequence(xs).map(_ => ())
  }

  /**
    * Traverses `xs` applying the function `f` to each element.
    */
  def traverse[T, S, E](xs: Iterable[T])(f: T => Validation[S, E]): Validation[List[S], E] = fastTraverse(xs)(f)

  /**
    * Traverses the given map, applying the function `f` to each value.
    */
  def traverseValues[K, V1, V2, E](xs: Map[K, V1])(f: V1 => Validation[V2, E]): Validation[Map[K, V2], E] = {
    traverse(xs) {
      case (k, v0) => mapN(f(v0))(v => k -> v)
    }.map(_.toMap)
  }

  /**
    * Traverses `o` applying the function `f` to the value, if it exists.
    */
  def traverseOpt[T, S, E](o: Option[T])(f: T => Validation[S, E]): Validation[Option[S], E] = o match {
    case None => Validation.SuccessNone
    case Some(x) => f(x) match {
      case Success(t, errs) => Success(Some(t), errs)
      case Failure(errs) => Failure(errs)
    }
  }

  /**
    * Traverses `xs` applying the function `f` to each element, ignoring non-error results.
    */
  def traverseX[T, E](xs: Iterable[T])(f: T => Validation[_, E]): Validation[Unit, E] = {
    traverse(xs)(f).map(_ => ())
  }

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
        case Success(v, e) =>
          successValues += v
          failureStream += e
        case Failure(e) => failureStream += e
      }
    }

    // Check whether we were successful or not.
    if (failureStream.isEmpty) {
      Success(successValues.toList, LazyList.empty)
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
      case Success(v1, errors) => Success(f(v1), errors)
      case Failure(errors) => Failure(errors)
    }

  /**
    * Maps over t1 and t2.
    */
  def mapN[T1, T2, U, E](t1: Validation[T1, E], t2: Validation[T2, E])
                        (f: (T1, T2) => U): Validation[U, E] =
    (t1, t2) match {
      case (Success(v1, e1), Success(v2, e2)) => Success(f(v1, v2), e1 #::: e2)
      case _ => Failure(t1.errors #::: t2.errors)
    }

  /**
    * Maps over t1, t2, and t3.
    */
  def mapN[T1, T2, T3, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E])
                            (f: (T1, T2, T3) => U): Validation[U, E] =
    (t1, t2, t3) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3)) => Success(f(v1, v2, v3), e1 #::: e2 #::: e3)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors)
    }

  /**
    * Maps over t1, t2, t3, and t4.
    */
  def mapN[T1, T2, T3, T4, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                 t4: Validation[T4, E])
                                (f: (T1, T2, T3, T4) => U): Validation[U, E] =
    (t1, t2, t3, t4) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4)) => Success(f(v1, v2, v3, v4), e1 #::: e2 #::: e3 #::: e4)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors)
    }

  /**
    * Maps over t1, t2, t3, t4, and t5.
    */
  def mapN[T1, T2, T3, T4, T5, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                     t4: Validation[T4, E], t5: Validation[T5, E])
                                    (f: (T1, T2, T3, T4, T5) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5)) => Success(f(v1, v2, v3, v4, v5), e1 #::: e2 #::: e3 #::: e4 #::: e5)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors)
    }

  /**
    * Maps over t1, t2, t3, t4, t5, and t6.
    */
  def mapN[T1, T2, T3, T4, T5, T6, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                         t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E])
                                        (f: (T1, T2, T3, T4, T5, T6) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5), Success(v6, e6)) => Success(f(v1, v2, v3, v4, v5, v6), e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6)
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
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5), Success(v6, e6), Success(v7, e7)) => Success(f(v1, v2, v3, v4, v5, v6, v7), e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors #::: t7.errors)
    }

  /**
    * Maps over t1, t2, t3, t4, t5, t6, t7, and t8.
    */
  def mapN[T1, T2, T3, T4, T5, T6, T7, T8, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                                 t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E],
                                                 t7: Validation[T7, E], t8: Validation[T8, E])
                                                (f: (T1, T2, T3, T4, T5, T6, T7, T8) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6, t7, t8) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5), Success(v6, e6), Success(v7, e7), Success(v8, e8)) => Success(f(v1, v2, v3, v4, v5, v6, v7, v8), e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7 #::: e8)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors #::: t7.errors #::: t8.errors)
    }

  /**
    * Maps over t1, t2, t3, t4, t5, t6, t7, t8, and t9.
    */
  def mapN[T1, T2, T3, T4, T5, T6, T7, T8, T9, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                                     t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E],
                                                     t7: Validation[T7, E], t8: Validation[T8, E], t9: Validation[T9, E])
                                                    (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => U): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6, t7, t8, t9) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5), Success(v6, e6), Success(v7, e7), Success(v8, e8), Success(v9, e9)) => Success(f(v1, v2, v3, v4, v5, v6, v7, v8, v9), e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7 #::: e8 #::: e9)
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors #::: t7.errors #::: t8.errors #::: t9.errors)
    }

  /**
    * FlatMaps over t1.
    */
  def flatMapN[T1, U, E](t1: Validation[T1, E])(f: T1 => Validation[U, E]): Validation[U, E] =
    t1 match {
      case Success(v1, e1) => f(v1) match {
        case Success(v2, e2) => Success(v2, e1 #::: e2)
        case Failure(e2) => Failure(e1 #::: e2)
      }
      case _ => Failure(t1.errors)
    }

  /**
    * FlatMaps over t1 and t2.
    */
  def flatMapN[T1, T2, U, E](t1: Validation[T1, E], t2: Validation[T2, E])
                            (f: (T1, T2) => Validation[U, E]): Validation[U, E] =
    (t1, t2) match {
      case (Success(v1, e1), Success(v2, e2)) => f(v1, v2) match {
        case Success(v3, e3) => Success(v3, e1 #::: e2 #::: e3)
        case Failure(e3) => Failure(e1 #::: e2 #::: e3)
      }
      case _ => Failure(t1.errors #::: t2.errors)
    }

  /**
    * FlatMaps over t1, t2, and t3.
    */
  def flatMapN[T1, T2, T3, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E])
                                (f: (T1, T2, T3) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3)) => f(v1, v2, v3) match {
        case Success(v4, e4) => Success(v4, e1 #::: e2 #::: e3 #::: e4)
        case Failure(e4) => Failure(e1 #::: e2 #::: e3 #::: e4)
      }
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors)
    }

  /**
    * FlatMaps over t1, t2, t3, and t4.
    */
  def flatMapN[T1, T2, T3, T4, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                     t4: Validation[T4, E])
                                    (f: (T1, T2, T3, T4) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3, t4) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4)) => f(v1, v2, v3, v4) match {
        case Success(v5, e5) => Success(v5, e1 #::: e2 #::: e3 #::: e4 #::: e5)
        case Failure(e5) => Failure(e1 #::: e2 #::: e3 #::: e4 #::: e5)
      }
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors)
    }

  /**
    * FlatMaps over t1, t2, t3, t4, and t5.
    */
  def flatMapN[T1, T2, T3, T4, T5, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                         t4: Validation[T4, E], t5: Validation[T5, E])
                                        (f: (T1, T2, T3, T4, T5) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3, t4, t5) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5)) => f(v1, v2, v3, v4, v5) match {
        case Success(v6, e6) => Success(v6, e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6)
        case Failure(e6) => Failure(e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6)
      }
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors)
    }

  /**
    * FlatMaps over t1, t2, t3, t4, t5, and t6.
    */
  def flatMapN[T1, T2, T3, T4, T5, T6, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                             t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E])
                                            (f: (T1, T2, T3, T4, T5, T6) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5), Success(v6, e6)) => f(v1, v2, v3, v4, v5, v6) match {
        case Success(v7, e7) => Success(v7, e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7)
        case Failure(e7) => Failure(e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7)
      }

      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors)
    }

  /**
    * FlatMaps over t1, t2, t3, t4, t5, and t6.
    */
  def flatMapN[T1, T2, T3, T4, T5, T6, T7, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                                 t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E],
                                                 t7: Validation[T7, E])
                                                (f: (T1, T2, T3, T4, T5, T6, T7) => Validation[U, E]): Validation[U, E] =
    (t1, t2, t3, t4, t5, t6, t7) match {
      case (Success(v1, e1), Success(v2, e2), Success(v3, e3), Success(v4, e4), Success(v5, e5), Success(v6, e6), Success(v7, e7)) => f(v1, v2, v3, v4, v5, v6, v7) match {
        case Success(v8, e8) => Success(v8, e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7 #::: e8)
        case Failure(e8) => Failure(e1 #::: e2 #::: e3 #::: e4 #::: e5 #::: e6 #::: e7 #::: e8)
      }
      case _ => Failure(t1.errors #::: t2.errors #::: t3.errors #::: t4.errors #::: t5.errors #::: t6.errors #::: t7.errors)
    }

  /**
    * Sequences over t1, t2, and t3.
    */
  def sequenceT[T1, T2, T3, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E]): Validation[(T1, T2, T3), E] =
    mapN(t1, t2, t3)(Function.untupled(identity))

  /**
    * Sequences over t1, t2, t3, and t4.
    */
  def sequenceT[T1, T2, T3, T4, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                      t4: Validation[T4, E]): Validation[(T1, T2, T3, T4), E] =
    mapN(t1, t2, t3, t4)(Function.untupled(identity))

  /**
    * Sequences over t1, t2, t3, t4, and t5.
    */
  def sequenceT[T1, T2, T3, T4, T5, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                          t4: Validation[T4, E], t5: Validation[T5, E]): Validation[(T1, T2, T3, T4, T5), E] =
    mapN(t1, t2, t3, t4, t5)(Function.untupled(identity))

  /**
    * Sequences over t1, t2, t3, t4, t5, and t6.
    */
  def sequenceT[T1, T2, T3, T4, T5, T6, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                              t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E]): Validation[(T1, T2, T3, T4, T5, T6), E] =
    mapN(t1, t2, t3, t4, t5, t6) {
      case (u1, u2, u3, u4, u5, u6) => (u1, u2, u3, u4, u5, u6)
    }

  /**
    * Sequences over t1, t2, t3, t4, t5, and t6.
    */
  def sequenceT[T1, T2, T3, T4, T5, T6, T7, U, E](t1: Validation[T1, E], t2: Validation[T2, E], t3: Validation[T3, E],
                                                  t4: Validation[T4, E], t5: Validation[T5, E], t6: Validation[T6, E],
                                                  t7: Validation[T7, E]): Validation[(T1, T2, T3, T4, T5, T6, T7), E] =
    mapN(t1, t2, t3, t4, t5, t6, t7) {
      case (u1, u2, u3, u4, u5, u6, u7) => (u1, u2, u3, u4, u5, u6, u7)
    }

  /**
    * Folds Right over `xs` using the function `f` with the initial value `zero`.
    */
  def foldRight[T, U, E](xs: Seq[T])(zero: Validation[U, E])(f: (T, U) => Validation[U, E]): Validation[U, E] = {
    xs.foldRight(zero) {
      case (a, acc) => flatMapN(acc) {
        case v => f(a, v)
      }
    }
  }

  /**
    * Adds an implicit `toSuccess` method.
    */
  implicit class ToSuccess[+T](val t: T) {
    def toSuccess[U >: T, E]: Validation[U, E] = Success(t, LazyList.empty[E])
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
  def fold[In, Out, Error](xs: Iterable[In], zero: Out)(f: (Out, In) => Validation[Out, Error]): Validation[Out, Error] = {
    xs.foldLeft(Success(zero, LazyList.empty): Validation[Out, Error]) {
      case (acc, a) => flatMapN(acc) {
        case value => f(value, a)
      }
    }
  }

}

