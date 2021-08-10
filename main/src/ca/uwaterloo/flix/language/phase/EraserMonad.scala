/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.phase.Eraser.{FTypes, emptyFTypes}
import ca.uwaterloo.flix.language.phase.sjvm.{ClosureInfo, NamespaceInfo}

import scala.collection.mutable

// TODO(JLS): namespaces not really needed
sealed case class EraserMonad[+T](value: T, fTypes: FTypes, namespaces: Set[NamespaceInfo], closures: Set[ClosureInfo]) {

  final def map[U](f: T => U): EraserMonad[U] =
    EraserMonad(f(value), fTypes, namespaces, closures)

  final def flatMap[U](f: T => EraserMonad[U]): EraserMonad[U] = {
    val monad0 = f(value)
    EraserMonad(monad0.value, fTypes union monad0.fTypes, namespaces union monad0.namespaces, closures)
  }

  final def copyWith(fTypes: FTypes = fTypes, namespaces: Set[NamespaceInfo] = namespaces, closures: Set[ClosureInfo] = closures): EraserMonad[T] =
    EraserMonad(value, fTypes, namespaces, closures)

}

object EraserMonad {

  final def toMonad[T](t: T): EraserMonad[T] =
    EraserMonad(t, emptyFTypes, Set(), Set())

  implicit class ToMonad[+T](val t: T) {
    def toMonad[U >: T]: EraserMonad[U] = EraserMonad.toMonad(t)
  }

  /**
   * Traverses `xs` applying the function `f` to each element.
   */
  def traverse[T, S](xs: Iterable[T])(f: T => EraserMonad[S]): EraserMonad[List[S]] = {
    // Check if the sequence is empty.
    if (xs.isEmpty) return Nil.toMonad

    // Two mutable arrays to hold the intermediate results.
    val values = mutable.ArrayBuffer.empty[S]
    var fTypes: FTypes = emptyFTypes
    var nss: Set[NamespaceInfo] = Set.empty
    var clss: Set[ClosureInfo] = Set.empty

    // Apply f to each element and collect the results.
    for (x <- xs) {
      val EraserMonad(v, ft, ns, cls) = f(x)
      values += v
      fTypes = fTypes union ft
      nss = nss union ns
      clss = clss union cls
    }

    EraserMonad(values.toList, fTypes, nss, clss)
  }

  /**
   * Folds Right over `xs` using the function `f` with the initial value `zero`.
   */
  def foldRight[T, U](xs: Iterable[T])(zero: EraserMonad[U])(f: (T, U) => EraserMonad[U]): EraserMonad[U] = {
    xs.foldRight(zero) {
      case (a, acc) => acc flatMap (v => f(a, v))
    }
  }

  def combine[T](em1: EraserMonad[T], em2: EraserMonad[_]): EraserMonad[T] =
    em1.flatMap(t => em2.map(_ => t))

  def combineN[T](value: T)(ms: EraserMonad[_]*): EraserMonad[T] = {
    ms.foldLeft[EraserMonad[T]](value.toMonad)((acc, em) => combine(acc, em))
  }

  /**
   * Maps over t1.
   */
  def mapN[T1, U](t1: EraserMonad[T1])
                 (f: T1 => U): EraserMonad[U] =
    t1 map f

  /**
   * Maps over t1 and t2.
   */
  def mapN[T1, T2, U](t1: EraserMonad[T1], t2: EraserMonad[T2])
                     (f: (T1, T2) => U): EraserMonad[U] = {
    combineN(f(t1.value, t2.value))(t1, t2)
  }

  /**
   * Maps over t1, t2, and t3.
   */
  def mapN[T1, T2, T3, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3])
                         (f: (T1, T2, T3) => U): EraserMonad[U] =
    combineN(f(t1.value, t2.value, t3.value))(t1, t2, t3)

  /**
   * Maps over t1, t2, t3, and t4.
   */
  def mapN[T1, T2, T3, T4, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                              t4: EraserMonad[T4])
                             (f: (T1, T2, T3, T4) => U): EraserMonad[U] =
    combineN(f(t1.value, t2.value, t3.value, t4.value))(t1, t2, t3, t4)

  /**
   * Maps over t1, t2, t3, t4, and t5.
   */
  def mapN[T1, T2, T3, T4, T5, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                  t4: EraserMonad[T4], t5: EraserMonad[T5])
                                 (f: (T1, T2, T3, T4, T5) => U): EraserMonad[U] =
    combineN(f(t1.value, t2.value, t3.value, t4.value, t5.value))(t1, t2, t3, t4, t5)

  /**
   * Maps over t1, t2, t3, t4, t5, and t6.
   */
  def mapN[T1, T2, T3, T4, T5, T6, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                      t4: EraserMonad[T4], t5: EraserMonad[T5], t6: EraserMonad[T6])
                                     (f: (T1, T2, T3, T4, T5, T6) => U): EraserMonad[U] =
    combineN(f(t1.value, t2.value, t3.value, t4.value, t5.value, t6.value))(t1, t2, t3, t4, t5, t6)

  /**
   * Maps over t1, t2, t3, t4, t5, t6, and t7.
   */
  def mapN[T1, T2, T3, T4, T5, T6, T7, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                          t4: EraserMonad[T4], t5: EraserMonad[T5], t6: EraserMonad[T6],
                                          t7: EraserMonad[T7])
                                         (f: (T1, T2, T3, T4, T5, T6, T7) => U): EraserMonad[U] =
    combineN(f(t1.value, t2.value, t3.value, t4.value, t5.value, t6.value, t7.value))(t1, t2, t3, t4, t5, t6, t7)

  /**
   * FlatMaps over t1.
   */
  def flatMapN[T1, U](t1: EraserMonad[T1])
                     (f: T1 => EraserMonad[U]): EraserMonad[U] =
    t1 flatMap f

  /**
   * FlatMaps over t1 and t2.
   */
  def flatMapN[T1, T2, U](t1: EraserMonad[T1], t2: EraserMonad[T2])
                         (f: (T1, T2) => EraserMonad[U]): EraserMonad[U] = {
    f(t1.value, t2.value).flatMap(combineN(_)(t1, t2))
  }

  /**
   * FlatMaps over t1, t2, and t3.
   */
  def flatMapN[T1, T2, T3, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3])
                             (f: (T1, T2, T3) => EraserMonad[U]): EraserMonad[U] =
    f(t1.value, t2.value, t3.value).flatMap(combineN(_)(t1, t2, t3))

  /**
   * FlatMaps over t1, t2, t3, and t4.
   */
  def flatMapN[T1, T2, T3, T4, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                  t4: EraserMonad[T4])
                                 (f: (T1, T2, T3, T4) => EraserMonad[U]): EraserMonad[U] =
    f(t1.value, t2.value, t3.value, t4.value).flatMap(combineN(_)(t1, t2, t3, t4))

  /**
   * FlatMaps over t1, t2, t3, t4, and t5.
   */
  def flatMapN[T1, T2, T3, T4, T5, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                      t4: EraserMonad[T4], t5: EraserMonad[T5])
                                     (f: (T1, T2, T3, T4, T5) => EraserMonad[U]): EraserMonad[U] =
    f(t1.value, t2.value, t3.value, t4.value, t5.value).flatMap(combineN(_)(t1, t2, t3, t4, t5))

  /**
   * FlatMaps over t1, t2, t3, t4, t5, and t6.
   */
  def flatMapN[T1, T2, T3, T4, T5, T6, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                          t4: EraserMonad[T4], t5: EraserMonad[T5], t6: EraserMonad[T6])
                                         (f: (T1, T2, T3, T4, T5, T6) => EraserMonad[U]): EraserMonad[U] =
    f(t1.value, t2.value, t3.value, t4.value, t5.value, t6.value).flatMap(combineN(_)(t1, t2, t3, t4, t5, t6))

  /**
   * FlatMaps over t1, t2, t3, t4, t5, t6, and t7.
   */
  def flatMapN[T1, T2, T3, T4, T5, T6, T7, U](t1: EraserMonad[T1], t2: EraserMonad[T2], t3: EraserMonad[T3],
                                              t4: EraserMonad[T4], t5: EraserMonad[T5], t6: EraserMonad[T6],
                                              t7: EraserMonad[T7])
                                             (f: (T1, T2, T3, T4, T5, T6, T7) => EraserMonad[U]): EraserMonad[U] =
    f(t1.value, t2.value, t3.value, t4.value, t5.value, t6.value, t7.value).flatMap(combineN(_)(t1, t2, t3, t4, t5, t6, t7))

}
