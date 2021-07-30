/*
 *  Copyright 2020 Matthew Lutze
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}


/**
  * Companion object for [[KindInferMonad]].
  */
object KindInferMonad {

  /**
    * Lifts the given value `a` into the type inference monad.
    */
  def point[a](x: a): KindInferMonad[a] = KindInferMonad(s => Ok((s, x)))

  /**
    * Collects the result of each type inference monad in `ts` going left to right.
    */
  def seqM[A](xs: List[KindInferMonad[A]]): KindInferMonad[List[A]] = xs match {
    case Nil => point(Nil)
    case y :: ys => y flatMap {
      case r => seqM(ys) map {
        case rs => r :: rs
      }
    }
  }

}

/**
  * A kind inference state monad that maintains the current substitution.
  */
case class KindInferMonad[A](run: KindSubstitution => Result[(KindSubstitution, A), KindError]) {
  /**
    * Applies the given function `f` to the value in the monad.
    */
  def map[B](f: A => B): KindInferMonad[B] = {
    def runNext(s0: KindSubstitution): Result[(KindSubstitution, B), KindError] = {
      // Run the original function and map over its result (since it may have error'd).
      run(s0) map {
        case (s, a) => (s, f(a))
      }
    }

    KindInferMonad(runNext)
  }

  /**
    * Applies the given function `f` to the value in the monad.
    */
  def flatMap[B](f: A => KindInferMonad[B]): KindInferMonad[B] = {
    def runNext(s0: KindSubstitution): Result[(KindSubstitution, B), KindError] = {
      // Run the original function and flatMap over its result (since it may have error'd).
      run(s0) flatMap {
        case (s, a) => f(a) match {
          // Unwrap the returned monad and apply the inner function g.
          case KindInferMonad(g) => g(s)
        }
      }
    }

    KindInferMonad(runNext)
  }

  // TODO: Necessary for pattern matching?
  // TODO: What should this return?
  def withFilter(f: A => Boolean): KindInferMonad[A] = KindInferMonad(x => run(x) match {
    case Ok((subst, t)) => if (f(t)) Ok((subst, t)) else Ok((subst, t))
    case Err(e) => Err(e)
  })

}
