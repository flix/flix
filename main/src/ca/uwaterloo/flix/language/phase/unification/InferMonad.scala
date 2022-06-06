/*
 *  Copyright 2020 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.{Rigidity, RigidityEnv, Symbol}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

/**
  * Companion object for [[InferMonad]].
  */
object InferMonad {

  /**
    * Lifts the given value `a` into the type inference monad.
    */
  def point[a](x: a): InferMonad[a] = InferMonad { case (s, renv) => Ok((s, renv, x)) }

  /**
    * Collects the result of each type inference monad in `ts` going left to right.
    */
  def seqM[A](xs: List[InferMonad[A]]): InferMonad[List[A]] = xs match {
    case Nil => point(Nil)
    case y :: ys => y flatMap {
      case r => seqM(ys) map {
        case rs => r :: rs
      }
    }
  }

}

/**
  * A type inference state monad that maintains the current substitution and rigidity environment.
  */
case class InferMonad[A](run: (Substitution, RigidityEnv) => Result[(Substitution, RigidityEnv, A), TypeError]) {
  /**
    * Applies the given function `f` to the value in the monad.
    */
  def map[B](f: A => B): InferMonad[B] = {
    def runNext(s0: Substitution, renv0: RigidityEnv): Result[(Substitution, RigidityEnv, B), TypeError] = {
      // Run the original function and map over its result (since it may have error'd).
      run(s0, renv0) map {
        case (s, renv, a) => (s, renv, f(a))
      }
    }

    InferMonad(runNext)
  }

  /**
    * Applies the given function `f` to the value in the monad.
    */
  def flatMap[B](f: A => InferMonad[B]): InferMonad[B] = {
    def runNext(s0: Substitution, renv0: RigidityEnv): Result[(Substitution, RigidityEnv, B), TypeError] = {
      // Run the original function and flatMap over its result (since it may have error'd).
      run(s0, renv0) flatMap {
        case (s, renv, a) => f(a) match {
          // Unwrap the returned monad and apply the inner function g.
          case InferMonad(g) => g(s, renv)
        }
      }
    }

    InferMonad(runNext)
  }

  /**
    * Applies the given function `f` to transform an error in the monad.
    */
  def transformError[B](f: TypeError => TypeError): InferMonad[A] = {
    def runNext(s0: Substitution, renv0: RigidityEnv): Result[(Substitution, RigidityEnv, A), TypeError] = {
      run(s0, renv0) match {
        case Ok(t) => Ok(t)
        case Err(e) => Err(f(e))
      }
    }

    InferMonad(runNext)

  }

  // TODO: Necessary for pattern matching?
  // TODO: What should this return?
  def withFilter(f: A => Boolean): InferMonad[A] = InferMonad {
    case (x, renv0) => run(x, renv0) match {
      case Ok((subst, renv, t)) => if (f(t)) Ok((subst, renv, t)) else Ok((subst, renv, t))
      case Err(e) => Err(e)
    }
  }

  // MATT for debugging
  lazy val result = run(Substitution.empty, Map.empty).get

  lazy val (subst, renv, value) = result

  /**
    * toString for debugging.
    * Eagerly evaluates the substitution.
    *
    * Must not be called from non-debugging code.
    */
  override def toString: String = {
    result.toString
  }
}
