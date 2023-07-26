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

import ca.uwaterloo.flix.language.ast.{Ast, LevelEnv, RigidityEnv, Symbol}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

/**
  * Companion object for [[InferMonad]].
  */
object InferMonad {

  /**
    * A constant for the InferMonad over the empty list.
    */
  val PointNil: InferMonad[Nil.type] = point(Nil)

  /**
    * Lifts the given value `x` into the type inference monad.
    */
  def point[A](x: A): InferMonad[A] = InferMonad { case (s, econstrs, renv, lenv) => Ok((s, econstrs, renv, lenv, x)) }

  /**
    * Lifts the given error `err` into the type inference monad.
    */
  def errPoint[A](err: TypeError): InferMonad[A] = InferMonad { case _ => Err(err) }

  /**
    * Collects the result of each type inference monad in `ts` going left to right.
    */
  def seqM[A](xs: List[InferMonad[A]]): InferMonad[List[A]] = xs match {
    case Nil => PointNil
    case y :: ys => y flatMap {
      case r => seqM(ys) map {
        case rs => r :: rs
      }
    }
  }

  /**
    * Applies the function `f` to each element in the list, combining the results.
    */
  def traverseM[A, B](xs: List[A])(f: A => InferMonad[B]): InferMonad[List[B]] = xs match {
    case Nil => PointNil
    case y :: ys => f(y) flatMap {
      case r => traverseM(ys)(f) map {
        case rs => r :: rs
      }
    }
  }

  def traverseOptM[A, B](xs: Option[A])(f: A => InferMonad[B]): InferMonad[Option[B]] = ???

}

/**
  * A type inference state monad that maintains the current substitution and rigidity environment.
  */
case class InferMonad[+A](run: (Substitution, List[Ast.BroadEqualityConstraint], RigidityEnv, LevelEnv) => Result[(Substitution, List[Ast.BroadEqualityConstraint], RigidityEnv, LevelEnv, A), TypeError]) {
  /**
    * Applies the given function `f` to the value in the monad.
    */
  def map[B](f: A => B): InferMonad[B] = {
    def runNext(s0: Substitution, econstrs0: List[Ast.BroadEqualityConstraint], renv0: RigidityEnv, lenv0: LevelEnv): Result[(Substitution, List[Ast.BroadEqualityConstraint], RigidityEnv, LevelEnv, B), TypeError] = {
      // Run the original function and map over its result (since it may have error'd).
      run(s0, econstrs0, renv0, lenv0) map {
        case (s, econstrs, renv, lenv, a) => (s, econstrs, renv, lenv, f(a))
      }
    }

    InferMonad(runNext)
  }

  /**
    * Applies the given function `f` to the value in the monad.
    */
  def flatMap[B](f: A => InferMonad[B]): InferMonad[B] = {
    def runNext(s0: Substitution, econstrs0: List[Ast.BroadEqualityConstraint], renv0: RigidityEnv, lenv0: LevelEnv): Result[(Substitution, List[Ast.BroadEqualityConstraint], RigidityEnv, LevelEnv, B), TypeError] = {
      // Run the original function and flatMap over its result (since it may have error'd).
      run(s0, econstrs0, renv0, lenv0) flatMap {
        case (s, econstrs, renv, lenv, a) => f(a) match { // TODO ASSOC-TYPES throwing econstrs away???
          // Unwrap the returned monad and apply the inner function g.
          case InferMonad(g) => g(s, econstrs, renv, lenv)
        }
      }
    }

    InferMonad(runNext)
  }

  /**
    * Applies the given function `f` to transform an error in the monad.
    */
  def transformError[B](f: TypeError => TypeError): InferMonad[A] = {
    def runNext(s0: Substitution, econstrs0: List[Ast.BroadEqualityConstraint], renv0: RigidityEnv, lenv0: LevelEnv): Result[(Substitution, List[Ast.BroadEqualityConstraint], RigidityEnv, LevelEnv, A), TypeError] = {
      run(s0, econstrs0, renv0, lenv0) match {
        case Ok(t) => Ok(t)
        case Err(e) => Err(f(e))
      }
    }

    InferMonad(runNext)
  }

  // TODO: Necessary for pattern matching?
  // TODO: What should this return?
  def withFilter(f: A => Boolean): InferMonad[A] = InferMonad {
    case (x, econstrs0, renv0, lenv0) => run(x, econstrs0, renv0, lenv0) match {
      case Ok((subst, econstrs, renv, lenv, t)) => if (f(t)) Ok((subst, econstrs, renv, lenv, t)) else Ok((subst, econstrs, renv, lenv, t))
      case Err(e) => Err(e)
    }
  }
}
