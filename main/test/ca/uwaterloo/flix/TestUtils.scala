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

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationError, ast}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{Options, Validation}
import org.scalatest.FunSuite

import scala.reflect.ClassTag

trait TestUtils {

  this: FunSuite =>

  /**
    * Compiles the given input string `s` with the given compilation options `o`.
    */
  def compile(s: String, o: Options): Validation[CompilationResult, CompilationError] = new Flix().setOptions(o).addStr(s).compile()

  /**
    * Asserts that the validation is a failure with a value of the parametric type `T`.
    */
  def expectError[T](result: Validation[CompilationResult, CompilationError])(implicit classTag: ClassTag[T]): Unit = result match {
    case Validation.Success(_) => fail(s"Expected Failure, but got Success.")
    case Validation.Failure(errors) =>
      val expected = classTag.runtimeClass
      val actuals = errors.map(_.getClass)

      if (!actuals.exists(expected.isAssignableFrom(_)))
        fail(s"Expected an error of type ${expected.getSimpleName}, but found ${actuals.mkString(", ")}.")
      else if (errors.exists(e => e.loc == SourceLocation.Unknown))
        fail("Error contains unknown source location.")
  }

  /**
    * Asserts that the validation does not contain a value of the parametric type `T`.
    */
  def rejectError[T](result: Validation[CompilationResult, CompilationError])(implicit classTag: ClassTag[T]): Unit = result match {
    case Validation.Success(_) => ()
    case Validation.Failure(errors) =>
      val rejected = classTag.runtimeClass
      val actuals = errors.map(_.getClass)

      if (actuals.exists(rejected.isAssignableFrom(_)))
        fail(s"Unexpected an error of type ${rejected.getSimpleName}.")
  }
}
