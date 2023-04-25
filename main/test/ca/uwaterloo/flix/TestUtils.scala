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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{Formatter, Options, Validation}
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.ClassTag

trait TestUtils {

  this: AnyFunSuite =>

  /**
    * Compiles the given input string `s` with the given compilation options `o`.
    */
  def compile(s: String, o: Options): Validation[CompilationResult, CompilationMessage] =
    new Flix().setOptions(o).addSourceCode("<test>", s).compile()

  private def errorString(errors: Seq[CompilationMessage]): String = {
    errors.map(_.message(Formatter.NoFormatter)).mkString("\n\n")
  }

  /**
    * Asserts that the validation is a failure with a value of the parametric type `T`.
    */
  def expectError[T](result: Validation[CompilationResult, CompilationMessage])(implicit classTag: ClassTag[T]): Unit = result match {
    case Validation.Success(_) => fail(s"Expected Failure, but got Success.")

    case failure =>
      val expected = classTag.runtimeClass
      val actuals = failure.errors.map(_.getClass).toList
  }

  /**
    * Asserts that the validation does not contain a value of the parametric type `T`.
    */
  def rejectError[T](result: Validation[CompilationResult, CompilationMessage])(implicit classTag: ClassTag[T]): Unit = result match {
    case Validation.Success(_) => ()

    case failure =>
      val rejected = classTag.runtimeClass
      val actuals = failure.errors.map(_.getClass)

      if (actuals.exists(rejected.isAssignableFrom(_)))
        fail(s"Unexpected an error of type ${rejected.getSimpleName}.")
  }

  /**
    * Asserts that the validation is successful.
    */
  def expectSuccess(result: Validation[CompilationResult, CompilationMessage]): Unit = result match {
    case Validation.Success(_) => ()
    case failure =>
      fail(s"Expected success, but found errors:\n\n${errorString(failure.errors)}.")
  }
}
