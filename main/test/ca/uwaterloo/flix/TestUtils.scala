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

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.util.Validation
import org.scalatest.FunSuite

import scala.reflect.ClassTag

trait TestUtils {

  this: FunSuite =>

  /**
    * Asserts that the validation is a failure with a value of the parametric type `T`.
    */
  def expectError[T](result: Validation[ExecutableAst, CompilationError])(implicit classTag: ClassTag[T]): Unit = result match {
    case Validation.Success(_, _) => fail(s"Expected Failure, but got Success.")
    case Validation.Failure(errors) =>
      val expected = classTag.runtimeClass
      val actual = errors.head.getClass
      if (expected != actual)
        fail(s"Expected an error of type ${expected.getSimpleName}, but got ${actual.getSimpleName}.")
  }

}
