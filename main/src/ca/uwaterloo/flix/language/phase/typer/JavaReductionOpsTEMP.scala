/*
 * Copyright 2026 Flix Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.jvm.JavaFieldRef
import ca.uwaterloo.flix.language.ast.shared.JConstructor
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaArgument
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import java.lang.constant.ClassDesc

/** Checks descriptor-based Java member resolution against reflective lookup. */
private[phase] object JavaReductionOpsTEMP {

  /** Compares the old reflective constructor result with the new descriptor-based best candidates. */
  def compareConstructors(owner: ClassDesc,
                          arguments: List[JavaArgument],
                          oldResult: Option[JConstructor],
                          newResult: Result[List[JConstructor], JavaLookupError],
                          loc: SourceLocation): Unit = {
    val query = s"${owner.displayName()}(${arguments.mkString(", ")})"
    newResult match {
      case Err(error) =>
        throw InternalCompilerException(s"Java constructor shadow lookup failed for '$query': $error", loc)
      case Ok(result) =>
        val matches = oldResult match {
          case None => result.isEmpty
          case Some(constructor) => result.contains(constructor)
        }
        if (!matches) {
          throw InternalCompilerException(
            s"Java constructor lookup mismatch for '$query': reflection=$oldResult, descriptor=$result",
            loc
          )
        }
    }
  }

  /** Compares the old reflective field result with the new descriptor-based field result. */
  def compareField(owner: ClassDesc,
                   name: String,
                   oldResult: Option[JavaFieldRef],
                   newResult: Result[Option[JavaFieldRef], JavaLookupError],
                   loc: SourceLocation): Unit = {
    val query = s"${owner.displayName()}.$name"
    newResult match {
      case Err(error) =>
        throw InternalCompilerException(s"Java field shadow lookup failed for '$query': $error", loc)
      case Ok(result) =>
        if (oldResult != result) {
          throw InternalCompilerException(
            s"Java field lookup mismatch for '$query': reflection=$oldResult, descriptor=$result",
            loc
          )
        }
    }
  }

}
