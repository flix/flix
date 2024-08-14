/*
 * Copyright 2024 Joseph Tan
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.ast.TypedAst

import java.lang.reflect.{Field, Method, Modifier}

object StructFieldCompleter {
  def getCompletions(e: ResolutionError.UndefinedStructField, root: TypedAst.Root): Iterable[Completion.StructFieldCompletion] = {
    val struct = e.sym
    val completions = root.structs(struct).fields.filter {
      case (k, v) => k.name.startsWith(e.field.name)
    }
    completions.values.map(field => Completion.StructFieldCompletion(field.sym.name))
  }
}
