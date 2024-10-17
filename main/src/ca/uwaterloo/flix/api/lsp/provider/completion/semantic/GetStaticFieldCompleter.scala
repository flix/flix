/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion.semantic

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.JvmUtils

object GetStaticFieldCompleter {

  def getCompletions(e: ResolutionError.UndefinedJvmStaticField): List[Completion] = {
    JvmUtils.getStaticFields(e.clazz).sortBy(_.getName).map(FieldCompletion(e.field, _))
  }

}
