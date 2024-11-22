/*
 * Copyright 2024 Chenhao Gao
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

import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.ast.shared.Resolution

/**
  * Provides completions for local scope variables.
  * Everything that is resolved to Resolution.Var in the LocalScope is considered a local scope variable, including arguments.
  */
object VarCompleter {
  def getCompletions(err: ResolutionError.UndefinedName): Iterable[Completion] = {
    val matchingVars = err.env.m.collect {
      case (k, v) if k.startsWith(err.qn.ident.name) && v.exists{
        case Resolution.Var(_) => true
        case _ => false
      } => k
    }
    matchingVars.map(Completion.VarCompletion(_))
  }
}
