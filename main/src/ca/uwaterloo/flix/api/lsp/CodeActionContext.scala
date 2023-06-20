/*
 * Copyright 2023 Holger Dal Mogensen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s.JsonAST._

object CodeActionContext {
  def parse(json: JValue): Result[CodeActionContext, String] = {
    val diagnosticsResult = json \ "diagnostics" match {
      case JArray(l) => Result.traverse(l)(Diagnostic.parse)
      case v => Err(s"Unexpected non-array diagnostics: '$v'.")
    }

    val onlyResult: Result[List[CodeActionKind], String] = json \ "only" match {
      case JNothing => Ok(List())
      case JArray(l) => Result.traverse(l)(CodeActionKind.parse)
      case v => Err(s"Unexpected non-array only-field: '$v'.")
    }

    val triggerKindResult: Result[Option[CodeActionTriggerKind], String] = json \ "triggerKind" match {
      case JNothing => Ok(None)
      case x => CodeActionTriggerKind.parse(x) match {
        case Ok(v) => Ok(Some(v))
        case Err(s) => Err(s)
      }
    }

    for {
      diagnostics <- diagnosticsResult
      only <- onlyResult
      triggerKind <- triggerKindResult
    } yield CodeActionContext(diagnostics, only, triggerKind)
  }
}

case class CodeActionContext(diagnostics: List[Diagnostic], only: List[CodeActionKind], triggerKind: Option[CodeActionTriggerKind]) {}
