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
import org.json4s.{JInt, JValue}

sealed trait CodeActionTriggerKind

object CodeActionTriggerKind {
  def parse(json: JValue): Result[CodeActionTriggerKind, String] = {
    def err(v: JValue) =
      Err(s"Code action trigger kind not recognized as a valid integer: '$v'.")

    json match {
      case JInt(i) => i.toInt match {
        case 1 => Ok(Invoked)
        case 2 => Ok(Automatic)
        case _ => err(json)
      }
      case _ => err(json)
    }
  }

  case object Invoked extends CodeActionTriggerKind

  case object Automatic extends CodeActionTriggerKind
}
