/*
 * Copyright 2020 Magnus Madsen
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

/**
  * Represents a `MessageType` in LSP.
  */
sealed trait MessageType {
  def toInt: Int = this match {
    case MessageType.Error => 1
    case MessageType.Warning => 2
    case MessageType.Info => 3
    case MessageType.Log => 4
  }
}

object MessageType {

  /**
    * An error message.
    */
  case object Error extends MessageType

  /**
    * A warning message.
    */
  case object Warning extends MessageType

  /**
    * An information message.
    */
  case object Info extends MessageType

  /**
    * A log message.
    */
  case object Log extends MessageType

}
