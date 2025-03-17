/*
 * Copyright 2025 Chenhao Gao
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

sealed trait UndefinedNameContextState

/**
  * Represents the state when parsing the undefined name context.
  *
  */
object UndefinedNameContextState {
  /**
    * Represents the start state.
    *
    * In this state we should skip any number of Malformed and expect an undefined name.
    */
  case object Start extends UndefinedNameContextState

  /**
    * Represents the state when we have found an undefined name.
    *
    * In this state we need to find of the number of arguments and pipelines applied to this undefined name.
    */
  case object CanBeApplied extends UndefinedNameContextState

  /**
    * Represents the state when we are done.
    */
  case object Done extends UndefinedNameContextState
}

case class UndefinedNameContext(applied: Option[Int], pipelined: Option[Int])
