/*
 * Copyright 2025 Magnus Madsen
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

sealed trait ExprContext

object ExprContext {

  /**
    * Represents an expression in an application context.
    *
    * For example, in `e1(e2, e3)` we have that `e1` is `UnderApply` but `e2` and `e3` are not.
    */
  case object InsideApply extends ExprContext

  /**
    * Represents an expression in a match context.
    *
    * For example, in `1 |> println` we have that `println` is `UnderPipeline`.
    *
    * Currently, the only pipeline considered is `|>`, so the number of arguments from the pipeline is always 1.
    */
  case object InsidePipeline extends ExprContext

  /**
    * Represents an expression in an unknown context.
    */
  case object Unknown extends ExprContext

}
