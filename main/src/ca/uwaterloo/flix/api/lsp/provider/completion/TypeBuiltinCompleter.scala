/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Index, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TypeBuiltinCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object TypeBuiltinCompleter {
  private def polycompletion(name: String, params: List[String], p: Priority): Completion = {
    val edit = params.zipWithIndex.map { case (param, i) => s"$${${i + 1}:$param}"}.mkString(s"$name[", ", ", "]")
    val finalName = params.mkString(s"$name[", ", ", "]")
    Completion.TypeBuiltinPolyCompletion(finalName, edit, p)
  }

  /**
    * Returns a List of Completion for builtin types.
    */
  def getCompletions: Iterable[Completion] =
    List(
      Completion.TypeBuiltinCompletion("Unit"      , Priority.Higher),
      Completion.TypeBuiltinCompletion("Bool"      , Priority.Higher),
      Completion.TypeBuiltinCompletion("Char"      , Priority.Higher),
      Completion.TypeBuiltinCompletion("Float64"   , Priority.Higher),
      Completion.TypeBuiltinCompletion("BigDecimal", Priority.Higher),
      Completion.TypeBuiltinCompletion("Int32"     , Priority.Higher),
      Completion.TypeBuiltinCompletion("Int64"     , Priority.Higher),
      Completion.TypeBuiltinCompletion("BigInt"    , Priority.Higher),
      Completion.TypeBuiltinCompletion("String"    , Priority.Higher),
      Completion.TypeBuiltinCompletion("Int8"      , Priority.Low),
      Completion.TypeBuiltinCompletion("Int16"     , Priority.Low),
      Completion.TypeBuiltinCompletion("Float32"   , Priority.Low),
      Completion.TypeBuiltinCompletion("Void"      , Priority.Low),
      polycompletion("Array"   , List("a", "r")    , Priority.High),
      polycompletion("Vector"  , List("a")         , Priority.High),
      polycompletion("Sender"  , List("t", "r")    , Priority.High),
      polycompletion("Receiver", List("t", "r")    , Priority.High),
      polycompletion("Lazy"    , List("t")         , Priority.High),
    )
}
