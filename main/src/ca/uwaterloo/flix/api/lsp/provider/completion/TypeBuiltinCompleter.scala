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
      // A
      polycompletion("Array"   , List("a", "r")    , Priority.Default),
      // B
      Completion.TypeBuiltinCompletion("BigDecimal", Priority.Low),
      Completion.TypeBuiltinCompletion("BigInt"    , Priority.High),
      Completion.TypeBuiltinCompletion("Bool"      , Priority.Higher),
      // C
      Completion.TypeBuiltinCompletion("Char"      , Priority.Default),
      // F
      Completion.TypeBuiltinCompletion("Float32"   , Priority.High),
      Completion.TypeBuiltinCompletion("Float64"   , Priority.Low),
      // I
      Completion.TypeBuiltinCompletion("Int16"     , Priority.Low),
      Completion.TypeBuiltinCompletion("Int32"     , Priority.Higher),
      Completion.TypeBuiltinCompletion("Int64"     , Priority.High),
      Completion.TypeBuiltinCompletion("Int8"      , Priority.Lower),
      // L
      polycompletion("Lazy"    , List("t")         , Priority.Default),
      // R
      polycompletion("Receiver", List("t", "r")    , Priority.Default),
      // S
      polycompletion("Sender"  , List("t", "r")    , Priority.Low),
      Completion.TypeBuiltinCompletion("String"    , Priority.High),
      // U
      Completion.TypeBuiltinCompletion("Unit"      , Priority.Default),
      // V
      polycompletion("Vector"  , List("a")         , Priority.High),
      Completion.TypeBuiltinCompletion("Void"      , Priority.Low),
    )
}
