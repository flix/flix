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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.language.errors.ResolutionError

object TypeBuiltinCompleter {
  private def polycompletion(name: String, params: List[String], range: Range, p: Priority): Completion = {
    val edit = params.zipWithIndex.map { case (param, i) => s"$${${i + 1}:$param}"}.mkString(s"$name[", ", ", "]")
    val finalName = params.mkString(s"$name[", ", ", "]")
    Completion.TypeBuiltinPolyCompletion(finalName, range, edit, p)
  }

  /**
    * Returns a List of Completion for builtin types.
    */
  def getCompletions(err: ResolutionError.UndefinedType): Iterable[Completion] = {
    val range = Range.from(err.loc)
    List(
      // A
      polycompletion("Array"   , List("a", "r")    , range,  Priority.Default),
      // B
      Completion.TypeBuiltinCompletion("BigDecimal", range, Priority.Low),
      Completion.TypeBuiltinCompletion("BigInt"    , range, Priority.High),
      Completion.TypeBuiltinCompletion("Bool"      , range, Priority.Higher),
      // C
      Completion.TypeBuiltinCompletion("Char"      , range, Priority.Default),
      // F
      Completion.TypeBuiltinCompletion("Float32"   , range, Priority.High),
      Completion.TypeBuiltinCompletion("Float64"   , range, Priority.Low),
      // I
      Completion.TypeBuiltinCompletion("Int16"     , range, Priority.Low),
      Completion.TypeBuiltinCompletion("Int32"     , range, Priority.Higher),
      Completion.TypeBuiltinCompletion("Int64"     , range, Priority.High),
      Completion.TypeBuiltinCompletion("Int8"      , range, Priority.Lower),
      // L
      polycompletion("Lazy"    , List("t")         , range, Priority.Default),
      // R
      polycompletion("Receiver", List("t")         , range, Priority.Low),
      polycompletion("Region"  , List("r")         , range, Priority.High),
      // S
      polycompletion("Sender"  , List("t")         , range, Priority.Low),
      Completion.TypeBuiltinCompletion("String"    , range, Priority.High),
      // U
      Completion.TypeBuiltinCompletion("Unit"      , range, Priority.Default),
      // V
      polycompletion("Vector"  , List("a")         , range, Priority.High),
      Completion.TypeBuiltinCompletion("Void"      , range, Priority.Low),
    )
  }
}
