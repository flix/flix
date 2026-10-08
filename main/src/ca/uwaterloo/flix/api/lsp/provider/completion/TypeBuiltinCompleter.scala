/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.Range

object TypeBuiltinCompleter {
  private def polycompletion(name: String, params: List[String], range: Range, p: Priority): Completion = {
    val edit = params.zipWithIndex.map { case (param, i) => s"$${${i + 1}:$param}"}.mkString(s"$name[", ", ", "]")
    val finalName = params.mkString(s"$name[", ", ", "]")
    Completion.TypeBuiltinPolyCompletion(finalName, edit, range, p)
  }

  /**
    * Returns a List of Completion for builtin types.
    */
  def getCompletions(range: Range): Iterable[Completion] =
    List(
      // A
      polycompletion("Array"   , List("a", "r")    , range,  Priority.Medium(0)),
      // B
      Completion.TypeBuiltinCompletion("BigDecimal", range, Priority.Low(0)),
      Completion.TypeBuiltinCompletion("BigInt"    , range, Priority.High(0)),
      Completion.TypeBuiltinCompletion("Bool"      , range, Priority.Higher(0)),
      // C
      Completion.TypeBuiltinCompletion("Char"      , range, Priority.Medium(0)),
      // F
      Completion.TypeBuiltinCompletion("Float32"   , range, Priority.High(0)),
      Completion.TypeBuiltinCompletion("Float64"   , range, Priority.Low(0)),
      // I
      Completion.TypeBuiltinCompletion("Int16"     , range, Priority.Low(0)),
      Completion.TypeBuiltinCompletion("Int32"     , range, Priority.Higher(0)),
      Completion.TypeBuiltinCompletion("Int64"     , range, Priority.High(0)),
      Completion.TypeBuiltinCompletion("Int8"      , range, Priority.Lower(0)),
      // L
      polycompletion("Lazy"    , List("t")         , range, Priority.Medium(0)),
      // R
      polycompletion("Receiver", List("t")         , range, Priority.Low(0)),
      polycompletion("Region"  , List("r")         , range, Priority.High(0)),
      // S
      polycompletion("Sender"  , List("t")         , range, Priority.Low(0)),
      Completion.TypeBuiltinCompletion("String"    , range, Priority.High(0)),
      // U
      Completion.TypeBuiltinCompletion("Unit"      , range, Priority.Medium(0)),
      // V
      polycompletion("Vector"  , List("a")         , range, Priority.High(0)),
      Completion.TypeBuiltinCompletion("Void"      , range, Priority.Low(0)),
    )
}
