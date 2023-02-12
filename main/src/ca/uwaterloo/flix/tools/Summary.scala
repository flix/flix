/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, Type}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Summary {

  /**
    * The column separator.
    */
  private val Separator = " & "

  /**
    * The end of line separator.
    */
  private val EndOfLine = " \\\\"

  /**
    * The width of the module column.
    */
  private val ModWidth = 30

  /**
    * The width of every other column.
    */
  private val ColWidth = 12

  /**
    * Returns `true` if the given module `mod` should be printed.
    */
  private def include(mod: String, lines: Int): Boolean =
    !mod.contains("/") && lines > 700

  def printSummary(v: Validation[Root, CompilationMessage]): Unit = mapN(v) {
    case root =>

      print(padR("Module", ModWidth))
      print(Separator)
      print(padL("Lines", ColWidth))
      print(Separator)
      print(padL("Functions", ColWidth))
      print(Separator)
      print(padL("Pure", ColWidth))
      print(Separator)
      print(padL("Impure", ColWidth))
      print(Separator)
      print(padL("Polymorphic", ColWidth))
      println(EndOfLine)

      var totalLines = 0
      var totalFunctions = 0
      var totalPureFunctions = 0
      var totalImpureFunctions = 0
      var totalEffPolymorphicFunctions = 0

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val defs = getDefs(source, root)

        val numberOfLines = loc.endLine
        val numberOfFunctions = defs.size
        val numberOfPureFunctions = defs.count(isPure)
        val numberOfImpureFunctions = defs.count(isImpure)
        val numberOfEffectPolymorphicFunctions = defs.count(isEffectPolymorphic)

        totalLines = totalLines + numberOfLines
        totalFunctions = totalFunctions + numberOfFunctions
        totalPureFunctions = totalPureFunctions + numberOfPureFunctions
        totalImpureFunctions = totalImpureFunctions + numberOfImpureFunctions
        totalEffPolymorphicFunctions = totalEffPolymorphicFunctions + numberOfEffectPolymorphicFunctions

        if (include(module, numberOfLines)) {
          print(padR(module, ModWidth))
          print(Separator)
          print(padL(format(numberOfLines), ColWidth))
          print(Separator)
          print(padL(format(numberOfFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfPureFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfImpureFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfEffectPolymorphicFunctions), ColWidth))
          println(EndOfLine)
        }
      }

      println("---")
      print(padR("Totals (incl. filtered)", ModWidth))
      print(Separator)
      print(padL(format(totalLines), ColWidth))
      print(Separator)
      print(padL(format(totalFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalPureFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalImpureFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalEffPolymorphicFunctions), ColWidth))
      println(EndOfLine)
  }

  /**
    * Returns all defs in the given `source`.
    */
  private def getDefs(source: Ast.Source, root: Root): Iterable[Def] =
    root.defs.collect {
      case (sym, defn) if sym.loc.source == source => defn
    }

  /**
    * Formats the given number `n`.
    */
  private def format(n: Int): String = f"$n%,d".replace(".", ",")

  /**
    * Returns `true` if the given `defn` is pure.
    */
  private def isPure(defn: Def): Boolean = defn.spec.pur == Type.Pure

  /**
    * Returns `true` if the given `defn` is impure.
    */
  private def isImpure(defn: Def): Boolean = defn.spec.pur == Type.Impure

  /**
    * Returns `true` if the given `defn` is effect polymorphic (neither pure or impure).
    */
  private def isEffectPolymorphic(defn: Def): Boolean = !isPure(defn) && !isImpure(defn)

  /**
    * Right-pads the given string `s` to length `l`.
    */
  private def padR(s: String, l: Int): String = s.padTo(l, ' ')

  /**
    * Left-pads the given string `s` to length `l`.
    */
  private def padL(s: String, l: Int): String = {
    if (s.length >= l) {
      return s
    }
    val sb = new StringBuilder
    while (sb.length < l - s.length) {
      sb.append(' ')
    }
    sb.append(s)
    sb.toString
  }

}
