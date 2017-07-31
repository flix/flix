/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Eff, EffectSet, Source, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that the expected effects of an expression does not match its actual effects.
  *
  * @param expected the expected effects.
  * @param inferred the inferred effects.
  * @param loc      the location where the error occurred.
  */
case class EffectError(expected: Eff, inferred: Eff, loc: SourceLocation) extends CompilationError {
  val kind: String = "Effect Error"
  val source: Source = loc.source
  val message: VirtualTerminal = {
    val vt = new VirtualTerminal()
    vt << Line(kind, source.format) << NewLine
    vt << ">> Inferred effect(s) do not match the expected effect(s)." << NewLine
    vt << NewLine
    vt << Code(loc, "mismatched effect(s).") << NewLine
    vt << NewLine
    vt << "Expected: " << Cyan(pretty(expected)) << NewLine
    vt << "Inferred: " << Magenta(pretty(inferred)) << NewLine
  }

  /**
    * Returns a human readable representation of the given effect `eff`.
    */
  private def pretty(eff: Eff): String = eff match {
    case Eff.Box(EffectSet.Bot) => "Bot"
    case Eff.Box(EffectSet.Top) => "Top"
    case Eff.Box(EffectSet.MayMust(may, must)) => s"may = {${may.mkString(", ")}}, must = {${must.mkString(", ")}}"
    case _ => eff.toString
  }
}