/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for redundancy errors.
  */
trait RedundancyError extends CompilationError {
  def kind: String = "Redundancy Error"
}

object RedundancyError {

  /**
    * An error raised to indicate that the enum with the symbol `sym` is not used.
    *
    * @param sym the unused enum symbol.
    */
  case class UnusedEnumSym(sym: Symbol.EnumSym) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused enum '" << Red(sym.name) << "'. Neither the enum nor its cases are ever used." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused enum.") << NewLine
      vt << NewLine
      vt << "Possible Fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the enum." << NewLine
      vt << "  (1)  Remove the enum." << NewLine
      vt << "  (3)  Mark the enum as public." << NewLine
      vt << "  (4)  Prefix the enum name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that in the enum with symbol `sym` the case `tag` is not used.
    *
    * @param sym the enum symbol.
    * @param tag the unused tag.
    */
  case class UnusedEnumTag(sym: Symbol.EnumSym, tag: Name.Ident) extends RedundancyError {
    val source: Source = tag.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused case '" << Red(tag.name) << "' in enum '" << Cyan(sym.name) << "'." << NewLine
      vt << NewLine
      vt << Code(tag.loc, "unused tag.") << NewLine
      vt << NewLine
      vt << "Possible Fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the case." << NewLine
      vt << "  (2)  Remove the case." << NewLine
      vt << "  (3)  Prefix the case with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the given formal parameter symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedFormalParam(sym: Symbol.VarSym) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused formal parameter '" << Red(sym.text) << "'. The parameter is not used within its scope." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused formal parameter.") << NewLine
      vt << NewLine
      vt << "Possible Fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the formal parameter." << NewLine
      vt << "  (2)  Remove the formal parameter." << NewLine
      vt << "  (3)  Replace the parameter name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  //------------------------------------------------------------------------------------------------------
  //------------------------------------------------------------------------------------------------------
  //------------------------------------------------------------------------------------------------------
  /**
    * An error raised to indicate that the given type parameter `ident` is not used within `defSym`.
    *
    * @param ident  the unused type variable
    * @param defSym the definition symbol.
    */
  case class UnusedTypeParam(ident: Name.Ident, defSym: Symbol.DefnSym) extends RedundancyError {
    val source: Source = ident.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused type parameter '" << Red(ident.name) << "'. The parameter is not used in the signature of '" << Cyan(defSym.text) << "'." << NewLine
      vt << NewLine
      vt << Code(ident.loc, "unused type parameter.") << NewLine
      vt << NewLine
      vt << "Remove the type parameter or use it in the function signature."
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the given variable symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedVarSym(sym: Symbol.VarSym) extends RedundancyError {
    // TODO: Suggest using wildcard.
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused local variable '" << Red(sym.text) << "'. The variable is not referenced within its scope." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused local variable.") << NewLine
      vt << NewLine
      vt << "Remove the variable declaration or use the variable within its scope."
      vt << NewLine
      vt
    }
  }

  // TODO: Refactor
  case class ImpossibleMatch(loc1: SourceLocation, loc2: SourceLocation) extends RedundancyError {
    val source: Source = loc1.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Impossible pattern match due to prior pattern match." << NewLine
      vt << NewLine
      vt << Code(loc2, "impossible due to prior match.") << NewLine
      vt << NewLine
      vt << ">> Previous match ensure that this cannot match" << NewLine
      vt << NewLine
      vt << ">> First pattern match was here:" << NewLine
      vt << Code(loc1, "first match.") << NewLine
      vt << ">> Second pattern match was here:" << NewLine
      vt << Code(loc2, "second match.") << NewLine
      vt << NewLine
      vt
    }
  }

  // TODO: Unused variable.

  // TODO: Unused type parameter.

  // TODO: Unused algebraic data type.

  // TODO: Unused ...

}
