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
    * An error raised to indicate that the variable symbol `sym` is hidden.
    *
    * @param sym the hidden variable symbol.
    * @param loc the source location of the use.
    */
  case class HiddenVarSym(sym: Symbol.VarSym, loc: SourceLocation) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Hidden variable symbol '" << Red(sym.text) << "'. The symbol is marked as unused." << NewLine
      vt << NewLine
      vt << Code(loc, "hidden symbol.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Don't use the variable symbol." << NewLine
      vt << "  (2)  Rename the underscore prefix from the variable symbol name." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that a variable has been shadowed.
    *
    * @param sym1 the shadowed variable.
    * @param sym2 the shadowing variable.
    */
  case class ShadowedVar(sym1: Symbol.VarSym, sym2: Symbol.VarSym) extends RedundancyError {
    val source: Source = sym1.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Shadowed variable: '" << Red(sym1.text) << "'." << NewLine
      vt << NewLine
      vt << Code(sym2.loc, "shadowing variable.") << NewLine
      vt << NewLine
      vt << "The shadowed variable was declared here:" << NewLine
      vt << NewLine
      vt << Code(sym1.loc, "shadowed variable.") << NewLine
      vt << NewLine
    }
  }

  /**
    * An error raised to indicate that an expression is trivial.
    *
    * @param loc the location of the expression.
    */
  case class TrivialExpression(loc: SourceLocation) extends RedundancyError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Trivial expression: It performs a suspicious computation." << NewLine
      vt << NewLine
      vt << Code(loc, "trivial expression.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Ensure that there is no typo or mistake in the expression." << NewLine
      vt << "  (2)  Simplify the expression." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the def with the symbol `sym` is not used.
    *
    * @param sym the unused enum symbol.
    */
  case class UnusedDefSym(sym: Symbol.DefnSym) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused definition '" << Red(sym.name) << "'. The definition is never referenced." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused definition.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the definition." << NewLine
      vt << "  (2)  Remove the definition." << NewLine
      vt << "  (3)  Mark the definition as public." << NewLine
      vt << "  (4)  Prefix the definition name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

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
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the enum." << NewLine
      vt << "  (2)  Remove the enum." << NewLine
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
      vt << "Possible fixes:" << NewLine
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
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the formal parameter." << NewLine
      vt << "  (2)  Remove the formal parameter." << NewLine
      vt << "  (3)  Prefix the formal parameter name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the given relation symbol `sym` is not used.
    *
    * @param sym the unused relation symbol.
    */
  case class UnusedRelSym(sym: Symbol.RelSym) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused relation '" << Red(sym.name) << "'. The relation is never referenced." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused relation.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the relation." << NewLine
      vt << "  (2)  Remove the relation." << NewLine
      vt << "  (3)  Mark the relation as public." << NewLine
      vt << "  (4)  Prefix the relation name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the given lattice symbol `sym` is not used.
    *
    * @param sym the unused lattice symbol.
    */
  case class UnusedLatSym(sym: Symbol.LatSym) extends RedundancyError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused lattice '" << Red(sym.name) << "'. The lattice is never referenced." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused relation.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the lattice." << NewLine
      vt << "  (2)  Remove the lattice." << NewLine
      vt << "  (3)  Mark the lattice as public." << NewLine
      vt << "  (4)  Prefix the lattice name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the given type parameter `ident` is not used.
    *
    * @param ident the unused type variable.
    */
  case class UnusedTypeParam(ident: Name.Ident) extends RedundancyError {
    val source: Source = ident.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused type parameter '" << Red(ident.name) << "'. The parameter is not referenced anywhere." << NewLine
      vt << NewLine
      vt << Code(ident.loc, "unused type parameter.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the type parameter." << NewLine
      vt << "  (2)  Remove type parameter." << NewLine
      vt << "  (3)  Prefix the type parameter name with an underscore." << NewLine
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
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unused local variable '" << Red(sym.text) << "'. The variable is not referenced within its scope." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unused local variable.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the local variable." << NewLine
      vt << "  (2)  Remove local variable declaration." << NewLine
      vt << "  (3)  Prefix the variable name with an underscore." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that an expression is useless.
    *
    * @param loc the location of the expression.
    */
  case class UselessExpression(loc: SourceLocation) extends RedundancyError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Useless expression: It has no side-effect(s) and its result is discarded." << NewLine
      vt << NewLine
      vt << Code(loc, "useless expression.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Use the result computed by the expression." << NewLine
      vt << "  (2)  Remove the expression statement." << NewLine
      vt << "  (3)  Introduce a let-binding with a wildcard name." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that a pattern match is useless, i.e. cannot match because of a prior pattern match.
    *
    * @param pat  the useless pattern.
    * @param loc1 the source location of the useless pattern.
    * @param loc2 the source location of the previous pattern.
    */
  case class UselessPatternMatch(pat: String, loc1: SourceLocation, loc2: SourceLocation) extends RedundancyError {
    val source: Source = loc2.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Useless pattern match due to prior pattern match." << NewLine
      vt << NewLine
      vt << Code(loc1, "useless match.") << NewLine
      vt << NewLine
      vt << s"The match value is: '" << Cyan(pat) << "' which is incompatible with current pattern." << NewLine
      vt << NewLine
      vt << "The previous pattern match was here:" << NewLine
      vt << NewLine
      vt << Code(loc2, "previous match.") << NewLine
      vt << NewLine
      vt
    }
  }

}
