/*
 *  Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{Name, Source, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends CompilationError {
  val kind = "Resolution Error"
}

object ResolutionError {

  /**
    * Ambiguous Name Error.
    *
    * @param qn   the ambiguous name.
    * @param ns   the current namespace.
    * @param loc1 the location where the 1st name is defined.
    * @param loc2 the location where the 2nd name is defined.
    * @param loc  the location where the error occurred.
    */
  case class AmbiguousName(qn: Name.QName, ns: Name.NName, loc1: SourceLocation, loc2: SourceLocation, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous name '" << Red(qn.toString) << "' Name refers to both a definition and an effect." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous name.") << NewLine
      vt << NewLine
      vt << Code(loc1, "the definition of the 1st match was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the definition of the 2nd match was here.") << NewLine
    }
  }

  /**
    * Ambiguous Tag Error.
    *
    * @param tag  the tag.
    * @param ns   the current namespace.
    * @param locs the source location of the matched tags.
    * @param loc  the location where the error occurred.
    */
  // TODO: Improve error message.
  case class AmbiguousTag(tag: String, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous tag '" << Red(tag) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous tag name.") << NewLine
      vt << NewLine
      vt << "The tag is defined in multiple enums:" << NewLine
      vt << NewLine
      for (l <- locs) {
        vt << Code(l, "tag is defined in this enum.") << NewLine
      }
      vt << Underline("Tip:") << " Prefix the tag with the enum name." << NewLine
    }
  }

  /**
    * Inaccessible Class Error.
    *
    * @param sym the definition symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleClass(sym: Symbol.ClassSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Class '" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible class.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the class as public." << NewLine
    }
  }

  /**
    * Inaccessible Def Error.
    *
    * @param sym the def symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleDef(sym: Symbol.DefnSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Definition '" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible definition.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the definition as public." << NewLine
    }
  }

  /**
    * Inaccessible Eff Error.
    *
    * @param sym the eff symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEff(sym: Symbol.EffSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Effect '" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible effect.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the effect as public." << NewLine
    }
  }

  /**
    * Inaccessible Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEnum(sym: Symbol.EnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Enum '" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible enum.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the definition as public." << NewLine
    }
  }

  /**
    * Unresolved Class Error.
    *
    * @param qn  the unresolved definition name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedClass(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined class '" << Red(qn.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "name not found") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent class?" << NewLine
    }
  }

  /**
    * Undefined Name Error.
    *
    * @param qn  the unresolved name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedName(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined name '" << Red(qn.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "name not found") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent definition?" << NewLine
    }
  }

  /**
    * Undefined Effect Error.
    *
    * @param qn  the unresolved name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedEff(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined effect '" << Red(qn.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "name not found") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent effect?" << NewLine
    }
  }

  /**
    * Undefined Table Error.
    *
    * @param qn  the table name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTable(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined table '" << Red(qn.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "table not found.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent table?" << NewLine
    }
  }

  /**
    * Undefined Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined tag '" << Red(tag) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "tag not found.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent tag?" << NewLine
    }
  }

  /**
    * Undefined Type Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined type '" << Red(qn.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "type not found.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent type?" << NewLine
    }
  }

  /**
    * Unhandled Effect Error.
    *
    * @param sym the unhandled effect symbol.
    */
  case class UnhandledEffect(sym: Symbol.EffSym) extends ResolutionError {
    val source: Source = sym.loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unhandled effect '" << Red(sym.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "no default handler.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent effect handler?" << NewLine
    }
  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedNativeClass(name: String, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined class '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined class.") << NewLine
    }
  }

}
