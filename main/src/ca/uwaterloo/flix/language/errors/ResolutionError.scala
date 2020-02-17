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

import java.lang.reflect.{Constructor, Field, Method}

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.Type._
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.tc.Show.ShowableSyntax
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
    * @param locs the locations where the names are defined.
    * @param loc  the location where the error occurred.
    */
  case class AmbiguousName(qn: Name.QName, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous name '" << Red(qn.toString) << "' Name refers to multiple definitions/effects/signatures." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous name.") << NewLine
      vt << NewLine
      for (loc1 <- locs) {
        vt << Code(loc1, "definition/effect/signature matches.") << NewLine
        vt << NewLine
      }
      vt
    }
  }

  /**
    * Ambiguous Type Error.
    *
    * @param qn   the ambiguous name.
    * @param ns   the current namespace.
    * @param locs the locations where the names are defined.
    * @param loc  the location where the error occurred.
    */
  case class AmbiguousType(qn: String, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous type '" << Red(qn.toString) << "'. Name refers to multiple types." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous type.") << NewLine
      vt << NewLine
      for (loc1 <- locs) {
        vt << Code(loc1, "type matches.") << NewLine
        vt << NewLine
      }
      vt
    }
  }

  /**
    * Ambiguous Relation or Lattice Error.
    *
    * @param qn   the ambiguous name.
    * @param ns   the current namespace.
    * @param locs the locations where the names are defined.
    * @param loc  the location where the error occurred.
    */
  case class AmbiguousRelationOrLattice(qn: Name.QName, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous name '" << Red(qn.toString) << "' Name refers to multiple relations or lattices." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous name.") << NewLine
      vt << NewLine
      for (loc1 <- locs) {
        vt << Code(loc1, "relation/lattice matches.") << NewLine
        vt << NewLine
      }
      vt
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
    * Illegal Type Error.
    *
    * @param tpe the illegal type.
    * @param loc the location where the error occurred.
    */
  case class IllegalType(tpe: Type, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal type: '" << Red(tpe.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal type.") << NewLine
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
    * Inaccessible Relation Error.
    *
    * @param sym the relation symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleRelation(sym: Symbol.RelSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Relation '" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible relation.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the relation as public." << NewLine
    }
  }

  /**
    * Inaccessible Lattice Error.
    *
    * @param sym the lattice symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleLattice(sym: Symbol.LatSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Lattice '" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible lattice.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the lattice as public." << NewLine
    }
  }

  /**
    * Non Relation or Lattice Error.
    *
    * @param tpe the non-relation, non-lattice type.
    * @param loc the location where the error occurred.
    */
  case class NonRelationOrLattice(tpe: Type, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Non-relation, non-lattice type '" << Red(tpe.show) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "non-relation, non-lattice type.") << NewLine
    }
  }

  /**
    * Recursion Limit Error.
    *
    * @param ident the type alias symbol.
    * @param limit the current recursion limit.
    * @param loc   the location where the error occurred.
    */
  case class RecursionLimit(ident: Symbol.TypeAliasSym, limit: Int, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Recursion limit (" << limit << ") reached while unfolding the '" << Red(ident.name) << "' type alias." << NewLine
      vt << NewLine
      vt << Code(loc, "recursion limit reached.") << NewLine
      vt << NewLine
      vt << "Ensure that there is no cyclic definition of type aliases."
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
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedJvmClass(name: String, loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined class '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined class.") << NewLine
    }
  }

  /**
    * An error raised to indicate that a matching constructor was not found.
    *
    * @param className    the class name.
    * @param signature    the signature of the constructor.
    * @param constructors the constructors in the class.
    * @param loc          the location of the constructor name.
    */
  case class UndefinedJvmConstructor(className: String, signature: List[Class[_]], constructors: List[Constructor[_]], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined constructor in class '" << Cyan(className) << "' with the given signature." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined constructor.") << NewLine
      vt << "No constructor matches the signature:" << NewLine
      vt << "  " << className << "(" << signature.map(_.toString).mkString(",") << ")" << NewLine << NewLine
      vt << "Available constructors:" << NewLine
      for (constructor <- constructors) {
        vt << "  " << stripAccessModifier(constructor.toString) << NewLine
      }
      vt
    }
  }

  /**
    * An error raised to indicate that a matching method was not found.
    *
    * @param className  the class name.
    * @param methodName the method name.
    * @param static     whether the method is static.
    * @param signature  the signature of the method.
    * @param methods    the methods of the class.
    * @param loc        the location of the method name.
    */
  case class UndefinedJvmMethod(className: String, methodName: String, static: Boolean, signature: List[Class[_]], methods: List[Method], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      if (!static) {
        vt << ">> Undefined " << Magenta("object") << " method '" << Red(methodName) << "' in class '" << Cyan(className) << "." << NewLine
      } else {
        vt << ">> Undefined " << Magenta("static") << " method '" << Red(methodName) << "' in class '" << Cyan(className) << "." << NewLine
      }
      vt << NewLine
      vt << Code(loc, "undefined method.") << NewLine
      vt << "No method matches the signature:" << NewLine
      vt << "  " << methodName << "(" << signature.map(_.toString).mkString(",") << ")" << NewLine << NewLine
      vt << "Available methods:" << NewLine
      for (method <- methods) {
        vt << "  " << stripAccessModifier(method.toString) << NewLine
      }
      vt
    }
  }

  /**
    * An error raised to indicate that the field name was not found.
    *
    * @param className the class name.
    * @param fieldName the field name.
    * @param static    whether the field is static.
    * @param fields    the fields of the class.
    * @param loc       the location of the method name.
    */
  case class UndefinedJvmField(className: String, fieldName: String, static: Boolean, fields: List[Field], loc: SourceLocation) extends ResolutionError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      if (!static) {
        vt << ">> Undefined " << Magenta("object") << " field '" << Red(fieldName) << "' in class '" << Cyan(className) << "." << NewLine
      } else {
        vt << ">> Undefined " << Magenta("static") << " field '" << Red(fieldName) << "' in class '" << Cyan(className) << "." << NewLine
      }
      vt << NewLine
      vt << Code(loc, "undefined field.") << NewLine
      vt << "Available fields:" << NewLine
      for (field <- fields) {
        vt << "  " << stripAccessModifier(field.toString) << NewLine
      }
      vt
    }
  }

  /**
    * Removes all access modifiers from the given string `s`.
    */
  private def stripAccessModifier(s: String): String =
    s.replace("public", "").
      replace("protected", "").
      replace("private", "")

}
