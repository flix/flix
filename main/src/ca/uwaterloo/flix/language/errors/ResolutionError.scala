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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, UnkindedType}
import ca.uwaterloo.flix.language.debug.{Audience, FormatKind, FormatType}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends CompilationError {
  def kind = "Resolution Error"
}

object ResolutionError {

  private implicit val audience: Audience = Audience.External

  /**
    * Ambiguous Name Error.
    *
    * @param qn   the ambiguous name.
    * @param ns   the current namespace.
    * @param locs the locations where the names are defined.
    * @param loc  the location where the error occurred.
    */
  case class AmbiguousName(qn: Name.QName, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    def summary: String = "Ambiguous name."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous name '" << Red(qn.toString) << "' Name refers to multiple definitions." << NewLine
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
    def summary: String = "Ambiguous type."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous type '" << Red(qn) << "'. Name refers to multiple types." << NewLine
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
    * Ambiguous Tag Error.
    *
    * @param tag  the tag.
    * @param ns   the current namespace.
    * @param locs the source location of the matched tags.
    * @param loc  the location where the error occurred.
    */
  case class AmbiguousTag(tag: String, ns: Name.NName, locs: List[SourceLocation], loc: SourceLocation) extends ResolutionError {
    def summary: String = "Ambiguous tag."
    def message: VirtualTerminal = {
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
  case class IllegalType(tpe: UnkindedType, loc: SourceLocation) extends ResolutionError {
    private def formatUnkindedType(tpe: UnkindedType): String = tpe.toString // TODO

    def summary: String = "Illegal type."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Illegal type: '" << Red(formatUnkindedType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "illegal type.") << NewLine
    }
  }

  /**
    * Inaccessible Class Error.
    *
    * @param sym the class symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleClass(sym: Symbol.ClassSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Class'" << Red(sym.toString) << s"' is not accessible from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "inaccessible class.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Mark the class as public." << NewLine
    }
  }

  /**
    * Sealed Class Error.
    *
    * @param sym the class symbol.
    * @param ns  the namespace from which the class is sealed.
    * @param loc the location where the error occurred.
    */
  case class SealedClass(sym: Symbol.ClassSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Sealed."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Class '" << Red(sym.toString) << s"' is sealed from the namespace '" << Cyan(ns.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "sealed class.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Move the instance or sub class to the class's namespace." << NewLine
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
    def summary: String = "Inaccessible."
    def message: VirtualTerminal = {
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
    * Inaccessible Sig Error.
    *
    * @param sym the sig symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleSig(sym: Symbol.SigSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."
    def message: VirtualTerminal = {
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
    def summary: String = "Inaccessible."
    def message: VirtualTerminal = {
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
    * Recursion Limit Error.
    *
    * @param ident the type alias symbol.
    * @param limit the current recursion limit.
    * @param loc   the location where the error occurred.
    */
  case class RecursionLimit(ident: Symbol.TypeAliasSym, limit: Int, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Recursion limit $limit reached while unfolding the ${ident.name} type alias."
    def message: VirtualTerminal = {
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
    def summary: String = "Undefined name."
    def message: VirtualTerminal = {
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
    * Undefined Sig Error.
    *
    * @param clazz  the class.
    * @param sig    the unresolved sig.
    * @param ns     the current namespace.
    * @param loc    the location where the error occurred.
    */
  case class UndefinedSig(clazz: Name.QName, sig: Name.Ident, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Undefined signature."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined signature '" << Red(sig.name) << "' in class '" << Red(clazz.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "signature not found") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent class or signature?" << NewLine
    }
  }

  /**
    * Undefined Class Error.
    *
    * @param qn  the unresolved class.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedClass(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Undefined class."
    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined class '" << Red(qn.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "class not found") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent class?" << NewLine
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
    def summary: String = "Undefined tag."
    def message: VirtualTerminal = {
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
    def summary: String = "Undefined type"
    def message: VirtualTerminal = {
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
    def summary: String = "Undefined class."
    def message: VirtualTerminal = {
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
    def summary: String = "Undefined constructor."
    def message: VirtualTerminal = {
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
    def summary: String = {
      if (!static) {
        s"Undefined object method."
      } else {
        s"Undefined static method."
      }
    }

    def message: VirtualTerminal = {
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
    def summary: String = {
      if (!static) {
        s"Undefined object field."
      } else {
        s"Undefined static field."
      }
    }

    def message: VirtualTerminal = {
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
    * An error raise to indicate a cycle in the class hierarchy.
    *
    * @param path the super class path from a class to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicClassHierarchy(path: List[Symbol.ClassSym], loc: SourceLocation) extends ResolutionError {
    override def summary: String = {
      val pathString = path.reverse.map(clazz => s"'${clazz.name}'").mkString(" extends ")
      "Cyclic inheritance: " + pathString
    }

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << NewLine
      vt << Code(loc, "Cyclic inheritance.") << NewLine
      vt << NewLine
      vt << "The following classes are in the cycle:"
      for (List(subClass, superClass) <- path.reverse.sliding(2)) {
        vt << s"$subClass extends $superClass" << NewLine
      }
      vt
    }
  }

  // MATT docs
  case class DuplicateDerivation(sym: Symbol.ClassSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError {
    override def summary: String = "" // MATT
    override def message: VirtualTerminal = new VirtualTerminal() // MATT
    override def loc: SourceLocation = SourceLocation.Unknown // MATT
  }

  // MATT docs
  case class IllegalDerivation(sym: Symbol.ClassSym, loc: SourceLocation) extends ResolutionError {
    override def summary: String = "" // MATT
    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }

  /**
    * Removes all access modifiers from the given string `s`.
    */
  private def stripAccessModifier(s: String): String =
    s.replace("public", "").
      replace("protected", "").
      replace("private", "")

}
