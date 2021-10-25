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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.Formatter

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends CompilationMessage {
  val kind = "Resolution Error"

  protected def appendLocations(locs: List[SourceLocation], s: String)(implicit formatter: Formatter): String = {
    locs.map(l => formatter.code(l, s)).mkString(System.lineSeparator() + System.lineSeparator())
  }
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


    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Ambiguous name '${formatter.red(qn.toString)}' Name refers to multiple definitions.
         |
         |${formatter.code(loc, "ambiguous name.")}
         |
         |${appendLocations(locs, "definition/effect/signature matches.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Ambiguous type '${formatter.red(qn)}'. Name refers to multiple types.
         |
         |${formatter.code(loc, "ambiguous type.")}
         |
         |${appendLocations(locs, "type matches.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Ambiguous tag '${formatter.red(tag)}'.
         |
         |${formatter.code(loc, "ambiguous tag name.")}
         |
         |The tag is defined in multiple enums:
         |
         |${appendLocations(locs, "tag is defined in this enum.")}
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Prefix the tag with the enum name.")

  }

  /**
    * Illegal Type Error.
    *
    * @param tpe the illegal type.
    * @param loc the location where the error occurred.
    */
  case class IllegalType(tpe: Type, loc: SourceLocation) extends ResolutionError {

    def summary: String = "Illegal type."

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Illegal type: '${formatter.red(FormatType.formatType(tpe))}'.
         |
         |${formatter.code(loc, "illegal type.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Class '${formatter.red(sym.toString)}' is not accessible from the namespace '${formatter.cyan(ns.toString)}'.
         |
         |${formatter.code(loc, "inaccessible class.")}
         |
         |""".stripMargin

    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Mark the class as public.")
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Class '${formatter.red(sym.toString)}' is sealed from the namespace '${formatter.cyan(ns.toString)}'.
         |
         |${formatter.code(loc, "sealed class.")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Move the instance or sub class to the class's namespace.")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Definition '${formatter.red(sym.toString)}' is not accessible from the namespace '${formatter.cyan(ns.toString)}'.
         |
         |${formatter.code(loc, "inaccessible definition.")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Mark the definition as public.")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Definition '${formatter.red(sym.toString)}' is not accessible from the namespace '${formatter.cyan(ns.toString)}'.
         |
         |${formatter.code(loc, "inaccessible definition.")}
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Mark the definition as public.")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Enum '${formatter.red(sym.toString)}' is not accessible from the namespace '${formatter.cyan(ns.toString)}'.
         |
         |${formatter.code(loc, "inaccessible enum.")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Mark the definition as public.")

  }

  /**
    * Inaccessible Type Alias Error.
    *
    * @param sym the type alias symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleTypeAlias(sym: Symbol.TypeAliasSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Inaccessible type alias ${sym.name}"

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Type alias '${formatter.red(sym.toString)}' is not accessible from the namespace '${formatter.cyan(ns.toString)}'.
         |
         |${formatter.code(loc, "inaccessible type alias.")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Mark the definition as public.")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Recursion limit ($limit) reached while unfolding the '${formatter.red(ident.name)}' type alias.
         |
         |${formatter.code(loc, "recursion limit reached.")}
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some("Ensure that there is no cyclic definition of type aliases.")

  }

  /**
    * An error raise to indicate a cycle in type aliases.
    *
    * @param path the type reference path from a type alias to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicTypeAliases(path: List[Symbol.TypeAliasSym], loc: SourceLocation) extends ResolutionError {
    private val fullCycle = path.last :: path

    def summary: String = {
      val pathString = fullCycle.map(alias => s"'${alias.name}'").mkString(" references ")
      "Cyclic type aliases: " + pathString
    }

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |
         |${formatter.code(loc, "Cyclic type aliases.")}
         |
         |The following type aliases are in the cycle:
         |$appendCycles
         |""".stripMargin
    }

    private def appendCycles: String = {
      var res = ""
      for (List(subClass, superClass) <- fullCycle.sliding(2)) {
        res += s"$subClass references $superClass" + System.lineSeparator()
      }
      res
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined name '${formatter.red(qn.toString)}'.
         |
         |${formatter.code(loc, "name not found")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Possible typo or non-existent definition?")

  }

  /**
    * Undefined Sig Error.
    *
    * @param clazz the class.
    * @param sig   the unresolved sig.
    * @param ns    the current namespace.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedSig(clazz: Name.QName, sig: Name.Ident, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Undefined signature."

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined signature '${formatter.red(sig.name)}' in class '${formatter.red(clazz.toString)}'.
         |
         |${formatter.code(loc, "signature not found")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Possible typo or non-existent class or signature?")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined class '${formatter.red(qn.toString)}'.
         |
         |${formatter.code(loc, "class not found")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Possible typo or non-existent class?")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined tag '${formatter.red(tag)}'.
         |
         |${formatter.code(loc, "tag not found.")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Possible typo or non-existent tag?")

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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined type '${formatter.red(qn.toString)}'.
         |
         |${formatter.code(loc, "type not found.")}
         |
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Possible typo or non-existent type?")

  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedJvmClass(name: String, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Undefined class."

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined class '${formatter.red(name)}'.
         |
         |${formatter.code(loc, "undefined class.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined constructor in class '${formatter.cyan(className)}' with the given signature.
         |
         |${formatter.code(loc, "undefined constructor.")}
         |No constructor matches the signature:
         |  $className (${signature.map(_.toString).mkString(",")})
         |
         |Available constructors:
         |$appendConstructors
         |""".stripMargin
    }


    private def appendConstructors: String = {
      var res = ""
      for (constructor <- constructors) {
        res += "  " + stripAccessModifier(constructor.toString) + System.lineSeparator()
      }
      res
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined ${formatter.magenta(keyword)} method '${formatter.red(methodName)}' in class '${formatter.cyan(className)}'.
         |
         |${formatter.code(loc, "undefined method.")}
         |No method matches the signature:
         |  $methodName(${signature.map(_.toString).mkString(",")})
         |
         |
         |Available methods:
         |$appendMethods
         |""".stripMargin
    }

    private def keyword: String = {
      if (static) "static" else "object"
    }

    private def appendMethods: String = {
      methods.map(m => "  " + stripAccessModifier(m.toString) + System.lineSeparator()).mkString
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
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

    def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined ${formatter.magenta(keyword)} field '${formatter.red(fieldName)}' in class '${formatter.cyan(className)}'.
         |
         |${formatter.code(loc, "undefined field.")}
         |Available fields:
         |$appendFields
         |""".stripMargin
    }

    private def keyword: String = {
      if (static) "static" else "object"
    }

    private def appendFields: String = {
      fields.map(f => "  " + stripAccessModifier(f.toString) + System.lineSeparator()).mkString
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
  }

  /**
    * An error raise to indicate a cycle in the class hierarchy.
    *
    * @param path the super class path from a class to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicClassHierarchy(path: List[Symbol.ClassSym], loc: SourceLocation) extends ResolutionError {
    private val fullCycle = path.last :: path

    override def summary: String = {
      val pathString = fullCycle.map(clazz => s"'${clazz.name}'").mkString(" extends ")
      "Cyclic inheritance: " + pathString
    }

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |
         |${formatter.code(loc, "Cyclic inheritance.")}
         |
         |The following classes are in the cycle:
         |
         |$cyclicClasses
         |""".stripMargin
    }

    private def cyclicClasses: String = {
      var res = ""
      for (List(subClass, superClass) <- fullCycle.sliding(2)) {
        res += s"$subClass extends $superClass" + System.lineSeparator()
      }
      res
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a duplicate derivation.
    *
    * @param sym  the class symbol of the duplicate derivation.
    * @param loc1 the location of the first occurrence.
    * @param loc2 the location of the second occurrence.
    */
  case class DuplicateDerivation(sym: Symbol.ClassSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError {
    override def summary: String = s"Duplicate derivation: ${sym.name}"

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate derivation '${formatter.red(sym.name)}'.
         |
         |${formatter.code(loc1, "the first occurrence was here.")}
         |
         |${formatter.code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    override def loc: SourceLocation = loc1

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Remove one of the occurrences.")

  }

  /**
    * An error raised to indicate an illegal derivation.
    *
    * @param sym       the class symbol of the illegal derivation.
    * @param legalSyms the list of class symbols of legal derivations.
    * @param loc       the location where the error occurred.
    */
  case class IllegalDerivation(sym: Symbol.ClassSym, legalSyms: List[Symbol.ClassSym], loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Illegal derivation: ${sym.name}"

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Illegal derivation '${formatter.red(sym.name)}'.
         |
         |${formatter.code(loc, "Illegal derivation.")}
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Only the following classes may be derived: ${legalSyms.map(_.name).mkString(", ")}.")

  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the type alias.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Under-applied type alias: ${sym.name}"

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Under-applied type alias '${formatter.red(sym.name)}'.
         |
         |${formatter.code(loc, "Under-applied type alias.")}
         |""".stripMargin
    }

    override def explain(implicit formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Type aliases must be fully applied.")

  }

  /**
    * Removes all access modifiers from the given string `s`.
    */
  private def stripAccessModifier(s: String): String =
    s.replace("public", "").
      replace("protected", "").
      replace("private", "")

}
