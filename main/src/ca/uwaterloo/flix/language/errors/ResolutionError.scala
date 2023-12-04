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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, UnkindedType}
import ca.uwaterloo.flix.util.Formatter

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends CompilationMessage {
  val kind = "Resolution Error"
}

object ResolutionError {

  // TODO: Support formatting of ill-kinded types.

  /**
    * An error raise to indicate a cycle in the class hierarchy.
    *
    * @param path the super class path from a class to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicClassHierarchy(path: List[Symbol.ClassSym], loc: SourceLocation) extends ResolutionError with Unrecoverable {
    private val fullCycle = path.last :: path

    override def summary: String = {
      val pathString = fullCycle.map(clazz => s"'${clazz.name}'").mkString(" extends ")
      "Cyclic inheritance: " + pathString
    }

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |${code(loc, "cyclic inheritance.")}
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
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raise to indicate a cycle in type aliases.
    *
    * @param path the type reference path from a type alias to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicTypeAliases(path: List[Symbol.TypeAliasSym], loc: SourceLocation) extends ResolutionError with Unrecoverable {
    private val fullCycle = path.last :: path

    def summary: String = {
      val pathString = fullCycle.map(alias => s"'${alias.name}'").mkString(" references ")
      "Cyclic type aliases: " + pathString
    }

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |${code(loc, "Cyclic type aliases.")}
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
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a duplicate associated type definition.
    *
    * @param sym  the associated type symbol.
    * @param loc1 the location of the first associated type definition.
    * @param loc2 the location of the second associated type definition.
    */
  case class DuplicateAssocTypeDef(sym: Symbol.AssocTypeSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError with Recoverable {
    override def summary: String = s"Duplicate associated type definition: $sym."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate associated type definition: ${red(sym.name)}.
         |
         |${code(loc2, "duplicate associated type definition.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None

    val loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate a duplicate derivation.
    *
    * @param sym  the class symbol of the duplicate derivation.
    * @param loc1 the location of the first occurrence.
    * @param loc2 the location of the second occurrence.
    */
  case class DuplicateDerivation(sym: Symbol.ClassSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError with Recoverable {
    override def summary: String = s"Duplicate derivation: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate derivation '${red(sym.name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    override def loc: SourceLocation = loc1

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove one of the occurrences."
    })

  }

  /**
    * An error raised to indicate that an associated type application is not allowed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalAssocTypeApplication(loc: SourceLocation) extends ResolutionError with Unrecoverable {
    override def summary: String = " Illegal associated type application."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal associated type application.
         |
         |${code(loc, "illegal associated type application.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "An associated type may only be applied to a variable."
    })
  }

  /**
    * Illegal Non-Java Type Error.
    *
    * @param tpe the illegal type.
    * @param loc the location where the error occurred.
    */
  case class IllegalNonJavaType(tpe: UnkindedType, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Illegal non-Java type. Expected class or interface type."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected non-Java type: '${red(tpe.toString)}'.
         |
         |${code(loc, "unexpected type.")}
         |
         |Expected a Java class or interface.
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a signature does not include the class's type parameter.
    *
    * @param sym the symbol of the signature.
    * @param loc the location where the error occurred.
    */
  case class IllegalSignature(sym: Symbol.SigSym, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Unexpected signature which does not mention the type variable of the class."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected signature '${red(sym.name)}' which does not mention the type variable of the class.
         |
         |${code(loc, "unexpected signature.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Every signature in a type class must mention the type variable of the class."
    })

  }

  /**
    * Illegal Type Error.
    *
    * @param tpe the illegal type.
    * @param loc the location where the error occurred.
    */
  case class IllegalType(tpe: UnkindedType, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = "Illegal type."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal type: '${red(tpe.toString)}'.
         |
         |${code(loc, "illegal type.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a wildcard type is used in an illegal position.
    *
    * @param ident the name of the wildcard type.
    * @param loc   the location where the error occurred.
    */
  case class IllegalWildType(ident: Name.Ident, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Illegal wildcard type: '$ident'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal wildcard type: '$ident'.
         |
         |${code(loc, "illegal wildcard type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Wildcard types (types starting with an underscore) are not allowed in this position."
    })
  }

  /**
    * Inaccessible Class Error.
    *
    * @param sym the class symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleClass(sym: Symbol.ClassSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Class '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible class.")}
         |
         |""".stripMargin

    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the class as public."
    })
  }

  /**
    * Inaccessible Def Error.
    *
    * @param sym the def symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleDef(sym: Symbol.DefnSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Definition '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible definition.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Effect Error.
    *
    * @param sym the effect symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEffect(sym: Symbol.EffectSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Inaccessible alias ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Effect '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible effect.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the effect as public."
    })

  }

  /**
    * Inaccessible Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEnum(sym: Symbol.EnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Enum '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible enum.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Op Error.
    *
    * @param sym the sig symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleOp(sym: Symbol.OpSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Operation '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible operation.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the operation as public."
    })

  }

  /**
    * Inaccessible Restrictable Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleRestrictableEnum(sym: Symbol.RestrictableEnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Enum '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible enum.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Sig Error.
    *
    * @param sym the sig symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleSig(sym: Symbol.SigSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Definition '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible definition.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Type Alias Error.
    *
    * @param sym the type alias symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleTypeAlias(sym: Symbol.TypeAliasSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Inaccessible type alias ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Type alias '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible type alias.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Mark the type alias as public."
    })

  }

  /**
    * An error raised to indicate that the method return type doesn't match.
    *
    * @param className    the class name.
    * @param methodName   the method name.
    * @param declaredType the declared type.
    * @param expectedType the expected type.
    * @param loc          the location of the method name.
    */
  case class MismatchedReturnType(className: String, methodName: String, declaredType: UnkindedType, expectedType: UnkindedType, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Mismatched return type."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         | >> Mismatched return type for method '${red(methodName)}' in class '${cyan(className)}'.
         |
         |${code(loc, "mismatched return type.")}
         |
         |Declared type: $declaredType
         |Expected type: $expectedType
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a missing associated type definition.
    *
    * @param name the name of the missing associated type definition.
    * @param loc  the location of the instance symbol where the error occurred.
    */
  case class MissingAssocTypeDef(name: String, loc: SourceLocation) extends ResolutionError with Recoverable {
    override def summary: String = s"Missing associated type definition: $name."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Missing associated type definition: $name.
         |
         |${code(loc, s"missing associated type definition: $name.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Sealed Class Error.
    *
    * @param sym the class symbol.
    * @param ns  the namespace from which the class is sealed.
    * @param loc the location where the error occurred.
    */
  case class SealedClass(sym: Symbol.ClassSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = "Sealed."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Class '${red(sym.toString)}' is sealed from the module '${cyan(ns.toString)}'.
         |
         |${code(loc, "sealed class.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Move the instance or sub class to the class's module."
    })

  }

  /**
    * Undefined associated type error.
    *
    * @param qn  associated type.
    * @param loc the location where the error occurred.
    */
  case class UndefinedAssocType(qn: Name.QName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined associated type: '$qn'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined associated type'${red(qn.toString)}'.
         |
         |${code(loc, "associated type not found.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent associated type?"
    })
  }

  /**
    * Undefined Class Error.
    *
    * @param qn  the unresolved class.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedClass(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined class: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined class '${red(qn.toString)}'.
         |
         |${code(loc, "class not found")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent class?"
    })

  }

  /**
    * Undefined Effect Error.
    *
    * @param qn  the unresolved effect.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedEffect(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined effect '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined effect '${red(qn.toString)}'.
         |
         |${code(loc, "effect not found")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent effect?"
    })

  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param msg  the Java error message.
    * @param loc  the location of the class name.
    */
  case class UndefinedJvmClass(name: String, msg: String, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Undefined Java class: '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined Java class '${red(name)}'.
         |
         |${code(loc, "undefined class.")}
         |
         |$msg
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = {
      if (raw".*\.[A-Z].*\.[A-Z].*".r matches name)
        Some(s"Static nested classes should be specified using '$$', e.g. java.util.Locale$$Builder")
      else
        None
    }
  }

  /**
    * An error raised to indicate that a matching constructor was not found.
    *
    * @param clazz        the class name.
    * @param signature    the signature of the constructor.
    * @param constructors the constructors in the class.
    * @param loc          the location of the constructor name.
    */
  case class UndefinedJvmConstructor(clazz: Class[_], signature: List[Class[_]], constructors: List[Constructor[_]], loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = "Undefined constructor."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined constructor in class '${cyan(clazz.getName)}' with the given signature.
         |
         |${code(loc, "undefined constructor.")}
         |
         |No constructor matches the signature:
         |  $clazz (${signature.map(_.toString).mkString(",")})
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
    def explain(formatter: Formatter): Option[String] = None
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
  case class UndefinedJvmField(className: String, fieldName: String, static: Boolean, fields: List[Field], loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = {
      if (!static) {
        s"Undefined object field."
      } else {
        s"Undefined static field."
      }
    }

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined ${magenta(keyword)} field '${red(fieldName)}' in class '${cyan(className)}'.
         |
         |${code(loc, "undefined field.")}
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
    def explain(formatter: Formatter): Option[String] = None
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
  case class UndefinedJvmMethod(className: String, methodName: String, static: Boolean, signature: List[Class[_]], methods: List[Method], loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = {
      if (!static) {
        s"Undefined object method."
      } else {
        s"Undefined static method."
      }
    }

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined ${magenta(keyword)} method '${red(methodName)}' in class '${cyan(className)}'.
         |
         |${code(loc, "undefined method.")}
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
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Undefined Kind Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedKind(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Undefined kind: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined kind '${red(qn.toString)}'.
         |
         |${code(loc, "undefined kind.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent kind?"
    })
  }

  /**
    * Undefined Name Error.
    *
    * @param qn    the unresolved name.
    * @param ns    the current namespace.
    * @param env   the variables in the scope.
    * @param isUse true if the undefined name occurs in a use.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedName(qn: Name.QName, ns: Name.NName, env: Map[String, Symbol.VarSym], isUse: Boolean, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined name: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined name '${red(qn.toString)}'.
         |
         |${code(loc, "name not found")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent definition?"
    })

  }

  /**
    * Undefined Op Error.
    *
    * @param qname the qualified name of the operation.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedOp(qname: Name.QName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined operation '${qname.toString}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined operation '${red(qname.toString)}'.
         |
         |${code(loc, "operation not found")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent operation?"
    })
  }

  /**
    * Undefined Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Undefined tag: '$tag'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined tag '${red(tag)}'.
         |
         |${code(loc, "tag not found.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent tag?"
    })

  }

  /**
    * Undefined Restrictable Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRestrictableTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined restrictable tag: '$tag'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined restrictable tag '${red(tag)}'.
         |
         |${code(loc, "tag not found.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent tag?"
    })

  }

  /**
    * Undefined Type Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined type: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined type '${red(qn.toString)}'.
         |
         |${code(loc, "type not found.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Possible typo or non-existent type?"
    })

  }

  /**
    * An error raised to indicate that the type variable was not found.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the undefined type variable.
    */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends ResolutionError with Recoverable {
    def summary: String = s"Undefined type variable '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined type variable '${red(name)}'.
         |
         |${code(loc, "undefined type variable.")}
         |""".stripMargin

    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Flix cannot find the type variable. Maybe there is a typo?"
    })
  }

  /**
    * An error raised to indicate that the local variable was not found.
    *
    * @param name the name of the variable.
    * @param loc  the location of the undefined variable.
    */
  case class UndefinedVar(name: String, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    def summary: String = s"Undefined variable '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined variable '${red(name)}'.
         |
         |${code(loc, "undefined variable.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Flix cannot find the variable. Maybe there is a typo?"
    })
  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the associated type.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedAssocType(sym: Symbol.AssocTypeSym, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    override def summary: String = s"Under-applied associated type: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Under-applied associated type '${red(sym.name)}'.
         |
         |${code(loc, "Under-applied associated type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Associated types must be fully applied."
    })

  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the type alias.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends ResolutionError with Unrecoverable {
    override def summary: String = s"Under-applied type alias: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Under-applied type alias '${red(sym.name)}'.
         |
         |${code(loc, "Under-applied type alias.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type aliases must be fully applied."
    })

  }

  /**
    * Removes all access modifiers from the given string `s`.
    */
  private def stripAccessModifier(s: String): String =
    s.replace("public", "").
      replace("protected", "").
      replace("private", "")

}
