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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatType, FormatTypeConstraint}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for redundancy errors.
  */
trait RedundancyError extends CompilationMessage {
  val kind: String = "Redundancy Error"
}

object RedundancyError {

  private implicit val audience: Audience = Audience.External

  /**
    * An error raised to indicate that the variable symbol `sym` is hidden.
    *
    * @param sym the hidden variable symbol.
    * @param loc the source location of the use.
    */
  case class HiddenVarSym(sym: Symbol.VarSym, loc: SourceLocation) extends RedundancyError {
    def summary: String = "Hidden variable symbol."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Hidden variable symbol '${red(sym.text)}'. The symbol is marked as unused.
         |
         |${code(loc, "hidden symbol.")}
         |""".stripMargin

    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Don't use the variable symbol.
         |  (2)  Rename the underscore prefix from the variable symbol name.
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that a variable has been shadowed.
    *
    * @param sym1 the shadowed variable.
    * @param sym2 the shadowing variable.
    */
  case class ShadowedVar(sym1: Symbol.VarSym, sym2: Symbol.VarSym) extends RedundancyError {
    def summary: String = "Shadowed variable."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Shadowed variable '${red(sym1.text)}'.
         |
         |${code(sym2.loc, "shadowing variable.")}
         |
         |The shadowed variable was declared here:
         |
         |${code(sym1.loc, "shadowed variable.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

    def loc: SourceLocation = sym1.loc min sym2.loc
  }

  /**
    * An error raised to indicate that the def with the symbol `sym` is not used.
    *
    * @param sym the unused enum symbol.
    */
  case class UnusedDefSym(sym: Symbol.DefnSym) extends RedundancyError {
    def summary: String = "Unused definition."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unused definition '${red(sym.name)}'. The definition is never referenced.
         |
         |${code(sym.loc, "unused definition.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""Possible fixes:
         |
         |  (1)  Use the definition.
         |  (2)  Remove the definition.
         |  (3)  Mark the definition as public.
         |  (4)  Prefix the definition name with an underscore.
         |
         |""".stripMargin
    })

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the enum with the symbol `sym` is not used.
    *
    * @param sym the unused enum symbol.
    */
  case class UnusedEnumSym(sym: Symbol.EnumSym) extends RedundancyError {
    def summary: String = "Unused enum."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unused enum '${red(sym.name)}'. Neither the enum nor its cases are ever used.
         |
         |${code(sym.loc, "unused enum.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Use the enum.
         |  (2)  Remove the enum.
         |  (3)  Mark the enum as public.
         |  (4)  Prefix the enum name with an underscore.
         |
         |""".stripMargin
    })

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that in the enum with symbol `sym` the case `tag` is not used.
    *
    * @param sym the enum symbol.
    * @param tag the unused tag.
    */
  case class UnusedEnumTag(sym: Symbol.EnumSym, tag: Name.Tag) extends RedundancyError {
    def summary: String = s"Unused case '${tag.name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unused case '${red(tag.name)}' in enum '${cyan(sym.name)}'.
         |
         |${code(tag.loc, "unused tag.")}
         |""".stripMargin

    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Use the case.
         |  (2)  Remove the case.
         |  (3)  Prefix the case with an underscore.
         |
         |""".stripMargin
    })

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the given formal parameter symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedFormalParam(sym: Symbol.VarSym) extends RedundancyError {
    def summary: String = "Unused formal parameter."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unused formal parameter '${red(sym.text)}'. The parameter is not used within its scope.
         |
         |${code(sym.loc, "unused formal parameter.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Use the formal parameter.
         |  (2)  Remove the formal parameter.
         |  (3)  Prefix the formal parameter name with an underscore.
         |
         |""".stripMargin
    })

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the given type parameter `ident` is not used.
    *
    * @param ident the unused type variable.
    */
  case class UnusedTypeParam(ident: Name.Ident) extends RedundancyError {
    def summary: String = "Unused type parameter."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unused type parameter '${red(ident.name)}'. The parameter is not referenced anywhere.
         |
         |${code(ident.loc, "unused type parameter.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Use the type parameter.
         |  (2)  Remove type parameter.
         |  (3)  Prefix the type parameter name with an underscore.
         |
         |""".stripMargin
    })

    def loc: SourceLocation = SourceLocation.mk(ident.sp1, ident.sp2)
  }

  /**
    * An error raised to indicate that the given variable symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedVarSym(sym: Symbol.VarSym) extends RedundancyError {
    def summary: String = "Unused local variable."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unused local variable '${red(sym.text)}'. The variable is not referenced within its scope.
         |
         |${code(sym.loc, "unused local variable.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Use the local variable.
         |  (2)  Remove local variable declaration.
         |  (3)  Prefix the variable name with an underscore.
         |
         |""".stripMargin
    })

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the given variable symbol `sym` is only used once in a constraint.
    *
    * @param sym the variable only used once
    * @param loc the location of the error
    */
  case class IllegalSingleVariable(sym: Symbol.VarSym, loc: SourceLocation) extends RedundancyError {
    def summary: String = s"Single use of variable '$sym'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> This variable is named, but only used once '${red(sym.text)}'. Use a wildcard instead?
         |
         |${code(loc, "the variable occurs here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Prefix the variable name with a wildcard.
         |  (2)  Replace the variable name with a wildcard.
         |  (3)  Check for any spelling mistakes.
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that an expression is useless.
    *
    * @param tpe the type of the expression.
    * @param loc the location of the expression.
    */
  case class UselessExpression(tpe: Type, loc: SourceLocation) extends RedundancyError {
    def summary: String = "Useless expression."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Useless expression: It has no side-effect(s) and its result is discarded.
         | The expression has type '${FormatType.formatWellKindedType(tpe)}'
         |
         |${code(loc, "useless expression.")}
         |
         |  The expression has type '${FormatType.formatWellKindedType(tpe)}'
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Use the result computed by the expression.
         |  (2)  Remove the expression statement.
         |  (3)  Introduce a let-binding with a wildcard name.
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that an impure function expression is useless
    * is statement position.
    *
    * @param tpe the type of the expression.
    * @param loc the location of the expression.
    */
  case class UnderAppliedFunction(tpe: Type, loc: SourceLocation) extends RedundancyError {
    def summary: String = "Under applied function. Missing function argument(s)?"

    private def applicationAdvice(tpe: Type): String = tpe.typeConstructor match {
      case Some(TypeConstructor.Arrow(arity)) if arity > 0 =>
        val typeStrings = tpe.arrowArgTypes.map(FormatType.formatWellKindedType).mkString(", ")
        val pluralS = if (arity == 1) "s" else ""
        s"Missing argument$pluralS of type: $typeStrings"
      case _ => "Missing function argument(s)?"
    }

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Under applied function. ${applicationAdvice(tpe)}
         |
         |
         |${code(loc, "the function is not fully-applied and hence has no effect.")}
         |
         |  The function has type '${FormatType.formatWellKindedType(tpe)}'
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Give the function (additional) arguments.
         |  (2)  Use the result computed by the expression.
         |  (3)  Remove the expression statement.
         |  (4)  Introduce a let-binding with a wildcard name.
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that a purity cast is redundant.
    *
    * @param loc the source location of the cast.
    */
  case class RedundantPurityCast(loc: SourceLocation) extends RedundancyError {
    def summary: String = "Redundant purity cast. The expression is already pure."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Redundant purity cast. The expression is already pure.
         |
         |${code(loc, "redundant cast.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an effect cast is redundant.
    *
    * @param loc the source location of the cast.
    */
  case class RedundantEffectCast(loc: SourceLocation) extends RedundancyError {
    def summary: String = "Redundant effect cast. The expression already has the same effect."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Redundant effect cast. The expression already has the same effect.
         |
         |${code(loc, "redundant cast.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a redundant type constraint.
    *
    * @param entailingTconstr the tconstr that entails the other.
    * @param redundantTconstr the tconstr that is made redundant by the other.
    * @param loc              the location where the error occured.
    */
  case class RedundantTypeConstraint(entailingTconstr: Ast.TypeConstraint, redundantTconstr: Ast.TypeConstraint, loc: SourceLocation) extends RedundancyError {
    def summary: String = "Redundant type constraint."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Type constraint '${red(FormatTypeConstraint.formatTypeConstraint(redundantTconstr))}' is entailed by type constraint '${green(FormatTypeConstraint.formatTypeConstraint(redundantTconstr))}'.
         |
         |${code(loc, "redundant type constraint.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Remove the type constraint.
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that the result of a pure expression is discarded.
    *
    * @param loc the location of the inner expression.
    */
  case class DiscardedPureValue(loc: SourceLocation) extends RedundancyError {
    def summary: String = "A pure expression cannot be discarded."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> A pure expression cannot be discarded.
         |
         |${code(loc, "pure expression.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a redundant discard of a unit value.
    *
    * @param loc the location of the inner expression.
    */
  case class RedundantDiscard(loc: SourceLocation) extends RedundancyError {
    def summary: String = "Redundant discard of unit value."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Redundant discard of unit value.
         |
         |${code(loc, "discarded unit value.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

}
