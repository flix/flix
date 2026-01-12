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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.shared.TraitConstraint
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.{FormatTraitConstraint, FormatType}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for redundancy errors.
  */
trait RedundancyError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.RedundancyError
}

object RedundancyError {

  /**
    * An error raised to indicate that the result of a pure expression is discarded.
    *
    * @param loc the location of the expression.
    */
  case class DiscardedPureExpression(loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E6736

    def summary: String = "Discarded pure expression."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Discarded pure expression.
         |
         |${src(loc, "discarded pure expression.")}
         |
         |${underline("Explanation:")} Discarding the result of a pure expression is pointless.
         |It means the expression itself might as well be removed.
         |
         |If you want to keep the expression, use:
         |
         |    let _ = <exp>
         |
         |although the compiler will remove the expression during code generation.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the extensible variant constructor `label` was used multiple times.
    *
    * @param label the name of the extensible variant constructor.
    * @param loc1  the location of the first pattern.
    * @param loc2  the location of the second pattern.
    */
  case class DuplicateExtPattern(label: Name.Label, loc1: SourceLocation, loc2: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E6843

    def summary: String = s"Duplicate extensible pattern '${label.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate extensible pattern '${red(label.name)}'.
         |
         |${src(loc1, "first occurrence.")}
         |
         |${src(loc2, "duplicate occurrence.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the variable symbol `sym` is hidden.
    *
    * @param sym the hidden variable symbol.
    * @param loc the source location of the use.
    */
  case class HiddenVarSym(sym: Symbol.VarSym, loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E6956

    def summary: String = s"Hidden variable symbol '${sym.text}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Hidden variable symbol '${red(sym.text)}'.
         |
         |${src(loc, "hidden symbol.")}
         |
         |${underline("Explanation:")} A hidden variable symbol cannot be accessed.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a checked effect cast is redundant.
    *
    * @param eff the effect of the expression.
    * @param loc the source location of the cast.
    */
  case class RedundantCheckedEffectCast(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7067

    def summary: String = "Redundant effect cast."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redundant effect cast.
         |
         |${src(loc, "redundant cast.")}
         |
         |The expression already has the '${cyan(FormatType.formatType(eff))}' effect.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a checked type cast is redundant.
    *
    * @param tpe the type of the expression.
    * @param loc the source location of the cast.
    */
  case class RedundantCheckedTypeCast(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7178

    def summary: String = "Redundant type cast."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redundant type cast.
         |
         |${src(loc, "redundant cast.")}
         |
         |The expression already has the type '${cyan(FormatType.formatType(tpe))}'.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a redundant discard of a unit value.
    *
    * @param loc the location of the inner expression.
    */
  case class RedundantDiscard(loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7281

    def summary: String = "Redundant discard of unit value."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redundant discard of unit value.
         |
         |${src(loc, "discarded unit value.")}
         |
         |${underline("Explanation:")} Discarding a unit value is redundant since unit
         |has no meaningful value to discard.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a redundant trait constraint.
    *
    * @param entailingTconstr the tconstr that entails the other.
    * @param redundantTconstr the tconstr that is made redundant by the other.
    * @param loc              the location where the error occured.
    */
  case class RedundantTraitConstraint(entailingTconstr: TraitConstraint, redundantTconstr: TraitConstraint, loc: SourceLocation)(implicit flix: Flix) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7394

    def summary: String = "Redundant trait constraint."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redundant trait constraint '${red(FormatTraitConstraint.formatTraitConstraint(redundantTconstr))}'.
         |
         |${src(loc, "redundant trait constraint.")}
         |
         |The constraint is entailed by '${cyan(FormatTraitConstraint.formatTraitConstraint(entailingTconstr))}'.
         |
         |${underline("Explanation:")} A trait constraint is redundant if it is implied by another
         |constraint. For example, if we have:
         |
         |    def foo(x: a): a with Order[a], Eq[a] = ...
         |
         |then the 'Eq[a]' constraint is redundant because 'Order[a]' already implies 'Eq[a]'.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an effect cast is redundant.
    *
    * @param loc the source location of the cast.
    */
  case class RedundantUncheckedEffectCast(loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7407

    def summary: String = "Redundant effect cast. The expression is already pure."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redundant effect cast. The expression is already pure.
         |
         |${src(loc, "redundant cast.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that unsafe was used on a pure expression.
    *
    * @param eff the effect that the block unsafely removes.
    * @param loc the source location of the unsafe block.
    */
  case class RedundantUnsafe(eff: Type, loc: SourceLocation)(implicit flix: Flix) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7623

    def summary: String = "Redundant unsafe block, the expression is pure."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redundant unsafe block, the expression is pure.
         |
         |${src(loc, "redundant unsafe block.")}
         |
         |${underline("Explanation:")} The block unsafely removes the '${cyan(FormatType.formatType(eff))}' effect,
         |but the body expression is pure.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a name has been shadowed.
    *
    * @param shadowed  the shadowed name.
    * @param shadowing the shadowing name.
    */
  case class ShadowedName(name: String, shadowed: SourceLocation, shadowing: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7736

    def summary: String = s"Shadowed name '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Shadowed name '${red(name)}'.
         |
         |${src(shadowed, "shadowed name.")}
         |
         |The shadowing name was declared here:
         |
         |${src(shadowing, "shadowing name.")}
         |""".stripMargin
    }

    def loc: SourceLocation = shadowed
  }

  /**
    * An error raised to indicate that a name is shadowing another name.
    *
    * @param shadowed  the shadowed name.
    * @param shadowing the shadowing name.
    */
  case class ShadowingName(name: String, shadowed: SourceLocation, shadowing: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7843

    def summary: String = s"Shadowing name '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Shadowing name '${red(name)}'.
         |
         |${src(shadowing, "shadowing name.")}
         |
         |The shadowed name was declared here:
         |
         |${src(shadowed, "shadowed name.")}
         |""".stripMargin
    }

    def loc: SourceLocation = shadowing
  }

  /**
    * An error raised to indicate that the def with the symbol `sym` is not used.
    *
    * @param sym the unused def symbol.
    */
  case class UnusedDefSym(sym: Symbol.DefnSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7956

    def summary: String = s"Unused definition '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused definition '${red(sym.name)}'. The definition is never referenced.
         |
         |${src(sym.loc, "unused definition.")}
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the effect with the symbol `sym` is not used.
    *
    * @param sym the unused effect symbol.
    */
  case class UnusedEffSym(sym: Symbol.EffSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8063

    def summary: String = s"Unused effect '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused effect '${red(sym.name)}'. The effect is never referenced.
         |
         |${src(sym.loc, "unused effect.")}
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the enum with the symbol `sym` is not used.
    *
    * @param sym the unused enum symbol.
    */
  case class UnusedEnumSym(sym: Symbol.EnumSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8176

    def summary: String = s"Unused enum '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused enum '${red(sym.name)}'. Neither the enum nor its cases are ever used.
         |
         |${src(sym.loc, "unused enum.")}
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that in the enum with symbol `sym` the case `tag` is not used.
    *
    * @param sym the enum symbol.
    * @param tag the unused tag.
    */
  case class UnusedEnumTag(sym: Symbol.EnumSym, tag: Symbol.CaseSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8289

    def summary: String = s"Unused case '${tag.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused case '${red(tag.name)}' in enum '${cyan(sym.name)}'.
         |
         |${src(tag.loc, "unused tag.")}
         |""".stripMargin
    }

    def loc: SourceLocation = tag.loc
  }

  /**
    * An error raised to indicate that the struct with the symbol `sym` is not used.
    *
    * @param sym the unused struct symbol.
    */
  case class UnusedStructSym(sym: Symbol.StructSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8392

    def summary: String = s"Unused struct '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused struct '${red(sym.name)}'.
         |
         |${src(sym.loc, "unused struct.")}
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the given formal parameter symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedFormalParam(sym: Symbol.VarSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8405

    def summary: String = s"Unused formal parameter '${sym.text}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused formal parameter '${red(sym.text)}'. The parameter is not used within its scope.
         |
         |${src(sym.loc, "unused formal parameter.")}
         |
         |${underline("Explanation:")} Flix does not allow unused formal parameters to prevent
         |programming bugs. An unused formal parameter can be prefixed with an underscore
         |to suppress this error.
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that the given type parameter `ident` is not used.
    *
    * @param ident the unused type variable.
    */
  case class UnusedTypeParam(ident: Name.Ident, loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8518

    def summary: String = s"Unused type parameter '${ident.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused type parameter '${red(ident.name)}'. The parameter is not referenced anywhere.
         |
         |${src(ident.loc, "unused type parameter.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the given type parameter `ident` is not used in the signature.
    *
    * @param ident the unused type variable.
    */
  case class UnusedTypeParamSignature(ident: Name.Ident, loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8629

    def summary: String = s"Unused type parameter '${ident.name}' in function signature."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused type parameter '${red(ident.name)}'. The parameter is not referenced in the signature.
         |
         |${src(ident.loc, "type parameter unused in function signature.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the given variable symbol `sym` is not used.
    *
    * @param sym the unused variable symbol.
    */
  case class UnusedVarSym(sym: Symbol.VarSym) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8736

    def summary: String = s"Unused local variable '${sym.text}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unused local variable '${red(sym.text)}'. The variable is not referenced within its scope.
         |
         |${src(sym.loc, "unused local variable.")}
         |
         |${underline("Explanation:")} Flix does not allow unused local variables to prevent
         |programming bugs. An unused local variable can be prefixed with an underscore
         |to suppress this error. For example:
         |
         |    let _${sym.text} = <exp>
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc
  }

  /**
    * An error raised to indicate that a case of an `ematch` expression is unreachable due to an earlier default case.
    *
    * @param defaultLoc the location of the default case.
    * @param loc        the location of the unreachable case.
    */
  case class UnreachableExtMatchCase(defaultLoc: SourceLocation, loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8843

    override def summary: String = "Unreachable case."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unreachable case. It is covered by a '_' pattern.
         |
         |${src(loc, "unreachable case.")}
         |
         |Covered by the following pattern:
         |
         |${src(defaultLoc, "covering pattern.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an expression is useless.
    *
    * @param loc the location of the expression.
    */
  case class UselessExpression(loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E8956

    def summary: String = "Useless expression."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Useless expression: It has no side-effect(s) and its result is discarded.
         |
         |${src(loc, "useless expression.")}
         |
         |${underline("Explanation:")} A useless expression has no side-effects and its result
         |is not used. Either use the result or remove the expression.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an `unsafe` block is useless.
    *
    * @param loc the source location of the unsafe block.
    */
  case class UselessUnsafe(loc: SourceLocation) extends RedundancyError {
    def code: ErrorCode = ErrorCode.E7512

    def summary: String = "Useless unsafe block."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Useless unsafe block.
         |
         |${src(loc, "useless unsafe block.")}
         |
         |${underline("Explanation:")} An unsafe block that runs the 'Pure' effect is useless
         |since 'Pure' means no effects.
         |""".stripMargin
    }
  }

}
