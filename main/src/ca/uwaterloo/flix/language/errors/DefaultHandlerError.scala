/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.fmt.FormatType.formatType
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for errors produced by [[ca.uwaterloo.flix.language.phase.typer.DefaultHandlers]].
  */
sealed trait DefaultHandlerError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.DefaultHandlerError
}

object DefaultHandlerError {

  /**
    * Returns the expected signature of the default handler `handlerSym` for the effect `handledEff`.
    *
    * For example: `pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E[t]) + IO`.
    */
  private def expectedSignature(handlerSym: Symbol.DefnSym, handledEff: Type)(implicit flix: Flix): String =
    s"pub def ${handlerSym.name}(f: Unit -> a \\ ef): a \\ (ef - ${formatType(handledEff)}) + IO"

  /**
    * An error raised to indicate that the handled effect does not occur in the effect of a default handler.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param actual     the declared effect of the default handler.
    * @param loc        the location of the declared effect.
    */
  case class DefaultHandlerDoesNotHandleEffect(handlerSym: Symbol.DefnSym, handledEff: Type, actual: Type, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0852

    def summary: String = s"Illegal default handler: '${handlerSym.name}' does not handle '${formatType(handledEff)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler: '${red(handlerSym.name)}' does not handle '${magenta(formatType(handledEff))}'.
         |
         |${highlight(loc, s"does not remove '${formatType(handledEff)}'", fmt)}
         |
         |${underline("Explanation:")} The effect of a default handler must remove the handled effect
         |'${formatType(handledEff)}' from the effect 'ef' of its thunk argument, but '${formatType(handledEff)}'
         |does not occur in the declared effect '${formatType(actual)}'. Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a default handler is not in the companion module of its effect.
    *
    * @param handlerSym the symbol of the default handler.
    * @param loc        the location of the default handler.
    */
  case class DefaultHandlerNotInModule(handlerSym: Symbol.DefnSym, loc: SourceLocation) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0621

    def summary: String = s"Misplaced default handler: '${handlerSym.name}' must be in the companion module of its effect."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Misplaced default handler: '${red(handlerSym.name)}' must be in the companion module of its effect.
         |
         |${highlight(loc, "must be in companion module", fmt)}
         |
         |${underline("Explanation:")} A default handler must be defined inside the companion
         |module of the effect it handles. For example:
         |
         |  pub eff E {
         |      pub def op(): Unit
         |  }
         |
         |  mod E {
         |      @DefaultHandler
         |      pub def runWithIO(f: Unit -> a \\ ef): a \\ (ef - E) + IO = ...
         |  }
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that there are multiple default handlers for the same effect.
    *
    * @param sym  the symbol of the effect.
    * @param loc1 the location of the first default handler.
    * @param loc2 the location of the second default handler.
    */
  case class DuplicateDefaultHandler(sym: Symbol.EffSym, loc1: SourceLocation, loc2: SourceLocation) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0734

    def summary: String = s"Duplicate default handler for effect '${sym.name}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Duplicate default handler for effect '${red(sym.name)}'.
         |
         |${highlight(loc1, "first occurrence", fmt)}
         |
         |${highlight(loc2, "duplicate", fmt)}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that a default handler does not take exactly one argument.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param arity      the number of arguments of the default handler.
    * @param loc        the location of the first extraneous argument.
    */
  case class IllegalDefaultHandlerArity(handlerSym: Symbol.DefnSym, handledEff: Type, arity: Int, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0838

    def summary: String = s"Illegal default handler: '${handlerSym.name}' must take exactly one argument, but takes $arity."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler: '${red(handlerSym.name)}' must take exactly one argument, but takes ${red(arity.toString)}.
         |
         |${highlight(loc, "extraneous argument", fmt)}
         |
         |${underline("Explanation:")} A default handler takes a single thunk argument of type 'Unit -> a \\ ef'.
         |Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a default handler has a trait or equality constraint.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param loc        the location of the constraint.
    */
  case class IllegalDefaultHandlerConstraint(handlerSym: Symbol.DefnSym, handledEff: Type, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0871

    def summary: String = s"Illegal default handler: '${handlerSym.name}' must not have trait or equality constraints."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler: '${red(handlerSym.name)}' must not have trait or equality constraints.
         |
         |${highlight(loc, "illegal constraint", fmt)}
         |
         |${underline("Explanation:")} A default handler must be applicable to every entry point, i.e. for
         |every result type 'a', every effect 'ef', and every type argument of the handled effect.
         |Hence its signature cannot be constrained. Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the effect of a default handler is not `(ef - E) + IO`.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param expected   the expected effect.
    * @param actual     the declared effect.
    * @param loc        the location of the declared effect.
    */
  case class IllegalDefaultHandlerEffect(handlerSym: Symbol.DefnSym, handledEff: Type, expected: Type, actual: Type, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0864

    def summary: String = s"Illegal default handler effect: expected '${formatType(expected)}', found '${formatType(actual)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler effect: expected '${cyan(formatType(expected))}', found '${red(formatType(actual))}'.
         |
         |${highlight(loc, "illegal effect", fmt)}
         |
         |${underline("Explanation:")} A default handler must remove the handled effect '${formatType(handledEff)}'
         |from the effect 'ef' of its thunk argument, and it may only introduce the 'IO' effect.
         |Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the handled effect of a default handler is not applied to distinct type variables.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param actual     the handled effect as it occurs in the declared effect, e.g. `E[Int32]`.
    * @param loc        the location of that occurrence.
    */
  case class IllegalDefaultHandlerEffectArguments(handlerSym: Symbol.DefnSym, handledEff: Type, actual: Type, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0863

    def summary: String = s"Illegal default handler effect: expected '${formatType(handledEff)}', found '${formatType(actual)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler effect: expected '${cyan(formatType(handledEff))}', found '${red(formatType(actual))}'.
         |
         |${highlight(loc, "illegal type arguments", fmt)}
         |
         |${underline("Explanation:")} A default handler must handle '${formatType(handledEff)}' for all type arguments.
         |Hence the handled effect must be applied to distinct type variables that do not occur
         |elsewhere in the signature. Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the parameter of a default handler is not a thunk `Unit -> a \ ef`.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param actual     the declared type of the parameter.
    * @param loc        the location of the declared type of the parameter.
    */
  case class IllegalDefaultHandlerParameter(handlerSym: Symbol.DefnSym, handledEff: Type, actual: Type, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0839

    def summary: String = s"Illegal default handler parameter: expected 'Unit -> a \\ ef', found '${formatType(actual)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler parameter: expected '${cyan("Unit -> a \\ ef")}', found '${red(formatType(actual))}'.
         |
         |${highlight(loc, "illegal parameter type", fmt)}
         |
         |${underline("Explanation:")} The argument of a default handler must be a thunk 'Unit -> a \\ ef' where
         |'a' and 'ef' are type variables, since the handler must work for every result type and
         |every effect. Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the return type of a default handler is not the result type of its thunk.
    *
    * @param handlerSym the symbol of the default handler.
    * @param handledEff the handled effect applied to its type parameters, e.g. `E[t]`.
    * @param expected   the expected return type, i.e. the type variable `a` of the thunk `Unit -> a \ ef`.
    * @param actual     the declared return type.
    * @param loc        the location of the declared return type.
    */
  case class IllegalDefaultHandlerReturnType(handlerSym: Symbol.DefnSym, handledEff: Type, expected: Type, actual: Type, loc: SourceLocation)(implicit flix: Flix) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E0851

    def summary: String = s"Illegal default handler return type: expected '${formatType(expected)}', found '${formatType(actual)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Illegal default handler return type: expected '${cyan(formatType(expected))}', found '${red(formatType(actual))}'.
         |
         |${highlight(loc, "illegal return type", fmt)}
         |
         |${underline("Explanation:")} A default handler must return the result of its thunk argument, i.e. the
         |type variable 'a' of 'Unit -> a \\ ef'. Expected signature:
         |
         |  ${expectedSignature(handlerSym, handledEff)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a default handler is not public.
    *
    * @param handlerSym the symbol of the handler.
    * @param loc        the location of the handler.
    */
  case class NonPublicDefaultHandler(handlerSym: Symbol.DefnSym, loc: SourceLocation) extends DefaultHandlerError {
    def code: ErrorCode = ErrorCode.E1738

    def summary: String = s"Non-public default handler: '${handlerSym.name}' must be declared 'pub'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Non-public default handler: '${red(handlerSym.name)}' must be declared '${cyan("pub")}'.
         |
         |${highlight(loc, "non-public default handler.", fmt)}
         |""".stripMargin
    }
  }

}
