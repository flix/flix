/*
 * Copyright 2024 Magnus Madsen, Herluf Baggesen
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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext
import ca.uwaterloo.flix.language.ast.{SourceLocation, SyntaxTree, TokenKind}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for parser errors.
  */
sealed trait ParseError extends CompilationMessage with Recoverable {
  val kind = "Parse Error"
  val sctx: SyntacticContext
}

object ParseError {

  /**
    * When reporting an error we want to report what tokens were expected at the particular location.
    * Sometimes it's best to not list all the options if there is too many, while other times it's fine.
    * For instance using "<expression>" in an error rather than the ~30 tokens that can lead an expression,
    * is much more useful to a programmer.
    * But if there's just 3 options, listing them is better.
    * [[NamedTokenSet]] models names for the large sets of tokens and provides [[NamedTokenSet.FromKinds]] for the small ones.
    * Examples:
    * Expected any expression but found a "}":
    * UnexpectedToken(expected = NamedTokenSet.Expression, actual = Some(TokenKind.CurlyR), ...)
    * Expected an "+", "-" or "name" but found a "}":
    * UnexpectedToken(expected = NamedTokenSet.FromKinds(Set(TokenKind.Plus, TokenKind.Minus, TokenKind.NameLowerCase)), actual = Some(TokenKind.CurlyR), ...)
    */
  sealed trait NamedTokenSet {
    def display(fmt: Formatter): String
  }

  object NamedTokenSet {

    case object Alias extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<alias>")
    }

    case object CatchRule extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<catch-rule>")
    }

    case object Declaration extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<declaration>")
    }

    case object Effect extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<effect>")
    }

    case object Expression extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<expression>")
    }

    case object FixpointConstraint extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<fixpoint-constraint>")
    }

    case object ForFragment extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<for-generator>")
    }

    case object KeyValuePair extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("`key -> value`")
    }

    case object Literal extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<literal>")
    }

    case object MatchRule extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<match-rule>")
    }

    case object Name extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<name>")
    }

    case object Parameter extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<parameter>")
    }

    case object Pattern extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<pattern>")
    }

    case object Tuple extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<tuple>")
    }

    case object Type extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<type>")
    }

    case object WithRule extends NamedTokenSet {
      def display(fmt: Formatter): String = fmt.cyan("<with-rule>")
    }

    case class FromKinds(kinds: Set[TokenKind]) extends NamedTokenSet {
      def display(fmt: Formatter) = s"${prettyJoin(kinds.toList.map(t => fmt.cyan(t.display)))}"
    }

    case class FromTreeKinds(kinds: Set[SyntaxTree.TreeKind]) extends NamedTokenSet {
      def display(fmt: Formatter) = s"${prettyJoin(kinds.toList.map(k => fmt.cyan(s"<$k>")))}"
    }
  }

  /**
    * An error raised to indicate that something was syntactically malformed.
    * This error is very generic, and should be avoided in favor of other ParserErrors if possible.
    *
    * @param namedTokenSet Name of the malformed token set.
    * @param sctx          The syntactic context.
    * @param hint          Optional hint with more details about the error.
    * @param loc           The source location.
    */
  case class Malformed(namedTokenSet: NamedTokenSet, sctx: SyntacticContext, hint: Option[String] = None, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = s"Malformed ${namedTokenSet.display(Formatter.NoFormatter)}."

    def message(fmt: Formatter): String = {
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s""">> Malformed ${fmt.red(namedTokenSet.display(fmt))}.
         |
         |${fmt.code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that one or more sequential comments are misplaced.
    *
    * @param sctx The syntactic context.
    * @param loc  The source location.
    */
  case class MisplacedComments(sctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = s"Misplaced comment(s)."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Misplaced comment(s).
         |
         |${code(loc, s"Here")}
         |Hint: Place comments on their own line.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that one or more doc-comments are misplaced.
    *
    * @param sctx The syntactic context.
    * @param loc  The source location.
    */
  case class MisplacedDocComments(sctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = s"Misplaced doc-comment(s)."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Misplaced doc-comment(s).
         |
         |${code(loc, s"Here")}
         |Hint: doc-comments must annotate declarations.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a scoped expression is missing a scope.
    *
    * @param token Name of the expression missing a scope. See [[TokenKind.display]].
    * @param sctx  The syntactic context.
    * @param loc   The source location.
    */
  case class MissingScope(token: TokenKind, sctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = s"Expected scope on ${token.display}."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Expected ${red("scope")} on ${cyan(token.display)}.
         |
         |${code(loc, s"Here")}
         |Hint: Add a scope using `@ <scope>`
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that no items were found in a context where one or more is needed.
    *
    * @param expected Names of the items that are expected at least one of. See [[TokenKind.display]].
    * @param sctx     The syntactic context.
    * @param hint     Optional hint with more details about the error
    * @param loc      The source location.
    */
  case class NeedAtleastOne(expected: NamedTokenSet, sctx: SyntacticContext, hint: Option[String] = None, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = s"Expected at least one ${expected.display(Formatter.NoFormatter)}."

    def message(fmt: Formatter): String = {
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s""">> Expected at least one ${expected.display(fmt)}.
         |
         |${fmt.code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a trailing separator is present.
    *
    * @param separator Name of the separator that is trailing. See [[TokenKind.display]].
    * @param sctx      The syntactic context.
    * @param loc       The source location.
    */
  case class TrailingSeparator(separator: TokenKind, sctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = s"Trailing ${separator.display}."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Trailing ${red(separator.display)}.
         |
         |${code(loc, s"Here")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unexpected token was found.
    *
    * @param expected Names of the tokens that are expected at the location.
    * @param actual   Name of the token that was actually found. See [[TokenKind.display]].
    * @param sctx     The syntactic context.
    * @param hint     Optional hint with more details about the error
    * @param loc      The source location.
    */
  case class UnexpectedToken(expected: NamedTokenSet, actual: Option[TokenKind], sctx: SyntacticContext, hint: Option[String] = None, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($sctx)"

    def summary: String = {
      val expectedStr = s"Expected ${expected.display(Formatter.NoFormatter)}"
      val actualStr = actual.map(a => s" before $a").getOrElse("")
      s"$expectedStr$actualStr."
    }

    def message(fmt: Formatter): String = {
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      val expectedStr = s"Expected ${expected.display(fmt)}"
      val actualStr = actual.map(a => s" before ${fmt.red(a.display)}").getOrElse("")
      s""">> $expectedStr$actualStr.
         |
         |${fmt.code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * Joins items nicely with comma separation ending with an "or".
    * For instance prettyJoin(List("def", "enum", "trait")) gives "def, enum or trait".
    */
  private def prettyJoin[T](items: Seq[T]): String = items match {
    case i1 :: i2 :: Nil => s"$i1 or $i2"
    case i1 :: Nil => s"$i1"
    case i :: tail => s"$i, ${prettyJoin(tail)}"
  }
}

