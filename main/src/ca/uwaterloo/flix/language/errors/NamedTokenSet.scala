/*
 * Copyright 2024 Magnus Madsen, Herluf Baggesen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.{SyntaxTree, TokenKind}
import ca.uwaterloo.flix.util.Formatter

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

  case object ExtMatchRule extends NamedTokenSet {
    def display(fmt: Formatter): String = fmt.cyan("<ext-match-rule>")
  }

  case object ExtPattern extends NamedTokenSet {
    def display(fmt: Formatter): String = fmt.cyan("<ext-pattern>")
  }

  case object ExtTag extends NamedTokenSet {
    def display(fmt: Formatter): String = fmt.cyan("<ext-tag>")
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
