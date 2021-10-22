package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

object Format {

  def line(left: String, right: String): String = s"<Line><Left>$left</Left><Right>$right</Right></Line>"

  def code(loc: SourceLocation, text: String): String = s"<Code><Loc>${loc.format}</Loc>${this.text(text)}</Code>"

  def text(s: String): String = s"<Text>$s</Text>"

  def black(s: String): String = s"<Black>$s</Black>"

  def blue(s: String): String = s"<Blue>$s</Blue>"

  def cyan(s: String): String = s"<Cyan>$s</Cyan>"

  def green(s: String): String = s"<Green>$s</Green>"

  def magenta(s: String): String = s"<Magenta>$s</Magenta>"

  def red(s: String): String = s"<Red>$s</Red>"

  def yellow(s: String): String = s"<Yellow>$s</Yellow>"

  def white(s: String): String = s"<White>$s</White>"

  def bold(s: String): String = s"<Bold>$s</Bold>"

  def underline(s: String): String = s"<Underline>$s</Underline>"

}
