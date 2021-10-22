package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

object Format {

  def line(left: String, right: String): String = ""

  def code(loc: SourceLocation, text: String): String = ""

  def text(s: String): String = ""

  def black(s: String): String = ""

  def blue(s: String): String = ""

  def cyan(s: String): String = ""

  def green(s: String): String = ""

  def magenta(s: String): String = ""

  def red(s: String): String = ""

  def yellow(s: String): String = ""

  def white(s: String): String = ""

  def bold(s: String): String = ""

  def underline(s: String): String = ""

}
