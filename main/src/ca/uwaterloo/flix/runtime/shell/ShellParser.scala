/*
 * Copyright 2022 Paul Butcher
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.runtime.shell

import org.jline.reader.{EOFError, ParsedLine, Parser}
import org.jline.reader.Parser.ParseContext

import java.util
import scala.jdk.CollectionConverters.*

/**
  * Minimal implementation of `ParsedLine`, necessary to keep jline happy.
  *
  * JLine uses these values to implement syntax highlighting, line continuation on
  * unclosed string, brackets, etc. Because we have all of this functionality
  * switched off, we just need to return something that conforms to the `ParsedLine`
  * interface.
  *
  * https://github.com/jline/jline3/blob/master/reader/src/main/java/org/jline/reader/ParsedLine.java
  */
class Parsed(s: String) extends ParsedLine {

  def wordIndex(): Int = -1
  def word(): String = s
  def wordCursor(): Int = s.length
  def words(): util.List[String] = Nil.asJava
  def cursor() = -1
  def line(): String = s
}

/**
  * Implementation of jline's `Parser` to trigger jline's line continuation functionality
  */
class ShellParser extends Parser {
  def parse(s: String, cursor: Int, context: Parser.ParseContext): ParsedLine = {

    if (context == Parser.ParseContext.ACCEPT_LINE) {

      val lines = s.linesIterator.toList

      // If the input starts with two backslashes on a line ...
      if (lines.headOption.contains("\\\\")) {

        // ... and doesn't end with two backslashes on a line, throw `EOFError` to trigger line continuation
        if (!(lines.size > 1 && lines.last == "\\\\"))
          throw new EOFError(-1, -1, "Escaped new line", "newline")
      } else {

        // Otherwise, if the last character in the string is a backslash, throw `EOFError` to trigger line continuation
        if (s.lastOption.contains('\\'))
          throw new EOFError(-1, -1, "Escaped new line", "newline")
      }
    }
    new Parsed(s)
  }
}
