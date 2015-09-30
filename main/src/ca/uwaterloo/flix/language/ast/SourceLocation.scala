package ca.uwaterloo.flix.language.ast

import java.nio.file.Path

/**
 * Companion object for the [[SourceLocation]] class.
 */
object SourceLocation {

  val Unknown = SourceLocation(None, 0, 0)

  val Test = SourceLocation(None, 1, 1)
}

/**
 * Represents a physical location in a source.
 *
 * @param path The optional path of the source.
 * @param line The line number.
 * @param column The column number.
 */
case class SourceLocation(path: Option[Path], line: Int, column: Int) {
  /**
   * Returns a formatted string representation of `this` source location.
   */
  val format: String = path match {
    case None => s"<<unknown>>:$line:$column"
    case Some(p) => s"$p:$line:$column"
  }
}

// TODO: Consider lineBegin, columnBegin, lineEnd, columnEnd

