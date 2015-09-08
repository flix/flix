package ca.uwaterloo.flix.lang

import java.nio.file.Path

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

// TODO: Consider source locations which have an end point.
