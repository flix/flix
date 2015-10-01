package ca.uwaterloo.flix.language

import java.nio.file.Path

import ca.uwaterloo.flix.util.ConsoleCtx

package object ast {

  /**
   * A common super-type for input sources.
   */
  sealed trait SourceInput

  object SourceInput {

    /**
     * An input source that is backed by a regular string.
     */
    case class Str(str: String) extends SourceInput

    /**
     * An input source that is backed by a regular file.
     */
    case class File(path: Path) extends SourceInput

    case object Test extends SourceInput

  }

  /**
   * A class that represent a physical source position inside a source input.
   *
   * @param lineNumber the line number.
   * @param colNumber the column number.
   * @param line the line.
   */
  case class SourcePosition(lineNumber: Int, colNumber: Int, line: String)

  /**
   * Companion object for the [[SourceLocation]] class.
   */
  object SourceLocation {
    // TODO: Merge these into one.
    val Unknown = SourceLocation(SourceInput.Test, 0, 0, 0, 0, "")
    val Test = SourceLocation(SourceInput.Test, 1, 1, 1, 1, "")
  }

  /**
   * A class that represents the physical source location of some parsed syntactic entity.
   *
   * @param source the source input.
   * @param beginLine the line number where the entity begins.
   * @param beginCol the column number where the entity begins.
   * @param endLine the line number where the entity ends.
   * @param endCol the column number where the entity ends.
   * @param line the optional line (if the syntactic entity occurs on one line).
   */
  case class SourceLocation(source: SourceInput, beginLine: Int, beginCol: Int, endLine: Int, endCol: Int, line: String) {

    val formatSource: String = source match {
      case SourceInput.File(p) => p.toString
      case SourceInput.Str(_) => "???"
      case SourceInput.Test => "test"
    }

    /**
     * Returns a formatted string representation of `this` source location.
     */
    // TODO Remove?
    val format: String = source match {
      case SourceInput.Str(_) => s"<<unknown>>:$beginLine:$beginCol"
      case SourceInput.File(p) => s"$p:$beginLine:$beginCol"
      case SourceInput.Test => "test"
    }

    def underline(implicit consoleCtx: ConsoleCtx): String = {
      val lineNo = beginLine.toString + "|"
      val line1 = lineNo + line + "\n"
      val line2 = " " * (beginCol + lineNo.length - 1) + consoleCtx.red("^" * (endCol - beginCol))
      line1 + line2
    }
  }


}
