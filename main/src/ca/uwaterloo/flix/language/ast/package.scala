package ca.uwaterloo.flix.language

import java.nio.file.Path

import ca.uwaterloo.flix.util.ConsoleCtx

package object ast {

  // TODO: Move into Ast.

  /**
    * A common super-type for sources.
    */
  sealed trait SourceInput {
    def format: String = this match {
      case SourceInput.TxtFile(p) => p.getFileName.toString
      case SourceInput.ZipFile(p) => p.getFileName.toString
      case SourceInput.Str(_) => "???"
    }
  }

  object SourceInput {

    /**
      * An source that is backed by a regular string.
      */
    case class Str(str: String) extends SourceInput

    /**
      * An source that is backed by a regular file.
      */
    case class TxtFile(path: Path) extends SourceInput

    /**
      * An source that is backed by a zip file.
      */
    case class ZipFile(path: Path) extends SourceInput

  }

  object SourcePosition {
    /**
      * Represents an unknown source position.
      */
    val Unknown: SourcePosition = SourcePosition(SourceInput.Str(""), 0, 0, () => "")
  }

  /**
    * A class that represent a physical source position inside a source input.
    *
    * @param line the line number.
    * @param col  the column number.
    * @param code the line.
    */
  case class SourcePosition(source: SourceInput, line: Int, col: Int, code: () => String)

  /**
    * Companion object for the [[SourceLocation]] class.
    */
  object SourceLocation {
    /**
      * Represents an unknown source location.
      */
    val Unknown: SourceLocation = mk(SourcePosition.Unknown, SourcePosition.Unknown)

    def mk(b: SourcePosition, e: SourcePosition): SourceLocation = {
      assert(b.source == e.source)
      SourceLocation(b.source, b.line, b.col, e.line, e.col, b.code)
    }

    implicit object Order extends Ordering[SourceLocation] {

      import scala.math.Ordered.orderingToOrdered

      def compare(x: SourceLocation, y: SourceLocation): Int =
        (x.source.format, x.beginLine, x.beginCol) compare(y.source.format, y.beginLine, y.beginCol)
    }

  }

  /**
    * A class that represents the physical source location of some parsed syntactic entity.
    *
    * @param source    the source input.
    * @param beginLine the line number where the entity begins.
    * @param beginCol  the column number where the entity begins.
    * @param endLine   the line number where the entity ends.
    * @param endCol    the column number where the entity ends.
    * @param line      the optional line (if the syntactic entity occurs on one line).
    */
  case class SourceLocation(source: SourceInput, beginLine: Int, beginCol: Int, endLine: Int, endCol: Int, line: () => String) {

    /**
      * Returns a formatted string representation of `this` source location.
      */
    def format: String = s"${source.format}:$beginLine:$beginCol"

    /**
      * Returns this line of code with the source location underlined.
      */
    def underline(implicit consoleCtx: ConsoleCtx): String = {
      val lineNo = beginLine.toString + "|"
      val line1 = lineNo + line() + "\n"
      val line2 = " " * (beginCol + lineNo.length - 1) + consoleCtx.red("^" * (endCol - beginCol))
      line1 + line2
    }
  }

  /**
    * A class that tracks the amount of time spent in each phase of the compiler.
    *
    * @param parser the time spent in the parser.
    */
  case class Time(parser: Long, weeder: Long, resolver: Long, typer: Long, verifier: Long)

}
