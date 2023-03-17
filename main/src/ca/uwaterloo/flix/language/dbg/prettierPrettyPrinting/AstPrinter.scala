package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc.Indent
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  val IREXTENSION: String = "flixir"

  val WIDTH: Int = 80

  val INDENT: Indent = Doc.indentationLevel(4)

  /**
    * Prints the given Ast to `build/ast/<phase>.flixir` if
    * `flix.options.xprintasts` contains the phase name.
    */
  def printAst(phase: => String, ast: => DocAst.Program)(implicit flix: Flix): Unit = {
    val phaseName = phase
    if (flix.options.xprintasts.contains(phaseName)) {
      val buildAstsPath = Path.of("./build/asts/").toAbsolutePath
      val filePath = buildAstsPath.resolve(s"$phaseName.$IREXTENSION")
      Files.createDirectories(buildAstsPath)

      // Check if the file already exists.
      if (Files.exists(filePath)) {
        // Check that the file is a regular file.
        if (!Files.isRegularFile(filePath, LinkOption.NOFOLLOW_LINKS)) {
          throw InternalCompilerException(s"Unable to write to non-regular file: '$filePath'.", SourceLocation.Unknown)
        }

        // Check if the file is writable.
        if (!Files.isWritable(filePath)) {
          throw InternalCompilerException(s"Unable to write to read-only file: '$filePath'.", SourceLocation.Unknown)
        }
      }

      implicit val i: Indent = INDENT
      val docAst = DocAstFormatter.format(ast)
      val str = docAst.map(Doc.pretty(WIDTH, _)).mkString("\n\n")
      Files.write(filePath, str.getBytes)
    }
  }

}
