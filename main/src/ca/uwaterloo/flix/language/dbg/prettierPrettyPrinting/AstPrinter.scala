package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc.Indent
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /**
    * The extension of intermediate flix files.
    */
  val IREXTENSION: String = "flixir"

  /**
    * Prints `ast` to `build/ast/<phase>.<AstPrinter.IREXTENSION>` if
    * `flix.options.xprintasts` contains `phase`.
    *
    * @param ast    call-by-name to avoid premature computation
    * @param width  the maximum width of the output, default 80
    * @param indent the indentation width of each indentation level, default 4
    */
  def printAst(phase: String, ast: => DocAst.Program, width: Int = 80, indent: Int = 4)(implicit flix: Flix): Unit = {
    implicit val i: Indent = Doc.indentationLevel(indent)
    val phaseName = phase
    if (flix.options.xprintasts.contains(phaseName)) {
      val buildAstsPath = flix.options.output.getOrElse(Path.of("./build/")).resolve("asts/")
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

      val docAst = DocAstFormatter.format(ast)
      val str = docAst.map(Doc.pretty(width, _)).mkString("\n\n")
      Files.write(filePath, str.getBytes)
    }
  }

}
