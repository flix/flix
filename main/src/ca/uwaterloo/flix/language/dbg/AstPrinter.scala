package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.dbg.Doc.Indent
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /** default indentation width of 4 spaces */
  private val Indent = 4

  /** default maximum output width of 80 chars */
  private val Width = 80

  /**
    * The extension of intermediate flix files.
    */
  val IrExtension: String = "flixir"

  /**
    * Prints `ast` to `build/ast/<phase>.<AstPrinter.IrExtension>` if
    * `flix.options.xprintasts` contains `phase`.
    *
    * @param ast    call-by-name to avoid premature computation
    */
  def printAst(phase: String, ast: => DocAst.Program)(implicit flix: Flix): Unit = {
    implicit val i: Indent = Doc.indentationLevel(Indent)
    val phaseName = phase
    if (flix.options.xprintasts.contains(phaseName)) {
      val buildAstsPath = flix.options.output.getOrElse(Path.of("./build/")).resolve("asts/")
      val filePath = buildAstsPath.resolve(s"$phaseName.$IrExtension")
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
      val str = docAst.map(Doc.pretty(Width, _)).mkString("\n\n")
      Files.write(filePath, str.getBytes)
    }
  }

}
