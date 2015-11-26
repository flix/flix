package ca.uwaterloo.flix.language

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.Flix
import ca.uwaterloo.flix.language.ast.{SourceInput, TypedAst}
import ca.uwaterloo.flix.language.frontend.ast.ParsedAst
import ca.uwaterloo.flix.language.frontend.phase.Parser
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.util.AnsiConsole
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.util.{Failure, Success}

/**
 * A compiler's primary function is to compile, organize the compilation, and go right back to compiling.
 * It compiles basically only those things that require to be compiled, ignoring things that should not be compiled.
 * The main way a compiler compiles, is to compile the things to be compiled until the compilation is complete.
 *
 * via Olivier Danvy.
 *
 * Inspiration for better error messages:
 *
 * http://clang.llvm.org/diagnostics.html
 * http://elm-lang.org/blog/compiler-errors-for-humans
 */

object Compiler {

  /**
   * A common super-type for compilation errors.
   */
  trait CompilationError extends Flix.FlixError {
    /**
     * Returns a human readable string representation of the error.
     */
    def format: String
  }

  /**
   * An error raised to indicate a parse error.
   *
   * @param msg the error message.
   * @param src the source input.
   */
  case class ParseError(msg: String, src: SourceInput) extends CompilationError {
    val format =
      s"""${ConsoleCtx.blue(s"-- PARSE ERROR ------------------------------------------------- ${src.format}")}
         |
         |${ConsoleCtx.red(msg)}
         """.stripMargin
  }

  /**
   * An exception thrown to indicate an internal compiler error.
   *
   * This exception should never be thrown if the compiler is implemented correctly.
   *
   * @param message the error message.
   */
  case class InternalCompilerError(message: String) extends RuntimeException(message)

  /**
   * The console context used to format error messages.
   */
  implicit val ConsoleCtx = new AnsiConsole()

  /**
   * Returns the abstract syntax tree of the given string `input`.
   */
  def parse(source: SourceInput): Validation[ParsedAst.Root, CompilationError] = {
    val parser = new Parser(source)
    parser.Root.run() match {
      case Success(ast) => ast.toSuccess
      case Failure(e: org.parboiled2.ParseError) => ParseError(parser.formatError(e), source).toFailure
      case Failure(e) => ParseError(e.getMessage, source).toFailure
    }
  }

  /**
   * Returns the abstract syntax tree of the given `string`.
   */
  def parse(string: String): Validation[ParsedAst.Root, CompilationError] =
    parse(SourceInput.Str(string))

  /**
   * Returns the abstract syntax tree of the given `strings`.
   */
  def parseStrings(strings: Traversable[String]): Validation[ParsedAst.Root, CompilationError] = {
    @@(strings map parse) map {
      case asts => asts.reduce[ParsedAst.Root] {
        case (ast1, ast2) => ParsedAst.Root(ast1.declarations ++ ast2.declarations, ast1.time) // TODO: Merge trees differently due to time
      }
    }
  }

  /**
   * Returns the abstract syntax tree of the given `path`.
   */
  def parse(path: Path): Validation[ParsedAst.Root, CompilationError] =
    if (!Files.exists(path))
      throw new RuntimeException(s"Path '$path' does not exist.")
    else if (!Files.isReadable(path))
      throw new RuntimeException(s"Path '$path' is not readable.")
    else if (!Files.isRegularFile(path))
      throw new RuntimeException(s"Path '$path' is not a regular file.")
    else {
      if (path.getFileName.toString.endsWith(".flix.zip"))
        parse(SourceInput.ZipFile(path))
      else
        parse(SourceInput.TxtFile(path))
    }

  /**
   * Returns the abstract syntax tree of the given `paths`.
   */
  def parsePaths(paths: Traversable[Path]): Validation[ParsedAst.Root, CompilationError] = {
    @@(paths map parse) map {
      case asts => asts.reduce[ParsedAst.Root] {
        case (ast1, ast2) => ParsedAst.Root(ast1.declarations ++ ast2.declarations, ast1.time) // TODO: Merge trees differently due to time
      }
    }
  }

  /**
   * Compiles the given `string`.
   */
  def compile(string: String): Validation[TypedAst.Root, CompilationError] = compileStrings(List(string))

  /**
   * Compiles the given `path`.
   */
  def compile(path: Path): Validation[TypedAst.Root, CompilationError] = compilePaths(List(path))

  /**
   * Compiles the given `strings`.
   */
  def compileStrings(strings: Traversable[String]): Validation[TypedAst.Root, CompilationError] = {
    parseStrings(strings) flatMap {
      case past => Weeder.weed(past) flatMap {
        case wast => Resolver.resolve(wast) flatMap {
          case rast => Typer.typecheck(rast)
        }
      }
    }
  }

  /**
   * Compiles the given `paths`.
   */
  def compilePaths(paths: Traversable[Path]): Validation[TypedAst.Root, CompilationError] = {
    parsePaths(paths) flatMap {
      case past => Weeder.weed(past) flatMap {
        case wast => Resolver.resolve(wast) flatMap {
          case rast => Typer.typecheck(rast)
        }
      }
    }
  }

}
