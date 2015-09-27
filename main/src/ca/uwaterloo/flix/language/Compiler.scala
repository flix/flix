package ca.uwaterloo.flix.language

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.language.ast.{SourceLocation, ResolvedAst, ParsedAst}
import ca.uwaterloo.flix.language.phase._
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.io.Source
import scala.util.{Failure, Success}

/**
 * A compiler's primary function is to compile, organize the compilation, and go right back to compiling.
 * It compiles basically only those things that require to be compiled, ignoring things that should not be compiled.
 * The main way a compiler compiles, is to compile the things to be compiled until the compilation is complete.
 *
 * via Olivier Danvy.
 */

// TODO: http://elm-lang.org/blog/compiler-errors-for-humans

object Compiler {

  // TODO: DOC
  case class InternalCompilerError(message: String) extends RuntimeException(message)

  /**
   * Returns the abstract syntax tree of the given `paths`.
   */
  def parse(paths: Traversable[Path]): ParsedAst.Root = {
    val asts = paths map parse
    asts.reduce[ParsedAst.Root] {
      case (ast1, ast2) => ParsedAst.Root(ast1.declarations ++ ast2.declarations)
    }
  }

  /**
   * Returns the abstract syntax tree of the given `path`.
   */
  def parse(path: Path): ParsedAst.Root =
    if (!Files.exists(path))
      throw new RuntimeException(s"Path '$path' does not exist.")
    else if (!Files.isReadable(path))
      throw new RuntimeException(s"Path '$path' is not readable.")
    else if (!Files.isRegularFile(path))
      throw new RuntimeException(s"Path '$path' is not a regular file.")
    else
      parse(Source.fromFile(path.toFile).getLines().mkString("\n"), Some(path))

  /**
   * Returns the abstract syntax tree of the given string `input`.
   */
  def parse(input: String, path: Option[Path]): ParsedAst.Root = {
    val parser = new Parser(path, input)
    parser.Root.run() match {
      case Success(ast) => ast
      case Failure(e: ParseError) => throw new RuntimeException(parser.formatError(e))
      case Failure(e) => throw new RuntimeException("Unexpected error during parsing run: " + e)
    }
  }

  // TODO: Allow input from other sources.
  def compile(paths: Traversable[Path]): Unit = {
    Console.print("Parsing: ")
    val past = parse(paths)
    Console.println("Success!")

    Console.print("Weeding: ")
    val wast = Weeder.weed(past)
    if (wast.isFailure) {
      println()
      wast.errors.foreach(e => println(e.format))
      return
    }
    Console.println("Success!")

    Console.print("Resolution: ")
    val rast = Resolver.resolve(wast.get)
    if (rast.isFailure) {
      println()
      rast.errors.foreach(e => println(e.format))
      return
    }
    Console.println("Success!")

    Console.print("Typechecking: ")
    val tast = Typer.typecheck(rast.get)
    if (tast.isFailure) {
      tast.errors.foreach(e => println(e.format))
      return
    }
    Console.println("Success!")

  }

}
