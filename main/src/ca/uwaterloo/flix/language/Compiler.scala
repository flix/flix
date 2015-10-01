package ca.uwaterloo.flix.language

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.language.ast.{SourceInput, TypedAst, ResolvedAst, ParsedAst}
import ca.uwaterloo.flix.language.phase.Parser
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.util.{AnsiConsole, StopWatch}

import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}

/**
 * A compiler's primary function is to compile, organize the compilation, and go right back to compiling.
 * It compiles basically only those things that require to be compiled, ignoring things that should not be compiled.
 * The main way a compiler compiles, is to compile the things to be compiled until the compilation is complete.
 *
 * via Olivier Danvy.
 *
 * Inspiration for better error messages: http://elm-lang.org/blog/compiler-errors-for-humans
 */

object Compiler {

  /**
   * A common super-type for compilation errors.
   */
  trait CompilationError {
    /**
     * Returns a human readable string representation of the error.
     */
    def format: String
  }

  /**
   * An exception thrown to indicate an internal compiler error.
   *
   * This exception should never be thrown if the compiler is implemented correctly.
   *
   * @param message the error message.
   */
  case class InternalCompilerError(message: String) extends RuntimeException(message)

  implicit val ConsoleCtx = new AnsiConsole()

  /**
   * Returns the abstract syntax tree of the given `paths`.
   */
  // TODO: Return validation
  def parse(paths: Traversable[Path]): ParsedAst.Root = {
    val asts = paths map parse
    asts.reduce[ParsedAst.Root] {
      case (ast1, ast2) => ParsedAst.Root(ast1.declarations ++ ast2.declarations)
    }
  }

  /**
   * Returns the abstract syntax tree of the given `path`.
   */
  // TODO: Return validation
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
  // TODO: Return validation
  def parse(input: String, path: Option[Path]): ParsedAst.Root = {
    val parser = new Parser(SourceInput.File(path.get))
    parser.Root.run() match {
      case Success(ast) => ast
      case Failure(e: ParseError) => throw new RuntimeException(parser.formatError(e))
      case Failure(e) => throw new RuntimeException("Unexpected error during parsing run: " + e)
    }
  }

  /**
   * Compiles the source code of the given `path`.
   */
  def compile(path: Path): Option[TypedAst.Root] = compile(List(path))

  /**
   * Compiles  the source code of the given `paths`.
   */
  def compile(paths: Traversable[Path]): Option[TypedAst.Root] = {
    val stopWatch = new StopWatch()

    val past = parse(paths)
    println(f"Parser:     ${stopWatch.click() / 1000000}%4d msec.")

    val wast = Weeder.weed(past)
    if (wast.isFailure) {
      println()
      wast.errors.foreach(e => println(e.format))
      Console.println("Aborting due to previous errors.")
      return None
    }
    println(f"Weeder:     ${stopWatch.click() / 1000000}%4d msec.")

    val rast = Resolver.resolve(wast.get)
    if (rast.isFailure) {
      println()
      rast.errors.foreach(e => println(e.format))
      Console.println("Aborting due to previous errors.")
      return None
    }
    println(f"Resolver:   ${stopWatch.click() / 1000000}%4d msec.")

    val tast = Typer.typecheck(rast.get)
    if (tast.isFailure) {
      tast.errors.foreach(e => println(e.format))
      Console.println("Aborting due to previous errors.")
      return None
    }
    println(f"Typer:      ${stopWatch.click() / 1000000}%4d msec.")

    println()
    Console.println("Compilation successful.")
    Some(tast.get)
  }

  // TODO: Experimental section -----------------------------------------------

  object Expression {
    def compile(input: String): TypedAst.Expression = {

      // TODO: Consider using a prelude

//      val past = new Parser(None, input).Expression.run().get
//
//      val wast = Weeder.Expression.compile(past).get
//
//      // TODO: SymbolTable?
//      val rast = Resolver.Expression.resolve(wast, List.empty, Resolver.SymbolTable.empty).get
//
//      // TODO: Symbols?
//      val tast = Typer.Expression.typer(rast, ResolvedAst.Root(Map.empty, Map.empty, Map.empty, Map.empty, List.empty, List.empty))
//
//      tast.get
      ???
    }
  }

}
