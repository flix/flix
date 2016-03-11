package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.{Parser, Resolver, Typer, Weeder}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{AnsiConsole, Validation}

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
    * An error raised to indicate a parse error.
    *
    * @param msg the error message.
    * @param src the source input.
    */
  case class ParseError(msg: String, src: SourceInput) extends CompilationError {
    val message =
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
    * Returns the typed AST corresponding to the given `inputs`.
    */
  def compile(inputs: List[SourceInput], hooks: Map[Symbol.Resolved, Ast.Hook]): Validation[TypedAst.Root, CompilationError] = {
    val t = System.nanoTime()
    val pasts = @@(inputs.map(parse))
    val e = System.nanoTime() - t

    val root = pasts map {
      case asts => ParsedAst.Program(asts, Time(e, 0, 0, 0, 0))
    }

    root flatMap {
      case past => Weeder.weed(past, hooks) flatMap {
        case wast => Resolver.resolve(wast) flatMap {
          case rast => Typer.typecheck(rast)
        }
      }
    }
  }

}
