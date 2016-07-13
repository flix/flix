/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
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
  def compile(inputs: List[SourceInput], hooks: Map[Symbol.Resolved, Ast.Hook])(implicit genSym: GenSym): Validation[TypedAst.Root, CompilationError] = {
    val t = System.nanoTime()
    val pasts = @@(inputs.map(parse))
    val e = System.nanoTime() - t

    val root = pasts map {
      case asts => ParsedAst.Program(asts, Time.Default.copy(parser = e))
    }

    root flatMap {
      case past => Weeder.weed(past, hooks) flatMap {
        case wast =>
          // TODO
          //Namer.namer(wast) map {
          //  case nast => Typer2.typer(nast)
          //}

          Resolver.resolve(wast) flatMap {
          case rast => Typer.typecheck(rast)
        }
      }
    }
  }

}
