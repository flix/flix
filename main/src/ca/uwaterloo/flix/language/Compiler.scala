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

import java.nio.charset.Charset
import java.nio.file.Files
import java.util.zip.ZipFile

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.util.StreamOps
import ca.uwaterloo.flix.util.Highlight._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream

import scala.util.{Try,Failure, Success}

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
    val kind = "Parse Error"
    val source = src
    val message =
      s"""|>> Parse Error:
          |
          |${Red(msg)}
          """.stripMargin
  }

  
  /*
    * Implicitly assumed default charset.
    */
  val DefaultCharset = Charset.forName("UTF-8")

  /**
    * Returns the abstract syntax tree of the given string `input`.
    */
  def parse(source: SourceInput): Validation[ParsedAst.Root, CompilationError] = {

    val input: String = source match {
      case SourceInput.Internal(name,text) => text
      case SourceInput.Str(str) => str
      case SourceInput.TxtFile(path) =>
        new String(Files.readAllBytes(path), DefaultCharset)
      case SourceInput.ZipFile(path) =>
        val file = new ZipFile(path.toFile)
        val entry = file.entries().nextElement()
        val inputStream = file.getInputStream(entry)
        new String(StreamOps.readAllBytes(inputStream), DefaultCharset)
    }

    val inputstream = new ANTLRInputStream(input)
    val lexer = new FlixLexer(inputstream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new FlixParser(tokens)
    val visitor = new AST_FlixVisitor(source,input)
    Try(parser.start()) match {
      case Success(tree) => visitor.visitStart(tree).toSuccess
      case Failure(e) => ParseError(e.getMessage, source).toFailure
    }
  }

  /**
    * Returns the typed AST corresponding to the given `inputs`.
    */
  def compile(inputs: List[SourceInput], hooks:  Map[Symbol.DefnSym, Ast.Hook])(implicit genSym: GenSym): Validation[TypedAst.Root, CompilationError] = {
    val t = System.nanoTime()
    val pasts = @@(inputs.map(parse))
    val e = System.nanoTime() - t

    val root = pasts map {
      case asts => ParsedAst.Program(asts, Time.Default.copy(parser = e))
    }

    for (
      parsedAst <- root;
      weededAst <- Weeder.weed(parsedAst, hooks);
      namedAst <- Namer.namer(weededAst);
      typedAst <- Typer.typer(namedAst)
    ) yield typedAst
  }

}
