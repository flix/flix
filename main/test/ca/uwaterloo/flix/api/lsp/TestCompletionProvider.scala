/*
 * Copyright 2025 Chenhao Gao
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

package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestCompletionProvider extends AnyFunSuite {
  /**
    * A list of programs to test invariants on.
    */
  private val Programs = List(
        s"""
           |/**
           |  * A simple program that reads a name from the console and prints a greeting.
           |  */
           |def main(): Unit \\ IO =
           |    run {
           |        Console.println("Please enter your name: ");
           |        let name = Console.readln();
           |        Console.println("Hello $${name}")
           |    } with Console.runWithIO
           |
           |""".stripMargin,
        s"""
           |// A simple program that reads a name from the console and prints a greeting.
           |    def main(): Unit \\ IO =
           |    run {
           |        let timestamp = Clock.currentTime(TimeUnit.Milliseconds);
           |        println("$${timestamp} ms since the epoc")
           |    } with Clock.runWithIO
           |
           |""".stripMargin,
        s"""
           |///
           |/// A simple program that reads a name from the console and prints a greeting.
           |///
           |def main(): Unit \\ {Net, IO} =
           |    run {
           |        let url = "http://example.com/";
           |        Logger.info("Downloading URL: '$${url}'");
           |        match HttpWithResult.get(url, Map.empty()) {
           |            case Result.Ok(response) =>
           |                let file = "data.txt";
           |                Logger.info("Saving response to file: '$${file}'");
           |                let body = Http.Response.body(response);
           |                match FileWriteWithResult.write(str = body, file) {
           |                    case Result.Ok(_) =>
           |                        Logger.info("Response saved to file: '$${file}'")
           |                    case Result.Err(err) =>
           |                        Logger.fatal("Unable to write file: '$${err}'")
           |                }
           |            case Result.Err(err) =>
           |                Logger.fatal("Unable to download URL: '$${err}'")
           |        }
           |    } with FileWriteWithResult.runWithIO
           |      with HttpWithResult.runWithIO
           |      with Logger.runWithIO
           |
           |""".stripMargin,
    s"""
       |// Map the function f over the list l in parallel.
       |def parMap(f: a -> b, l: List[a]): List[b] = match l {
       |    case Nil     => Nil
       |    case x :: xs =>
       |        par (r <- f(x); rs <- parMap(f, xs))
       |            yield r :: rs
       |}
       |
       |// The main function.
       |//
       |def main(): Unit \\ IO =
       |    let l = List.range(1, 100);
       |    println(parMap(x -> x + 1, l))
       |
       |""".stripMargin
  )

  test("No completions after complete keyword") {
    Programs.foreach { program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val source = mkSource(program)
      val keywordTokens = root.tokens(source).toList.filter(_.kind.isKeyword)
      keywordTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.autoComplete(Uri, pos, errors)(root, flix)
          assert(completions.items.isEmpty)
        }
      }
    }
  }

  test("No completions after complete literal") {
    Programs.foreach { program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val source = mkSource(program)
      // Find all the literal tokens that are on a single line
      val literalTokens = root.tokens(source).toList.filter(_.kind.isLiteral)
      literalTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.autoComplete(Uri, pos, errors)(root, flix)
          assert(completions.items.isEmpty)
        }
      }
    }
  }

  test("No completions inside comment") {
    Programs.foreach { program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val source = mkSource(program)
      // Find all the literal tokens that are on a single line
      val commentTokens = root.tokens(source).toList.filter(_.kind.isComment)
      commentTokens.foreach { token =>
        // We will test all possible offsets in the keyword, including the start and end of the keyword
        getAllPositionsWithinToken(token).foreach { pos =>
          val completions = CompletionProvider.autoComplete(Uri, pos, errors)(root, flix)
          assert(completions.items.isEmpty)
        }
      }
    }
  }

  test("No completions when defining the name for defs"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.defs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  test("No completions when defining the name for enums"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.enums.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  test("No completions when defining the name for sigs"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.sigs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  test("No completions when defining the name for traits"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.traits.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  test("No completions when defining the name for effects"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.effects.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  test("No completions when defining the name for structs"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.structs.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  test("No completions when defining the name for type aliases"){
    Programs.foreach{ program =>
      val (root, flix, errors) = compile(program, Options.Default)
      val allNameDefLocs = root.typeAliases.keys.filter(_.src.name.startsWith(Uri)).map(_.loc)
      allNameDefLocs.foreach{ loc =>
        val completions = CompletionProvider.autoComplete(Uri, Position.from(loc.sp2), errors)(root, flix)
        assert(completions.items.isEmpty)
      }
    }
  }

  /**
    * The uri of the test source.
    */
  private val Uri = "<test>"

  /**
    * Compiles the given input string `s` with the given compilation options `o`.
    */
  private def compile(s: String, o: Options): (Root, Flix, List[CompilationMessage]) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    val flix = new Flix().setOptions(o).addSourceCode(Uri, s)
    flix.check() match {
      case (Some(root), errors) => (root, flix, errors)
      case (None, _) => fail("Compilation failed: a root is expected.")
    }
  }

  /**
    * Returns all positions within the given token.
    *
    * For example, give a token "def", we will return a list of positions:
    * - |def
    * - d|ef
    * - de|f
    * - def|
    *
    * If the token spans multiple lines, we will return all the positions in all the lines, both sides inclusive.
    */
  private def getAllPositionsWithinToken(token: Token): List[Position] = {
    val initialLine = token.sp1.lineOneIndexed
    val initialCol = token.sp1.colOneIndexed.toInt

    token.text
      .scanLeft((initialLine, initialCol)) { case ((line, col), char) =>
        if (char == '\n') (line + 1, 1)
        else (line, col + 1)
      }
      .map { case (line, col) => Position(line, col) }
      .toList
  }

  /**
    * Creates a source object from the given string `content`.
    */
  private def mkSource(content: String): Source = {
    val sctx = SecurityContext.AllPermissions
    val input = Input.Text(Uri, content, sctx)
    Source(input, content.toCharArray)
  }
}
