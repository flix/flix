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

class TestCompletionProvider extends AnyFunSuite  {

  test("No completions after complete keyword"){
    checkInvariant(
      token => token.kind.isKeyword,
      identity,
      assertEmptyCompletions
    )
  }

  test("No completions after complete literal"){
    checkInvariant(
      token => token.kind.isLiteral,
      identity,
      assertEmptyCompletions
    )
  }

  /**
    * A list of programs to test invariants on.
    */
  private val Programs = List(
    s"""
       |def main(): Unit \\ IO =
       |    run {
       |        Console.println("Please enter your name: ");
       |        let name = Console.readln();
       |        Console.println("Hello $${name}")
       |    } with Console.runWithIO
       |
       |""".stripMargin,
    s"""
       |def main(): Unit \\ IO =
       |    run {
       |        let timestamp = Clock.currentTime(TimeUnit.Milliseconds);
       |        println("$${timestamp} ms since the epoc")
       |    } with Clock.runWithIO
       |
       |""".stripMargin,
    s"""
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
       |def parMap(f: a -> b, l: List[a]): List[b] = match l {
       |    case Nil     => Nil
       |    case x :: xs =>
       |        par (r <- f(x); rs <- parMap(f, xs))
       |            yield r :: rs
       |}
       |
       |def main(): Unit \\ IO =
       |    let l = List.range(1, 100);
       |    println(parMap(x -> x + 1, l))
       |
       |""".stripMargin
  )

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
    * Creates a source object from the given string `content`.
    */
  private def mkSource(content: String): Source = {
    val sctx = SecurityContext.AllPermissions
    val input = Input.Text(Uri, content, sctx)
    Source(input, content.toCharArray)
  }

  /**
    * Checks the completion provider invariant for the given token filter, string transformer and completion assertion.
    * For all the Programs to be tested, we use the filter to select tokens to transform, apply the transformer to the selected tokens,
    * call the completion provider with the transformed program and check the completions against the assertion.
    *
    * @param tokenFilter          the filter to select tokens to transform.
    * @param stringTransformer    the transformer to apply to the selected tokens.
    * @param completionAssertion  the assertion to check the completions against.
    */
  private def checkInvariant(tokenFilter: Token => Boolean, stringTransformer: String => String, completionAssertion: CompletionList => Unit ): Unit = {
    Programs.foreach { program =>
      // Compile the original program so that we can locate all target tokens.
      val (root, flix, errors) = compile(program, Options.Default)
      val source = mkSource(program)
      val tokensToTransform = root.tokens(source).toList.filter(tokenFilter)
      tokensToTransform.foreach { token =>
        val replacingString = stringTransformer(token.text)
        val newProgram = program.substring(0, token.start) + replacingString + program.substring(token.end)
        val (newRoot, newFlix, newErrors) = if (stringTransformer != identity)
          compile(newProgram, Options.Default)
        else (root, flix, errors)
        val newPos = Position(token.sp1.line, token.sp1.col + replacingString.length) // position after the replacement
        val completions = CompletionProvider.autoComplete(Uri, newPos, newErrors)(newRoot, newFlix)
        completionAssertion(completions)
      }
    }
  }

  /**
    * Asserts that the given completions are empty.
    */
  private def assertEmptyCompletions(completions: CompletionList): Unit = {
    assert(completions.items.isEmpty)
  }

  /**
    * A function that returns the input string as is.
    */
  private def identity(s: String): String = s
}
