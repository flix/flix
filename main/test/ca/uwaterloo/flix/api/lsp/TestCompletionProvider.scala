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
  /**
    * A list of programs to test invariants on.
    */
  private val programs = List(
    s"""
       |def main(): Unit \\ IO =
       |    run {
       |        Console.println("Please enter your name: ");
       |        let name = Console.readln();
       |        Console.println("Hello $${name}")
       |    } with Console.runWithIO
       |""".stripMargin,
    s"""
       |def main(): Unit \\ IO =
       |    run {
       |        let timestamp = Clock.currentTime(TimeUnit.Milliseconds);
       |        println("$${timestamp} ms since the epoc")
       |    } with Clock.runWithIO
       |""".stripMargin
  )

  /**
    * The uri of the test source.
    */
  private val uri = "<test>"

  /**
    * Compiles the given input string `s` with the given compilation options `o`.
    */
  private def compile(s: String, o: Options): (Root, Flix, List[CompilationMessage]) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    val flix = new Flix().setOptions(o).addSourceCode(uri, s)
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
    val input = Input.Text(uri, content, sctx)
    Source(input, content.toCharArray)
  }

  /**
    * Checks the completion provider invariant for the given token filter, string transformer and completion assertion.
    * For all the programs to be tested, we use the filter to select tokens to transform, apply the transformer to the selected tokens,
    * call the completion provider with the transformed program and check the completions against the assertion.
    *
    * @param tokenFilter          the filter to select tokens to transform.
    * @param stringTransformer    the transformer to apply to the selected tokens.
    * @param completionAssertion  the assertion to check the completions against.
    */
  private def checkInvariant(tokenFilter: Token => Boolean, stringTransformer: String => String, completionAssertion: CompletionList => Unit ): Unit = {
    programs.foreach { program =>
      val (root, _, _) = compile(program, Options.Default)
      val source = mkSource(program)
      val tokensToTransform = root.tokens(source).toList.filter(tokenFilter)
      tokensToTransform.foreach { token =>
        val replacingString = stringTransformer(token.text)
        val newProgram = program.substring(0, token.start) + replacingString + program.substring(token.end)
        val (newRoot, newFlix, newErrors) = compile(newProgram, Options.Default)
        val newPos = Position(token.sp1.line, token.sp1.col + replacingString.length) // position after the replacement
        val completions = CompletionProvider.autoComplete(uri, newPos, newProgram, newErrors)(newRoot, newFlix)
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

  test("No completions after complete keyword"){
    checkInvariant(
      token => token.kind.isKeyword,
      identity,
      assertEmptyCompletions
    )
  }
}
