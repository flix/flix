/*
 * Copyright 2022 Paul Butcher
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.language.phase.Parser.Letters
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.parboiled2.CharPredicate

object CompletionProvider {
  private implicit val audience: Audience = Audience.External

  def autoComplete(uri: String, pos: Position, source: Option[String])(implicit root: TypedAst.Root): JObject = {
    val line = source.flatMap(lineAt(_, pos.line - 1))
    val word = line.flatMap(wordAt(_, pos.character - 1))
    println(s"word: $word")

    val completions = CompletionList(isIncomplete = true, List())
    ("status" -> "success") ~ ("result" -> completions.toJSON)
  }

  /**
    * Optionally returns line number `n` in the string `s`.
    */
  private def lineAt(s: String, n: Int): Option[String] = {
    import java.io.{BufferedReader, StringReader}

    val br = new BufferedReader(new StringReader(s))

    var i = 0
    var line = br.readLine()
    while (line != null && i < n) {
      line = br.readLine()
      i = i + 1
    }
    Option(line)
  }

  /**
    * Optionally returns the word at the given index `n` in the string `s`.
    */
  private def wordAt(s: String, n: Int): Option[(Int, Int, String)] = {
    val isWordChar = Letters.LegalLetter ++ Letters.OperatorLetter ++
      Letters.MathLetter ++ Letters.GreekLetter ++ CharPredicate("@")

    // Bounds Check
    if (!(0 <= n && n <= s.length)) {
      return None
    }

    // Determine if the word is to the left of us, to the right of us, or out of bounds.
    val leftOf = 0 < n && isWordChar(s.charAt(n - 1))
    val rightOf = n < s.length && isWordChar(s.charAt(n))

    val i = (leftOf, rightOf) match {
      case (true, _) => n - 1
      case (_, true) => n
      case _ => return None
    }

    // Compute the beginning of the word.
    var begin = i
    while (0 < begin && isWordChar(s.charAt(begin - 1))) {
      begin = begin - 1
    }

    // Compute the ending of the word.
    var end = i
    while (end < s.length && isWordChar(s.charAt(end))) {
      end = end + 1
    }

    // Return the word.
    Some((begin, end, s.substring(begin, end)))
  }
}