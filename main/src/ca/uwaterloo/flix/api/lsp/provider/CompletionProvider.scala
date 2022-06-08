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

  def autoComplete(_uri: String, pos: Position, source: Option[String])(implicit root: TypedAst.Root): JObject = {
    val completions = source.flatMap(Context(_, pos)) match {
      case None => Nil
      case Some(context) => getCompletions(context)
    }

    ("status" -> "success") ~ ("result" -> CompletionList(isIncomplete = true, completions).toJSON)
  }

  private def getCompletions(context: Context)(implicit root: TypedAst.Root): List[CompletionItem] = {
    List(
      CompletionItem(label = "foo",
        filterText = context.word,
        textEdit = TextEdit(context.range, "newtext"),
        kind = CompletionItemKind.Keyword)
    )
  }

  private object Context {
    /**
      * Characters that constitute a word.
      * This is more permissive than the parser, but that's OK.
      */
    private val isWordChar = Letters.LegalLetter ++ Letters.OperatorLetter ++
        Letters.MathLetter ++ Letters.GreekLetter ++ CharPredicate("@")

    def apply(source: String, pos: Position) = {
      val x = pos.character - 1
      val y = pos.line - 1
      for(line <- source.linesWithSeparators.slice(y, y + 1).toList.headOption) yield {
        val (prefix, suffix) = line.splitAt(x)
        val reversedPrefix = prefix.reverse
        val wordStart = reversedPrefix.takeWhile(isWordChar).reverse
        val wordEnd = suffix.takeWhile(isWordChar)
        val word = wordStart + wordEnd
        val start = x - wordStart.length
        val end = x + wordEnd.length
        val previousWord = reversedPrefix.dropWhile(isWordChar).dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
        val range = Range(Position(y, start), Position(y, end))
        new Context(range, word, previousWord)
      }
    }
  }

  private class Context(val range: Range, val word: String, val previousWord: String)
}
