/*
 * Copyright 2023 Xavier deSouza
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

package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseEnumTagCompletion
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.SourceLocation

object UseEnumTagCompleter {
  /**
   * Returns an Iterable of Completions for enum tag usages.
   */
  def getCompletions(context: CompletionContext)(implicit root: TypedAst.Root): Iterable[UseEnumTagCompletion] = {
    //Need to return completion possibilities regardless of whether a tag was provided.
    stripWord(context) match {
      case Some(word) =>
        val segments = word.split('.').toList
        getUseEnumTagCompletionsWithTag(segments) ++ getUseEnumTagCompletionsNoTag(segments)
      case None => Nil
    }
  }

  /**
   * Returns an Iterable of completions for enum tag uses without a tag present.
   */
  private def getUseEnumTagCompletionsNoTag(segments: List[String])(implicit root: TypedAst.Root): Iterable[UseEnumTagCompletion] = {
    val sym = mkEnumSym(segments)
    root.enums.get(sym) match {
      case Some(enm) =>
        enm.cases.map {
          case (_, cas) => UseEnumTagCompletion(sym, cas)
        }
      case None => Nil
    }
  }

  /**
   * Returns an Iterable of completions for enum tag usages with tags present.
   */
  private def getUseEnumTagCompletionsWithTag(segments: List[String])(implicit root: TypedAst.Root): Iterable[UseEnumTagCompletion] = {
    if (segments.isEmpty) {
      return Nil
    }

    //Create a new tag that matches the user's syntax.
    val tag = segments.takeRight(1).mkString
    val withoutTag = segments.dropRight(1)
    val sym = mkEnumSym(withoutTag)

    root.enums.get(sym) match {
      case Some(enm) =>
        enm.cases.flatMap {
          case (cSym, cas) =>
            if (matches(cSym, tag)) {
              Some(UseEnumTagCompletion(sym, cas))
            }
            else {
              None
            }
        }
      case None => Nil
    }
  }

  /**
   * Strips `context` to only include text after "use..."
   *
   * @param ctx the completion context provided by the user
   * @return Some stripped String if the context matches the required input format, otherwise None
   */
  private def stripWord(ctx: CompletionContext): Option[String] = {
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(word) => Some(word)
      case _ => None
    }
  }

  /**
   * Returns true if `tag` matches the provided case.
   *
   * @param sym the CaseSym of the Enum case to compare.
   * @param tag the provided tag.
   * @return true if the provied tag matches `sym`, and false otherwise.
   */
  private def matches(sym: Symbol.CaseSym, tag: String): Boolean = sym.name.startsWith(tag)

  /**
   * Returns an updated EnumSym.
   *
   * Symbol.mkEnumSym("A.B.C.Color") would yield a namespace of "A" and a name of "B.C.Color".
   *
   * Conversely, this function would yield a namespace of "A.B.C" and a name of "Color".
   *
   * @param segment the name as a List[String], segmented by `.`.
   * @return the correctly represented Enum Sym with the full name.
   */
  private def mkEnumSym(segment: List[String]): Symbol.EnumSym = {
    if (segment.isEmpty) {
      new Symbol.EnumSym(Nil, "", SourceLocation.Unknown)
    } else {
      val ns = segment.dropRight(1)
      val name = segment.takeRight(1).mkString
      new Symbol.EnumSym(ns, name, SourceLocation.Unknown)
    }
  }

}
