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

object UseEnumTagCompleter extends Completer {
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[UseEnumTagCompletion] = {
    stripWord(context) match {
      case Some(word) => {
        val segments = word.split('.').toList
        getUseEnumTagCompletionsWithTag(segments) ++ getUseEnumTagCompletionsNoTag(segments)
      }
      case None => Nil
    }
  }

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

  private def getUseEnumTagCompletionsWithTag(segments: List[String])(implicit root: TypedAst.Root): Iterable[UseEnumTagCompletion] = {
    if (segments.isEmpty) {
      Nil
    }
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

  private def stripWord(ctx: CompletionContext): Option[String] = {
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(word) => Some(word)
      case _ => None
    }
  }

  private def matches(sym: Symbol.CaseSym, tag: String): Boolean = sym.name.startsWith(tag)

  private def mkEnumSym(segment: List[String]): Symbol.EnumSym = {
    if (segment.isEmpty) {
      new Symbol.EnumSym(None, Nil, "", SourceLocation.Unknown)
    } else {
      val ns = segment.dropRight(1)
      val name = segment.takeRight(1).mkString
      new Symbol.EnumSym(None, ns, name, SourceLocation.Unknown)
    }
  }

}
