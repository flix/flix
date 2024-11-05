/*
 * Copyright 2024 Chenhao Gao
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

import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypeConstructor, TypedAst}
import scala.annotation.TypeConstraint

object MagicMatchCompleter {

  /**
    * Returns a list of Completions for match, triggered by expr.match.
    *
    * Example:
    * Given an identifier `x` of an enum type `Color` with cases `Red`, `Green`, and `Blue`,
    * typing `x.match` will trigger the completion to expand to:
    *
    * match x {
    *   case Red => ???
    *   case Green => ???
    *   case Blue => ???
    * }
    */
  def getCompletions(name: String, tpe: Type ,ctx: CompletionContext)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (!"match".startsWith(name)) return Nil

    val ident = extractIdentifier(ctx.word)
    getEnumSym(tpe) match {
      case Some(sym) =>
        val casesString = generateCasesString(root.enums(sym).cases)
        val matchExpr = s"match $ident {\n$casesString}"
        val prompt = s"$ident.match"
        Completion.SnippetCompletion(prompt, matchExpr, "Expand to a full match expression.") :: Nil
      case None => Nil
    }
  }

  /**
    * Generates the string representation of the cases of an enum.
    */
  private def generateCasesString(cases: Map[Symbol.CaseSym, TypedAst.Case]): String = {
    cases.toList.sortBy(_._1.loc).foldLeft(("", 1))({
      case ((acc, z), (sym, cas)) =>
        val (str, k) = cas.tpe.typeConstructor match {
          case Some(TypeConstructor.Unit) => (s"$sym => $${${z + 1}:???}", z + 1)
          case Some(TypeConstructor.Tuple(arity)) => (List.range(1, arity + 1)
            .map(elem => s"$${${elem + z}:_elem$elem}")
            .mkString(s"$sym(", ", ", s") => $${${arity + z + 1}:???}"), z + arity + 1)
          case _ => (s"$sym($${${z + 1}:_elem}) => $${${z + 2}:???}", z + 2)
        }
        (acc + "    case " + str + "\n", k)
    })._1
  }

  /**
    * Extract the substring before the last . as the identifier
    */
  private def extractIdentifier(word: String): String = {
    word.substring(0, word.lastIndexOf("."))
  }

  /**
   * Returns the enum symbol of the given type, if it is an enum.
   */
  private def getEnumSym(tpe: Type): Option[Symbol.EnumSym] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) => Some(sym)
    case _ => None
  }
}
