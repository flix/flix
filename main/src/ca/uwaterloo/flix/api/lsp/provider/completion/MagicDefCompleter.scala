/*
 * Copyright 2025 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, QualifiedSym}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}

object MagicDefCompleter {

  def getCompletions(ident: Name.Ident, tpe: Type, range: Range, loc: SourceLocation, root: TypedAst.Root): Iterable[Completion] = {
    val prefix = ident.name
    val baseExp = loc.text.getOrElse("")

    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) => getComp(sym, prefix, baseExp, range, root)
      case Some(TypeConstructor.Struct(sym, _)) => getComp(sym, prefix, baseExp, range, root)
      case _ => Nil
    }
  }

  private def getComp(sym: QualifiedSym, prefix: String, baseExp: String, range: Range, root: TypedAst.Root): Iterable[Completion] = {
    val candidates = root.defs.values.collect {
      case defn if sym.namespace ::: sym.name :: Nil == defn.sym.namespace => defn
    }.filter {
      case defn => defn.spec.fparams.nonEmpty
    }.filter {
      case defn => CompletionUtils.isAvailable(defn.spec) //&& CompletionUtils.matchesName(decl.sym, qn, qualified = false)
    }.filter {
      case defn => defn.sym.text.startsWith(prefix)
    }

    candidates.map {
      case defn =>
        val label = baseExp + "." + defn.sym.text
        val snippet = getSnippet(defn.sym.toString, defn.spec.fparams.init, baseExp)
        Completion.MagicDefCompletion(label, snippet, defn, range, Priority.Lower(0), AnchorPosition(0, 0, 0), qualified = false, inScope = false, ExprContext.Unknown)
    }
  }

  private def getSnippet(name: String, fparams: List[TypedAst.FormalParam], lastArg: String): String = {
    val argsWithHoles = fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
    }
    s"$name(${argsWithHoles.mkString(", ")}, $lastArg)"
  }

}

