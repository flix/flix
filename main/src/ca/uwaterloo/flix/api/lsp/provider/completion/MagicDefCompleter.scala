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
import ca.uwaterloo.flix.language.ast.shared.QualifiedSym
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Type, TypeConstructor, TypedAst}

object MagicDefCompleter {

  def getCompletions(ident: Name.Ident, tpe: Type, range: Range, loc: SourceLocation, root: TypedAst.Root): Iterable[Completion] = {
    val prefix = ident.name
    val baseExp = loc.text.getOrElse("")

    // Lookup defs for types (i.e. enums and structs) that have companion modules.
    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) => getComp(sym, prefix, baseExp, range, root)
      case Some(TypeConstructor.Struct(sym, _)) => getComp(sym, prefix, baseExp, range, root)
      case _ => Nil
    }
  }

  private def getComp(sym: QualifiedSym, prefix: String, baseExp: String, range: Range, root: TypedAst.Root): Iterable[Completion] = {
    val matchedDefs = root.defs.values.filter {
      case defn => inCompanionMod(sym, defn) &&
        defn.spec.fparams.nonEmpty &&
        CompletionUtils.isAvailable(defn.spec) && // CompletionUtils.matchesName(decl.sym, qn, qualified = false)
        defn.sym.text.startsWith(prefix)
    }

    matchedDefs.map {
      case defn =>
        val label = baseExp + "." + defn.sym.text
        val snippet = getSnippet(defn.sym, defn.spec.fparams.init, baseExp)
        Completion.MagicDefCompletion(label, snippet, defn, range, Priority.Lower(0), qualified = false, inScope = false)
    }
  }

  /**
    * Returns `true` if the given `defn` is in the companion module of `sym`.
    */
  private def inCompanionMod(sym: QualifiedSym, defn: TypedAst.Def): Boolean = sym.namespace ::: sym.name :: Nil == defn.sym.namespace

  /**
    * Returns a string of the form:
    *
    * {{{
    *   A.B.C.f({1:?arg1}, {2:?arg2}, lastArg)
    * }}}
    */
  private def getSnippet(sym: QualifiedSym, fparams: List[TypedAst.FormalParam], lastArg: String): String = {
    val argsWithHoles = fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
    }
    s"$sym(${argsWithHoles.mkString(", ")}, $lastArg)"
  }

}

