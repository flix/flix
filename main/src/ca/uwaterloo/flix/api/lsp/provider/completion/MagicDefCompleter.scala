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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.language.ast.shared.{QualifiedSym, Scope}
import ca.uwaterloo.flix.language.ast.{Name, RigidityEnv, SourceLocation, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2

object MagicDefCompleter {

  /**
    * A completer for expressions like:
    *
    * {{{
    *   foo.bar
    * }}}
    *
    * where foo is an expression that has an enum or struct type and bar is a prefix of a def
    * in the companion module of the enum or struct.
    *
    * For example:
    *
    * {{{
    *   let l = List.range(1, 100);
    *   l.f
    * }}}
    *
    * would propose:
    *
    * - List.filter(...)
    * - List.fold(...)
    *
    * and so on.
    */
  def getCompletions(ident: Name.Ident, tpe: Type, range: Range, loc: SourceLocation, root: TypedAst.Root)(implicit flix: Flix): Iterable[Completion] = {
    val prefix = ident.name // the incomplete def name, i.e. the "bar" part.
    val baseExp = loc.text.getOrElse("") // the expression, but as a string, i.e. the "foo" part.

    // Lookup defs for types (i.e. enums and structs) that have companion modules.
    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) => getCompletionsForSym(sym, prefix, baseExp, tpe, range, root)
      case Some(TypeConstructor.Struct(sym, _)) => getCompletionsForSym(sym, prefix, baseExp, tpe, range, root)
      case _ => Nil
    }
  }

  /**
    * Returns the relevant def completions for the given qualified symbol (an enum or struct).
    */
  private def getCompletionsForSym(sym: QualifiedSym, prefix: String, baseExp: String, tpe: Type, range: Range, root: TypedAst.Root)(implicit flix: Flix): Iterable[Completion] = {
    val matchedDefs = root.defs.values.filter {
      case defn =>
        inCompanionMod(sym, defn) &&                            // Include only defs in the companion module.
          CompletionUtils.isAvailable(defn.spec) &&             // Include only defs that are public.
          CompletionUtils.fuzzyMatch(prefix, defn.sym.text) &&  // Include only defs that fuzzy match what the user has written.
          expMatchesLastArgType(tpe, defn.spec, root)           // Include only defs whose last parameter type unifies with the expression type.
    }

    matchedDefs.map {
      case defn =>
        val label = baseExp + "." + defn.sym.text // VSCode requires the code to be a prefix of the label.
        val snippet = getSnippet(defn.sym, defn.spec.fparams.init, baseExp)
        Completion.MagicDefCompletion(defn, label, snippet, range, Priority.Lower(0))
    }
  }

  /**
    * Returns `true` if the given `defn` is in the companion module of `sym`.
    */
  private def inCompanionMod(sym: QualifiedSym, defn: TypedAst.Def): Boolean = sym.namespace ::: sym.name :: Nil == defn.sym.namespace

  /**
    * Returns `true` if the given expression type `tpe` unifies with the last parameter type of the given `spec`.
    */
  private def expMatchesLastArgType(tpe: Type, spec: TypedAst.Spec, root: TypedAst.Root)(implicit flix: Flix): Boolean = {
    spec.fparams.lastOption match {
      case Some(lastParam) =>
        ConstraintSolver2.fullyUnify(tpe, lastParam.tpe, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix).isDefined
      case None => false
    }
  }

  /**
    * Returns a string of the form:
    *
    * {{{
    *   A.B.C.f({1:?arg1}, {2:?arg2}, lastArg)
    * }}}
    */
  private def getSnippet(sym: QualifiedSym, fparamsExceptLast: List[TypedAst.FormalParam], lastArg: String): String = {
    if (fparamsExceptLast.isEmpty) {
      s"$sym($lastArg)"
    } else {
      val argsWithHoles = fparamsExceptLast.zipWithIndex.map {
        case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
      }
      s"$sym(${argsWithHoles.mkString(", ")}, $lastArg)"
    }
  }

}
