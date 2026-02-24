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
    * where foo is an expression that has an enum, struct, or primitive type and bar is a prefix
    * of a def in the companion module of the type.
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
    * and similarly:
    *
    * {{{
    *   let x = 42;
    *   x.bi
    * }}}
    *
    * would propose:
    *
    * - Int32.bitCount(x)
    *
    * and so on.
    */
  def getCompletions(ident: Name.Ident, tpe: Type, range: Range, loc: SourceLocation, root: TypedAst.Root)(implicit flix: Flix): Iterable[Completion] = {
    val prefix = ident.name // the incomplete def name, i.e. the "bar" part.
    val baseExp = loc.text.getOrElse("") // the expression, but as a string, i.e. the "foo" part.

    // Lookup defs for types (i.e. enums, structs, and primitives) that have companion modules.
    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) =>
        getCompletionsForModule(sym.namespace ::: sym.name :: Nil, prefix, baseExp, tpe, range, root)
      case Some(TypeConstructor.Struct(sym, _)) =>
        getCompletionsForModule(sym.namespace ::: sym.name :: Nil, prefix, baseExp, tpe, range, root)
      case Some(tc) =>
        primitiveCompanionModule(tc) match {
          case Some(ns) => getCompletionsForModule(ns, prefix, baseExp, tpe, range, root)
          case None     => Nil
        }
      case None => Nil
    }
  }

  /**
    * Returns the relevant def completions for the given module namespace.
    */
  private def getCompletionsForModule(moduleNamespace: List[String], prefix: String, baseExp: String, tpe: Type, range: Range, root: TypedAst.Root)(implicit flix: Flix): Iterable[Completion] = {
    val matchedDefs = root.defs.values.filter {
      case defn =>
        defn.sym.namespace == moduleNamespace &&                // Include only defs in the companion module.
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
    * Returns the companion module namespace for the given primitive type constructor, if any.
    */
  private def primitiveCompanionModule(tc: TypeConstructor): Option[List[String]] = tc match {
    case TypeConstructor.Bool       => Some(List("Bool"))
    case TypeConstructor.Char       => Some(List("Char"))
    case TypeConstructor.Float32    => Some(List("Float32"))
    case TypeConstructor.Float64    => Some(List("Float64"))
    case TypeConstructor.BigDecimal => Some(List("BigDecimal"))
    case TypeConstructor.Int8       => Some(List("Int8"))
    case TypeConstructor.Int16      => Some(List("Int16"))
    case TypeConstructor.Int32      => Some(List("Int32"))
    case TypeConstructor.Int64      => Some(List("Int64"))
    case TypeConstructor.BigInt     => Some(List("BigInt"))
    case TypeConstructor.Str        => Some(List("String"))
    case TypeConstructor.Regex      => Some(List("Regex"))
    case _                          => None
  }

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
