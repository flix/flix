/*
 * Copyright 2022 Matthew Lutze
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
import ca.uwaterloo.flix.api.lsp.{Entity, Index}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.phase.unification.Unification

object HoleCompletion {

  /**
    * Gets completions for when the cursor position is on a hole expression with an expression
    */
  def getHoleCompletion(ctx: CompletionContext, index: Index, root: TypedAst.Root)(implicit flix: Flix): Iterable[Completion] = {
    val entity = index.query(ctx.uri, ctx.pos)
    entity match {
      case Some(Entity.Exp(TypedAst.Expr.HoleWithExp(TypedAst.Expr.Var(sym, sourceType, _), targetType, _, loc))) =>
        HoleCompletion.candidates(sourceType, targetType, root)
          .map(root.defs(_))
          .filter(_.spec.mod.isPublic)
          .zipWithIndex
          .map { case (decl, idx) => Completion.HoleCompletion( sym, decl, f"$idx%09d", loc) }
      case _ => Nil
    }
  }

  /**
    * Returns the set of definitions whose final parameter type and return type match the given `sourceType` and `targetType`, respectively.
    *
    * For example, for source type `List[String]` and target type `String`,
    * the candidates would include `List.toString : List[a] -> String` and  `List.join : (String, List[String]) -> String`
    */
  private def candidates(sourceType: Type, targetType: Type, root: TypedAst.Root)(implicit flix: Flix): List[Symbol.DefnSym] = {
    // Top scope is used since we're comparing with declarations, which are at the top scope.
    val matchType = Type.mkArrowWithEffect(
      sourceType,
      Type.freshVar(Kind.Eff, SourceLocation.Unknown)(Scope.Top, flix),
      targetType,
      SourceLocation.Unknown
    )

    val matches = root.defs.values.flatMap {
      case TypedAst.Def(sym, spec, _, loc) =>
        val lastArrow = Type.mkArrowWithEffect(
          spec.fparams.last.tpe,
          spec.eff,
          spec.retTpe,
          SourceLocation.Unknown
        )
        // TODO modify to take renv as a parameter
        Unification.fullyUnifyTypes(matchType, lastArrow, RigidityEnv.empty, root.eqEnv)(Scope.Top, flix) match {
          case Some(subst) =>
            // Track the size of all the types in the substitution.
            // A smaller substitution means a more precise unification match.
            val size = subst.m.values.map(_.size).sum
            Some((sym, spec, size))
          case None =>
            None
        }
    }.toList

    //
    // Sort the matched symbols by:
    // - The size of the generated substitution (smaller is better) followed by:
    // - The number of parameters (fewer is better) followed by:
    // - The number of type variables (fewer is better) followed by:
    // - The symbol.
    //
    matches.sortBy {
      case (sym, spec, size) => (size, spec.fparams.length, spec.declaredScheme.quantifiers.length, sym.toString)
    } map {
      case (sym, _, _) => sym
    }
  }

}
