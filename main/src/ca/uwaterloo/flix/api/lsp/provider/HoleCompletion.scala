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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.phase.unification.Unification

object HoleCompletion {

  /**
    * Returns the set of definitions whose final parameter type and return type match the given `sourceType` and `targetType`, respectively.
    *
    * For example, for source type `List[String]` and target type `String`,
    * the candidates would include `List.toString : List[a] -> String` and  `List.join : (String, List[String]) -> String`
    */
  def candidates(sourceType: Type, targetType: Type, root: TypedAst.Root)(implicit flix: Flix): Set[Symbol.DefnSym] = {
    val matchType = Type.mkArrowWithEffect(
      sourceType,
      Type.freshVar(Kind.Bool, SourceLocation.Unknown),
      Type.freshVar(Kind.Effect, SourceLocation.Unknown),
      targetType,
      SourceLocation.Unknown
    )

    root.defs.values.flatMap {
      case TypedAst.Def(sym, spec, _) =>
        val lastArrow = Type.mkArrowWithEffect(
          spec.fparams.last.tpe,
          spec.pur,
          spec.eff,
          spec.retTpe,
          SourceLocation.Unknown
        )
        // TODO modify to take renv as a parameter
        if (Unification.unifiesWith(matchType, lastArrow, RigidityEnv.empty)) {
          Some(sym)
        } else {
          None
        }
    }.toSet
  }
}
