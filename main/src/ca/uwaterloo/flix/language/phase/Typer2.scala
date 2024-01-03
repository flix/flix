/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Validation.{flatMapN, mapN}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListMap

object Typer2 {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.phase("Typer") {
    flatMapN(TypeInference.run(root, oldRoot, changeSet)) {
      case (defSubsts, sigSubsts) =>
        TypeReconstruction.run(root, defSubsts, sigSubsts, oldRoot, changeSet)
    }
  }

  /**
    * Reconstructs types in the given defs.
    */
  private def visitDefs(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    flix.subphase("Defs") {
      val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)
      ParOps.parTraverseValues(staleDefs) {
        case defn => visitDef(defn, root, classEnv, eqEnv)
      }.map(_ ++ freshDefs)
    }
  }

  /**
    * Reconstructs types in the given def.
    */
  private def visitDef(defn: KindedAst.Def, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = {
    val substVal = TypeInference.visitDefn(defn, assumedTconstrs = Nil, root, classEnv, eqEnv)
    mapN(substVal) {
      case subst => TypeReconstruction.visitDef(defn, root, subst)
    }
  }
}
