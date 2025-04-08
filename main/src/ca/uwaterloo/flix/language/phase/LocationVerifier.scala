/*
 * Copyright 2025 Chenhao Gao
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
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.{ChangeSet, SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.LocationError
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue

/**
  * The LocationVerifier verifies the locations of the Flix program.
  */
object LocationVerifier {
  def run(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (TypedAst.Root, List[LocationError]) = flix.phaseNew("LocationVerifier") {
    implicit val sctx: SharedContext = SharedContext.mk()
    val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef))
    (root.copy(defs = defs), List.empty[LocationError])
  }

  private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext): TypedAst.Def = {
    visitExp(defn.exp)
    defn
  }

  private def visitExp(exp0: Expr)(implicit sctx: SharedContext): Unit = exp0 match {
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      verifyParentContainment(loc, List(exp1.loc, exp2.loc, exp3.loc))
    case _ => ()
  }

  /**
    * Verifies that the parent location contains all the children locations.
    *
    * @param parentLoc        the location of the parent node.
    * @param childrenLocation the locations of the child nodes.
    */
  private def verifyParentContainment(parentLoc: SourceLocation, childrenLocation: List[SourceLocation])(implicit sctx: SharedContext): Unit =
    childrenLocation.foreach { loc =>
      if (!parentLoc.contains(loc)) {
        sctx.errors.add(LocationError.ChildOutOfBoundError(parentLoc, loc))
      }
    }

  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[LocationError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[LocationError])
}

