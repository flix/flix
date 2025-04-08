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
  *
  * We currently enforce three invariants:
  *   - The location of the child node must be contained in the location of its parent node.
  *   - The locations of the nodes must be in appearance order.
  *   - The parent node and its last child node must have the same ending.
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
      verifyParentContainment(loc, List(exp1.loc, exp2.loc, exp3.loc))
      verifyAppearanceOrder(List(exp1.loc, exp2.loc, exp3.loc))
      verifySameEnding(loc, exp1.loc)
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
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
    * Verifies that the locations are in appearance order.
    *
    * @param locs the list of locations to verify, in the order of appearance.
    */
  private def verifyAppearanceOrder(locs: List[SourceLocation])(implicit sctx: SharedContext): Unit = {
    locs.sliding(2).foreach {
      case List(prevLoc, currLoc) =>
        if (!prevLoc.isBefore(currLoc)) {
          sctx.errors.add(LocationError.AppearanceOrderError(prevLoc, currLoc))
        }
      case _ => ()
    }
  }

  /**
    * Verifies that the parent location and the child location have the same ending.
    *
    * @param parentLoc the location of the parent node.
    * @param loc       the location of the child node.
    */
  private def verifySameEnding(parentLoc: SourceLocation, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    if (parentLoc.sp2 != loc.sp2) {
      sctx.errors.add(LocationError.DifferentEndingError(parentLoc, loc))
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

