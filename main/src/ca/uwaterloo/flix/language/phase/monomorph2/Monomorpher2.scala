/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{MonoAst, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

/**
  * Entry point for constraint-based monomorphization, following the approach of "The Simple
  * Essence of Monomorphization" by Matthew Lutze, Philipp Schuster, and Jonathan Immanuel
  * Brachthäuser.
  *
  * At a high level, this pipeline works as follows:
  *
  *   - 1. [[ConstraintGen]] generates flow constraints describing how concrete types and
  *     type shapes propagate into the type-parameter slots of every polymorphic def/enum/struct/
  *     restrictable-enum.
  *   - 2. [[NonMonomorphizableCheck]] rejects programs with no finite solution (e.g. polymorphic
  *     recursion) before solving, so the next step cannot loop forever.
  *   - 3. [[ConstraintSolver]] solves the flow constraints to a fixpoint, producing the set of
  *     concrete instantiations each polymorphic symbol must be specialized at.
  *   - 4. [[Specialize]] specializes (and lowers) every def/enum/struct/
  *     restrictable-enum accordingly.
  *
  * Caution: step 4's lowering can synthesize references to specific stdlib defs/enums that step 1
  * would not otherwise have any reason to see. Any such construct needs its own constraints
  * generated in step 1, or it won't be in the solution by the time step 4 needs to specialize it.
  */
object Monomorpher2 {

  /** Performs constraint-based monomorphization of the given AST `root`. */
  def run(root: TypedAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Monomorpher2") {
    val constraints = ConstraintGen.generate(root)
    NonMonomorphizableCheck.checkMonomorphizable(constraints)
    val solution = ConstraintSolver.solve(constraints, root)
    Specialize.run(root, solution)
  }
}
