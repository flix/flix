/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.{Flix, FlixEvent}
import ca.uwaterloo.flix.language.ast.shared.TraitConstraint
import ca.uwaterloo.flix.language.ast.{KindedAst, RigidityEnv}
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.Typer.checkAssocTypes
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolverInterface, InfResult}
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, TraitEnv}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.ConcurrentLinkedQueue

class TestTypeSimplifier extends AnyFunSuite with TestUtils {

  test("TypeCheckWithSimplifier") {
    val constraints = new ConcurrentLinkedQueue[(KindedAst.Def, InfResult, RigidityEnv, List[TraitConstraint], TraitEnv, EqualityEnv, KindedAst.Root)]()
    implicit val flix: Flix = new Flix().setOptions(Options.DefaultTest)
    flix.addListener {
      case FlixEvent.NewConstraintsDef(defn, infResult, renv, tconstrs, tenv, eqEnv, root) =>
        constraints.add((defn, infResult, renv, tconstrs, tenv, eqEnv, root))
      case _ => ()
    }
    expectSuccess(flix.compile())

    constraints.forEach {
      case (defn, infResult0, renv, tconstrs, tenv, eqEnv, root) =>
        val infResult = infResult0.copy(constrs = infResult0.constrs.map(TypeSimplifier.simplifyConstraint))
        val (_, errs) = ConstraintSolverInterface.visitDef(defn, infResult, renv, tconstrs, tenv, eqEnv, root)
        // checkAssocTypes should also be checked but that is a private method
        if (errs.nonEmpty) {
          fail(s"Expected no type checking errors, found ${errs.size} errors (first is: ${errs.head.summary})")
        }

    }
  }

}
