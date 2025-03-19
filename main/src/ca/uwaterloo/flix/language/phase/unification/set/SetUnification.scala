/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, BoolUnificationException, SveAlgorithm}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra, ZhegalkinExpr}
import ca.uwaterloo.flix.util.Result

import scala.collection.immutable.IntMap
import scala.collection.mutable

object SetUnification {

  /**
   * The maximum number of variables an equation may contain before it is considered too complex.
   */
  val MaxVars: Int = 12 // Up to 2^12 = 4,096 terms per Zhegalkin polynomial.

  /**
    * Enable simple rewrite rules.
    */
  var EnableRewriteRules: Boolean = true

  /**
    * Enables simple statistics tracking.
    */
  var EnableStats: Boolean = false

  /**
    * Tracks the number of constraints eliminated by each rewrite rule.
    */
  val ElimPerRule: mutable.Map[Phase, Int] = mutable.Map.empty

  /**
   * Tracks the number of variables eliminated by each rewrite rule.
   */
  val VarElimPerRule: mutable.Map[Phase, Int] = mutable.Map.empty

  /**
    * Represents the name of phase.
    */
  sealed trait Phase

  object Phase {
    final case object ConstantPropagation extends Phase

    final case object VariablePropagation extends Phase

    final case object VariableAssignment extends Phase

    final case object ReflexiveAndDuplicate extends Phase

    final case object SuccessiveVariableElimination extends Phase

    final case object Trivial extends Phase
  }

  /** Represents the running mutable state of the solver. */
  final class State(initialEquations: List[Equation]) {
    /** The remaining equations to solve. */
    var eqs: List[Equation] = initialEquations
    /** The current substitution, which has already been applied to `eqs`. */
    var subst: SetSubstitution = SetSubstitution.empty
  }

  /** A listener that observes the operations of [[solve]]. */
  sealed trait SolverListener {
    /** Is called before a unification phase starts. */
    def onEnterPhase(phaseName: String, state: State): Unit = ()

    /** Is called when a unification phase completes. If it made progress, `state` is `true`. */
    def onExitPhase(state: State, progress: Boolean): Unit = ()
  }

  final object SolverListener {

    /** The [[SolverListener]] that does nothing. */
    val DoNothing: SolverListener = new SolverListener {}

    def stringListener(p: String => Unit): SolverListener = new SolverListener {
      override def onEnterPhase(phaseName: String, state: State): Unit =
        p(s"Phase: $phaseName")

      override def onExitPhase(state: State, progress: Boolean): Unit =
        if (progress) p(stateString(state.eqs, state.subst))
    }
  }

  /**
    * Attempts to solve the equation system `eqs` to find the most general substitution.
    *
    * If the returned [[Equation]] list is empty, then the returned substitution is the most general
    * solution to `eqs`.
    *
    * If the returned [[Equation]] list is non-empty, then the returned substitution is the most general
    * solution to the removed equations.
    *
    * All returned equations are either marked [[Equation.Status.Unsolvable]] or
    * [[Equation.Status.Timeout]]. The returned equations might not exist in `eqs` directly, but
    * will be derived from it.
    */
  def solve(l: List[Equation])(implicit listener: SolverListener): (List[Equation], SetSubstitution) = {
    val state = new State(l)

    if (EnableRewriteRules) {
      runWithState(state, runRule(constantPropagation), Phase.ConstantPropagation)
      runWithState(state, runRule(trivial), Phase.Trivial)
      runWithState(state, runRule(variablePropagation), Phase.VariablePropagation)
      runWithState(state, runRule(trivial), Phase.Trivial)
      runWithState(state, runRule(variableAssignment), Phase.VariableAssignment)
      runWithState(state, runRule(trivial), Phase.Trivial)
      runWithState(state, reflexiveAndDuplicate, Phase.ReflexiveAndDuplicate)
      runWithState(state, runRule(trivial), Phase.Trivial)
    }

    sveAll(state.eqs) match {
      case Result.Ok(s) =>
        // Success: We solved all equations using SVE.
        // Mark all equations as solved and update the substitution.
        state.eqs = Nil
        state.subst = s @@ state.subst
      case Result.Err(eqs) =>
        // Failure: We found a conflict.
        // Store the unsolvable equations and reset the partial substitution.
        state.eqs = eqs
        state.subst = SetSubstitution.empty
    }

    (state.eqs, state.subst)
  }

  /**
    * Runs the given equation system solver `phase` on `state`.
    */
  private def runWithState(state: State, f: List[Equation] => Option[(List[Equation], SetSubstitution)], phase: Phase)(implicit listener: SolverListener): Unit = {
    listener.onEnterPhase(phase.toString, state)

    var numberOfVars: Int = 0
    if (EnableStats) {
      numberOfVars = state.eqs.map(_.varsOf.size).sum
    }

    f(state.eqs) match {
      case Some((eqs, subst)) =>

        if (EnableStats) {
          synchronized {
            {
              // Eliminated Constraints
              val count = ElimPerRule.getOrElse(phase, 0)
              val delta = state.eqs.length - eqs.length
              ElimPerRule.put(phase, count + delta)
            }

            {
              // Eliminated Vars
              val count = VarElimPerRule.getOrElse(phase, 0)
              val delta = numberOfVars - eqs.map(_.varsOf.size).sum
              VarElimPerRule.put(phase,  count + delta)
            }
          }
        }

        state.eqs = eqs
        state.subst = subst @@ state.subst
        listener.onExitPhase(state, progress = true)

      case None =>
        listener.onExitPhase(state, progress = false)
    }
  }

  /**
    * Eliminates redundant equations:
    *   - (Reflexive): Equations where the left and right are the same, i.e. `[f ~ f, l] -> [l]`.
    *   - (Duplicate): Equations that occur more than once, i.e. `[e, e, l] -> [e, l]`.
    *
    * Always succeeds.
    */
  private def reflexiveAndDuplicate(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
    var result: List[Equation] = Nil
    val seen = mutable.Set.empty[Equation]
    var changed = false

    for (eq <- eqs) {
      if (eq.f1 == eq.f2 || seen.contains(eq)) {
        // Don't add to result.
        changed = true
      } else {
        // Add to result, thus not making a change.
        seen.add(eq)
        result = eq :: result
      }
    }

    if (changed) Some((result.reverse, SetSubstitution.empty)) else None
  }

  /** Run a unification rule on an equation system in a fixpoint. */
  private def runRule(rule: Equation => Option[(List[Equation], SetSubstitution)])(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
    // Procedure:
    //   - Run the `rule` on all the (pending) equations in `iterationWorkList`, building
    //     `overallSubst` and producing `iterationOutput`.
    //   - If any rule made progress, store `iterationOutput` back into `iterationWorkList` and repeat.

    // The running substitution - applied to `iterationWorkList` equations lazily.
    var overallSubst = SetSubstitution.empty
    var overallProgress = false

    // The mutable state of the iteration.
    var iterationWorkList = eqs
    var iterationOutput: List[Equation] = Nil
    var iterationProgress = true // this is `true` to run the loop at least once.

    // Run iterations until there is no progress.
    while (iterationProgress) {
      iterationProgress = false

      for (eq0 <- iterationWorkList) {
        val eq = overallSubst.apply(eq0)
        // If `eq` is marked with error or timeout, don't try again.
        val appliedRule = if (eq.isPending) rule(eq) else None

        appliedRule match {
          case Some((outputEqs, outputSubst)) =>
            // We made progress - signal that and update state.
            iterationProgress = true
            overallProgress = true

            iterationOutput = outputEqs ++ iterationOutput
            overallSubst = outputSubst @@ overallSubst

          case None =>
            // No progress, just add to the output.
            iterationOutput = eq :: iterationOutput
        }
      }

      // Swap `iterationOutput` into `iterationWorkList` for next iteration or for return.
      iterationWorkList = iterationOutput
      iterationOutput = Nil
    }

    if (overallProgress) {
      // We apply `overallSubst` lazily, not all equations have seen all substitution information.
      val resultEqs = iterationWorkList.map(overallSubst.apply)
      Some((resultEqs, overallSubst))
    } else {
      None
    }
  }

  //
  // Rules.
  //

  /**
    * Solves equations that trivially hold (like `univ ~ univ`) and marks trivially unsolvable
    * equations (like `univ ~ empty`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `univ ~ univ` becomes `({}, [])`
    *   - `univ ~ empty` becomes `({univ ~error empty}, [])`
    *   - `x1 ~ x1` becomes `({}, [])`
    *   - `x2 ~ univ` becomes `({x2 ~ univ}, [])`
    */
  private def trivial(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq

    val success = Some((Nil, SetSubstitution.empty))
    val failure = Some((List(eq.toUnsolvable), SetSubstitution.empty))

    (f1, f2) match {
      // Equations that are trivial.
      case (Univ, Univ) => success
      case (Empty, Empty) => success
      case (Cst(c1), Cst(c2)) if c1 == c2 => success
      case (Var(x1), Var(x2)) if x1 == x2 => success
      case (ElemSet(e1), ElemSet(e2)) if e1 == e2 => success

      // Equations that are in conflict.
      // The cases here were determined by careful profiling.
      case (Empty, Cst(_)) => failure
      case (Cst(_), Empty) => failure
      case (ElemSet(_), Empty) => failure
      case (Empty, ElemSet(_)) => failure

      // Equations that are neither trivial nor in obvious conflict.
      case _ => None // Cannot do anything.
    }
  }

  /**
    * Solves equations of ground assignments to variables (e.g. `x ~ c1 ∪ e2`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> f])`
    *   - `f1 ∪ f2 ∪ .. ~ empty` becomes `({f1 ~ empty, f2 ~ empty, ..}, [])`
    *
    * The cases were determined by careful profiling.
    *
    * This also applies to the symmetric equations.
    */
  private def constantPropagation(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      // x ~ f, where f is ground
      // ---
      // {},
      // [x -> f]
      case (Var(x), f) if f.isGround =>
        Some((Nil, SetSubstitution.singleton(x, f)))

      // Symmetric case.
      case (f, Var(x)) if f.isGround =>
        Some((Nil, SetSubstitution.singleton(x, f)))

      // f1 ∪ f2 ∪ .. ~ empty
      // ---
      // {f1 ~ empty, f2 ~ empty, ..},
      // []
      case (Union(l), Empty) =>
        val eqs = l.toList.map(Equation.mk(_, Empty, loc))
        Some((eqs, SetSubstitution.empty))

      // Symmetric Case.
      case (Empty, Union(l)) =>
        val eqs = l.toList.map(Equation.mk(_, Empty, loc))
        Some((eqs, SetSubstitution.empty))

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Solves variable alias equations (e.g. `x1 ~ x2`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x1 ~ x1` becomes `({}, [])`
    *   - `x1 ~ x2` becomes `({}, [x1 -> x2])`
    *
    * The cases were determined by careful profiling.
    *
    * There is a binding-bias towards lower variables, such that `x1 ~ x2` and `x2 ~ x1` both become `({}, [x1 -> x2])`.
    */
  private def variablePropagation(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      // x1 ~ x1
      // ---
      // {},
      // []
      case (Var(x), Var(y)) if x == y =>
        Some((Nil, SetSubstitution.empty))

      // x1 ~ x2
      // ---
      // {},
      // [x1 -> x2]
      case (x0@Var(_), y0@Var(_)) =>
        val (x, y) = if (x0.x > y0.x) (y0, x0) else (x0, y0)
        Some((Nil, SetSubstitution.singleton(x.x, y)))

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Solves non-recursive variable assignments (e.g. `x1 ~ x2 ∪ c4`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ~ f` where `f` does not contain `x` becomes `({}, [x -> f])`
    *
    * This also applies to the symmetric equations.
    */
  private def variableAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      // x ~ f, where f does not contain x
      // ---
      // {},
      // [x -> f]
      case (v@Var(x), f) if !f.contains(v) =>
        Some((Nil, SetSubstitution.singleton(x, f)))

      // Symmetric case.
      case (f, v@Var(x)) if !f.contains(v) =>
        Some((Nil, SetSubstitution.singleton(x, f)))

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Attempts to solve all the given equations `eqs` using the SVE algorithm.
    *
    * Returns `Result.Ok(s)` with a complete substitution `s` if all equations were solved.
    * Returns `Result.Err(l)` with a list of unsolveable equations. (At least one equation is unsolveable.)
    */
  private def sveAll(eqs: List[Equation]): Result[SetSubstitution, List[Equation]] = {
    // Return immediately if there are no equations to solve.
    if (eqs.isEmpty) {
      return Result.Ok(SetSubstitution.empty)
    }

    // Return immediately if there already is an equation that is unsolvable (i.e. in conflict).
    if (eqs.exists(_.isUnsolvable)) {
      return Result.Err(eqs.map(_.toUnsolvable))
    }

    if (EnableStats) {
      // Eliminated Vars
      val count = VarElimPerRule.getOrElse(Phase.SuccessiveVariableElimination, 0)
      val delta = eqs.map(_.varsOf.size).sum
      VarElimPerRule.put(Phase.SuccessiveVariableElimination,  count + delta)
    }

    // Return immediately if there is an equation that has too many variables.
    for (eq <- eqs) {
      val allVars = eq.f1.varsOf ++ eq.f2.varsOf
      if (allVars.size > MaxVars) {
        return Result.Err(List(eq.toTimeout(s"Unification too complex: The equation contains ${allVars.size} variables which exceeds the limit of $MaxVars.")))
      }
    }

    // Convert all equations to Zhegalkin polynomials.
    implicit val alg: BoolAlg[ZhegalkinExpr] = ZhegalkinAlgebra
    val l = eqs.map {
      case Equation(f1, f2, _, _) =>
        val x = Zhegalkin.toZhegalkin(f1)
        val y = Zhegalkin.toZhegalkin(f2)
        (x, y)
    }

    // Solve *ALL* equations via SVE and obtain the substitution.
    try {
      val subst = SveAlgorithm.sveAll(l)

      // Reconstruct a set substitution.
      val m = subst.m.foldLeft(IntMap.empty[SetFormula]) {
        case (acc, (x, e)) => acc.updated(x, Zhegalkin.toSetFormula(e))
      }

      if (EnableStats) {
        val count = ElimPerRule.getOrElse(Phase.SuccessiveVariableElimination, 0)
        ElimPerRule.put(Phase.SuccessiveVariableElimination, count + eqs.length)
      }

      Result.Ok(SetSubstitution(m))
    } catch {
      case _: BoolUnificationException =>
        // SVE failed. We give up. We indiscriminately mark all equations as unsolvable.
        Result.Err(eqs.map(_.toUnsolvable))
    }
  }

  //
  // Checking and Debugging.
  //

  /** Returns a multiline string of the given [[Equation]]s and [[SetSubstitution]]. */
  private def stateString(eqs: List[Equation], subst: SetSubstitution): String = {
    val sb = new StringBuilder()
    sb.append("Equations:\n")
    for (eq <- eqs) sb.append(s"  $eq\n")
    sb.append(subst)
    sb.append("\n")
    sb.toString
  }
}
