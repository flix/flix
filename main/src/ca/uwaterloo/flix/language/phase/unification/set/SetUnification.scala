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
import ca.uwaterloo.flix.util.Result

import scala.collection.mutable

object SetUnification {

  /**
    * The static parameters of set unification.
    *
    * @param sizeThreshold if positive, [[solve]] will give up before SVE if there are more
    *                      equations than this
    * @param permutationLimit if positive, the maximum number of permutations that
    *                         [[svePermutations]] will try
    * @param sveRecSizeThreshold if positive, [[sve]] will give up on formulas beyond this size
    * @param svePermutationExitSize [[svePermutations]] will stop searching early if it finds a
    *                               substitution smaller than this
    */
  final case class Options(
                            sizeThreshold: Int,
                            permutationLimit: Int,
                            sveRecSizeThreshold: Int,
                            svePermutationExitSize: Int
                          )

  final object Options {
    /** The default [[Options]]. */
    val default: Options = Options(10, 10, 100_000, 0)
  }

  /** Represents the running mutable state of the solver. */
  final class State(initialEquations: List[Equation]) {
    /** The remaining equations to solve. */
    var eqs: List[Equation] = initialEquations
    /** The current substitution, which has already been applied to `eqs`. */
    var subst: SetSubstitution = SetSubstitution.empty
  }

  /**
    * A listener that observes the operations of [[solve]].
    *
    *   - `onEnterPhase(phaseName: String, state: State): Unit`
    *   - `enExitPhase(state: State): Unit`
    */
  final case class SolverListener(
                                   onEnterPhase: (String, State) => Unit,
                                   onExitPhase: (State, Boolean) => Unit,
                                   onSveRecCall: SetFormula => Unit
                                 )

  final object SolverListener {

    /** The [[SolverListener]] that does nothing. */
    val doNothing: SolverListener = SolverListener(
      (_, _) => (),
      (_, _) => (),
      _ => ()
    )

    def stringListener(p: String => Unit): SolverListener = {
      SolverListener(
        onEnterPhase = (phaseName, _) => p(s"Phase: $phaseName"),
        onExitPhase = (state, progress) => if (progress) p(stateString(state.eqs, state.subst)),
        onSveRecCall = f => p(s"sve call: $f")
      )
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
  def solve(l: List[Equation])(implicit listener: SolverListener, opts: Options): (List[Equation], SetSubstitution) = {
    val state = new State(l)
    val trivialPhaseName = "Trivial Equations"

    runWithState(state, runRule(constantAssignment), "Constant Assignment")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, runRule(variableAlias), "Variable Aliases")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, runRule(variableAssignment), "Simple Variable Assignment")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, duplicatedAndReflective, "Duplicates and Reflective")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, assertSveEquationCount, "Assert Size")
    runWithState(state, svePermutations, "SVE")

    // Experiment with Zhegalkin polynomials.
    //        for ((_, f) <- state.subst.m) {
    //          f match {
    //            case SetFormula.Empty => // nop
    //            case SetFormula.Var(_) => // nop
    //            case SetFormula.ElemSet(_) => // nop
    //            case SetFormula.Cst(_) => // nop
    //            case _ =>
    //              def withBound(s: String, b: Int): String = {
    //                val len = s.length
    //                if (len < b) s else s.substring(0, b - 3) + s"... ${len - (b + 3)} more"
    //              }
    //
    //              val z = Zhegalkin.toZhegalkin(f)
    //              val s1 = withBound(f.toString, 100)
    //              val s2 = withBound(z.toString, 100)
    //              println(f"$s1%100s -- $s2")
    //          }
    //        }

    (state.eqs, state.subst)
  }

  /** Marks all equations as [[Equation.Status.Timeout]] if there are more than [[Options.sizeThreshold]]. */
  private def assertSveEquationCount(eqs: List[Equation])(implicit opts: Options): Option[(List[Equation], SetSubstitution)] = {
    if (opts.sizeThreshold > 0 && eqs.length > opts.sizeThreshold) {
      val errMsg = s"Amount of leftover equations for SVE (${eqs.length}) is over the threshold (${opts.sizeThreshold})."
      Some(eqs.map(_.toTimeout(errMsg)), SetSubstitution.empty)
    } else None
  }

  /**
    * Runs the given equation system solver `phase` on `state`, printing debugging information
    * according to [[Options]].
    */
  private def runWithState(state: State, phase: List[Equation] => Option[(List[Equation], SetSubstitution)], phaseName: String)(implicit listener: SolverListener): Unit = {
    listener.onEnterPhase(phaseName, state)

    phase(state.eqs) match {
      case Some((eqs, subst)) =>
        state.eqs = eqs
        state.subst = subst @@ state.subst
        listener.onExitPhase(state, true)

      case None =>
        listener.onExitPhase(state, false)
    }
  }

  /**
    * Eliminates redundant equations
    *   - equations that occur multiple times
    *   - `f ~ f` (reflective, syntactically trivial)
    */
  private def duplicatedAndReflective(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
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

    if (changed) Some(result.reverse, SetSubstitution.empty) else None
  }

  /** Solves `eqs` with [[sve]], trying multiple different orderings to minimize substitution size. */
  private def svePermutations(eqs0: List[Equation])(implicit listener: SolverListener, opts: Options): Option[(List[Equation], SetSubstitution)] = {
    val eqs = eqs0.map{
      case Equation(f1, f2, status, loc) =>
        Equation.mk(selectiveExponentialForm(f1), selectiveExponentialForm(f2), loc, status)
    }
    // We solve the first `permutationLimit` permutations of `eqs` and pick the one that
    // both successfully solves it and has the smallest substitution.
    val permutations = if (opts.permutationLimit > 0) eqs.permutations.take(opts.permutationLimit) else eqs.permutations
    var bestEqs: List[Equation] = Nil
    var bestSubst = SetSubstitution.empty
    var bestSize = -1

    def noPreviousPermutation(): Boolean = bestSize == -1

    // Go through the permutations, tracking the best one.
    var stop = false
    for (s <- permutations.map(runRule(sve)) if !stop) s match {
      case Some((ruleEqs, s)) =>
        val firstSolution = bestEqs.nonEmpty && ruleEqs.isEmpty
        val sSize = s.totalFormulaSize
        val smallestSubstitution = sSize < bestSize
        if (noPreviousPermutation() || firstSolution || smallestSubstitution) {
          bestEqs = ruleEqs
          bestSize = sSize
          bestSubst = s
          // If we have a solution and it is below the good-enough threshold of opts, exit early.
          if (bestEqs.isEmpty && bestSize <= opts.svePermutationExitSize) stop = true
        }
      case None => ()
    }
    if (noPreviousPermutation()) None
    else Some(bestEqs, bestSubst)
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
      Some(resultEqs, overallSubst)
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

    def error(): Option[(List[Equation], SetSubstitution)] = {
      Some(List(eq.toUnsolvable), SetSubstitution.empty)
    }

    def success(): Option[(List[Equation], SetSubstitution)] = {
      Some(Nil, SetSubstitution.empty)
    }

    (f1, f2) match {
      // Equations that are solved.
      case (Univ, Univ) => success()
      case (Empty, Empty) => success()
      case (Cst(c1), Cst(c2)) if c1 == c2 => success()
      case (Var(x1), Var(x2)) if x1 == x2 => success()
      case (ElemSet(e1), ElemSet(e2)) if e1 == e2 => success()
      // Equations that are Unsolvable.
      case (Univ, Empty) => error()
      case (Univ, ElemSet(_)) => error()
      case (Univ, Cst(_)) => error()
      case (Empty, Univ) => error()
      case (Empty, ElemSet(_)) => error()
      case (Empty, Cst(_)) => error()
      case (ElemSet(_), Univ) => error()
      case (ElemSet(e), Empty) if e.nonEmpty => error()
      case (ElemSet(i1), ElemSet(i2)) if i1 != i2 => error()
      case (ElemSet(_), Cst(_)) => error()
      case (Cst(_), Univ) => error()
      case (Cst(_), Empty) => error()
      case (Cst(_), ElemSet(_)) => error()
      case (Cst(c1), Cst(c2)) if c1 != c2 => error()
      // Equations that are not trivial.
      case _ => None // Cannot do anything.
    }
  }

  /**
    * Solves equations of ground assignments to variables (e.g. `x ~ c1 ∪ e2`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> f])`
    *   - `!x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> !f])`
    *   - `f1 ∩ f2 ∩ .. ~ univ` becomes `({f1 ~ univ, f2 ~ univ, ..}, [])`
    *   - `f1 ∪ f2 ∪ .. ~ empty` becomes `({f1 ~ empty, f2 ~ empty, ..}, [])`
    *   - `f1 ~ f2` where [[SetFormula.isGround]] is true on both sides, becomes `({}, [])` if it
    *     holds or `({f1 ~error f2}, [])` if it does not.
    *
    * This also applies to the symmetric equations.
    */
  private def constantAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      // x ~ f, where f is ground
      // ---
      // {},
      // [x -> f]
      case (Var(x), f) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // Symmetric case.
      case (f, Var(x)) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // !x ~ f, where f is ground
      // ---
      // {},
      // [x -> !f]
      case (Compl(Var(x)), f) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // Symmetric case.
      case (f, Compl(Var(x))) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // f1 ∩ f2 ∩ .. ~ univ
      // ---
      // {f1 ~ univ, f2 ~ univ, ..},
      // []
      case (inter@Inter(_, _, _, _, _, _, _), Univ) =>
        val eqs = inter.mapSubformulas(Equation.mk(_, Univ, loc))
        Some(eqs, SetSubstitution.empty)

      // Symmetric case.
      case (Univ, inter@Inter(_, _, _, _, _, _, _)) =>
        val eqs = inter.mapSubformulas(Equation.mk(_, Univ, loc))
        Some(eqs, SetSubstitution.empty)

      // f1 ∪ f2 ∪ .. ~ empty
      // ---
      // {f1 ~ empty, f2 ~ empty, ..},
      // []
      case (union@Union(_, _, _, _, _, _, _), Empty) =>
        val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
        Some(eqs, SetSubstitution.empty)

      // Symmetric Case.
      case (Empty, union@Union(_, _, _, _, _, _, _)) =>
        val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
        Some(eqs, SetSubstitution.empty)

      // f1 ~ f2, where f1 and f2 are ground
      // ---
      // {}, [] if solved
      // {f1 ~error f2}, [] if unsolvable
      case (f1, f2) if f1.isGround && f2.isGround =>
        if (isEquivalent(f1, f2)) {
          Some(Nil, SetSubstitution.empty)
        } else {
          Some(List(eq.toUnsolvable), SetSubstitution.empty)
        }

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
    *   - `!x1 ~ !x1` becomes `({}, [])`
    *   - `!x1 ~ !x2` becomes `({}, [x1 -> x2])`
    *
    * There is a binding-bias towards lower variables, such that `x1 ~ x2` and `x2 ~ x1` both
    * become `({}, [x1 -> x2])`.
    */
  private def variableAlias(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      // x1 ~ x1
      // ---
      // {},
      // []
      case (Var(x), Var(y)) if x == y =>
        Some(Nil, SetSubstitution.empty)

      // x1 ~ x2
      // ---
      // {},
      // [x1 -> x2]
      case (x0@Var(_), y0@Var(_)) =>
        val (x, y) = if (x0.x > y0.x) (y0, x0) else (x0, y0)
        Some(Nil, SetSubstitution.singleton(x.x, y))

      // !x1 ~ !x1
      // ---
      // {},
      // []
      case (Compl(Var(x)), Compl(Var(y))) if x == y =>
        Some(Nil, SetSubstitution.empty)

      // !x1 ~ !x2
      // ---
      // {},
      // [x1 -> x2]
      case (Compl(x0@Var(_)), Compl(y0@Var(_))) =>
        // Make this rule stable on symmetric equations.
        val (x, y) = if (x0.x < y0.x) (x0, y0) else (y0, x0)
        Some(Nil, SetSubstitution.singleton(x.x, y))

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
    *   - `!x ~ f` where `f` does not contain `x` becomes `({}, [x -> !f])`
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
        Some(Nil, SetSubstitution.singleton(x, f))

      // Symmetric case.
      case (f, v@Var(x)) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // !x ~ f, where f does not contain x
      // ---
      // {},
      // [x -> !f]
      case (Compl(v@Var(x)), f) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // Symmetric case.
      case (f, Compl(v@Var(x))) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Solves equations using successive-variable-elimination, i.e. exhaustive instantiation.
    *
    * SVE can always make progress, so [[None]] is never returned.
    *
    * Always returns no equations or `eq` marked as [[Equation.Status.Unsolvable]] or
    * [[Equation.Status.Timeout]].
    */
  private def sve(eq: Equation)(implicit listener: SolverListener, opts: Options): Option[(List[Equation], SetSubstitution)] = {
    val query = mkEmptyQuery(eq.f1, eq.f2)
    val fvs = query.variables.toList
    try {
      val subst = successiveVariableElimination(query, fvs)
      Some(Nil, subst)
    } catch {
      case NoSolutionException() => Some(List(eq.toUnsolvable), SetSubstitution.empty)
      case ComplexException(msg) => Some(List(eq.toTimeout(msg)), SetSubstitution.empty)
    }
  }

  /**
    * The Successive Variable Elimination algorithm.
    *
    * Returns the most-general unifier of the equation `f ~ empty` where `fvs` is the free
    * variables in `f`. If there is no unifier then [[NoSolutionException]] is thrown.
    *
    * Eliminates variables recursively from `fvs`.
    *
    * If the formula that is recursively built is ever larger than `recSizeThreshold` then
    * [[ComplexException]] is thrown. If `recSizeThreshold` is non-positive then there is no
    * checking.
    */
  private def successiveVariableElimination(f: SetFormula, fvs: List[Int])(implicit listener: SolverListener, opts: Options): SetSubstitution = fvs match {
    case Nil =>
      // `fvs` is empty so `f` has no variables.
      // The remaining constants are rigid so `f` has to be empty no matter their instantiation.
      // Return the empty substitution if `f` is equivalent to `empty`.
      if (isEmptyEquivalent(f)) SetSubstitution.empty
      else throw NoSolutionException()

    case x :: xs =>
      val f0 = SetSubstitution.singleton(x, Empty)(f)
      val f1 = SetSubstitution.singleton(x, Univ)(f)
      val recFormula = propagation(mkInter(f0, f1))
      listener.onSveRecCall(recFormula)
      assertSveRecSize(recFormula)
      val se = successiveVariableElimination(recFormula, xs)
      val xFormula = selectiveExponentialForm(propagation(mkUnion(se(f0), mkDifference(Var(x), se(f1)))))
      // We can safely use `unsafeExtend` because `xFormula` contains no variables and we only add
      // each variable of `fvs` once (which is assumed to have no duplicates).
      // `se`, `x`, and `xFormula` therefore have disjoint variables.
      se.unsafeExtend(x, xFormula)
  }

  /** Throws [[ComplexException]] if `f` is larger than [[Options.sveRecSizeThreshold]]. */
  private def assertSveRecSize(f: SetFormula)(implicit opts: Options): Unit = {
    if (opts.sveRecSizeThreshold > 0) {
      val fSize = f.size
      if (fSize > opts.sveRecSizeThreshold) throw ComplexException(
        s"SetFormula size ($fSize) is over recursive SVE threshold (${opts.sveRecSizeThreshold})."
      )
    }
  }

  /** Thrown by [[successiveVariableElimination]] to indicate that there is no solution. */
  private case class NoSolutionException() extends RuntimeException

  /**
    * Thrown to indicate that a [[SetFormula]], an [[Equation]], or a [[SetSubstitution]] is too
    * big.
    */
  private case class ComplexException(msg: String) extends RuntimeException

  //
  // Checking and Debugging.
  //

  /**
    * Verifies that `subst` is a solution to `eqs`.
    *
    * Note: Does not verify that `subst` is the most general solution.
    *
    * Note: This function is very slow since [[SetFormula.isEquivalent]] is very slow.
    */
  def verifySubst(eqs: List[Equation], subst: SetSubstitution): Result[Unit, String] = {
    // Apply the substitution to every equation and check that it is solved.
    for (e <- eqs) {
      // We want to check that `s(f1) ~ s(f2)`
      val f1 = subst.apply(e.f1)
      val f2 = subst.apply(e.f2)
      if (SetFormula.isEquivalent(f1, f2)) ()
      else {
        val msg =
          s"""  Incorrect Substitution:
             |  Original  : $e
             |  with Subst: ${Equation(f1, f2, e.status, e.loc)}
             |""".stripMargin
        return Result.Err(msg)
      }
    }
    Result.Ok(())
  }

  /** Returns a multiline string of the given [[Equation]]s and [[SetSubstitution]]. */
  def stateString(eqs: List[Equation], subst: SetSubstitution): String = {
    val sb = new StringBuilder()
    sb.append("Equations:\n")
    for (eq <- eqs) sb.append(s"  $eq\n")
    sb.append(subst)
    sb.append("\n")
    sb.toString
  }
}
