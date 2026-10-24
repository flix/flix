/*
 * Copyright 2024 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.api.CompilerConstants
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolUnificationException, CofiniteIntSet, SveAlgorithm, SveMode}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.TwoList

import scala.collection.immutable.{IntMap, SortedSet}
import scala.collection.mutable

object SetUnification {

  /**
   * The maximum number of variables an equation may contain before it is considered too complex.
   */
  val MaxVars: Int = CompilerConstants.MaxEffUnificationVars

  /** The maximum number of passes over the rule sequence. */
  private val MaxIterations: Int = 32

  /** The maximum number of equations that distribution may produce per system. */
  private val MaxSplits: Int = 512

  /** The maximum number of equations that one distribution may produce. */
  private val MaxSplitWidth: Int = 8

  /** The maximum size of a substitution range before the rewrite rules stop. */
  private val MaxRangeSize: Int = 256

  /** The maximum number of members of a (flattened) symmetric difference that is expanded. */
  private val MaxXorExpand: Int = 3

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

    final case object RecursiveAssignment extends Phase

    final case object BoundAssignment extends Phase

    final case object UnionAssignment extends Phase

    final case object ReflexiveAndDuplicate extends Phase

    final case object SuccessiveVariableElimination extends Phase

    final case object Trivial extends Phase

    final case object Simplify extends Phase

    final case object InterAssignment extends Phase

    final case object CoverAssignment extends Phase
  }

  /** Represents the running mutable state of the solver. */
  final class State(initialEquations: List[Equation]) {
    /** The remaining equations to solve. */
    var eqs: List[Equation] = initialEquations
    /** The current substitution, which has already been applied to `eqs`. */
    var subst: SetSubstitution = SetSubstitution.empty
    /** The number of equations produced by distribution so far (see [[MaxSplits]]). */
    var splits: Int = 0

    /**
      * For each variable bound by the substitution built so far, the other variables that occur in
      * its range. Used to refuse bindings that would make ranges depend on each other cyclically,
      * since composing such bindings grows the substitution exponentially.
      */
    val deps: mutable.Map[Int, Set[Int]] = mutable.Map.empty

    /** Returns `true` if `target` is reachable from `from` in the dependency graph. */
    def reaches(from: Int, target: Int): Boolean = {
      val seen = mutable.Set.empty[Int]
      var work = List(from)
      while (work.nonEmpty) {
        val v = work.head
        work = work.tail
        if (v == target) return true
        if (seen.add(v)) {
          work = deps.getOrElse(v, Set.empty).toList ::: work
        }
      }
      false
    }

    /** Returns `true` if binding `x -> f` would create a dependency cycle through another variable. */
    def wouldCycle(x: Int, f: SetFormula): Boolean =
      (f.varsOf - x).exists(v => reaches(v, x))

    /** Records the binding `x -> f` in the dependency graph. */
    def addBinding(x: Int, f: SetFormula): Unit = {
      val vs = f.varsOf - x
      val keepsX = f.varsOf.contains(x)
      for ((u, ds) <- deps.toList if ds.contains(x)) {
        deps(u) = ((if (keepsX) ds else ds - x) ++ vs) - u
      }
      deps(x) = (deps.getOrElse(x, Set.empty) ++ vs) - x
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
    *
    * The rewrite rules run as a sequence that is repeated until no rule makes progress (or
    * [[MaxIterations]] is reached), since the substitution of one rule often enables another:
    * a bound on a variable turns the equations of the lambdas inside a region into further bounds.
    *
    * The equations that remain after the rewrite rules are solved by SVE at the granularity
    * given by `mode` (see [[SveAlgorithm.sveAll]]).
    */
  def solve(l: List[Equation], mode: SveMode): (List[Equation], SetSubstitution) = {
    val state = new State(l)

    if (EnableRewriteRules) {
      var progress = true
      var iterations = 0
      while (progress && iterations < MaxIterations) {
        progress = false
        iterations += 1

        def step(f: List[Equation] => Option[(List[Equation], SetSubstitution)], phase: Phase): Unit = {
          val before = state.eqs
          runWithState(state, f, phase)
          if (state.eqs ne before) progress = true
        }

        step(runRule(state, simplify), Phase.Simplify)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, constantPropagation(state)), Phase.ConstantPropagation)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, variablePropagation), Phase.VariablePropagation)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, variableAssignment), Phase.VariableAssignment)
        step(runRule(state, trivial), Phase.Trivial)
        step(recursiveAssignment(state), Phase.RecursiveAssignment)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, simplify), Phase.Simplify)
        step(runRule(state, constantPropagation(state)), Phase.ConstantPropagation)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, boundAssignment(state)), Phase.BoundAssignment)
        step(runRule(state, trivial), Phase.Trivial)
        step(reflexiveAndDuplicate, Phase.ReflexiveAndDuplicate)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, unionAssignment(state)), Phase.UnionAssignment)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, interAssignment), Phase.InterAssignment)
        step(runRule(state, trivial), Phase.Trivial)
        step(runRule(state, coverAssignment), Phase.CoverAssignment)
        step(runRule(state, trivial), Phase.Trivial)

        // Safety valve: stop rewriting if the system or the substitution grows too much.
        if (state.eqs.length > 4 * l.length + 64 || state.subst.m.valuesIterator.exists(_.size > MaxRangeSize)) {
          progress = false
        }
      }
    }

    sveAll(state.eqs, mode) match {
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
  private def runWithState(state: State, f: List[Equation] => Option[(List[Equation], SetSubstitution)], phase: Phase): Unit = {
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

      case None =>
    }
  }

  /**
    * Eliminates redundant equations:
    *   - (Reflexive): Equations where the left and right are the same, i.e. `[f ~ f, l] -> [l]`.
    *   - (Duplicate): Equations that occur more than once, up to their location, i.e. `[e, e, l] -> [e, l]`.
    *
    * Always succeeds.
    */
  private def reflexiveAndDuplicate(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
    var result: List[Equation] = Nil
    val seen = mutable.Set.empty[(SetFormula, SetFormula)]
    var changed = false

    for (eq <- eqs) {
      // Equations are compared without their locations, so that the same constraint from
      // different sites (e.g. the cases of a handler) counts as a duplicate.
      if (eq.f1 == eq.f2 || seen.contains((eq.f1, eq.f2))) {
        // Don't add to result.
        changed = true
      } else {
        // Add to result, thus not making a change.
        seen.add((eq.f1, eq.f2))
        result = eq :: result
      }
    }

    if (changed) Some((result.reverse, SetSubstitution.empty)) else None
  }

  /** Run a unification rule on an equation system in a fixpoint. */
  private def runRule(state: State, rule: Equation => Option[(List[Equation], SetSubstitution)])(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
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
            for ((x, f) <- outputSubst.m) state.addBinding(x, f)

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
    *   - `f ~ f` becomes `({}, [])` for any `f` (the sides of normalized equations are comparable)
    *   - `!c ~ empty` becomes `({!c ~error empty}, [])`, since `c` cannot be `univ`
    *   - `c1 ∩ !c2 ∩ e ~ empty` becomes `({.. ~error empty}, [])`: an intersection of ground literals
    *     without complementary pairs is non-empty for some instantiation of its constants
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
      case _ if f1 == f2 => success

      // Equations that are in conflict.
      // The cases here were determined by careful profiling.
      case (Empty, Cst(_)) => failure
      case (Cst(_), Empty) => failure
      case (ElemSet(_), Empty) => failure
      case (Empty, ElemSet(_)) => failure
      // `!c ~ empty` would force the constant `c` to be `univ`.
      case (Compl(Cst(_)), Empty) => failure
      case (Empty, Compl(Cst(_))) => failure
      case (Compl(ElemSet(_)), Empty) => failure
      case (Empty, Compl(ElemSet(_))) => failure
      // An intersection of ground literals without complementary pairs is non-empty for some
      // instantiation of its constants.
      case (Inter(l), Empty) if l.forall(isGroundLiteral) => groundCube(f1, success, failure)
      case (Empty, Inter(l)) if l.forall(isGroundLiteral) => groundCube(f2, success, failure)

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
    *   - `!x ~ empty` becomes `({}, [x -> univ])`
    *   - `f1 ∪ f2 ∪ .. ~ empty` becomes `({f1 ~ empty, f2 ~ empty, ..}, [])`
    *   - `(f1 ∪ f2 ∪ ..) ∩ g ~ empty` becomes `({f1 ∩ g ~ empty, f2 ∩ g ~ empty, ..}, [])`
    *
    * The cases were determined by careful profiling.
    *
    * This also applies to the symmetric equations.
    */
  private def constantPropagation(state: State)(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
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

      // !x ~ empty
      // ---
      // {},
      // [x -> univ]
      case (Compl(Var(x)), Empty) =>
        Some((Nil, SetSubstitution.singleton(x, Univ)))

      // Symmetric case.
      case (Empty, Compl(Var(x))) =>
        Some((Nil, SetSubstitution.singleton(x, Univ)))

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

      // (f1 ∪ f2 ∪ ..) ∩ g ~ empty
      // ---
      // {f1 ∩ g ~ empty, f2 ∩ g ~ empty, ..},
      // []
      case (Inter(l), Empty) =>
        distributeUnionOverEmptyInter(state, l, loc)

      // Symmetric Case.
      case (Empty, Inter(l)) =>
        distributeUnionOverEmptyInter(state, l, loc)

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Splits the equation `f1 ∩ f2 ∩ .. ~ empty`, whose members are `l`, along the first member
    * that is a union: `(g1 ∪ g2 ∪ ..) ∩ h ~ empty` holds exactly when every `gi ∩ h ~ empty` holds,
    * where `h` is the intersection of the remaining members.
    *
    * Returns [[None]] if no member is a union.
    *
    * The split keeps the equations produced by subeffect constraints small: `e1 ∪ e2 ∪ .. ⊑ t` is
    * read as `(e1 ∪ e2 ∪ ..) ∩ ¬t ~ empty` and would otherwise collect the variables of every
    * member in a single equation.
    *
    * Repeated splits amount to a disjunctive normal form, so a union with more than
    * [[MaxSplitWidth]] members is not split, and at most [[MaxSplits]] equations are produced per
    * system; the rest is left to SVE.
    */
  private def distributeUnionOverEmptyInter(state: State, l: TwoList[SetFormula], loc: SourceLocation): Option[(List[Equation], SetSubstitution)] = {
    val members = l.toList
    members.collectFirst { case u: Union => u } match {
      case None => None
      case Some(u) if u.l.length > MaxSplitWidth || state.splits + u.l.length > MaxSplits => None
      case Some(u) =>
        state.splits += u.l.length
        // The intersection of the members other than `u`. There is at least one, since `l` has two or more members.
        val h = members.filterNot(_ eq u).foldLeft(Univ: SetFormula)(mkInter)
        val eqs = u.l.toList.map(g => Equation.mk(norm(mkInter(g, h)), Empty, loc))
        Some((eqs, SetSubstitution.empty))
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
    * Solves bounds on variables (e.g. `x ∩ ¬c ~ empty`, which is the inclusion `x ⊆ c`, and
    * `c ∩ ¬x ~ empty`, which is the inclusion `c ⊆ x`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ∩ g ~ empty` where `g` does not contain `x` becomes `({}, [x -> x ∩ ¬g])`
    *   - `¬x ∩ g ~ empty` where `g` does not contain `x` becomes `({}, [x -> x ∪ g])`
    *
    * These are the substitutions that successive variable elimination computes for the equations:
    * every solution of the first has `x ⊆ ¬g`, and `x ∩ ¬g` with `x` free ranges over exactly those
    * sets; every solution of the second has `g ⊆ x`, and `x ∪ g` with `x` free ranges over exactly
    * those sets.
    *
    * A binding whose range would make the substitution cyclic (see [[State.wouldCycle]]) is not
    * made, since every later composition would then substitute the range into itself.
    *
    * This also applies to the symmetric equations.
    */
  private def boundAssignment(state: State)(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      case (Inter(l), Empty) => boundAssignmentOf(state, l)
      case (Empty, Inter(l)) => boundAssignmentOf(state, l)
      case _ => None
    }
  }

  /**
    * Returns the substitution `[x -> x ∩ ¬g]` for the equation `x ∩ g ~ empty` whose members are `l`,
    * where `x` is the first member that is a variable not occurring in the other members `g` and
    * whose binding creates no dependency cycle.
    *
    * Returns [[None]] if there is no such member.
    */
  private def boundAssignmentOf(state: State, l: TwoList[SetFormula]): Option[(List[Equation], SetSubstitution)] = {
    val members = l.toList
    val candidates = members.iterator.flatMap {
      // x ∩ g ~ empty, where g does not contain x.
      case v: Var if !members.exists(m => !(m eq v) && m.contains(v)) =>
        val g = members.filterNot(_ eq v).foldLeft(Univ: SetFormula)(mkInter)
        Some((v.x, norm(mkInter(v, mkCompl(g)))))

      // ¬x ∩ g ~ empty, where g does not contain x.
      case c@Compl(v: Var) if !members.exists(m => !(m eq c) && m.contains(v)) =>
        val g = members.filterNot(_ eq c).foldLeft(Univ: SetFormula)(mkInter)
        Some((v.x, norm(mkUnion2(v, g))))

      case _ => None
    }
    candidates.find { case (x, f) => !state.wouldCycle(x, f) }.map {
      case (x, f) => (Nil, SetSubstitution.singleton(x, f))
    }
  }

  /**
    * Solves equations between a ground formula and a union of ground formulas and variables (e.g.
    * `c ~ c ∪ x1 ∪ x2`, which is the effect of a region whose statements are lambdas with free
    * effects).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `g ~ d ∪ x1 ∪ .. ∪ xn` where `g` and `d` are ground and the `xi` are distinct variables
    *     becomes `({d ∩ ¬g ~ empty}, [x1 -> x1 ∩ g, .., xn-1 -> xn-1 ∩ g, xn -> (xn ∩ g) ∪ (g ∩ ¬d ∩ ¬x1 ∩ .. ∩ ¬xn-1)])`
    *
    * The equation holds exactly when `d ⊆ g`, every `xi ⊆ g`, and the `xi` cover `g ∩ ¬d`. The
    * substitution is a most general unifier of the last two conditions: every `xi` is restricted to
    * `g`, and `xn` additionally covers what the others leave out. The first condition is ground and
    * is returned as an equation, unless `d` is `empty` or `g` itself. `d` is the union of the
    * ground members, or `empty` if there are none.
    * Nested unions are flattened, since a pipeline of higher-order calls nests its effects.
    *
    * The rule keeps such equations away from successive variable elimination, whose cost is
    * exponential in the number of variables.
    *
    * The rule does not apply if the range of `xn`, which mentions the other variables, would make
    * the substitution cyclic (see [[State.wouldCycle]]).
    *
    * This also applies to the symmetric equations.
    */
  private def unionAssignment(state: State)(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      case (g, Union(l)) if g.isGround => unionAssignmentOf(state, g, flattenUnions(l.toList), loc)
      case (Union(l), g) if g.isGround => unionAssignmentOf(state, g, flattenUnions(l.toList), loc)
      case _ => None
    }
  }

  /**
    * Returns the solution of `g ~ m1 ∪ .. ∪ mk` for the ground formula `g` and the members `members`,
    * if every member is ground or a variable and at least one member is a variable.
    *
    * Returns [[None]] otherwise.
    */
  private def unionAssignmentOf(state: State, g: SetFormula, members: List[SetFormula], loc: SourceLocation): Option[(List[Equation], SetSubstitution)] = {
    val xs = members.collect { case v: Var => v }.distinct
    val grounds = members.filterNot(_.isInstanceOf[Var])
    if (xs.isEmpty || !grounds.forall(_.isGround)) {
      None
    } else if (xs.init.exists(x => state.reaches(x.x, xs.last.x))) {
      // The last variable's range mentions the other variables, which must not depend on it.
      None
    } else {
      val d = grounds.foldLeft(Empty: SetFormula)(mkUnion2)
      val init = xs.init
      val last = xs.last

      // x1 -> x1 ∩ g, .., xn-1 -> xn-1 ∩ g
      val restricted = init.foldLeft(IntMap.empty[SetFormula]) {
        case (acc, x) => acc.updated(x.x, norm(mkInter(x, g)))
      }
      // xn -> (xn ∩ g) ∪ (g ∩ ¬d ∩ ¬x1 ∩ .. ∩ ¬xn-1)
      val rest = init.foldLeft(mkDiff2(g, d))((acc, x) => mkInter(acc, mkCompl(x)))
      val subst = SetSubstitution(restricted.updated(last.x, norm(mkUnion2(mkInter(last, g), rest))))

      // d ⊆ g, i.e. d ∩ ¬g ~ empty
      val eqs = if (d == Empty || d == g) Nil else List(Equation.mk(mkDiff2(d, g), Empty, loc))
      Some((eqs, subst))
    }
  }

  /**
    * Solves equations in which a variable occurs on both sides, as the variable itself on one side
    * and as a member of a union or an intersection on the other (e.g. `x ~ x ∪ f`, which is the
    * effect of a handler whose cases call the continuation).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ~ x ∪ f1 ∪ .. ∪ fn` where no `fi` contains `x` becomes `({}, [x -> x ∪ f1 ∪ .. ∪ fn])`
    *   - `x ~ x ∩ f1 ∩ .. ∩ fn` where no `fi` contains `x` becomes `({}, [x -> x ∩ f1 ∩ .. ∩ fn])`
    *
    * These are the substitutions that successive variable elimination computes for the equations:
    * `x ~ x ∪ f` holds exactly when `f ⊆ x`, and `x ∪ f` with `x` free ranges over exactly those
    * sets (dually for intersection). Nested unions (intersections) are flattened, and `x` may occur
    * more than once among the members.
    *
    * The rule runs as a single pass over the equations. A variable that already occurs in the range
    * of a substitution made by the pass is never bound by it: `x ~ x ∪ y` followed by `y ~ y ∪ x`
    * would otherwise give `[x -> x ∪ y ∪ x, y -> y ∪ x]`, a cyclic substitution whose repeated
    * application grows formulas exponentially. The same holds across passes, through the
    * dependency graph of the whole solve (see [[State.wouldCycle]]). Such equations are left for
    * successive variable elimination.
    *
    * This also applies to the symmetric equations.
    */
  private def recursiveAssignment(state: State)(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
    var subst = SetSubstitution.empty
    // The variables that occur in the ranges of `subst`, including the bound variables themselves.
    var rangeVars = Set.empty[Int]
    var rest: List[Equation] = Nil
    var changed = false

    for (eq0 <- eqs) {
      val eq = subst.apply(eq0)
      recursiveAssignmentOf(eq) match {
        case RecursiveAssignment.Trivial =>
          changed = true

        case RecursiveAssignment.Bind(x, f) if !rangeVars.contains(x) && !state.wouldCycle(x, f) =>
          // Since `x` occurs in no range of `subst`, the composition leaves the existing ranges unchanged.
          subst = SetSubstitution.singleton(x, f) @@ subst
          state.addBinding(x, f)
          rangeVars = rangeVars ++ f.varsOf
          changed = true

        case _ =>
          rest = eq :: rest
      }
    }

    if (changed) Some((rest.reverse.map(subst.apply), subst)) else None
  }

  /** The outcome of [[recursiveAssignmentOf]] on one equation. */
  private sealed trait RecursiveAssignment

  private object RecursiveAssignment {
    /** The rule does not apply. */
    case object None extends RecursiveAssignment

    /** The equation holds trivially (e.g. `x ~ x ∪ x`). */
    case object Trivial extends RecursiveAssignment

    /** The equation is solved by `[x -> f]`. */
    case class Bind(x: Int, f: SetFormula) extends RecursiveAssignment
  }

  /** Matches `eq` against the shapes solved by [[recursiveAssignment]]. */
  private def recursiveAssignmentOf(eq: Equation): RecursiveAssignment = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      case (v@Var(_), Union(l)) => recursiveAssignmentOf(v, flattenUnions(l.toList), Empty, mkUnion2)
      case (Union(l), v@Var(_)) => recursiveAssignmentOf(v, flattenUnions(l.toList), Empty, mkUnion2)
      case (v@Var(_), Inter(l)) => recursiveAssignmentOf(v, flattenInters(l.toList), Univ, mkInter)
      case (Inter(l), v@Var(_)) => recursiveAssignmentOf(v, flattenInters(l.toList), Univ, mkInter)
      case _ => RecursiveAssignment.None
    }
  }

  /**
    * Matches `v ~ v ⊙ m1 ⊙ .. ⊙ mn` for the members `members` of the union or intersection (`⊙`) with
    * unit `unit` and binary constructor `mk`. The rule applies if `v` is a member and no other
    * member contains `v`.
    */
  private def recursiveAssignmentOf(v: Var, members: List[SetFormula], unit: SetFormula, mk: (SetFormula, SetFormula) => SetFormula): RecursiveAssignment = {
    val (self, others) = members.partition(_ == v)
    if (self.isEmpty || others.exists(_.contains(v))) {
      RecursiveAssignment.None
    } else if (others.isEmpty) {
      RecursiveAssignment.Trivial
    } else {
      RecursiveAssignment.Bind(v.x, norm(mk(v, others.foldLeft(unit)(mk))))
    }
  }

  /** Decides `f ~ empty` for an intersection `f` of ground literals. */
  private def groundCube(f: SetFormula, success: Option[(List[Equation], SetSubstitution)], failure: Option[(List[Equation], SetSubstitution)]): Option[(List[Equation], SetSubstitution)] = norm(f) match {
    case Empty => success
    case Cst(_) | ElemSet(_) | Compl(Cst(_)) | Compl(ElemSet(_)) => failure
    case Inter(l) if l.forall(isGroundLiteral) => failure
    case _ => None
  }

  /** Returns `true` if `f` is a constant or element, or the complement of one. */
  private def isGroundLiteral(f: SetFormula): Boolean = f match {
    case Cst(_) | ElemSet(_) | Compl(Cst(_)) | Compl(ElemSet(_)) => true
    case _ => false
  }

  /**
    * Solves equations between a ground formula and the intersection of a variable with ground
    * formulas (e.g. `c ~ x ∩ d`, which arises when a variable restricted by an earlier rule is
    * then equated to a ground effect).
    *
    *   - `g ~ x ∩ h` where `g` and `h` are ground becomes `({g ∩ ¬h ~ empty}, [x -> g ∪ (x ∩ ¬h)])`
    *
    * The equation holds exactly when `g ⊆ h` and `x ∩ h = g`, and `g ∪ (x ∩ ¬h)` with `x` free
    * ranges over exactly the sets whose intersection with `h` is `g`. The range mentions no other
    * variable, so it cannot make the substitution cyclic.
    *
    * This also applies to the symmetric equations.
    */
  private def interAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      case (g, Inter(l)) if g.isGround => interAssignmentOf(g, l.toList, loc)
      case (Inter(l), g) if g.isGround => interAssignmentOf(g, l.toList, loc)
      case _ => None
    }
  }

  /**
    * Returns the solution of `g ~ m1 ∩ .. ∩ mk` for the ground formula `g` and the members `members`,
    * if exactly one member is a variable and the others are ground.
    *
    * Returns [[None]] otherwise.
    */
  private def interAssignmentOf(g: SetFormula, members: List[SetFormula], loc: SourceLocation): Option[(List[Equation], SetSubstitution)] = {
    val (xs, grounds) = members.partition(_.isInstanceOf[Var])
    xs match {
      case List(x: Var) if grounds.forall(_.isGround) =>
        val h = grounds.foldLeft(Univ: SetFormula)(mkInter)
        val subst = SetSubstitution.singleton(x.x, norm(mkUnion2(g, mkInter(x, mkCompl(h)))))
        val check = norm(mkDiff2(g, h))
        val eqs = if (check == Empty) Nil else List(Equation.mk(check, Empty, loc))
        Some((eqs, subst))
      case _ => None
    }
  }

  /**
    * Solves equations between a ground formula and a union that contains it (e.g.
    * `c ~ c ∪ x1 ∪ (x2 ∩ d)`, the effect of a lambda whose body includes a call with the
    * declared effect).
    *
    *   - `g ~ g ∪ f1 ∪ .. ∪ fn` where `g` is ground becomes `({(f1 ∪ .. ∪ fn) ∩ ¬g ~ empty}, [])`
    *
    * The equation holds exactly when every `fi ⊆ g`.
    *
    * This also applies to the symmetric equations.
    */
  private def coverAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      case (g, Union(l)) if g.isGround => coverAssignmentOf(g, l.toList, loc)
      case (Union(l), g) if g.isGround => coverAssignmentOf(g, l.toList, loc)
      case _ => None
    }
  }

  /**
    * Returns the solution of `g ~ m1 ∪ .. ∪ mk` for the ground formula `g` and the members `members`,
    * if `g` is one of the members.
    *
    * Returns [[None]] otherwise.
    */
  private def coverAssignmentOf(g: SetFormula, members: List[SetFormula], loc: SourceLocation): Option[(List[Equation], SetSubstitution)] = {
    val (self, others) = members.partition(_ == g)
    if (self.isEmpty) None
    else {
      val rest = norm(mkInter(others.foldLeft(Empty: SetFormula)(mkUnion2), mkCompl(g)))
      Some((if (rest == Empty) Nil else List(Equation.mk(rest, Empty, loc)), SetSubstitution.empty))
    }
  }

  /**
    * Rewrites both sides of `eq` into the normal form of [[norm]], so that the other rules see flat
    * unions and intersections with the complements on the atoms: `e ∩ ¬(x ∪ e) ~ empty` becomes
    * `e ∩ ¬x ∩ ¬e ~ empty` and then `empty ~ empty`, and `e ∩ ¬(x ∪ c) ~ empty` becomes
    * `e ∩ ¬x ∩ ¬c ~ empty`, which [[boundAssignment]] solves. Ground equations with equal sides
    * (e.g. `c ~ c ∪ empty` after a substitution) are then removed by [[trivial]].
    *
    * Returns [[None]] if neither side changes.
    */
  private def simplify(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, status, loc) = eq
    val n1 = norm(f1)
    val n2 = norm(f2)
    if (n1 == f1 && n2 == f2) None
    else Some((List(Equation.mk(n1, n2, loc, status)), SetSubstitution.empty))
  }

  /**
    * Returns `f` in negation normal form: complements occur only on constants, elements, and
    * variables; unions and intersections are flattened, without units, duplicates, or a member
    * together with its complement; elements are merged; and members are sorted.
    */
  def norm(f: SetFormula): SetFormula = f match {
    case Univ | Empty | Cst(_) | Var(_) | ElemSet(_) => f
    case Compl(g) => normCompl(g)
    case Inter(l) => mkInterAll(l.toList.map(norm))
    case Union(l) => mkUnionAll(l.toList.map(norm))
    case Xor(l) =>
      // Nested symmetric differences are flattened first: a Zhegalkin polynomial arrives as a
      // left-nested chain of binary `⊕` nodes, and expanding each level would double the formula
      // per level.
      val ms = flattenXors(l).map(norm)
      if (ms.length <= MaxXorExpand) ms.reduceLeft(normXor2) else Xor(ms)
  }

  /** Returns `fs` with nested symmetric differences flattened into their members. */
  private def flattenXors(fs: List[SetFormula]): List[SetFormula] = fs.flatMap {
    case Xor(l) => flattenXors(l)
    case f => f :: Nil
  }

  /** Returns `¬g` in the normal form of [[norm]]. */
  private def normCompl(g: SetFormula): SetFormula = g match {
    case Univ => Empty
    case Empty => Univ
    case Cst(_) | Var(_) | ElemSet(_) => Compl(g)
    case Compl(h) => norm(h)
    case Inter(l) => mkUnionAll(l.toList.map(normCompl))
    case Union(l) => mkInterAll(l.toList.map(normCompl))
    case Xor(_) => norm(g) match {
      case n@Xor(_) => Compl(n)
      case n => normCompl(n)
    }
  }

  /** Returns `a ⊕ b` as `(a ∩ ¬b) ∪ (¬a ∩ b)` for normalized `a` and `b`. */
  private def normXor2(a: SetFormula, b: SetFormula): SetFormula =
    mkUnionAll(List(mkInterAll(List(a, normCompl(b))), mkInterAll(List(normCompl(a), b))))

  /** Returns the normalized intersection of the normalized formulas `ms0`. */
  private def mkInterAll(ms0: List[SetFormula]): SetFormula = {
    val ms = ms0.flatMap { case Inter(l) => l.toList; case m => List(m) }
    if (ms.contains(Empty)) return Empty
    var pos: Option[SortedSet[Int]] = None
    var neg: SortedSet[Int] = SortedSet.empty
    val rest = mutable.LinkedHashSet.empty[SetFormula]
    for (m <- ms) m match {
      case Univ => ()
      case ElemSet(s) => pos = Some(pos.fold(s)(_ intersect s))
      case Compl(ElemSet(s)) => neg = neg ++ s
      case _ => rest += m
    }
    for (m <- rest) m match {
      case Compl(h) => if (rest.contains(h)) return Empty
      case _ => ()
    }
    val elems: List[SetFormula] = pos match {
      case Some(s) =>
        val d = s -- neg
        if (d.isEmpty) return Empty else List(ElemSet(d))
      case None => if (neg.isEmpty) Nil else List(Compl(ElemSet(neg)))
    }
    // Absorption: `a ∩ (a ∪ b) = a`. A union member is dropped if one of its members is implied by the
    // other members of the intersection.
    def covered(u: SetFormula): Boolean = u match {
      case ElemSet(s) => pos.exists(_.subsetOf(s))
      case Compl(ElemSet(s)) => pos.isEmpty && s.subsetOf(neg)
      case Inter(l) => l.forall(covered)
      case _ => rest.contains(u)
    }
    val kept = rest.toList.filter {
      case Union(l) => !l.exists(covered)
      case _ => true
    }
    (elems ++ kept).sorted(FormulaOrdering) match {
      case Nil => Univ
      case one :: Nil => one
      case x :: y :: tail => Inter(TwoList(x, y, tail))
    }
  }

  /** Returns the normalized union of the normalized formulas `ms0`. */
  private def mkUnionAll(ms0: List[SetFormula]): SetFormula = {
    val ms = ms0.flatMap { case Union(l) => l.toList; case m => List(m) }
    if (ms.contains(Univ)) return Univ
    var pos: SortedSet[Int] = SortedSet.empty
    var neg: Option[SortedSet[Int]] = None
    val rest = mutable.LinkedHashSet.empty[SetFormula]
    for (m <- ms) m match {
      case Empty => ()
      case ElemSet(s) => pos = pos ++ s
      case Compl(ElemSet(s)) => neg = Some(neg.fold(s)(_ intersect s))
      case _ => rest += m
    }
    for (m <- rest) m match {
      case Compl(h) => if (rest.contains(h)) return Univ
      case _ => ()
    }
    val elems: List[SetFormula] = neg match {
      case Some(n) =>
        // pos ∪ ¬n = ¬(n ∖ pos)
        val d = n -- pos
        if (d.isEmpty) return Univ else List(Compl(ElemSet(d)))
      case None => if (pos.isEmpty) Nil else List(ElemSet(pos))
    }
    // Absorption: `a ∪ (a ∩ b) = a`. An intersection member is dropped if one of its members is implied
    // by the other members of the union.
    def covered(u: SetFormula): Boolean = u match {
      case ElemSet(s) => s.subsetOf(pos)
      case Compl(ElemSet(s)) => neg.exists(_.subsetOf(s))
      case Union(l) => l.forall(covered)
      case _ => rest.contains(u)
    }
    val kept = rest.toList.filter {
      case Inter(l) => !l.exists(covered)
      case _ => true
    }
    (elems ++ kept).sorted(FormulaOrdering) match {
      case Nil => Empty
      case one :: Nil => one
      case x :: y :: tail => Union(TwoList(x, y, tail))
    }
  }

  /** A total order on formulas, used to sort the members of unions and intersections. */
  private object FormulaOrdering extends Ordering[SetFormula] {
    private def rank(f: SetFormula): Int = f match {
      case Univ => 0
      case Empty => 1
      case Cst(_) => 2
      case ElemSet(_) => 3
      case Var(_) => 4
      case Compl(_) => 5
      case Inter(_) => 6
      case Union(_) => 7
      case Xor(_) => 8
    }

    def compare(a: SetFormula, b: SetFormula): Int = (a, b) match {
      case (Cst(x), Cst(y)) => Integer.compare(x, y)
      case (Var(x), Var(y)) => Integer.compare(x, y)
      case (ElemSet(x), ElemSet(y)) => compareInts(x.toList, y.toList)
      case (Compl(x), Compl(y)) => compare(x, y)
      case (Inter(x), Inter(y)) => compareList(x.toList, y.toList)
      case (Union(x), Union(y)) => compareList(x.toList, y.toList)
      case (Xor(x), Xor(y)) => compareList(x, y)
      case _ => Integer.compare(rank(a), rank(b))
    }

    private def compareList(x: List[SetFormula], y: List[SetFormula]): Int = (x, y) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
      case (a :: as, b :: bs) =>
        val c = compare(a, b)
        if (c != 0) c else compareList(as, bs)
    }

    private def compareInts(x: List[Int], y: List[Int]): Int = (x, y) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
      case (a :: as, b :: bs) =>
        val c = Integer.compare(a, b)
        if (c != 0) c else compareInts(as, bs)
    }
  }

  /** Returns `fs` with nested unions flattened into their members. */
  private def flattenUnions(fs: List[SetFormula]): List[SetFormula] = fs.flatMap {
    case Union(l) => flattenUnions(l.toList)
    case f => f :: Nil
  }

  /** Returns `fs` with nested intersections flattened into their members. */
  private def flattenInters(fs: List[SetFormula]): List[SetFormula] = fs.flatMap {
    case Inter(l) => flattenInters(l.toList)
    case f => f :: Nil
  }

  /**
    * Attempts to solve all the given equations `eqs` using the SVE algorithm.
    *
    * Returns `Result.Ok(s)` with a complete substitution `s` if all equations were solved.
    * Returns `Result.Err(l)` with a list of unsolveable equations. (At least one equation is unsolveable.)
    */
  private def sveAll(eqs: List[Equation], mode: SveMode): Result[SetSubstitution, List[Equation]] = {
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
    implicit val alg: ZhegalkinAlgebra[CofiniteIntSet] = EffUnification3.Algebra
    val l = eqs.map {
      case Equation(f1, f2, _, _) =>
        val x = Zhegalkin.toZhegalkin(f1)(alg, CofiniteIntSet.LatticeOps)
        val y = Zhegalkin.toZhegalkin(f2)(alg, CofiniteIntSet.LatticeOps)
        (x, y)
    }

    // Solve *ALL* equations via SVE and obtain the substitution.
    try {
      val subst = SveAlgorithm.sveAll(l, mode)

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
}
