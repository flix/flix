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

import scala.collection.immutable.IntMap
import scala.collection.mutable

object SetUnification {

  /**
   * The maximum number of variables an equation may contain before it is considered too complex.
   */
  val MaxVars: Int = CompilerConstants.MaxEffUnificationVars

  /** The maximum number of passes over the rule sequence (see [[solve]]). */
  private val MaxIterations: Int = 32

  /** Represents the running mutable state of the solver. */
  final class State(initialEquations: List[Equation]) {
    /** The remaining equations to solve. */
    var eqs: List[Equation] = initialEquations
    /** The current substitution, which has already been applied to `eqs`. */
    var subst: SetSubstitution = SetSubstitution.empty

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
    * [[MaxIterations]] is reached), since the substitution of one rule often enables another: a
    * bound on a variable turns the equations it occurs in into further bounds, and a restriction
    * made by [[unionAssignment]] makes [[interAssignment]] applicable.
    *
    * The equations that remain after the rewrite rules are solved by SVE at the granularity
    * given by `mode` (see [[SveAlgorithm.sveAll]]).
    */
  def solve(l: List[Equation], mode: SveMode): (List[Equation], SetSubstitution) = {
    val state = new State(l)

    var progress = true
    var iterations = 0
    while (progress && iterations < MaxIterations) {
      progress = false
      iterations += 1

      def step(f: List[Equation] => Option[(List[Equation], SetSubstitution)]): Unit = {
        val before = state.eqs
        runWithState(state, f)
        if (state.eqs ne before) progress = true
      }

      step(runRule(state, constantPropagation))
      step(runRule(state, trivial))
      step(runRule(state, variablePropagation))
      step(runRule(state, trivial))
      step(runRule(state, variableAssignment))
      step(runRule(state, trivial))
      step(recursiveAssignment(state))
      step(runRule(state, trivial))
      step(runRule(state, constantPropagation))
      step(runRule(state, trivial))
      step(runRule(state, coverAssignment))
      step(runRule(state, trivial))
      step(runRule(state, boundAssignment(state)))
      step(runRule(state, trivial))
      step(reflexiveAndDuplicate)
      step(runRule(state, trivial))
      step(runRule(state, unionAssignment(state)))
      step(runRule(state, trivial))
      step(runRule(state, interAssignment))
      step(runRule(state, trivial))

      // Safety valve: stop if distribution has grown the system far beyond its input.
      if (state.eqs.length > 4 * l.length + 64) {
        progress = false
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
    * Runs the given equation system solver `f` on `state`.
    */
  private def runWithState(state: State, f: List[Equation] => Option[(List[Equation], SetSubstitution)]): Unit = {
    f(state.eqs) match {
      case Some((eqs, subst)) =>
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
    *   - `!x ~ empty` becomes `({}, [x -> univ])`
    *   - `f1 ∪ f2 ∪ .. ~ empty` becomes `({f1 ~ empty, f2 ~ empty, ..}, [])`
    *   - `(f1 ∪ f2 ∪ ..) ∩ g ~ empty` becomes `({f1 ∩ g ~ empty, f2 ∩ g ~ empty, ..}, [])`
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
        distributeUnionOverEmptyInter(l, loc)

      // Symmetric Case.
      case (Empty, Inter(l)) =>
        distributeUnionOverEmptyInter(l, loc)

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
    */
  private def distributeUnionOverEmptyInter(l: TwoList[SetFormula], loc: SourceLocation): Option[(List[Equation], SetSubstitution)] = {
    val members = l.toList
    members.collectFirst { case u: Union => u } match {
      case None => None
      case Some(u) =>
        // The intersection of the members other than `u`. There is at least one, since `l` has two or more members.
        val h = members.filterNot(_ eq u).foldLeft(Univ: SetFormula)(mkInter)
        val eqs = u.l.toList.map(g => Equation.mk(simplify(mkInter(g, h)), Empty, loc))
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
    * This also applies to the symmetric equations.
    */
  private def boundAssignment(state: State)(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      case (Inter(_), Empty) => boundAssignmentOf(state, f1)
      case (Empty, Inter(_)) => boundAssignmentOf(state, f2)
      case _ => None
    }
  }

  /**
    * Returns the substitution `[x -> x ∩ ¬g]` for the equation `x ∩ g ~ empty` whose members are `l`,
    * where `x` is the first member that is a variable not occurring in the other members `g`.
    *
    * Returns [[None]] if there is no such member.
    */
  private def boundAssignmentOf(state: State, f: SetFormula): Option[(List[Equation], SetSubstitution)] = simplify(f) match {
    // The intersection is empty under every assignment (e.g. `e ∩ !x ∩ !e`), so the equation holds.
    case Empty => Some((Nil, SetSubstitution.empty))

    case Inter(l) =>
      // The members are flat and distinct, so a variable nested in a member intersection is visible.
      val members = l.toList
      val candidates = members.iterator.flatMap {
        // x ∩ g ~ empty, where g does not contain x.
        case v: Var if !members.exists(m => !(m eq v) && m.contains(v)) =>
          val g = members.filterNot(_ eq v).foldLeft(Univ: SetFormula)(mkInter)
          Some((v.x, simplify(mkInter(v, mkCompl(g)))))

        // ¬x ∩ g ~ empty, where g does not contain x.
        case c@Compl(v: Var) if !members.exists(m => !(m eq c) && m.contains(v)) =>
          val g = members.filterNot(_ eq c).foldLeft(Univ: SetFormula)(mkInter)
          Some((v.x, simplify(mkUnion2(v, g))))

        case _ => None
      }
      candidates.find { case (x, range) => !state.wouldCycle(x, range) }.map {
        case (x, range) => (Nil, SetSubstitution.singleton(x, range))
      }

    case _ => None
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
        case (acc, x) => acc.updated(x.x, simplify(mkInter(x, g)))
      }
      // xn -> (xn ∩ g) ∪ (g ∩ ¬d ∩ ¬x1 ∩ .. ∩ ¬xn-1)
      val rest = init.foldLeft(mkDiff2(g, d))((acc, x) => mkInter(acc, mkCompl(x)))
      val subst = SetSubstitution(restricted.updated(last.x, simplify(mkUnion2(mkInter(last, g), rest))))

      // d ⊆ g, i.e. d ∩ ¬g ~ empty
      val eqs = if (d == Empty || d == g) Nil else List(Equation.mk(mkDiff2(d, g), Empty, loc))
      Some((eqs, subst))
    }
  }

  /**
    * Solves equations between a ground formula and a union that contains it (e.g.
    * `c ~ c ∪ x1 ∪ (x2 ∩ d)`, the effect of a lambda whose body includes a call with the
    * declared effect).
    *
    *   - `g ~ g ∪ f1 ∪ .. ∪ fn` where `g` is ground becomes `({f1 ∩ ¬g ~ empty, .., fn ∩ ¬g ~ empty}, [])`
    *   - `g1 ∪ .. ∪ gk ~ g1 ∪ .. ∪ gk ∪ f1 ∪ .. ∪ fn` where the `gi` are ground becomes the same
    *
    * The equation holds exactly when every `fi ⊆ g`. The inclusions are emitted one per member, so
    * that [[boundAssignment]] can solve them in the same pass.
    *
    * This also applies to the symmetric equations.
    */
  private def coverAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      case (g, Union(l)) if g.isGround => coverAssignmentOf(g, flattenUnions(l.toList), loc)
      case (Union(l), g) if g.isGround => coverAssignmentOf(g, flattenUnions(l.toList), loc)
      case _ => None
    }
  }

  /**
    * Returns the inclusions `fi ∩ ¬g ~ empty` for the members `fi` of `members` that are not members
    * of `g`, for the ground formula `g` and the members `members` of a union that contains `g`: the
    * union contains `g` if `g` is a member, or if `g` is a union all of whose members are members.
    *
    * Returns [[None]] if the union does not contain `g`.
    */
  private def coverAssignmentOf(g: SetFormula, members: List[SetFormula], loc: SourceLocation): Option[(List[Equation], SetSubstitution)] = {
    val gs = g match {
      case Union(l) => flattenUnions(l.toList)
      case _ => g :: Nil
    }
    if (!gs.forall(members.contains)) {
      None
    } else {
      val notG = mkCompl(g)
      val others = members.filterNot(gs.contains)
      val eqs = others.map(f => simplify(mkInter(f, notG))).filter(_ != Empty).map(Equation.mk(_, Empty, loc))
      Some((eqs, SetSubstitution.empty))
    }
  }

  /**
    * Solves equations between a ground formula and the intersection of a variable with ground
    * formulas (e.g. `c ~ x ∩ d`, which arises when a variable restricted by [[unionAssignment]]
    * is then equated to a ground effect).
    *
    *   - `g ~ x ∩ h` where `g` and `h` are ground becomes `({g ∩ ¬h ~ empty}, [x -> g ∪ (x ∩ ¬h)])`
    *
    * The equation holds exactly when `g ⊆ h` and `x ∩ h = g`, and `g ∪ (x ∩ ¬h)` with `x` free
    * ranges over exactly the sets whose intersection with `h` is `g`. The range mentions no other
    * variable.
    *
    * This also applies to the symmetric equations.
    */
  private def interAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      case (g, Inter(l)) if g.isGround => interAssignmentOf(g, flattenInters(l.toList), loc)
      case (Inter(l), g) if g.isGround => interAssignmentOf(g, flattenInters(l.toList), loc)
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
        val subst = SetSubstitution.singleton(x.x, simplify(mkUnion2(g, mkInter(x, mkCompl(h)))))
        // g ⊆ h, i.e. g ∩ ¬h ~ empty
        val check = simplify(mkDiff2(g, h))
        val eqs = if (check == Empty || g == h) Nil else List(Equation.mk(check, Empty, loc))
        Some((eqs, subst))
      case _ => None
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
    * application grows formulas exponentially. Such equations are left for successive variable
    * elimination.
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
      RecursiveAssignment.Bind(v.x, simplify(mk(v, others.foldLeft(unit)(mk))))
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

    // Return immediately if there is an equation that has too many variables.
    for (eq <- eqs) {
      val allVars = eq.f1.varsOf ++ eq.f2.varsOf
      if (allVars.size > MaxVars) {
        return Result.Err(List(eq.toTimeout(s"Unification too complex: The equation contains ${allVars.size} variables which exceeds the limit of $MaxVars")))
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

      Result.Ok(SetSubstitution(m))
    } catch {
      case _: BoolUnificationException =>
        // SVE failed. We give up. We indiscriminately mark all equations as unsolvable.
        Result.Err(eqs.map(_.toUnsolvable))
    }
  }
}
