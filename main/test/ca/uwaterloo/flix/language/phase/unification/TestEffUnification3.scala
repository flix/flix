/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, SubstitutionTree, TypeConstraint}
import ca.uwaterloo.flix.util.Result
import org.scalatest.funsuite.AnyFunSuite

/**
  * Tests [[EffUnification3.unifyAll]] on systems that mix effect equalities and subeffect constraints.
  */
class TestEffUnification3 extends AnyFunSuite with TestUtils {

  private implicit val flix: Flix = new Flix()
  private implicit val scope: RegionScope = RegionScope.Top

  private val loc: SourceLocation = SourceLocation.Unknown

  private val Crash: Type = Type.Cst(TypeConstructor.Effect(Symbol.mkEffSym("Crash"), Kind.Eff), loc)

  private def mkVar(): Type.Var = Type.freshVar(Kind.Eff, loc)

  private def mkEq(tpe1: Type, tpe2: Type): TypeConstraint.Equality =
    TypeConstraint.Equality(tpe1, tpe2, Provenance.Match(tpe1, tpe2, loc))

  private def mkSub(eff1: Type, eff2: Type): TypeConstraint.SubEff =
    TypeConstraint.SubEff(eff1, eff2, Provenance.ExpectEffect(expected = eff2, actual = eff1, loc))

  private def union(effs: Type*): Type = Type.mkUnion(effs.toList, loc)

  /** Solves the system and returns the substitution, failing the test if the system is unsolvable. */
  private def solve(eqs: List[TypeConstraint.Equality], subs: List[TypeConstraint.SubEff], renv: RigidityEnv = RigidityEnv.empty): Substitution =
    EffUnification3.unifyAll(eqs, subs, scope, renv) match {
      case Result.Ok(subst) => subst
      case Result.Err(conflicts) => fail(s"Expected a solution, but got conflicts: $conflicts")
    }

  /** Asserts that the system is unsolvable. */
  private def assertConflict(eqs: List[TypeConstraint.Equality], subs: List[TypeConstraint.SubEff], renv: RigidityEnv = RigidityEnv.empty): Unit =
    EffUnification3.unifyAll(eqs, subs, scope, renv) match {
      case Result.Ok(subst) => fail(s"Expected a conflict, but got the solution: $subst")
      case Result.Err(_) => ()
    }

  /** Returns `true` if `eff1 ⊑ eff2` holds, i.e. `eff1 ∩ ¬eff2` simplifies to `Pure`. */
  private def isSubset(eff1: Type, eff2: Type): Boolean =
    EffUnification3.simplify(Type.mkDifference(eff1, eff2, loc)) == Type.Pure

  /** Solves the constraints with the full constraint solver and returns the leftovers and the substitution. */
  private def solveAllTypes(constrs: List[TypeConstraint], renv: RigidityEnv = RigidityEnv.empty): (List[TypeConstraint], Substitution) =
    ConstraintSolver2.solveAllTypes(constrs)(scope, renv, EqualityEnv.empty, flix)

  test("SubEff.Empty") {
    val subst = solve(Nil, Nil)
    assert(subst.isEmpty)
  }

  test("SubEff.Ground.01") {
    // Pure ⊑ IO
    val subst = solve(Nil, List(mkSub(Type.Pure, Type.IO)))
    assert(subst.isEmpty)
  }

  test("SubEff.Ground.02") {
    // IO ⊑ IO ∪ Crash
    val subst = solve(Nil, List(mkSub(Type.IO, union(Type.IO, Crash))))
    assert(subst.isEmpty)
  }

  test("SubEff.Ground.03") {
    // IO ⊑ Pure is unsolvable.
    assertConflict(Nil, List(mkSub(Type.IO, Type.Pure)))
  }

  test("SubEff.Ground.04") {
    // IO ∪ Crash ⊑ IO is unsolvable.
    assertConflict(Nil, List(mkSub(union(Type.IO, Crash), Type.IO)))
  }

  test("SubEff.Upper.01") {
    // x ⊑ IO has a non-variable upper bound, so it is read laxly: x stays within IO but is not
    // forced to be IO.
    val x = mkVar()
    val subst = solve(Nil, List(mkSub(x, Type.IO)))
    assert(isSubset(subst(x), Type.IO))
    assert(subst(x) != Type.IO)
  }

  test("SubEff.Upper.02") {
    // x ⊑ IO with x ~ Pure: x ↦ Pure.
    val x = mkVar()
    val subst = solve(List(mkEq(x, Type.Pure)), List(mkSub(x, Type.IO)))
    assert(subst(x) == Type.Pure)
  }

  test("SubEff.Upper.03") {
    // x ⊑ IO with x ~ Crash is unsolvable.
    val x = mkVar()
    assertConflict(List(mkEq(x, Crash)), List(mkSub(x, Type.IO)))
  }

  test("SubEff.Lower.01") {
    // IO ⊑ x has a flexible upper bound, so it is read strictly: x ↦ IO.
    val x = mkVar()
    val subst = solve(Nil, List(mkSub(Type.IO, x)))
    assert(subst(x) == Type.IO)
  }

  test("SubEff.Lower.02") {
    // Pure ⊑ x with x ~ IO: the strict reading fails, the lax reading gives x ↦ IO.
    val x = mkVar()
    val subst = solve(List(mkEq(x, Type.IO)), List(mkSub(Type.Pure, x)))
    assert(subst(x) == Type.IO)
  }

  test("SubEff.Lower.03") {
    // Two lower bounds on one variable: IO ⊑ y and Crash ⊑ y. The strict reading conflicts; the lax
    // reading gives y ⊇ IO ∪ Crash with y otherwise free.
    val y = mkVar()
    val subst = solve(Nil, List(mkSub(Type.IO, y), mkSub(Crash, y)))
    assert(isSubset(union(Type.IO, Crash), subst(y)))
    assert(!isSubset(subst(y), union(Type.IO, Crash)))
  }

  test("SubEff.Lower.04") {
    // IO ⊑ x with x ~ Crash is unsolvable.
    val x = mkVar()
    assertConflict(List(mkEq(x, Crash)), List(mkSub(Type.IO, x)))
  }

  test("SubEff.Var.01") {
    // x ⊑ y with y ~ IO: y is IO and x stays within IO.
    val x = mkVar()
    val y = mkVar()
    val subst = solve(List(mkEq(y, Type.IO)), List(mkSub(x, y)))
    assert(isSubset(subst(x), Type.IO))
    assert(subst(y) == Type.IO)
  }

  test("SubEff.Var.02") {
    // x ⊑ y with both flexible is solved strictly by equating the two.
    val x = mkVar()
    val y = mkVar()
    val subst = solve(Nil, List(mkSub(x, y)))
    assert(subst(y) == x || subst(x) == y || subst(x) == subst(y))
  }

  test("SubEff.Lax.01") {
    // IO ⊑ x and x ⊑ IO ∪ Crash: the solution satisfies both constraints.
    val x = mkVar()
    val subst = solve(Nil, List(mkSub(Type.IO, x), mkSub(x, union(Type.IO, Crash))))
    assert(isSubset(Type.IO, subst(x)))
    assert(isSubset(subst(x), union(Type.IO, Crash)))
  }

  test("SubEff.Lax.02") {
    // A def declared IO ∪ Crash whose body is x ∪ IO where x is a flexible variable:
    // x ∪ IO ⊑ IO ∪ Crash. The solution must keep x within IO ∪ Crash.
    val x = mkVar()
    val subst = solve(Nil, List(mkSub(union(x, Type.IO), union(Type.IO, Crash))))
    assert(isSubset(union(subst(x), Type.IO), union(Type.IO, Crash)))
  }

  test("SubEff.Lax.03") {
    // The declared effect is larger than the body's: the free variable of the body is not forced to
    // absorb the unused declared effect.
    val x = mkVar()
    val subst = solve(Nil, List(mkSub(union(x, Type.IO), union(Type.IO, Crash))))
    assert(isSubset(subst(x), union(Type.IO, Crash)))
    assert(!isSubset(Crash, subst(x)))
  }

  test("SubEff.Rigid.01") {
    // r ⊑ IO for a rigid r is unsolvable.
    val r = mkVar()
    val renv = RigidityEnv.empty.markRigid(r.sym)
    assertConflict(Nil, List(mkSub(r, Type.IO)), renv)
  }

  test("SubEff.Rigid.02") {
    // r ⊑ r ∪ IO for a rigid r is solvable.
    val r = mkVar()
    val renv = RigidityEnv.empty.markRigid(r.sym)
    val subst = solve(Nil, List(mkSub(r, union(r, Type.IO))), renv)
    assert(subst.isEmpty)
  }

  test("SubEff.Rigid.03") {
    // x ⊑ r for a rigid r and flexible x: x stays within r.
    val r = mkVar()
    val x = mkVar()
    val renv = RigidityEnv.empty.markRigid(r.sym)
    val subst = solve(Nil, List(mkSub(x, r)), renv)
    assert(isSubset(subst(x), r))
  }

  test("SubEff.Rigid.04") {
    // IO ⊑ r for a rigid r is unsolvable: the upper bound is rigid, so the constraint is read laxly.
    val r = mkVar()
    val renv = RigidityEnv.empty.markRigid(r.sym)
    assertConflict(Nil, List(mkSub(Type.IO, r)), renv)
  }

  test("SubEff.Mixed.01") {
    // A lambda whose body has effect y, passed where IO is expected:
    //   y ⊑ e   (the lambda's arrow effect e is at least its body)
    //   e ~ IO  (the argument expectation)
    val y = mkVar()
    val e = mkVar()
    val subst = solve(List(mkEq(e, Type.IO)), List(mkSub(y, e)))
    assert(subst(e) == Type.IO)
    assert(isSubset(subst(y), Type.IO))
  }

  test("SubEff.Mixed.02") {
    // As above, but the body effect is Crash: unsolvable.
    val e = mkVar()
    assertConflict(List(mkEq(e, Type.IO)), List(mkSub(Crash, e)))
  }

  test("SubEff.Solver.Trivial.01") {
    // IO ⊑ IO ∪ x holds for any x and must not constrain x.
    val x = mkVar()
    val (leftovers, subst) = solveAllTypes(List(mkSub(Type.IO, union(Type.IO, x))))
    assert(leftovers.isEmpty)
    assert(subst.isEmpty)
  }

  test("SubEff.Solver.Trivial.02") {
    // x ⊑ x ∪ IO holds for any x and must not constrain x.
    val x = mkVar()
    val (leftovers, subst) = solveAllTypes(List(mkSub(x, union(x, Type.IO))))
    assert(leftovers.isEmpty)
    assert(subst.isEmpty)
  }

  test("SubEff.Solver.Trivial.03") {
    // r ⊑ r ∪ IO holds for a rigid r.
    val r = mkVar()
    val renv = RigidityEnv.empty.markRigid(r.sym)
    val (leftovers, subst) = solveAllTypes(List(mkSub(r, union(r, Type.IO))), renv)
    assert(leftovers.isEmpty)
    assert(subst.isEmpty)
  }

  test("SubEff.Solver.Conflict.01") {
    // IO ⊑ Crash is reported as a leftover.
    val (leftovers, _) = solveAllTypes(List(mkSub(Type.IO, Crash)))
    assert(leftovers.nonEmpty)
  }

  test("SubEff.Solver.Mixed.01") {
    // A def declared IO whose body is a lambda application: the lambda effect e is at least its body y,
    // the body is IO, and the def's inferred effect e must be within IO.
    val y = mkVar()
    val e = mkVar()
    val (leftovers, subst) = solveAllTypes(List(mkSub(y, e), mkEq(y, Type.IO), mkSub(e, Type.IO)))
    assert(leftovers.isEmpty)
    assert(subst(e) == Type.IO)
    assert(subst(y) == Type.IO)
  }

  test("SubEff.Solver.Region.01") {
    // A subeffect constraint nested under a purification constraint is solved in the branch of its region:
    //   o ~ (x)[r ↦ Pure] ∧ { IO ⊑ x }
    // binds x ↦ IO in the branch of r, after which the purified constraint binds o ↦ IO at the root.
    val r = Symbol.freshRegionSym(Name.Ident("r", loc))
    val o = mkVar()
    val x = Type.freshVar(Kind.Eff, loc)(scope.enter(r), flix)
    val nested = List(mkSub(Type.IO, x))
    val constr = TypeConstraint.Purification(r, o, x, Provenance.Match(o, x, loc), nested)
    val (leftovers, tree) = ConstraintSolver2.solveAll(List(constr), SubstitutionTree.empty)(scope, RigidityEnv.empty, TraitEnv.empty, EqualityEnv.empty, flix)
    assert(leftovers.isEmpty)
    assert(tree.root(o) == Type.IO)
    assert(tree.branches(r).root(x) == Type.IO)
  }
}
