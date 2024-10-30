/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.BoolAlg
import ca.uwaterloo.flix.util.{CofiniteIntSet, InternalCompilerException}

import scala.collection.immutable.SortedSet

object Zhegalkin {

  /** Represents a variable. */
  case class ZhegalkinVar(v: Int, flexible: Boolean) extends Ordered[ZhegalkinVar] {
    override def toString: String = if (flexible) s"x$v" else s"x!$v"

    override def equals(obj: Any): Boolean = obj match {
      case that: ZhegalkinVar => this.v == that.v
      case _ => false
    }

    override def hashCode(): Int = v

    override def compare(that: ZhegalkinVar): Int = this.v.compare(that.v)
  }

  /** Companion object for [[ZhegalkinConstant]] */
  private object ZhegalkinConstant {
    /** A Zhegalkin constant that represents the empty set. */
    val empty: ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.empty)

    /** A Zhegalkin constant that represents the university. */
    val universe: ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.universe)
  }

  /** Represents a set Zhegalkin constant (i.e. a set or co-set) */
  case class ZhegalkinConstant(s: CofiniteIntSet) {
    def compl: ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.complement(s))

    def union(that: ZhegalkinConstant): ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.union(s, that.s))

    def inter(that: ZhegalkinConstant): ZhegalkinConstant = ZhegalkinConstant(CofiniteIntSet.intersection(s, that.s))

    override def toString: String = {
      if (s.isEmpty) "Ø"
      else if (s.isUniverse) "𝓤"
      else s match {
        case CofiniteIntSet.Set(xs) => s"{${xs.mkString(", ")}}"
        case CofiniteIntSet.Compl(xs) => s"¬{${xs.mkString(", ")}}"
      }
    }
  }

  /** Represents a Zhegalkin term: c ∩ x1 ∩ x2 ∩ ... ∩ xn */
  case class ZhegalkinTerm(cst: ZhegalkinConstant, vars: SortedSet[ZhegalkinVar]) {
    override def toString: String =
      if (vars.isEmpty)
        cst.toString
      else
        s"$cst ∩ ${vars.mkString(" ∩ ")}"
  }

  /** Companion object for [[ZhegalkinExpr]] */
  private object ZhegalkinExpr {
    /** A Zhegalkin expression that represents the empty set, i.e. the zero element of the algebra. */
    val zero: ZhegalkinExpr = ZhegalkinExpr(ZhegalkinConstant.empty, Nil)

    /** A Zhegalkin expression that represents the universe, i.e. the one element of the algebra. */
    val one: ZhegalkinExpr = ZhegalkinExpr(ZhegalkinConstant.universe, Nil)

    /** Returns a Zhegalkin expression that represents a single variable, i.e. x ~~ Ø ⊕ (𝓤 ∩ x) */
    def mkVar(x: ZhegalkinVar): ZhegalkinExpr = ZhegalkinExpr(ZhegalkinConstant.empty, List(ZhegalkinTerm(ZhegalkinConstant.universe, SortedSet(x))))
  }

  /** Represents a Zhegalkin expr: c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn */
  case class ZhegalkinExpr(cst: ZhegalkinConstant, terms: List[ZhegalkinTerm]) {
    def vars: SortedSet[ZhegalkinVar] = terms.foldLeft(SortedSet.empty[ZhegalkinVar]) {
      case (s, t) => s ++ t.vars
    }

    private val grouped: Map[SortedSet[ZhegalkinVar], List[ZhegalkinTerm]] = terms.groupBy(_.vars)

    if (grouped.exists(_._2.length > 1)) {
      throw InternalCompilerException("Invariant violated: Duplicate term", SourceLocation.Unknown)
    }

    override def toString: String =
      if (terms.isEmpty)
        cst.toString
      else
        s"$cst ⊕ ${terms.map(t => s"($t)").mkString(" ⊕ ")}"
  }

  /**
    * A smart constructor to filter empty intersections.
    */
  private def mkZhegalkinExpr(cst: ZhegalkinConstant, terms: List[ZhegalkinTerm]): ZhegalkinExpr =
    ZhegalkinExpr(cst, terms.filter(t => !t.cst.s.isEmpty))

  /** Returns the xor of the two Zhegalkin constants */
  private def mkXor(c1: ZhegalkinConstant, c2: ZhegalkinConstant): ZhegalkinConstant = {
    // a ⊕ b = (a ∪ b) - (a ∩ b) = (a ∪ b) ∩ ¬(a ∩ b)
    c1.union(c2).inter(c1.inter(c2).compl)
  }

  /** Returns the xor of the two Zhegalkin expressions. */
  private def mkXor(z1: ZhegalkinExpr, z2: ZhegalkinExpr): ZhegalkinExpr = (z1, z2) match {
    case (ZhegalkinExpr(c1, ts1), ZhegalkinExpr(c2, ts2)) =>
      val c = mkXor(c1, c2)
      // Eliminate duplicates: t ⊕ t = 0
      val tsr1 = (ts1 ++ ts2).groupBy(identity).collect { case (k, v) if v.size % 2 != 0 => k }.toList

      // Merge coefficients: (c1 ∩ x1 ∩ x2) ⊕ (c2 ∩ x1 ∩ x2)
      val grouped = tsr1.groupBy(_.vars).toList
      val resTerms = grouped.map {
        case (vars, l) =>
          val mergedCst: ZhegalkinConstant = l.foldLeft(ZhegalkinConstant(CofiniteIntSet.empty)) { // Neutral element for Xor
            case (acc, t) => mkXor(acc, t.cst) // Distributive law: (c1 ∩ A) ⊕ (c2 ∩ A) = (c1 ⊕ c2) ∩ A
          }
          ZhegalkinTerm(mergedCst, vars)
      }
      mkZhegalkinExpr(c, resTerms)
  }

  /** Returns the complement of the Zhegalkin expr. */
  private def zmkNot(a: ZhegalkinExpr): ZhegalkinExpr =
    // ¬a = 1 ⊕ a
    mkXor(ZhegalkinExpr.one, a)

  //
  // (c1 ⊕ t11 ⊕ t12 ⊕ ... ⊕ t1n) ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //   =   (c1  ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //     ⊕ (t11 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //     ⊕ (t12 ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m)
  //
  private def zmkInter(z1: ZhegalkinExpr, z2: ZhegalkinExpr): ZhegalkinExpr = z1 match {
    case ZhegalkinExpr(c1, ts1) =>
      val zero = mkInterConstantExpr(c1, z2)
      ts1.foldLeft(zero) {
        case (acc, z) => mkXor(acc, mkInterTermExpr(z, z2))
      }
  }

  //
  // c ∩ (c2 ∩ x1 ∩ x2 ∩ ... ∩ xn) = (c ∩ c2) ∩ x1 ∩ x2 ∩ ... ∩ xn)
  //
  private def mkInterConstantTerm(c: ZhegalkinConstant, t: ZhegalkinTerm): ZhegalkinTerm = t match {
    case ZhegalkinTerm(c2, vars) =>
      ZhegalkinTerm(c.inter(c2), vars)
  }

  //
  // c ∩ (c2 ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m) = (c ∩ c2) ⊕ t21 ⊕ t22 ⊕ ... ⊕ t2m
  //
  private def mkInterConstantExpr(c: ZhegalkinConstant, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(c2, terms) =>
      val ts = terms.map(t => mkInterConstantTerm(c, t))
      mkZhegalkinExpr(c.inter(c2), ts)
  }

  //
  // t ∩ (c2 ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = (t ∩ c2) ⊕ (t ∩ t1) ⊕ (t ∩ t2) ⊕ ... ⊕ (t ∩ tn)
  //
  private def mkInterTermExpr(t: ZhegalkinTerm, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(c2, terms) =>
      val zero: ZhegalkinExpr = mkZhegalkinExpr(ZhegalkinConstant.empty, List(mkInterConstantTerm(c2, t)))
      terms.foldLeft(zero) {
        case (acc, t2) => mkXor(acc, mkZhegalkinExpr(ZhegalkinConstant.empty, List(mkInterTermTerm(t, t2))))
      }
  }

  // (c1 ∩ x11 ∩ x12 ∩ ... ∩ x1n) ∩ (c2 ∩ x21 ∩ x22 ∩ ... ∩ x2m)
  private def mkInterTermTerm(t1: ZhegalkinTerm, t2: ZhegalkinTerm): ZhegalkinTerm = (t1, t2) match {
    case (ZhegalkinTerm(c1, vars1), ZhegalkinTerm(c2, vars2)) =>
      ZhegalkinTerm(c1.inter(c2), vars1 ++ vars2)
  }

  /** Returns the union of the two Zhegalkin expressions. */
  private def zmkUnion(a: ZhegalkinExpr, b: ZhegalkinExpr): ZhegalkinExpr = {
    /** a ⊕ b = a ⊕ b ⊕ (a ∩ b) */
    mkXor(mkXor(a, b), zmkInter(a, b))
  }

  //
  // map(f, c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn) = c ⊕ map(f, t1) ⊕ map(f, t2) ⊕ ... ⊕ map(f, tn)
  //
  private def mapExpr(f: Int => ZhegalkinExpr, z: ZhegalkinExpr): ZhegalkinExpr = z match {
    case ZhegalkinExpr(_, Nil) => z

    case ZhegalkinExpr(cst, terms) => terms.foldLeft(ZhegalkinExpr(cst, Nil)) {
      case (acc, term) => mkXor(acc, mapTerm(f, term))
    }
  }

  //
  // map(f, c ∩ x1 ∩ x2 ∩ ... ∩ xn) = c ∩ map(f, x1) ∩ map(f, x2) ∩ ... ∩ map(f, xn)
  //
  private def mapTerm(f: Int => ZhegalkinExpr, t: ZhegalkinTerm): ZhegalkinExpr = t match {
    case ZhegalkinTerm(cst, vars) => vars.foldLeft(ZhegalkinExpr(cst, Nil)) {
      case (acc, x) => zmkInter(f(x.v), acc)
    }
  }

  /**
    * Returns `true` if the given Zhegalkin expression `z` represents the empty set.
    *
    * Note: The representation of Zhegalkin polynomials is unique, hence we can use a simple equality check here.
    */
  private def isEmpty(z: ZhegalkinExpr): Boolean = z == ZhegalkinExpr.zero

  // TODO: Need to distinguish free and rigid variables.
  private def zfreeVars(z: ZhegalkinExpr): SortedSet[Int] = z match {
    case ZhegalkinExpr(_, terms) => terms.foldLeft(SortedSet.empty[Int]) {
      case (acc, term) => acc ++ freeVarsTerm(term)
    }
  }

  private def freeVarsTerm(t: Zhegalkin.ZhegalkinTerm): SortedSet[Int] = t match {
    case ZhegalkinTerm(_, vars) => vars.filter(x => x.flexible).map(_.v)
  }

  /**
    * Returns the given set formula as a Zhegalkin polynomial.
    */
  def toZhegalkin(f: SetFormula): ZhegalkinExpr = f match {
    case SetFormula.Univ => ZhegalkinExpr(ZhegalkinConstant.universe, Nil)
    case SetFormula.Empty => ZhegalkinExpr(ZhegalkinConstant.empty, Nil)
    case Cst(c) => ZhegalkinExpr(ZhegalkinConstant.empty, List(ZhegalkinTerm(ZhegalkinConstant.universe, SortedSet(ZhegalkinVar(c, flexible = false)))))
    case Var(x) => ZhegalkinExpr(ZhegalkinConstant.empty, List(ZhegalkinTerm(ZhegalkinConstant.universe, SortedSet(ZhegalkinVar(x, flexible = true)))))
    case ElemSet(s) =>
      ZhegalkinExpr(ZhegalkinConstant(CofiniteIntSet.mkSet(s)), Nil)
    case Compl(f) => zmkNot(toZhegalkin(f))
    case Inter(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(zmkInter)
    case Union(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other) =>
      val terms = SetFormula.subformulasOf(elemPos, cstsPos, varsPos, elemNeg, cstsNeg, varsNeg, other).toList
      val polys = terms.map(toZhegalkin)
      polys.reduce(zmkUnion)
  }

  /** Returns the given Zhegalkin expression: `c ⊕ t1 ⊕ t2 ⊕ ... ⊕ tn` as a SetFormula. */
  def toSetFormula(z: ZhegalkinExpr): SetFormula = {
    val variables = z.vars
    val disjs = variables.subsets().map(pos => {
      val insts = variables.iterator.map {
        case zv@ZhegalkinVar(i, isFlexible) =>
          val v = if (isFlexible) Var(i) else Cst(i)
          if (pos.contains(zv)) v else mkCompl(v)
      }.toList
      mkInterAll(fromCofiniteIntSet(evaluate(z, pos)) :: insts)
    })
    mkUnionAll(disjs.toList)
  }

  /** Evaluates `z` where all variables in `pos` are universe and all others are empty. */
  private def evaluate(z: ZhegalkinExpr, pos: SortedSet[ZhegalkinVar]): CofiniteIntSet = {
    val ZhegalkinExpr(cst, terms) = z
    (cst.s :: terms.map(evaluate(_, pos))).reduce(CofiniteIntSet.xor)
  }

  /** Evaluates `z` where all variables in `pos` are universe and all others are empty. */
  private def evaluate(z: ZhegalkinTerm, pos: SortedSet[ZhegalkinVar]): CofiniteIntSet = {
    val ZhegalkinTerm(cst, vars) = z

    def instVar(v: ZhegalkinVar): CofiniteIntSet = if (pos.contains(v)) CofiniteIntSet.universe else CofiniteIntSet.empty

    (cst.s :: vars.toList.map(instVar)).reduce(CofiniteIntSet.intersection(_, _: CofiniteIntSet))
  }

  /** Returns the [[SetFormula]] representation of `s`. */
  private def fromCofiniteIntSet(s: CofiniteIntSet): SetFormula = s match {
    case CofiniteIntSet.Set(s) => mkElemSet(s)
    case CofiniteIntSet.Compl(s) => mkCompl(mkElemSet(s))
  }

  /**
    * Returns the given Zhegalkin term `c ∩ x1 ∩ x2 ∩ ... ∩ xn` as SetFormula.
    */
  def toSetFormula(t: ZhegalkinTerm): SetFormula = t match {
    case ZhegalkinTerm(cst, vars) => vars.foldLeft(toSetFormula(cst)) {
      case (acc, x) if x.flexible => SetFormula.mkInter(acc, SetFormula.Var(x.v))
      case (acc, x) => SetFormula.mkInter(acc, SetFormula.Cst(x.v))
    }
  }

  /**
    * Returns the given Zhegalkin constant as a SetFormula.
    */
  def toSetFormula(c: ZhegalkinConstant): SetFormula = c match {
    case ZhegalkinConstant(s) => s match {
      case CofiniteIntSet.Set(s) => SetFormula.mkElemSet(s)
      case CofiniteIntSet.Compl(s) => SetFormula.mkCompl(SetFormula.mkElemSet(s))
    }
  }

  object ZhegalkinAlgebra extends BoolAlg[ZhegalkinExpr] {
    override def isEquivBot(f: ZhegalkinExpr): Boolean = isEmpty(f)

    override def mkBot: ZhegalkinExpr = ZhegalkinExpr.zero

    override def mkTop: ZhegalkinExpr = ZhegalkinExpr.one

    override def mkCst(id: Int): ZhegalkinExpr = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = false))

    override def mkVar(id: Int): ZhegalkinExpr = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = true))

    override def mkNot(f: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.zmkNot(f)

    override def mkOr(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.zmkUnion(f1, f2)

    override def mkAnd(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.zmkInter(f1, f2)

    override def freeVars(f: ZhegalkinExpr): SortedSet[Int] = Zhegalkin.zfreeVars(f)

    override def map(f: ZhegalkinExpr)(fn: Int => ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.mapExpr(fn, f)
  }

}

