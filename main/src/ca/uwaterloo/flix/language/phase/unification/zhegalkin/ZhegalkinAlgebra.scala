package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.phase.unification.shared.BoolAlg
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.Zhegalkin.{ZhegalkinExpr, ZhegalkinVar}

import scala.collection.immutable.SortedSet

object ZhegalkinAlgebra extends BoolAlg[ZhegalkinExpr] {
  override def isEquivBot(f: ZhegalkinExpr): Boolean = Zhegalkin.isEmpty(f)

  override def mkBot: ZhegalkinExpr = ZhegalkinExpr.zero

  override def mkTop: ZhegalkinExpr = ZhegalkinExpr.one

  override def mkCst(id: Int): ZhegalkinExpr = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = false))

  override def mkVar(id: Int): ZhegalkinExpr = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = true))

  override def mkNot(f: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.zmkNot(f)

  override def mkOr(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.zmkUnion(f1, f2)

  override def mkAnd(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.zmkInter(f1, f2)

  // Performance: We must override the default implementation of `mkXor` to increase performance.
  override def mkXor(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.mkXor(f1, f2)

  override def freeVars(f: ZhegalkinExpr): SortedSet[Int] = Zhegalkin.zfreeVars(f)

  override def map(f: ZhegalkinExpr)(fn: Int => ZhegalkinExpr): ZhegalkinExpr = Zhegalkin.mapExpr(fn, f)
}
