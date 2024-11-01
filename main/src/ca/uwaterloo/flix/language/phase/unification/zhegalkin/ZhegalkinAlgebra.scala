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

import ca.uwaterloo.flix.language.phase.unification.shared.BoolAlg

import scala.collection.immutable.SortedSet

object ZhegalkinAlgebra extends BoolAlg[ZhegalkinExpr] {

  override def isEquivBot(f: ZhegalkinExpr): Boolean = ZhegalkinExpr.isEmpty(f)

  override def mkBot: ZhegalkinExpr = ZhegalkinExpr.zero

  override def mkTop: ZhegalkinExpr = ZhegalkinExpr.one

  override def mkCst(id: Int): ZhegalkinExpr = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = false))

  override def mkVar(id: Int): ZhegalkinExpr = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = true))

  override def mkNot(f: ZhegalkinExpr): ZhegalkinExpr = ZhegalkinExpr.zmkNot(f)

  override def mkOr(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = ZhegalkinExpr.zmkUnion(f1, f2)

  override def mkAnd(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = ZhegalkinExpr.zmkInter(f1, f2)

  // Performance: We must override the default implementation of `mkXor` to increase performance.
  override def mkXor(f1: ZhegalkinExpr, f2: ZhegalkinExpr): ZhegalkinExpr = ZhegalkinExpr.mkXor(f1, f2)

  override def freeVars(f: ZhegalkinExpr): SortedSet[Int] = ZhegalkinExpr.zfreeVars(f)

  override def map(f: ZhegalkinExpr)(fn: Int => ZhegalkinExpr): ZhegalkinExpr = ZhegalkinExpr.mapExpr(fn, f)

}
