/*
 * Copyright 2023 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Purity.Pure
import ca.uwaterloo.flix.language.ast.shared.{Annotations, Constant, ExpPosition, Source}

import java.lang.reflect.Method

object ReducedAst {

  val empty: Root = Root(Map.empty, Map.empty, Set.empty, List.empty, None, Set.empty, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  effects: Map[Symbol.EffectSym, Effect],
                  types: Set[MonoType],
                  anonClasses: List[AnonClass],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation]) {

    def getMain: Option[Def] = entryPoint.flatMap(defs.get)

  }

  /**
    * pcPoints is initialized by [[ca.uwaterloo.flix.language.phase.Reducer]].
    */
  case class Def(ann: Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[FormalParam], fparams: List[FormalParam], lparams: List[LocalParam], pcPoints: Int, expr: Expr, tpe: MonoType, unboxedType: UnboxedType, loc: SourceLocation) {
    var method: Method = _
    val arrowType: MonoType.Arrow = MonoType.Arrow(fparams.map(_.tpe), tpe, expr.purity)
  }

  /** Remember the unboxed return type for test function generation. */
  case class UnboxedType(tpe: MonoType)

  case class Effect(ann: Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, ann: Annotations, mod: Ast.Modifiers, fparams: List[FormalParam], tpe: MonoType, purity: Purity, loc: SourceLocation)

  sealed trait Expr {
    def tpe: MonoType

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Constant, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyClo(exp: Expr, exps: List[Expr], ct: ExpPosition, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], ct: ExpPosition, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplySelfTail(sym: Symbol.DefnSym, actuals: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Stmt(exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, exp: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], ct: ExpPosition, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: MonoType, purity: Purity, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  case class AnonClass(name: String, clazz: java.lang.Class[?], tpe: MonoType, methods: List[JvmMethod], loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: MonoType, loc: SourceLocation)

  case class LocalParam(sym: Symbol.VarSym, tpe: MonoType)

}

