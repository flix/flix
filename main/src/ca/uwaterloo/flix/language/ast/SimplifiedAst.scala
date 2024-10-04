/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Ast.IntroducedBy
import ca.uwaterloo.flix.language.ast.Purity.Pure
import ca.uwaterloo.flix.language.ast.shared.{Annotations, Constant, Source}
import ca.uwaterloo.flix.language.phase.ClosureConv

object SimplifiedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, None, Set.empty, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  effects: Map[Symbol.EffectSym, Effect],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[FormalParam], exp: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation)

  case class Enum(ann: Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], cases: Map[Symbol.CaseSym, Case], loc: SourceLocation)

  case class Struct(ann: Annotations, mod: Ast.Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], fields: List[StructField], loc: SourceLocation)

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

    case class Lambda(fparams: List[FormalParam], exp: Expr, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Apply(exp: Expr, args: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    @IntroducedBy(ClosureConv.getClass)
    case class LambdaClosure(cparams: List[FormalParam], fparams: List[FormalParam], freeVars: List[FreeVar], exp: Expr, tpe: MonoType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyClo(exp: Expr, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class LetRec(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, exp: Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: MonoType, purity: Purity, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  /** [[Type]] is used here because [[Enum]] declarations are not monomorphized! */
  case class Case(sym: Symbol.CaseSym, tpe: Type, loc: SourceLocation)

  /** [[Type]] is used here because [[Struct]] declarations are not monomorphized! */
  case class StructField(sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: MonoType, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: MonoType, loc: SourceLocation)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: MonoType) extends Ordered[FreeVar] {
    override def compare(that: FreeVar): Int = this.sym.compare(that.sym)
  }

}
