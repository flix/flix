/*
 * Copyright 2025 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.shared.SymUse.{EffSymUse, OpSymUse}
import ca.uwaterloo.flix.language.ast.shared.*

object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  effects: Map[Symbol.EffSym, Effect],
                  mainEntryPoint: Option[Symbol.DefnSym],
                  entryPoints: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Annotations, mod: Modifiers, sym: Symbol.DefnSym, cparams: List[FormalParam], fparams: List[FormalParam], exp: Expr, tpe: SimpleType, unboxedType: UnboxedType, loc: SourceLocation) {
    val arrowType: SimpleType.Arrow = SimpleType.mkArrow(fparams.map(_.tpe), tpe)
  }

  /** Remember the unboxed return type for test function generation. */
  case class UnboxedType(tpe: SimpleType)

  case class Enum(ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, Case], loc: SourceLocation)

  case class Struct(ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, fields: List[StructField], loc: SourceLocation)

  case class Effect(ann: Annotations, mod: Modifiers, sym: Symbol.EffSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, ann: Annotations, mod: Modifiers, fparams: List[FormalParam], tpe: SimpleType, purity: Purity, loc: SourceLocation)

  sealed trait Expr {
    def tpe: SimpleType

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Constant, loc: SourceLocation) extends Expr {
      def tpe: SimpleType = cst.tpe

      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: SimpleType, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyClo(exp1: Expr, exp2: Expr, ct: ExpPosition, tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], ct: ExpPosition, tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplyOp(sym: Symbol.OpSym, exps: List[Expr], tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class ApplySelfTail(sym: Symbol.DefnSym, actuals: List[Expr], tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr {
      // Note: We use an implicit representation of type and purity to aid correctness and to save memory.
      def tpe: SimpleType = exp2.tpe

      def purity: Purity = Purity.combine(exp1.purity, exp2.purity)
    }

    case class Stmt(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr {
      // Note: We use an implicit representation of type and purity to aid correctness and to save memory.
      def tpe: SimpleType = exp2.tpe

      def purity: Purity = Purity.combine(exp1.purity, exp2.purity)
    }

    case class Region(sym: Symbol.VarSym, exp: Expr, tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class RunWith(exp: Expr, effUse: EffSymUse, rules: List[HandlerRule], ct: ExpPosition, tpe: SimpleType, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: SimpleType, purity: Purity, constructors: List[JvmConstructor], methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  case class Case(sym: Symbol.CaseSym, tpes: List[SimpleType], loc: SourceLocation)

  case class StructField(sym: Symbol.StructFieldSym, tpe: SimpleType, loc: SourceLocation)

  case class AnonClass(name: String, clazz: java.lang.Class[?], tpe: SimpleType, constructors: List[JvmConstructor], methods: List[JvmMethod], loc: SourceLocation)

  case class JvmConstructor(exp: Expr, tpe: SimpleType, purity: Purity, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, tpe: SimpleType, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: OpSymUse, fparams: List[FormalParam], exp: Expr)

  // Note: We deliberately omit the source location because it (a) is unused and (b) takes memory.
  case class FormalParam(sym: Symbol.VarSym, tpe: SimpleType)

  // Note: We deliberately omit the source location because it (a) is unused and (b) takes memory.
  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym)

  // Note: We deliberately omit the source location because it (a) is unused and (b) takes memory.
  case class LocalParam(sym: Symbol.VarSym, tpe: SimpleType)

}

