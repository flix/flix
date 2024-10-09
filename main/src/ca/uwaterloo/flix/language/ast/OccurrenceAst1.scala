/*
 * Copyright 2024 Magnus Madsen, Christian Bonde, Patrick Lundvig, Anna Krogh
 * 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.shared.{Annotations, Constant, Modifiers, Source}

object OccurrenceAst1 {

  case class Root(defs: Map[Symbol.DefnSym, OccurrenceAst1.Def],
                  effects: Map[Symbol.EffectSym, OccurrenceAst1.Effect],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Annotations, mod: Modifiers, sym: Symbol.DefnSym, fparams: List[(OccurrenceAst1.FormalParam, Occur)], exp: OccurrenceAst1.Expr, context: DefContext, tpe: MonoType, purity: Purity, loc: SourceLocation)

  case class Effect(ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, ann: Annotations, mod: Modifiers, fparams: List[FormalParam], tpe: MonoType, purity: Purity, loc: SourceLocation)

  sealed trait Expr {
    def tpe: MonoType

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Constant, tpe: MonoType, loc: SourceLocation) extends OccurrenceAst1.Expr {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends OccurrenceAst1.Expr {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[OccurrenceAst1.Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class ApplyClo(exp: OccurrenceAst1.Expr, exps: List[OccurrenceAst1.Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[OccurrenceAst1.Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class ApplyLocalDef(sym: Symbol.VarSym, exps: List[OccurrenceAst1.Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class IfThenElse(exp1: OccurrenceAst1.Expr, exp2: OccurrenceAst1.Expr, exp3: OccurrenceAst1.Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, OccurrenceAst1.Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class Let(sym: Symbol.VarSym, exp1: OccurrenceAst1.Expr, exp2: OccurrenceAst1.Expr, occur: Occur, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class Stmt(exp1: OccurrenceAst1.Expr, exp2: OccurrenceAst1.Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class Scope(sym: Symbol.VarSym, exp: OccurrenceAst1.Expr, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class TryCatch(exp: OccurrenceAst1.Expr, rules: List[OccurrenceAst1.CatchRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class TryWith(exp: OccurrenceAst1.Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class Do(op: Ast.OpSymUse, exps: List[OccurrenceAst1.Expr], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst1.Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: MonoType, purity: Purity, methods: List[OccurrenceAst1.JvmMethod], loc: SourceLocation) extends OccurrenceAst1.Expr

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[OccurrenceAst1.FormalParam], clo: OccurrenceAst1.Expr, retTpe: MonoType, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: OccurrenceAst1.Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: OccurrenceAst1.Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Modifiers, tpe: MonoType, loc: SourceLocation)

  sealed trait Occur

  object Occur {

    /**
      * Represents a variable that is not used in an expression.
      */
    case object Dead extends Occur

    /**
      * Represents a variables that occur exactly once in an expression.
      */
    case object Once extends Occur

    /**
      * Represents a variable that occur in expressions more than once.
      */
    case object Many extends Occur

    /**
      * Represents a variable that occur in more than one branch, e.g. match cases.
      */
    case object ManyBranch extends Occur

    /**
      * Represents a variable that we explicitly do not want to inline.
      */
    case object DontInline extends Occur
  }

  /**
    * `OccurDef` contains information that indicates whether or not a def should be inlined
    * A def is `isDirectCall` if
    * the expression consist of a single (non-self) call with trivial arguments
    * `occur` represents the number of times a def is references in the entire program.
    * `size` denotes the cumulative weight of each expression in the body of the def
    */
  case class DefContext(isDirectCall: Boolean, occur: Occur, size: Int, isSelfRecursive: Boolean)

}


