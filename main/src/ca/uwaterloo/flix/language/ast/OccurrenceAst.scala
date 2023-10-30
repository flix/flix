/*
 * Copyright 2022 Magnus Madsen, Christian Bonde, Patrick Lundvig, Anna Krogh
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

import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.Purity.Pure

object OccurrenceAst {

  case class Root(defs: Map[Symbol.DefnSym, OccurrenceAst.Def],
                  enums: Map[Symbol.EnumSym, OccurrenceAst.Enum],
                  effects: Map[Symbol.EffectSym, OccurrenceAst.Effect],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[OccurrenceAst.FormalParam], fparams: List[OccurrenceAst.FormalParam], exp: OccurrenceAst.Expression, context: DefContext, tpe: MonoType, purity: Purity, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, OccurrenceAst.Case], tpe: MonoType, loc: SourceLocation)

  case class Effect(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, ann: Ast.Annotations, mod: Ast.Modifiers, fparams: List[FormalParam], tpe: MonoType, purity: Purity, loc: SourceLocation)

  sealed trait Expression {
    def tpe: MonoType

    def purity: Purity

    def loc: SourceLocation
  }

  object Expression {

    case class Constant(cst: Ast.Constant, tpe: MonoType, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[OccurrenceAst.Expression], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplyClo(exp: OccurrenceAst.Expression, exps: List[OccurrenceAst.Expression], ct: Ast.CallType, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[OccurrenceAst.Expression], ct: Ast.CallType, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[OccurrenceAst.FormalParam], actuals: List[OccurrenceAst.Expression], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class IfThenElse(exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, exp3: OccurrenceAst.Expression, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, OccurrenceAst.Expression], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, occur: Occur, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Scope(sym: Symbol.VarSym, exp: OccurrenceAst.Expression, tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class TryCatch(exp: OccurrenceAst.Expression, rules: List[OccurrenceAst.CatchRule], tpe: MonoType, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, purity: Purity, methods: List[OccurrenceAst.JvmMethod], loc: SourceLocation) extends OccurrenceAst.Expression

  }

  case class Case(sym: Symbol.CaseSym, tpe: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[OccurrenceAst.FormalParam], clo: OccurrenceAst.Expression, retTpe: MonoType, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: OccurrenceAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: MonoType, loc: SourceLocation)

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


