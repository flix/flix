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
import ca.uwaterloo.flix.language.ast.Purity.{Impure, Pure}

import java.lang.reflect.{Constructor, Field, Method}

object OccurrenceAst {

  case class Root(defs: Map[Symbol.DefnSym, OccurrenceAst.Def],
                  enums: Map[Symbol.EnumSym, OccurrenceAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, cparams: List[OccurrenceAst.FormalParam], fparams: List[OccurrenceAst.FormalParam], exp: OccurrenceAst.Expression, context: DefContext, tpe: Type, purity: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, OccurrenceAst.Case], tpe: Type, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Expression {

    case class Constant(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplyClo(exp: OccurrenceAst.Expression, args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplyCloTail(exp: OccurrenceAst.Expression, args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[OccurrenceAst.FormalParam], actuals: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class IfThenElse(exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, exp3: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, occur: Occur, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Scope(sym: Symbol.VarSym, exp: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Tag(sym: Symbol.CaseSym, exp: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Untag(sym: Symbol.CaseSym, exp: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Index(base: OccurrenceAst.Expression, offset: scala.Int, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Tuple(elms: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class RecordSelect(exp: OccurrenceAst.Expression, field: Name.Field, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class RecordExtend(field: Name.Field, value: OccurrenceAst.Expression, rest: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class RecordRestrict(field: Name.Field, rest: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class ArrayLit(elms: List[OccurrenceAst.Expression], tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayNew(elm: OccurrenceAst.Expression, len: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayLoad(base: OccurrenceAst.Expression, index: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayStore(base: OccurrenceAst.Expression, index: OccurrenceAst.Expression, elm: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayLength(base: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class Ref(exp: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class Deref(exp: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class Assign(exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class InstanceOf(exp: OccurrenceAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = exp.purity

      def tpe: Type = Type.Bool
    }

    case class Cast(exp: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class TryCatch(exp: OccurrenceAst.Expression, rules: List[OccurrenceAst.CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class InvokeMethod(method: Method, exp: OccurrenceAst.Expression, args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[OccurrenceAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class GetField(field: Field, exp: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class PutField(field: Field, exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class GetStaticField(field: Field, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class PutStaticField(field: Field, exp: OccurrenceAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends OccurrenceAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, methods: List[OccurrenceAst.JvmMethod], loc: SourceLocation) extends OccurrenceAst.Expression

    case class Spawn(exp1: OccurrenceAst.Expression, exp2: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class Lazy(exp: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class Force(exp: OccurrenceAst.Expression, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Pure
    }

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

    case class MatchError(tpe: Type, loc: SourceLocation) extends OccurrenceAst.Expression {
      def purity: Purity = Impure
    }

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[OccurrenceAst.FormalParam], clo: OccurrenceAst.Expression, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: OccurrenceAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

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
   *  A def is `isDirectCall` if
   *  the expression consist of a single (non-self) call with trivial arguments
   *  `occur` represents the number of times a def is references in the entire program.
   *  `size` denotes the cumulative weight of each expression in the body of the def
   */
  case class DefContext(isDirectCall: Boolean, occur: Occur, size: Int, isSelfRecursive: Boolean)

}


