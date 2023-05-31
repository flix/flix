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

import java.lang.reflect.{Constructor, Field, Method}
import ca.uwaterloo.flix.language.ast.Ast.{IntroducedBy, Source}
import ca.uwaterloo.flix.language.ast.Purity.{Impure, Pure}
import ca.uwaterloo.flix.language.phase.{ClosureConv, LambdaLift}

object SimplifiedAst {

  val empty: Root = SimplifiedAst.Root(Map.empty, Map.empty, None, Map.empty)

  case class Root(defs: Map[Symbol.DefnSym, SimplifiedAst.Def],
                  enums: Map[Symbol.EnumSym, SimplifiedAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, purity: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, SimplifiedAst.Case], tpe: Type, loc: SourceLocation)

  sealed trait Expression {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Lambda(fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Apply(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class LambdaClosure(cparams: List[SimplifiedAst.FormalParam], fparams: List[SimplifiedAst.FormalParam], freeVars: List[FreeVar], exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    @IntroducedBy(LambdaLift.getClass)
    case class Closure(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyClo(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    @IntroducedBy(ClosureConv.getClass)
    case class ApplyDef(sym: Symbol.DefnSym, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Unary(sop: SemanticOperator, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Binary(sop: SemanticOperator, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class IfThenElse(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Branch(exp: Expression, branches: Map[Symbol.LabelSym, SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class LetRec(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Region(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Scope(sym: Symbol.VarSym, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ScopeExit(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Is(sym: Symbol.CaseSym, exp: SimplifiedAst.Expression, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Tag(sym: Symbol.CaseSym, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Untag(sym: Symbol.CaseSym, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Index(base: SimplifiedAst.Expression, offset: scala.Int, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tuple(elms: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class RecordSelect(exp: SimplifiedAst.Expression, field: Name.Field, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordExtend(field: Name.Field, value: SimplifiedAst.Expression, rest: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class ArrayLit(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayNew(elm: SimplifiedAst.Expression, len: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayLoad(base: SimplifiedAst.Expression, index: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayStore(base: SimplifiedAst.Expression, index: SimplifiedAst.Expression, elm: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class ArrayLength(base: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Ref(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class Deref(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class Assign(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class InstanceOf(exp: SimplifiedAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool

      def purity: Purity = exp.purity
    }

    case class Cast(exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class TryCatch(exp: SimplifiedAst.Expression, rules: List[SimplifiedAst.CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class InvokeMethod(method: Method, exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[SimplifiedAst.Expression], tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class GetField(field: Field, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class PutField(field: Field, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class GetStaticField(field: Field, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class PutStaticField(field: Field, exp: SimplifiedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation) extends SimplifiedAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, methods: List[SimplifiedAst.JvmMethod], loc: SourceLocation) extends SimplifiedAst.Expression

    case class Spawn(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class Lazy(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class Force(exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Pure
    }

    case class HoleError(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

    case class MatchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      def purity: Purity = Impure
    }

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: SimplifiedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class FreeVar(sym: Symbol.VarSym, tpe: Type) extends Ordered[FreeVar] {
    override def compare(that: FreeVar): Int = this.sym.compare(that.sym)
  }

}
