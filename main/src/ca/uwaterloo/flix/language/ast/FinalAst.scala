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

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

object FinalAst {

  case class Root(defs: Map[Symbol.DefnSym, FinalAst.Def],
                  enums: Map[Symbol.EnumSym, FinalAst.Enum],
                  latticeOps: Map[MonoType, FinalAst.LatticeOps],
                  properties: List[FinalAst.Property],
                  specialOps: Map[SpecialOperator, Map[MonoType, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, FinalAst.Case], tpeDeprecated: MonoType, loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: FinalAst.Expression) {
    def loc: SourceLocation = defn.loc
  }

  case class LatticeOps(tpe: MonoType, bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Unit
    }

    case class Null(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class True(loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Bool
    }

    case class False(loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe = MonoType.Str
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    // TODO: Get rid of the fnMonoType here.
    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], fnMonoType: MonoType, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyClo(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyCloTail(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], actuals: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class IfThenElse(exp1: FinalAst.Expression, exp2: FinalAst.Expression, exp3: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Branch(exp: FinalAst.Expression, branches: Map[Symbol.LabelSym, FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe: MonoType = MonoType.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: Name.Tag, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Index(base: FinalAst.Expression, offset: scala.Int, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Tuple(elms: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordEmpty(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordSelect(exp: FinalAst.Expression, field: Name.Field, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordExtend(field: Name.Field, value: FinalAst.Expression, rest: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordRestrict(field: Name.Field, rest: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLit(elms: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayNew(elm: FinalAst.Expression, len: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLoad(base: FinalAst.Expression, index: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayStore(base: FinalAst.Expression, index: FinalAst.Expression, elm: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLength(base: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArraySlice(base: FinalAst.Expression, beginIndex: FinalAst.Expression, endIndex: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Ref(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Deref(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Assign(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Existential(fparam: FinalAst.FormalParam, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      def tpe: MonoType = MonoType.Bool
    }

    case class Universal(fparam: FinalAst.FormalParam, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      def tpe: MonoType = MonoType.Bool
    }

    case class Cast(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class TryCatch(exp: FinalAst.Expression, rules: List[FinalAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InvokeMethod(method: Method, exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class GetField(field: Field, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class PutField(field: Field, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class GetStaticField(field: Field, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class PutStaticField(field: Field, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class NewChannel(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class GetChannel(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class PutChannel(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class SelectChannel(rules: List[FinalAst.SelectChannelRule], default: Option[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Spawn(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Lazy(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Force(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class MatchError(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

  }

  case class SelectChannelRule(sym: Symbol.VarSym, chan: FinalAst.Expression, exp: FinalAst.Expression)

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: FinalAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)

  case class FreeVar(sym: Symbol.VarSym, tpe: MonoType)

}
