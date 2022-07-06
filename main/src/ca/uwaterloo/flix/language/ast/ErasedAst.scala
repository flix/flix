/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
                  enums: Map[Symbol.EnumSym, ErasedAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], tpeDeprecated: MonoType, loc: SourceLocation)

  sealed trait Expression {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Unit
    }

    case class Null(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class True(loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Bool
    }

    case class False(loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Str
    }

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[ErasedAst.Expression], fnMonoType: MonoType, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ApplyClo(exp: ErasedAst.Expression, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ApplyCloTail(exp: ErasedAst.Expression, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class IfThenElse(exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, exp3: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Branch(exp: ErasedAst.Expression, branches: Map[Symbol.LabelSym, ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe: MonoType = MonoType.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Index(base: ErasedAst.Expression, offset: scala.Int, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Tuple(elms: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class RecordEmpty(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class RecordSelect(exp: ErasedAst.Expression, field: Name.Field, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression, rest: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ArrayLit(elms: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ArrayNew(elm: ErasedAst.Expression, len: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ArrayLoad(base: ErasedAst.Expression, index: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ArrayStore(base: ErasedAst.Expression, index: ErasedAst.Expression, elm: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ArrayLength(base: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class ArraySlice(base: ErasedAst.Expression, beginIndex: ErasedAst.Expression, endIndex: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Ref(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Deref(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Assign(exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Cast(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class TryCatch(exp: ErasedAst.Expression, rules: List[ErasedAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class InvokeMethod(method: Method, exp: ErasedAst.Expression, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class GetField(field: Field, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class PutField(field: Field, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class GetStaticField(field: Field, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class PutStaticField(field: Field, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class NewObject(clazz: java.lang.Class[_], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression {
      final val name = s"Anon${this.hashCode}"
    }

    case class NewChannel(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class GetChannel(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class PutChannel(exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class SelectChannel(rules: List[ErasedAst.SelectChannelRule], default: Option[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Spawn(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Lazy(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class Force(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class MatchError(tpe: MonoType, loc: SourceLocation) extends ErasedAst.Expression

    case class BoxBool(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Boolean.TYPE)
    }

    case class BoxInt8(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Byte.TYPE)
    }

    case class BoxInt16(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Short.TYPE)
    }

    case class BoxInt32(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Integer.TYPE)
    }

    case class BoxInt64(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Long.TYPE)
    }

    case class BoxChar(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Character.TYPE)
    }

    case class BoxFloat32(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Float.TYPE)
    }

    case class BoxFloat64(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Native(java.lang.Double.TYPE)
    }

    case class UnboxBool(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Bool
    }

    case class UnboxInt8(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int8
    }

    case class UnboxInt16(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int16
    }

    case class UnboxInt32(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int32
    }

    case class UnboxInt64(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Int64
    }

    case class UnboxChar(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Char
    }

    case class UnboxFloat32(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Float32
    }

    case class UnboxFloat64(exp: ErasedAst.Expression, loc: SourceLocation) extends ErasedAst.Expression {
      final val tpe = MonoType.Float64
    }
  }

  case class SelectChannelRule(sym: Symbol.VarSym, chan: ErasedAst.Expression, exp: ErasedAst.Expression)

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)
}
