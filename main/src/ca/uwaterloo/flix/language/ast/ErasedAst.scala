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
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.phase.sjvm.{ClosureInfo, NamespaceInfo}

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  // TODO(JLS): add ast traversal sets/lists/map here
  // TODO(JLS): add enums like FinalAst i think
  // example: tuples: Set[RType[_ <: PType]]
  case class Root(functions: Map[Symbol.DefnSym, Def[_ <: PType]],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  functionTypes: Set[RType[PReference[PFunction[_ <: PType]]]],
                  closures: Set[ClosureInfo[_ <: PType]],
                  enumSyms: Set[Symbol.EnumSym],
                  namespaces: Set[NamespaceInfo])

  case class Def[T <: PType](ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FormalParam], exp: Expression[T], tpe: RType[PReference[PFunction[T]]], loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, Case], loc: SourceLocation)

  case class LatticeOps(tpe: RType[PType], bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression[T <: PType] {
    def tpe: RType[T]

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends Expression[PReference[PUnit]] {
      final val tpe = RType.RReference(RRefType.RUnit)
    }

    case class Null[S <: PRefType](tpe: RType[PReference[S]], loc: SourceLocation) extends Expression[PReference[S]]

    case class True(loc: SourceLocation) extends Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class False(loc: SourceLocation) extends Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends Expression[PChar] {
      final val tpe = RType.RChar
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends Expression[PFloat32] {
      final val tpe = RType.RFloat32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends Expression[PFloat64] {
      final val tpe = RType.RFloat64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends Expression[PInt8] {
      final val tpe = RType.RInt8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends Expression[PInt16] {
      final val tpe = RType.RInt16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends Expression[PInt32] {
      final val tpe = RType.RInt32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends Expression[PInt64] {
      final val tpe = RType.RInt64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends Expression[PReference[PBigInt]] {
      final val tpe = RType.RReference(RRefType.RBigInt)
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends Expression[PReference[PStr]] {
      final val tpe = RType.RReference(RRefType.RStr)
    }

    case class Var[T <: PType](sym: Symbol.VarSym, tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Closure[T <: PType](sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: RType[PReference[PFunction[T]]], loc: SourceLocation) extends Expression[PReference[PFunction[T]]]

    case class ApplyClo[T <: PType](exp: Expression[PReference[PFunction[T]]], args: List[Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class ApplyDef[T <: PType](sym: Symbol.DefnSym, args: List[Expression[_ <: PType]], functionType: RType[PReference[PFunction[T]]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class ApplyCloTail[T <: PType](exp: Expression[PReference[PFunction[T]]], args: List[Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class ApplyDefTail[T <: PType](sym: Symbol.DefnSym, args: List[Expression[_ <: PType]], functionType: RType[PReference[PFunction[T]]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class ApplySelfTail[T <: PType](sym: Symbol.DefnSym, formals: List[FormalParam], actuals: List[Expression[_ <: PType]], functionType: RType[PReference[PFunction[T]]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    // Unary expressions
    case class BoolNot(exp: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Float32Neg(exp: Expression[PFloat32], tpe: RType[PFloat32], loc: SourceLocation) extends Expression[PFloat32]

    case class Float64Neg(exp: Expression[PFloat64], tpe: RType[PFloat64], loc: SourceLocation) extends Expression[PFloat64]

    case class Int8Neg(exp: Expression[PInt8], tpe: RType[PInt8], loc: SourceLocation) extends Expression[PInt8]

    case class Int8Not(exp: Expression[PInt8], tpe: RType[PInt8], loc: SourceLocation) extends Expression[PInt8]

    case class Int16Neg(exp: Expression[PInt16], tpe: RType[PInt16], loc: SourceLocation) extends Expression[PInt16]

    case class Int16Not(exp: Expression[PInt16], tpe: RType[PInt16], loc: SourceLocation) extends Expression[PInt16]

    case class Int32Neg(exp: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int32Not(exp: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int64Neg(exp: Expression[PInt64], tpe: RType[PInt64], loc: SourceLocation) extends Expression[PInt64]

    case class Int64Not(exp: Expression[PInt64], tpe: RType[PInt64], loc: SourceLocation) extends Expression[PInt64]

    case class BigIntNeg(exp: Expression[PReference[PBigInt]], tpe: RType[PReference[PBigInt]], loc: SourceLocation) extends Expression[PReference[PBigInt]]

    case class BigIntNot(exp: Expression[PReference[PBigInt]], tpe: RType[PReference[PBigInt]], loc: SourceLocation) extends Expression[PReference[PBigInt]]

    case class ObjEqNull[T <: PRefType](exp: Expression[PReference[T]], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class ObjNeqNull[T <: PRefType](exp: Expression[PReference[T]], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    // Binary expressions
    case class BoolLogicalOp(op: LogicalOp, exp1: Expression[PInt32], exp2: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class BoolEquality(op: EqualityOp, exp1: Expression[PInt32], exp2: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class CharComparison(op: ComparisonOp, exp1: Expression[PChar], exp2: Expression[PChar], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Float32Arithmetic(op: ArithmeticOp, exp1: Expression[PFloat32], exp2: Expression[PFloat32], tpe: RType[PFloat32], loc: SourceLocation) extends Expression[PFloat32]

    case class Float32Comparison(op: ComparisonOp, exp1: Expression[PFloat32], exp2: Expression[PFloat32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Float64Arithmetic(op: ArithmeticOp, exp1: Expression[PFloat64], exp2: Expression[PFloat64], tpe: RType[PFloat64], loc: SourceLocation) extends Expression[PFloat64]

    case class Float64Comparison(op: ComparisonOp, exp1: Expression[PFloat64], exp2: Expression[PFloat64], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int8Arithmetic(op: ArithmeticOp, exp1: Expression[PInt8], exp2: Expression[PInt8], tpe: RType[PInt8], loc: SourceLocation) extends Expression[PInt8]

    case class Int16Arithmetic(op: ArithmeticOp, exp1: Expression[PInt16], exp2: Expression[PInt16], tpe: RType[PInt16], loc: SourceLocation) extends Expression[PInt16]

    case class Int32Arithmetic(op: ArithmeticOp, exp1: Expression[PInt32], exp2: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int64Arithmetic(op: ArithmeticOp, exp1: Expression[PInt64], exp2: Expression[PInt64], tpe: RType[PInt64], loc: SourceLocation) extends Expression[PInt64]

    case class BigIntArithmetic(op: ArithmeticOp, exp1: Expression[PReference[PBigInt]], exp2: Expression[PReference[PBigInt]], tpe: RType[PReference[PBigInt]], loc: SourceLocation) extends Expression[PReference[PBigInt]]

    case class Int8Bitwise(op: BitwiseOp, exp1: Expression[PInt8], exp2: Expression[PInt8], tpe: RType[PInt8], loc: SourceLocation) extends Expression[PInt8]

    case class Int16Bitwise(op: BitwiseOp, exp1: Expression[PInt16], exp2: Expression[PInt16], tpe: RType[PInt16], loc: SourceLocation) extends Expression[PInt16]

    case class Int32Bitwise(op: BitwiseOp, exp1: Expression[PInt32], exp2: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int64Bitwise(op: BitwiseOp, exp1: Expression[PInt64], exp2: Expression[PInt64], tpe: RType[PInt64], loc: SourceLocation) extends Expression[PInt64]

    case class BigIntBitwise(op: BitwiseOp, exp1: Expression[PReference[PBigInt]], exp2: Expression[PReference[PBigInt]], tpe: RType[PReference[PBigInt]], loc: SourceLocation) extends Expression[PReference[PBigInt]]

    case class Int8Comparison(op: ComparisonOp, exp1: Expression[PInt8], exp2: Expression[PInt8], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int16Comparison(op: ComparisonOp, exp1: Expression[PInt16], exp2: Expression[PInt16], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int32Comparison(op: ComparisonOp, exp1: Expression[PInt32], exp2: Expression[PInt32], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class Int64Comparison(op: ComparisonOp, exp1: Expression[PInt64], exp2: Expression[PInt64], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class BigIntComparison(op: ComparisonOp, exp1: Expression[PReference[PBigInt]], exp2: Expression[PReference[PBigInt]], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class StringConcat(exp1: Expression[PReference[PStr]], exp2: Expression[PReference[PStr]], tpe: RType[PReference[PStr]], loc: SourceLocation) extends Expression[PReference[PStr]]

    case class StringEquality(op: EqualityOp, exp1: Expression[PReference[PStr]], exp2: Expression[PReference[PStr]], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class IfThenElse[T <: PType](exp1: Expression[PInt32], exp2: Expression[T], exp3: Expression[T], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Branch[T <: PType](exp: Expression[T], branches: Map[Symbol.LabelSym, Expression[T]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class JumpTo[T <: PType](sym: Symbol.LabelSym, tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Let[T <: PType](sym: Symbol.VarSym, exp1: Expression[PType], exp2: Expression[T], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: Expression[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PInt32] {
      final val tpe: RType[PInt32] = RType.RBool
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: Expression[_ <: PType], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PReference[PAnyObject]]

    case class Untag[T <: PType](sym: Symbol.EnumSym, tag: Name.Tag, exp: Expression[PReference[PAnyObject]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Index[T <: PType](base: Expression[PReference[PAnyObject]], offset: scala.Int, tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Tuple(elms: List[Expression[_ <: PType]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PReference[PAnyObject]]

    case class RecordEmpty(tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PReference[PAnyObject]]

    case class RecordSelect[T <: PType](exp: Expression[PReference[PAnyObject]], field: Name.Field, tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class RecordExtend(field: Name.Field, value: Expression[_ <: PType], rest: Expression[PReference[PAnyObject]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PReference[PAnyObject]]

    case class RecordRestrict(field: Name.Field, rest: Expression[PReference[PAnyObject]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PReference[PAnyObject]]

    case class ArrayLit[T <: PType](elms: List[Expression[T]], tpe: RType[PReference[PArray[T]]], loc: SourceLocation) extends Expression[PReference[PArray[T]]]

    case class ArrayNew[T <: PType](elm: Expression[T], len: Expression[PInt32], tpe: RType[PReference[PArray[T]]], loc: SourceLocation) extends Expression[PReference[PArray[T]]]

    case class ArrayLoad[T <: PType](base: Expression[PReference[PArray[T]]], index: Expression[PInt32], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class ArrayStore[T <: PType](base: Expression[PReference[PArray[T]]], index: Expression[PInt32], elm: Expression[T], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends Expression[PReference[PUnit]]

    case class ArrayLength[T <: PType](base: Expression[PReference[PArray[T]]], tpe: RType[PInt32], loc: SourceLocation) extends Expression[PInt32]

    case class ArraySlice[T <: PType](base: Expression[PReference[PArray[T]]], beginIndex: Expression[PInt32], endIndex: Expression[PInt32], tpe: RType[PReference[PArray[T]]], loc: SourceLocation) extends Expression[PReference[PArray[T]]]

    case class Ref[T <: PType](exp: Expression[T], tpe: RType[PReference[PRef[T]]], loc: SourceLocation) extends Expression[PReference[PRef[T]]]

    case class Deref[T <: PType](exp: Expression[PReference[PRef[T]]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Assign[T <: PType](exp1: Expression[PReference[PRef[T]]], exp2: Expression[T], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends Expression[PReference[PUnit]]

    case class Existential(fparam: FormalParam, exp: Expression[PInt32], loc: SourceLocation) extends Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class Universal(fparam: FormalParam, exp: Expression[PInt32], loc: SourceLocation) extends Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class Cast[T1 <: PType, T2 <: PType](exp: Expression[T2], tpe: RType[T1], loc: SourceLocation) extends Expression[T1]

    case class TryCatch[T <: PType](exp: Expression[T], rules: List[CatchRule[T]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class InvokeConstructor(constructor: Constructor[_], args: List[Expression[_ <: PType]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends Expression[PReference[PAnyObject]]

    case class InvokeMethod[T1 <: PType, T2 <: PRefType](method: Method, exp: Expression[PReference[T2]], args: List[Expression[_ <: PType]], tpe: RType[T1], loc: SourceLocation) extends Expression[T1]

    case class InvokeStaticMethod[T <: PType](method: Method, args: List[Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class GetField[T <: PType](field: Field, exp: Expression[PReference[PAnyObject]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class PutField(field: Field, exp1: Expression[PReference[PAnyObject]], exp2: Expression[_ <: PType], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends Expression[PReference[PUnit]]

    case class GetStaticField[T <: PType](field: Field, tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class PutStaticField(field: Field, exp: Expression[_ <: PType], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends Expression[PReference[PUnit]]

    case class NewChannel[T <: PType](exp: Expression[PInt32], tpe: RType[PReference[PChan[T]]], loc: SourceLocation) extends Expression[PReference[PChan[T]]]

    case class GetChannel[T <: PType](exp: Expression[PReference[PChan[T]]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class PutChannel[T <: PType](exp1: Expression[PReference[PChan[T]]], exp2: Expression[T], tpe: RType[PReference[PChan[T]]], loc: SourceLocation) extends Expression[PReference[PChan[T]]]

    case class SelectChannel[T <: PType](rules: List[SelectChannelRule[T]], default: Option[Expression[T]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class Spawn(exp: Expression[PType], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends Expression[PReference[PUnit]]

    case class Lazy[T <: PType](exp: Expression[T], tpe: RType[PReference[PLazy[T]]], loc: SourceLocation) extends Expression[PReference[PLazy[T]]]

    case class Force[T <: PType](exp: Expression[PReference[PLazy[T]]], tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class HoleError[T <: PType](sym: Symbol.HoleSym, tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class MatchError[T <: PType](tpe: RType[T], loc: SourceLocation) extends Expression[T]

    case class BoxInt8(exp: Expression[PInt8], loc: SourceLocation) extends Expression[PReference[PBoxedInt8]] {
      final val tpe = RType.RReference(RRefType.RBoxedInt8)
    }

    case class BoxInt16(exp: Expression[PInt16], loc: SourceLocation) extends Expression[PReference[PBoxedInt16]] {
      final val tpe = RType.RReference(RRefType.RBoxedInt16)
    }

    case class BoxInt32(exp: Expression[PInt32], loc: SourceLocation) extends Expression[PReference[PBoxedInt32]] {
      final val tpe = RType.RReference(RRefType.RBoxedInt32)
    }

    case class BoxInt64(exp: Expression[PInt64], loc: SourceLocation) extends Expression[PReference[PBoxedInt64]] {
      final val tpe = RType.RReference(RRefType.RBoxedInt64)
    }

    case class BoxChar(exp: Expression[PChar], loc: SourceLocation) extends Expression[PReference[PBoxedChar]] {
      final val tpe = RType.RReference(RRefType.RBoxedChar)
    }

    case class BoxFloat32(exp: Expression[PFloat32], loc: SourceLocation) extends Expression[PReference[PBoxedFloat32]] {
      final val tpe = RType.RReference(RRefType.RBoxedFloat32)
    }

    case class BoxFloat64(exp: Expression[PFloat64], loc: SourceLocation) extends Expression[PReference[PBoxedFloat64]] {
      final val tpe = RType.RReference(RRefType.RBoxedFloat64)
    }

    case class UnboxInt8(exp: Expression[PReference[PBoxedInt8]], loc: SourceLocation) extends Expression[PInt8] {
      final val tpe = RType.RInt8
    }

    case class UnboxInt16(exp: Expression[PReference[PBoxedInt16]], loc: SourceLocation) extends Expression[PInt16] {
      final val tpe = RType.RInt16
    }

    case class UnboxInt32(exp: Expression[PReference[PBoxedInt32]], loc: SourceLocation) extends Expression[PInt32] {
      final val tpe = RType.RInt32
    }

    case class UnboxInt64(exp: Expression[PReference[PBoxedInt64]], loc: SourceLocation) extends Expression[PInt64] {
      final val tpe = RType.RInt64
    }

    case class UnboxChar(exp: Expression[PReference[PBoxedChar]], loc: SourceLocation) extends Expression[PChar] {
      final val tpe = RType.RChar
    }

    case class UnboxFloat32(exp: Expression[PReference[PBoxedFloat32]], loc: SourceLocation) extends Expression[PFloat32] {
      final val tpe = RType.RFloat32
    }

    case class UnboxFloat64(exp: Expression[PReference[PBoxedFloat64]], loc: SourceLocation) extends Expression[PFloat64] {
      final val tpe = RType.RFloat64
    }

  }

  case class SelectChannelRule[T <: PType](sym: Symbol.VarSym, chan: Expression[PReference[PChan[PType]]], exp: Expression[T])

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: RType[PType], loc: SourceLocation)

  case class CatchRule[T <: PType](sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expression[T])

  case class FormalParam(sym: Symbol.VarSym, tpe: RType[_ <: PType])

  case class FreeVar(sym: Symbol.VarSym, tpe: RType[_ <: PType])


  sealed trait Operator

  sealed trait ArithmeticOp extends Operator

  object ArithmeticOp {

    case object Add extends ArithmeticOp

    case object Sub extends ArithmeticOp

    case object Mul extends ArithmeticOp

    case object Div extends ArithmeticOp

    case object Rem extends ArithmeticOp

  }

  sealed trait ComparisonOp extends Operator

  object ComparisonOp {

    case object Lt extends ComparisonOp

    case object Le extends ComparisonOp

    case object Gt extends ComparisonOp

    case object Ge extends ComparisonOp

  }

  sealed trait EqualityOp extends ComparisonOp

  object EqualityOp {

    case object Eq extends EqualityOp

    case object Ne extends EqualityOp

  }

  sealed trait LogicalOp extends Operator

  object LogicalOp {

    case object And extends LogicalOp

    case object Or extends LogicalOp

  }

  sealed trait BitwiseOp extends Operator

  object BitwiseOp {

    case object And extends BitwiseOp

    case object Or extends BitwiseOp

    case object Xor extends BitwiseOp

    case object Shl extends BitwiseOp

    case object Shr extends BitwiseOp

  }

}

