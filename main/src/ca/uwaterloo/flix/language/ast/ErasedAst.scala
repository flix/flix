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
import ca.uwaterloo.flix.language.phase.sjvm.NamespaceInfo

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  // TODO(JLS): add ast traversal sets/lists/map here
  // example: tuples: Set[RType[_ <: PType]]
  case class Root(defs: Map[Symbol.DefnSym, ErasedAst.Def],
                  //enums: Map[Symbol.EnumSym, ErasedAst.Enum],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  functionTypes: Set[RType[PReference[PFunction]]],
                  namespaces: Set[NamespaceInfo])

  // TODO(JLS): This method, of using T <: PType and the _ <: PType at use is probably better and should be used elsewhere
  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression[_ <: PType], tpe: RType[PReference[PFunction]], loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Name.Tag, ErasedAst.Case], loc: SourceLocation)

  case class LatticeOps(tpe: RType[PType], bot: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym)

  sealed trait Expression[T <: PType] {
    def tpe: RType[T]

    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]] {
      final val tpe = RType.RReference(RRefType.RUnit)
    }

    case class Null[S <: PRefType](tpe: RType[PReference[S]], loc: SourceLocation) extends ErasedAst.Expression[PReference[S]]

    case class True(loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class False(loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends ErasedAst.Expression[PChar] {
      final val tpe = RType.RChar
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ErasedAst.Expression[PFloat32] {
      final val tpe = RType.RFloat32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ErasedAst.Expression[PFloat64] {
      final val tpe = RType.RFloat64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ErasedAst.Expression[PInt8] {
      final val tpe = RType.RInt8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ErasedAst.Expression[PInt16] {
      final val tpe = RType.RInt16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = RType.RInt32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ErasedAst.Expression[PInt64] {
      final val tpe = RType.RInt64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ErasedAst.Expression[PReference[PBigInt]] {
      final val tpe = RType.RReference(RRefType.RBigInt)
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ErasedAst.Expression[PReference[PStr]] {
      final val tpe = RType.RReference(RRefType.RStr)
    }

    case class Var[T <: PType](sym: Symbol.VarSym, tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: RType[PReference[PFunction]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PFunction]]

    case class ApplyClo[T <: PType](exp: ErasedAst.Expression[PReference[PFunction]], args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDef[T <: PType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyCloTail[T <: PType](exp: ErasedAst.Expression[PReference[PFunction]], args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplyDefTail[T <: PType](sym: Symbol.DefnSym, args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ApplySelfTail[T <: PType](sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    // TODO(JLS): maybe make multiple classes for different exp types
    case class Unary[T <: PType](sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression[PType], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Binary[T <: PType](sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression[PType], exp2: ErasedAst.Expression[PType], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class IfThenElse[T <: PType](exp1: ErasedAst.Expression[PInt32], exp2: ErasedAst.Expression[T], exp3: ErasedAst.Expression[T], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Branch[T <: PType](exp: ErasedAst.Expression[T], branches: Map[Symbol.LabelSym, ErasedAst.Expression[T]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class JumpTo[T <: PType](sym: Symbol.LabelSym, tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Let[T <: PType](sym: Symbol.VarSym, exp1: ErasedAst.Expression[PType], exp2: ErasedAst.Expression[T], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Is(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe: RType[PInt32] = RType.RBool
    }

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PType], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class Untag[T <: PType](sym: Symbol.EnumSym, tag: Name.Tag, exp: ErasedAst.Expression[PReference[PAnyObject]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Index[T <: PType](base: ErasedAst.Expression[PReference[PAnyObject]], offset: scala.Int, tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Tuple(elms: List[ErasedAst.Expression[_ <: PType]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class RecordEmpty(tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class RecordSelect[T <: PType](exp: ErasedAst.Expression[PReference[PAnyObject]], field: Name.Field, tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class RecordExtend(field: Name.Field, value: ErasedAst.Expression[_ <: PType], rest: ErasedAst.Expression[PReference[PAnyObject]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class RecordRestrict(field: Name.Field, rest: ErasedAst.Expression[PReference[PAnyObject]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class ArrayLit[T <: PType](elms: List[ErasedAst.Expression[T]], tpe: RType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class ArrayNew[T <: PType](elm: ErasedAst.Expression[T], len: ErasedAst.Expression[PInt32], tpe: RType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class ArrayLoad[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], index: ErasedAst.Expression[PInt32], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class ArrayStore[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], index: ErasedAst.Expression[PInt32], elm: ErasedAst.Expression[T], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class ArrayLength[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], tpe: RType[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PInt32]

    case class ArraySlice[T <: PType](base: ErasedAst.Expression[PReference[PArray[T]]], beginIndex: ErasedAst.Expression[PInt32], endIndex: ErasedAst.Expression[PInt32], tpe: RType[PReference[PArray[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PArray[T]]]

    case class Ref[T <: PType](exp: ErasedAst.Expression[T], tpe: RType[PReference[PRef[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PRef[T]]]

    case class Deref[T <: PType](exp: ErasedAst.Expression[PReference[PRef[T]]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Assign[T <: PType](exp1: ErasedAst.Expression[PReference[PRef[T]]], exp2: ErasedAst.Expression[T], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class Existential(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class Universal(fparam: ErasedAst.FormalParam, exp: ErasedAst.Expression[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
      final val tpe = RType.RBool
    }

    case class Cast[T1 <: PType, T2 <: PType](exp: ErasedAst.Expression[T2], tpe: RType[T1], loc: SourceLocation) extends ErasedAst.Expression[T1]

    case class TryCatch[T <: PType](exp: ErasedAst.Expression[T], rules: List[ErasedAst.CatchRule[T]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[PReference[PAnyObject]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PAnyObject]]

    case class InvokeMethod[T <: PType](method: Method, exp: ErasedAst.Expression[PReference[PAnyObject]], args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class InvokeStaticMethod[T <: PType](method: Method, args: List[ErasedAst.Expression[_ <: PType]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class GetField[T <: PType](field: Field, exp: ErasedAst.Expression[PReference[PAnyObject]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutField(field: Field, exp1: ErasedAst.Expression[PReference[PAnyObject]], exp2: ErasedAst.Expression[_ <: PType], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class GetStaticField[T <: PType](field: Field, tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutStaticField(field: Field, exp: ErasedAst.Expression[_ <: PType], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class NewChannel[T <: PType](exp: ErasedAst.Expression[PInt32], tpe: RType[PReference[PChan[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PChan[T]]]

    case class GetChannel[T <: PType](exp: ErasedAst.Expression[PReference[PChan[T]]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class PutChannel[T <: PType](exp1: ErasedAst.Expression[PReference[PChan[T]]], exp2: ErasedAst.Expression[T], tpe: RType[PReference[PChan[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PChan[T]]]

    case class SelectChannel[T <: PType](rules: List[ErasedAst.SelectChannelRule[T]], default: Option[ErasedAst.Expression[T]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class Spawn(exp: ErasedAst.Expression[PType], tpe: RType[PReference[PUnit]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PUnit]]

    case class Lazy[T <: PType](exp: ErasedAst.Expression[T], tpe: RType[PReference[PLazy[T]]], loc: SourceLocation) extends ErasedAst.Expression[PReference[PLazy[T]]]

    case class Force[T <: PType](exp: ErasedAst.Expression[PReference[PLazy[T]]], tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class HoleError[T <: PType](sym: Symbol.HoleSym, tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

    case class MatchError[T <: PType](tpe: RType[T], loc: SourceLocation) extends ErasedAst.Expression[T]

  }

  case class SelectChannelRule[T <: PType](sym: Symbol.VarSym, chan: ErasedAst.Expression[PReference[PChan[PType]]], exp: ErasedAst.Expression[T])

  case class Case(sym: Symbol.EnumSym, tag: Name.Tag, tpeDeprecated: RType[PType], loc: SourceLocation)

  case class CatchRule[T <: PType](sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression[T])

  case class FormalParam(sym: Symbol.VarSym, tpe: RType[_ <: PType])

  case class FreeVar(sym: Symbol.VarSym, tpe: RType[PType])

  case class BoxInt8(exp: ErasedAst.Expression[PInt8], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt8]] {
    final val tpe = RType.RReference(RRefType.RBoxedInt8)
  }

  case class BoxInt16(exp: ErasedAst.Expression[PInt16], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt16]] {
    final val tpe = RType.RReference(RRefType.RBoxedInt16)
  }

  case class BoxInt32(exp: ErasedAst.Expression[PInt32], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt32]] {
    final val tpe = RType.RReference(RRefType.RBoxedInt32)
  }

  case class BoxInt64(exp: ErasedAst.Expression[PInt64], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedInt64]] {
    final val tpe = RType.RReference(RRefType.RBoxedInt64)
  }

  case class BoxChar(exp: ErasedAst.Expression[PChar], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedChar]] {
    final val tpe = RType.RReference(RRefType.RBoxedChar)
  }

  case class BoxFloat32(exp: ErasedAst.Expression[PFloat32], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedFloat32]] {
    final val tpe = RType.RReference(RRefType.RBoxedFloat32)
  }

  case class BoxFloat64(exp: ErasedAst.Expression[PFloat64], loc: SourceLocation) extends ErasedAst.Expression[PReference[PBoxedFloat64]] {
    final val tpe = RType.RReference(RRefType.RBoxedFloat64)
  }

  case class UnboxInt8(exp: ErasedAst.Expression[PReference[PBoxedInt8]], loc: SourceLocation) extends ErasedAst.Expression[PInt8] {
    final val tpe = RType.RInt8
  }

  case class UnboxInt16(exp: ErasedAst.Expression[PReference[PBoxedInt16]], loc: SourceLocation) extends ErasedAst.Expression[PInt16] {
    final val tpe = RType.RInt16
  }

  case class UnboxInt32(exp: ErasedAst.Expression[PReference[PBoxedInt32]], loc: SourceLocation) extends ErasedAst.Expression[PInt32] {
    final val tpe = RType.RInt32
  }

  case class UnboxInt64(exp: ErasedAst.Expression[PReference[PBoxedInt64]], loc: SourceLocation) extends ErasedAst.Expression[PInt64] {
    final val tpe = RType.RInt64
  }

  case class UnboxChar(exp: ErasedAst.Expression[PReference[PBoxedChar]], loc: SourceLocation) extends ErasedAst.Expression[PChar] {
    final val tpe = RType.RChar
  }

  case class UnboxFloat32(exp: ErasedAst.Expression[PReference[PBoxedFloat32]], loc: SourceLocation) extends ErasedAst.Expression[PFloat32] {
    final val tpe = RType.RFloat32
  }

  case class UnboxFloat64(exp: ErasedAst.Expression[PReference[PBoxedFloat64]], loc: SourceLocation) extends ErasedAst.Expression[PFloat64] {
    final val tpe = RType.RFloat64
  }

}

