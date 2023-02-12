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
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, ErasedAst.Case], tpeDeprecated: MonoType, loc: SourceLocation)

  sealed trait Expression {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Expression {
    case class Cst(cst: Ast.Constant, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ApplyClo(exp: ErasedAst.Expression, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ApplyCloTail(exp: ErasedAst.Expression, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[ErasedAst.FormalParam], actuals: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class IfThenElse(exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, exp3: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Branch(exp: ErasedAst.Expression, branches: Map[Symbol.LabelSym, ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Let(sym: Symbol.VarSym, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Region(tpe: MonoType, loc: SourceLocation) extends Expression

    case class Scope(sym: Symbol.VarSym, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class ScopeExit(exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Is(sym: Symbol.CaseSym, exp: ErasedAst.Expression, loc: SourceLocation) extends Expression {
      final val tpe: MonoType = MonoType.Bool
    }

    case class Tuple(elms: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ArrayLit(elms: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class ArraySlice(base: ErasedAst.Expression, beginIndex: ErasedAst.Expression, endIndex: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Cast(exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class TryCatch(exp: ErasedAst.Expression, rules: List[ErasedAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class InvokeMethod(method: Method, exp: ErasedAst.Expression, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class InvokeStaticMethod(method: Method, args: List[ErasedAst.Expression], tpe: MonoType, loc: SourceLocation) extends Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, methods: List[ErasedAst.JvmMethod], loc: SourceLocation) extends Expression

    case class Intrinsic0(op: ErasedAst.IntrinsicOperator0, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Intrinsic1(op: ErasedAst.IntrinsicOperator1, exp: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Intrinsic2(op: ErasedAst.IntrinsicOperator2, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

    case class Intrinsic3(op: ErasedAst.IntrinsicOperator3, exp1: ErasedAst.Expression, exp2: ErasedAst.Expression, exp3: ErasedAst.Expression, tpe: MonoType, loc: SourceLocation) extends Expression

  }

  sealed trait IntrinsicOperator0

  object IntrinsicOperator0 {

    case object RecordEmpty extends IntrinsicOperator0

    case class GetStaticField(field: Field) extends IntrinsicOperator0

    case class HoleError(sym: Symbol.HoleSym) extends IntrinsicOperator0

    case object MatchError extends IntrinsicOperator0

  }

  sealed trait IntrinsicOperator1

  object IntrinsicOperator1 {

    case class Tag(sym: Symbol.CaseSym) extends IntrinsicOperator1

    case class Untag(sym: Symbol.CaseSym) extends IntrinsicOperator1

    case class Index(idx: Int) extends IntrinsicOperator1

    case class RecordSelect(field: Name.Field) extends IntrinsicOperator1

    case class RecordRestrict(field: Name.Field) extends IntrinsicOperator1

    case object Ref extends IntrinsicOperator1

    case object Deref extends IntrinsicOperator1

    case object ArrayLength extends IntrinsicOperator1

    case object Lazy extends IntrinsicOperator1

    case object Force extends IntrinsicOperator1

    case class GetField(field: Field) extends IntrinsicOperator1

    case class PutStaticField(field: Field) extends IntrinsicOperator1

    case object BoxBool extends IntrinsicOperator1

    case object BoxInt8 extends IntrinsicOperator1

    case object BoxInt16 extends IntrinsicOperator1

    case object BoxInt32 extends IntrinsicOperator1

    case object BoxInt64 extends IntrinsicOperator1

    case object BoxChar extends IntrinsicOperator1

    case object BoxFloat32 extends IntrinsicOperator1

    case object BoxFloat64 extends IntrinsicOperator1

    case object UnboxBool extends IntrinsicOperator1

    case object UnboxInt8 extends IntrinsicOperator1

    case object UnboxInt16 extends IntrinsicOperator1

    case object UnboxInt32 extends IntrinsicOperator1

    case object UnboxInt64 extends IntrinsicOperator1

    case object UnboxChar extends IntrinsicOperator1

    case object UnboxFloat32 extends IntrinsicOperator1

    case object UnboxFloat64 extends IntrinsicOperator1

  }

  sealed trait IntrinsicOperator2

  object IntrinsicOperator2 {

    case class RecordExtend(field: Name.Field) extends IntrinsicOperator2

    case object Assign extends IntrinsicOperator2

    case object ArrayNew extends IntrinsicOperator2

    case object ArrayLoad extends IntrinsicOperator2

    case object Spawn extends IntrinsicOperator2

    case class PutField(field: Field) extends IntrinsicOperator2

  }

  sealed trait IntrinsicOperator3

  object IntrinsicOperator3 {

    case object ArrayStore extends IntrinsicOperator3

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[ErasedAst.FormalParam], clo: ErasedAst.Expression, retTpe: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ErasedAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)
}
