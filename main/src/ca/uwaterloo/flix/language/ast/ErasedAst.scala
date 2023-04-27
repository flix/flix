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
import ca.uwaterloo.flix.language.phase.jvm.{AnonClassInfo, ClosureInfo}

import java.lang.reflect.{Constructor, Field, Method}

object ErasedAst {

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  closures: Set[ClosureInfo],
                  anonClasses: Set[AnonClassInfo])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FormalParam], exp: Expr, tpe: MonoType, loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, Case], tpeDeprecated: MonoType, loc: SourceLocation)

  sealed trait Expr {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOperator, exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: MonoType, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, exp: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: MonoType, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class App(op: IntrinsicOp, exps: List[Expr], tpe: MonoType, loc: SourceLocation) extends Expr

  }

  sealed trait IntrinsicOp

  object IntrinsicOp {

    case object Region extends IntrinsicOp

    case object RecordEmpty extends IntrinsicOp

    case class GetStaticField(field: Field) extends IntrinsicOp

    case class HoleError(sym: Symbol.HoleSym) extends IntrinsicOp

    case object MatchError extends IntrinsicOp

    case class Unary(sop: SemanticOperator) extends IntrinsicOp

    case class Is(sym: Symbol.CaseSym) extends IntrinsicOp

    case class Tag(sym: Symbol.CaseSym) extends IntrinsicOp

    case class Untag(sym: Symbol.CaseSym) extends IntrinsicOp

    case class InstanceOf(clazz: Class[_]) extends IntrinsicOp

    case object Cast extends IntrinsicOp

    case class Index(idx: Int) extends IntrinsicOp

    case class RecordSelect(field: Name.Field) extends IntrinsicOp

    case class RecordRestrict(field: Name.Field) extends IntrinsicOp

    case object Ref extends IntrinsicOp

    case object Deref extends IntrinsicOp

    case object ArrayLength extends IntrinsicOp

    case object Lazy extends IntrinsicOp

    case object Force extends IntrinsicOp

    case class GetField(field: Field) extends IntrinsicOp

    case class PutStaticField(field: Field) extends IntrinsicOp

    case object BoxBool extends IntrinsicOp

    case object BoxInt8 extends IntrinsicOp

    case object BoxInt16 extends IntrinsicOp

    case object BoxInt32 extends IntrinsicOp

    case object BoxInt64 extends IntrinsicOp

    case object BoxChar extends IntrinsicOp

    case object BoxFloat32 extends IntrinsicOp

    case object BoxFloat64 extends IntrinsicOp

    case object UnboxBool extends IntrinsicOp

    case object UnboxInt8 extends IntrinsicOp

    case object UnboxInt16 extends IntrinsicOp

    case object UnboxInt32 extends IntrinsicOp

    case object UnboxInt64 extends IntrinsicOp

    case object UnboxChar extends IntrinsicOp

    case object UnboxFloat32 extends IntrinsicOp

    case object UnboxFloat64 extends IntrinsicOp

    case class Closure(sym: Symbol.DefnSym) extends IntrinsicOp

    case class ApplyDef(sym: Symbol.DefnSym) extends IntrinsicOp

    case class ApplyDefTail(sym: Symbol.DefnSym) extends IntrinsicOp

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FormalParam]) extends IntrinsicOp

    case object Tuple extends IntrinsicOp

    case object ArrayLit extends IntrinsicOp

    case class InvokeConstructor(constructor: Constructor[_]) extends IntrinsicOp

    case class InvokeStaticMethod(method: Method) extends IntrinsicOp

    case class RecordExtend(field: Name.Field) extends IntrinsicOp

    case object Assign extends IntrinsicOp

    case object ArrayNew extends IntrinsicOp

    case object ArrayLoad extends IntrinsicOp

    case object Spawn extends IntrinsicOp

    case object ScopeExit extends IntrinsicOp

    case class PutField(field: Field) extends IntrinsicOp

    case object ArrayStore extends IntrinsicOp

    case object ApplyClo extends IntrinsicOp

    case object ApplyCloTail extends IntrinsicOp

    case class InvokeMethod(method: Method) extends IntrinsicOp

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], clo: Expr, retTpe: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)
}
