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

    case class Intrinsic(op: IntrinsicOperator, exps: List[Expr], tpe: MonoType, loc: SourceLocation) extends Expr

    case class Intrinsic1(op: IntrinsicOperator1, exp: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Intrinsic2(op: IntrinsicOperator2, exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Intrinsic3(op: IntrinsicOperator3, exp1: Expr, exp2: Expr, exp3: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class IntrinsicN(op: IntrinsicOperatorN, exps: List[Expr], tpe: MonoType, loc: SourceLocation) extends Expr

    case class Intrinsic1N(op: IntrinsicOperator1N, exp: Expr, exps: List[Expr], tpe: MonoType, loc: SourceLocation) extends Expr

  }

  sealed trait Stmt

  object Stmt {

    case class Ret(exp: Expr, tpe: MonoType, loc: SourceLocation) extends Stmt

    case class Let(sym: Symbol.VarSym, exp1: Stmt, exp2: Stmt, tpe: MonoType, loc: SourceLocation) extends Stmt

    case class Do(sym: Symbol.OpSym, exps: List[Expr], tpe: MonoType, loc: SourceLocation) extends Stmt

    case class Handle(sym: Symbol.OpSym, stmt: Stmt, loc: SourceLocation) extends Stmt

  }

  sealed trait IntrinsicOperator

  case object IntrinsicOperator {

    // Intrinsic0

    case class Cst(cst: Ast.Constant) extends IntrinsicOperator

    case object Region extends IntrinsicOperator

    case object RecordEmpty extends IntrinsicOperator

    case class GetStaticField(field: Field) extends IntrinsicOperator

    case class HoleError(sym: Symbol.HoleSym) extends IntrinsicOperator

    case object MatchError extends IntrinsicOperator

    // Intrinsic1

    case class Unary(sop: SemanticOperator) extends IntrinsicOperator

    case class Is(sym: Symbol.CaseSym) extends IntrinsicOperator

    // Intrinsic2

    // case class RecordExtend(field: Name.Field) extends IntrinsicOperator

    // case object Assign extends IntrinsicOperator

    // case object ArrayNew extends IntrinsicOperator

    // case object ArrayLoad extends IntrinsicOperator

    case object Spawn extends IntrinsicOperator

    // case object ScopeExit extends IntrinsicOperator

    // case class PutField(field: Field) extends IntrinsicOperator

    // Intrinsic3

    // IntrinsicN

    // Intrinsic1N

  }

  sealed trait IntrinsicOperator1

  object IntrinsicOperator1 {

    case class Tag(sym: Symbol.CaseSym) extends IntrinsicOperator1

    case class Untag(sym: Symbol.CaseSym) extends IntrinsicOperator1

    case class InstanceOf(clazz: Class[_]) extends IntrinsicOperator1

    case object Cast extends IntrinsicOperator1

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

    case object ScopeExit extends IntrinsicOperator2

    case class PutField(field: Field) extends IntrinsicOperator2

  }

  sealed trait IntrinsicOperator3

  object IntrinsicOperator3 {

    case object ArrayStore extends IntrinsicOperator3

  }

  sealed trait IntrinsicOperatorN

  object IntrinsicOperatorN {

    case class Closure(sym: Symbol.DefnSym) extends IntrinsicOperatorN

    case class ApplyDef(sym: Symbol.DefnSym) extends IntrinsicOperatorN

    case class ApplyDefTail(sym: Symbol.DefnSym) extends IntrinsicOperatorN

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FormalParam]) extends IntrinsicOperatorN

    case object Tuple extends IntrinsicOperatorN

    case object ArrayLit extends IntrinsicOperatorN

    case class InvokeConstructor(constructor: Constructor[_]) extends IntrinsicOperatorN

    case class InvokeStaticMethod(method: Method) extends IntrinsicOperatorN

  }

  sealed trait IntrinsicOperator1N

  object IntrinsicOperator1N {

    case object ApplyClo extends IntrinsicOperator1N

    case object ApplyCloTail extends IntrinsicOperator1N

    case class InvokeMethod(method: Method) extends IntrinsicOperator1N

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], clo: Expr, retTpe: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)

}
