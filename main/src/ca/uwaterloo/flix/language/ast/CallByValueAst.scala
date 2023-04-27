/*
 * Copyright 2020 Magnus Madsen
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

import java.lang.reflect.{Constructor, Field, Method}

object CallByValueAst {

  case class Root(defs: Map[Symbol.DefnSym, CallByValueAst.Def],
                  enums: Map[Symbol.EnumSym, CallByValueAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, fparams: List[CallByValueAst.FormalParam], stmt: CallByValueAst.Stmt, tpe: Type, loc: SourceLocation)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, CallByValueAst.Case], tpeDeprecated: Type, loc: SourceLocation)

  sealed trait Expr {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[Expr], tpe: Type, loc: SourceLocation) extends Expr {
      def purity: Purity = Pure
    }

    case class TryCatch(exp: Expr, /* require body to be pure */ rules: List[CatchRule], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, purity: Purity, /* no control effects */ methods: List[JvmMethod], loc: SourceLocation) extends Expr

    // TODO: Minus all the applies stuff in Intrinsic.
    case class ApplyPure(op: AtomicOp, exps: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Expr


    // Introduce run for force/lazy

    // TODO: Enforce spawn e, e must be control pure. Put spawn here. (Wrap into run?)

  }

  sealed trait Stmt {
    def tpe: Type

    def purity: Purity

    def loc: SourceLocation
  }

  object Stmt {

    case class Ret(exp: Expr, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class IfThenElse(exp: Expr, stmt1: Stmt, stmt2: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class Branch(stmt: Stmt, branches: Map[Symbol.LabelSym, Stmt], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class LetVal(sym: Symbol.VarSym, stmt1: Stmt, stmt2: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class LetRecVal(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp: Expr, stmt: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class Scope(sym: Symbol.VarSym, stmt: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyClo(exp: Expr, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyDef(sym: Symbol.DefnSym, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyCloTail(exp: Expr, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[CallByValueAst.FormalParam], actuals: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    //    case class Do(sym: Symbol.OpSym, exps: List[Expr], tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

    //    case class Handle(sym: Symbol.OpSym, stmt: Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Stmt

  }

  sealed trait AtomicOp

  object AtomicOp {

    case class Closure(sym: Symbol.DefnSym) extends AtomicOp

    case class Unary(sop: SemanticOperator) extends AtomicOp

    case class Binary(sop: SemanticOperator) extends AtomicOp

    case object Region extends AtomicOp

    case object ScopeExit extends AtomicOp

    case class Is(sym: Symbol.CaseSym) extends AtomicOp

    case class Tag(sym: Symbol.CaseSym) extends AtomicOp

    case class Untag(sym: Symbol.CaseSym) extends AtomicOp

    case class Index(idx: Int) extends AtomicOp

    case object Tuple extends AtomicOp

    case object RecordEmpty extends AtomicOp

    case class RecordSelect(field: Name.Field) extends AtomicOp

    case class RecordExtend(field: Name.Field) extends AtomicOp

    case class RecordRestrict(field: Name.Field) extends AtomicOp

    case object ArrayLit extends AtomicOp

    case object ArrayNew extends AtomicOp

    case object ArrayLoad extends AtomicOp

    case object ArrayStore extends AtomicOp

    case object ArrayLength extends AtomicOp

    case object Ref extends AtomicOp

    case object Deref extends AtomicOp

    case object Assign extends AtomicOp

    case class InstanceOf(clazz: Class[_]) extends AtomicOp

    case object Cast extends AtomicOp

    case class InvokeConstructor(constructor: Constructor[_]) extends AtomicOp

    case class InvokeMethod(method: Method) extends AtomicOp

    case class InvokeStaticMethod(method: Method) extends AtomicOp

    case class GetField(field: Field) extends AtomicOp

    case class PutField(field: Field) extends AtomicOp

    case class GetStaticField(field: Field) extends AtomicOp

    case class PutStaticField(field: Field) extends AtomicOp

    case object Spawn extends AtomicOp

    case object Lazy extends AtomicOp

    case object Force extends AtomicOp

    case object BoxBool extends AtomicOp

    case object BoxInt8 extends AtomicOp

    case object BoxInt16 extends AtomicOp

    case object BoxInt32 extends AtomicOp

    case object BoxInt64 extends AtomicOp

    case object BoxChar extends AtomicOp

    case object BoxFloat32 extends AtomicOp

    case object BoxFloat64 extends AtomicOp

    case object UnboxBool extends AtomicOp

    case object UnboxInt8 extends AtomicOp

    case object UnboxInt16 extends AtomicOp

    case object UnboxInt32 extends AtomicOp

    case object UnboxInt64 extends AtomicOp

    case object UnboxChar extends AtomicOp

    case object UnboxFloat32 extends AtomicOp

    case object UnboxFloat64 extends AtomicOp

    case class HoleError(sym: Symbol.HoleSym) extends AtomicOp

    case object MatchError extends AtomicOp

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[CallByValueAst.FormalParam], clo: CallByValueAst.Expr, retTpe: Type, purity: Purity, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: CallByValueAst.Expr)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

}

