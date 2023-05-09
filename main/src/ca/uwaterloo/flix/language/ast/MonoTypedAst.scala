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

import ca.uwaterloo.flix.language.ast.Ast.Source

import java.lang.reflect.Method

object MonoTypedAst {

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FormalParam], stmt: Stmt, tpe: MonoType, loc: SourceLocation) {
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

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: MonoType, loc: SourceLocation) extends Expr

    case class ApplyClo(exp: Expr, args: List[Expr], ct: Ast.CallType, tpe: MonoType, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, args: List[Expr], ct: Ast.CallType, tpe: MonoType, loc: SourceLocation) extends Expr

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FormalParam], actuals: List[Expr], tpe: MonoType, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Branch(exp: Expr, branches: Map[Symbol.LabelSym, Expr], tpe: MonoType, loc: SourceLocation) extends Expr

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, exp: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: MonoType, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class Spawn(exp1: Expr, exp2: Expr, tpe: MonoType, loc: SourceLocation) extends Expr

  }

  sealed trait Stmt {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Stmt {

    case class Ret(expr: Expr, tpe: MonoType, loc: SourceLocation) extends Stmt

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], clo: Expr, retTpe: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)

}
