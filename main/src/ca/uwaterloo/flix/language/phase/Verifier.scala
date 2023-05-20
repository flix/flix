/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.{MonoType, MonoTypedAst, SourceLocation}
import ca.uwaterloo.flix.language.ast.MonoTypedAst.{Def, Expr, Stmt}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * We use this phase to catch miscompilation before we do bytecode generation.
  */
object Verifier {
  def run(root: MonoTypedAst.Root)(implicit flix: Flix): MonoTypedAst.Root = flix.phase("Verifier") {
    ParOps.parMap(root.defs.values)(visitDef)
    root
  }

  private def visitDef(def0: Def): Unit = {
    visitStmt(def0.stmt)
  }

  private def visitExpr(expr: MonoTypedAst.Expr): MonoType = expr match {
    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Unit => expect(expected = MonoType.Unit, actual = tpe, loc)
      case Constant.Null => ???
      case Constant.Bool(lit) => ???
      case Constant.Char(lit) => ???
      case Constant.Float32(lit) => ???
      case Constant.Float64(lit) => ???
      case Constant.BigDecimal(lit) => ???
      case Constant.Int8(lit) => ???
      case Constant.Int16(lit) => ???
      case Constant.Int32(lit) => ???
      case Constant.Int64(lit) => ???
      case Constant.BigInt(lit) => ???
      case Constant.Str(lit) => ???
      case Constant.Regex(lit) => ???
    }
    case Expr.Var(sym, tpe, loc) => tpe
    case Expr.ApplyAtomic(op, exps, tpe, loc) => tpe
    case Expr.ApplyClo(exp, args, ct, tpe, loc) => tpe
    case Expr.ApplyDef(sym, args, ct, tpe, loc) => tpe
    case Expr.ApplySelfTail(sym, formals, actuals, tpe, loc) => tpe
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, loc) => tpe
    case Expr.Branch(exp, branches, tpe, loc) => tpe
    case Expr.JumpTo(sym, tpe, loc) => tpe
    case Expr.Let(sym, exp1, exp2, tpe, loc) => tpe
    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) => tpe
    case Expr.Scope(sym, exp, tpe, loc) => tpe
    case Expr.TryCatch(exp, rules, tpe, loc) => tpe
    case Expr.NewObject(name, clazz, tpe, methods, loc) => tpe
    case Expr.Spawn(exp1, exp2, tpe, loc) => tpe
  }

  private def visitStmt(stmt: MonoTypedAst.Stmt): MonoType = stmt match {
    case Stmt.Ret(expr, tpe, loc) =>
      visitExpr(expr)
  }

  private def expect(expected: MonoType, actual: MonoType, loc: SourceLocation): MonoType = {
    if (expected == actual)
      actual
    else
      throw InternalCompilerException(s"Expected: '$expected', but got: '$actual'.", loc)
  }

}

