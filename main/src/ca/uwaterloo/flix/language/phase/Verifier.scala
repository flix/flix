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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.{MonoType, MonoTypedAst, SourceLocation}
import ca.uwaterloo.flix.language.ast.MonoTypedAst.{Def, Expr, Stmt}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * We use this phase to catch miscompilation before we do bytecode generation.
  */
object Verifier {

  def run(root: MonoTypedAst.Root)(implicit flix: Flix): MonoTypedAst.Root = flix.phase("Verifier") {
    //ParOps.parMap(root.defs.values)(visitDef)
    root.defs.values.foreach(visitDef)
    root
  }

  private def visitDef(def0: Def): Unit = {
    visitStmt(def0.stmt)(Map.empty) // TODO: Formalparams.
  }

  private def visitExpr(expr: MonoTypedAst.Expr)(implicit env: Map[Symbol.VarSym, MonoType]): MonoType = expr match {
    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Unit => expect(expected = MonoType.Unit, actual = tpe, loc)
      case Constant.Null => tpe
      case Constant.Bool(_) => expect(expected = MonoType.Bool, actual = tpe, loc)
      case Constant.Char(_) => expect(expected = MonoType.Char, actual = tpe, loc)
      case Constant.Float32(_) => expect(expected = MonoType.Float32, actual = tpe, loc)
      case Constant.Float64(_) => expect(expected = MonoType.Float64, actual = tpe, loc)
      case Constant.BigDecimal(_) => expect(expected = MonoType.BigDecimal, actual = tpe, loc)
      case Constant.Int8(_) => expect(expected = MonoType.Int8, actual = tpe, loc)
      case Constant.Int16(_) => expect(expected = MonoType.Int16, actual = tpe, loc)
      case Constant.Int32(_) => expect(expected = MonoType.Int32, actual = tpe, loc)
      case Constant.Int64(_) => expect(expected = MonoType.Int64, actual = tpe, loc)
      case Constant.BigInt(_) => expect(expected = MonoType.BigInt, actual = tpe, loc)
      case Constant.Str(_) => expect(expected = MonoType.Str, actual = tpe, loc)
      case Constant.Regex(_) => expect(expected = MonoType.Regex, actual = tpe, loc)
    }

    case Expr.Var(_, tpe, _) => tpe

    case Expr.ApplyAtomic(op, exps, tpe, loc) => tpe // TODO

    case Expr.ApplyClo(exp, args, ct, tpe, loc) => tpe // TODO

    case Expr.ApplyDef(sym, args, ct, tpe, loc) => tpe // TODO

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, loc) => tpe // TODO

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      // TODO: Need to recurse
      expect(expected = MonoType.Bool, actual = visitExpr(exp1), loc)
      expect(expected = tpe, actual = visitExpr(exp2), loc)
      expect(expected = tpe, actual = visitExpr(exp3), loc)

    case Expr.Branch(exp, branches, tpe, loc) => tpe // TODO

    case Expr.JumpTo(sym, tpe, loc) => tpe // TODO

    case Expr.Let(sym, exp1, exp2, tpe, loc) => tpe // TODO

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) => tpe // TODO

    case Expr.Scope(sym, exp, tpe, loc) => tpe // TODO

    case Expr.TryCatch(exp, rules, tpe, loc) => tpe // TODO

    case Expr.NewObject(name, clazz, tpe, methods, loc) => tpe // TODO

    case Expr.Spawn(exp1, exp2, tpe, loc) => tpe // TODO
  }

  private def visitStmt(stmt: MonoTypedAst.Stmt)(implicit env: Map[Symbol.VarSym, MonoType]): MonoType = stmt match {
    case Stmt.Ret(expr, tpe, loc) =>
      visitExpr(expr)
  }

  private def expect(expected: MonoType, actual: MonoType, loc: SourceLocation): MonoType = {
    if (expected == actual)
      actual
    else
      throw InternalCompilerException(s"Expected: '$expected', but got: '$actual'.", loc)
  }

  private def expectEq(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation): MonoType = {
    if (tpe1 == tpe2)
      tpe1
    else
      throw InternalCompilerException(s"Expected equal types: $tpe1 and $tpe2", loc)
  }

}

