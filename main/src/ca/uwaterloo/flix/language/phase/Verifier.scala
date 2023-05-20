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
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Verify the AST before bytecode generation.
  */
object Verifier {

  def run(root: MonoTypedAst.Root)(implicit flix: Flix): MonoTypedAst.Root = flix.phase("Verifier") {
    root.defs.values.foreach(visitDef)
    root
  }

  private def visitDef(decl: Def): Unit = {
    val env = decl.formals.foldLeft(Map.empty[Symbol.VarSym, MonoType]) {
      case (macc, fparam) => macc + (fparam.sym -> fparam.tpe)
    }
    try {
      visitStmt(decl.stmt)(env, Map.empty)
    } catch {
      case MismatchedTypes(tpe1, tpe2, loc) =>
        println(s"Mismatched types near ${loc.format}")
        println()
        println(s"  tpe1 = $tpe1")
        println(s"  tpe2 = $tpe2")
        println()
    }
  }

  private def visitExpr(expr: MonoTypedAst.Expr)(implicit env: Map[Symbol.VarSym, MonoType], lenv: Map[Symbol.LabelSym, MonoType]): MonoType = expr match {

    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Unit => check(expect = MonoType.Unit, actual = tpe, loc)
      case Constant.Null => tpe
      case Constant.Bool(_) => check(expect = MonoType.Bool, actual = tpe, loc)
      case Constant.Char(_) => check(expect = MonoType.Char, actual = tpe, loc)
      case Constant.Float32(_) => check(expect = MonoType.Float32, actual = tpe, loc)
      case Constant.Float64(_) => check(expect = MonoType.Float64, actual = tpe, loc)
      case Constant.BigDecimal(_) => check(expect = MonoType.BigDecimal, actual = tpe, loc)
      case Constant.Int8(_) => check(expect = MonoType.Int8, actual = tpe, loc)
      case Constant.Int16(_) => check(expect = MonoType.Int16, actual = tpe, loc)
      case Constant.Int32(_) => check(expect = MonoType.Int32, actual = tpe, loc)
      case Constant.Int64(_) => check(expect = MonoType.Int64, actual = tpe, loc)
      case Constant.BigInt(_) => check(expect = MonoType.BigInt, actual = tpe, loc)
      case Constant.Str(_) => check(expect = MonoType.Str, actual = tpe, loc)
      case Constant.Regex(_) => check(expect = MonoType.Regex, actual = tpe, loc)
    }

    case Expr.Var(sym, tpe, loc) => env.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown variable sym: '$sym", loc)
      case Some(declaredType) =>
        check(expect = declaredType, actual = tpe, loc)
    }

    case Expr.ApplyAtomic(op, exps, tpe, loc) =>
      // TODO
      val types = exps.map(visitExpr)

      tpe

    case Expr.ApplyClo(exp, exps, ct, tpe, loc) =>
      val t = visitExpr(exp)
      val ts = exps.map(visitExpr)

      tpe // TODO

    case Expr.ApplyDef(sym, exps, ct, tpe, loc) =>
      val ts = exps.map(visitExpr)

      tpe // TODO

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val ts = actuals.map(visitExpr)

      tpe // TODO

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val condType = visitExpr(exp1)
      val thenType = visitExpr(exp2)
      val elseType = visitExpr(exp3)
      check(expect = MonoType.Bool, actual = condType, loc)
      check(expect = tpe, actual = thenType, loc)
      check(expect = tpe, actual = elseType, loc)

    case Expr.Branch(exp, branches, tpe, loc) =>
      branches.foreach {
        case (label, body) => check(expect = tpe, actual = visitExpr(body), loc)
      }
      tpe

    case Expr.JumpTo(sym, tpe, loc) => lenv.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown label sym: '$sym'.", loc)
      case Some(declaredType) => check(expect = declaredType, actual = tpe, loc)
    }

    case Expr.Let(sym, exp1, exp2, tpe, loc) =>
      val letBoundType = visitExpr(exp1)
      val bodyType = visitExpr(exp2)(env + (sym -> letBoundType), lenv)
      check(expect = bodyType, actual = tpe, loc)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) => tpe // TODO

    case Expr.Scope(sym, exp, tpe, loc) => tpe // TODO

    case Expr.TryCatch(exp, rules, tpe, loc) => tpe // TODO

    case Expr.NewObject(name, clazz, tpe, methods, loc) => tpe // TODO

    case Expr.Spawn(exp1, exp2, tpe, loc) =>
      val tpe1 = visitExpr(exp1)
      val tpe2 = visitExpr(exp2)
      check(expect = tpe, actual = tpe2, loc)
  }

  private def visitStmt(stmt: MonoTypedAst.Stmt)(implicit env: Map[Symbol.VarSym, MonoType], lenv: Map[Symbol.LabelSym, MonoType]): MonoType = stmt match {
    case Stmt.Ret(expr, tpe, loc) =>
      visitExpr(expr)
  }

  private def check(expect: MonoType, actual: MonoType, loc: SourceLocation): MonoType = {
    if (expect == actual)
      actual
    else
      throw MismatchedTypes(expect, actual, loc)
  }

  private case class MismatchedTypes(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation) extends RuntimeException

}

