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
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoType, MonoTypedAst, SemanticOperator, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.ast.MonoTypedAst.{CatchRule, Def, Expr, Root, Stmt}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Verify the AST before bytecode generation.
  */
object Verifier {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Verifier") {
    root.defs.values.foreach(d => visitDef(d)(root))
    root
  }

  private def visitDef(decl: Def)(implicit root: Root): Unit = {
    val env = decl.formals.foldLeft(Map.empty[Symbol.VarSym, MonoType]) {
      case (macc, fparam) => macc + (fparam.sym -> fparam.tpe)
    }
    try {
      visitStmt(decl.stmt)(root, env, Map.empty)
    } catch {
      case UnexpectedType(expected, found, loc) =>
        println(s"Unexpected type near ${loc.format}")
        println()
        println(s"  expected = $expected")
        println(s"  found    = $found")
        println()

      case MismatchedTypes(tpe1, tpe2, loc) =>
        println(s"Mismatched types near ${loc.format}")
        println()
        println(s"  tpe1 = $tpe1")
        println(s"  tpe2 = $tpe2")
        println()
    }
  }


  private def visitExpr(expr: MonoTypedAst.Expr)(implicit root: Root, env: Map[Symbol.VarSym, MonoType], lenv: Map[Symbol.LabelSym, MonoType]): MonoType = expr match {

    case Expr.Cst(cst, tpe, loc) => cst match {
      case Constant.Unit => check(expected = MonoType.Unit)(actual = tpe, loc)
      case Constant.Null => tpe
      case Constant.Bool(_) => check(expected = MonoType.Bool)(actual = tpe, loc)
      case Constant.Char(_) => check(expected = MonoType.Char)(actual = tpe, loc)
      case Constant.Float32(_) => check(expected = MonoType.Float32)(actual = tpe, loc)
      case Constant.Float64(_) => check(expected = MonoType.Float64)(actual = tpe, loc)
      case Constant.BigDecimal(_) => check(expected = MonoType.BigDecimal)(actual = tpe, loc)
      case Constant.Int8(_) => check(expected = MonoType.Int8)(actual = tpe, loc)
      case Constant.Int16(_) => check(expected = MonoType.Int16)(actual = tpe, loc)
      case Constant.Int32(_) => check(expected = MonoType.Int32)(actual = tpe, loc)
      case Constant.Int64(_) => check(expected = MonoType.Int64)(actual = tpe, loc)
      case Constant.BigInt(_) => check(expected = MonoType.BigInt)(actual = tpe, loc)
      case Constant.Str(_) => check(expected = MonoType.Str)(actual = tpe, loc)
      case Constant.Regex(_) => check(expected = MonoType.Regex)(actual = tpe, loc)
    }

    case Expr.Var(sym, tpe1, loc) => env.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown variable sym: '$sym'", loc)
      case Some(tpe2) =>
        checkEq(tpe1, tpe2, loc)
    }

    case Expr.ApplyAtomic(op, exps, tpe, loc) =>
      val ts = exps.map(visitExpr)

      op match {
        case AtomicOp.Unary(sop) => sop match {
          case op: SemanticOperator.BoolOp =>
            val List(t) = ts
            check(expected = MonoType.Bool)(actual = t, loc)
          case _ => tpe // TODO: VERIFIER: Add rest
        }
        case AtomicOp.Binary(sop) => sop match {
          case SemanticOperator.Int32Op.Add =>
            val List(t1, t2) = ts
            check(expected = MonoType.Int32)(t1, loc)
            check(expected = MonoType.Int32)(t2, loc)

          case _ => tpe // TODO: VERIFIER: Add rest

        }
        case AtomicOp.Tag(sym) =>
          root.enums.get(sym.enumSym) match {
            case None => throw InternalCompilerException(s"Unknown enum sym: '$sym'", loc)
            case Some(e) => e.cases.get(sym) match {
              case None => throw InternalCompilerException(s"Unknown enum case sym: '$sym' of '${e.sym}'", loc)
              case Some(caze) =>
                val List(t) = ts
                check(expected = caze.tpeDeprecated)(t, loc)
                check(expected = e.tpeDeprecated)(tpe, loc)
                tpe
            }
          }
        case AtomicOp.Untag(sym) =>
          root.enums.get(sym.enumSym) match {
            case None => throw InternalCompilerException(s"Unknown enum sym: '$sym'", loc)
            case Some(e) => e.cases.get(sym) match {
              case None => throw InternalCompilerException(s"Unknown enum case sym: '$sym' of '${e.sym}'", loc)
              case Some(caze) =>
                val List(t) = ts
                check(expected = e.tpeDeprecated)(t, loc)
                check(expected = caze.tpeDeprecated)(tpe, loc)
                tpe
            }
          }
        case _ => tpe // TODO: VERIFIER: Add rest
      }

    case Expr.ApplyClo(exp, exps, ct, tpe, loc) =>
      val lamType1 = visitExpr(exp)
      val lamType2 = MonoType.Arrow(exps.map(visitExpr), tpe)
      checkEq(lamType1, lamType2, loc)
      tpe

    case Expr.ApplyDef(sym, exps, ct, tpe, loc) =>
      val declared = root.defs(sym).tpe
      val actual = MonoType.Arrow(exps.map(visitExpr), tpe)
      check(expected = declared)(actual = actual, loc)
      tpe

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val declared = root.defs(sym).tpe
      val actual = MonoType.Arrow(actuals.map(visitExpr), tpe)
      check(expected = declared)(actual = actual, loc)
      tpe

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val condType = visitExpr(exp1)
      val thenType = visitExpr(exp2)
      val elseType = visitExpr(exp3)
      check(expected = MonoType.Bool)(actual = condType, loc)
      checkEq(tpe, thenType, loc)
      checkEq(tpe, elseType, loc)

    case Expr.Branch(exp, branches, tpe, loc) =>
      val lenv1 = branches.foldLeft(lenv) {
        case (acc, (label, _)) => acc + (label -> tpe)
      }
      branches.foreach {
        case (label, body) =>
          checkEq(tpe, visitExpr(body)(root, env, lenv1), loc)
      }
      tpe

    case Expr.JumpTo(sym, tpe1, loc) => lenv.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown label sym: '$sym'.", loc)
      case Some(tpe2) => checkEq(tpe1, tpe2, loc)
    }

    case Expr.Let(sym, exp1, exp2, tpe, loc) =>
      val letBoundType = visitExpr(exp1)
      val bodyType = visitExpr(exp2)(root, env + (sym -> letBoundType), lenv)
      checkEq(bodyType, tpe, loc)

    case Expr.LetRec(varSym, _, defSym, exp1, exp2, tpe, loc) =>
      val env1 = env + (varSym -> exp1.tpe)
      val letBoundType = visitExpr(exp1)(root, env1, lenv)
      val bodyType = visitExpr(exp2)(root, env1, lenv)
      checkEq(bodyType, tpe, loc)

    case Expr.Scope(sym, exp, tpe, loc) =>
      checkEq(tpe, visitExpr(exp)(root, env + (sym -> MonoType.Region), lenv), loc)

    case Expr.TryCatch(exp, rules, tpe, loc) =>
      for (CatchRule(sym, clazz, exp) <- rules) {
        checkEq(tpe, visitExpr(exp)(root, env + (sym -> MonoType.Native(clazz)), lenv), loc)
      }
      val t = visitExpr(exp)
      checkEq(tpe, t, loc)

    case Expr.NewObject(name, clazz, tpe, methods, loc) =>
      // TODO: VERIFIER: Add support for NewObject.
      tpe

    case Expr.Spawn(exp1, exp2, tpe1, loc) =>
      visitExpr(exp1)
      val tpe2 = visitExpr(exp2)
      checkEq(tpe1, tpe2, loc)
  }

  private def visitStmt(stmt: MonoTypedAst.Stmt)(implicit root: Root, env: Map[Symbol.VarSym, MonoType], lenv: Map[Symbol.LabelSym, MonoType]): MonoType = stmt match {
    case Stmt.Ret(expr, tpe, loc) =>
      visitExpr(expr)
  }

  /**
    * Asserts that the the given type `expected` is equal to the `actual` type.
    */
  private def check(expected: MonoType)(actual: MonoType, loc: SourceLocation): MonoType = {
    if (expected == actual)
      expected
    else
      throw UnexpectedType(expected, actual, loc)
  }

  /**
    * Asserts that the two given types `tpe1` and `tpe2` are the same.
    */
  private def checkEq(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation): MonoType = {
    if (tpe1 == tpe2)
      tpe1
    else
      throw MismatchedTypes(tpe1, tpe2, loc)
  }

  /**
    * An exception raised because the `expected` type does not match the `found` type.
    */
  private case class UnexpectedType(expected: MonoType, found: MonoType, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised because `tpe1` is not equal to `tpe2`.
    */
  private case class MismatchedTypes(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation) extends RuntimeException

}
