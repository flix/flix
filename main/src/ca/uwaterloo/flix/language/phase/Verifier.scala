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
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoType, Name, SemanticOp, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}
import scala.annotation.tailrec

/**
  * Verify the AST before bytecode generation.
  */
object Verifier {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Verifier") {
    if (flix.options.xnoverify) {
      root
    } else {
      ParOps.parMap(root.defs.values)(visitDef(_)(root))
      root
    }
  }

  private def visitDef(decl: Def)(implicit root: Root): Unit = {
    val env = (decl.cparams ++ decl.fparams).foldLeft(Map.empty[Symbol.VarSym, MonoType]) {
      case (macc, fparam) => macc + (fparam.sym -> fparam.tpe)
    }
    val ret = visitExpr(decl.expr)(root, env, Map.empty)
    checkEq(decl.tpe, ret, decl.loc)
  }


  private def visitExpr(expr: Expr)(implicit root: Root, env: Map[Symbol.VarSym, MonoType], lenv: Map[Symbol.LabelSym, MonoType]): MonoType = expr match {

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
      case Constant.Str(_) => check(expected = MonoType.String)(actual = tpe, loc)
      case Constant.Regex(_) => check(expected = MonoType.Regex)(actual = tpe, loc)
    }

    case Expr.Var(sym, tpe1, loc) => env.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown variable sym: '$sym'", sym.loc)
      case Some(tpe2) =>
        checkEq(tpe1, tpe2, loc)
    }

    case Expr.ApplyAtomic(op, exps, tpe, _, loc) =>
      val ts = exps.map(visitExpr)

      op match {
        case AtomicOp.Unary(sop) =>
          val List(t) = ts
          val opTpe = sop match {
            case SemanticOp.BoolOp.Not => MonoType.Bool
            case SemanticOp.Float32Op.Neg => MonoType.Float32
            case SemanticOp.Float64Op.Neg => MonoType.Float64
            case SemanticOp.Int8Op.Neg => MonoType.Int8
            case SemanticOp.Int8Op.Not => MonoType.Int8
            case SemanticOp.Int16Op.Neg => MonoType.Int16
            case SemanticOp.Int16Op.Not => MonoType.Int16
            case SemanticOp.Int32Op.Neg => MonoType.Int32
            case SemanticOp.Int32Op.Not => MonoType.Int32
            case SemanticOp.Int64Op.Neg => MonoType.Int64
            case SemanticOp.Int64Op.Not => MonoType.Int64
          }
          check(expected = opTpe)(actual = t, loc)
          check(expected = tpe)(actual = opTpe, loc)

        case AtomicOp.Binary(sop) =>
          val List(t1, t2) = ts
          val (argTpe1, argTpe2, resTpe) = sop match {
            case SemanticOp.BoolOp.And => (MonoType.Bool, MonoType.Bool, MonoType.Bool)
            case SemanticOp.BoolOp.Neq => (MonoType.Bool, MonoType.Bool, MonoType.Bool)
            case SemanticOp.BoolOp.Eq => (MonoType.Bool, MonoType.Bool, MonoType.Bool)
            case SemanticOp.BoolOp.Or => (MonoType.Bool, MonoType.Bool, MonoType.Bool)

            case SemanticOp.CharOp.Eq => (MonoType.Char, MonoType.Char, MonoType.Bool)
            case SemanticOp.CharOp.Neq => (MonoType.Char, MonoType.Char, MonoType.Bool)
            case SemanticOp.CharOp.Ge => (MonoType.Char, MonoType.Char, MonoType.Bool)
            case SemanticOp.CharOp.Gt => (MonoType.Char, MonoType.Char, MonoType.Bool)
            case SemanticOp.CharOp.Le => (MonoType.Char, MonoType.Char, MonoType.Bool)
            case SemanticOp.CharOp.Lt => (MonoType.Char, MonoType.Char, MonoType.Bool)

            case SemanticOp.Float32Op.Eq => (MonoType.Float32, MonoType.Float32, MonoType.Bool)
            case SemanticOp.Float32Op.Neq => (MonoType.Float32, MonoType.Float32, MonoType.Bool)
            case SemanticOp.Float32Op.Ge => (MonoType.Float32, MonoType.Float32, MonoType.Bool)
            case SemanticOp.Float32Op.Gt => (MonoType.Float32, MonoType.Float32, MonoType.Bool)
            case SemanticOp.Float32Op.Le => (MonoType.Float32, MonoType.Float32, MonoType.Bool)
            case SemanticOp.Float32Op.Lt => (MonoType.Float32, MonoType.Float32, MonoType.Bool)
            case SemanticOp.Float32Op.Add => (MonoType.Float32, MonoType.Float32, MonoType.Float32)
            case SemanticOp.Float32Op.Div => (MonoType.Float32, MonoType.Float32, MonoType.Float32)
            case SemanticOp.Float32Op.Exp => (MonoType.Float32, MonoType.Float32, MonoType.Float32)
            case SemanticOp.Float32Op.Mul => (MonoType.Float32, MonoType.Float32, MonoType.Float32)
            case SemanticOp.Float32Op.Sub => (MonoType.Float32, MonoType.Float32, MonoType.Float32)

            case SemanticOp.Float64Op.Eq => (MonoType.Float64, MonoType.Float64, MonoType.Bool)
            case SemanticOp.Float64Op.Neq => (MonoType.Float64, MonoType.Float64, MonoType.Bool)
            case SemanticOp.Float64Op.Ge => (MonoType.Float64, MonoType.Float64, MonoType.Bool)
            case SemanticOp.Float64Op.Gt => (MonoType.Float64, MonoType.Float64, MonoType.Bool)
            case SemanticOp.Float64Op.Le => (MonoType.Float64, MonoType.Float64, MonoType.Bool)
            case SemanticOp.Float64Op.Lt => (MonoType.Float64, MonoType.Float64, MonoType.Bool)
            case SemanticOp.Float64Op.Add => (MonoType.Float64, MonoType.Float64, MonoType.Float64)
            case SemanticOp.Float64Op.Div => (MonoType.Float64, MonoType.Float64, MonoType.Float64)
            case SemanticOp.Float64Op.Exp => (MonoType.Float64, MonoType.Float64, MonoType.Float64)
            case SemanticOp.Float64Op.Mul => (MonoType.Float64, MonoType.Float64, MonoType.Float64)
            case SemanticOp.Float64Op.Sub => (MonoType.Float64, MonoType.Float64, MonoType.Float64)

            case SemanticOp.Int8Op.Eq => (MonoType.Int8, MonoType.Int8, MonoType.Bool)
            case SemanticOp.Int8Op.Neq => (MonoType.Int8, MonoType.Int8, MonoType.Bool)
            case SemanticOp.Int8Op.Ge => (MonoType.Int8, MonoType.Int8, MonoType.Bool)
            case SemanticOp.Int8Op.Gt => (MonoType.Int8, MonoType.Int8, MonoType.Bool)
            case SemanticOp.Int8Op.Le => (MonoType.Int8, MonoType.Int8, MonoType.Bool)
            case SemanticOp.Int8Op.Lt => (MonoType.Int8, MonoType.Int8, MonoType.Bool)
            case SemanticOp.Int8Op.Add => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Div => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Exp => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Mul => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Sub => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Rem => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.And => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Or => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Xor => (MonoType.Int8, MonoType.Int8, MonoType.Int8)
            case SemanticOp.Int8Op.Shl => (MonoType.Int8, MonoType.Int32, MonoType.Int8)
            case SemanticOp.Int8Op.Shr => (MonoType.Int8, MonoType.Int32, MonoType.Int8)

            case SemanticOp.Int16Op.Eq => (MonoType.Int16, MonoType.Int16, MonoType.Bool)
            case SemanticOp.Int16Op.Neq => (MonoType.Int16, MonoType.Int16, MonoType.Bool)
            case SemanticOp.Int16Op.Ge => (MonoType.Int16, MonoType.Int16, MonoType.Bool)
            case SemanticOp.Int16Op.Gt => (MonoType.Int16, MonoType.Int16, MonoType.Bool)
            case SemanticOp.Int16Op.Le => (MonoType.Int16, MonoType.Int16, MonoType.Bool)
            case SemanticOp.Int16Op.Lt => (MonoType.Int16, MonoType.Int16, MonoType.Bool)
            case SemanticOp.Int16Op.Add => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Div => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Exp => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Mul => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Sub => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Rem => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.And => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Or => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Xor => (MonoType.Int16, MonoType.Int16, MonoType.Int16)
            case SemanticOp.Int16Op.Shl => (MonoType.Int16, MonoType.Int32, MonoType.Int16)
            case SemanticOp.Int16Op.Shr => (MonoType.Int16, MonoType.Int32, MonoType.Int16)

            case SemanticOp.Int32Op.Eq => (MonoType.Int32, MonoType.Int32, MonoType.Bool)
            case SemanticOp.Int32Op.Neq => (MonoType.Int32, MonoType.Int32, MonoType.Bool)
            case SemanticOp.Int32Op.Ge => (MonoType.Int32, MonoType.Int32, MonoType.Bool)
            case SemanticOp.Int32Op.Gt => (MonoType.Int32, MonoType.Int32, MonoType.Bool)
            case SemanticOp.Int32Op.Le => (MonoType.Int32, MonoType.Int32, MonoType.Bool)
            case SemanticOp.Int32Op.Lt => (MonoType.Int32, MonoType.Int32, MonoType.Bool)
            case SemanticOp.Int32Op.Add => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Div => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Exp => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Mul => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Sub => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Rem => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.And => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Or => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Xor => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Shl => (MonoType.Int32, MonoType.Int32, MonoType.Int32)
            case SemanticOp.Int32Op.Shr => (MonoType.Int32, MonoType.Int32, MonoType.Int32)

            case SemanticOp.Int64Op.Eq => (MonoType.Int64, MonoType.Int64, MonoType.Bool)
            case SemanticOp.Int64Op.Neq => (MonoType.Int64, MonoType.Int64, MonoType.Bool)
            case SemanticOp.Int64Op.Ge => (MonoType.Int64, MonoType.Int64, MonoType.Bool)
            case SemanticOp.Int64Op.Gt => (MonoType.Int64, MonoType.Int64, MonoType.Bool)
            case SemanticOp.Int64Op.Le => (MonoType.Int64, MonoType.Int64, MonoType.Bool)
            case SemanticOp.Int64Op.Lt => (MonoType.Int64, MonoType.Int64, MonoType.Bool)
            case SemanticOp.Int64Op.Add => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Div => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Exp => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Mul => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Sub => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Rem => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.And => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Or => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Xor => (MonoType.Int64, MonoType.Int64, MonoType.Int64)
            case SemanticOp.Int64Op.Shl => (MonoType.Int64, MonoType.Int32, MonoType.Int64)
            case SemanticOp.Int64Op.Shr => (MonoType.Int64, MonoType.Int32, MonoType.Int64)

            case SemanticOp.StringOp.Concat => (MonoType.String, MonoType.String, MonoType.String)
          }
          check(expected = argTpe1)(t1, loc)
          check(expected = argTpe2)(t2, loc)
          check(expected = tpe)(actual = resTpe, loc)

        case AtomicOp.Is(sym) =>
          val List(t1) = ts
          check(expected = MonoType.Enum(sym.enumSym))(actual = t1, loc)
          check(expected = MonoType.Bool)(actual = tpe, loc)

        case AtomicOp.Tag(sym) =>
          val List(t1) = ts
          check(expected = MonoType.Enum(sym.enumSym))(actual = tpe, loc)

        case AtomicOp.Untag(sym) =>
          val List(t1) = ts
          check(expected = MonoType.Enum(sym.enumSym))(actual = t1, loc)
          tpe

        case AtomicOp.ArrayLength =>
          val List(t1) = ts
          t1 match {
            case MonoType.Array(_) => check(expected = MonoType.Int32)(actual = tpe, loc)
            case _ => failMismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.ArrayNew =>
          val List(t1, t2) = ts
          val arrType = MonoType.Array(t1)
          checkEq(arrType, tpe, loc)
          check(expected = MonoType.Int32)(actual = t2, loc)
          tpe

        case AtomicOp.ArrayLit =>
          tpe match {
            case MonoType.Array(elmt) =>
              ts.foreach(t => checkEq(elmt, t, loc))
              tpe
            case _ => failMismatchedShape(tpe, "Array", loc)
          }

        case AtomicOp.ArrayLoad =>
          val List(t1, t2) = ts
          t1 match {
            case MonoType.Array(elmt) =>
              check(expected = MonoType.Int32)(actual = t2, loc)
              checkEq(elmt, tpe, loc)
            case _ => failMismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.ArrayStore =>
          val List(t1, t2, t3) = ts
          t1 match {
            case MonoType.Array(elmt) =>
              check(expected = MonoType.Int32)(actual = t2, loc)
              checkEq(elmt, t3, loc)
              check(expected = MonoType.Unit)(actual = tpe, loc)
            case _ => failMismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.Ref =>
          val List(t1) = ts
          val refType = MonoType.Ref(t1)
          checkEq(refType, tpe, loc)

        case AtomicOp.Deref =>
          val List(t1) = ts
          t1 match {
            case MonoType.Ref(elm) => checkEq(elm, tpe, loc)
            case _ => failMismatchedShape(t1, "Ref", loc)
          }

        case AtomicOp.Lazy =>
          val List(t1) = ts
          tpe match {
            case MonoType.Lazy(elmt) =>
              val fun = MonoType.Arrow(List(MonoType.Unit), elmt)
              checkEq(t1, fun, loc)
              tpe
            case _ => failMismatchedShape(tpe, "Lazy", loc)
          }

        case AtomicOp.Force =>
          val List(t1) = ts
          t1 match {
            case MonoType.Lazy(elm) => checkEq(elm, tpe, loc)
            case _ => failMismatchedShape(t1, "Lazy", loc)
          }

        case AtomicOp.Tuple =>
          val tup = MonoType.Tuple(ts)
          checkEq(tup, tpe, loc)

        case AtomicOp.Index(idx: Int) =>
          val List(t1) = ts
          t1 match {
            case MonoType.Tuple(elms) => checkEq(elms(idx), tpe, loc)
            case _ => failMismatchedShape(t1, "Tuple", loc)
          }

        case AtomicOp.Assign =>
          val List(t1, t2) = ts
          t1 match {
            case MonoType.Ref(elm) =>
              checkEq(t2, elm, loc)
              check(expected = MonoType.Unit)(actual = tpe, loc)
            case _ => failMismatchedShape(t1, "Ref", loc)
          }

        // Match- and Hole-errors match with any type
        case AtomicOp.HoleError(_) =>
          tpe

        case AtomicOp.MatchError =>
          tpe

        case AtomicOp.RecordEmpty =>
          check(expected = MonoType.RecordEmpty)(actual = tpe, loc)

        case AtomicOp.RecordExtend(label) =>
          val List(t1, t2) = ts
          removeFromRecordType(tpe, label.name, loc) match {
            case (rec, Some(valtype)) =>
              checkEq(rec, t2, loc)
              checkEq(valtype, t1, loc)
              tpe
            case (_, None) => failMismatchedShape(tpe, s"Record with ${label.name}", loc)
          }

        case AtomicOp.RecordRestrict(label) =>
          val List(t1) = ts
          removeFromRecordType(t1, label.name, loc) match {
            case (rec, Some(_)) =>
              checkEq(tpe, rec, loc)
            case (_, None) => failMismatchedShape(t1, s"Record with ${label.name}", loc)
          }

        case AtomicOp.RecordSelect(label) =>
          val List(t1) = ts
          selectFromRecordType(t1, label.name, loc) match {
            case Some(elmt) =>
              checkEq(tpe, elmt, loc)
            case None => failMismatchedShape(t1, s"Record with '${label.name}'", loc)
          }

        case AtomicOp.Closure(sym) =>
          val defn = root.defs(sym)
          val signature = MonoType.Arrow(defn.fparams.map(_.tpe), defn.tpe)

          val decl = MonoType.Arrow(defn.cparams.map(_.tpe), signature)
          val actual = MonoType.Arrow(ts, tpe)

          checkEq(decl, actual, loc)
          tpe

        case AtomicOp.Box =>
          check(expected = MonoType.Object)(actual = tpe, loc)

        case AtomicOp.Unbox =>
          val List(t1) = ts
          check(expected = MonoType.Object)(actual = t1, loc)
          tpe

        // cast may result in any type
        case AtomicOp.Cast =>
          tpe

        case AtomicOp.Region =>
          check(expected = MonoType.Region)(actual = tpe, loc)

        case AtomicOp.Spawn =>
          val List(t1, t2) = ts
          t1 match {
            case MonoType.Arrow(List(MonoType.Unit), _) => ()
            case _ => failMismatchedShape(t1, "Arrow(List(Unit), _)", loc)
          }

          check(expected = MonoType.Region)(actual = t2, loc)
          check(expected = MonoType.Unit)(actual = tpe, loc)

        case _ => tpe // TODO: VERIFIER: Add rest
      }

    case Expr.ApplyClo(exp, exps, ct, tpe, _, loc) =>
      val lamType1 = visitExpr(exp)
      val lamType2 = MonoType.Arrow(exps.map(visitExpr), tpe)
      checkEq(lamType1, lamType2, loc)
      tpe

    case Expr.ApplyDef(sym, exps, ct, tpe, _, loc) =>
      val defn = root.defs(sym)
      val declared = MonoType.Arrow(defn.fparams.map(_.tpe), defn.tpe)
      val actual = MonoType.Arrow(exps.map(visitExpr), tpe)
      check(expected = declared)(actual = actual, loc)
      tpe

    case Expr.ApplySelfTail(sym, actuals, tpe, _, loc) =>
      val defn = root.defs(sym)
      val declared = MonoType.Arrow(defn.fparams.map(_.tpe), defn.tpe)
      val actual = MonoType.Arrow(actuals.map(visitExpr), tpe)
      check(expected = declared)(actual = actual, loc)
      tpe

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      val condType = visitExpr(exp1)
      val thenType = visitExpr(exp2)
      val elseType = visitExpr(exp3)
      check(expected = MonoType.Bool)(actual = condType, exp1.loc)
      checkEq(tpe, thenType, exp2.loc)
      checkEq(tpe, elseType, exp3.loc)

    case Expr.Branch(exp, branches, tpe, _, loc) =>
      val lenv1 = branches.foldLeft(lenv) {
        case (acc, (label, _)) => acc + (label -> tpe)
      }
      branches.foreach {
        case (label, body) =>
          checkEq(tpe, visitExpr(body)(root, env, lenv1), loc)
      }
      tpe

    case Expr.JumpTo(sym, tpe1, _, loc) => lenv.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown label sym: '$sym'.", loc)
      case Some(tpe2) => checkEq(tpe1, tpe2, loc)
    }

    case Expr.Let(sym, exp1, exp2, tpe, _, loc) =>
      val letBoundType = visitExpr(exp1)
      val bodyType = visitExpr(exp2)(root, env + (sym -> letBoundType), lenv)
      checkEq(bodyType, tpe, loc)

    case Expr.LetRec(varSym, _, defSym, exp1, exp2, tpe, _, loc) =>
      val env1 = env + (varSym -> exp1.tpe)
      val letBoundType = visitExpr(exp1)(root, env1, lenv)
      val bodyType = visitExpr(exp2)(root, env1, lenv)
      checkEq(bodyType, tpe, loc)

    case Expr.Stmt(exp1, exp2, tpe, _, loc) =>
      val firstType = visitExpr(exp1)
      val secondType = visitExpr(exp2)
      checkEq(secondType, tpe, loc)

    case Expr.Scope(sym, exp, tpe, _, loc) =>
      checkEq(tpe, visitExpr(exp)(root, env + (sym -> MonoType.Region), lenv), loc)

    case Expr.TryCatch(exp, rules, tpe, _, loc) =>
      for (CatchRule(sym, clazz, exp) <- rules) {
        checkEq(tpe, visitExpr(exp)(root, env + (sym -> MonoType.Native(clazz)), lenv), exp.loc)
      }
      val t = visitExpr(exp)
      checkEq(tpe, t, loc)

    case Expr.TryWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val exptype = visitExpr(exp) match {
        case MonoType.Arrow(List(MonoType.Unit), t) => t
        case e => failMismatchedShape(e, "Arrow(List(Unit), _)", exp.loc)
      }

      val effect = root.effects.getOrElse(effUse.sym,
        throw InternalCompilerException(s"Unknown effect sym: '${effUse.sym}'", effUse.loc))
      val ops = effect.ops.map(op => op.sym -> op).toMap

      for (rule <- rules) {
        val ruletype = visitExpr(rule.exp)
        val op = ops.getOrElse(rule.op.sym,
          throw InternalCompilerException(s"Unknown operation sym: '${rule.op.sym}'", rule.op.loc))

        val params = op.fparams.map(_.tpe)
        val resumptionType = MonoType.Arrow(List(op.tpe), exptype)
        val signature = MonoType.Arrow(params :+ resumptionType, exptype)

        checkEq(ruletype, signature, rule.exp.loc)
      }

      checkEq(tpe, exptype, loc)

    case Expr.Do(opUse, exps, tpe, purity, loc) =>
      val ts = exps.map(visitExpr)
      val eff = root.effects.getOrElse(opUse.sym.eff,
        throw InternalCompilerException(s"Unknown effect sym: '${opUse.sym.eff}'", opUse.loc))
      val op = eff.ops.find(_.sym == opUse.sym)
        .getOrElse(throw InternalCompilerException(s"Unknown operation sym: '${opUse.sym}'", opUse.loc))

      val oprestype = op.tpe match {
        case MonoType.Void => tpe // should match any return type
        case t => t
      }

      val sig = MonoType.Arrow(ts, tpe)
      val opsig = MonoType.Arrow(
        op.fparams.map(_.tpe), oprestype
      )

      checkEq(sig, opsig, loc)
      tpe

    case Expr.NewObject(_, clazz, tpe, _, methods, loc) =>
      for (m <- methods) {
        val exptype = visitExpr(m.exp)
        val signature = MonoType.Arrow(m.fparams.map(_.tpe), m.tpe)
        checkEq(signature, exptype, m.loc)
      }
      checkEq(tpe, MonoType.Native(clazz), loc)

  }

  /**
    * Asserts that the the given type `expected` is equal to the `actual` type.
    */
  private def check(expected: MonoType)(actual: MonoType, loc: SourceLocation): MonoType = {
    if (expected == actual)
      expected
    else failUnexpectedType(actual, expected, loc)
  }

  /**
    * Asserts that the two given types `tpe1` and `tpe2` are the same.
    */
  private def checkEq(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation): MonoType = {
    if (tpe1 == tpe2)
      tpe1
    else failMismatchedTypes(tpe1, tpe2, loc)
  }

  /**
    * Remove the type associated with `label` from the given record type `rec`.
    * If `rec` is not a record, return `None`.
    */
  private def removeFromRecordType(rec: MonoType, label: String, loc: SourceLocation): (MonoType, Option[MonoType]) = rec match {
    case MonoType.RecordEmpty => (rec, None)
    case MonoType.RecordExtend(lbl, valtype, rest) =>
      if (label == lbl) (rest, Some(valtype))
      else {
        val (rec, opt) = removeFromRecordType(rest, label, loc)
        (MonoType.RecordExtend(lbl, valtype, rec), opt)
      }
    case _ => failMismatchedShape(rec, "Record", loc)
  }

  /**
    * Get the type associated with `label` in the given record type `rec`.
    * If `rec` is not a record, return `None`
    */
  @tailrec
  private def selectFromRecordType(rec: MonoType, label: String, loc: SourceLocation): Option[MonoType] = rec match {
    case MonoType.RecordExtend(lbl, valtype, rest) =>
      if (lbl == label)
        Some(valtype)
      else
        selectFromRecordType(rest, label, loc)
    case MonoType.RecordEmpty => None
    case _ => failMismatchedShape(rec, "Record", loc)
  }

  /**
    * Throw `InternalCompilerException` because the `found` does not match the shape specified by `expected`
    */
  private def failMismatchedShape(found: MonoType, expected: String, loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Mismatched shape near ${loc.format}: expected = \'$expected\', found = $found", loc
    )

  /**
    * Throw `InternalCompilerException` because the `expected` type does not match the `found` type
    */
  private def failUnexpectedType(found: MonoType, expected: MonoType, loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Unexpected type near ${loc.format}: expected = $expected, found = $found", loc
    )

  /**
    * Throw `InternalCompilerException` because `tpe1` is not equal to `tpe2`
    */
  private def failMismatchedTypes(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Mismatched types near ${loc.format}: tpe1 = $tpe1, tpe2 = $tpe2", loc
    )
}
