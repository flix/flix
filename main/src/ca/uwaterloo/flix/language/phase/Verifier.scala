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
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoType, ReducedAst, SemanticOp, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Verify the AST before bytecode generation.
  */
object Verifier {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Verifier") {
    if (flix.options.xverify) {
      root.defs.values.foreach(d => visitDef(d)(root))
    }
    root
  }

  private def visitDef(decl: Def)(implicit root: Root): Unit = {
    val env = (decl.cparams ++ decl.fparams).foldLeft(Map.empty[Symbol.VarSym, MonoType]) {
      case (macc, fparam) => macc + (fparam.sym -> fparam.tpe)
    }
    try {
      visitExpr(decl.expr)(root, env, Map.empty)
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

      case MismatchedShape(tpe, expected, loc) =>
        println(s"Mismatched shape near ${loc.format}")
        println()
        println(s"  expected = \'$expected\'")
        println(s"  found    = $tpe")
        println()
    }
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
      case None => throw InternalCompilerException(s"Unknown variable sym: '$sym'", loc)
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
            case _ => throw MismatchedShape(t1, "Array", loc)
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
            case _ => throw MismatchedShape(tpe, "Array", loc)
          }

        case AtomicOp.ArrayLoad =>
          val List(t1, t2) = ts
          t1 match {
            case MonoType.Array(elmt) =>
              check(expected = MonoType.Int32)(actual = t2, loc)
              checkEq(elmt, tpe, loc)
            case _ => throw MismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.ArrayStore =>
          val List(t1, t2, t3) = ts
          t1 match {
            case MonoType.Array(elmt) =>
              check(expected = MonoType.Int32)(actual = t2, loc)
              checkEq(elmt, t3, loc)
              check(expected = MonoType.Unit)(actual = tpe, loc)
            case _ => throw MismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.Ref =>
          val List(t1) = ts
          val refType = MonoType.Ref(t1)
          checkEq(refType, tpe, loc)

        case AtomicOp.Deref =>
          val List(t1) = ts
          t1 match {
            case MonoType.Ref(elm) => checkEq(elm, tpe, loc)
            case _ => throw MismatchedShape(t1, "Ref", loc)
          }

        case AtomicOp.Lazy =>
          val List(t1) = ts
          tpe match {
            case MonoType.Lazy(elmt) =>
              val fun = MonoType.Arrow(List(MonoType.Unit), elmt)
              checkEq(t1, fun, loc)
              tpe
            case _ => throw MismatchedShape(tpe, "Lazy", loc)
          }

        case AtomicOp.Force =>
          val List(t1) = ts
          t1 match {
            case MonoType.Lazy(elm) => checkEq(elm, tpe, loc)
            case _ => throw MismatchedShape(t1, "Lazy", loc)
          }

        case AtomicOp.Tuple =>
          val tup = MonoType.Tuple(ts)
          checkEq(tup, tpe, loc)

        case AtomicOp.Index(idx: Int) =>
          val List(t1) = ts
          t1 match {
            case MonoType.Tuple(elms) => checkEq(elms(idx), tpe, loc)
            case _ => throw MismatchedShape(t1, "Tuple", loc)
          }

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
      check(expected = MonoType.Bool)(actual = condType, loc)
      checkEq(tpe, thenType, loc)
      checkEq(tpe, elseType, loc)

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
        checkEq(tpe, visitExpr(exp)(root, env + (sym -> MonoType.Native(clazz)), lenv), loc)
      }
      val t = visitExpr(exp)
      checkEq(tpe, t, loc)

    case Expr.TryWith(_, _, _, _, tpe, _, _) =>
      // TODO: VERIFIER: Add support for TryWith.
      tpe

    case Expr.Do(_, _, tpe, _, _) =>
      // TODO: VERIFIER: Add support for Do.
      tpe

    case Expr.NewObject(name, clazz, tpe, methods, _, loc) =>
      // TODO: VERIFIER: Add support for NewObject.
      tpe

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
    * An exception raised because `tpe` did not match an expected shape
    */
  private case class MismatchedShape(tpe: MonoType, expected: String, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised because the `expected` type does not match the `found` type.
    */
  private case class UnexpectedType(expected: MonoType, found: MonoType, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised because `tpe1` is not equal to `tpe2`.
    */
  private case class MismatchedTypes(tpe1: MonoType, tpe2: MonoType, loc: SourceLocation) extends RuntimeException

}
