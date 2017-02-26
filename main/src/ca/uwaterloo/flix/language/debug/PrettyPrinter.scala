/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object PrettyPrinter {

  object Simplified {

    def fmtRoot(root: Root): VirtualTerminal = {
      val vt = new VirtualTerminal()
      for ((sym, defn) <- root.definitions.toList.sortBy(_._1.loc)) {
        vt << Bold("def") << " " << Blue(sym.toString) << "("
        for (fparam <- defn.formals) {
          fmtParam(fparam, vt)
          vt << ", "
        }
        vt << ") = "
        vt.indent().newLine()
        fmtExp(defn, vt)
        vt.dedent()
        vt.newLine()
        vt.newLine()
      }
      vt
    }

    def fmtExp(defn: Definition.Constant, vt: VirtualTerminal): Unit = {
      fmtExp(defn.exp, vt)
    }

    def fmtExp(exp0: Expression, vt: VirtualTerminal): Unit = {
      def visitExp(e0: Expression): Unit = e0 match {
        case Expression.Unit => vt.text("Unit")
        case Expression.True => vt.text("true")
        case Expression.False => vt.text("false")
        case Expression.Char(lit) => vt.text("'").text(lit).text("'")
        case Expression.Float32(lit) => vt.text(lit).text("f32")
        case Expression.Float64(lit) => vt.text(lit).text("f32")
        case Expression.Int8(lit) => vt.text(lit).text("i8")
        case Expression.Int16(lit) => vt.text(lit).text("i16")
        case Expression.Int32(lit) => vt.text(lit).text("i32")
        case Expression.Int64(lit) => vt.text(lit).text("i64")
        case Expression.BigInt(lit) => vt.text(lit).text("ii")
        case Expression.Str(lit) => vt.text("\"").text(lit).text("\"")
        case Expression.LoadBool(base, offset) => ???
        case Expression.LoadInt8(base, offset) => ???
        case Expression.LoadInt16(base, offset) => ???
        case Expression.LoadInt32(base, offset) => ???
        case Expression.StoreBool(base, offset, value) => ???
        case Expression.StoreInt8(base, offset, value) => ???
        case Expression.StoreInt16(base, offset, value) => ???
        case Expression.StoreInt32(base, offset, value) => ???
        case Expression.Var(sym, tpe, loc) => fmtSym(sym, vt)
        case Expression.Ref(sym, tpe, loc) => fmtSym(sym, vt)
        case Expression.Lambda(fparams, body, tpe, loc) =>
          vt.text("(")
          for (fparam <- fparams) {
            vt.text(fparam.sym.toString)
            vt.text(", ")
          }
          vt.text(")")
          vt.text(" -> ")
          visitExp(body)

        case Expression.Hook(hook, tpe, loc) => vt.text(hook.sym.toString)

        case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
          vt.text("MkClosureRef(")
          visitExp(ref)
          vt.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, vt)
            vt.text(", ")
          }
          vt.text("])")

        case Expression.MkClosure(lambda, freeVars, tpe, loc) =>
          vt.text("MkClosure(")
          visitExp(lambda)
          vt.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, vt)
            vt.text(", ")
          }
          vt.text("])")

        case Expression.ApplyRef(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyTail(name, formals, args, tpe, loc) =>
          vt.text("ApplyTail")
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyHook(hook, args, tpe, loc) =>
          fmtSym(hook.sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.Apply(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.Unary(op, exp, tpe, loc) =>
          fmtUnaryOp(op, vt)
          visitExp(exp)

        case Expression.Binary(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtBinaryOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          vt << Bold("if") << " ("
          visitExp(exp1)
          vt.text(") {")
          vt.indent().newLine()
          visitExp(exp2)
          vt.dedent().newLine()
          vt.text("} ")
          vt << Bold("else") << " {"
          vt.indent().newLine()
          visitExp(exp3)
          vt.dedent()
          vt.newLine()
          vt.text("}")

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          visitExp(exp1)
          vt.text(";").newLine()
          visitExp(exp2)

        case Expression.Is(exp, tag, loc) =>
          visitExp(exp)
          vt.text(" is ")
          vt.text(tag)

        case Expression.Tag(enum, tag, exp, tpe, loc) => exp match {
          case Expression.Unit => vt.text(tag)
          case _ =>
            vt.text(tag).text("(")
            visitExp(exp)
            vt.text(")")
        }

        case Expression.Untag(tag, exp, tpe, loc) =>
          vt.text("Untag(")
          visitExp(exp)
          vt.text(")")

        case Expression.Index(exp, offset, tpe, loc) =>
          visitExp(exp)
          vt.text("[")
          vt.text(offset)
          vt.text("]")

        case Expression.Tuple(elms, tpe, loc) =>
          vt.text("(")
          for (elm <- elms) {
            visitExp(elm)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.Existential(fparam, exp, loc) =>
          vt.text("∃(")
          fmtParam(fparam, vt)
          vt.text("). ")
          visitExp(exp)

        case Expression.Universal(fparam, exp, loc) =>
          vt.text("∀(")
          fmtParam(fparam, vt)
          vt.text("). ")
          visitExp(exp)

        case Expression.UserError(tpe, loc) => vt << Red("UserError")
        case Expression.MatchError(tpe, loc) => vt << Red("MatchError")
        case Expression.SwitchError(tpe, loc) => vt << Red("SwitchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: FormalParam, vt: VirtualTerminal): Unit = {
      fmtSym(p.sym, vt)
      vt.text(": ")
      vt.text(p.tpe.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, vt: VirtualTerminal): Unit = {
      vt << Blue(sym.toString)
    }

    def fmtSym(sym: Symbol.VarSym, vt: VirtualTerminal): Unit = {
      vt << Cyan(sym.toString)
    }

    def fmtUnaryOp(op: UnaryOperator, vt: VirtualTerminal): Unit = op match {
      case UnaryOperator.LogicalNot => vt.text("!")
      case UnaryOperator.Plus => vt.text("+")
      case UnaryOperator.Minus => vt.text("-")
      case UnaryOperator.BitwiseNegate => vt.text("~~~")
    }

    def fmtBinaryOp(op: BinaryOperator, vt: VirtualTerminal): VirtualTerminal = op match {
      case BinaryOperator.Plus => vt.text("+")
      case BinaryOperator.Minus => vt.text("-")
      case BinaryOperator.Times => vt.text("*")
      case BinaryOperator.Divide => vt.text("/")
      case BinaryOperator.Modulo => vt.text("%")
      case BinaryOperator.Exponentiate => vt.text("**")
      case BinaryOperator.Less => vt.text("<")
      case BinaryOperator.LessEqual => vt.text("<=")
      case BinaryOperator.Greater => vt.text(">")
      case BinaryOperator.GreaterEqual => vt.text(">=")
      case BinaryOperator.Equal => vt.text("==")
      case BinaryOperator.NotEqual => vt.text("!=")
      case BinaryOperator.LogicalAnd => vt.text("&&")
      case BinaryOperator.LogicalOr => vt.text("||")
      case BinaryOperator.BitwiseAnd => vt.text("&&&")
      case BinaryOperator.BitwiseOr => vt.text("|||")
      case BinaryOperator.BitwiseXor => vt.text("^^^")
      case BinaryOperator.BitwiseLeftShift => vt.text("<<<")
      case BinaryOperator.BitwiseRightShift => vt.text(">>>")
    }

  }

}
