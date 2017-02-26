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
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object PrettyPrinter {

  object Simplified {

    def fmtRoot(root: Root): VirtualTerminal = {
      val o = new VirtualTerminal()
      for ((sym, defn) <- root.definitions.toList.sortBy(_._1.loc)) {
        o.bold("def").text(" ").blue(sym).text("(")
        for (fparam <- defn.formals) {
          fmtParam(fparam, o)
          o.text(", ")
        }
        o.text(") = ").indent().newLine()
        fmtExp(defn, o)
        o.dedent()
        o.newLine()
        o.newLine()
      }
      o
    }

    def fmtExp(defn: Definition.Constant, o: VirtualTerminal): Unit = {
      fmtExp(defn.exp, o)
    }

    def fmtExp(exp0: Expression, o: VirtualTerminal): Unit = {
      def visitExp(e0: Expression): Unit = e0 match {
        case Expression.Unit => o.text("Unit")
        case Expression.True => o.text("true")
        case Expression.False => o.text("false")
        case Expression.Char(lit) => o.text("'").text(lit).text("'")
        case Expression.Float32(lit) => o.text(lit).text("f32")
        case Expression.Float64(lit) => o.text(lit).text("f32")
        case Expression.Int8(lit) => o.text(lit).text("i8")
        case Expression.Int16(lit) => o.text(lit).text("i16")
        case Expression.Int32(lit) => o.text(lit).text("i32")
        case Expression.Int64(lit) => o.text(lit).text("i64")
        case Expression.BigInt(lit) => o.text(lit).text("ii")
        case Expression.Str(lit) => o.text("\"").text(lit).text("\"")
        case Expression.LoadBool(base, offset) => ???
        case Expression.LoadInt8(base, offset) => ???
        case Expression.LoadInt16(base, offset) => ???
        case Expression.LoadInt32(base, offset) => ???
        case Expression.StoreBool(base, offset, value) => ???
        case Expression.StoreInt8(base, offset, value) => ???
        case Expression.StoreInt16(base, offset, value) => ???
        case Expression.StoreInt32(base, offset, value) => ???
        case Expression.Var(sym, tpe, loc) => fmtSym(sym, o)
        case Expression.Ref(sym, tpe, loc) => fmtSym(sym, o)
        case Expression.Lambda(fparams, body, tpe, loc) =>
          o.text("(")
          for (fparam <- fparams) {
            o.text(fparam.sym.toString)
            o.text(", ")
          }
          o.text(")")
          o.text(" -> ")
          visitExp(body)

        case Expression.Hook(hook, tpe, loc) => o.text(hook.sym.toString)

        case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
          o.text("MkClosureRef(")
          visitExp(ref)
          o.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, o)
            o.text(", ")
          }
          o.text("])")

        case Expression.MkClosure(lambda, freeVars, tpe, loc) =>
          o.text("MkClosure(")
          visitExp(lambda)
          o.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, o)
            o.text(", ")
          }
          o.text("])")

        case Expression.ApplyRef(sym, args, tpe, loc) =>
          fmtSym(sym, o)
          o.text("(")
          for (arg <- args) {
            visitExp(arg)
            o.text(", ")
          }
          o.text(")")

        case Expression.ApplyTail(name, formals, args, tpe, loc) =>
          o.text("ApplyTail")
          o.text("(")
          for (arg <- args) {
            visitExp(arg)
            o.text(", ")
          }
          o.text(")")

        case Expression.ApplyHook(hook, args, tpe, loc) =>
          fmtSym(hook.sym, o)
          o.text("(")
          for (arg <- args) {
            visitExp(arg)
            o.text(", ")
          }
          o.text(")")

        case Expression.Apply(exp, args, tpe, loc) =>
          visitExp(exp)
          o.text("(")
          for (arg <- args) {
            visitExp(arg)
            o.text(", ")
          }
          o.text(")")

        case Expression.Unary(op, exp, tpe, loc) =>
          fmtUnaryOp(op, o)
          visitExp(exp)

        case Expression.Binary(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          o.text(" ")
          fmtUnaryOp(op, o)
          o.text(" ")
          visitExp(exp2)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          o.bold("if").text(" (")
          visitExp(exp1)
          o.text(") {")
          o.indent().newLine()
          visitExp(exp2)
          o.dedent().newLine()
          o.text("} ").bold("else").text(" {")
          o.indent().newLine()
          visitExp(exp3)
          o.dedent()
          o.newLine()
          o.text("}")

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          o.bold("let").text(" ")
          fmtSym(sym, o)
          o.text(" = ")
          visitExp(exp1)
          o.text(";").newLine()
          visitExp(exp2)

        case Expression.Is(exp, tag, loc) =>
          visitExp(exp)
          o.text(" is ")
          o.text(tag)

        case Expression.Tag(enum, tag, exp, tpe, loc) => exp match {
          case Expression.Unit => o.text(tag)
          case _ =>
            o.text(tag).text("(")
            visitExp(exp)
            o.text(")")
        }

        case Expression.Untag(tag, exp, tpe, loc) =>
          o.text("Untag(")
          visitExp(exp)
          o.text(")")

        case Expression.Index(exp, offset, tpe, loc) =>
          visitExp(exp)
          o.text("[")
          o.text(offset)
          o.text("]")

        case Expression.Tuple(elms, tpe, loc) =>
          o.text("(")
          for (elm <- elms) {
            visitExp(elm)
            o.text(", ")
          }
          o.text(")")

        case Expression.Existential(fparam, exp, loc) =>
          o.text("∃(")
          fmtParam(fparam, o)
          o.text("). ")
          visitExp(exp)

        case Expression.Universal(fparam, exp, loc) =>
          o.text("∀(")
          fmtParam(fparam, o)
          o.text("). ")
          visitExp(exp)

        case Expression.UserError(tpe, loc) => o.red("UserError")
        case Expression.MatchError(tpe, loc) => o.red("MatchError")
        case Expression.SwitchError(tpe, loc) => o.red("SwitchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: FormalParam, o: VirtualTerminal): Unit = {
      fmtSym(p.sym, o)
      o.text(": ")
      o.text(p.tpe.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, o: VirtualTerminal): Unit = {
      o.blue(sym)
    }

    def fmtSym(sym: Symbol.VarSym, o: VirtualTerminal): Unit = {
      o.cyan(sym)
    }

    def fmtUnaryOp(op: UnaryOperator, o: VirtualTerminal): Unit = op match {
      case UnaryOperator.LogicalNot => o.text("!")
      case UnaryOperator.Plus => o.text("+")
      case UnaryOperator.Minus => o.text("-")
      case UnaryOperator.BitwiseNegate => o.text("~~~")
    }

    def fmtUnaryOp(op: BinaryOperator, o: VirtualTerminal): Unit = op match {
      case BinaryOperator.Plus => o.text("+")
      case BinaryOperator.Minus => o.text("-")
      case BinaryOperator.Times => o.text("*")
      case BinaryOperator.Divide => o.text("/")
      case BinaryOperator.Modulo => o.text("%")
      case BinaryOperator.Exponentiate => o.text("**")
      case BinaryOperator.Less => o.text("<")
      case BinaryOperator.LessEqual => o.text("<=")
      case BinaryOperator.Greater => o.text(">")
      case BinaryOperator.GreaterEqual => o.text(">=")
      case BinaryOperator.Equal => o.text("==")
      case BinaryOperator.NotEqual => o.text("!=")
      case BinaryOperator.LogicalAnd => o.text("&&")
      case BinaryOperator.LogicalOr => o.text("||")
      case BinaryOperator.BitwiseAnd => o.text("&&&")
      case BinaryOperator.BitwiseOr => o.text("|||")
      case BinaryOperator.BitwiseXor => o.text("^^^")
      case BinaryOperator.BitwiseLeftShift => o.text("<<<")
      case BinaryOperator.BitwiseRightShift => o.text(">>>")
    }

  }

}
