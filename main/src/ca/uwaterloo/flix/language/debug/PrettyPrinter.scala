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
      for ((sym, defn) <- root.defs.toList.sortBy(_._1.loc)) {
        vt << Bold("def") << " " << Blue(sym.toString) << "("
        for (fparam <- defn.fparams) {
          fmtParam(fparam, vt)
          vt << ", "
        }
        vt << ") = "
        vt << Indent << NewLine
        fmtExp(defn, vt)
        vt << Dedent << NewLine << NewLine
      }
      vt
    }

    def fmtExp(defn: SimplifiedAst.Def, vt: VirtualTerminal): Unit = {
      fmtExp(defn.exp, vt)
    }

    def fmtExp(exp0: Expression, vt: VirtualTerminal): Unit = {
      def visitExp(e0: Expression): Unit = e0 match {
        case Expression.Unit => vt.text("Unit")
        case Expression.True => vt.text("true")
        case Expression.False => vt.text("false")
        case Expression.Char(lit) => vt.text("'").text(lit.toString).text("'")
        case Expression.Float32(lit) => vt.text(lit.toString).text("f32")
        case Expression.Float64(lit) => vt.text(lit.toString).text("f32")
        case Expression.Int8(lit) => vt.text(lit.toString).text("i8")
        case Expression.Int16(lit) => vt.text(lit.toString).text("i16")
        case Expression.Int32(lit) => vt.text(lit.toString).text("i32")
        case Expression.Int64(lit) => vt.text(lit.toString).text("i64")
        case Expression.BigInt(lit) => vt.text(lit.toString()).text("ii")
        case Expression.Str(lit) => vt.text("\"").text(lit).text("\"")

        case Expression.Var(sym, tpe, loc) => fmtSym(sym, vt)

        case Expression.Def(sym, tpe, loc) => fmtSym(sym, vt)

        case Expression.Eff(sym, tpe, loc) => fmtSym(sym, vt)

        case Expression.Lambda(fparams, body, tpe, loc) =>
          vt.text("(")
          for (fparam <- fparams) {
            vt.text(fparam.sym.toString)
            vt.text(", ")
          }
          vt.text(")")
          vt.text(" -> ")
          visitExp(body)

        case Expression.Apply(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.LambdaClosure(lambda, freeVars, tpe, loc) =>
          vt.text("LambdaClosure(")
          visitExp(lambda)
          vt.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, vt)
            vt.text(", ")
          }
          vt.text("])")

        case Expression.Closure(sym, freeVars, tpe, loc) =>
          vt.text("Closure(")
          fmtSym(sym, vt)
          vt.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, vt)
            vt.text(", ")
          }
          vt.text("])")

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyDef(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")
          vt.text(")")

        case Expression.ApplyEff(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyEffTail(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplySelfTail(name, formals, args, tpe, loc) =>
          vt.text("ApplySelfTail")
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          fmtUnaryOp(op, vt)
          visitExp(exp)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtBinaryOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          vt << Bold("if") << " ("
          visitExp(exp1)
          vt.text(") {")
          vt << Indent << NewLine
          visitExp(exp2)
          vt << Dedent << NewLine
          vt.text("} ")
          vt << Bold("else") << " {"
          vt << Indent << NewLine
          visitExp(exp3)
          vt << Dedent << NewLine
          vt.text("}")

        case Expression.Branch(exp, branches, tpe, loc) =>
          vt << "branch {" << Indent << NewLine
          visitExp(exp)
          vt << NewLine
          for ((sym, b) <- branches) {
            fmtSym(sym, vt)
            vt << ":" << Indent << NewLine
            visitExp(b)
            vt << Dedent << NewLine
          }
          vt << "}" << Dedent << NewLine

        case Expression.JumpTo(sym, tpe, loc) =>
          vt << "jumpto" << " "
          fmtSym(sym, vt)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          vt << Indent << NewLine
          visitExp(exp1)
          vt << Dedent
          vt << ";" << NewLine
          visitExp(exp2)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          visitExp(exp1)
          vt << ";" << NewLine
          visitExp(exp2)

        case Expression.Is(sym, tag, exp, loc) =>
          visitExp(exp)
          vt.text(" is ")
          vt.text(tag)

        case Expression.Tag(sym, tag, exp, tpe, loc) => exp match {
          case Expression.Unit => vt.text(tag)
          case _ =>
            vt.text(tag).text("(")
            visitExp(exp)
            vt.text(")")
        }

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          vt.text("Untag(")
          visitExp(exp)
          vt.text(")")

        case Expression.Index(exp, offset, tpe, loc) =>
          visitExp(exp)
          vt.text("[")
          vt.text(offset.toString)
          vt.text("]")

        case Expression.Tuple(elms, tpe, loc) =>
          vt.text("(")
          for (elm <- elms) {
            visitExp(elm)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ArrayLit(elms, tpe, loc) =>
          vt.text("[")
          for (elm <- elms){
            visitExp(elm)
            vt.text(",")
          }
          vt.text("]")

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          vt.text("[")
          visitExp(elm)
          vt.text(";")
          vt.text(len.toString)
          vt.text("]")

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(index)
          vt.text("]")

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(index)
          vt.text("]")
          vt.text(" = ")
          visitExp(elm)

        case Expression.ArrayLength(base, tpe, loc) =>
          vt.text("length")
          vt.text("[")
          visitExp(base)
          vt.text("]")

        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(beginIndex)
          vt.text("..")
          visitExp(endIndex)
          vt.text("]")

        case Expression.Ref(exp, tpe, loc) =>
          vt.text("ref ")
          visitExp(exp)

        case Expression.Deref(exp, tpe, loc) =>
          vt.text("deref ")
          visitExp(exp)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" := ")
          visitExp(exp2)

        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          vt << "do" << Indent << NewLine
          visitExp(exp)
          vt.text("with {")
          for (HandlerBinding(sym, handler) <- bindings) {
            vt << "eff "
            fmtSym(sym, vt)
            vt << " = "
            visitExp(handler)
          }
          vt.text("}")
          vt << Dedent << NewLine

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

        case Expression.NativeConstructor(constructor, args, tpe, loc) =>
          vt.text(constructor.toString)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.NativeField(field, tpe, loc) => vt << field.toString

        case Expression.NativeMethod(method, args, tpe, loc) =>
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.UserError(tpe, loc) => vt << Red("UserError")
        case Expression.HoleError(sym, tpe, eff, loc) => Red("HoleError")
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

    def fmtSym(sym: Symbol.VarSym, vt: VirtualTerminal): Unit = {
      vt << Cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, vt: VirtualTerminal): Unit = {
      vt << Blue(sym.toString)
    }

    def fmtSym(sym: Symbol.EffSym, vt: VirtualTerminal): Unit = {
      vt << Red(sym.toString)
    }

    def fmtSym(sym: Symbol.LabelSym, vt: VirtualTerminal): Unit = {
      vt << Magenta(sym.toString)
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
