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

import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.Format

object PrettyPrinter {

  private implicit val audience: Audience = Audience.External

  object Lifted {

    def fmtRoot(root: Root): String = {
      val sb = new StringBuilder()
      for ((sym, defn) <- root.defs.toList.sortBy(_._1.loc)) {
        sb.append(s"${Format.bold("def")} ${Format.blue(sym.toString)}(")
        for (fparam <- defn.fparams) {
          sb.append(s"${fmtParam(fparam)}, ")
        }
        sb.append(") = ")
        sb.append(fmtDef(defn).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
        sb.append(System.lineSeparator() + System.lineSeparator())
      }
      sb.toString()
    }

    def fmtDef(defn: Def): String = {
      fmtExp(defn.exp)
    }

    def fmtExp(exp0: Expression): String = {
      def visitExp(e0: Expression): String = e0 match {
        case Expression.Unit(_) => Format.text("Unit")

        case Expression.Null(tpe, _) => Format.text("null")

        case Expression.True(_) => Format.text("true")

        case Expression.False(_) => Format.text("false")

        case Expression.Char(lit, _) => Format.text("'") + Format.text(lit.toString) + Format.text("'")

        case Expression.Float32(lit, _) => Format.text(lit.toString) + Format.text("f32")

        case Expression.Float64(lit, _) => Format.text(lit.toString) + Format.text("f32")

        case Expression.Int8(lit, _) => Format.text(lit.toString) + Format.text("i8")

        case Expression.Int16(lit, _) => Format.text(lit.toString) + Format.text("i16")

        case Expression.Int32(lit, _) => Format.text(lit.toString) + Format.text("i32")

        case Expression.Int64(lit, _) => Format.text(lit.toString) + Format.text("i64")

        case Expression.BigInt(lit, _) => Format.text(lit.toString()) + Format.text("ii")

        case Expression.Str(lit, _) => Format.text("\"") + Format.text(lit) + Format.text("\"")

        case Expression.Var(sym, tpe, loc) => fmtSym(sym)

        case Expression.Closure(sym, freeVars, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.text("Closure("))
            .append(fmtSym(sym))
            .append(Format.text(", ["))
          for (freeVar <- freeVars) {
            sb.append(fmtSym(freeVar.sym))
              .append(Format.text(", "))
          }
          sb.append(Format.text("])"))
            .toString()

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append(Format.text("("))
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(fmtSym(sym))
            .append(Format.text("("))
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .append(Format.text(")"))
            .toString()

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append(Format.text("*("))
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(fmtSym(sym))
            .append(Format.text("*("))
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.ApplySelfTail(name, formals, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.text("ApplySelfTail"))
            .append(Format.text("*("))
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          fmtUnaryOp(op) + visitExp(exp)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp1))
            .append(Format.text(" "))
            .append(fmtBinaryOp(op))
            .append(Format.text(" "))
            .append(visitExp(exp2))
            .toString()

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.bold("if") + " (")
            .append(visitExp(exp1))
            .append(Format.text(") {"))
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp2).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append(Format.text("} "))
            .append(Format.bold("else") + " {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp3).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append(Format.text("}"))
            .toString()

        case Expression.Branch(exp, branches, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("branch {")
            .append((" " * 2) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
          for ((sym, b) <- branches) {
            sb.append((" " * 4) + fmtSym(sym).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(":")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(b).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          sb.append("}")
            .append(System.lineSeparator())
            .toString()

        case Expression.JumpTo(sym, tpe, loc) => s"jumpto ${fmtSym(sym)}"

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.bold("let "))
            .append(fmtSym(sym))
            .append(Format.text(" = "))
            .append(visitExp(exp1).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(";")
            .append(System.lineSeparator())
            .append(visitExp(exp2))
            .toString()

        case Expression.Is(sym, tag, exp, loc) => visitExp(exp) + Format.text(" is ") + Format.text(tag.name)

        case Expression.Tag(sym, tag, exp, tpe, loc) => exp match {
          case Expression.Unit(_) => Format.text(tag.name)
          case _ =>
            val sb = new StringBuilder()
            sb.append(Format.text(tag.name))
              .append(Format.text("("))
              .append(visitExp(exp))
              .append(Format.text(")"))
              .toString()
        }

        case Expression.Untag(sym, tag, exp, tpe, loc) => Format.text("Untag(") + visitExp(exp) + Format.text(")")

        case Expression.Index(exp, offset, tpe, loc) =>
          visitExp(exp) +
            Format.text("[") +
            Format.text(offset.toString) +
            Format.text("]")

        case Expression.Tuple(elms, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.text("("))
          for (elm <- elms) {
            sb.append(visitExp(elm))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.RecordEmpty(tpe, loc) => Format.text("{}")

        case Expression.RecordSelect(exp, field, tpe, loc) =>
          visitExp(exp) +
            Format.text(".") +
            Format.text(field.name)

        case Expression.RecordExtend(field, value, rest, tpe, loc) =>
          Format.text("{ ") +
            Format.text(field.name) +
            Format.text(" = ") +
            visitExp(value) +
            Format.text(" | ") +
            visitExp(rest) +
            Format.text(" }")

        case Expression.RecordRestrict(field, rest, tpe, loc) =>
          Format.text("{ -") +
            Format.text(field.name) +
            Format.text(" | ") +
            visitExp(rest) +
            Format.text("}")

        case Expression.ArrayLit(elms, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.text("["))
          for (elm <- elms) {
            sb.append(visitExp(elm))
            sb.append(Format.text(","))
          }
          sb.append(Format.text("]"))
            .toString()

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          Format.text("[") +
            visitExp(elm) +
            Format.text(";") +
            Format.text(len.toString) +
            Format.text("]")

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          visitExp(base) +
            Format.text("[") +
            visitExp(index) +
            Format.text("]")

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          visitExp(base) +
            Format.text("[") +
            visitExp(index) +
            Format.text("]") +
            Format.text(" = ") +
            visitExp(elm)

        case Expression.ArrayLength(base, tpe, loc) =>
          Format.text("length") +
            Format.text("[") +
            visitExp(base) +
            Format.text("]")

        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
          visitExp(base) +
            Format.text("[") +
            visitExp(beginIndex) +
            Format.text("..") +
            visitExp(endIndex) +
            Format.text("]")

        case Expression.Ref(exp, tpe, loc) => Format.text("ref ") + visitExp(exp)

        case Expression.Deref(exp, tpe, loc) => Format.text("deref ") + visitExp(exp)

        case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) + Format.text(" := ") + visitExp(exp2)

        case Expression.Existential(fparam, exp, loc) =>
          Format.text("∃(") +
            fmtParam(fparam) +
            Format.text("). ") +
            visitExp(exp)

        case Expression.Universal(fparam, exp, loc) =>
          Format.text("∀(") +
            fmtParam(fparam) +
            Format.text("). ") +
            visitExp(exp)

        case Expression.Cast(exp, tpe, loc) =>
          visitExp(exp) +
            Format.text(" as ") +
            Format.text(tpe.toString)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("try {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("} catch {")
            .append(System.lineSeparator())
          for (CatchRule(sym, clazz, body) <- rules) {
            sb.append("  case ")
              .append(fmtSym(sym))
              .append(s": ${clazz.toString} => ")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(body).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          sb.append("}")
            .append(System.lineSeparator())
            .toString()

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.text(constructor.toString))
            .append(Format.text("("))
          for (e <- args) {
            sb.append(visitExp(e))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append(Format.text("."))
            .append(Format.text(method.getDeclaringClass.getCanonicalName + "." + method.getName))
            .append(Format.text("("))
          for (e <- args) {
            sb.append(visitExp(e))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.text(method.getDeclaringClass.getCanonicalName + "." + method.getName))
            .append(Format.text("("))
          for (e <- args) {
            sb.append(visitExp(e))
              .append(Format.text(", "))
          }
          sb.append(Format.text(")"))
            .toString()

        case Expression.GetField(field, exp, tpe, loc) =>
          Format.text("get field ") +
            Format.text(field.getName) +
            Format.text(" of ") +
            visitExp(exp)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          Format.text("put field ") +
            Format.text(field.getName) +
            Format.text(" of ") +
            visitExp(exp1) +
            Format.text(" value ") +
            visitExp(exp2)

        case Expression.GetStaticField(field, tpe, loc) => Format.text("get static field ") + Format.text(field.getName)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          Format.text("put static field ") +
            Format.text(field.getName) +
            Format.text(" value ") +
            visitExp(exp)

        case Expression.NewChannel(exp, tpe, loc) => Format.text("Channel") + Format.text(" ") + visitExp(exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) + Format.text(" <- ") + visitExp(exp2)

        case Expression.GetChannel(exp, tpe, loc) => Format.text("<- ") + visitExp(exp)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("select {")
            .append(System.lineSeparator())
          for (SelectChannelRule(sym, chan, exp) <- rules) {
            sb.append("  case ")
              .append(fmtSym(sym))
              .append(" <- ")
              .append(visitExp(chan).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
              .append(" => ")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          default match {
            case Some(exp) =>
              sb.append("  case _ => ")
                .append(System.lineSeparator())
                .append((" " * 4) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
                .append(System.lineSeparator())
            case None =>
          }
          sb.append("}")
            .toString()

        case Expression.Spawn(exp, tpe, loc) => Format.text("spawn ") + visitExp(exp)

        case Expression.Lazy(exp, tpe, loc) => Format.text("lazy ") + visitExp(exp)

        case Expression.Force(exp, tpe, loc) => Format.text("force ") + visitExp(exp)

        case Expression.HoleError(sym, tpe, loc) => Format.red("HoleError")
        case Expression.MatchError(tpe, loc) => Format.red("MatchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: FormalParam): String = {
      fmtSym(p.sym) + Format.text(": ") + Format.text(FormatType.formatType(p.tpe))
    }

    def fmtSym(sym: Symbol.VarSym): String = {
      Format.cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym): String = {
      Format.blue(sym.toString)
    }

    def fmtSym(sym: Symbol.LabelSym): String = {
      Format.magenta(sym.toString)
    }

    def fmtUnaryOp(op: UnaryOperator): String = op match {
      case UnaryOperator.LogicalNot => Format.text("not")
      case UnaryOperator.Plus => Format.text("+")
      case UnaryOperator.Minus => Format.text("-")
      case UnaryOperator.BitwiseNegate => Format.text("~~~")
    }

    def fmtBinaryOp(op: BinaryOperator): String = op match {
      case BinaryOperator.Plus => Format.text("+")
      case BinaryOperator.Minus => Format.text("-")
      case BinaryOperator.Times => Format.text("*")
      case BinaryOperator.Divide => Format.text("/")
      case BinaryOperator.Modulo => Format.text("%")
      case BinaryOperator.Exponentiate => Format.text("**")
      case BinaryOperator.Less => Format.text("<")
      case BinaryOperator.LessEqual => Format.text("<=")
      case BinaryOperator.Greater => Format.text(">")
      case BinaryOperator.GreaterEqual => Format.text(">=")
      case BinaryOperator.Equal => Format.text("==")
      case BinaryOperator.NotEqual => Format.text("!=")
      case BinaryOperator.Spaceship => Format.text("<=>")
      case BinaryOperator.LogicalAnd => Format.text("and")
      case BinaryOperator.LogicalOr => Format.text("or")
      case BinaryOperator.BitwiseAnd => Format.text("&&&")
      case BinaryOperator.BitwiseOr => Format.text("|||")
      case BinaryOperator.BitwiseXor => Format.text("^^^")
      case BinaryOperator.BitwiseLeftShift => Format.text("<<<")
      case BinaryOperator.BitwiseRightShift => Format.text(">>>")
    }
  }
}
