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

package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.fmt.Audience
import ca.uwaterloo.flix.language.fmt.FormatType.formatWellKindedType
import ca.uwaterloo.flix.util.Formatter

object PrettyPrinter {

  private implicit val audience: Audience = Audience.External

  object Lifted {

    def fmtRoot(root: Root, formatter: Formatter): String = {
      import formatter._
      val sb = new StringBuilder()
      for ((sym, defn) <- root.defs.toList.sortBy(_._1.loc)) {
        sb.append(s"${bold("def")} ${blue(sym.toString)}(")
        for (fparam <- defn.fparams) {
          sb.append(s"${fmtParam(fparam, formatter)}, ")
        }
        sb.append(") & ")
        sb.append(defn.exp.purity)
        sb.append(" = ")
        sb.append(fmtDef(defn, formatter).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
        sb.append(System.lineSeparator() + System.lineSeparator())
      }
      sb.toString()
    }

    def fmtDef(defn: Def, formatter: Formatter): String = {
      fmtExp(defn.exp, formatter)
    }

    def fmtExp(exp0: Expression, formatter: Formatter): String = {
      def visitExp(e0: Expression): String = e0 match {
        case Expression.Unit(_) => "Unit"

        case Expression.Null(tpe, _) => "null"

        case Expression.True(_) => "true"

        case Expression.False(_) => "false"

        case Expression.Char(lit, _) => "'" + lit.toString + "'"

        case Expression.Float32(lit, _) => lit.toString + "f32"

        case Expression.Float64(lit, _) => lit.toString + "f32"

        case Expression.Int8(lit, _) => lit.toString + "i8"

        case Expression.Int16(lit, _) => lit.toString + "i16"

        case Expression.Int32(lit, _) => lit.toString + "i32"

        case Expression.Int64(lit, _) => lit.toString + "i64"

        case Expression.BigInt(lit, _) => lit.toString() + "ii"

        case Expression.Str(lit, _) => "\"" + lit + "\""

        case Expression.Var(sym, tpe, loc) => fmtSym(sym, formatter)

        case Expression.Closure(sym, closureArgs, _, _) =>
          val sb = new StringBuilder()
          sb.append("Closure(")
            .append(fmtSym(sym, formatter))
            .append(", [")
          for (arg <- closureArgs) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append("])")
            .toString()

        case Expression.ApplyClo(exp, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append("(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.ApplyDef(sym, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(fmtSym(sym, formatter))
            .append("(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .append(")")
            .toString()

        case Expression.ApplyCloTail(exp, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append("*(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.ApplyDefTail(sym, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(fmtSym(sym, formatter))
            .append("*(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.ApplySelfTail(name, formals, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append("ApplySelfTail")
            .append("*(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.Unary(sop, op, exp, tpe, _, loc) =>
          fmtUnaryOp(op) + visitExp(exp)

        case Expression.Binary(sop, op, exp1, exp2, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp1))
            .append(" ")
            .append(fmtBinaryOp(op))
            .append(" ")
            .append(visitExp(exp2))
            .toString()

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(formatter.bold("if") + " (")
            .append(visitExp(exp1))
            .append(") {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp2).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("} ")
            .append(formatter.bold("else") + " {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp3).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("}")
            .toString()

        case Expression.Branch(exp, branches, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append("branch {")
            .append((" " * 2) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
          for ((sym, b) <- branches) {
            sb.append((" " * 4) + fmtSym(sym, formatter).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(":")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(b).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          sb.append("}")
            .append(System.lineSeparator())
            .toString()

        case Expression.JumpTo(sym, tpe, purity, loc) => s"jumpto ${fmtSym(sym, formatter)}"

        case Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
          val sb = new StringBuilder()
          sb.append(formatter.bold("let "))
            .append(fmtSym(sym, formatter))
            .append(" = ")
            .append(visitExp(exp1).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(";")
            .append(System.lineSeparator())
            .append(visitExp(exp2))
            .toString()

        case Expression.LetRec(varSym, _, _, exp1, exp2, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(formatter.bold("let rec"))
            .append(fmtSym(varSym, formatter))
            .append(" = ")
            .append(visitExp(exp1).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(";")
            .append(System.lineSeparator())
            .append(visitExp(exp2))
            .toString()

        case Expression.Is(sym, tag, exp, _, loc) => visitExp(exp) + " is " + tag.name

        case Expression.Tag(sym, tag, exp, tpe, _, loc) => exp match {
          case Expression.Unit(_) => tag.name
          case _ =>
            val sb = new StringBuilder()
            sb.append(tag.name)
              .append("(")
              .append(visitExp(exp))
              .append(")")
              .toString()
        }

        case Expression.Untag(sym, tag, exp, tpe, _, loc) => "Untag(" + visitExp(exp) + ")"

        case Expression.Index(exp, offset, tpe, _, loc) =>
          visitExp(exp) +
            "[" +
            offset.toString +
            "]"

        case Expression.Tuple(elms, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append("(")
          for (elm <- elms) {
            sb.append(visitExp(elm))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.RecordEmpty(tpe, loc) => "{}"

        case Expression.RecordSelect(exp, field, tpe, _, loc) =>
          visitExp(exp) +
            "." +
            field.name

        case Expression.RecordExtend(field, value, rest, tpe, _, loc) =>
          "{ " +
            field.name +
            " = " +
            visitExp(value) +
            " | " +
            visitExp(rest) +
            " }"

        case Expression.RecordRestrict(field, rest, tpe, _, loc) =>
          "{ -" +
            field.name +
            " | " +
            visitExp(rest) +
            "}"

        case Expression.ArrayLit(elms, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("[")
          for (elm <- elms) {
            sb.append(visitExp(elm))
            sb.append(",")
          }
          sb.append("]")
            .toString()

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          "[" +
            visitExp(elm) +
            ";" +
            len.toString +
            "]"

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          visitExp(base) +
            "[" +
            visitExp(index) +
            "]"

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          visitExp(base) +
            "[" +
            visitExp(index) +
            "]" +
            " = " +
            visitExp(elm)

        case Expression.ArrayLength(base, tpe, _, loc) =>
          "length" +
            "[" +
            visitExp(base) +
            "]"

        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
          visitExp(base) +
            "[" +
            visitExp(beginIndex) +
            ".." +
            visitExp(endIndex) +
            "]"

        case Expression.Ref(exp, tpe, loc) => "ref " + visitExp(exp)

        case Expression.Deref(exp, tpe, loc) => "deref " + visitExp(exp)

        case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) + " := " + visitExp(exp2)

        case Expression.Cast(exp, tpe, _, loc) =>
          visitExp(exp) +
            " as " +
            tpe.toString

        case Expression.TryCatch(exp, rules, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append("try {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("} catch {")
            .append(System.lineSeparator())
          for (CatchRule(sym, clazz, body) <- rules) {
            sb.append("  case ")
              .append(fmtSym(sym, formatter))
              .append(s": ${clazz.toString} => ")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(body).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          sb.append("}")
            .append(System.lineSeparator())
            .toString()

        case Expression.InvokeConstructor(constructor, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(constructor.toString)
            .append("(")
          for (e <- args) {
            sb.append(visitExp(e))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.InvokeMethod(method, exp, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append(".")
            .append(method.getDeclaringClass.getCanonicalName + "." + method.getName)
            .append("(")
          for (e <- args) {
            sb.append(visitExp(e))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.InvokeStaticMethod(method, args, tpe, _, loc) =>
          val sb = new StringBuilder()
          sb.append(method.getDeclaringClass.getCanonicalName + "." + method.getName)
            .append("(")
          for (e <- args) {
            sb.append(visitExp(e))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.GetField(field, exp, tpe, _, loc) =>
          "get field " +
            field.getName +
            " of " +
            visitExp(exp)

        case Expression.PutField(field, exp1, exp2, tpe, _, loc) =>
          "put field " +
            field.getName +
            " of " +
            visitExp(exp1) +
            " value " +
            visitExp(exp2)

        case Expression.GetStaticField(field, tpe, _, loc) => "get static field " + field.getName

        case Expression.PutStaticField(field, exp, tpe, _, loc) =>
          "put static field " +
            field.getName +
            " value " +
            visitExp(exp)

        case Expression.NewObject(clazz, _, _, _) =>
          "object " +
            clazz.getName +
            "{ }"

        case Expression.NewChannel(exp, tpe, loc) => "Channel" + " " + visitExp(exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) + " <- " + visitExp(exp2)

        case Expression.GetChannel(exp, tpe, loc) => "<- " + visitExp(exp)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("select {")
            .append(System.lineSeparator())
          for (SelectChannelRule(sym, chan, exp) <- rules) {
            sb.append("  case ")
              .append(fmtSym(sym, formatter))
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

        case Expression.Spawn(exp, tpe, loc) => "spawn " + visitExp(exp)

        case Expression.Lazy(exp, tpe, loc) => "lazy " + visitExp(exp)

        case Expression.Force(exp, tpe, loc) => "force " + visitExp(exp)

        case Expression.HoleError(sym, tpe, loc) => formatter.red("HoleError")
        case Expression.MatchError(tpe, loc) => formatter.red("MatchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: FormalParam, formatter: Formatter): String = {
      fmtSym(p.sym, formatter) + ": " + formatWellKindedType(p.tpe)
    }

    def fmtSym(sym: Symbol.VarSym, formatter: Formatter): String = {
      formatter.cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, formatter: Formatter): String = {
      formatter.blue(sym.toString)
    }

    def fmtSym(sym: Symbol.LabelSym, formatter: Formatter): String = {
      formatter.magenta(sym.toString)
    }

    def fmtUnaryOp(op: UnaryOperator): String = op match {
      case UnaryOperator.LogicalNot => "not"
      case UnaryOperator.Plus => "+"
      case UnaryOperator.Minus => "-"
      case UnaryOperator.BitwiseNegate => "~~~"
    }

    def fmtBinaryOp(op: BinaryOperator): String = op match {
      case BinaryOperator.Plus => "+"
      case BinaryOperator.Minus => "-"
      case BinaryOperator.Times => "*"
      case BinaryOperator.Divide => "/"
      case BinaryOperator.Remainder => "%"
      case BinaryOperator.Exponentiate => "**"
      case BinaryOperator.Less => "<"
      case BinaryOperator.LessEqual => "<="
      case BinaryOperator.Greater => ">"
      case BinaryOperator.GreaterEqual => ">="
      case BinaryOperator.Equal => "=="
      case BinaryOperator.NotEqual => "!="
      case BinaryOperator.Spaceship => "<=>"
      case BinaryOperator.LogicalAnd => "and"
      case BinaryOperator.LogicalOr => "or"
      case BinaryOperator.BitwiseAnd => "&&&"
      case BinaryOperator.BitwiseOr => "|||"
      case BinaryOperator.BitwiseXor => "^^^"
      case BinaryOperator.BitwiseLeftShift => "<<<"
      case BinaryOperator.BitwiseRightShift => ">>>"
    }
  }
}
