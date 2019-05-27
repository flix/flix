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

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.tc.Show.ShowableSyntax
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object PrettyPrinter {

  object Simplified {

    def fmtRoot(root: SimplifiedAst.Root): VirtualTerminal = {
      val vt = new VirtualTerminal()
      for ((sym, defn) <- root.defs.toList.sortBy(_._1.loc)) {
        vt << Bold("def") << " " << Blue(sym.toString) << "("
        for (fparam <- defn.fparams) {
          fmtParam(fparam, vt)
          if (fparam != defn.fparams.last) vt << ", "
        }
        vt << s"): ${defn.tpe.typeArguments.last.show} = "
        vt << Indent << NewLine
        fmtExp(defn, vt)
        vt << Dedent << NewLine << NewLine
      }
      vt
    }

    def fmtExp(defn: SimplifiedAst.Def, vt: VirtualTerminal): Unit = {
      fmtExp(defn.exp, vt)
    }

    def fmtExp(exp0: SimplifiedAst.Expression, vt: VirtualTerminal): Unit = {
      def visitExp(e0: SimplifiedAst.Expression): Unit = e0 match {
        case SimplifiedAst.Expression.Unit => vt.text("Unit")
        case SimplifiedAst.Expression.True => vt.text("true")
        case SimplifiedAst.Expression.False => vt.text("false")
        case SimplifiedAst.Expression.Char(lit) => vt.text("'").text(lit.toString).text("'")
        case SimplifiedAst.Expression.Float32(lit) => vt.text(lit.toString).text("f32")
        case SimplifiedAst.Expression.Float64(lit) => vt.text(lit.toString).text("f32")
        case SimplifiedAst.Expression.Int8(lit) => vt.text(lit.toString).text("i8")
        case SimplifiedAst.Expression.Int16(lit) => vt.text(lit.toString).text("i16")
        case SimplifiedAst.Expression.Int32(lit) => vt.text(lit.toString).text("i32")
        case SimplifiedAst.Expression.Int64(lit) => vt.text(lit.toString).text("i64")
        case SimplifiedAst.Expression.BigInt(lit) => vt.text(lit.toString()).text("ii")
        case SimplifiedAst.Expression.Str(lit) => vt.text("\"").text(lit).text("\"")

        case SimplifiedAst.Expression.Var(sym, tpe, loc) => fmtSym(sym, vt)

        case SimplifiedAst.Expression.Def(sym, tpe, loc) => fmtSym(sym, vt)

        case SimplifiedAst.Expression.Eff(sym, tpe, loc) => fmtSym(sym, vt)

        case SimplifiedAst.Expression.Lambda(fparams, body, tpe, loc) =>
          vt.text("(")
          for (fparam <- fparams) {
            vt.text(fparam.sym.toString)
            if (fparam != fparams.last) vt.text(", ")
          }
          vt.text(")")
          vt.text(" -> ")
          visitExp(body)

        case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
          vt.text("LambdaClosure(")
          visitExp(exp)
          vt.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, vt)
            if (freeVar != freeVars.last) vt.text(", ")
          }
          vt.text("])")

        case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
          vt.text("Closure(")
          fmtSym(sym, vt)
          vt.text(", [")
          for (freeVar <- freeVars) {
            fmtSym(freeVar.sym, vt)
            if (freeVar != freeVars.last) vt.text(", ")
          }
          vt.text("])")

        case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.ApplyDef(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")
          vt.text(")")

        case SimplifiedAst.Expression.ApplyEff(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.ApplySelfTail(name, formals, args, tpe, loc) =>
          vt.text("ApplySelfTail")
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            if (arg != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
          fmtUnaryOp(op, vt)
          visitExp(exp)

        case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtBinaryOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
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

        case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
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

        case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
          vt << "jumpto" << " "
          fmtSym(sym, vt)

        case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          vt << Indent << NewLine
          visitExp(exp1)
          vt << Dedent
          vt << ";" << NewLine
          visitExp(exp2)

        case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          visitExp(exp1)
          vt << ";" << NewLine
          visitExp(exp2)

        case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
          visitExp(exp)
          vt.text(" is ")
          vt.text(tag)

        case SimplifiedAst.Expression.Tag(sym, tag, exp, tpe, loc) => exp match {
          case SimplifiedAst.Expression.Unit => vt.text(tag)
          case _ =>
            vt.text(tag).text("(")
            visitExp(exp)
            vt.text(")")
        }

        case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
          vt.text("Untag(")
          visitExp(exp)
          vt.text(")")

        case SimplifiedAst.Expression.Index(exp, offset, tpe, loc) =>
          visitExp(exp)
          vt.text("[")
          vt.text(offset.toString)
          vt.text("]")

        case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
          vt.text("(")
          for (elm <- elms) {
            visitExp(elm)
            if (elm != elms.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
          vt.text("{}")

        case SimplifiedAst.Expression.RecordSelect(exp, label, tpe, loc) =>
          visitExp(exp)
          vt.text(".")
          vt.text(label)

        case SimplifiedAst.Expression.RecordExtend(label, value, rest, tpe, loc) =>
          vt.text("{ ")
          vt.text(label)
          vt.text(" = ")
          visitExp(value)
          vt.text(" | ")
          visitExp(rest)
          vt.text(" }")

        case SimplifiedAst.Expression.RecordRestrict(label, rest, tpe, loc) =>
          vt.text("{ -")
          vt.text(label)
          vt.text(" | ")
          visitExp(rest)
          vt.text("}")

        case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
          vt.text("[")
          for (elm <- elms) {
            visitExp(elm)
            vt.text(",")
          }
          vt.text("]")

        case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
          vt.text("[")
          visitExp(elm)
          vt.text(";")
          vt.text(len.toString)
          vt.text("]")

        case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(index)
          vt.text("]")

        case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(index)
          vt.text("]")
          vt.text(" = ")
          visitExp(elm)

        case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
          vt.text("length")
          vt.text("[")
          visitExp(base)
          vt.text("]")

        case SimplifiedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(beginIndex)
          vt.text("..")
          visitExp(endIndex)
          vt.text("]")

        case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
          vt.text("ref ")
          visitExp(exp)

        case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
          vt.text("deref ")
          visitExp(exp)

        case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" := ")
          visitExp(exp2)

        case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe, loc) =>
          vt << "do" << Indent << NewLine
          visitExp(exp)
          vt.text("with {")
          for (SimplifiedAst.HandlerBinding(sym, handler) <- bindings) {
            vt << "eff "
            fmtSym(sym, vt)
            vt << " = "
            visitExp(handler)
          }
          vt.text("}")
          vt << Dedent << NewLine

        case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
          vt.text("∃(")
          fmtParam(fparam, vt)
          vt.text("). ")
          visitExp(exp)

        case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
          vt.text("∀(")
          fmtParam(fparam, vt)
          vt.text("). ")
          visitExp(exp)

        case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
          vt << "try {" << Indent << NewLine
          visitExp(exp)
          vt << Dedent << NewLine
          vt << "} catch {" << Indent << NewLine
          for (SimplifiedAst.CatchRule(sym, clazz, body) <- rules) {
            vt << "case "
            fmtSym(sym, vt)
            vt << ": " << clazz.toString << " => "
            visitExp(body)
          }
          vt << Dedent << NewLine << "}" << NewLine

        case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
          vt.text(constructor.toString)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            if (e != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.NativeField(field, tpe, loc) => vt << field.toString

        case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            if (e != args.last) vt.text(", ")
          }
          vt.text(")")

        case SimplifiedAst.Expression.NewChannel(exp, tpe, loc) =>
          vt.text("Channel")
          vt.text(" ")
          visitExp(exp)

        case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" <- ")
          visitExp(exp2)

        case SimplifiedAst.Expression.GetChannel(exp, tpe, loc) =>
          vt.text("<- ")
          visitExp(exp)

        case SimplifiedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
          vt << "select {" << Indent << NewLine
          for (SimplifiedAst.SelectChannelRule(sym, chan, exp) <- rules) {
            vt << "case "
            fmtSym(sym, vt)
            vt << " <- "
            visitExp(chan)
            vt << " => "
            visitExp(exp)
            vt << NewLine
          }
          default match {
            case Some(exp) =>
              vt << "case _ => "
              visitExp(exp)
              vt << NewLine
            case None =>
          }
          vt << Dedent << "}"

        case SimplifiedAst.Expression.Spawn(exp, tpe, loc) =>
          vt.text("spawn ")
          visitExp(exp)

        case SimplifiedAst.Expression.Sleep(exp, tpe, loc) =>
          vt.text("sleep ")
          visitExp(exp)

        case SimplifiedAst.Expression.FixpointConstraint(c, tpe, loc) =>
          vt.text("<constraint>")

        case SimplifiedAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text("<+>")
          visitExp(exp2)

        case SimplifiedAst.Expression.FixpointSolve(exp, tpe, loc) =>
          vt.text("solve ")
          visitExp(exp)

        case SimplifiedAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
          vt.text("project ")
          fmtPredicateWithParam(pred, vt)
          vt.text(" ")
          visitExp(exp)

        case SimplifiedAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text("|=")
          visitExp(exp2)

        case SimplifiedAst.Expression.UserError(tpe, loc) => vt << Red("UserError")
        case SimplifiedAst.Expression.HoleError(sym, tpe, loc) => Red("HoleError")
        case SimplifiedAst.Expression.MatchError(tpe, loc) => vt << Red("MatchError")
        case SimplifiedAst.Expression.SwitchError(tpe, loc) => vt << Red("SwitchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: SimplifiedAst.FormalParam, vt: VirtualTerminal): Unit = {
      fmtSym(p.sym, vt)
      vt.text(": ")
      vt.text(p.tpe.show)
    }

    def fmtPredicateWithParam(p: SimplifiedAst.PredicateWithParam, vt: VirtualTerminal): Unit = p match {
      case SimplifiedAst.PredicateWithParam(sym, exp) =>
        vt.text(sym.toString)
        vt.text("<")
        fmtExp(exp, vt)
        vt.text(">")
    }

    def fmtSym(sym: Symbol.VarSym, vt: VirtualTerminal): Unit = {
      vt << Cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, vt: VirtualTerminal): Unit = {
      vt << Blue(sym.toString)
    }

    def fmtSym(sym: Symbol.EffSym, vt: VirtualTerminal): Unit = {
      vt << Yellow(sym.toString)
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

  object Typed {
    val moreTypes = true

    def fmtRoot(root: TypedAst.Root): VirtualTerminal = {
      val vt = new VirtualTerminal()
      for ((sym, defn) <- root.defs.toList.sortBy(_._1.loc)) {
        vt << Bold("def") << " " << Blue(sym.toString)
        if (defn.tparams.nonEmpty) vt << "["
        for (tparam <- defn.tparams) {
          fmtTParam(tparam, vt)
        }
        if (defn.tparams.nonEmpty) vt << "]"
        vt << "("
        for (fparam <- defn.fparams) {
          fmtParam(fparam, vt)
          if (fparam != defn.fparams.last) vt << ", "
        }
        vt << s"): "
        if (defn.tpe.isArrow) {
          vt << defn.tpe.typeArguments.last.show
        }
        vt << " = "
        vt << Indent << NewLine
        fmtExp(defn, vt)
        vt << Dedent << NewLine << NewLine
      }
      vt
    }

    def fmtExp(defn: TypedAst.Def, vt: VirtualTerminal): Unit = {
      fmtExp(defn.exp, vt)
    }

    def fmtExp(exp0: TypedAst.Expression, vt: VirtualTerminal): Unit = {
      def visitExp(e0: TypedAst.Expression): Unit = e0 match {
        case TypedAst.Expression.Unit(loc) => vt.text("Unit")
        case TypedAst.Expression.True(loc) => vt.text("true")
        case TypedAst.Expression.False(loc) => vt.text("false")
        case TypedAst.Expression.Char(lit, loc) => vt.text("'").text(lit.toString).text("'")
        case TypedAst.Expression.Float32(lit, loc) => vt.text(lit.toString).text("f32")
        case TypedAst.Expression.Float64(lit, loc) => vt.text(lit.toString).text("f32")
        case TypedAst.Expression.Int8(lit, loc) => vt.text(lit.toString).text("i8")
        case TypedAst.Expression.Int16(lit, loc) => vt.text(lit.toString).text("i16")
        case TypedAst.Expression.Int32(lit, loc) => vt.text(lit.toString).text("i32")
        case TypedAst.Expression.Int64(lit, loc) => vt.text(lit.toString).text("i64")
        case TypedAst.Expression.BigInt(lit, loc) => vt.text(lit.toString()).text("ii")
        case TypedAst.Expression.Str(lit, loc) => vt.text("\"").text(lit).text("\"")

        case TypedAst.Expression.Var(sym, tpe, eff, loc) => fmtSym(sym, vt)

        case TypedAst.Expression.Def(sym, tpe, eff, loc) => fmtSym(sym, vt)

        case TypedAst.Expression.Eff(sym, tpe, eff, loc) => fmtSym(sym, vt)

        case TypedAst.Expression.Lambda(fparam, body, tpe, eff, loc) =>
          vt.text("(")
          vt.text(fparam.sym.toString)
          if (moreTypes) vt << ": " << fparam.tpe.show
          vt << ")" << " -> " << Indent << NewLine << "("
          visitExp(body)
          vt << ")"
          if (moreTypes) vt << "[" << tpe.show << "]"
          vt << Dedent << NewLine

        case TypedAst.Expression.Apply(exp, arg, tpe, eff, loc) =>
          vt << "("
          visitExp(exp)
          vt.text(") (")
          visitExp(arg)
          vt.text(")")

        case TypedAst.Expression.Unary(op, exp, tpe, eff, loc) =>
          fmtUnaryOp(op, vt)
          visitExp(exp)

        case TypedAst.Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtBinaryOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
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

        case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          vt << Indent << NewLine
          visitExp(exp1)
          vt << Dedent
          vt << ";" << NewLine
          visitExp(exp2)

        case TypedAst.Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
          vt << Bold("let") << " "
          fmtSym(sym, vt)
          vt.text(" = ")
          visitExp(exp1)
          vt << ";" << NewLine
          visitExp(exp2)

        case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => exp match {
          case TypedAst.Expression.Unit(_) => vt.text(tag)
          case _ =>
            vt.text(tag).text("(")
            visitExp(exp)
            vt.text(")")
        }

        case TypedAst.Expression.Tuple(elms, tpe, eff, loc) =>
          vt.text("(")
          for (elm <- elms) {
            visitExp(elm)
            if (elm != elms.last) vt.text(", ")
          }
          vt.text(")")

        case TypedAst.Expression.RecordEmpty(tpe, eff, loc) =>
          vt.text("{}")

        case TypedAst.Expression.RecordSelect(exp, label, tpe, eff, loc) =>
          visitExp(exp)
          vt.text(".")
          vt.text(label)

        case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
          vt.text("{ ")
          vt.text(label)
          vt.text(" = ")
          visitExp(value)
          vt.text(" | ")
          visitExp(rest)
          vt.text(" }")

        case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) =>
          vt.text("{ -")
          vt.text(label)
          vt.text(" | ")
          visitExp(rest)
          vt.text("}")

        case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) =>
          vt.text("[")
          for (elm <- elms) {
            visitExp(elm)
            vt.text(",")
          }
          vt.text("]")

        case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) =>
          vt.text("[")
          visitExp(elm)
          vt.text(";")
          vt.text(len.toString)
          vt.text("]")

        case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(index)
          vt.text("]")

        case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(index)
          vt.text("]")
          vt.text(" = ")
          visitExp(elm)

        case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) =>
          vt.text("length")
          vt.text("[")
          visitExp(base)
          vt.text("]")

        case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) =>
          visitExp(base)
          vt.text("[")
          visitExp(beginIndex)
          vt.text("..")
          visitExp(endIndex)
          vt.text("]")

        case TypedAst.Expression.Ref(exp, tpe, eff, loc) =>
          vt.text("ref ")
          visitExp(exp)

        case TypedAst.Expression.Deref(exp, tpe, eff,  loc) =>
          vt.text("deref ")
          visitExp(exp)

        case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
          visitExp(exp1)
          vt.text(" := ")
          visitExp(exp2)

        case TypedAst.Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
          vt << "do" << Indent << NewLine
          visitExp(exp)
          vt.text("with {")
          for (TypedAst.HandlerBinding(sym, handler) <- bindings) {
            vt << "eff "
            fmtSym(sym, vt)
            vt << " = "
            visitExp(handler)
          }
          vt.text("}")
          vt << Dedent << NewLine

        case TypedAst.Expression.Existential(fparam, exp, eff, loc) =>
          vt.text("∃(")
          fmtParam(fparam, vt)
          vt.text("). ")
          visitExp(exp)

        case TypedAst.Expression.Universal(fparam, exp, eff, loc) =>
          vt.text("∀(")
          fmtParam(fparam, vt)
          vt.text("). ")
          visitExp(exp)

        case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
          vt << "try {" << Indent << NewLine
          visitExp(exp)
          vt << Dedent << NewLine
          vt << "} catch {" << Indent << NewLine
          for (TypedAst.CatchRule(sym, clazz, body) <- rules) {
            vt << "case "
            fmtSym(sym, vt)
            vt << ": " << clazz.toString << " => "
            visitExp(body)
          }
          vt << Dedent << NewLine << "}" << NewLine

        case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
          vt.text(constructor.toString)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            if (e != args.last) vt.text(", ")
          }
          vt.text(")")

        case TypedAst.Expression.NativeField(field, tpe, eff, loc) => vt << field.toString

        case TypedAst.Expression.NativeMethod(method, args, tpe, eff, loc) =>
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            if (e != args.last) vt.text(", ")
          }
          vt.text(")")

        case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) =>
          vt.text("Channel")
          vt.text(" ")
          visitExp(exp)

        case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
          visitExp(exp1)
          vt.text(" <- ")
          visitExp(exp2)

        case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) =>
          vt.text("<- ")
          visitExp(exp)

        case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) =>
          vt << "select {" << Indent << NewLine
          for (TypedAst.SelectChannelRule(sym, chan, exp) <- rules) {
            vt << "case "
            fmtSym(sym, vt)
            vt << " <- "
            visitExp(chan)
            vt << " => "
            visitExp(exp)
            vt << NewLine
          }
          default match {
            case Some(exp) =>
              vt << "case _ => "
              visitExp(exp)
              vt << NewLine
            case None =>
          }
          vt << Dedent << "}"

        case TypedAst.Expression.Spawn(exp, tpe, eff, loc) =>
          vt.text("spawn ")
          visitExp(exp)

        case TypedAst.Expression.Sleep(exp, tpe, eff, loc) =>
          vt.text("sleep ")
          visitExp(exp)

        case TypedAst.Expression.FixpointConstraint(c, tpe, eff, loc) =>
          vt.text("<constraint>")

        case TypedAst.Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
          visitExp(exp1)
          vt.text("<+>")
          visitExp(exp2)

        case TypedAst.Expression.FixpointSolve(exp, tpe, eff, loc) =>
          vt.text("solve ")
          visitExp(exp)

        case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
          vt.text("project ")
          fmtPredicateWithParam(pred, vt)
          vt.text(" ")
          visitExp(exp)

        case TypedAst.Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
          visitExp(exp1)
          vt.text("|=")
          visitExp(exp2)

        case TypedAst.Expression.UserError(tpe, eff, loc) => vt << Red("UserError")

        case TypedAst.Expression.ApplyWithKont(exp1, exp2, exp3, tpe, eff, loc) =>
          visitExp(exp1)
          vt << "(" << Indent << NewLine
          visitExp(exp2)
          vt << NewLine << ", " << NewLine
          visitExp(exp3)
          vt << Dedent << NewLine << ")"

        case TypedAst.Expression.LambdaWithKont(fparam1, fparam2, exp, tpe, eff, loc) =>
          vt.text("(")
          vt.text(fparam1.sym.toString)
          if (moreTypes) vt << ": " << fparam1.tpe.show
          vt.text(", ")
          vt.text(fparam2.sym.toString)
          if (moreTypes) vt << ": " << fparam2.tpe.show
          vt.text(")")
          vt.text(" -> ")
          vt << Indent << NewLine << "("
          visitExp(exp)
          vt << ")"
          if (moreTypes) vt << "[" << tpe.show << "]"
          vt << Dedent << NewLine

        case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.CPSReset(exp, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.CPSShift(exp, tpe, eff, loc) =>
          vt << "shift("
          visitExp(exp)
          vt << ")"
          if (moreTypes) vt <<"[" << tpe.show << "]"
        case TypedAst.Expression.Cast(exp, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.Hole(sym, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.Switch(rules, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.VectorLength(base, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.VectorStore(base, index, elm, tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.Wild(tpe, eff, loc) => vt << "?"
        case TypedAst.Expression.SwitchError(tpe, eff, loc) => vt << Red("switch error") << ": [" << tpe.show << "]"
      }

      visitExp(exp0)
    }

    def fmtParam(p: TypedAst.FormalParam, vt: VirtualTerminal): Unit = {
      fmtSym(p.sym, vt)
      vt.text(": ")
      vt.text(p.tpe.show)
    }

    def fmtTParam(p: TypedAst.TypeParam, vt: VirtualTerminal): Unit = {
      vt.text(p.tpe.show)
    }

    def fmtPredicateWithParam(p: TypedAst.PredicateWithParam, vt: VirtualTerminal): Unit = p match {
      case TypedAst.PredicateWithParam(sym, exp) =>
        vt.text(sym.toString)
        vt.text("<")
        fmtExp(exp, vt)
        vt.text(">")
    }

    def fmtSym(sym: Symbol.VarSym, vt: VirtualTerminal): Unit = {
      vt << Cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, vt: VirtualTerminal): Unit = {
      vt << Blue(sym.toString)
    }

    def fmtSym(sym: Symbol.EffSym, vt: VirtualTerminal): Unit = {
      vt << Yellow(sym.toString)
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
