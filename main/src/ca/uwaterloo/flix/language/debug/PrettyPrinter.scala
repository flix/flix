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


import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object PrettyPrinter {

  private implicit val audience: Audience = Audience.External

  object Lifted {

    import ca.uwaterloo.flix.language.ast.LiftedAst._

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
        fmtDef(defn, vt)
        vt << Dedent << NewLine << NewLine
      }
      vt
    }

    def fmtDef(defn: Def, vt: VirtualTerminal): Unit = {
      fmtExp(defn.exp, vt)
    }

    def fmtExp(exp0: Expression, vt: VirtualTerminal): Unit = {
      def visitExp(e0: Expression): Unit = e0 match {
        case Expression.Unit(_) => vt.text("Unit")

        case Expression.Null(tpe, _) => vt.text("null")

        case Expression.True(_) => vt.text("true")

        case Expression.False(_) => vt.text("false")

        case Expression.Char(lit, _) => vt.text("'").text(lit.toString).text("'")

        case Expression.Float32(lit, _) => vt.text(lit.toString).text("f32")

        case Expression.Float64(lit, _) => vt.text(lit.toString).text("f32")

        case Expression.Int8(lit, _) => vt.text(lit.toString).text("i8")

        case Expression.Int16(lit, _) => vt.text(lit.toString).text("i16")

        case Expression.Int32(lit, _) => vt.text(lit.toString).text("i32")

        case Expression.Int64(lit, _) => vt.text(lit.toString).text("i64")

        case Expression.BigInt(lit, _) => vt.text(lit.toString()).text("ii")

        case Expression.Str(lit, _) => vt.text("\"").text(lit).text("\"")

        case Expression.Var(sym, tpe, loc) => fmtSym(sym, vt)

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

        case Expression.Is(sym, tag, exp, loc) =>
          visitExp(exp)
          vt.text(" is ")
          vt.text(tag.name)

        case Expression.Tag(sym, tag, exp, tpe, loc) => exp match {
          case Expression.Unit(_) => vt.text(tag.name)
          case _ =>
            vt.text(tag.name).text("(")
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

        case Expression.RecordEmpty(tpe, loc) =>
          vt.text("{}")

        case Expression.RecordSelect(exp, field, tpe, loc) =>
          visitExp(exp)
          vt.text(".")
          vt.text(field.name)

        case Expression.RecordExtend(field, value, rest, tpe, loc) =>
          vt.text("{ ")
          vt.text(field.name)
          vt.text(" = ")
          visitExp(value)
          vt.text(" | ")
          visitExp(rest)
          vt.text(" }")

        case Expression.RecordRestrict(field, rest, tpe, loc) =>
          vt.text("{ -")
          vt.text(field.name)
          vt.text(" | ")
          visitExp(rest)
          vt.text("}")

        case Expression.ArrayLit(elms, tpe, loc) =>
          vt.text("[")
          for (elm <- elms) {
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

        case Expression.Cast(exp, tpe, loc) =>
          visitExp(exp)
          vt.text(" as ")
          vt.text(tpe.toString)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          vt << "try {" << Indent << NewLine
          visitExp(exp)
          vt << Dedent << NewLine
          vt << "} catch {" << Indent << NewLine
          for (CatchRule(sym, clazz, body) <- rules) {
            vt << "case "
            fmtSym(sym, vt)
            vt << ": " << clazz.toString << " => "
            visitExp(body)
          }
          vt << Dedent << NewLine << "}" << NewLine

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          vt.text(constructor.toString)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text(".")
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.GetField(field, exp, tpe, loc) =>
          vt.text("get field ")
          vt.text(field.getName)
          vt.text(" of ")
          visitExp(exp)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          vt.text("put field ")
          vt.text(field.getName)
          vt.text(" of ")
          visitExp(exp1)
          vt.text(" value ")
          visitExp(exp2)

        case Expression.GetStaticField(field, tpe, loc) =>
          vt.text("get static field ")
          vt.text(field.getName)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          vt.text("put static field ")
          vt.text(field.getName)
          vt.text(" value ")
          visitExp(exp)

        case Expression.NewChannel(exp, tpe, loc) =>
          vt.text("Channel")
          vt.text(" ")
          visitExp(exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" <- ")
          visitExp(exp2)

        case Expression.GetChannel(exp, tpe, loc) =>
          vt.text("<- ")
          visitExp(exp)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          vt << "select {" << Indent << NewLine
          for (SelectChannelRule(sym, chan, exp) <- rules) {
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

        case Expression.Spawn(exp, tpe, loc) =>
          vt.text("spawn ")
          visitExp(exp)

        case Expression.Lazy(exp, tpe, loc) =>
          vt.text("lazy ")
          visitExp(exp)

        case Expression.Force(exp, tpe, loc) =>
          vt.text("force ")
          visitExp(exp)

        case Expression.HoleError(sym, tpe, loc) => Red("HoleError")
        case Expression.MatchError(tpe, loc) => vt << Red("MatchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: FormalParam, vt: VirtualTerminal): Unit = {
      fmtSym(p.sym, vt)
      vt.text(": ")
      vt.text(FormatType.formatType(p.tpe))
    }

    def fmtSym(sym: Symbol.VarSym, vt: VirtualTerminal): Unit = {
      vt << Cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, vt: VirtualTerminal): Unit = {
      vt << Blue(sym.toString)
    }

    def fmtSym(sym: Symbol.LabelSym, vt: VirtualTerminal): Unit = {
      vt << Magenta(sym.toString)
    }

    def fmtUnaryOp(op: UnaryOperator, vt: VirtualTerminal): Unit = op match {
      case UnaryOperator.LogicalNot => vt.text("not")
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
      case BinaryOperator.Spaceship => vt.text("<=>")
      case BinaryOperator.LogicalAnd => vt.text("and")
      case BinaryOperator.LogicalOr => vt.text("or")
      case BinaryOperator.BitwiseAnd => vt.text("&&&")
      case BinaryOperator.BitwiseOr => vt.text("|||")
      case BinaryOperator.BitwiseXor => vt.text("^^^")
      case BinaryOperator.BitwiseLeftShift => vt.text("<<<")
      case BinaryOperator.BitwiseRightShift => vt.text(">>>")
    }

  }

  object Erased {

    import ErasedAst._

    def fmtRoot(root: Root): VirtualTerminal = {
      val vt = new VirtualTerminal()
      for ((sym, defn) <- root.functions.toList.sortBy(_._1.loc)) {
        vt << Bold("def") << " " << Blue(sym.toString) << "("
        for (fparam <- defn.formals) {
          fmtParam(fparam, vt)
          vt << ", "
        }
        vt << "): "
        fmtTpe(RType.squeezeFunction(RType.squeezeReference(defn.tpe)).result, vt)
        vt << " = "
        vt << Indent << NewLine
        fmtDef(defn, vt)
        vt << Dedent << NewLine << NewLine
      }
      vt
    }

    def fmtDef[T <: PType](defn: Def[T], vt: VirtualTerminal): Unit = {
      fmtExp(defn.exp, vt)
    }

    def fmtExp[T <: PType](exp0: Expression[T], vt: VirtualTerminal): Unit = {
      def visitExp[T0 <: PType](exp: Expression[T0]): Unit = fmtExp(exp, vt)

      exp0 match {
        case Expression.Unit(_) => vt.text("Unit")

        case Expression.Null(_, _) => vt.text("null")

        case Expression.True(_) => vt.text("true")

        case Expression.False(_) => vt.text("false")

        case Expression.Char(lit, _) => vt.text("'").text(lit.toString).text("'")

        case Expression.Float32(lit, _) => vt.text(lit.toString).text("f32")

        case Expression.Float64(lit, _) => vt.text(lit.toString).text("f32")

        case Expression.Int8(lit, _) => vt.text(lit.toString).text("i8")

        case Expression.Int16(lit, _) => vt.text(lit.toString).text("i16")

        case Expression.Int32(lit, _) => vt.text(lit.toString).text("i32")

        case Expression.Int64(lit, _) => vt.text(lit.toString).text("i64")

        case Expression.BigInt(lit, _) => vt.text(lit.toString()).text("ii")

        case Expression.Str(lit, _) => vt.text("\"").text(lit).text("\"")

        case Expression.Var(sym, tpe, loc) => fmtSym(sym, vt)

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

        case Expression.ApplyDef(sym, args, fnTpe, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")
          vt.text(")")

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplyDefTail(sym, args, fnTpe, tpe, loc) =>
          fmtSym(sym, vt)
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.ApplySelfTail(name, formals, args, fnTpe, tpe, loc) =>
          vt.text("ApplySelfTail")
          vt.text("*(")
          for (arg <- args) {
            visitExp(arg)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.BoolNot(exp, _, _) =>
          vt.text("not")
          visitExp(exp)

        case Expression.Float32Neg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.Float64Neg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.Int8Neg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.Int8Not(exp, _, _) =>
          vt.text("~~~")
          visitExp(exp)

        case Expression.Int16Neg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.Int16Not(exp, _, _) =>
          vt.text("~~~")
          visitExp(exp)

        case Expression.Int32Neg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.Int32Not(exp, _, _) =>
          vt.text("~~~")
          visitExp(exp)

        case Expression.Int64Neg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.Int64Not(exp, _, _) =>
          vt.text("~~~")
          visitExp(exp)

        case Expression.BigIntNeg(exp, _, _) =>
          vt.text("-")
          visitExp(exp)

        case Expression.BigIntNot(exp, _, _) =>
          vt.text("~~~")
          visitExp(exp)

        case Expression.ObjEqNull(exp, _, _) =>
          vt.text("isNull?(")
          visitExp(exp)
          vt.text(")")

        case Expression.ObjNeqNull(exp, _, _) =>
          vt.text("isNotNull?(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoolLogicalOp(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.BoolEquality(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.CharComparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Float32Arithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Float32Comparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Float64Arithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Float64Comparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int8Arithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int16Arithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int32Arithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int64Arithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.BigIntArithmetic(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int8Bitwise(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int16Bitwise(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int32Bitwise(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int64Bitwise(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.BigIntBitwise(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int8Comparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int16Comparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int32Comparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.Int64Comparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.BigIntComparison(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
          vt.text(" ")
          visitExp(exp2)

        case Expression.StringConcat(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" concat ")
          visitExp(exp2)

        case Expression.StringEquality(op, exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" ")
          fmtOp(op, vt)
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

        case Expression.Is(sym, tag, exp, loc) =>
          visitExp(exp)
          vt.text(" is ")
          vt.text(tag.name)

        case Expression.Tag(sym, tag, exp, tpe, loc) => exp match {
          case Expression.Unit(_) => vt.text(tag.name)
          case _ =>
            vt.text(tag.name).text("(")
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

        case Expression.RecordEmpty(tpe, loc) =>
          vt.text("{}")

        case Expression.RecordSelect(exp, field, tpe, loc) =>
          visitExp(exp)
          vt.text(".")
          vt.text(field.name)

        case Expression.RecordExtend(field, value, rest, tpe, loc) =>
          vt.text("{ ")
          vt.text(field.name)
          vt.text(" = ")
          visitExp(value)
          vt.text(" | ")
          visitExp(rest)
          vt.text(" }")

        case Expression.RecordRestrict(field, rest, tpe, loc) =>
          vt.text("{ -")
          vt.text(field.name)
          vt.text(" | ")
          visitExp(rest)
          vt.text("}")

        case Expression.ArrayLit(elms, tpe, loc) =>
          vt.text("[")
          for (elm <- elms) {
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

        case Expression.Cast(exp, tpe, loc) =>
          visitExp(exp)
          vt.text(" as ")
          fmtTpe(tpe, vt)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          vt << "try {" << Indent << NewLine
          visitExp(exp)
          vt << Dedent << NewLine
          vt << "} catch {" << Indent << NewLine
          for (CatchRule(sym, clazz, body) <- rules) {
            vt << "case "
            fmtSym(sym, vt)
            vt << ": " << clazz.toString << " => "
            visitExp(body)
          }
          vt << Dedent << NewLine << "}" << NewLine

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          vt.text(constructor.toString)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          visitExp(exp)
          vt.text(".")
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          vt.text(method.getDeclaringClass.getCanonicalName + "." + method.getName)
          vt.text("(")
          for (e <- args) {
            visitExp(e)
            vt.text(", ")
          }
          vt.text(")")

        case Expression.GetField(field, exp, tpe, loc) =>
          vt.text("get field ")
          vt.text(field.getName)
          vt.text(" of ")
          visitExp(exp)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          vt.text("put field ")
          vt.text(field.getName)
          vt.text(" of ")
          visitExp(exp1)
          vt.text(" value ")
          visitExp(exp2)

        case Expression.GetStaticField(field, tpe, loc) =>
          vt.text("get static field ")
          vt.text(field.getName)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          vt.text("put static field ")
          vt.text(field.getName)
          vt.text(" value ")
          visitExp(exp)

        case Expression.NewChannel(exp, tpe, loc) =>
          vt.text("Channel")
          vt.text(" ")
          visitExp(exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          visitExp(exp1)
          vt.text(" <- ")
          visitExp(exp2)

        case Expression.GetChannel(exp, tpe, loc) =>
          vt.text("<- ")
          visitExp(exp)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          vt << "select {" << Indent << NewLine
          for (SelectChannelRule(sym, chan, exp) <- rules) {
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

        case Expression.Spawn(exp, tpe, loc) =>
          vt.text("spawn ")
          visitExp(exp)

        case Expression.Lazy(exp, tpe, loc) =>
          vt.text("lazy ")
          visitExp(exp)

        case Expression.Force(exp, tpe, loc) =>
          vt.text("force ")
          visitExp(exp)

        case Expression.HoleError(sym, tpe, loc) => Red("HoleError")
        case Expression.MatchError(tpe, loc) => vt << Red("MatchError")

        case Expression.BoxInt8(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoxInt16(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoxInt32(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoxInt64(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoxChar(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoxFloat32(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.BoxFloat64(exp, loc) =>
          vt.text("box(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxInt8(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxInt16(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxInt32(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxInt64(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxChar(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxFloat32(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")

        case Expression.UnboxFloat64(exp, loc) =>
          vt.text("unbox(")
          visitExp(exp)
          vt.text(")")
      }
    }

    def fmtTpe[T <: PType](tpe: RType[T], vt: VirtualTerminal): Unit = tpe match {
        case RBool => vt.text("Bool")
        case RInt8 => vt.text("Int8")
        case RInt16 => vt.text("Int16")
        case RInt32 => vt.text("Int32")
        case RInt64 => vt.text("Int64")
        case RChar => vt.text("Char")
        case RFloat32 => vt.text("Float32")
        case RFloat64 => vt.text("Float64")
        case RReference(referenceType) => fmtRefTpe(referenceType, vt)
      }

    def fmtRefTpe[T <: PRefType](tpe: RRefType[T], vt: VirtualTerminal): Unit = {
      def visitComp[T0 <: PType](preFix: String, tpe: RType[T0]): Unit = {vt.text(preFix+"["); fmtTpe(tpe, vt); vt.text("]")}
      tpe match {
        case RBoxedBool =>  vt.text("BoxedBool")
        case RBoxedInt8 => vt.text("BoxedInt8")
        case RBoxedInt16 => vt.text("BoxedInt16")
        case RBoxedInt32 => vt.text("BoxedInt32")
        case RBoxedInt64 => vt.text("BoxedInt64")
        case RBoxedChar => vt.text("BoxedChar")
        case RBoxedFloat32 => vt.text("BoxedFloat32")
        case RBoxedFloat64 => vt.text("BoxedFloat64")
        case RUnit => vt.text("Unit")
        case RArray(tpe) => visitComp("Array", tpe)
        case RChannel(tpe) => visitComp("Channel", tpe)
        case RLazy(tpe) => visitComp("Lazy", tpe)
        case RRef(tpe) => visitComp("Ref", tpe)
        case RVar(id) => vt.text("TYPEVAR")
        case RTuple(elms) => vt.text("Tuple["); vt.text("..."); vt.text("]")
        case REnum(sym, args) => vt.text(sym.toString+"["); vt.text("..."); vt.text("]")
        case RBigInt => vt.text("BigInt")
        case RStr => vt.text("String")
        case RArrow(args, result) =>
          vt.text("(")
          args.foreach(a => {
            fmtTpe(a, vt)
            vt.text(", ")
          })
          vt.text(") -> ")
          fmtTpe(result, vt)
        case RRecordEmpty => vt.text("RecordEmpty")
        case RRecordExtend(field, value, rest) => vt.text("RecordExtend...")
        case RSchemaEmpty => vt.text("SchemaEmpty")
        case RSchemaExtend(name, tpe, rest) => vt.text("SchemaExtend...")
        case RRelation(tpes) => vt.text("Relation...")
        case RLattice(tpes) => vt.text("Lattice...")
        case RNative(clazz) => vt.text("Native("); vt.text(clazz.getSimpleName); vt.text(")")
        case RObject => vt.text("Object")
      }
    }

    def fmtParam(p: FormalParam, vt: VirtualTerminal): Unit = {
      fmtSym(p.sym, vt)
      vt.text(": ")
      fmtTpe(p.tpe, vt)
    }

    def fmtSym(sym: Symbol.VarSym, vt: VirtualTerminal): Unit = {
      vt << Cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym, vt: VirtualTerminal): Unit = {
      vt << Blue(sym.toString)
    }

    def fmtSym(sym: Symbol.LabelSym, vt: VirtualTerminal): Unit = {
      vt << Magenta(sym.toString)
    }

    def fmtOp(op: ErasedAst.Operator, vt: VirtualTerminal): VirtualTerminal = op match {
      case operator: ArithmeticOp => operator match {
        case ArithmeticOp.Add => vt.text("+")
        case ArithmeticOp.Sub => vt.text("-")
        case ArithmeticOp.Mul => vt.text("*")
        case ArithmeticOp.Div => vt.text("/")
        case ArithmeticOp.Rem => vt.text("%")
        case ArithmeticOp.Exp => vt.text("**")
      }
      case operator: ComparisonOp => operator match {
        case ComparisonOp.Lt => vt.text("<")
        case ComparisonOp.Le => vt.text("<=")
        case ComparisonOp.Gt => vt.text(">")
        case ComparisonOp.Ge => vt.text(">=")
        case operator: EqualityOp => operator match {
          case EqualityOp.Eq => vt.text("==")
          case EqualityOp.Ne => vt.text("!=")
        }
      }
      case operator: LogicalOp => operator match {
        case LogicalOp.And => vt.text("and")
        case LogicalOp.Or => vt.text("or")
      }
      case operator: BitwiseOp => operator match {
        case BitwiseOp.And => vt.text("&&&")
        case BitwiseOp.Or => vt.text("|||")
        case BitwiseOp.Xor => vt.text("^^^")
        case BitwiseOp.Shl => vt.text("<<<")
        case BitwiseOp.Shr => vt.text(">>>")
      }
    }

  }

}
