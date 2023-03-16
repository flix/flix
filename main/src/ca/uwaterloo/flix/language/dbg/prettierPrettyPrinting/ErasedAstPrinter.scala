package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.ast.Symbol._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers.{ErasedPrinter, OperatorPrinter}

import scala.annotation.tailrec


object ErasedAstPrinter {

  /**
    * More principled than `pretty` but causes stackoverflow somehow.
    */
  private def doc(root: Root)(implicit i: Indent): Doc = {
    val defs = root.
      defs.
      toList.
      sortBy { case (sym, _) => sym.toString }.
      map { case (_, defn) => doc(defn) }
    group(fold(_ <> breakWith("") <> breakWith("") <> _, defs))
  }

  def pretty(root: Root, width: Int)(implicit i: Indent): String = {
    val defs = root.
      defs.
      toList.
      sortBy { case (sym, _) => sym.toString }.
      map { case (_, defn) => doc(defn) }.
      map(Doc.pretty(width, _))
    defs.mkString("\n\n")
  }

  def doc(defn: Def)(implicit i: Indent): Doc = {
    defnf(
      defn.sym.toString,
      defn.formals.map(doc),
      returnTypeDoc(defn.tpe),
      doc(defn.exp, paren = false)
    )
  }

  def doc(f: FormalParam)(implicit i: Indent): Doc = {
    paramf(text(f.sym.toString) <> text("%") <> text(f.sym.getStackOffset.toString), MonoTypePrinter.doc(f.tpe))
  }

  def doc(sym: VarSym): Doc = text(sym.toString) <> text("%") <> text(sym.getStackOffset.toString)

  def doc(exp: Expression, paren: Boolean = true)(implicit i: Indent): Doc = {
    DocAstFormatter.format(ErasedPrinter.print(exp))


//    exp match {
//      case Expression.ApplyClo(exp, args, _, _) =>
//        val output = applyf(doc(exp) <> metaText("clo"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplyDef(sym, args, _, _) =>
//        val output = applyf(doc(sym) <> metaText("def"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplyCloTail(exp, args, _, _) =>
//        val output = applyf(doc(exp) <> metaText("clotail"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplyDefTail(sym, args, _, _) =>
//        val output = applyf(doc(sym) <> metaText("deftail"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplySelfTail(sym, _, actuals, _, _) =>
//        val output = applyf(doc(sym) <> metaText("selftail"), actuals.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.Branch(exp, branches, tpe, loc) =>
//        val output = metaText("Branch")
//        output
//      case Expression.JumpTo(sym, tpe, loc) =>
//        val output = metaText("JumpTo")
//        output
//      case Expression.Is(sym, exp, loc) => metaText("Is")
//      case Expression.Untag(sym, exp, tpe, loc) => metaText("Untag")
//      case Expression.Index(base, offset, _, _) =>
//        val output = tupleIndexf(doc(base), offset)
//        par(output)
//      case Expression.Tuple(elms, _, _) =>
//        val output = tuplef(elms.map(doc(_, paren = false)))
//        output
//      case Expression.RecordEmpty(_, _) =>
//        val output = emptyRecordf()
//        output
//      case Expression.RecordSelect(exp, field, _, _) =>
//        val output = recordSelectf(doc(exp), text(field.name))
//        par(output)
//      case e@Expression.RecordExtend(_, _, _, _, _) =>
//
//        @tailrec
//        def recordDoc(exp: Expression, fields: List[(Doc, Doc)]): Doc = exp match {
//          case Expression.RecordExtend(field, value, rest, _, _) =>
//            recordDoc(rest, (text(field.name), doc(value, paren = false)) :: fields)
//          case Expression.RecordEmpty(_, _) =>
//            recordExtendf(fields.reverse, None)
//          case other =>
//            recordExtendf(fields.reverse, Some(doc(other, paren = false)))
//        }
//
//        recordDoc(e, Nil)
//      case Expression.RecordRestrict(field, rest, tpe, loc) =>
//        val output = metaText("RecordRestrict")
//        output
//      case Expression.ArrayLit(elms, _, _) =>
//        val output = arrayListf(elms.map(doc(_, paren = false)))
//        output
//      case Expression.ArrayNew(elm, len, tpe, loc) => metaText("ArrayNew")
//      case Expression.ArrayLoad(base, index, tpe, loc) => metaText("ArrayLoad")
//      case Expression.ArrayStore(base, index, elm, tpe, loc) => metaText("ArrayStore")
//      case Expression.ArrayLength(base, tpe, loc) => metaText("ArrayLength")
//      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => metaText("ArraySlice")
//      case Expression.Assign(exp1, exp2, _, _) =>
//        val output = assignf(doc(exp1), doc(exp2))
//        par(output)
//      case Expression.TryCatch(exp, rules, tpe, loc) => metaText("TryCatch")
//      case Expression.GetField(field, exp, tpe, loc) => metaText("GetField")
//      case Expression.PutField(field, exp1, exp2, tpe, loc) => metaText("PutField")
//      case Expression.GetStaticField(field, tpe, loc) => metaText("GetStaticField")
//      case Expression.PutStaticField(field, exp, tpe, loc) => metaText("PutStaticField")
//      case Expression.NewObject(name, clazz, tpe, methods, loc) => metaText("NewObject")
//      case Expression.Spawn(exp1, exp2, _, _) =>
//        val output = spawnf(doc(exp1, paren = false), doc(exp2))
//        par(output)
//      case _ => text("unknown")
//    }
  }

  def returnTypeDoc(tpe: MonoType)(implicit i: Indent): Doc = tpe match {
    case MonoType.Arrow(_, result) => MonoTypePrinter.doc(result)
    case _ => metaText("NoReturnType")
  }

  def doc(sym: HoleSym): Doc = text(sym.toString)

  def doc(sym: DefnSym): Doc = text(sym.toString)

  def doc(sym: CaseSym): Doc = text(sym.toString)

}
