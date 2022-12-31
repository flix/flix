package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.{Ast, MonoType}
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.Symbol._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil._

import scala.annotation.tailrec


object ErasedAstPrinter {

  def doc(root: Root)(implicit i: Indent): Doc = {
    val defs = root.
      defs.
      toList.
      sortBy{case (sym, _) => sym.namespace.mkString("/")}/*.filter { case (sym, _) => sym.namespace == List("One") }*/.
      map { case (_, defn) => doc(defn) }
    group(fold(_ <> breakWith("") <> breakWith("") <> _, defs))
  }

  def doc(defn: Def)(implicit i: Indent): Doc = {
    defnf(
      defn.sym.toString,
      defn.formals.map(doc),
      returnTypeDoc(defn.tpe),
      doc(defn.exp, paren = false, inBlock = true)
    )
  }

  def doc(f: FormalParam)(implicit i: Indent): Doc = {
    paramf(text(f.sym.toString) <> text("%") <> text(f.sym.getStackOffset.toString), MonoTypePrinter.doc(f.tpe))
  }

  def doc(sym: VarSym): Doc = text(sym.toString) <> text("%") <> text(sym.getStackOffset.toString)

  sealed trait Position

  def doc(exp: Expression, paren: Boolean = true, inBlock: Boolean = false)(implicit i: Indent): Doc = {
    def par(d: Doc): Doc = if (paren) parens(d) else d

    exp match {
      case Expression.Cst(cst, _, _) =>
        val output = ConstantPrinter.doc(cst)
        output
      case Expression.Var(sym, _, _) =>
        val output = doc(sym)
        output
      case Expression.Closure(sym, closureArgs, _, _) =>
        applyf(doc(sym) <> metaText("buildclo"), closureArgs.map(doc(_)))
      case Expression.ApplyClo(exp, args, _, _) =>
        val output = applyf(doc(exp) <> metaText("clo"), args.map(a => doc(a, paren = false)))
        par(output)
      case Expression.ApplyDef(sym, args, _, _) =>
        val output = applyf(doc(sym) <> metaText("def"), args.map(a => doc(a, paren = false)))
        par(output)
      case Expression.ApplyCloTail(exp, args, _, _) =>
        val output = applyf(doc(exp) <> metaText("clotail"), args.map(a => doc(a, paren = false)))
        par(output)
      case Expression.ApplyDefTail(sym, args, _, _) =>
        val output = applyf(doc(sym) <> metaText("deftail"), args.map(a => doc(a, paren = false)))
        par(output)
      case Expression.ApplySelfTail(sym, _, actuals, _, _) =>
        val output = applyf(doc(sym) <> metaText("clotail"), actuals.map(a => doc(a, paren = false)))
        par(output)
      case Expression.Unary(_, op, exp, _, _) =>
        val output = OperatorPrinter.doc(op) <> doc(exp)
        par(output)
      case Expression.Binary(_, op, exp1, exp2, _, _) =>
        val output = doc(exp1) <+> OperatorPrinter.doc(op) <+> doc(exp2)
        par(output)
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        val output = itef(doc(exp1, paren = false), doc(exp2, paren = false, inBlock = true), doc(exp3, paren = false, inBlock = true))
        par(output)
      case Expression.Branch(exp, branches, tpe, loc) =>
        metaText("Branch")
      case Expression.JumpTo(sym, tpe, loc) =>
        metaText("JumpTo")
      case Expression.Let(_, _, _, _, _) =>
        val es = collectLetBlock(exp, Nil)
        val output = if (inBlock) seqf(es) else seqBlockf(es)
        output
      case Expression.LetRec(_, _, _, _, _, _, _) =>
        val es = collectLetBlock(exp, Nil)
        val output = if (inBlock) seqf(es) else seqBlockf(es)
        output
      case Expression.Region(tpe, loc) => metaText("Region")
      case Expression.Scope(sym, exp, tpe, loc) => metaText("Scope")
      case Expression.Is(sym, exp, loc) => metaText("Is")
      case Expression.Tag(sym, exp, tpe, loc) =>
        applyf(doc(sym), List(doc(exp)))
      case Expression.Untag(sym, exp, tpe, loc) => metaText("Untag")
      case Expression.Index(base, offset, tpe, loc) => metaText("Index")
      case Expression.Tuple(elms, _, _) =>
        tuplef(elms.map(doc(_, paren = false)))
      case Expression.RecordEmpty(tpe, loc) => metaText("RecordEmpty")
      case Expression.RecordSelect(exp, field, tpe, loc) => metaText("RecordSelect")
      case Expression.RecordExtend(field, value, rest, tpe, loc) => metaText("RecordExtend")
      case Expression.RecordRestrict(field, rest, tpe, loc) => metaText("RecordRestrict")
      case Expression.ArrayLit(elms, tpe, loc) => metaText("ArrayLit")
      case Expression.ArrayNew(elm, len, tpe, loc) => metaText("ArrayNew")
      case Expression.ArrayLoad(base, index, tpe, loc) => metaText("ArrayLoad")
      case Expression.ArrayStore(base, index, elm, tpe, loc) => metaText("ArrayStore")
      case Expression.ArrayLength(base, tpe, loc) => metaText("ArrayLength")
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => metaText("ArraySlice")
      case Expression.Ref(exp, _, _) =>
        val output = par(text("ref") <+> doc(exp))
        par(output)
      case Expression.Deref(exp, _, _) =>
        val output = par(text("deref") <+> doc(exp))
        par(output)
      case Expression.Assign(exp1, exp2, _, _) =>
        val output = assignf(doc(exp1), doc(exp2))
        par(output)
      case Expression.Cast(exp, tpe, _) =>
        val output = castf(doc(exp, paren = false), MonoTypePrinter.doc(tpe))
        par(output)
      case Expression.TryCatch(exp, rules, tpe, loc) => metaText("TryCatch")
      case Expression.InvokeConstructor(constructor, args, tpe, loc) => metaText("InvokeConstructor")
      case Expression.InvokeMethod(method, exp, args, _, _) =>
        val output = applyJavaf(method, doc(exp), args.map(doc(_, paren = false)))
        par(output)
      case Expression.InvokeStaticMethod(method, args, _, _) =>
        val output = applyStaticJavaf(method, args.map(doc(_, paren = false)))
        output
      case Expression.GetField(field, exp, tpe, loc) => metaText("GetField")
      case Expression.PutField(field, exp1, exp2, tpe, loc) => metaText("PutField")
      case Expression.GetStaticField(field, tpe, loc) => metaText("GetStaticField")
      case Expression.PutStaticField(field, exp, tpe, loc) => metaText("PutStaticField")
      case Expression.NewObject(name, clazz, tpe, methods, loc) => metaText("NewObject")
      case Expression.Spawn(exp1, exp2, tpe, loc) => metaText("Spawn")
      case Expression.Lazy(exp, tpe, loc) => metaText("Lazy")
      case Expression.Force(exp, tpe, loc) => metaText("Force")
      case Expression.HoleError(sym, _, _) => text("?") <> doc(sym)
      case Expression.MatchError(_, _) => metaText("MatchError")
      case Expression.BoxBool(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxInt8(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxInt16(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxInt32(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxInt64(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxChar(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxFloat32(exp, _) => text("box") <+> doc(exp)
      case Expression.BoxFloat64(exp, _) => text("box") <+> doc(exp)
      case Expression.UnboxBool(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxInt8(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxInt16(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxInt32(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxInt64(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxChar(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxFloat32(exp, _) => text("unbox") <+> doc(exp)
      case Expression.UnboxFloat64(exp, _) => text("unbox") <+> doc(exp)
    }
  }

  @tailrec
  def collectLetBlock(e: Expression, acc: List[Doc])(implicit i: Indent): List[Doc] = e match {
    case Expression.Let(sym, exp1, exp2, _, _) =>
      val let = letf(doc(sym), None, doc(exp1, paren = false))
      collectLetBlock(exp2, let :: acc)
    case Expression.LetRec(varSym, _, _, exp1, exp2, _, _) =>
      val let = letrecf(doc(varSym), None, doc(exp1, paren = false))
      collectLetBlock(exp2, let :: acc)
    case other => (doc(other, paren = false) :: acc).reverse
  }

  def returnTypeDoc(tpe: MonoType)(implicit i: Indent): Doc = tpe match {
    case MonoType.Arrow(_, result) => MonoTypePrinter.doc(result)
    case _ => metaText("NoReturnType")
  }

  def doc(sym: HoleSym): Doc = text(sym.toString)

  def doc(sym: DefnSym): Doc = text(sym.toString)

  def doc(sym: CaseSym): Doc = text(sym.toString)

}
