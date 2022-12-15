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

  implicit val indent: Indent = INDENT

  def doc(root: Root): Doc = {
    val defs = root.
      defs.
      toList.
      sortBy{case (sym, _) => sym.namespace.mkString("/")}/*.filter { case (sym, _) => sym.namespace == List("One") }*/.
      map { case (_, defn) => doc(defn) }
    group(fold(_ <> breakWith("") <> breakWith("") <> _, defs))
  }

  def doc(defn: Def): Doc = {
    defnf(
      defn.sym.toString,
      defn.formals.map(doc),
      returnTypeDoc(defn.tpe),
      doc(defn.exp, paren = false, topDef = true)
    )
  }

  def doc(f: FormalParam): Doc = {
    paramf(doc(f.sym), MonoTypePrinter.doc(f.tpe))
  }

  def doc(sym: VarSym): Doc = text(sym.toString)

  sealed trait Position

  def doc(exp: Expression, paren: Boolean = true, topDef: Boolean = false): Doc = {
    def par(d: Doc): Doc = if (paren) parens(d) else d

    par(exp match {
      case Expression.Cst(cst, _, _) => doc(cst)
      case Expression.Var(sym, _, _) => doc(sym)
      case Expression.Closure(sym, closureArgs, tpe, loc) => text("<Closure>")
      case Expression.ApplyClo(exp, args, _, _) =>
        applyf(doc(exp, paren = false) <> text("[clo]"), args.map(a => doc(a, paren = false)))
      case Expression.ApplyDef(sym, args, _, _) =>
        applyf(doc(sym) <> text("[def]"), args.map(a => doc(a, paren = false)))
      case Expression.ApplyCloTail(exp, args, _, _) =>
        applyf(doc(exp, paren = false) <> text("[clotail]"), args.map(a => doc(a, paren = false)))
      case Expression.ApplyDefTail(sym, args, _, _) =>
        applyf(doc(sym) <> text("[deftail]"), args.map(a => doc(a, paren = false)))
      case Expression.ApplySelfTail(sym, _, actuals, _, _) =>
        applyf(doc(sym) <> text("[clotail]"), actuals.map(a => doc(a, paren = false)))
      case Expression.Unary(sop, op, exp, tpe, loc) => text("<Unary>")
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => text("<Binary>")
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        itef(doc(exp1, paren = false), doc(exp2, paren = false), doc(exp3, paren = false))
      case Expression.Branch(exp, branches, tpe, loc) => text("<Branch>")
      case Expression.JumpTo(sym, tpe, loc) => text("<JumpTo>")
      case Expression.Let(_, _, _, _, _) =>
        val es = collectLetBlock(exp, Nil)
        if (topDef) seqf(es) else seqBlockf(es)
      case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
        val es = collectLetBlock(exp, Nil)
        if (topDef) seqf(es) else seqBlockf(es)
      case Expression.Region(tpe, loc) => text("<Region>")
      case Expression.Is(sym, exp, loc) => text("<Is>")
      case Expression.Tag(sym, exp, tpe, loc) => text("<Tag>")
      case Expression.Untag(sym, exp, tpe, loc) => text("<Untag>")
      case Expression.Index(base, offset, tpe, loc) => text("<Index>")
      case Expression.Tuple(elms, tpe, loc) => text("<Tuple>")
      case Expression.RecordEmpty(tpe, loc) => text("<RecordEmpty>")
      case Expression.RecordSelect(exp, field, tpe, loc) => text("<RecordSelect>")
      case Expression.RecordExtend(field, value, rest, tpe, loc) => text("<RecordExtend>")
      case Expression.RecordRestrict(field, rest, tpe, loc) => text("<RecordRestrict>")
      case Expression.ArrayLit(elms, tpe, loc) => text("<ArrayLit>")
      case Expression.ArrayNew(elm, len, tpe, loc) => text("<ArrayNew>")
      case Expression.ArrayLoad(base, index, tpe, loc) => text("<ArrayLoad>")
      case Expression.ArrayStore(base, index, elm, tpe, loc) => text("<ArrayStore>")
      case Expression.ArrayLength(base, tpe, loc) => text("<ArrayLength>")
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => text("<ArraySlice>")
      case Expression.Ref(exp, _, _) =>
        par(text("ref") <+> doc(exp))
      case Expression.Deref(exp, _, _) =>
        par(text("deref") <+> doc(exp))
      case Expression.Assign(exp1, exp2, _, _) =>
        assignf(doc(exp1), doc(exp2))
      case Expression.Cast(exp, tpe, _) =>
        castf(doc(exp, paren = false), MonoTypePrinter.doc(tpe))
      case Expression.TryCatch(exp, rules, tpe, loc) => text("<TryCatch>")
      case Expression.InvokeConstructor(constructor, args, tpe, loc) => text("<InvokeConstructor>")
      case Expression.InvokeMethod(method, exp, args, tpe, loc) => text("<InvokeMethod>")
      case Expression.InvokeStaticMethod(method, args, tpe, loc) => text("<InvokeStaticMethod>")
      case Expression.GetField(field, exp, tpe, loc) => text("<GetField>")
      case Expression.PutField(field, exp1, exp2, tpe, loc) => text("<PutField>")
      case Expression.GetStaticField(field, tpe, loc) => text("<GetStaticField>")
      case Expression.PutStaticField(field, exp, tpe, loc) => text("<PutStaticField>")
      case Expression.NewObject(name, clazz, tpe, methods, loc) => text("<NewObject>")
      case Expression.Spawn(exp, tpe, loc) => text("<Spawn>")
      case Expression.Lazy(exp, tpe, loc) => text("<Lazy>")
      case Expression.Force(exp, tpe, loc) => text("<Force>")
      case Expression.HoleError(sym, _, _) => text("?") <> doc(sym)
      case Expression.MatchError(_, _) => text("<MatchError>")
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
    })
  }

  @tailrec
  def collectLetBlock(e: Expression, acc: List[Doc]): List[Doc] = e match {
    case Expression.Let(sym, exp1, exp2, _, _) =>
      val let = letf(doc(sym), None, doc(exp1, paren = false))
      collectLetBlock(exp2, let :: acc)
    case Expression.LetRec(varSym, _, _, exp1, exp2, _, _) =>
      val let = letrecf(doc(varSym), None, doc(exp1, paren = false))
      collectLetBlock(exp2, let :: acc)
    case other => (doc(other, paren = false) :: acc).reverse
  }

  def returnTypeDoc(tpe: MonoType): Doc = tpe match {
    case MonoType.Arrow(_, result) => MonoTypePrinter.doc(result)
    case _ => text("<NoReturnType>")
  }

  def doc(sym: HoleSym): Doc = text(sym.toString)

  def doc(sym: DefnSym): Doc = text(sym.toString)

  def doc(cst: Ast.Constant): Doc = cst match {
    case Constant.Unit => text("()")
    case Constant.Null => text("null")
    case Constant.Bool(lit) => text(lit.toString)
    case Constant.Char(lit) => text(lit.toString)
    case Constant.Float32(lit) => text(lit.toString)
    case Constant.Float64(lit) => text(lit.toString)
    case Constant.BigDecimal(lit) => text(lit.toString) <> text("ff")
    case Constant.Int8(lit) => text(lit.toString) <> text("i8")
    case Constant.Int16(lit) => text(lit.toString) <> text("i16")
    case Constant.Int32(lit) => text(lit.toString) <> text("i32")
    case Constant.Int64(lit) => text(lit.toString) <> text("i64")
    case Constant.BigInt(lit) => text(lit.toString) <> text("ii")
    case Constant.Str(lit) => applyf(text("String"), List(text(lit)))
  }

}
