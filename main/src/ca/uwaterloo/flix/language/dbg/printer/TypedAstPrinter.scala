package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.TypedAst.{Exp, ExtPattern, ExtTagPattern, Pattern}
import ca.uwaterloo.flix.language.ast.shared.SymUse.{DefSymUse, LocalDefSymUse, SigSymUse}
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import ca.uwaterloo.flix.language.dbg.DocAst

object TypedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: TypedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case TypedAst.Enum(_, ann, mod, sym, tparams, _, cases, _) =>
        DocAst.Enum(ann, mod, sym, tparams.map(printTypeParam), cases.values.map(printCase).toList)
    }.toList
    val defs = root.defs.values.map {
      case TypedAst.Def(sym, TypedAst.Spec(_, ann, mod, _, fparams, _, retTpe, eff, _, _), exp, _) =>
        DocAst.Def(ann, mod, sym, fparams.map(printFormalParam), TypePrinter.print(retTpe), TypePrinter.print(eff), print(exp))
    }.toList
    DocAst.Program(enums, defs, Nil)
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `e`.
    */
  private def print(e: TypedAst.Exp): DocAst.Exp = e match {
    case Exp.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Exp.Var(sym, _, _) => printVar(sym)
    case Exp.Hole(sym, _, _, _, _) => DocAst.Exp.Hole(sym)
    case Exp.HoleWithExp(exp, _, _, _, _) => DocAst.Exp.HoleWithExp(print(exp))
    case Exp.OpenAs(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.Use(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.Lambda(fparam, exp, _, _) => DocAst.Exp.Lambda(List(printFormalParam(fparam)), print(exp))
    case Exp.ApplyClo(exp1, exp2, _, _, _) => DocAst.Exp.App(print(exp1), List(print(exp2)))
    case Exp.ApplyDef(DefSymUse(sym, _), exps, _, _, _, _, _) => DocAst.Exp.ApplyDef(sym, exps.map(print))
    case Exp.ApplyLocalDef(LocalDefSymUse(sym, _), exps, _, _, _, _) => DocAst.Exp.App(DocAst.Exp.Var(sym), exps.map(print))
    case Exp.ApplyOp(op, exps, _, _, _) => DocAst.Exp.ApplyOp(op.sym, exps.map(print))
    case Exp.ApplySig(SigSymUse(sym, _), exps, _, _, _, _, _, _) => DocAst.Exp.App(DocAst.Exp.AsIs(sym.name), exps.map(print))
    case Exp.Unary(sop, exp, _, _, _) => DocAst.Exp.Unary(OpPrinter.print(sop), print(exp))
    case Exp.Binary(sop, exp1, exp2, _, _, _) => DocAst.Exp.Binary(print(exp1), OpPrinter.print(sop), print(exp2))
    case Exp.Let(bnd, exp1, exp2, _, _, _) => DocAst.Exp.Let(printVar(bnd.sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Exp.LocalDef(TypedAst.Binder(sym, _), fparams, exp1, exp2, tpe, eff, _) => DocAst.Exp.LocalDef(printVar(sym), fparams.map(printFormalParam), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)), print(exp1), print(exp2))
    case Exp.Region(TypedAst.Binder(sym, _), _, exp, _, _, _) => DocAst.Exp.Region(printVar(sym), print(exp))
    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Exp.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Exp.Stm(exp1, exp2, _, _, _) => DocAst.Exp.Stm(print(exp1), print(exp2))
    case Exp.Discard(exp, _, _) => DocAst.Exp.Discard(print(exp))
    case Exp.Match(exp, rules, _, _, _) => DocAst.Exp.Match(print(exp), rules.map(printMatchRule))
    case Exp.TypeMatch(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.RestrictableChoose(_, _, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.ExtMatch(exp, rules, _, _, _) => DocAst.Exp.ExtMatch(print(exp), rules.map(printExtMatchRule))
    case Exp.Tag(symUse, exps, _, _, _) => DocAst.Exp.Tag(symUse.sym, exps.map(print))
    case Exp.RestrictableTag(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.ExtTag(label, exps, _, _, _) => DocAst.Exp.ExtTag(label, exps.map(print))
    case Exp.Tuple(elms, _, _, _) => DocAst.Exp.Tuple(elms.map(print))
    case Exp.RecordSelect(exp, label, _, _, _) => DocAst.Exp.RecordSelect(label, print(exp))
    case Exp.RecordExtend(label, exp1, exp2, _, _, _) => DocAst.Exp.RecordExtend(label, print(exp1), print(exp2))
    case Exp.RecordRestrict(label, exp, _, _, _) => DocAst.Exp.RecordRestrict(label, print(exp))
    case Exp.ArrayLit(exps, exp, _, _, _) => DocAst.Exp.InRegion(DocAst.Exp.ArrayLit(exps.map(print)), print(exp))
    case Exp.ArrayNew(exp1, exp2, exp3, _, _, _) => DocAst.Exp.InRegion(DocAst.Exp.ArrayNew(print(exp1), print(exp2)), print(exp3))
    case Exp.ArrayLoad(exp1, exp2, _, _, _) => DocAst.Exp.ArrayLoad(print(exp1), print(exp2))
    case Exp.ArrayLength(exp, _, _) => DocAst.Exp.ArrayLength(print(exp))
    case Exp.ArrayStore(exp1, exp2, exp3, _, _) => DocAst.Exp.ArrayStore(print(exp1), print(exp2), print(exp3))
    case Exp.StructNew(sym, fields, region, _, _, _) => DocAst.Exp.StructNew(sym, fields.map { case (k, v) => (k.sym, print(v)) }, print(region))
    case Exp.StructGet(exp, field, _, _, _) => DocAst.Exp.StructGet(print(exp), field.sym)
    case Exp.StructPut(exp1, field, exp2, _, _, _) => DocAst.Exp.StructPut(print(exp1), field.sym, print(exp2))
    case Exp.VectorLit(exps, _, _, _) => DocAst.Exp.VectorLit(exps.map(print))
    case Exp.VectorLoad(exp1, exp2, _, _, _) => DocAst.Exp.VectorLoad(print(exp1), print(exp2))
    case Exp.VectorLength(exp, _) => DocAst.Exp.VectorLength(print(exp))
    case Exp.Ascribe(exp, _, _, tpe, _, _) => DocAst.Exp.AscriptionTpe(print(exp), TypePrinter.print(tpe))
    case Exp.InstanceOf(exp, clazz, _) => DocAst.Exp.InstanceOf(print(exp), clazz)
    case Exp.CheckedCast(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.UncheckedCast(_, _, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.Unsafe(exp, runEff, _, _, _) => DocAst.Exp.Unsafe(print(exp), TypePrinter.print(runEff))
    case Exp.Without(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.TryCatch(exp, rules, _, _, _) => DocAst.Exp.TryCatch(print(exp), rules.map(printCatchRule))
    case Exp.Throw(exp, _, _, _) => DocAst.Exp.Throw(print(exp))
    case Exp.Handler(symUse, rules, _, _, _, _, _) => DocAst.Exp.Handler(symUse.sym, rules.map(printHandlerRule))
    case Exp.RunWith(exp1, exp2, _, _, _) => DocAst.Exp.RunWith(print(exp1), print(exp2))
    case Exp.InvokeConstructor(constructor, exps, _, _, _) => DocAst.Exp.JavaInvokeConstructor(constructor, exps.map(print))
    case Exp.InvokeMethod(method, exp, exps, _, _, _) => DocAst.Exp.JavaInvokeMethod(method, print(exp), exps.map(print))
    case Exp.InvokeStaticMethod(method, exps, _, _, _) => DocAst.Exp.JavaInvokeStaticMethod(method, exps.map(print))
    case Exp.GetField(field, exp, _, _, _) => DocAst.Exp.JavaGetField(field, print(exp))
    case Exp.PutField(field, exp1, exp2, _, _, _) => DocAst.Exp.JavaPutField(field, print(exp1), print(exp2))
    case Exp.GetStaticField(field, _, _, _) => DocAst.Exp.JavaGetStaticField(field)
    case Exp.PutStaticField(field, exp, _, _, _) => DocAst.Exp.JavaPutStaticField(field, print(exp))
    case Exp.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Exp.NewObject(name, clazz, TypePrinter.print(tpe), methods.map(printJvmMethod))
    case Exp.NewChannel(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.GetChannel(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.PutChannel(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.SelectChannel(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.Spawn(exp1, exp2, _, _, _) => DocAst.Exp.Spawn(print(exp1), print(exp2))
    case Exp.ParYield(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.Lazy(exp, _, _) => DocAst.Exp.Lazy(print(exp))
    case Exp.Force(exp, _, _, _) => DocAst.Exp.Force(print(exp))
    case Exp.FixpointConstraintSet(_, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointLambda(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointMerge(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointQueryWithProvenance(_, _, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointQueryWithSelect(_, _, _, _, _, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointSolveWithProject(_, _, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointInjectInto(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.Error(_, _, _) => DocAst.Exp.Error
  }

  /**
    * Returns the [[DocAst.JvmMethod]] representation of `method`.
    */
  private def printJvmMethod(method: TypedAst.JvmMethod): DocAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, retTpe, _, _) =>
      DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printHandlerRule(rule: TypedAst.HandlerRule): (Symbol.OpSym, List[DocAst.Exp.AscriptionTpe], DocAst.Exp) = rule match {
    case TypedAst.HandlerRule(op, fparams, exp, _) => (op.sym, fparams.map(printFormalParam), print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printCatchRule(rule: TypedAst.CatchRule): (Symbol.VarSym, Class[?], DocAst.Exp) = rule match {
    case TypedAst.CatchRule(bnd, clazz, exp, _) => (bnd.sym, clazz, print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printMatchRule(rule: TypedAst.MatchRule): (DocAst.Exp, Option[DocAst.Exp], DocAst.Exp) = rule match {
    case TypedAst.MatchRule(pat, guard, exp, _) => (printPattern(pat), guard.map(print), print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printExtMatchRule(rule: TypedAst.ExtMatchRule): (DocAst.Exp, DocAst.Exp) = rule match {
    case TypedAst.ExtMatchRule(pat, exp, _) =>
      (printExtPattern(pat), print(exp))
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printPattern(pattern: TypedAst.Pattern): DocAst.Exp = pattern match {
    case Pattern.Wild(_, _) => DocAst.Exp.Wild
    case Pattern.Var(TypedAst.Binder(sym, _), _, _) => printVar(sym)
    case Pattern.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Pattern.Tag(symUse, pats, _, _) => DocAst.Exp.Tag(symUse.sym, pats.map(printPattern))
    case Pattern.Tuple(elms, _, _) => DocAst.Exp.Tuple(elms.map(printPattern).toList)
    case Pattern.Record(pats, pat, _, _) => printRecordPattern(pats, pat)
    case Pattern.Error(_, _) => DocAst.Exp.Error
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pats`.
    */
  private def printRecordPattern(pats: List[Record.RecordLabelPattern], pat: Pattern): DocAst.Exp = {
    pats.foldRight(printPattern(pat)) {
      case (TypedAst.Pattern.Record.RecordLabelPattern(label, rest, _, _), acc) =>
        DocAst.Exp.RecordExtend(label, printPattern(rest), acc)
    }
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printExtPattern(pattern: TypedAst.ExtPattern): DocAst.Exp = {
    pattern match {
      case ExtPattern.Default(_) => DocAst.Exp.Wild
      case ExtPattern.Tag(label, pats, _) => DocAst.Pattern.ExtTag(label, pats.map(printExtTagPattern))
      case ExtPattern.Error(_) => DocAst.Exp.Error
    }
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printExtTagPattern(pattern: TypedAst.ExtTagPattern): DocAst.Exp = pattern match {
    case ExtTagPattern.Wild(_, _) => DocAst.Exp.Wild
    case ExtTagPattern.Var(TypedAst.Binder(sym, _), _, _) => DocAst.Exp.Var(sym)
    case ExtTagPattern.Unit(_, _) => DocAst.Exp.Unit
    case ExtTagPattern.Error(_, _) => DocAst.Exp.Error
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `sym`.
    */
  private def printVar(sym: Symbol.VarSym): DocAst.Exp = {
    DocAst.Exp.Var(sym)
  }

  /**
    * Returns the [[DocAst.Exp.AscriptionTpe]] representation of `fp`.
    */
  private def printFormalParam(fp: TypedAst.FormalParam): DocAst.Exp.AscriptionTpe = {
    val TypedAst.FormalParam(bnd, tpe, _, _) = fp
    DocAst.Exp.AscriptionTpe(DocAst.Exp.Var(bnd.sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Case]] representation of `caze`.
    */
  private def printCase(caze: TypedAst.Case): DocAst.Case = caze match {
    case TypedAst.Case(sym, tpes, _, _) => DocAst.Case(sym, tpes.map(TypePrinter.print))
  }

  /**
    * Returns the [[DocAst.TypeParam]] representation of `tp`.
    */
  private def printTypeParam(tp: TypedAst.TypeParam): DocAst.TypeParam = tp match {
    case TypedAst.TypeParam(_, sym, _) => DocAst.TypeParam(sym)
  }

}
