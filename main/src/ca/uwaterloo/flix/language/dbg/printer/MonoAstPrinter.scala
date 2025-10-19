package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.{MonoAst, Symbol}
import ca.uwaterloo.flix.language.ast.MonoAst.{Exp, ExtPattern, ExtTagPattern, Pattern}
import ca.uwaterloo.flix.language.dbg.DocAst

object MonoAstPrinter {

  /** Returns the [[DocAst.Program]] representation of `root`. */
  def print(root: MonoAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case MonoAst.Enum(_, ann, mod, sym, tparams, cases, _) =>
        DocAst.Enum(ann, mod, sym, tparams.map(printTypeParam), cases.values.map(printCase).toList)
    }.toList
    val defs = root.defs.values.map {
      case MonoAst.Def(sym, MonoAst.Spec(_, ann, mod, fparams, _, retTpe, eff, _), exp, _) =>
        val fps = fparams.map(printFormalParam)
        val rtpe = TypePrinter.print(retTpe)
        val ef = TypePrinter.print(eff)
        val e = print(exp)
        DocAst.Def(ann, mod, sym, fps, rtpe, ef, e)
    }.toList
    DocAst.Program(enums, defs, Nil)
  }

  private def print(e: MonoAst.Exp): DocAst.Exp = e match {
    case Exp.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Exp.Var(sym, _, _) => printVar(sym)
    case Exp.Lambda(fparam, exp, _, _) => DocAst.Exp.Lambda(List(printFormalParam(fparam)), print(exp))
    case Exp.ApplyAtomic(op, exps, tpe, eff, _) => OpPrinter.print(op, exps.map(print), TypePrinter.print(tpe), TypePrinter.print(eff))
    case Exp.ApplyClo(exp1, exp2, _, _, _) => DocAst.Exp.App(print(exp1), List(print(exp2)))
    case Exp.ApplyDef(sym, exps, _, _, _, _) => DocAst.Exp.ApplyDef(sym, exps.map(print))
    case Exp.ApplyLocalDef(sym, exps, _, _, _) => DocAst.Exp.ApplyLocalDef(sym, exps.map(print))
    case Exp.ApplyOp(sym, exps, _, _, _) => DocAst.Exp.ApplyOp(sym, exps.map(print))
    case Exp.Let(sym, exp1, exp2, _, _, _, _) => DocAst.Exp.Let(printVar(sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Exp.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, _) => DocAst.Exp.LocalDef(printVar(sym), fparams.map(printFormalParam), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)), print(exp1), print(exp2))
    case Exp.Region(sym, _, exp, _, _, _) => DocAst.Exp.Region(printVar(sym), print(exp))
    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Exp.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Exp.Stm(exp1, exp2, _, _, _) => DocAst.Exp.Stm(print(exp1), print(exp2))
    case Exp.Discard(exp, _, _) => DocAst.Exp.Discard(print(exp))
    case Exp.Match(exp, rules, _, _, _) => DocAst.Exp.Match(print(exp), rules.map(printMatchRule))
    case Exp.ExtMatch(exp, rules, _, _, _) => DocAst.Exp.ExtMatch(print(exp), rules.map(printExtMatchRule))
    case Exp.VectorLit(exps, _, _, _) => DocAst.Exp.VectorLit(exps.map(print))
    case Exp.VectorLoad(exp1, exp2, _, _, _) => DocAst.Exp.VectorLoad(print(exp1), print(exp2))
    case Exp.VectorLength(exp, _) => DocAst.Exp.VectorLength(print(exp))
    case Exp.Cast(exp, tpe, eff, _) => DocAst.Exp.UncheckedCast(print(exp), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)))
    case Exp.TryCatch(exp, rules, _, _, _) => DocAst.Exp.TryCatch(print(exp), rules.map(printCatchRule))
    case Exp.RunWith(exp, effUse, rules, _, _, _) => DocAst.Exp.RunWithHandler(print(exp), effUse.sym, rules.map(printHandlerRule))
    case Exp.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Exp.NewObject(name, clazz, TypePrinter.print(tpe), methods.map(printJvmMethod))
  }

  /** Returns the [[DocAst.JvmMethod]] representation of `method`. */
  private def printJvmMethod(method: MonoAst.JvmMethod): DocAst.JvmMethod = method match {
    case MonoAst.JvmMethod(ident, fparams, exp, retTpe, _, _) =>
      DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
  }

  /** Returns the [[DocAst]] representation of `rule`. */
  private def printHandlerRule(rule: MonoAst.HandlerRule): (Symbol.OpSym, List[DocAst.Exp.AscriptionTpe], DocAst.Exp) = rule match {
    case MonoAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam), print(exp))
  }

  /** Returns the [[DocAst]] representation of `rule`. */
  private def printCatchRule(rule: MonoAst.CatchRule): (Symbol.VarSym, Class[?], DocAst.Exp) = rule match {
    case MonoAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
  }

  /** Returns the [[DocAst]] representation of `rule`. */
  private def printMatchRule(rule: MonoAst.MatchRule): (DocAst.Exp, Option[DocAst.Exp], DocAst.Exp) = rule match {
    case MonoAst.MatchRule(pat, guard, exp) => (printPattern(pat), guard.map(print), print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printExtMatchRule(rule: MonoAst.ExtMatchRule): (DocAst.Exp, DocAst.Exp) = rule match {
    case MonoAst.ExtMatchRule(pat, exp, _) =>
      (printExtPattern(pat), print(exp))
  }

  /** Returns the [[DocAst.Exp]] representation of `pattern`. */
  private def printPattern(pattern: MonoAst.Pattern): DocAst.Exp = pattern match {
    case Pattern.Wild(_, _) => DocAst.Exp.Wild
    case Pattern.Var(sym, _, _, _) => printVar(sym)
    case Pattern.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Pattern.Tag(symUse, pats, _, _) => DocAst.Exp.Tag(symUse.sym, pats.map(printPattern))
    case Pattern.Tuple(elms, _, _) => DocAst.Exp.Tuple(elms.map(printPattern).toList)
    case Pattern.Record(pats, pat, _, _) => printRecordPattern(pats, pat)
  }

  /** Returns the [[DocAst.Exp]] representation of `pats`. */
  private def printRecordPattern(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern): DocAst.Exp = {
    pats.foldRight(printPattern(pat)) {
      case (MonoAst.Pattern.Record.RecordLabelPattern(label, recordPat, _, _), acc) =>
        DocAst.Exp.RecordExtend(label, printPattern(recordPat), acc)
    }
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printExtPattern(pattern: MonoAst.ExtPattern): DocAst.Exp = pattern match {
    case ExtPattern.Default(_) => DocAst.Pattern.Default
    case ExtPattern.Tag(label, pats, _) => DocAst.Pattern.ExtTag(label, pats.map(printExtTagPattern))
    }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printExtTagPattern(pattern: MonoAst.ExtTagPattern): DocAst.Exp = pattern match {
    case ExtTagPattern.Wild(_, _) => DocAst.Exp.Wild
    case ExtTagPattern.Var(sym, _, _, _) => DocAst.Exp.Var(sym)
    case ExtTagPattern.Unit(_, _) => DocAst.Exp.Unit
  }

  /** Returns the [[DocAst.Exp]] representation of `sym`. */
  private def printVar(sym: Symbol.VarSym): DocAst.Exp =
    DocAst.Exp.Var(sym)

  /** Returns the [[DocAst.Exp.AscriptionTpe]] representation of `fp`. */
  private def printFormalParam(fp: MonoAst.FormalParam): DocAst.Exp.AscriptionTpe = {
    val MonoAst.FormalParam(sym, tpe, _, _) = fp
    DocAst.Exp.AscriptionTpe(DocAst.Exp.Var(sym), TypePrinter.print(tpe))
  }

  /** Returns the [[DocAst.Case]] representation of `caze`. */
  private def printCase(caze: MonoAst.Case): DocAst.Case = caze match {
    case MonoAst.Case(sym, tpes, _) => DocAst.Case(sym, tpes.map(TypePrinter.print))
  }

  /** Returns the [[DocAst.TypeParam]] representation of `tp`. */
  private def printTypeParam(tp: MonoAst.TypeParam): DocAst.TypeParam = tp match {
    case MonoAst.TypeParam(_, sym, _) => DocAst.TypeParam(sym)
  }
}
