package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.{MonoAst, Symbol}
import ca.uwaterloo.flix.language.ast.MonoAst.{Expr, ExtPattern, ExtTagPattern, Pattern}
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

  private def print(e: MonoAst.Expr): DocAst.Expr = e match {
    case Expr.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _) => printVar(sym)
    case Expr.Lambda(fparam, exp, _, _) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.ApplyAtomic(op, exps, tpe, eff, _) => OpPrinter.print(op, exps.map(print), TypePrinter.print(tpe), TypePrinter.print(eff))
    case Expr.ApplyClo(exp1, exp2, _, _, _) => DocAst.Expr.App(print(exp1), List(print(exp2)))
    case Expr.ApplyDef(sym, exps, _, _, _, _) => DocAst.Expr.ApplyDef(sym, exps.map(print))
    case Expr.ApplyLocalDef(sym, exps, _, _, _) => DocAst.Expr.ApplyLocalDef(sym, exps.map(print))
    case Expr.ApplyOp(sym, exps, _, _, _) => DocAst.Expr.ApplyOp(sym, exps.map(print))
    case Expr.Let(sym, exp1, exp2, _, _, _, _) => DocAst.Expr.Let(printVar(sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, _) => DocAst.Expr.LocalDef(printVar(sym), fparams.map(printFormalParam), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)), print(exp1), print(exp2))
    case Expr.Scope(sym, _, exp, _, _, _) => DocAst.Expr.Scope(printVar(sym), print(exp))
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, _, _) => DocAst.Expr.Discard(print(exp))
    case Expr.Match(exp, rules, _, _, _) => DocAst.Expr.Match(print(exp), rules.map(printMatchRule))
    case Expr.ExtMatch(exp, rules, _, _, _) => DocAst.Expr.ExtMatch(print(exp), rules.map(printExtMatchRule))
    case Expr.VectorLit(exps, _, _, _) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, _, _, _) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, _) => DocAst.Expr.VectorLength(print(exp))
    case Expr.Cast(exp, tpe, eff, _) => DocAst.Expr.UncheckedCast(print(exp), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)))
    case Expr.TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map(printCatchRule))
    case Expr.RunWith(exp, effUse, rules, _, _, _) => DocAst.Expr.RunWithHandler(print(exp), effUse.sym, rules.map(printHandlerRule))
    case Expr.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expr.NewObject(name, clazz, TypePrinter.print(tpe), methods.map(printJvmMethod))
  }

  /** Returns the [[DocAst.JvmMethod]] representation of `method`. */
  private def printJvmMethod(method: MonoAst.JvmMethod): DocAst.JvmMethod = method match {
    case MonoAst.JvmMethod(ident, fparams, exp, retTpe, _, _) =>
      DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
  }

  /** Returns the [[DocAst]] representation of `rule`. */
  private def printHandlerRule(rule: MonoAst.HandlerRule): (Symbol.OpSym, List[DocAst.Expr.AscriptionTpe], DocAst.Expr) = rule match {
    case MonoAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam), print(exp))
  }

  /** Returns the [[DocAst]] representation of `rule`. */
  private def printCatchRule(rule: MonoAst.CatchRule): (Symbol.VarSym, Class[?], DocAst.Expr) = rule match {
    case MonoAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
  }

  /** Returns the [[DocAst]] representation of `rule`. */
  private def printMatchRule(rule: MonoAst.MatchRule): (DocAst.Expr, Option[DocAst.Expr], DocAst.Expr) = rule match {
    case MonoAst.MatchRule(pat, guard, exp) => (printPattern(pat), guard.map(print), print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printExtMatchRule(rule: MonoAst.ExtMatchRule): (DocAst.Expr, DocAst.Expr) = rule match {
    case MonoAst.ExtMatchRule(pat, exp, _) =>
      (printExtPattern(pat), print(exp))
  }

  /** Returns the [[DocAst.Expr]] representation of `pattern`. */
  private def printPattern(pattern: MonoAst.Pattern): DocAst.Expr = pattern match {
    case Pattern.Wild(_, _) => DocAst.Expr.Wild
    case Pattern.Var(sym, _, _, _) => printVar(sym)
    case Pattern.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Pattern.Tag(symUse, pats, _, _) => DocAst.Expr.Tag(symUse.sym, pats.map(printPattern))
    case Pattern.Tuple(elms, _, _) => DocAst.Expr.Tuple(elms.map(printPattern).toList)
    case Pattern.Record(pats, pat, _, _) => printRecordPattern(pats, pat)
  }

  /** Returns the [[DocAst.Expr]] representation of `pats`. */
  private def printRecordPattern(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern): DocAst.Expr = {
    pats.foldRight(printPattern(pat)) {
      case (MonoAst.Pattern.Record.RecordLabelPattern(label, recordPat, _, _), acc) =>
        DocAst.Expr.RecordExtend(label, printPattern(recordPat), acc)
    }
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `pattern`.
    */
  private def printExtPattern(pattern: MonoAst.ExtPattern): DocAst.Expr = pattern match {
    case ExtPattern.Default(_) => DocAst.Pattern.Default
    case ExtPattern.Tag(label, pats, _) => DocAst.Pattern.ExtTag(label, pats.map(printExtTagPattern))
    }

  /**
    * Returns the [[DocAst.Expr]] representation of `pattern`.
    */
  private def printExtTagPattern(pattern: MonoAst.ExtTagPattern): DocAst.Expr = pattern match {
    case ExtTagPattern.Wild(_, _) => DocAst.Expr.Wild
    case ExtTagPattern.Var(sym, _, _, _) => DocAst.Expr.Var(sym)
    case ExtTagPattern.Unit(_, _) => DocAst.Expr.Unit
  }

  /** Returns the [[DocAst.Expr]] representation of `sym`. */
  private def printVar(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)

  /** Returns the [[DocAst.Expr.AscriptionTpe]] representation of `fp`. */
  private def printFormalParam(fp: MonoAst.FormalParam): DocAst.Expr.AscriptionTpe = {
    val MonoAst.FormalParam(sym, tpe, _, _) = fp
    DocAst.Expr.AscriptionTpe(DocAst.Expr.Var(sym), TypePrinter.print(tpe))
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
