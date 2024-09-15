package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.ResolvedAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.ast.{ResolvedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst


object ResolvedAstPrinter {

  def print(root: ResolvedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case ResolvedAst.Declaration.Def(sym, spec, exp) =>
        DocAst.Def(spec.ann, spec.mod, sym, spec.fparams.map(printFormalParam), DocAst.Type.Unknown, DocAst.Eff.AsIs("Unknown"), print(exp))
    }.toList
    DocAst.Program(Nil, defs)
  }

  private def print(exp: ResolvedAst.Expr): DocAst.Expr = exp match {
    case Expr.Var(sym, loc) => printVarSym(sym)
    case Expr.Def(sym, loc) => DocAst.Expr.AsIs(sym.text)
    case Expr.Sig(sym, loc) => DocAst.Expr.AsIs(sym.name)
    case Expr.Hole(sym, loc) => DocAst.Expr.Hole(sym)
    case Expr.HoleWithExp(exp, loc) => DocAst.Expr.HoleWithExp(print(exp))
    case Expr.OpenAs(symUse, exp, loc) => DocAst.Expr.Unknown
    case Expr.Use(sym, alias, exp, loc) => DocAst.Expr.Unknown
    case Expr.Cst(cst, loc) => ConstantPrinter.print(cst)
    case Expr.Apply(exp, exps, loc) => DocAst.Expr.App(print(exp), exps.map(print))
    case Expr.Lambda(fparam, exp, loc) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.Unary(sop, exp, loc) => DocAst.Expr.Unary(OpPrinter.print(sop), print(exp))
    case Expr.Binary(sop, exp1, exp2, loc) => DocAst.Expr.Binary(print(exp1), OpPrinter.print(sop), print(exp2))
    case Expr.IfThenElse(exp1, exp2, exp3, loc) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, loc) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, loc) => DocAst.Expr.Discard(print(exp))
    case Expr.Let(sym, mod, exp1, exp2, loc) => DocAst.Expr.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case Expr.LetRec(sym, ann, mod, exp1, exp2, loc) => DocAst.Expr.LetRec(printVarSym(sym), None, print(exp1), print(exp2))
    case Expr.Region(tpe, loc) => DocAst.Expr.Region
    case Expr.Scope(sym, regionVar, exp, loc) => DocAst.Expr.Scope(printVarSym(sym), print(exp))
    case Expr.Match(exp, rules, loc) => DocAst.Expr.Match(print(exp), rules.map {
      case ResolvedAst.MatchRule(pat, guard, exp) => (printPattern(pat), guard.map(print), print(exp))
    })
    case Expr.TypeMatch(exp, rules, loc) => DocAst.Expr.TypeMatch(print(exp), rules.map{
      case ResolvedAst.TypeMatchRule(sym, tpe, exp) => (printVarSym(sym), DocAst.Type.Unknown, print(exp))
    })
    case Expr.RestrictableChoose(star, exp, rules, loc) => DocAst.Expr.Unknown
    case Expr.Tag(sym, exp, loc) => DocAst.Expr.Tag(sym.sym, List(print(exp)))
    case Expr.RestrictableTag(sym, exp, isOpen, loc) => DocAst.Expr.Unknown
    case Expr.Tuple(exps, loc) => DocAst.Expr.Tuple(exps.map(print))
    case Expr.RecordEmpty(loc) => DocAst.Expr.RecordEmpty
    case Expr.RecordSelect(exp, label, loc) => DocAst.Expr.RecordSelect(label, print(exp))
    case Expr.RecordExtend(label, value, rest, loc) => DocAst.Expr.RecordExtend(label, print(value), print(rest))
    case Expr.RecordRestrict(label, rest, loc) => DocAst.Expr.RecordRestrict(label, print(rest))
    case Expr.ArrayLit(exps, exp, loc) => DocAst.Expr.Unknown
    case Expr.ArrayNew(exp1, exp2, exp3, loc) => DocAst.Expr.Unknown
    case Expr.ArrayLoad(base, index, loc) => DocAst.Expr.ArrayLoad(print(base), print(index))
    case Expr.ArrayStore(base, index, elm, loc) => DocAst.Expr.ArrayStore(print(base), print(index), print(elm))
    case Expr.ArrayLength(base, loc) => DocAst.Expr.ArrayLength(print(base))
    case Expr.StructNew(sym, exps, region, loc) => DocAst.Expr.StructNew(sym, exps.map{
      case (sym, exp) => (sym.sym, print(exp))
    }, print(region))
    case Expr.StructGet(e, sym, loc) => DocAst.Expr.StructGet(print(e), sym.sym)
    case Expr.StructPut(exp1, sym, exp2, loc) => DocAst.Expr.StructPut(print(exp1), sym.sym, print(exp2))
    case Expr.VectorLit(exps, loc) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, loc) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, loc) => DocAst.Expr.VectorLength(print(exp))
    case Expr.Ascribe(exp, expectedType, expectedEff, loc) => DocAst.Expr.Ascription(print(exp), DocAst.Type.Unknown)
    case Expr.InstanceOf(exp, clazz, loc) => DocAst.Expr.InstanceOf(print(exp), clazz)
    case Expr.CheckedCast(cast, exp, loc) => DocAst.Expr.Cast(print(exp), DocAst.Type.Unknown)
    case Expr.UncheckedCast(exp, declaredType, declaredEff, loc) => DocAst.Expr.Cast(print(exp), DocAst.Type.Unknown)
    case Expr.UncheckedMaskingCast(exp, loc) => DocAst.Expr.Cast(print(exp), DocAst.Type.Unknown)
    case Expr.Without(exp, eff, loc) => DocAst.Expr.Without(print(exp), eff.sym)
    case Expr.TryCatch(exp, rules, loc) => DocAst.Expr.TryCatch(print(exp), rules.map{
      case ResolvedAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
    })
    case Expr.Throw(exp, loc) => DocAst.Expr.Throw(print(exp))
    case Expr.TryWith(exp, eff, rules, loc) => DocAst.Expr.TryWith(print(exp), eff.sym, rules.map {
      case ResolvedAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam).toList, print(exp))
    })
    case Expr.Do(op, exps, loc) => DocAst.Expr.Do(op.sym, exps.map(print))
    case Expr.InvokeConstructor2(clazz, exps, loc) => DocAst.Expr.Unknown
    case Expr.InvokeMethod2(exp, methodName, exps, loc) => DocAst.Expr.Unknown
    case Expr.InvokeStaticMethod2(clazz, methodName, exps, loc) => DocAst.Expr.Unknown
    case Expr.GetField2(exp, fieldName, loc) => DocAst.Expr.Unknown
    case Expr.InvokeConstructorOld(constructor, exps, loc) => DocAst.Expr.Unknown
    case Expr.InvokeMethodOld(method, clazz, exp, exps, loc) => DocAst.Expr.Unknown
    case Expr.InvokeStaticMethodOld(method, exps, loc) => DocAst.Expr.Unknown
    case Expr.GetFieldOld(field, clazz, exp, loc) => DocAst.Expr.Unknown
    case Expr.PutField(field, clazz, exp1, exp2, loc) => DocAst.Expr.Unknown
    case Expr.GetStaticField(field, loc) => DocAst.Expr.Unknown
    case Expr.PutStaticField(field, exp, loc) => DocAst.Expr.Unknown
    case Expr.NewObject(name, clazz, methods, loc) => DocAst.Expr.Unknown
    case Expr.NewChannel(exp1, exp2, loc) => DocAst.Expr.Unknown
    case Expr.GetChannel(exp, loc) => DocAst.Expr.Unknown
    case Expr.PutChannel(exp1, exp2, loc) => DocAst.Expr.Unknown
    case Expr.SelectChannel(rules, default, loc) => DocAst.Expr.Unknown
    case Expr.Spawn(exp1, exp2, loc) => DocAst.Expr.Unknown
    case Expr.ParYield(frags, exp, loc) => DocAst.Expr.Unknown
    case Expr.Lazy(exp, loc) => DocAst.Expr.Unknown
    case Expr.Force(exp, loc) => DocAst.Expr.Unknown
    case Expr.FixpointConstraintSet(cs, loc) => DocAst.Expr.Unknown
    case Expr.FixpointLambda(pparams, exp, loc) => DocAst.Expr.Unknown
    case Expr.FixpointMerge(exp1, exp2, loc) => DocAst.Expr.Unknown
    case Expr.FixpointSolve(exp, loc) => DocAst.Expr.Unknown
    case Expr.FixpointFilter(pred, exp, loc) => DocAst.Expr.Unknown
    case Expr.FixpointInject(exp, pred, loc) => DocAst.Expr.Unknown
    case Expr.FixpointProject(pred, exp1, exp2, loc) => DocAst.Expr.Unknown
    case Expr.Error(m) => DocAst.Expr.Error
  }

  private def printPattern(pat: ResolvedAst.Pattern): DocAst.Expr = pat match {
    case Pattern.Wild(loc) => DocAst.Expr.Wild
    case Pattern.Var(sym, loc) => printVarSym(sym)
    case Pattern.Cst(cst, loc) => ConstantPrinter.print(cst)
    case Pattern.Tag(sym, pat, loc) => DocAst.Expr.Tag(sym.sym, List(printPattern(pat)))
    case Pattern.Tuple(pats, loc) => DocAst.Expr.Tuple(pats.map(printPattern))
    case Pattern.Record(pats, pat, loc) => DocAst.Expr.Unknown
    case Pattern.RecordEmpty(loc) => DocAst.Expr.RecordEmpty
    case Pattern.Error(loc) => DocAst.Expr.Error
  }

  /**
    * Returns the [[DocAst.Expr.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ResolvedAst.FormalParam): DocAst.Expr.Ascription = {
    val ResolvedAst.FormalParam(sym, _, _, _) = fp
    DocAst.Expr.Ascription(printVarSym(sym), DocAst.Type.Unknown)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)


}
