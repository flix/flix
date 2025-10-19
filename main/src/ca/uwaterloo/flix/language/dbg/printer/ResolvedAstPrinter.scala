/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.ResolvedAst.{Exp, ExtPattern, ExtTagPattern, Pattern}
import ca.uwaterloo.flix.language.ast.shared.SymUse.{DefSymUse, LocalDefSymUse, SigSymUse}
import ca.uwaterloo.flix.language.ast.{ResolvedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst


object ResolvedAstPrinter {

  /** Returns the [[DocAst.Program]] representation of `root`. */
  def print(root: ResolvedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case ResolvedAst.Declaration.Def(sym, spec, exp, _) =>
        DocAst.Def(
          spec.ann,
          spec.mod,
          sym,
          spec.fparams.map(printFormalParam),
          UnkindedTypePrinter.print(spec.tpe),
          spec.eff.map(UnkindedTypePrinter.print).getOrElse(DocAst.Type.Pure),
          print(exp)
        )
    }.toList
    DocAst.Program(Nil, defs, Nil)
  }

  /** Returns the [[DocAst.Exp]] representation of `exp`. */
  private def print(exp0: ResolvedAst.Exp): DocAst.Exp = exp0 match {
    case Exp.Var(sym, _) => printVarSym(sym)
    case Exp.Hole(sym, _, _) => DocAst.Exp.Hole(sym)
    case Exp.HoleWithExp(exp, _, _) => DocAst.Exp.HoleWithExp(print(exp))
    case Exp.OpenAs(_, _, _) => DocAst.Exp.Unknown
    case Exp.Use(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.Cst(cst, _) => ConstantPrinter.print(cst)
    case Exp.ApplyClo(exp1, exp2, _) => DocAst.Exp.App(print(exp1), List(print(exp2)))
    case Exp.ApplyDef(DefSymUse(sym, _), exps, _) => DocAst.Exp.ApplyDef(sym, exps.map(print))
    case Exp.ApplyLocalDef(LocalDefSymUse(sym, _), exps, _) => DocAst.Exp.App(printVarSym(sym), exps.map(print))
    case Exp.ApplyOp(symUse, exps, _) => DocAst.Exp.ApplyOp(symUse.sym, exps.map(print))
    case Exp.ApplySig(SigSymUse(sym, _), exps, _) => DocAst.Exp.App(DocAst.Exp.AsIs(sym.name), exps.map(print))
    case Exp.Lambda(fparam, exp, _, _) => DocAst.Exp.Lambda(List(printFormalParam(fparam)), print(exp))
    case Exp.Unary(sop, exp, _) => DocAst.Exp.Unary(OpPrinter.print(sop), print(exp))
    case Exp.Binary(sop, exp1, exp2, _) => DocAst.Exp.Binary(print(exp1), OpPrinter.print(sop), print(exp2))
    case Exp.IfThenElse(exp1, exp2, exp3, _) => DocAst.Exp.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Exp.Stm(exp1, exp2, _) => DocAst.Exp.Stm(print(exp1), print(exp2))
    case Exp.Discard(exp, _) => DocAst.Exp.Discard(print(exp))
    case Exp.Let(sym, exp1, exp2, _) => DocAst.Exp.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case Exp.LocalDef(sym, fparams, exp1, exp2, _) => DocAst.Exp.LocalDef(DocAst.Exp.Var(sym), fparams.map(printFormalParam), None, None, print(exp1), print(exp2))
    case Exp.Region(sym, _, exp, _) => DocAst.Exp.Region(printVarSym(sym), print(exp))
    case Exp.Match(exp, rules, _) => DocAst.Exp.Match(print(exp), rules.map {
      case ResolvedAst.MatchRule(pat, guard, body, _) => (printPattern(pat), guard.map(print), print(body))
    })
    case Exp.TypeMatch(exp, rules, _) => DocAst.Exp.TypeMatch(print(exp), rules.map {
      case ResolvedAst.TypeMatchRule(sym, tpe, body, _) => (printVarSym(sym), UnkindedTypePrinter.print(tpe), print(body))
    })
    case Exp.RestrictableChoose(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.ExtMatch(exp, rules, _) => DocAst.Exp.ExtMatch(print(exp), rules.map(printExtMatchRule))
    case Exp.Tag(symUse, exps, _) => DocAst.Exp.Tag(symUse.sym, exps.map(print))
    case Exp.RestrictableTag(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.ExtTag(label, exps, _) => DocAst.Exp.ExtTag(label, exps.map(print))
    case Exp.Tuple(exps, _) => DocAst.Exp.Tuple(exps.map(print))
    case Exp.RecordSelect(exp, label, _) => DocAst.Exp.RecordSelect(label, print(exp))
    case Exp.RecordExtend(label, value, rest, _) => DocAst.Exp.RecordExtend(label, print(value), print(rest))
    case Exp.RecordRestrict(label, rest, _) => DocAst.Exp.RecordRestrict(label, print(rest))
    case Exp.ArrayLit(exps, exp, _) => DocAst.Exp.InRegion(DocAst.Exp.ArrayLit(exps.map(print)), print(exp))
    case Exp.ArrayNew(exp1, exp2, exp3, _) => DocAst.Exp.InRegion(DocAst.Exp.ArrayNew(print(exp1), print(exp2)), print(exp3))
    case Exp.ArrayLoad(base, index, _) => DocAst.Exp.ArrayLoad(print(base), print(index))
    case Exp.ArrayStore(base, index, elm, _) => DocAst.Exp.ArrayStore(print(base), print(index), print(elm))
    case Exp.ArrayLength(base, _) => DocAst.Exp.ArrayLength(print(base))
    case Exp.StructNew(sym, exps, region, _) => DocAst.Exp.StructNew(sym, exps.map {
      case (symUse, exp) => (symUse.sym, print(exp))
    }, print(region))
    case Exp.StructGet(exp, symUse, _) => DocAst.Exp.StructGet(print(exp), symUse.sym)
    case Exp.StructPut(exp1, symUse, exp2, _) => DocAst.Exp.StructPut(print(exp1), symUse.sym, print(exp2))
    case Exp.VectorLit(exps, _) => DocAst.Exp.VectorLit(exps.map(print))
    case Exp.VectorLoad(exp1, exp2, _) => DocAst.Exp.VectorLoad(print(exp1), print(exp2))
    case Exp.VectorLength(exp, _) => DocAst.Exp.VectorLength(print(exp))
    case Exp.Ascribe(exp, tpe, eff, _) => DocAst.Exp.AscriptionEff(print(exp), tpe.map(UnkindedTypePrinter.print), eff.map(UnkindedTypePrinter.print))
    case Exp.InstanceOf(exp, clazz, _) => DocAst.Exp.InstanceOf(print(exp), clazz)
    case Exp.CheckedCast(cast, exp, _) => DocAst.Exp.CheckedCast(cast, print(exp))
    case Exp.UncheckedCast(exp, tpe, eff, _) => DocAst.Exp.UncheckedCast(print(exp), tpe.map(UnkindedTypePrinter.print), eff.map(UnkindedTypePrinter.print))
    case Exp.Unsafe(exp, runEff, _) => DocAst.Exp.Unsafe(print(exp), UnkindedTypePrinter.print(runEff))
    case Exp.Without(exp, symUse, _) => DocAst.Exp.Without(print(exp), symUse.sym)
    case Exp.TryCatch(exp, rules, _) => DocAst.Exp.TryCatch(print(exp), rules.map {
      case ResolvedAst.CatchRule(sym, clazz, body, _) => (sym, clazz, print(body))
    })
    case Exp.Throw(exp, _) => DocAst.Exp.Throw(print(exp))
    case Exp.Handler(symUse, rules, _) => DocAst.Exp.Handler(symUse.sym, rules.map {
      case ResolvedAst.HandlerRule(opSymUse, fparams, exp, _) => (opSymUse.sym, fparams.map(printFormalParam), print(exp))
    })
    case Exp.RunWith(exp1, exp2, _) => DocAst.Exp.RunWith(print(exp1), print(exp2))
    case Exp.InvokeConstructor(_, _, _) => DocAst.Exp.Unknown
    case Exp.InvokeMethod(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.InvokeStaticMethod(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.GetField(_, _, _) => DocAst.Exp.Unknown
    case Exp.PutField(_, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.GetStaticField(_, _) => DocAst.Exp.Unknown
    case Exp.PutStaticField(_, _, _) => DocAst.Exp.Unknown
    case Exp.NewObject(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.NewChannel(_, _) => DocAst.Exp.Unknown
    case Exp.GetChannel(_, _) => DocAst.Exp.Unknown
    case Exp.PutChannel(_, _, _) => DocAst.Exp.Unknown
    case Exp.SelectChannel(_, _, _) => DocAst.Exp.Unknown
    case Exp.Spawn(_, _, _) => DocAst.Exp.Unknown
    case Exp.ParYield(_, _, _) => DocAst.Exp.Unknown
    case Exp.Lazy(_, _) => DocAst.Exp.Unknown
    case Exp.Force(_, _) => DocAst.Exp.Unknown
    case Exp.FixpointConstraintSet(_, _) => DocAst.Exp.Unknown
    case Exp.FixpointLambda(_, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointMerge(_, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointQueryWithProvenance(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointQueryWithSelect(_, _, _, _, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointSolveWithProject(_, _, _, _) => DocAst.Exp.Unknown
    case Exp.FixpointInjectInto(_, _, _) => DocAst.Exp.Unknown
    case Exp.Error(_) => DocAst.Exp.Error
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printExtMatchRule(rule: ResolvedAst.ExtMatchRule): (DocAst.Exp, DocAst.Exp) = rule match {
    case ResolvedAst.ExtMatchRule(pat, exp, _) =>
      (printExtPattern(pat), print(exp))
  }

  /** Returns the [[DocAst.Exp]] representation of `pat`. */
  private def printPattern(pat: ResolvedAst.Pattern): DocAst.Exp = pat match {
    case Pattern.Wild(_) => DocAst.Exp.Wild
    case Pattern.Var(sym, _) => printVarSym(sym)
    case Pattern.Cst(cst, _) => ConstantPrinter.print(cst)
    case Pattern.Tag(symUse, pats, _) => DocAst.Exp.Tag(symUse.sym, pats.map(printPattern))
    case Pattern.Tuple(pats, _) => DocAst.Exp.Tuple(pats.map(printPattern).toList)
    case Pattern.Record(_, _, _) => DocAst.Exp.Unknown
    case Pattern.Error(_) => DocAst.Exp.Error
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pat`.
    */
  private def printExtPattern(pat: ResolvedAst.ExtPattern): DocAst.Exp = pat match {
    case ExtPattern.Default(_) => DocAst.Exp.Wild
    case ExtPattern.Tag(label, pats, _) => DocAst.Pattern.ExtTag(label, pats.map(printExtTagPattern))
    case ExtPattern.Error(_) => DocAst.Exp.Error
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pat`.
    */
  private def printExtTagPattern(pat: ResolvedAst.ExtTagPattern): DocAst.Exp = pat match {
    case ExtTagPattern.Wild(_) => DocAst.Exp.Wild
    case ExtTagPattern.Var(sym, _) => DocAst.Exp.Var(sym)
    case ExtTagPattern.Unit(_) => DocAst.Exp.Unit
    case ExtTagPattern.Error(_) => DocAst.Exp.Error
  }

  /** Returns the [[DocAst.Exp.AscriptionTpe]] representation of `fp`. */
  private def printFormalParam(fp: ResolvedAst.FormalParam): DocAst.Exp.AscriptionTpe = fp match {
    case ResolvedAst.FormalParam(sym, Some(tpe), _) =>
      DocAst.Exp.AscriptionTpe(printVarSym(sym), UnkindedTypePrinter.print(tpe))
    case ResolvedAst.FormalParam(sym, None, _) =>
      DocAst.Exp.AscriptionTpe(printVarSym(sym), DocAst.Type.Wild)
  }

  /** Returns the [[DocAst.Exp]] representation of `sym`. */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Exp =
    DocAst.Exp.Var(sym)


}
