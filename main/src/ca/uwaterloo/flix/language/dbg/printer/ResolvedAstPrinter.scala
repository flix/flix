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

import ca.uwaterloo.flix.language.ast.ResolvedAst.{Expr, Pattern}
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

  /** Returns the [[DocAst.Expr]] representation of `exp`. */
  private def print(exp: ResolvedAst.Expr): DocAst.Expr = exp match {
    case Expr.Var(sym, _) => printVarSym(sym)
    case Expr.Hole(sym, _, _) => DocAst.Expr.Hole(sym)
    case Expr.HoleWithExp(exp, _, _) => DocAst.Expr.HoleWithExp(print(exp))
    case Expr.OpenAs(_, _, _) => DocAst.Expr.Unknown
    case Expr.Use(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Cst(cst, _) => ConstantPrinter.print(cst)
    case Expr.ApplyClo(exp, exps, _) => DocAst.Expr.App(print(exp), exps.map(print))
    case Expr.ApplyDef(DefSymUse(sym, _), exps, _) => DocAst.Expr.ApplyDef(sym, exps.map(print))
    case Expr.ApplyLocalDef(LocalDefSymUse(sym, _), exps, _) => DocAst.Expr.App(printVarSym(sym), exps.map(print))
    case Expr.ApplySig(SigSymUse(sym, _), exps, _) => DocAst.Expr.App(DocAst.Expr.AsIs(sym.name), exps.map(print))
    case Expr.Lambda(fparam, exp, _, _) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.Unary(sop, exp, _) => DocAst.Expr.Unary(OpPrinter.print(sop), print(exp))
    case Expr.Binary(sop, exp1, exp2, _) => DocAst.Expr.Binary(print(exp1), OpPrinter.print(sop), print(exp2))
    case Expr.IfThenElse(exp1, exp2, exp3, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, _) => DocAst.Expr.Discard(print(exp))
    case Expr.Let(sym, exp1, exp2, _) => DocAst.Expr.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case Expr.LocalDef(sym, fparams, exp1, exp2, _) => DocAst.Expr.LocalDef(DocAst.Expr.Var(sym), fparams.map(printFormalParam), None, None, print(exp1), print(exp2))
    case Expr.Region(_, _) => DocAst.Expr.Region
    case Expr.Scope(sym, _, exp, _) => DocAst.Expr.Scope(printVarSym(sym), print(exp))
    case Expr.Match(exp, rules, _) => DocAst.Expr.Match(print(exp), rules.map {
      case ResolvedAst.MatchRule(pat, guard, exp, _) => (printPattern(pat), guard.map(print), print(exp))
    })
    case Expr.TypeMatch(exp, rules, _) => DocAst.Expr.TypeMatch(print(exp), rules.map {
      case ResolvedAst.TypeMatchRule(sym, tpe, exp, loc) => (printVarSym(sym), UnkindedTypePrinter.print(tpe), print(exp))
    })
    case Expr.RestrictableChoose(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Tag(symUse, exps, _) => DocAst.Expr.Tag(symUse.sym, exps.map(print))
    case Expr.RestrictableTag(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Tuple(exps, _) => DocAst.Expr.Tuple(exps.map(print))
    case Expr.RecordSelect(exp, label, _) => DocAst.Expr.RecordSelect(label, print(exp))
    case Expr.RecordExtend(label, value, rest, _) => DocAst.Expr.RecordExtend(label, print(value), print(rest))
    case Expr.RecordRestrict(label, rest, _) => DocAst.Expr.RecordRestrict(label, print(rest))
    case Expr.ArrayLit(exps, exp, _) => DocAst.Expr.InRegion(DocAst.Expr.ArrayLit(exps.map(print)), print(exp))
    case Expr.ArrayNew(exp1, exp2, exp3, _) => DocAst.Expr.InRegion(DocAst.Expr.ArrayNew(print(exp1), print(exp2)), print(exp3))
    case Expr.ArrayLoad(base, index, _) => DocAst.Expr.ArrayLoad(print(base), print(index))
    case Expr.ArrayStore(base, index, elm, _) => DocAst.Expr.ArrayStore(print(base), print(index), print(elm))
    case Expr.ArrayLength(base, _) => DocAst.Expr.ArrayLength(print(base))
    case Expr.StructNew(sym, exps, region, _) => DocAst.Expr.StructNew(sym, exps.map {
      case (symUse, exp) => (symUse.sym, print(exp))
    }, print(region))
    case Expr.StructGet(exp, symUse, _) => DocAst.Expr.StructGet(print(exp), symUse.sym)
    case Expr.StructPut(exp1, symUse, exp2, _) => DocAst.Expr.StructPut(print(exp1), symUse.sym, print(exp2))
    case Expr.VectorLit(exps, _) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, _) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, _) => DocAst.Expr.VectorLength(print(exp))
    case Expr.Ascribe(exp, tpe, eff, _) => DocAst.Expr.AscriptionEff(print(exp), tpe.map(UnkindedTypePrinter.print), eff.map(UnkindedTypePrinter.print))
    case Expr.InstanceOf(exp, clazz, _) => DocAst.Expr.InstanceOf(print(exp), clazz)
    case Expr.CheckedCast(cast, exp, _) => DocAst.Expr.CheckedCast(cast, print(exp))
    case Expr.UncheckedCast(exp, tpe, eff, _) => DocAst.Expr.UncheckedCast(print(exp), tpe.map(UnkindedTypePrinter.print), eff.map(UnkindedTypePrinter.print))
    case Expr.Unsafe(exp, runEff, _) => DocAst.Expr.Unsafe(print(exp), UnkindedTypePrinter.print(runEff))
    case Expr.Without(exp, symUse, _) => DocAst.Expr.Without(print(exp), symUse.sym)
    case Expr.TryCatch(exp, rules, _) => DocAst.Expr.TryCatch(print(exp), rules.map {
      case ResolvedAst.CatchRule(sym, clazz, exp, _) => (sym, clazz, print(exp))
    })
    case Expr.Throw(exp, _) => DocAst.Expr.Throw(print(exp))
    case Expr.Handler(symUse, rules, _) => DocAst.Expr.Handler(symUse.sym, rules.map {
      case ResolvedAst.HandlerRule(symUse, fparams, exp, _) => (symUse.sym, fparams.map(printFormalParam), print(exp))
    })
    case Expr.RunWith(exp1, exp2, _) => DocAst.Expr.RunWith(print(exp1), print(exp2))
    case Expr.Do(symUse, exps, _) => DocAst.Expr.Do(symUse.sym, exps.map(print))
    case Expr.InvokeConstructor(_, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeMethod(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeStaticMethod(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.GetField(_, _, _) => DocAst.Expr.Unknown
    case Expr.PutField(_, _, _, _, _) => DocAst.Expr.Unknown
    case Expr.GetStaticField(_, _) => DocAst.Expr.Unknown
    case Expr.PutStaticField(_, _, _) => DocAst.Expr.Unknown
    case Expr.NewObject(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.NewChannel(_, _) => DocAst.Expr.Unknown
    case Expr.GetChannel(_, _) => DocAst.Expr.Unknown
    case Expr.PutChannel(_, _, _) => DocAst.Expr.Unknown
    case Expr.SelectChannel(_, _, _) => DocAst.Expr.Unknown
    case Expr.Spawn(_, _, _) => DocAst.Expr.Unknown
    case Expr.ParYield(_, _, _) => DocAst.Expr.Unknown
    case Expr.Lazy(_, _) => DocAst.Expr.Unknown
    case Expr.Force(_, _) => DocAst.Expr.Unknown
    case Expr.FixpointConstraintSet(_, _) => DocAst.Expr.Unknown
    case Expr.FixpointLambda(_, _, _) => DocAst.Expr.Unknown
    case Expr.FixpointMerge(_, _, _) => DocAst.Expr.Unknown
    case Expr.FixpointSolve(_, _) => DocAst.Expr.Unknown
    case Expr.FixpointFilter(_, _, _) => DocAst.Expr.Unknown
    case Expr.FixpointInject(_, _, _) => DocAst.Expr.Unknown
    case Expr.FixpointProject(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Error(_) => DocAst.Expr.Error
  }

  /** Returns the [[DocAst.Expr]] representation of `pat`. */
  private def printPattern(pat: ResolvedAst.Pattern): DocAst.Expr = pat match {
    case Pattern.Wild(_) => DocAst.Expr.Wild
    case Pattern.Var(sym, _) => printVarSym(sym)
    case Pattern.Cst(cst, _) => ConstantPrinter.print(cst)
    case Pattern.Tag(symUse, pats, _) => DocAst.Expr.Tag(symUse.sym, pats.map(printPattern))
    case Pattern.Tuple(pats, _) => DocAst.Expr.Tuple(pats.map(printPattern))
    case Pattern.Record(_, _, _) => DocAst.Expr.Unknown
    case Pattern.Error(_) => DocAst.Expr.Error
  }

  /** Returns the [[DocAst.Expr.AscriptionTpe]] representation of `fp`. */
  private def printFormalParam(fp: ResolvedAst.FormalParam): DocAst.Expr.AscriptionTpe = fp match {
    case ResolvedAst.FormalParam(sym, _, Some(tpe), _) =>
      DocAst.Expr.AscriptionTpe(printVarSym(sym), UnkindedTypePrinter.print(tpe))
    case ResolvedAst.FormalParam(sym, _, None, _) =>
      DocAst.Expr.AscriptionTpe(printVarSym(sym), DocAst.Type.Wild)
  }

  /** Returns the [[DocAst.Expr]] representation of `sym`. */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)


}
