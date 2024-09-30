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
import ca.uwaterloo.flix.language.ast.{Ast, ResolvedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst


object ResolvedAstPrinter {

  /** Returns the [[DocAst.Program]] representation of `root`. */
  def print(root: ResolvedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case ResolvedAst.Declaration.Def(sym, spec, exp) =>
        DocAst.Def(spec.ann, spec.mod, sym, spec.fparams.map(printFormalParam), DocAst.Type.Unknown, DocAst.Eff.AsIs("Unknown"), print(exp))
    }.toList
    DocAst.Program(Nil, defs)
  }

  /** Returns the [[DocAst.Expr]] representation of `exp`. */
  private def print(exp: ResolvedAst.Expr): DocAst.Expr = exp match {
    case Expr.Var(sym, _) => printVarSym(sym)
    case Expr.Def(sym, _) => DocAst.Expr.AsIs(sym.text)
    case Expr.Sig(sym, _) => DocAst.Expr.AsIs(sym.name)
    case Expr.Hole(sym, _) => DocAst.Expr.Hole(sym)
    case Expr.HoleWithExp(exp, _) => DocAst.Expr.HoleWithExp(print(exp))
    case Expr.OpenAs(_, _, _) => DocAst.Expr.Unknown
    case Expr.Use(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Cst(cst, _) => ConstantPrinter.print(cst)
    case Expr.Apply(exp, exps, _) => DocAst.Expr.App(print(exp), exps.map(print))
    case Expr.ApplyDef(Ast.DefSymUse(sym, _), exps, _) => DocAst.Expr.ApplyDef(sym, exps.map(print), None)
    case Expr.ApplySig(sym, exps, _) => DocAst.Expr.App(DocAst.Expr.AsIs(sym.name), exps.map(print))
    case Expr.Lambda(fparam, exp, _) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.Unary(sop, exp, _) => DocAst.Expr.Unary(OpPrinter.print(sop), print(exp))
    case Expr.Binary(sop, exp1, exp2, _) => DocAst.Expr.Binary(print(exp1), OpPrinter.print(sop), print(exp2))
    case Expr.IfThenElse(exp1, exp2, exp3, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, _) => DocAst.Expr.Discard(print(exp))
    case Expr.Let(sym, _, exp1, exp2, _) => DocAst.Expr.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case Expr.LetRec(sym, _, _, exp1, exp2, _) => DocAst.Expr.LetRec(printVarSym(sym), None, print(exp1), print(exp2))
    case Expr.Region(_, _) => DocAst.Expr.Region
    case Expr.Scope(sym, _, exp, _) => DocAst.Expr.Scope(printVarSym(sym), print(exp))
    case Expr.Match(exp, rules, _) => DocAst.Expr.Match(print(exp), rules.map {
      case ResolvedAst.MatchRule(pat, guard, exp) => (printPattern(pat), guard.map(print), print(exp))
    })
    case Expr.TypeMatch(exp, rules, _) => DocAst.Expr.TypeMatch(print(exp), rules.map{
      case ResolvedAst.TypeMatchRule(sym, _, exp) => (printVarSym(sym), DocAst.Type.Unknown, print(exp))
    })
    case Expr.RestrictableChoose(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Tag(sym, exp, _) => DocAst.Expr.Tag(sym.sym, List(print(exp)))
    case Expr.RestrictableTag(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.Tuple(exps, _) => DocAst.Expr.Tuple(exps.map(print))
    case Expr.RecordEmpty(_) => DocAst.Expr.RecordEmpty
    case Expr.RecordSelect(exp, label, _) => DocAst.Expr.RecordSelect(label, print(exp))
    case Expr.RecordExtend(label, value, rest, _) => DocAst.Expr.RecordExtend(label, print(value), print(rest))
    case Expr.RecordRestrict(label, rest, _) => DocAst.Expr.RecordRestrict(label, print(rest))
    case Expr.ArrayLit(_, _, _) => DocAst.Expr.Unknown
    case Expr.ArrayNew(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.ArrayLoad(base, index, _) => DocAst.Expr.ArrayLoad(print(base), print(index))
    case Expr.ArrayStore(base, index, elm, _) => DocAst.Expr.ArrayStore(print(base), print(index), print(elm))
    case Expr.ArrayLength(base, _) => DocAst.Expr.ArrayLength(print(base))
    case Expr.StructNew(sym, exps, region, _) => DocAst.Expr.StructNew(sym, exps.map{
      case (sym, exp) => (sym.sym, print(exp))
    }, print(region))
    case Expr.StructGet(e, sym, _) => DocAst.Expr.StructGet(print(e), sym.sym)
    case Expr.StructPut(exp1, sym, exp2, _) => DocAst.Expr.StructPut(print(exp1), sym.sym, print(exp2))
    case Expr.VectorLit(exps, _) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, _) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, _) => DocAst.Expr.VectorLength(print(exp))
    case Expr.Ascribe(exp, _, _, _) => DocAst.Expr.Ascription(print(exp), DocAst.Type.Unknown)
    case Expr.InstanceOf(exp, clazz, _) => DocAst.Expr.InstanceOf(print(exp), clazz)
    case Expr.CheckedCast(_, exp, _) => DocAst.Expr.Cast(print(exp), DocAst.Type.Unknown)
    case Expr.UncheckedCast(exp, _, _, _) => DocAst.Expr.Cast(print(exp), DocAst.Type.Unknown)
    case Expr.UncheckedMaskingCast(exp, _) => DocAst.Expr.Cast(print(exp), DocAst.Type.Unknown)
    case Expr.Without(exp, eff, _) => DocAst.Expr.Without(print(exp), eff.sym)
    case Expr.TryCatch(exp, rules, _) => DocAst.Expr.TryCatch(print(exp), rules.map{
      case ResolvedAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
    })
    case Expr.Throw(exp, _) => DocAst.Expr.Throw(print(exp))
    case Expr.TryWith(exp, eff, rules, _) => DocAst.Expr.TryWith(print(exp), eff.sym, rules.map {
      case ResolvedAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam).toList, print(exp))
    })
    case Expr.Do(op, exps, _) => DocAst.Expr.Do(op.sym, exps.map(print))
    case Expr.InvokeConstructor2(_, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeMethod2(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeStaticMethod2(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.GetField2(_, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeConstructorOld(_, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeMethodOld(_, _, _, _, _) => DocAst.Expr.Unknown
    case Expr.InvokeStaticMethodOld(_, _, _) => DocAst.Expr.Unknown
    case Expr.GetFieldOld(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.PutField(_, _, _, _, _) => DocAst.Expr.Unknown
    case Expr.GetStaticField(_, _) => DocAst.Expr.Unknown
    case Expr.PutStaticField(_, _, _) => DocAst.Expr.Unknown
    case Expr.NewObject(_, _, _, _) => DocAst.Expr.Unknown
    case Expr.NewChannel(_, _, _) => DocAst.Expr.Unknown
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
    case Pattern.Tag(sym, pat, _) => DocAst.Expr.Tag(sym.sym, List(printPattern(pat)))
    case Pattern.Tuple(pats, _) => DocAst.Expr.Tuple(pats.map(printPattern))
    case Pattern.Record(_, _, _) => DocAst.Expr.Unknown
    case Pattern.RecordEmpty(_) => DocAst.Expr.RecordEmpty
    case Pattern.Error(_) => DocAst.Expr.Error
  }

  /** Returns the [[DocAst.Expr.Ascription]] representation of `fp`. */
  private def printFormalParam(fp: ResolvedAst.FormalParam): DocAst.Expr.Ascription = {
    val ResolvedAst.FormalParam(sym, _, _, _) = fp
    DocAst.Expr.Ascription(printVarSym(sym), DocAst.Type.Unknown)
  }

  /** Returns the [[DocAst.Expr]] representation of `sym`. */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)


}
