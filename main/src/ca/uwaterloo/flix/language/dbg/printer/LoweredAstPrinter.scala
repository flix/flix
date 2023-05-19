/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.LoweredAst
import ca.uwaterloo.flix.language.ast.LoweredAst.Expression
import ca.uwaterloo.flix.language.dbg.DocAst

class LoweredAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: LoweredAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case LoweredAst.Enum(_, ann, mod, sym, _, _, cases0, _, _) =>
        val cases = cases0.values.map {
          case LoweredAst.Case(sym, _, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case LoweredAst.Def(sym, LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, pur, tconstrs, loc), LoweredAst.Impl(exp, inferredScheme)) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          fparams.map(printFormalParam),
          TypePrinter.print(retTpe),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: LoweredAst.Expression): DocAst.Expression = e match {
    case Expression.Cst(cst, tpe, loc) => ConstantPrinter.print(cst)
    case Expression.Wild(tpe, loc) =>
    case Expression.Var(sym, tpe, loc) => ???
    case Expression.Def(sym, tpe, loc) => ???
    case Expression.Sig(sym, tpe, loc) => ???
    case Expression.Hole(sym, tpe, loc) => ???
    case Expression.Lambda(fparam, exp, tpe, loc) => ???
    case Expression.Apply(exp, exps, tpe, pur, loc) => ???
    case Expression.Unary(sop, exp, tpe, pur, loc) => ???
    case Expression.Binary(sop, exp1, exp2, tpe, pur, loc) => ???
    case Expression.Let(sym, mod, exp1, exp2, tpe, pur, loc) => ???
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, loc) => ???
    case Expression.Region(tpe, loc) => ???
    case Expression.Scope(sym, regionVar, exp, tpe, pur, loc) => ???
    case Expression.ScopeExit(exp1, exp2, tpe, pur, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, loc) => ???
    case Expression.Stm(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Discard(exp, pur, loc) => ???
    case Expression.Match(exp, rules, tpe, pur, loc) => ???
    case Expression.TypeMatch(exp, rules, tpe, pur, loc) => ???
    case Expression.RelationalChoose(exps, rules, tpe, pur, loc) => ???
    case Expression.Tag(sym, exp, tpe, pur, loc) => ???
    case Expression.Tuple(elms, tpe, pur, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, pur, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, pur, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, pur, loc) => ???
    case Expression.ArrayLit(exps, exp, tpe, pur, loc) => ???
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, pur, loc) => ???
    case Expression.ArrayLength(base, pur, loc) => ???
    case Expression.ArrayStore(base, index, elm, pur, loc) => ???
    case Expression.VectorLit(exps, tpe, pur, loc) => ???
    case Expression.VectorLoad(exp1, exp2, tpe, pur, loc) => ???
    case Expression.VectorLength(exp, loc) => ???
    case Expression.Ref(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Deref(exp, tpe, pur, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Ascribe(exp, tpe, pur, loc) => ???
    case Expression.InstanceOf(exp, clazz, loc) => ???
    case Expression.Cast(exp, declaredType, declaredPur, tpe, pur, loc) => ???
    case Expression.Without(exp, effUse, tpe, pur, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, pur, loc) => ???
    case Expression.TryWith(exp, effUse, rules, tpe, pur, loc) => ???
    case Expression.Do(op, exps, pur, loc) => ???
    case Expression.Resume(exp, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, pur, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, pur, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, pur, loc) => ???
    case Expression.GetField(field, exp, tpe, pur, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, pur, loc) => ???
    case Expression.GetStaticField(field, tpe, pur, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, pur, loc) => ???
    case Expression.NewObject(name, clazz, tpe, pur, methods, loc) => ???
    case Expression.Spawn(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, pur, loc) => ???
  }
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `stmt`.
    */
  def print(stmt: LoweredAst.Stmt): DocAst.Expression = stmt match {
    case Stmt.Ret(expr, _, _) => DocAst.Expression.Ret(print(expr))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LoweredAst.FormalParam): DocAst.Expression.Ascription = {
    val LoweredAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expression.Ascription(printVarSym(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.Var(sym)
}
