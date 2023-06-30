/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object SimplifiedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: SimplifiedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case SimplifiedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case SimplifiedAst.Case(sym, tpe, _) =>
            DocAst.Case(sym, TypePrinter.print(tpe))
        }.toList
        DocAst.Enum(ann, mod, sym, Nil, cases)
    }.toList
    val defs = root.defs.values.map {
      case SimplifiedAst.Def(ann, mod, sym, formals, exp, tpe, _, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          formals.map(printFormalParam),
          DocAst.Type.Arrow(Nil, TypePrinter.print(tpe)),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: SimplifiedAst.Expression): DocAst.Expression = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case Def(sym, _, _) => DocAst.Expression.Def(sym)
    case Lambda(fparams, exp, _, _) => DocAst.Expression.Lambda(fparams.map(printFormalParam), print(exp))
    case Apply(exp, args, _, _, _) => DocAst.Expression.App(print(exp), args.map(print))
    case LambdaClosure(cparams, fparams, _, exp, _, _) => DocAst.Expression.Lambda((cparams ++ fparams).map(printFormalParam), print(exp))
    case ApplyAtomic(op, exps, tpe, _, loc) => DocAst.Expression.fromAtomic(op)(exps.map(print), TypePrinter.print(tpe), loc)
    case ApplyClo(exp, args, _, _, _) => DocAst.Expression.ApplyClo(print(exp), args.map(print))
    case ApplyDef(sym, args, _, _, _) => DocAst.Expression.ApplyDef(sym, args.map(print))
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _, _) => DocAst.Expression.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Expression.Let(printVarSym(sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case LetRec(sym, exp1, exp2, _, _, _) => DocAst.Expression.LetRec(printVarSym(sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Scope(sym, exp, _, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case Ref(exp, _, _) => DocAst.Expression.Ref(print(exp))
    case Deref(exp, _, _) => DocAst.Expression.Deref(print(exp))
    case Assign(exp1, exp2, _, _) => DocAst.Expression.Assign(print(exp1), print(exp2))
    case InstanceOf(exp, clazz, _) => DocAst.Expression.InstanceOf(print(exp), clazz)
    case Cast(exp, tpe, _, _) => DocAst.Expression.Cast(print(exp), TypePrinter.print(tpe))
    case TryCatch(exp, rules, _, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map {
      case SimplifiedAst.CatchRule(sym, clazz, exp) =>
        (sym, clazz, print(exp))
    })
    case TryWith(exp, effUse, rules, _, _, _) => DocAst.Expression.TryWith(print(exp), effUse.sym, rules.map {
      case SimplifiedAst.HandlerRule(op, fparams, exp) =>
        (op.sym, fparams.map(printFormalParam), print(exp))
    })
    case Do(op, exps, _, _, _) => DocAst.Expression.Do(op.sym, exps.map(print))
    case Resume(exp, _, _) => DocAst.Expression.Resume(print(exp))
    case InvokeConstructor(constructor, args, _, _, _) => DocAst.Expression.JavaInvokeConstructor(constructor, args.map(print))
    case InvokeMethod(method, exp, args, _, _, _) => DocAst.Expression.JavaInvokeMethod(method, print(exp), args.map(print))
    case InvokeStaticMethod(method, args, _, _, _) => DocAst.Expression.JavaInvokeStaticMethod(method, args.map(print))
    case GetField(field, exp, _, _, _) => DocAst.Expression.JavaGetField(field, print(exp))
    case PutField(field, exp1, exp2, _, _, _) => DocAst.Expression.JavaPutField(field, print(exp1), print(exp2))
    case GetStaticField(field, _, _, _) => DocAst.Expression.JavaGetStaticField(field)
    case PutStaticField(field, exp, _, _, _) => DocAst.Expression.JavaPutStaticField(field, print(exp))
    case NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expression.NewObject(name, clazz, TypePrinter.print(tpe), methods.map {
      case SimplifiedAst.JvmMethod(ident, fparams, exp, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
    })
    case Spawn(exp1, exp2, _, _) => DocAst.Expression.Spawn(print(exp1), print(exp2))
    case Lazy(exp, _, _) => DocAst.Expression.Lazy(print(exp))
    case Force(exp, _, _) => DocAst.Expression.Force(print(exp))
    case HoleError(sym, _, _) => DocAst.Expression.HoleError(sym)
    case MatchError(_, _) => DocAst.Expression.MatchError
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: SimplifiedAst.FormalParam): DocAst.Expression.Ascription = {
    val SimplifiedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expression.Ascription(printVarSym(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.Var(sym)

}
