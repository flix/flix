/*
 * Copyright 2015-2016 Ming-Ho Yee
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.LiftedAst.SelectChannelRule
import ca.uwaterloo.flix.language.ast.SimplifiedAst.ConstraintParam
import ca.uwaterloo.flix.language.ast.{Ast, LiftedAst, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object LambdaLift extends Phase[SimplifiedAst.Root, LiftedAst.Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, LiftedAst.Def]

  /**
    * Performs lambda lifting on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[LiftedAst.Root, CompilationError] = flix.phase("LambdaLift") {
    // A mutable map to hold lambdas that are lifted to the top level.
    val m: TopLevel = mutable.Map.empty

    val newDefs = root.defs.map {
      case (sym, decl) => sym -> liftDef(decl, m)
    }

    val newEnums = root.enums.map {
      case (sym, enum0) => sym -> visitEnum(enum0)
    }

    LiftedAst.Root(
      newDefs ++ m,
      newEnums,
      root.reachable,
      root.sources
    ).toSuccess
  }

  /**
    * Performs lambda lifting on the given definition `def0`.
    */
  private def liftDef(def0: SimplifiedAst.Def, m: TopLevel)(implicit flix: Flix): LiftedAst.Def = def0 match {
    case SimplifiedAst.Def(ann, mod, sym, fparams, exp, tpe, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = liftExp(def0.sym.namespace, def0.exp, def0.sym.name, m)

      LiftedAst.Def(ann, mod, sym, fs, e, tpe, loc)
  }

  /**
    * Translates the given simplified enum declaration `enum0` into a lifted enum declaration.
    */
  private def visitEnum(enum0: SimplifiedAst.Enum): LiftedAst.Enum = enum0 match {
    case SimplifiedAst.Enum(mod, sym, cases, tpeDeprecated, loc) =>
      val cs = cases.map {
        case (tag, SimplifiedAst.Case(_, _, tpeDeprecated, loc)) => tag -> LiftedAst.Case(sym, tag, tpeDeprecated, loc)
      }
      LiftedAst.Enum(mod, sym, cs, tpeDeprecated, loc)
  }

  /**
    * Performs lambda lifting on the given expression `exp0` using the given `name` as part of the lifted name.
    */
  private def liftExp(ns: List[String], exp0: SimplifiedAst.Expression, name: String, m: TopLevel)(implicit flix: Flix): LiftedAst.Expression = {
    /**
      * Performs closure conversion and lambda lifting on the given expression `exp0`.
      */
    def visitExp(e: SimplifiedAst.Expression): LiftedAst.Expression = e match {
      case SimplifiedAst.Expression.Unit(loc) => LiftedAst.Expression.Unit(loc)

      case SimplifiedAst.Expression.Null(tpe, loc) => LiftedAst.Expression.Null(tpe, loc)

      case SimplifiedAst.Expression.True(loc) => LiftedAst.Expression.True(loc)

      case SimplifiedAst.Expression.False(loc) => LiftedAst.Expression.False(loc)

      case SimplifiedAst.Expression.Char(lit, loc) => LiftedAst.Expression.Char(lit, loc)

      case SimplifiedAst.Expression.Float32(lit, loc) => LiftedAst.Expression.Float32(lit, loc)

      case SimplifiedAst.Expression.Float64(lit, loc) => LiftedAst.Expression.Float64(lit, loc)

      case SimplifiedAst.Expression.Int8(lit, loc) => LiftedAst.Expression.Int8(lit, loc)

      case SimplifiedAst.Expression.Int16(lit, loc) => LiftedAst.Expression.Int16(lit, loc)

      case SimplifiedAst.Expression.Int32(lit, loc) => LiftedAst.Expression.Int32(lit, loc)

      case SimplifiedAst.Expression.Int64(lit, loc) => LiftedAst.Expression.Int64(lit, loc)

      case SimplifiedAst.Expression.BigInt(lit, loc) => LiftedAst.Expression.BigInt(lit, loc)

      case SimplifiedAst.Expression.Str(lit, loc) => LiftedAst.Expression.Str(lit, loc)

      case SimplifiedAst.Expression.Var(sym, tpe, loc) => LiftedAst.Expression.Var(sym, tpe, loc)

      case SimplifiedAst.Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        // Recursively lift the inner expression.
        val liftedExp = visitExp(exp)

        // Generate a fresh symbol for the new lifted definition.
        val freshSymbol = Symbol.freshDefnSym(ns, name, loc)

        // Construct annotations and modifiers for the fresh definition.
        val ann = Ast.Annotations.Empty
        val mod = Ast.Modifiers(Ast.Modifier.Synthetic :: Nil)

        // Construct the formal parameters.
        val fs = fparams.map(visitFormalParam)

        // Construct a new definition.
        val defn = LiftedAst.Def(ann, mod, freshSymbol, fs, liftedExp, tpe, loc)

        // Add the new definition to the map of lifted definitions.
        m += (freshSymbol -> defn)

        // Construct the free variables.
        val fvs = freeVars.map(visitFreeVar)

        // Construct the closure expression.
        LiftedAst.Expression.Closure(freshSymbol, fvs, tpe, loc)

      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars.map(visitFreeVar)
        LiftedAst.Expression.Closure(sym, fvs, tpe, loc)

      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        LiftedAst.Expression.ApplyClo(e, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map visitExp
        LiftedAst.Expression.ApplyDef(sym, as, tpe, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Unary(sop, op, e, tpe, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        LiftedAst.Expression.Binary(sop, op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        LiftedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        LiftedAst.Expression.Branch(e, bs, tpe, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        LiftedAst.Expression.JumpTo(sym, tpe, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        LiftedAst.Expression.Let(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Is(sym, tag, e, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Untag(sym, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Index(exp, offset, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Index(e, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visitExp
        LiftedAst.Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        LiftedAst.Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, field, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.RecordSelect(e, field, tpe, loc)

      case SimplifiedAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        LiftedAst.Expression.RecordExtend(field, v, r, tpe, loc)

      case SimplifiedAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
        val r = visitExp(rest)
        LiftedAst.Expression.RecordRestrict(field, r, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visitExp
        LiftedAst.Expression.ArrayLit(es, tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visitExp(elm)
        val l = visitExp(len)
        LiftedAst.Expression.ArrayNew(e, l, tpe, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        LiftedAst.Expression.ArrayLoad(b, i, tpe, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        LiftedAst.Expression.ArrayStore(b, i, e, tpe, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = visitExp(base)
        LiftedAst.Expression.ArrayLength(b, tpe, loc)

      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visitExp(base)
        val i1 = visitExp(startIndex)
        val i2 = visitExp(endIndex)
        LiftedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Ref(e, tpe, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Deref(e, tpe, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        LiftedAst.Expression.Assign(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp)
        LiftedAst.Expression.Existential(p, e, loc)

      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp)
        LiftedAst.Expression.Universal(p, e, loc)

      case SimplifiedAst.Expression.Cast(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Cast(e, tpe, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            LiftedAst.CatchRule(sym, clazz, b)
        }
        LiftedAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args.map(visitExp)
        LiftedAst.Expression.InvokeConstructor(constructor, as, tpe, loc)

      case SimplifiedAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        LiftedAst.Expression.InvokeMethod(method, e, as, tpe, loc)

      case SimplifiedAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args.map(visitExp)
        LiftedAst.Expression.InvokeStaticMethod(method, as, tpe, loc)

      case SimplifiedAst.Expression.GetField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.GetField(field, e, tpe, loc)

      case SimplifiedAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        LiftedAst.Expression.PutField(field, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.GetStaticField(field, tpe, loc) =>
        LiftedAst.Expression.GetStaticField(field, tpe, loc)

      case SimplifiedAst.Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.PutStaticField(field, e, tpe, loc)

      case SimplifiedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.NewChannel(e, tpe, loc)

      case SimplifiedAst.Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.GetChannel(e, tpe, loc)

      case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        LiftedAst.Expression.PutChannel(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs: List[LiftedAst.SelectChannelRule] = rules map {
          case SimplifiedAst.SelectChannelRule.SelectGet(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            LiftedAst.SelectChannelRule.SelectGet(sym, c, e)
          case SimplifiedAst.SelectChannelRule.SelectPut(chan, value, exp) =>
            val c = visitExp(chan)
            val v = visitExp(value)
            val e = visitExp(exp)
            LiftedAst.SelectChannelRule.SelectPut(c, v, e)
        }

        val d = default.map(visitExp)

        LiftedAst.Expression.SelectChannel(rs, d, tpe, loc)

      case SimplifiedAst.Expression.Spawn(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Spawn(e, tpe, loc)

      case SimplifiedAst.Expression.Lazy(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Lazy(e, tpe, loc)

      case SimplifiedAst.Expression.Force(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expression.Force(e, tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, loc) =>
        LiftedAst.Expression.HoleError(sym, tpe, loc)

      case SimplifiedAst.Expression.MatchError(tpe, loc) =>
        LiftedAst.Expression.MatchError(tpe, loc)

      case SimplifiedAst.Expression.Def(_, _, _) => throw InternalCompilerException(s"Unexpected expression.")
      case SimplifiedAst.Expression.Lambda(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
      case SimplifiedAst.Expression.Apply(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    }

    visitExp(exp0)
  }

  /**
    * Translates the given simplified formal parameter `fparam` into a lifted formal parameter.
    */
  private def visitFormalParam(fparam: SimplifiedAst.FormalParam): LiftedAst.FormalParam = fparam match {
    case SimplifiedAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * Translates the given simplified free variable `fv` into a lifted free variable.
    */
  private def visitFreeVar(fv: SimplifiedAst.FreeVar): LiftedAst.FreeVar = fv match {
    case SimplifiedAst.FreeVar(sym, tpe) => LiftedAst.FreeVar(sym, tpe)
  }

}
