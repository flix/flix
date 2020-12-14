/*
 * Copyright 2020 Andreas Heglingegård
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast.Symbol

//import scala.collection.immutable.Map

object LiftedAstOps {


  /**
    * Replaces all parameter variables and in the expression with fresh variables
    */
  def refreshVarNames(defn: Def)(implicit flix: Flix): Def = {

    def visitTermBody(body: Term.Body, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): Term.Body = body match {
      case Term.Body.Wild(_, _) => body
      case Term.Body.QuantVar(sym, tpe, loc) =>
        val (varMap, _) = maps
        val s = varMap(sym)
        Term.Body.QuantVar(s, tpe, loc)

      case Term.Body.CapturedVar(sym, tpe, loc) =>
        val (varMap, _) = maps
        val s = varMap(sym)
        Term.Body.CapturedVar(s, tpe, loc)

      case Term.Body.Lit(exp, tpe, loc) =>
        val e = visit(exp, maps)
        Term.Body.Lit(e, tpe, loc)
    }

    def visitPredicateBody(body: Predicate.Body, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): Predicate.Body = body match {
      case Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
        val ts = terms.map(b => visitTermBody(b, maps))
        Predicate.Body.Atom(pred, den, polarity, ts, tpe, loc)

      case Predicate.Body.Guard(exp, loc) =>
        val e = visit(exp, maps)
        Predicate.Body.Guard(e, loc)
    }


    def visitTermHead(head: Term.Head, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): Term.Head = head match {
      case Term.Head.QuantVar(sym, tpe, loc) =>
        val s = Symbol.freshVarSym(sym)
        Term.Head.QuantVar(s, tpe, loc)

      case Term.Head.CapturedVar(sym, tpe, loc) =>
        val s = Symbol.freshVarSym(sym)
        Term.Head.CapturedVar(s, tpe, loc)

      case Term.Head.Lit(lit, tpe, loc) =>
        val l = visit(lit, maps)
        Term.Head.Lit(l, tpe, loc)

      case Term.Head.App(exp, args, tpe, loc) =>
        val (varMap, _) = maps
        val e = visit(exp, maps)
        val ags = args.map(vs => varMap(vs))
        Term.Head.App(e, ags, tpe, loc)
    }

    def visitPredicateHead(head: Predicate.Head, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym]))
    : Predicate.Head = head match {
      case Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
        val ts = terms.map(t => visitTermHead(t, maps))
        Predicate.Head.Atom(pred, den, ts, tpe, loc)

      case Predicate.Head.Union(exp, tpe, loc) =>
        val e = visit(exp, maps)
        Predicate.Head.Union(e, tpe, loc)
    }


    def visitConstraintParam(cp: ConstraintParam)
    : (ConstraintParam, (Symbol.VarSym, Symbol.VarSym)) = cp match {
      case ConstraintParam.HeadParam(sym, tpe, loc) =>
        val newSym = Symbol.freshVarSym(sym)
        (ConstraintParam.HeadParam(newSym, tpe, loc), sym -> newSym)

      case ConstraintParam.RuleParam(sym, tpe, loc) =>
        val newSym = Symbol.freshVarSym(sym)
        (ConstraintParam.RuleParam(newSym, tpe, loc), sym -> newSym)
    }

    def visitConstraint(c: Constraint, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): Constraint = c match {
      case Constraint(cparams, head, body, loc) =>
        val (varMap, labelMap) = maps
        val startTuple: (List[ConstraintParam], Map[Symbol.VarSym, Symbol.VarSym]) = (Nil, varMap)
        val (newCParams, newVarMap) = cparams.foldLeft(startTuple) {
          case ((cpsAcc, varMapAcc), cp) =>
            val (newCp, newVarMapEntry) = visitConstraintParam(cp)
            (newCp :: cpsAcc, varMapAcc + newVarMapEntry)
        }
        val h = visitPredicateHead(head, (newVarMap, labelMap))
        val b = body.map(bP => visitPredicateBody(bP, (newVarMap, labelMap)))
        Constraint(newCParams.reverse, h, b, loc)
    }

    def visitOption(op: Option[Expression], varMap: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): Option[Expression] = op match {
      case Some(exp) => Some(visit(exp, varMap))
      case None => None
    }

    def visitSelectChannelRule(rule: SelectChannelRule, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): SelectChannelRule = {
      val (varMap, labelMap) = maps
      val SelectChannelRule(sym, chan, exp) = rule
      val c = visit(chan, maps)
      val newSym = Symbol.freshVarSym(sym)
      val newVarMap = varMap + (sym -> newSym)
      val e = visit(exp, (newVarMap, labelMap))
      SelectChannelRule(newSym, c, e)
    }

    /**
      * Replaces the variable in the CatchRule with a fresh one, and propagates that and other VarSym replacements
      */
    def visitCatchRule(cr: CatchRule, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): CatchRule = {
      val (varMap, labelMap) = maps
      val CatchRule(sym, clazz, exp) = cr
      val newSym = Symbol.freshVarSym(sym)
      val newVarMap = varMap + (sym -> newSym)
      val e = visit(exp, (newVarMap, labelMap))
      CatchRule(newSym, clazz, e)
    }

    def visitFormalParams(fParam: FormalParam, varMap: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fParam match {
      case FormalParam(sym, mod, tpe, loc) =>
        val s = varMap(sym)
        FormalParam(s, mod, tpe, loc)
    }

    def visit(exp0: Expression, maps: (Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])): Expression = {
      val (varMap, labelMap) = maps
      exp0 match {
        case Expression.Unit => exp0
        case Expression.Null(_) => exp0
        case Expression.True => exp0
        case Expression.False => exp0
        case Expression.Char(_) => exp0
        case Expression.Float32(_) => exp0
        case Expression.Float64(_) => exp0
        case Expression.Int8(_) => exp0
        case Expression.Int16(_) => exp0
        case Expression.Int32(_) => exp0
        case Expression.Int64(_) => exp0
        case Expression.BigInt(_) => exp0
        case Expression.Str(_) => exp0
        case Expression.Var(sym, tpe, loc) =>
          val (varMap, _) = maps
          val s = varMap(sym)
          Expression.Var(s, tpe, loc)

        case Expression.Closure(sym, freeVars, tpe, loc) =>
          val fvs = freeVars.map {
            case FreeVar(sym, tpe) =>
              val newVar = varMap(sym)
              FreeVar(newVar, tpe)
          }
          Expression.Closure(sym, fvs, tpe, loc)

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = visit(exp, maps)
          val a = args.map(ea => visit(ea, maps))
          Expression.ApplyClo(e, a, tpe, loc)

        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val a = args.map(ea => visit(ea, maps))
          Expression.ApplyDef(sym, a, tpe, loc)

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = visit(exp, maps)
          val a = args.map(ea => visit(ea, maps))
          Expression.ApplyCloTail(e, a, tpe, loc)

        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val a = args.map(ea => visit(ea, maps))
          Expression.ApplyDefTail(sym, a, tpe, loc)

        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
          val fs = formals.map(f => visitFormalParams(f, varMap))
          val as = actuals.map(e => visit(e, maps))
          Expression.ApplySelfTail(sym, fs, as, tpe, loc)

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          val e3 = visit(exp3, maps)
          Expression.IfThenElse(e1, e2, e3, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          val newLabelMap = branches.keys.foldLeft(labelMap)(
            (mapAcc, labelSym) => mapAcc + (labelSym -> Symbol.freshLabel(labelSym))
          )
          val e = visit(exp, (varMap, newLabelMap))
          val b = branches.map {
            case (lSym, expLSym) => newLabelMap(lSym) -> visit(expLSym, (varMap, newLabelMap))
          }
          Expression.Branch(e, b, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) =>
          val (_, labelMap) = maps
          val newSym = labelMap(sym)
          Expression.JumpTo(newSym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val newSym = Symbol.freshVarSym(sym)
          val newVarMap = varMap + (sym -> newSym)
          val e2 = visit(exp2, (newVarMap, labelMap))
          Expression.Let(newSym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = visit(exp, maps)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = visit(base, maps)
          Expression.Index(b, offset, tpe, loc)

        case Expression.IndexMut(base, offset, toInsert, tpe, loc) =>
          val b = visit(base, maps)
          val ti = visit(toInsert, maps)
          Expression.IndexMut(b, offset, ti, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms.map(e => visit(e, maps))
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(_, _) => exp0

        case Expression.RecordSelect(exp, field, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.RecordSelect(e, field, tpe, loc)

        case Expression.RecordExtend(field, value, rest, tpe, loc) =>
          val v = visit(value, maps)
          val r = visit(rest, maps)
          Expression.RecordExtend(field, v, r, tpe, loc)

        case Expression.RecordRestrict(field, rest, tpe, loc) =>
          val r = visit(rest, maps)
          Expression.RecordRestrict(field, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms.map(e => visit(e, maps))
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = visit(elm, maps)
          val l = visit(len, maps)
          Expression.ArrayNew(e, l, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = visit(base, maps)
          val i = visit(index, maps)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = visit(base, maps)
          val i = visit(index, maps)
          val e = visit(elm, maps)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = visit(base, maps)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
          val b = visit(base, maps)
          val bi = visit(beginIndex, maps)
          val ei = visit(endIndex, maps)
          Expression.ArraySlice(b, bi, ei, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val FormalParam(sym, _, _, _) = fparam
          val newVarSym = Symbol.freshVarSym(sym)
          val newVarMap = varMap + (sym -> newVarSym)
          val e = visit(exp, (newVarMap, labelMap))
          val fp = visitFormalParams(fparam, newVarMap)
          Expression.Existential(fp, e, loc)

        case Expression.Universal(fparam, exp, loc) =>
          val FormalParam(sym, _, _, _) = fparam
          val newVarSym = Symbol.freshVarSym(sym)
          val newVarMap = varMap + (sym -> newVarSym)
          val e = visit(exp, (newVarMap, labelMap))
          val fp = visitFormalParams(fparam, newVarMap)
          Expression.Universal(fp, e, loc)

        case Expression.Cast(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = visit(exp, maps)
          val r = rules.map(cr => visitCatchRule(cr, maps))
          Expression.TryCatch(e, r, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val as = args.map(a => visit(a, maps))
          Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val e = visit(exp, maps)
          val as = args.map(a => visit(a, maps))
          Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val as = args.map(a => visit(a, maps))
          Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => exp0

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules.map(r => visitSelectChannelRule(r, maps))
          val d = visitOption(default, maps)
          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.Spawn(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Spawn(e, tpe, loc)

        case Expression.Lazy(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Lazy(e, tpe, loc)

        case Expression.Force(exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.Force(e, tpe, loc)

        case Expression.FixpointConstraintSet(cs, tpe, loc) =>
          val newCs = cs.map(c => visitConstraint(c, maps))
          Expression.FixpointConstraintSet(newCs, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(pred, exp, tpe, loc) =>
          val e = visit(exp, maps)
          Expression.FixpointProject(pred, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, loc) =>
          val e1 = visit(exp1, maps)
          val e2 = visit(exp2, maps)
          val e3 = visit(exp3, maps)
          Expression.FixpointFold(pred, e1, e2, e3, tpe, loc)

        case Expression.HoleError(_, _, _) => exp0
        case Expression.MatchError(_, _) => exp0

      }
    }

    val variablesMap = Map.from(defn.fparams.map {
      case FormalParam(sym, _, _, _) => sym -> Symbol.freshVarSym(sym)
    })

    val newExp = visit(defn.exp, (variablesMap, Map.empty))
    val newFParams = defn.fparams.map(fp => visitFormalParams(fp, variablesMap))
    defn.copy(exp = newExp, fparams = newFParams)
  }

}
