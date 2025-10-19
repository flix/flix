/*
 *  Copyright 2017 Magnus Madsen and Jason Mittertreiner
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head
import ca.uwaterloo.flix.language.ast.shared.LabelledPrecedenceGraph.{Label, LabelledEdge}
import ca.uwaterloo.flix.language.ast.shared.{Fixity, LabelledPrecedenceGraph, Polarity, Scope}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.language.phase.PredDeps.termTypesAndDenotation
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.util.{ParOps, Result}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * A negated subgoal is generalized here to a subgoal that is negated
  * or fixed, collectively called a strong dependency.
  *
  * Reports a [[StratificationError]] if the constraints cannot be stratified.
  */
object Stratifier {
  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): (Root, List[StratificationError]) = flix.phaseNew("Stratifier") {
    // Construct a new shared context.
    implicit val sctx: SharedContext = SharedContext.mk()

    implicit val g: LabelledPrecedenceGraph = root.precedenceGraph
    implicit val r: Root = root

    // Compute the stratification at every datalog expression in the ast.
    val ds = ParOps.parMapValues(root.defs)(visitDef)
    val is = ParOps.parMapValueList(root.instances)(visitInstance)
    val ts = ParOps.parMapValues(root.traits)(visitTrait)

    (root.copy(defs = ds, instances = is, traits = ts), sctx.errors.asScala.toList)
  }

  /**
    * Performs Stratification of the given trait `t0`.
    */
  private def visitTrait(t0: TypedAst.Trait)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): TypedAst.Trait = {
    val nl = t0.laws.map(visitDef)
    val ns = t0.sigs.map(visitSig)
    t0.copy(laws = nl, sigs = ns)
  }

  /**
    * Performs Stratification of the given sig `s0`.
    */
  private def visitSig(s0: TypedAst.Sig)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): TypedAst.Sig = {
    val newExp = s0.exp.map(visitExp)
    s0.copy(exp = newExp)
  }

  /**
    * Performs Stratification of the given instance `i0`.
    */
  private def visitInstance(i0: TypedAst.Instance)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): TypedAst.Instance = {
    val ds = i0.defs.map(visitDef)
    i0.copy(defs = ds)
  }

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): Def = {
    val e = visitExp(def0.exp)
    def0.copy(exp = e)
  }

  /**
    * Performs stratification of the given expression `exp0`.
    */
  private def visitExp(exp0: Exp)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): Exp = exp0 match {
    case Exp.Cst(_, _, _) => exp0

    case Exp.Var(_, _, _) => exp0

    case Exp.Hole(_, _, _, _, _) => exp0

    case Exp.HoleWithExp(exp, env, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.HoleWithExp(e, env, tpe, eff, loc)

    case Exp.OpenAs(sym, exp, tpe, loc) =>
      val e = visitExp(exp)
      Exp.OpenAs(sym, e, tpe, loc)

    case Exp.Use(sym, alias, exp, loc) =>
      val e = visitExp(exp)
      Exp.Use(sym, alias, e, loc)

    case Exp.Lambda(fparam, exp, tpe, loc) =>
      val e = visitExp(exp)
      Exp.Lambda(fparam, e, tpe, loc)

    case Exp.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.ApplyClo(e1, e2, tpe, eff, loc)

    case Exp.ApplyDef(symUse, exps, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.ApplyDef(symUse, es, targs, itpe, tpe, eff, loc)

    case Exp.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.ApplyLocalDef(symUse, es, arrowTpe, tpe, eff, loc)

    case Exp.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.ApplyOp(sym, es, tpe, eff, loc)

    case Exp.ApplySig(symUse, exps, targ, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.ApplySig(symUse, es, targ, targs, itpe, tpe, eff, loc)

    case Exp.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Unary(sop, e, tpe, eff, loc)

    case Exp.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.Binary(sop, e1, e2, tpe, eff, loc)

    case Exp.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.Let(sym, e1, e2, tpe, eff, loc)

    case Exp.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.LocalDef(sym, fparams, e1, e2, tpe, eff, loc)

    case Exp.Region(sym, regionVar, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Region(sym, regionVar, e, tpe, eff, loc)

    case Exp.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Exp.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case Exp.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.Stm(e1, e2, tpe, eff, loc)

    case Exp.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      Exp.Discard(e, eff, loc)

    case Exp.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      Exp.Match(e, rs, tpe, eff, loc)

    case Exp.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitTypeMatchRule)
      Exp.TypeMatch(e, rs, tpe, eff, loc)

    case Exp.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitRestrictableChooseRule)
      Exp.RestrictableChoose(star, e, rs, tpe, eff, loc)

    case Exp.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitExtMatchRule)
      Exp.ExtMatch(e, rs, tpe, eff, loc)

    case Exp.Tag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.Tag(symUse, es, tpe, eff, loc)

    case Exp.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.RestrictableTag(symUse, es, tpe, eff, loc)

    case Exp.ExtTag(label, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.ExtTag(label, es, tpe, eff, loc)

    case Exp.Tuple(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.Tuple(es, tpe, eff, loc)

    case Exp.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.RecordSelect(e, label, tpe, eff, loc)

    case Exp.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.RecordExtend(label, e1, e2, tpe, eff, loc)

    case Exp.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.RecordRestrict(label, e, tpe, eff, loc)

    case Exp.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val e = visitExp(exp)
      Exp.ArrayLit(es, e, tpe, eff, loc)

    case Exp.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Exp.ArrayNew(e1, e2, e3, tpe, eff, loc)

    case Exp.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.ArrayLoad(e1, e2, tpe, eff, loc)

    case Exp.ArrayLength(exp, eff, loc) =>
      val e = visitExp(exp)
      Exp.ArrayLength(e, eff, loc)

    case Exp.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Exp.ArrayStore(e1, e2, e3, eff, loc)

    case Exp.StructNew(sym, fields0, region0, tpe, eff, loc) =>
      val fields = fields0.map {
        case (name, e0) => name -> visitExp(e0)
      }
      val region = visitExp(region0)
      Exp.StructNew(sym, fields, region, tpe, eff, loc)

    case Exp.StructGet(e0, field, tpe, eff, loc) =>
      val e = visitExp(e0)
      Exp.StructGet(e, field, tpe, eff, loc)

    case Exp.StructPut(exp1, field, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.StructPut(e1, field, e2, tpe, eff, loc)

    case Exp.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.VectorLit(es, tpe, eff, loc)

    case Exp.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.VectorLoad(e1, e2, tpe, eff, loc)

    case Exp.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Exp.VectorLength(e, loc)

    case Exp.Ascribe(exp, expectedType, expectedEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Ascribe(e, expectedType, expectedEff, tpe, eff, loc)

    case Exp.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      Exp.InstanceOf(e, clazz, loc)

    case Exp.CheckedCast(cast, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.CheckedCast(cast, e, tpe, eff, loc)

    case Exp.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.UncheckedCast(e, declaredType, declaredEff, tpe, eff, loc)

    case Exp.Unsafe(exp, runEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Unsafe(e, runEff, tpe, eff, loc)

    case Exp.Without(exp, symUse, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Without(e, symUse, tpe, eff, loc)

    case Exp.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitTryCatchRule)
      Exp.TryCatch(e, rs, tpe, eff, loc)

    case Exp.Throw(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Throw(e, tpe, eff, loc)

    case Exp.Handler(symUse, rules, bodyTpe, bodyEff, handledEff, tpe, loc) =>
      val rs = rules.map(visitRunWithRule)
      Exp.Handler(symUse, rs, bodyTpe, bodyEff, handledEff, tpe, loc)

    case Exp.RunWith(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.RunWith(e1, e2, tpe, eff, loc)

    case Exp.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.InvokeConstructor(constructor, es, tpe, eff, loc)

    case Exp.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      Exp.InvokeMethod(method, e, es, tpe, eff, loc)

    case Exp.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.InvokeStaticMethod(method, es, tpe, eff, loc)

    case Exp.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.GetField(field, e, tpe, eff, loc)

    case Exp.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.PutField(field, e1, e2, tpe, eff, loc)

    case Exp.GetStaticField(field, tpe, eff, loc) =>
      Exp.GetStaticField(field, tpe, eff, loc)

    case Exp.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.PutStaticField(field, e, tpe, eff, loc)

    case Exp.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      Exp.NewObject(name, clazz, tpe, eff, ms, loc)

    case Exp.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.NewChannel(e, tpe, eff, loc)

    case Exp.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.GetChannel(e, tpe, eff, loc)

    case Exp.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.PutChannel(e1, e2, tpe, eff, loc)

    case Exp.SelectChannel(rules, exp, tpe, eff, loc) =>
      val e = exp.map(visitExp)
      val rs = rules.map(visitSelectChannelRule)
      Exp.SelectChannel(rs, e, tpe, eff, loc)

    case Exp.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.Spawn(e1, e2, tpe, eff, loc)

    case Exp.ParYield(frags, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val fs = frags.map(visitParYieldFragment)
      Exp.ParYield(fs, e, tpe, eff, loc)

    case Exp.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      Exp.Lazy(e, tpe, loc)

    case Exp.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Exp.Force(e, tpe, eff, loc)

    case Exp.FixpointConstraintSet(cs0, tpe, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      val cs = cs0.map(reorder)
      Exp.FixpointConstraintSet(cs, tpe, loc)

    case Exp.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      Exp.FixpointLambda(pparams, exp, tpe, eff, loc)

    case Exp.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Exp.FixpointMerge(e1, e2, tpe, eff, loc)

    case Exp.FixpointQueryWithProvenance(exps, Head.Atom(pred, den, terms, tpe2, loc2), withh, tpe1, eff1, loc1) =>
      val es = exps.map(visitExp)
      val ts = terms.map(visitExp)
      Exp.FixpointQueryWithProvenance(es, Head.Atom(pred, den, ts, tpe2, loc2), withh, tpe1, eff1, loc1)

    case Exp.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val qe = visitExp(queryExp)
      val ss = selects.map(visitExp)
      val w = where.map(visitExp)
      Exp.FixpointQueryWithSelect(es, qe, ss, from, w, pred, tpe, eff, loc)

    case Exp.FixpointSolveWithProject(exps, optPreds, mode, tpe, eff, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      val es = exps.map(visitExp)
      Exp.FixpointSolveWithProject(es, optPreds, mode, tpe, eff, loc)

    case Exp.FixpointInjectInto(exps, predsAndArities, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      Exp.FixpointInjectInto(es, predsAndArities, tpe, eff, loc)

    case Exp.Error(m, tpe, eff) =>
      Exp.Error(m, tpe, eff)

  }

  private def visitMatchRule(rule: MatchRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): MatchRule = rule match {
    case MatchRule(pat, exp1, exp2, loc) =>
      val e1 = exp1.map(visitExp)
      val e2 = visitExp(exp2)
      MatchRule(pat, e1, e2, loc)
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): TypeMatchRule = rule match {
    case TypeMatchRule(sym, t, exp1, loc) =>
      val e1 = visitExp(exp1)
      TypeMatchRule(sym, t, e1, loc)
  }

  private def visitRestrictableChooseRule(rule: RestrictableChooseRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): RestrictableChooseRule = rule match {
    case RestrictableChooseRule(pat, exp1) =>
      val e1 = visitExp(exp1)
      RestrictableChooseRule(pat, e1)
  }

  private def visitExtMatchRule(rule: ExtMatchRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): ExtMatchRule = rule match {
    case ExtMatchRule(pat, exp, loc) =>
      val e1 = visitExp(exp)
      ExtMatchRule(pat, e1, loc)
  }

  private def visitTryCatchRule(rule: CatchRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): CatchRule = rule match {
    case CatchRule(sym, clazz, exp1, loc) =>
      val e1 = visitExp(exp1)
      CatchRule(sym, clazz, e1, loc)
  }

  private def visitRunWithRule(rule: HandlerRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): HandlerRule = rule match {
    case HandlerRule(op, fparams, exp1, loc) =>
      val e1 = visitExp(exp1)
      HandlerRule(op, fparams, e1, loc)
  }

  private def visitJvmMethod(method: JvmMethod)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): JvmMethod = method match {
    case JvmMethod(ident, fparams, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      JvmMethod(ident, fparams, e, tpe, eff, loc)
  }

  private def visitSelectChannelRule(rule: SelectChannelRule)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): SelectChannelRule = rule match {
    case SelectChannelRule(sym, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      SelectChannelRule(sym, e1, e2, loc)
  }

  private def visitParYieldFragment(frag: ParYieldFragment)(implicit g: LabelledPrecedenceGraph, sctx: SharedContext, root: Root, flix: Flix): ParYieldFragment = frag match {
    case ParYieldFragment(pat, exp1, loc1) =>
      val e1 = visitExp(exp1)
      ParYieldFragment(pat, e1, loc1)
  }

  /**
    * Reorders a constraint such that its negated atoms and loop predicates occur last.
    */
  private def reorder(c0: Constraint): Constraint = {
    /**
      * Returns `true` if the body predicate is negated.
      */
    def isNegativeOrLoop(p: Predicate.Body): Boolean = p match {
      case Predicate.Body.Atom(_, _, Polarity.Negative, _, _, _, _) => true
      case Predicate.Body.Functional(_, _, _) => true
      case _ => false
    }

    // Order the predicates from first to last.
    val last = c0.body filter isNegativeOrLoop
    val first = c0.body filterNot isNegativeOrLoop

    // Reassemble the constraint.
    c0.copy(body = first ::: last)
  }

  /**
    * Computes the stratification of the given labelled graph `g` for the given row type `tpe` at the given source location `loc`.
    */
  private def stratify(g: LabelledPrecedenceGraph, tpe: Type, loc: SourceLocation)(implicit sctx: SharedContext, root: Root, flix: Flix): Unit = {
    // The key is the set of predicates that occur in the row type.
    val key = predicateSymbolsOf(tpe)

    // Compute the restricted labelled graph.
    val rg = g.restrict(key, labelEq(_, _))

    // Compute the stratification.
    UllmansAlgorithm.stratify(labelledGraphToDependencyGraph(rg), tpe, loc) match {
      case Result.Ok(_) => ()
      case Result.Err(e) =>
        sctx.errors.add(e)
        ()
    }
  }

  /**
    * Returns the map of predicates that appears in the given Schema `tpe`.
    */
  private def predicateSymbolsOf(tpe: Type): Map[Name.Pred, Label] = {
    @tailrec
    def visitType(tpe: Type, acc: Map[Name.Pred, Label]): Map[Name.Pred, Label] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), predType, _), rest, _) =>
        val (terms, labelDen) = termTypesAndDenotation(predType)
        val label = Label(pred, labelDen, terms.length, terms)
        visitType(rest, acc + (pred -> label))
      case _ => acc
    }

    Type.eraseAliases(tpe) match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, _), schemaRow, _) => visitType(schemaRow, Map.empty)
      case _ =>
        // We would like to assume that `tpe` must be a schema type. However, because type inference is resilient it is
        // possible that the stratifier is run on an expression where type inference was only partially successful.
        // Hence we may arrive here. If that happens there is nothing to be done.
        Map.empty
    }
  }

  /**
    * Returns `true` if the two given labels `l1` and `l2` are considered equal.
    */
  private def labelEq(l1: Label, l2: Label)(implicit root: Root, flix: Flix): Boolean = {
    l1.pred == l2.pred &&
      l1.den == l2.den &&
      l1.arity == l2.arity &&
      unifiableTermTypes(l1, l2)
  }

  /**
    * Returns `true` if `l1` and `l2` have unifiable term types.
    *
    * N.B.: The two must have the same number of terms.
    */
  private def unifiableTermTypes(l1: Label, l2: Label)(implicit root: Root, flix: Flix): Boolean = {
    l1.terms.zip(l2.terms).forall {
      case (t1, t2) => ConstraintSolver2.fullyUnify(t1, t2, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix).isDefined // TODO ASSOC-TYPES empty right? // TODO LEVELS top OK?
    }
  }

  /**
    * Computes the dependency graph from the labelled graph, throwing the labels away.
    * If a labelled edge is either negative or fixed it is transformed to a strong edge.
    */
  private def labelledGraphToDependencyGraph(g: LabelledPrecedenceGraph): UllmansAlgorithm.DependencyGraph =
    g.edges.map {
      case LabelledEdge(head, Polarity.Positive, Fixity.Loose, _, body, loc) =>
        // Positive, loose edges require that the strata of the head is equal to,
        // or below, the strata of the body hence a weak edge.
        UllmansAlgorithm.DependencyEdge.Weak(head, body, loc)
      case LabelledEdge(head, _, _, _, body, loc) =>
        // Edges that are either negatively bound or fixed are strong since they require
        // that the strata of the head is strictly higher than the strata of the body.
        UllmansAlgorithm.DependencyEdge.Strong(head, body, loc)
    }.toSet

  private object SharedContext {
    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the errors in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[StratificationError])

}
