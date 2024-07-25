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
import ca.uwaterloo.flix.language.ast.Ast._
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.shared.Fixity
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.language.phase.PredDeps.termTypesAndDenotation
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{ParOps, Result, SharedContext, Validation}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

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
  def run(root: Root)(implicit flix: Flix): Validation[Root, StratificationError] = flix.phase("Stratifier") {
    // Construct a new shared context.
    implicit val sctx: SharedContext[StratificationError] = SharedContext.mk()

    implicit val g: LabelledPrecedenceGraph = root.precedenceGraph
    implicit val r: Root = root

    // Compute the stratification at every datalog expression in the ast.
    val ds = ParOps.parMapValues(root.defs)(visitDef)
    val is = ParOps.parMapValues(root.instances)(visitInstances)
    val newTraits = ParOps.parTraverseValues(root.traits)(visitTrait(_))

    flatMapN(newTraits) {
      case ts => Validation.toSuccessOrSoftFailure(root.copy(defs = ds, instances = is, traits = ts), sctx.errors.asScala)
    }
  }(DebugValidation())

  /**
    * Performs Stratification of the given trait `t0`.
    */
  private def visitTrait(t0: TypedAst.Trait)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): Validation[TypedAst.Trait, StratificationError] = {
    val nl = visitDefs(t0.laws)
    val ns = visitSigs(t0.sigs)
    Validation.success(t0.copy(laws = nl, sigs = ns))
  }

  /**
    * Performs Stratification of the given sig `s0`.
    */
  private def visitSig(s0: TypedAst.Sig)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): TypedAst.Sig = {
    val newExp = visitExp(s0.exp)
    s0.copy(exp = newExp)
  }

  /**
    * Performs Stratification of the given sigs `s0s`.
    */
  private def visitSigs(s0s: List[TypedAst.Sig])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[TypedAst.Sig] = {
    s0s.map(visitSig)
  }

  /**
    * Performs Stratification of the given instance `i0`.
    */
  private def visitInstance(i0: TypedAst.Instance)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): TypedAst.Instance = {
    val ds = visitDefs(i0.defs)
    i0.copy(defs = ds)
  }

  /**
    * Performs Stratification of the given instances `is0`.
    */
  private def visitInstances(is0: List[TypedAst.Instance])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[TypedAst.Instance] = {
    is0.map(visitInstance)
  }

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): Def = {
    val e = visitExp(def0.exp)
    def0.copy(exp = e)
  }

  /**
    * Performs stratification of the given definitions `defs0`.
    */
  private def visitDefs(defs0: List[Def])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[Def] = {
    defs0.map(visitDef)
  }

  /**
    * Performs stratification of the given expression `exp0`.
    */
  private def visitExp(exp0: Expr)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): Expr = exp0 match {
    case Expr.Cst(_, _, _) => exp0

    case Expr.Var(_, _, _) => exp0

    case Expr.Def(_, _, _) => exp0

    case Expr.Sig(_, _, _) => exp0

    case Expr.Hole(_, _, _) => exp0

    case Expr.HoleWithExp(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.HoleWithExp(e, tpe, eff, loc)

    case Expr.OpenAs(sym, exp, tpe, loc) =>
      val e = visitExp(exp)
      Expr.OpenAs(sym, e, tpe, loc)

    case Expr.Use(sym, alias, exp, loc) =>
      val e = visitExp(exp)
      Expr.Use(sym, alias, e, loc)

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val e = visitExp(exp)
      Expr.Lambda(fparam, e, tpe, loc)

    case Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      Expr.Apply(e, es, tpe, eff, loc)

    case Expr.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Unary(sop, e, tpe, eff, loc)

    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Binary(sop, e1, e2, tpe, eff, loc)

    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Let(sym, mod, e1, e2, tpe, eff, loc)

    case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.LetRec(sym, ann, mod, e1, e2, tpe, eff, loc)

    case Expr.Region(_, _) => exp0

    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Scope(sym, regionVar, e, tpe, eff, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Stm(e1, e2, tpe, eff, loc)

    case Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      Expr.Discard(e, eff, loc)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = visitMatchRules(rules)
      Expr.Match(e, rs, tpe, eff, loc)

    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = visitTypeMatchRules(rules)
      Expr.TypeMatch(e, rs, tpe, eff, loc)

    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = visitRestrictableChooseRules(rules)
      Expr.RestrictableChoose(star, e, rs, tpe, eff, loc)

    case Expr.Tag(sym, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Tag(sym, e, tpe, eff, loc)

    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.RestrictableTag(sym, e, tpe, eff, loc)

    case Expr.Tuple(exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      Expr.Tuple(es, tpe, eff, loc)

    case Expr.RecordEmpty(tpe, loc) =>
      Expr.RecordEmpty(tpe, loc)

    case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.RecordSelect(e, label, tpe, eff, loc)

    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.RecordExtend(label, e1, e2, tpe, eff, loc)

    case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.RecordRestrict(label, e, tpe, eff, loc)

    case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = visitExps(exps)
      val e = visitExp(exp)
      Expr.ArrayLit(es, e, tpe, eff, loc)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.ArrayNew(e1, e2, e3, tpe, eff, loc)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.ArrayLoad(e1, e2, tpe, eff, loc)

    case Expr.ArrayLength(exp, eff, loc) =>
      val e = visitExp(exp)
      Expr.ArrayLength(e, eff, loc)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.ArrayStore(e1, e2, e3, eff, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      Expr.VectorLit(es, tpe, eff, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Expr.VectorLength(e, loc)

    case Expr.Ref(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Ref(e1, e2, tpe, eff, loc)

    case Expr.Deref(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Deref(e, tpe, eff, loc)

    case Expr.Assign(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Assign(e1, e2, tpe, eff, loc)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Ascribe(e, tpe, eff, loc)

    case Expr.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      Expr.InstanceOf(e, clazz, loc)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.CheckedCast(cast, e, tpe, eff, loc)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.UncheckedCast(e, declaredType, declaredEff, tpe, eff, loc)

    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.UncheckedMaskingCast(e, tpe, eff, loc)

    case Expr.Without(exp, sym, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Without(e, sym, tpe, eff, loc)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = visitCatchRules(rules)
      Expr.TryCatch(e, rs, tpe, eff, loc)

    case Expr.TryWith(exp, sym, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = visitHandlerRules(rules)
      Expr.TryWith(e, sym, rs, tpe, eff, loc)

    case Expr.Do(sym, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      Expr.Do(sym, es, tpe, eff, loc)

    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      Expr.InvokeConstructor(constructor, es, tpe, eff, loc)

    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      Expr.InvokeMethod(method, e, es, tpe, eff, loc)

    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      Expr.InvokeStaticMethod(method, es, tpe, eff, loc)

    case Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.GetField(field, e, tpe, eff, loc)

    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.PutField(field, e1, e2, tpe, eff, loc)

    case Expr.GetStaticField(field, tpe, eff, loc) =>
      Expr.GetStaticField(field, tpe, eff, loc)

    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.PutStaticField(field, e, tpe, eff, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val ms = visitJvmMethods(methods)
      Expr.NewObject(name, clazz, tpe, eff, ms, loc)

    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.NewChannel(e1, e2, tpe, eff, loc)

    case Expr.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.GetChannel(e, tpe, eff, loc)

    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.PutChannel(e1, e2, tpe, eff, loc)

    case Expr.SelectChannel(rules, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = visitSelectChannelRules(rules)
      Expr.SelectChannel(rs, e, tpe, eff, loc)

    case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Spawn(e1, e2, tpe, eff, loc)

    case Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val fs = visitParYieldFragments(frags)
      Expr.ParYield(fs, e, tpe, eff, loc)

    case Expr.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expr.Lazy(e, tpe, loc)

    case Expr.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.Force(e, tpe, eff, loc)

    case Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      val cs = cs0.map(reorder)
      Expr.FixpointConstraintSet(cs, tpe, loc)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      Expr.FixpointLambda(pparams, exp, tpe, eff, loc)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.FixpointMerge(e1, e2, tpe, eff, loc)

    case Expr.FixpointSolve(exp, tpe, eff, loc) =>
      // Compute the stratification.
      stratify(g, tpe, loc)
      val e = visitExp(exp)
      Expr.FixpointSolve(e, tpe, eff, loc)

    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.FixpointFilter(pred, e, tpe, eff, loc)

    case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.FixpointInject(e, pred, tpe, eff, loc)

    case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      Expr.FixpointProject(pred, e, tpe, eff, loc)

    case Expr.Error(m, tpe, eff) =>
      Expr.Error(m, tpe, eff)

  }

  private def visitExp(exp0: Option[Expr])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): Option[Expr] = {
    exp0.map(visitExp)
  }

  private def visitExps(exps0: List[Expr])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[Expr] = {
    exps0.map(visitExp)
  }

  private def visitMatchRule(rule: MatchRule)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): MatchRule = rule match {
    case MatchRule(pat, exp1, exp2) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      MatchRule(pat, e1, e2)
  }

  private def visitMatchRules(rules: List[MatchRule])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[MatchRule] = {
    rules.map(visitMatchRule)
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): TypeMatchRule = rule match {
    case TypeMatchRule(sym, t, exp1) =>
      val e1 = visitExp(exp1)
      TypeMatchRule(sym, t, e1)
  }

  private def visitTypeMatchRules(rules: List[TypeMatchRule])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[TypeMatchRule] = {
    rules.map(visitTypeMatchRule)
  }

  private def visitRestrictableChooseRule(rule: RestrictableChooseRule)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): RestrictableChooseRule = rule match {
    case RestrictableChooseRule(pat, exp1) =>
      val e1 = visitExp(exp1)
      RestrictableChooseRule(pat, e1)
  }

  private def visitRestrictableChooseRules(rules: List[RestrictableChooseRule])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[RestrictableChooseRule] = {
    rules.map(visitRestrictableChooseRule)
  }

  private def visitTryCatchRule(rule: CatchRule)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): CatchRule = rule match {
    case CatchRule(sym, clazz, exp1) =>
      val e1 = visitExp(exp1)
      CatchRule(sym, clazz, e1)
  }

  private def visitCatchRules(rules: List[CatchRule])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[CatchRule] = {
    rules.map(visitTryCatchRule)
  }

  private def visitTryWithRule(rule: HandlerRule)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): HandlerRule = rule match {
    case HandlerRule(op, fparams, exp1) =>
      val e1 = visitExp(exp1)
      HandlerRule(op, fparams, e1)
  }

  private def visitHandlerRules(rules: List[HandlerRule])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[HandlerRule] = {
    rules.map(visitTryWithRule)
  }

  private def visitJvmMethod(method: JvmMethod)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): JvmMethod = method match {
    case JvmMethod(ident, fparams, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      JvmMethod(ident, fparams, e, tpe, eff, loc)
  }

  private def visitJvmMethods(methods: List[JvmMethod])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[JvmMethod] = {
    methods.map(visitJvmMethod)
  }

  private def visitSelectChannelRule(rule: SelectChannelRule)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): SelectChannelRule = rule match {
    case SelectChannelRule(sym, exp1, exp2) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      SelectChannelRule(sym, e1, e2)
  }

  private def visitSelectChannelRules(rules: List[SelectChannelRule])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[SelectChannelRule] = {
    rules.map(visitSelectChannelRule)
  }

  private def visitParYieldFragment(frag: ParYieldFragment)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): ParYieldFragment = frag match {
    case ParYieldFragment(pat, exp1, loc1) =>
      val e1 = visitExp(exp1)
      ParYieldFragment(pat, e1, loc1)
  }

  private def visitParYieldFragments(frags: List[ParYieldFragment])(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix, sctx: SharedContext[StratificationError]): List[ParYieldFragment] = {
    frags.map(visitParYieldFragment)
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
  private def stratify(g: LabelledPrecedenceGraph, tpe: Type, loc: SourceLocation)(implicit flix: Flix, sctx: SharedContext[StratificationError]): Unit = {
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
  private def labelEq(l1: Label, l2: Label)(implicit flix: Flix): Boolean = {
    val isEqPredicate = l1.pred == l2.pred
    val isEqDenotation = l1.den == l2.den
    val isEqArity = l1.arity == l2.arity
    val isEqTermTypes = l1.terms.zip(l2.terms).forall {
      case (t1, t2) => Unification.unifiesWith(t1, t2, RigidityEnv.empty, ListMap.empty) // TODO ASSOC-TYPES empty right?
    }

    isEqPredicate && isEqDenotation && isEqArity && isEqTermTypes
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

}
