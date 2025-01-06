package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Pattern, RestrictableChoosePattern, Root}
import ca.uwaterloo.flix.language.ast.shared.{BroadEqualityConstraint, DependencyGraph, EqualityConstraint, Input, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.MultiMap

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

object Dependencies {
  /**
    * Updates the dependency graph of the given `root`.
    * We visit all the definitions, effects, enums, instances, structs, traits, and type aliases to find the dependencies.
    *
    * Most of the time we just recursively visit the sub-nodes. We only add dependencies for three base cases:
    *   - Type
    *   - SymUse
    *   - Instance
    */
  def run(root: Root)(implicit flix: Flix): (Root, Unit) = flix.phaseNew("Dependencies") {
    implicit val sctx: SharedContext = SharedContext(new ConcurrentHashMap[(Input, Input), Unit]())
    ParOps.parMap(root.defs.values)(visitDef)
    ParOps.parMap(root.effects.values)(visitEff)
    ParOps.parMap(root.enums.values)(visitEnum)
    ParOps.parMap(root.instances.values)(visitInstances)
    ParOps.parMap(root.structs.values)(visitStruct)
    ParOps.parMap(root.traits.values)(visitTrait)
    ParOps.parMap(root.typeAliases.values)(visitTypeAlias)

    var deps = MultiMap.empty[Input, Input]
    sctx.deps.forEach((k, _) => deps = deps + k)
    val dg = DependencyGraph(deps)
    (root.copy(dependencyGraph = dg), ())
  }

  private def addToSharedContext(src: SourceLocation, dst: SourceLocation)(implicit sctx: SharedContext): Unit = {
    sctx.deps.put((src.sp1.source.input, dst.sp1.source.input), ())
  }

  /**
    * Returns the dependencies of the given type.
    * One of the base case for the recursive visiting.
    */
  private def visitType(tpe: Type)(implicit sctx: SharedContext): Unit = tpe match {
    case Type.Alias(cst, _, _, loc) => addToSharedContext(cst.loc, loc)
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => addToSharedContext(sym.loc, loc)
    case Type.Cst(TypeConstructor.Struct(sym, _), loc) => addToSharedContext(sym.loc, loc)
    case _ => ()
  }

  /**
    * Returns the dependencies of the given SymUse.
    * One of the base case for the recursive visiting.
    */
  private def visitSymUse(use: SymUse.SymUse)(implicit sctx: SharedContext): Unit = use match {
    case SymUse.AssocTypeSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.CaseSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.DefSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.EffectSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.OpSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.SigSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.StructFieldSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case SymUse.TraitSymUse(sym, loc) => addToSharedContext(sym.loc, loc)
    case _ => ()
  }

  /**
   * Returns the dependencies of the given instance.
   * One of the base case for the recursive visiting.
   */
  private def visitInstances(instances: List[TypedAst.Instance])(implicit sctx: SharedContext): Unit =
    instances.foreach(instance => addToSharedContext(instance.trt.sym.loc, instance.loc))

  private def visitSpec(spec: TypedAst.Spec)(implicit sctx: SharedContext): Unit = {
    visitType(spec.retTpe)
    visitScheme(spec.declaredScheme)
    visitType(spec.eff)
    spec.fparams.foreach(visitFParam)
    spec.tconstrs.foreach(visitTraitConstraint)
    spec.econstrs.foreach(visitEqualityConstraint)
  }

  private def visitScheme(scheme: Scheme)(implicit sctx: SharedContext): Unit = {
    visitType(scheme.base)
    scheme.tconstrs.foreach(visitTraitConstraint)
    scheme.econstrs.foreach(visitBroadEqualityConstraint)
  }

  private def visitTraitConstraint(tc: TraitConstraint)(implicit sctx: SharedContext): Unit =
    visitType(tc.arg)

  private def visitEqualityConstraint(ec: EqualityConstraint)(implicit sctx: SharedContext): Unit = {
    visitType(ec.tpe1)
    visitType(ec.tpe2)
  }


  private def visitBroadEqualityConstraint(ec: BroadEqualityConstraint)(implicit sctx: SharedContext): Unit = {
    visitType(ec.tpe1)
    visitType(ec.tpe2)
  }

  private def visitBnd(bnd: TypedAst.Binder)(implicit sctx: SharedContext): Unit =
    visitType(bnd.tpe)

  private def visitFParam(fparam: TypedAst.FormalParam)(implicit sctx: SharedContext): Unit = {
    visitBnd(fparam.bnd)
    visitType(fparam.tpe)
  }

  private def visitMatchRule(matchRule: TypedAst.MatchRule)(implicit sctx: SharedContext): Unit = {
    visitPattern(matchRule.pat)
    visitExp(matchRule.exp)
    matchRule.guard.toList.foreach(visitExp)
  }

  private def visitPattern(pattern: TypedAst.Pattern)(implicit sctx: SharedContext): Unit = pattern match {
    case Pattern.Var(bnd, tpe, _) => visitBnd(bnd)
      visitType(tpe)
    case Pattern.Tag(sym, pats, tpe, _) => visitSymUse(sym)
      visitType(tpe)
      pats.foreach(visitPattern)
    case Pattern.Tuple(pats, tpe, _) => visitType(tpe)
      pats.foreach(visitPattern)
    case Pattern.Record(pats, pat, tpe, _) => visitPattern(pat)
      visitType(tpe)
      pats.foreach(visitRecordLabelPattern)
    case pat => visitType(pat.tpe)
  }

  private def visitRecordLabelPattern(pattern: Record.RecordLabelPattern)(implicit sctx: SharedContext): Unit = {
    visitPattern(pattern.pat)
    visitType(pattern.tpe)
  }

  private def visitTypeMatchRule(matchRule: TypedAst.TypeMatchRule)(implicit sctx: SharedContext): Unit = {
    visitBnd(matchRule.bnd)
    visitType(matchRule.tpe)
    visitExp(matchRule.exp)
  }

  private def visitRestrictableChooseRule(rule: TypedAst.RestrictableChooseRule)(implicit sctx: SharedContext): Unit = {
    visitExp(rule.exp)
    visitRestrictableChoosePattern(rule.pat)
  }

  private def visitRestrictableChoosePattern(pattern: TypedAst.RestrictableChoosePattern)(implicit sctx: SharedContext): Unit = pattern match {
    case RestrictableChoosePattern.Tag(sym, pats, tpe, _) => visitSymUse(sym)
      visitType(tpe)
      pats.foreach(visitVarOrWild)
    case RestrictableChoosePattern.Error(tpe, _) => visitType(tpe)
  }

  private def visitVarOrWild(pattern: TypedAst.RestrictableChoosePattern.VarOrWild)(implicit sctx: SharedContext): Unit = pattern match {
    case RestrictableChoosePattern.Var(bnd, tpe, _) => visitBnd(bnd)
      visitType(tpe)
    case RestrictableChoosePattern.Wild(tpe, _) => visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) => visitType(tpe)
  }

  private def visitField(field: (SymUse.StructFieldSymUse, Expr))(implicit sctx: SharedContext): Unit = {
    visitSymUse(field._1)
    visitExp(field._2)
  }

  private def visitCatchRule(catchRule: TypedAst.CatchRule)(implicit sctx: SharedContext): Unit = {
    visitBnd(catchRule.bnd)
    visitExp(catchRule.exp)
  }

  private def visitHandlerRule(handlerRule: TypedAst.HandlerRule)(implicit sctx: SharedContext): Unit = {
    visitSymUse(handlerRule.op)
    visitExp(handlerRule.exp)
    handlerRule.fparams.foreach(visitFParam)
  }

  private def visitJvmMethod(method: TypedAst.JvmMethod)(implicit sctx: SharedContext): Unit = {
    visitType(method.retTpe)
    visitType(method.eff)
    visitExp(method.exp)
    method.fparams.foreach(visitFParam)
  }

  private def visitSelectChannelRule(rule: TypedAst.SelectChannelRule)(implicit sctx: SharedContext): Unit = {
    visitBnd(rule.bnd)
    visitExp(rule.chan)
    visitExp(rule.exp)
  }

  private def visitParYieldFragment(fragment: TypedAst.ParYieldFragment)(implicit sctx: SharedContext): Unit = {
    visitExp(fragment.exp)
    visitPattern(fragment.pat)
  }

  private def visitConstrait(constraint: TypedAst.Constraint)(implicit sctx: SharedContext): Unit = {
    visitHead(constraint.head)
    constraint.cparams.foreach(visitContraintParam)
    constraint.body.foreach(visitConstraintBody)
  }

  private def visitHead(head: TypedAst.Predicate.Head)(implicit sctx: SharedContext): Unit = head match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, tpe, _) => visitType(tpe)
      terms.foreach(visitExp)
  }

  private def visitContraintParam(cp: TypedAst.ConstraintParam)(implicit sctx: SharedContext): Unit = {
    visitType(cp.tpe)
    visitBnd(cp.bnd)
  }

  private def visitConstraintBody(cb: Body)(implicit sctx: SharedContext): Unit = cb match {
    case Body.Atom(_, _, _, _, terms, tpe, _) => visitType(tpe)
      terms.foreach(visitPattern)
    case Body.Functional(outBnds, exp, _) => visitExp(exp)
      outBnds.foreach(visitBnd)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  private def visitPParam(pparam: TypedAst.PredicateParam)(implicit sctx: SharedContext): Unit =
    visitType(pparam.tpe)

  private def visitOp(Op: TypedAst.Op)(implicit sctx: SharedContext): Unit =
    visitSpec(Op.spec)

  private def visitCase(cas: TypedAst.Case)(implicit sctx: SharedContext): Unit = {
    visitScheme(cas.sc)
    cas.tpes.foreach(visitType)
  }

  private def visitStructField(structField: TypedAst.StructField)(implicit sctx: SharedContext): Unit =
    visitType(structField.tpe)

  private def visitAssocTypeSig(assoc: TypedAst.AssocTypeSig)(implicit sctx: SharedContext): Unit =
    assoc.tpe.toList.foreach(visitType)

  private def visitSig(sig: TypedAst.Sig)(implicit sctx: SharedContext): Unit = {
    visitSpec(sig.spec)
    sig.exp.toList.foreach(visitExp)
  }

  private def visitExp(exp: Expr)(implicit sctx: SharedContext): Unit = exp match {
    case Expr.Cst(_, tpe, _) => visitType(tpe)

    case Expr.Var(_, tpe, _) => visitType(tpe)

    case Expr.Hole(_, tpe, eff, _) => visitType(tpe)
      visitType(eff)

    case Expr.HoleWithExp(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.OpenAs(symUse, exp, tpe, _) => visitSymUse(symUse)
      visitExp(exp)
      visitType(tpe)

    case Expr.Use(_, _, exp, _) => visitExp(exp)

    case Expr.Lambda(fparam, exp, tpe, _) => visitFParam(fparam)
      visitExp(exp)
      visitType(tpe)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplyDef(symUse, exps, itpe, tpe, eff, _) => visitSymUse(symUse)
      visitType(itpe)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, _) => visitSymUse(symUse)
      visitType(arrowTpe)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.ApplySig(symUse, exps, itpe, tpe, eff, _) => visitSymUse(symUse)
      visitType(itpe)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.Unary(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Binary(_, exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Let(bnd, exp1, exp2, tpe, eff, _) => visitBnd(bnd)
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, _) => visitBnd(bnd)
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)
      fparams.foreach(visitFParam)

    case Expr.Region(tpe, _) => visitType(tpe)

    case Expr.Scope(bnd, _, exp, tpe, eff, _) => visitBnd(bnd)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(tpe)
      visitType(eff)

    case Expr.Stm(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Discard(exp, eff, _) => visitExp(exp)
      visitType(eff)

    case Expr.Match(exp, rules, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      rules.foreach(visitMatchRule)

    case Expr.TypeMatch(exp, rules, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      rules.foreach(visitTypeMatchRule)

    case Expr.RestrictableChoose(_, exp, rules, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      rules.foreach(visitRestrictableChooseRule)

    case Expr.Tag(sym, exps, tpe, eff, _) => visitSymUse(sym)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.RestrictableTag(sym, exps, tpe, eff, _) => visitSymUse(sym)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.Tuple(exps, tpe, eff, _) => exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordEmpty(tpe, _) => visitType(tpe)

    case Expr.RecordSelect(exp, _, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordExtend(_, exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordRestrict(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLit(exps, exp, tpe, eff, _) => exps.foreach(visitExp)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLength(exp, eff, _) => visitExp(exp)
      visitType(eff)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(eff)

    case Expr.StructNew(_, fields, region, tpe, eff, _) => fields.foreach(visitField)
      visitExp(region)
      visitType(tpe)
      visitType(eff)

    case Expr.StructGet(exp, sym, tpe, eff, _) => visitExp(exp)
      visitSymUse(sym)
      visitType(tpe)
      visitType(eff)

    case Expr.StructPut(exp1, sym, exp2, tpe, eff, _) => visitExp(exp1)
      visitSymUse(sym)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLit(exps, tpe, eff, _) => exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLength(exp, _) => visitExp(exp)

    case Expr.Ascribe(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.InstanceOf(exp, _, _) => visitExp(exp)

    case Expr.CheckedCast(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      declaredType.toList.foreach(visitType)
      declaredEff.toList.foreach(visitType)

    case Expr.Unsafe(exp, runEff, tpe, eff, _) => visitExp(exp)
      visitType(runEff)
      visitType(tpe)
      visitType(eff)

    case Expr.Without(exp, effUse, tpe, eff, _) => visitExp(exp)
      visitSymUse(effUse)
      visitType(tpe)
      visitType(eff)

    case Expr.TryCatch(exp, rules, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      rules.foreach(visitCatchRule)

    case Expr.Throw(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.TryWith(exp, effUse, rules, tpe, eff, _) => visitExp(exp)
      visitSymUse(effUse)
      visitType(tpe)
      visitType(eff)
      rules.foreach(visitHandlerRule)

    case Expr.Do(op, exps, tpe, eff, _) => visitSymUse(op)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.InvokeConstructor(_, exps, tpe, eff, _) => exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeMethod(_, exp, exps, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      exps.foreach(visitExp)

    case Expr.InvokeStaticMethod(_, exps, tpe, eff, _) => exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.GetField(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.PutField(_, exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.GetStaticField(_, tpe, eff, _) => visitType(tpe)
      visitType(eff)

    case Expr.PutStaticField(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.NewObject(_, _, tpe, eff, methods, _) => visitType(tpe)
      visitType(eff)
      methods.foreach(visitJvmMethod)

    case Expr.NewChannel(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.PutChannel(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.SelectChannel(rules, default, tpe, eff, _) => visitType(tpe)
      visitType(eff)
      rules.foreach(visitSelectChannelRule)
      default.toList.foreach(visitExp)

    case Expr.Spawn(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ParYield(frags, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      frags.foreach(visitParYieldFragment)

    case Expr.Lazy(exp, tpe, _) => visitExp(exp)
      visitType(tpe)

    case Expr.Force(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointConstraintSet(cs, tpe, _) => visitType(tpe)
      cs.foreach(visitConstrait)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
      pparams.foreach(visitPParam)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, _) => visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointSolve(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointFilter(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointInject(exp, _, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointProject(_, exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Error(_, tpe, eff) => visitType(tpe)
      visitType(eff)

    case Expr.GetChannel(exp, tpe, eff, _) => visitExp(exp)
      visitType(tpe)
      visitType(eff)
  }

  private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext): Unit = {
    visitExp(defn.exp)
    visitSpec(defn.spec)
  }

  private def visitEff(eff: TypedAst.Effect)(implicit sctx: SharedContext): Unit = eff.ops.foreach(visitOp)

  private def visitEnum(enm: TypedAst.Enum)(implicit sctx: SharedContext): Unit =
    enm.cases.values.foreach(visitCase)

  private def visitStruct(struct: TypedAst.Struct)(implicit sctx: SharedContext): Unit = {
    visitScheme(struct.sc)
    struct.fields.values.foreach(visitStructField)
  }

  private def visitTrait(trt: TypedAst.Trait)(implicit sctx: SharedContext): Unit = {
    trt.superTraits.foreach(visitTraitConstraint)
    trt.assocs.foreach(visitAssocTypeSig)
    trt.sigs.foreach(visitSig)
    trt.laws.foreach(visitDef)
  }

  private def visitTypeAlias(typeAlias: TypedAst.TypeAlias)(implicit sctx: SharedContext): Unit =
    visitType(typeAlias.tpe)

  private case class SharedContext(deps: ConcurrentMap[(Input, Input), Unit])
}
