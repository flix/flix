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

object Dependencies {
  /**
    * Updates the dependency graph of the given `root`.
    * We visit all the definitions, effects, enums, instances, structs, traits, and type aliases to find the dependencies.
    * Each visit will return a list from the source location to the destination location.
    *
    * Most of the time we just recursively visit the sub-nodes, we only add dependencies for three base cases:
    *   - Type
    *   - SymUse
    *   - Instance
    */
  def run(root: Root)(implicit flix: Flix): (Root, Unit) = flix.phaseNew("Dependencies") {
    val defDeps = ParOps.parMap(root.defs.values)(visitDef).flatten
    val effDeps = ParOps.parMap(root.effects.values)(visitEff).flatten
    val enumDeps = ParOps.parMap(root.enums.values)(visitEnum).flatten
    val instanceDeps = ParOps.parMap(root.instances.values)(visitInstances).flatten
    val structDeps = ParOps.parMap(root.structs.values)(visitStruct).flatten
    val traitDeps = ParOps.parMap(root.traits.values)(visitTrait).flatten
    val typeAliasDeps = ParOps.parMap(root.typeAliases.values)(visitTypeAlias).flatten
    val allDeps = defDeps ++ effDeps ++ enumDeps ++ instanceDeps ++ structDeps ++ traitDeps ++ typeAliasDeps

    val deps = allDeps.foldLeft(MultiMap.empty[Input, Input]) {
      case (acc, (src, dst)) => acc + (src.sp1.source.input, dst.sp1.source.input)
    }
    val dg = DependencyGraph(deps)
    (root.copy(dependencyGraph = dg), ())
  }

  /**
    * Returns the dependencies of the given type.
    * One of the base case for the recursive visiting.
    */
  private def visitType(tpe: Type): List[(SourceLocation, SourceLocation)] = tpe match {
    case Type.Alias(cst, _, _, loc) => List((cst.sym.loc, loc))
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => List((sym.loc, loc))
    case Type.Cst(TypeConstructor.Struct(sym, _), loc) => List((sym.loc, loc))
    case _ => Nil
  }

  /**
    * Returns the dependencies of the given SymUse.
    * One of the base case for the recursive visiting.
    */
  private def visitSymUse(use: SymUse.SymUse): List[(SourceLocation, SourceLocation)] = use match {
    case SymUse.AssocTypeSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.CaseSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.DefSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.EffectSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.OpSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.SigSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.StructFieldSymUse(sym, loc) => List((sym.loc, loc))
    case SymUse.TraitSymUse(sym, loc) => List((sym.loc, loc))
    case _ => Nil
  }

  private def visitSpec(spec: TypedAst.Spec): List[(SourceLocation, SourceLocation)] = {
    spec.fparams.flatMap(visitFParam) ++ visitType(spec.retTpe) ++ visitScheme(spec.declaredScheme) ++ visitType(spec.eff) ++
      spec.tconstrs.flatMap(visitTraitConstraint) ++ spec.econstrs.flatMap(visitEqualityConstraint)
  }

  private def visitScheme(scheme: Scheme): List[(SourceLocation, SourceLocation)] =
    scheme.tconstrs.flatMap(visitTraitConstraint) ++ scheme.econstrs.flatMap(visitBroadEqualityConstraint) ++ visitType(scheme.base)

  private def visitTraitConstraint(tc: TraitConstraint): List[(SourceLocation, SourceLocation)] =
    visitType(tc.arg)

  private def visitEqualityConstraint(ec: EqualityConstraint): List[(SourceLocation, SourceLocation)] =
    visitType(ec.tpe1) ++ visitType(ec.tpe2)

  private def visitBroadEqualityConstraint(ec: BroadEqualityConstraint): List[(SourceLocation, SourceLocation)] =
    visitType(ec.tpe1) ++ visitType(ec.tpe2)

  private def visitBnd(bnd: TypedAst.Binder): List[(SourceLocation, SourceLocation)] =
    visitType(bnd.tpe)

  private def visitFParam(fparam: TypedAst.FormalParam): List[(SourceLocation, SourceLocation)] =
    visitBnd(fparam.bnd) ++ visitType(fparam.tpe)

  private def visitMatchRule(matchRule: TypedAst.MatchRule): List[(SourceLocation, SourceLocation)] =
    visitPattern(matchRule.pat) ++ matchRule.guard.toList.flatMap(visitExp) ++ visitExp(matchRule.exp)

  private def visitPattern(pattern: TypedAst.Pattern): List[(SourceLocation, SourceLocation)] = pattern match {
    case Pattern.Var(bnd, tpe, _) => visitBnd(bnd) ++ visitType(tpe)
    case Pattern.Tag(sym, pats, tpe, _) => visitSymUse(sym) ++ pats.flatMap(visitPattern) ++ visitType(tpe)
    case Pattern.Tuple(pats, tpe, _) => pats.flatMap(visitPattern) ++ visitType(tpe)
    case Pattern.Record(pats, pat, tpe, _) => pats.flatMap(visitRecordLabelPattern) ++ visitPattern(pat) ++ visitType(tpe)
    case pat => visitType(pat.tpe)
  }

  private def visitRecordLabelPattern(pattern: Record.RecordLabelPattern): List[(SourceLocation, SourceLocation)] =
    visitPattern(pattern.pat) ++ visitType(pattern.tpe)

  private def visitTypeMatchRule(matchRule: TypedAst.TypeMatchRule): List[(SourceLocation, SourceLocation)] =
    visitBnd(matchRule.bnd) ++ visitType(matchRule.tpe) ++ visitExp(matchRule.exp)

  private def visitRestrictableChooseRule(rule: TypedAst.RestrictableChooseRule): List[(SourceLocation, SourceLocation)] =
    visitExp(rule.exp) ++ visitRestrictableChoosePattern(rule.pat)

  private def visitRestrictableChoosePattern(pattern: TypedAst.RestrictableChoosePattern): List[(SourceLocation, SourceLocation)] = pattern match {
    case RestrictableChoosePattern.Tag(sym, pats, tpe, _) => visitSymUse(sym) ++ pats.flatMap(visitVarOrWild) ++ visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) => visitType(tpe)
  }

  private def visitVarOrWild(pattern: TypedAst.RestrictableChoosePattern.VarOrWild): List[(SourceLocation, SourceLocation)] = pattern match {
    case RestrictableChoosePattern.Var(bnd, tpe, _) => visitBnd(bnd) ++ visitType(tpe)
    case RestrictableChoosePattern.Wild(tpe, _) => visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) => visitType(tpe)
  }

  private def visitField(field: (SymUse.StructFieldSymUse, Expr)): List[(SourceLocation, SourceLocation)] =
    visitSymUse(field._1) ++ visitExp(field._2)

  private def visitCatchRule(catchRule: TypedAst.CatchRule): List[(SourceLocation, SourceLocation)] =
    visitBnd(catchRule.bnd) ++ visitExp(catchRule.exp)

  private def visitHandlerRule(handlerRule: TypedAst.HandlerRule): List[(SourceLocation, SourceLocation)] =
    visitSymUse(handlerRule.op) ++ handlerRule.fparams.flatMap(visitFParam) ++ visitExp(handlerRule.exp)

  private def visitJvmMethod(method: TypedAst.JvmMethod): List[(SourceLocation, SourceLocation)] =
    method.fparams.flatMap(visitFParam) ++ visitType(method.retTpe) ++ visitType(method.eff) ++ visitExp(method.exp)

  private def visitSelectChannelRule(rule: TypedAst.SelectChannelRule): List[(SourceLocation, SourceLocation)] =
    visitBnd(rule.bnd) ++ visitExp(rule.chan) ++ visitExp(rule.exp)

  private def visitParYieldFragment(fragment: TypedAst.ParYieldFragment): List[(SourceLocation, SourceLocation)] =
    visitExp(fragment.exp) ++ visitPattern(fragment.pat)

  private def visitConstrait(constraint: TypedAst.Constraint): List[(SourceLocation, SourceLocation)] =
    constraint.cparams.flatMap(visitContraintParam) ++ visitHead(constraint.head) ++ constraint.body.flatMap(visitConstraintBody)

  private def visitHead(head: TypedAst.Predicate.Head): List[(SourceLocation, SourceLocation)] = head match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, tpe, _) => terms.flatMap(visitExp) ++ visitType(tpe)
  }

  private def visitContraintParam(cp: TypedAst.ConstraintParam): List[(SourceLocation, SourceLocation)] =
    visitType(cp.tpe) ++ visitBnd(cp.bnd)

  private def visitConstraintBody(cb: Body): List[(SourceLocation, SourceLocation)] = cb match {
    case Body.Atom(_, _, _, _, terms, tpe, _) => terms.flatMap(visitPattern) ++ visitType(tpe)
    case Body.Functional(outBnds, exp, _) => outBnds.flatMap(visitBnd) ++ visitExp(exp)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  private def visitPParam(pparam: TypedAst.PredicateParam): List[(SourceLocation, SourceLocation)] =
    visitType(pparam.tpe)

  private def visitOp(Op: TypedAst.Op): List[(SourceLocation, SourceLocation)] =
    visitSpec(Op.spec)

  private def visitCase(cas: TypedAst.Case): List[(SourceLocation, SourceLocation)] =
    cas.tpes.flatMap(visitType) ++ visitScheme(cas.sc)

  private def visitStructField(structField: TypedAst.StructField): List[(SourceLocation, SourceLocation)] =
    visitType(structField.tpe)

  private def visitAssocTypeSig(assoc: TypedAst.AssocTypeSig): List[(SourceLocation, SourceLocation)] =
    assoc.tpe.toList.flatMap(visitType)

  private def visitSig(sig: TypedAst.Sig): List[(SourceLocation, SourceLocation)] =
    visitSpec(sig.spec) ++ sig.exp.toList.flatMap(visitExp)

  private def visitExp(exp: Expr): List[(SourceLocation, SourceLocation)] = exp match {
    case Expr.Cst(_, tpe, _) => visitType(tpe)

    case Expr.Var(_, tpe, _) => visitType(tpe)

    case Expr.Hole(_, tpe, eff, _) => visitType(tpe) ++ visitType(eff)

    case Expr.HoleWithExp(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.OpenAs(symUse, exp, tpe, _) => visitSymUse(symUse) ++ visitExp(exp) ++ visitType(tpe)

    case Expr.Use(_, _, exp, _) => visitExp(exp)

    case Expr.Lambda(fparam, exp, tpe, _) => visitFParam(fparam) ++ visitExp(exp) ++ visitType(tpe)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ApplyDef(symUse, exps, itpe, tpe, eff, _) => visitSymUse(symUse) ++ exps.flatMap(visitExp) ++ visitType(itpe) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, _) => visitSymUse(symUse) ++ exps.flatMap(visitExp) ++ visitType(arrowTpe) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ApplySig(symUse, exps, itpe, tpe, eff, _) => visitSymUse(symUse) ++ exps.flatMap(visitExp) ++ visitType(itpe) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Unary(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Binary(_, exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Let(bnd, exp1, exp2, tpe, eff, _) => visitBnd(bnd) ++ visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, _) => visitBnd(bnd) ++ fparams.flatMap(visitFParam) ++ visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Region(tpe, _) => visitType(tpe)

    case Expr.Scope(bnd, _, exp, tpe, eff, _) => visitBnd(bnd) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Stm(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Discard(exp, eff, _) => visitExp(exp) ++ visitType(eff)

    case Expr.Match(exp, rules, tpe, eff, _) => visitExp(exp) ++ rules.flatMap(visitMatchRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.TypeMatch(exp, rules, tpe, eff, _) => visitExp(exp) ++ rules.flatMap(visitTypeMatchRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RestrictableChoose(_, exp, rules, tpe, eff, _) => visitExp(exp) ++ rules.flatMap(visitRestrictableChooseRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Tag(sym, exps, tpe, eff, _) => visitSymUse(sym) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RestrictableTag(sym, exps, tpe, eff, _) => visitSymUse(sym) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Tuple(exps, tpe, eff, _) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RecordEmpty(tpe, _) => visitType(tpe)

    case Expr.RecordSelect(exp, _, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RecordExtend(_, exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RecordRestrict(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayLit(exps, exp, tpe, eff, _) => exps.flatMap(visitExp) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayLength(exp, eff, _) => visitExp(exp) ++ visitType(eff)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(eff)

    case Expr.StructNew(_, fields, region, tpe, eff, _) => fields.flatMap(visitField) ++ visitExp(region) ++ visitType(tpe) ++ visitType(eff)

    case Expr.StructGet(exp, sym, tpe, eff, _) => visitExp(exp) ++ visitSymUse(sym) ++ visitType(tpe) ++ visitType(eff)

    case Expr.StructPut(exp1, sym, exp2, tpe, eff, _) => visitExp(exp1) ++ visitSymUse(sym) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.VectorLit(exps, tpe, eff, _) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.VectorLength(exp, _) => visitExp(exp)

    case Expr.Ascribe(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InstanceOf(exp, _, _) => visitExp(exp)

    case Expr.CheckedCast(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, _) => visitExp(exp) ++ declaredType.toList.flatMap(visitType) ++ declaredEff.toList.flatMap(visitType) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Unsafe(exp, runEff, tpe, eff, _) => visitExp(exp) ++ visitType(runEff) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Without(exp, effUse, tpe, eff, _) => visitExp(exp) ++ visitSymUse(effUse) ++ visitType(tpe) ++ visitType(eff)

    case Expr.TryCatch(exp, rules, tpe, eff, _) => visitExp(exp) ++ rules.flatMap(visitCatchRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Throw(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.TryWith(exp, effUse, rules, tpe, eff, _) => visitExp(exp) ++ visitSymUse(effUse) ++ rules.flatMap(visitHandlerRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Do(op, exps, tpe, eff, _) => visitSymUse(op) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InvokeConstructor(_, exps, tpe, eff, _) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InvokeMethod(_, exp, exps, tpe, eff, _) => visitExp(exp) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InvokeStaticMethod(_, exps, tpe, eff, _) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.GetField(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.PutField(_, exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.GetStaticField(_, tpe, eff, _) => visitType(tpe) ++ visitType(eff)

    case Expr.PutStaticField(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.NewObject(_, _, tpe, eff, methods, _) => visitType(tpe) ++ visitType(eff) ++ methods.flatMap(visitJvmMethod)

    case Expr.NewChannel(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.PutChannel(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.SelectChannel(rules, default, tpe, eff, _) => rules.flatMap(visitSelectChannelRule) ++ default.toList.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Spawn(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ParYield(frags, exp, tpe, eff, _) => frags.flatMap(visitParYieldFragment) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Lazy(exp, tpe, _) => visitExp(exp) ++ visitType(tpe)

    case Expr.Force(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointConstraintSet(cs, tpe, _) => cs.flatMap(visitConstrait) ++ visitType(tpe)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, _) => pparams.flatMap(visitPParam) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointSolve(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointFilter(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointInject(exp, _, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointProject(_, exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Error(_, tpe, eff) => visitType(tpe) ++ visitType(eff)

    case Expr.GetChannel(exp, tpe, eff, _) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)
  }

  private def visitDef(defn: TypedAst.Def): List[(SourceLocation, SourceLocation)] =
    visitExp(defn.exp) ++ visitSpec(defn.spec)

  private def visitEff(eff: TypedAst.Effect): List[(SourceLocation, SourceLocation)] = eff.ops.flatMap(visitOp)

  private def visitEnum(enm: TypedAst.Enum): List[(SourceLocation, SourceLocation)] =
    enm.cases.values.flatMap(visitCase).toList

  /**
    * Returns the dependencies of the given instance.
    * One of the base case for the recursive visiting.
    */
  private def visitInstances(instances: List[TypedAst.Instance]): List[(SourceLocation, SourceLocation)] =
    instances.map(instance => (instance.trt.sym.loc, instance.loc))

  private def visitStruct(struct: TypedAst.Struct): List[(SourceLocation, SourceLocation)] =
    visitScheme(struct.sc) ++ struct.fields.values.flatMap(visitStructField)

  private def visitTrait(trt: TypedAst.Trait): List[(SourceLocation, SourceLocation)] =
    trt.superTraits.flatMap(visitTraitConstraint) ++ trt.assocs.flatMap(visitAssocTypeSig) ++ trt.sigs.flatMap(visitSig) ++ trt.laws.flatMap(visitDef)

  private def visitTypeAlias(typeAlias: TypedAst.TypeAlias): List[(SourceLocation, SourceLocation)] =
    visitType(typeAlias.tpe)
}
