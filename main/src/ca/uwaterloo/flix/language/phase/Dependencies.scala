package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.api.lsp.acceptors.AllAcceptor
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Pattern, Predicate, RestrictableChoosePattern, Root}
import ca.uwaterloo.flix.language.ast.shared.{BroadEqualityConstraint, DependencyGraph, EqualityConstraint, Input, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.MultiMap

object Dependencies {

  /** Checks the safety and well-formedness of `root`. */
  def run(root: Root)(implicit flix: Flix): (Root, Unit) = flix.phaseNew("Dependencies") {
    val defDeps = ParOps.parMap(root.defs.values)(visitDef).flatten
    val effDeps = ParOps.parMap(root.effects.values)(visitEff).flatten
    val enumDeps = ParOps.parMap(root.enums.values)(visitEnum).flatten
    val instanceDeps = ParOps.parMap(root.instances.values)(visitInstances).flatten
    val structDeps = ParOps.parMap(root.structs.values)(visitStruct).flatten
    // TODO: We should not depend on Consumer from LSP. Instead we should traverse the AST manually.
    // Moreover, we should traverse the AST in parallel and using changeSet.
    //
    //    object consumer extends Consumer {
    //
    //      /**
    //        * Adds a dependency `src -> dst` signifying that if `src` changes then `dst` must be recomputed.
    //        */
    //
    //      override def consumeAssocTypeSymUse(symUse: SymUse.AssocTypeSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeCaseSymUse(symUse: SymUse.CaseSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeDefSymUse(symUse: SymUse.DefSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeEffectSymUse(symUse: SymUse.EffectSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeInstance(ins: TypedAst.Instance): Unit = {
    //        addDependency(ins.trt.sym.loc, ins.loc)
    //      }
    //
    //      override def consumeOpSymUse(symUse: SymUse.OpSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = {
    //        addDependency(symUse.sym.loc, symUse.loc)
    //      }
    //
    //      override def consumeType(tpe: Type): Unit = tpe match {
    //        case Type.Alias(cst, _, _, loc) =>
    //          addDependency(cst.sym.loc, loc)
    //
    //        case Type.Cst(TypeConstructor.Enum(sym, _), loc) =>
    //          addDependency(sym.loc, loc)
    //
    //        case Type.Cst(TypeConstructor.Struct(sym, _), loc) =>
    //          addDependency(sym.loc, loc)
    //
    //        case _ => // nop
    //      }
    //
    //
    //    }
    //
    //    Visitor.visitRoot(root, consumer, AllAcceptor)
    //

    var deps: MultiMap[Input, Input] = MultiMap.empty
    defDeps.foreach(dep => deps += (dep._1.sp1.source.input, dep._2.sp1.source.input))
    val dg = DependencyGraph(deps)
    (root.copy(dependencyGraph = dg), ())
  }

  private def visitDef(defn: TypedAst.Def): List[(SourceLocation, SourceLocation)] =
    visitExp(defn.exp) ++ visitSpec(defn.spec)

  private def visitType(tpe: Type): List[(SourceLocation, SourceLocation)] = tpe match {
    case Type.Alias(cst, _, _, loc) => List((cst.loc, loc))
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => List((sym.loc, loc))
    case Type.Cst(TypeConstructor.Struct(sym, _), loc) => List((sym.loc, loc))
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

  private def visitBnd(bnd: TypedAst.Binder): List[(SourceLocation, SourceLocation)] = bnd match {
    case TypedAst.Binder(_, tpe) => visitType(tpe)
  }

  private def visitFParam(fparam: TypedAst.FormalParam): List[(SourceLocation, SourceLocation)] =
    visitBnd(fparam.bnd) ++ visitType(fparam.tpe)

  private def visitMatchRule(matchRule: TypedAst.MatchRule): List[(SourceLocation, SourceLocation)] =
    visitPattern(matchRule.pat) ++ matchRule.guard.toList.flatMap(visitExp) ++ visitExp(matchRule.exp)

  private def visitPattern(pattern: TypedAst.Pattern): List[(SourceLocation, SourceLocation)] = pattern match {
    case Pattern.Var(bnd, tpe, loc) => visitBnd(bnd) ++ visitType(tpe)
    case Pattern.Tag(sym, pats, tpe, loc) => visitSymUse(sym) ++ pats.flatMap(visitPattern) ++ visitType(tpe)
    case Pattern.Tuple(pats, tpe, loc) => pats.flatMap(visitPattern) ++ visitType(tpe)
    case Pattern.Record(pats, pat, tpe, loc) => pats.flatMap(visitRecordLabelPattern) ++ visitPattern(pat) ++ visitType(tpe)
    case pat => visitType(pat.tpe)
  }

  private def visitRecordLabelPattern(pattern: Record.RecordLabelPattern): List[(SourceLocation, SourceLocation)] =
    visitPattern(pattern.pat) ++ visitType(pattern.tpe)

  private def visitTypeMatchRule(matchRule: TypedAst.TypeMatchRule): List[(SourceLocation, SourceLocation)] =
    visitBnd(matchRule.bnd) ++ visitType(matchRule.tpe) ++ visitExp(matchRule.exp)

  private def visitRestrictableChooseRule(rule: TypedAst.RestrictableChooseRule): List[(SourceLocation, SourceLocation)] =
    visitExp(rule.exp) ++ visitRestrictableChoosePattern(rule.pat)

  private def visitRestrictableChoosePattern(pattern: TypedAst.RestrictableChoosePattern): List[(SourceLocation, SourceLocation)] = pattern match {
    case RestrictableChoosePattern.Tag(sym, pats, tpe, loc) => pats.flatMap(visitVarOrWild) ++ visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, loc) => visitType(tpe)
  }

  private def visitVarOrWild(pattern: TypedAst.RestrictableChoosePattern.VarOrWild): List[(SourceLocation, SourceLocation)] = pattern match {
    case RestrictableChoosePattern.Var(bnd, tpe, loc) => visitBnd(bnd) ++ visitType(tpe)
    case RestrictableChoosePattern.Wild(tpe, loc) => visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, loc) => visitType(tpe)
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
    constraint.cparams.flatMap(visitContraintParam) ++ constraint.body.flatMap(visitConstraintBody)

  private def visitContraintParam(cp: TypedAst.ConstraintParam): List[(SourceLocation, SourceLocation)] =
    visitType(cp.tpe) ++ visitBnd(cp.bnd)

  private def visitConstraintBody(cb: Body): List[(SourceLocation, SourceLocation)] = cb match {
    case Body.Atom(pred, den, polarity, fixity, terms, tpe, loc) => terms.flatMap(visitPattern) ++ visitType(tpe)
    case Body.Functional(outBnds, exp, loc) => outBnds.flatMap(visitBnd) ++ visitExp(exp)
    case Body.Guard(exp, loc) => visitExp(exp)
  }

  private def visitPParam(pparam: TypedAst.PredicateParam): List[(SourceLocation, SourceLocation)] =
    visitType(pparam.tpe)

  private def visitOp(Op: TypedAst.Op): List[(SourceLocation, SourceLocation)] =
    visitSpec(Op.spec)

  private def visitCase(cas: TypedAst.Case): List[(SourceLocation, SourceLocation)] =
    cas.tpes.flatMap(visitType) ++ visitScheme(cas.sc)

  private def visitAssocTypeDef(assoc: TypedAst.AssocTypeDef): List[(SourceLocation, SourceLocation)] =
    visitSymUse(assoc.sym) ++ visitType(assoc.arg) ++ visitType(assoc.tpe)

  private def visitStructField(structField: TypedAst.StructField): List[(SourceLocation, SourceLocation)] =
    visitType(structField.tpe)


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

    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, loc) => visitBnd(bnd) ++ fparams.flatMap(visitFParam) ++ visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Region(tpe, loc) => visitType(tpe)

    case Expr.Scope(bnd, regionVar, exp, tpe, eff, loc) => visitBnd(bnd) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Discard(exp, eff, loc) => visitExp(exp) ++ visitType(eff)

    case Expr.Match(exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.flatMap(visitMatchRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.flatMap(visitTypeMatchRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.flatMap(visitRestrictableChooseRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Tag(sym, exps, tpe, eff, loc) => visitSymUse(sym) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RestrictableTag(sym, exps, tpe, eff, loc) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Tuple(exps, tpe, eff, loc) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RecordEmpty(tpe, loc) => visitType(tpe)

    case Expr.RecordSelect(exp, label, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.RecordRestrict(label, exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => exps.flatMap(visitExp) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ArrayLength(exp, eff, loc) => visitExp(exp) ++ visitType(eff)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(eff)

    case Expr.StructNew(sym, fields, region, tpe, eff, loc) => fields.flatMap(visitField) ++ visitExp(region) ++ visitType(tpe) ++ visitType(eff)

    case Expr.StructGet(exp, sym, tpe, eff, loc) => visitExp(exp) ++ visitSymUse(sym) ++ visitType(tpe) ++ visitType(eff)

    case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitSymUse(sym) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.VectorLit(exps, tpe, eff, loc) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.VectorLength(exp, loc) => visitExp(exp)

    case Expr.Ascribe(exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InstanceOf(exp, clazz, loc) => visitExp(exp)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Unsafe(exp, runEff, tpe, eff, loc) => visitExp(exp) ++ visitType(runEff) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Without(exp, effUse, tpe, eff, loc) => visitExp(exp) ++ visitSymUse(effUse) ++ visitType(tpe) ++ visitType(eff)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.flatMap(visitCatchRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Throw(exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => visitExp(exp) ++ visitSymUse(effUse) ++ rules.flatMap(visitHandlerRule) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Do(op, exps, tpe, eff, loc) => visitSymUse(op) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => visitExp(exp) ++ exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => exps.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.GetField(field, exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.GetStaticField(field, tpe, eff, loc) => visitType(tpe) ++ visitType(eff)

    case Expr.PutStaticField(field, exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => visitType(tpe) ++ visitType(eff) ++ methods.flatMap(visitJvmMethod)

    case Expr.NewChannel(exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.SelectChannel(rules, default, tpe, eff, loc) => rules.flatMap(visitSelectChannelRule) ++ default.toList.flatMap(visitExp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.ParYield(frags, exp, tpe, eff, loc) => frags.flatMap(visitParYieldFragment) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Lazy(exp, tpe, loc) => visitExp(exp) ++ visitType(tpe)

    case Expr.Force(exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointConstraintSet(cs, tpe, loc) => cs.flatMap(visitConstrait) ++ visitType(tpe)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => pparams.flatMap(visitPParam) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointSolve(exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.Error(m, tpe, eff) => visitType(tpe) ++ visitType(eff)

    case Expr.GetChannel(exp, tpe, eff, loc) => visitExp(exp) ++ visitType(tpe) ++ visitType(eff)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => visitExp(exp) ++ declaredType.toList.flatMap(visitType) ++ declaredEff.toList.flatMap(visitType) ++ visitType(tpe) ++ visitType(eff)
  }

  private def visitEff(eff: TypedAst.Effect): List[(SourceLocation, SourceLocation)] = eff.ops.flatMap(visitOp)

  private def visitEnum(enm: TypedAst.Enum): List[(SourceLocation, SourceLocation)] =
    enm.cases.values.flatMap(visitCase).toList

  private def visitInstances(instances: List[TypedAst.Instance]): List[(SourceLocation, SourceLocation)] =
    instances.flatMap(instance =>
      visitSymUse(instance.trt) ++ visitType(instance.tpe) ++ instance.tconstrs.flatMap(visitTraitConstraint) ++ instance.assocs.flatMap(visitAssocTypeDef) ++ instance.defs.flatMap(visitDef)
    )

  private def visitStruct(struct: TypedAst.Struct): List[(SourceLocation, SourceLocation)] =
    visitScheme(struct.sc) ++ struct.fields.values.flatMap(visitStructField)

  //  private def addDependency(src: SourceLocation, dst: SourceLocation)(implicit deps: MultiMap[String, String]): Unit = {
//    deps += (src.sp1.source.input, dst.sp1.source.input)
//  }
}
