/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.*
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.TypedAst.{AssocTypeDef, Instance, *}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}

object Visitor {
  /**
    * Visits the root AST node and recursively visits
    * all children that are accepted by the acceptor,
    * "consuming" each node visited (including the root).
    *
    * By "consuming" what is meant is that the node is used
    * as the input to a function of the consumer associated with
    * the AST node type. Such consumer functions have no output,
    * but can have effects. For instance, the `consumer` can be defined such that each
    * expression containing a specific variable is collected in a list, via
    * mutation a variable.
    *
    * Note that if a node is not accepted, none of the nodes in its
    * subtree will be visited either. For instance, if a [[Def]] is not
    * accepted, none of the expressions in its `exp` will be visited eiter,
    * even if they would otherwise be accepted by the `acceptor`.
    *
    * @param root     The AST root node.
    * @param consumer A [[Consumer]] that defines what to do when visiting different types of AST nodes.
    * @param acceptor An [[Acceptor]] that defines the criteria for whether an AST node should be visited.
    */
  def visitRoot(root: Root, consumer: Consumer, acceptor: Acceptor): Unit = {

    implicit val c: Consumer = consumer
    implicit val a: Acceptor = acceptor

    // NB: the signatures in `root.sigs` are not visited here, since they will be visited after
    // their corresponding trait

    root.defs.values.foreach(visitDef)

    root.effects.values.foreach(visitEffect)

    root.enums.values.foreach(visitEnum)

    root.instances.values.foreach(visitInstance)

    root.structs.values.foreach(visitStruct)

    root.traits.values.foreach(visitTrait)

    root.typeAliases.values.foreach(visitTypeAlias)
  }

  private def visitEnum(enm: Enum)(implicit a: Acceptor, c: Consumer): Unit = {
    val Enum(_, ann, _, _, tparams, derives, cases, loc) = enm
    if (!a.accept(loc)) {
      return
    }

    c.consumeEnum(enm)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    visitDeriveList(derives)
    cases.values.foreach(visitCase)
  }

  private def visitDeriveList(derivations: Derivations)(implicit a: Acceptor, c: Consumer): Unit = {
    val Derivations(traits, loc) = derivations
    if (!a.accept(loc)) {
      return
    }

    c.consumeDerivations(derivations)

    traits.foreach(visitDerive)
  }

  private def visitDerive(derive: Derivation)(implicit a: Acceptor, c: Consumer): Unit = {
    val Derivation(_, loc) = derive
    if (!a.accept(loc)) {
      return
    }

    c.consumeDerivation(derive)
  }

  private def visitCase(cse: Case)(implicit a: Acceptor, c: Consumer): Unit = {
    val Case(_, tpes, _, loc) = cse
    if (!a.accept(loc)) {
      return
    }

    c.consumeCase(cse)
    tpes.foreach(visitType)
  }

  private def visitInstance(ins: Instance)(implicit a: Acceptor, c: Consumer): Unit = {
    val Instance(_, ann, _, trt, _, _, tconstrs, econstrs, assocs, defs, _, loc) = ins
    if (!a.accept(loc)) {
      return
    }

    c.consumeInstance(ins)

    visitAnnotations(ann)
    visitTraitSymUse(trt)
    tconstrs.foreach(visitTraitConstraint)
    econstrs.foreach(visitEqualityConstraint)
    assocs.foreach(visitAssocTypeDef)
    defs.foreach(visitDef)
  }

  private def visitTraitSymUse(symUse: TraitSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeTraitSymUse(symUse)
  }

  private def visitTraitConstraint(tc: TraitConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitConstraint(symUse, arg, loc) = tc
    if (!a.accept(loc)) {
      return
    }

    c.consumeTraitConstraint(tc)

    visitTraitSymUse(symUse)
    visitType(arg)
  }

  private def visitAssocTypeDef(tdefn: AssocTypeDef)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeDef(_, _, symUse, arg, _, loc) = tdefn
    if (!a.accept(loc)) {
      return
    }

    c.consumeAssocTypeDef(tdefn)

    visitAssocTypeSymUse(symUse)
    visitType(arg)
  }

  private def visitAssocTypeSymUse(symUse: AssocTypeSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeAssocTypeSymUse(symUse)
  }

  private def visitSig(sig: Sig)(implicit a: Acceptor, c: Consumer): Unit = {
    val Sig(_, spec, exp, loc) = sig
    if (!a.accept(loc)) {
      return
    }

    c.consumeSig(sig)

    visitSpec(spec)
    exp.foreach(visitExpr)
  }

  private def visitStruct(struct: Struct)(implicit a: Acceptor, c: Consumer): Unit = {
    val Struct(_, ann, _, _, tparams, _, fields, loc) = struct
    if (!a.accept(loc)) {
      return
    }

    c.consumeStruct(struct)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    fields.values.foreach(visitStructField)
  }

  private def visitStructField(field: StructField)(implicit a: Acceptor, c: Consumer): Unit = {
    val StructField(_, tpe, loc) = field
    if (!a.accept(loc)) {
      return
    }

    c.consumeStructField(field)

    visitType(tpe)
  }

  private def visitTrait(t: Trait)(implicit a: Acceptor, c: Consumer): Unit = {
    val Trait(_, ann, _, _, tparam, superTraits, assocs, sigs, laws, loc) = t
    if (!a.accept(loc)) {
      return
    }

    c.consumeTrait(t)

    visitAnnotations(ann)
    visitTypeParam(tparam)
    superTraits.foreach(visitTraitConstraint)
    assocs.foreach(visitAssocTypeSig)
    sigs.foreach(visitSig)
    laws.foreach(visitDef)
  }

  private def visitAssocTypeSig(assoc: AssocTypeSig)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeSig(_, _, _, tparam, _, tpe, loc) = assoc
    if (!a.accept(loc)) {
      return
    }

    c.consumeAssocTypeSig(assoc)

    visitTypeParam(tparam)
    tpe.foreach(visitType)
  }

  private def visitEffect(eff: Effect)(implicit a: Acceptor, c: Consumer): Unit = {
    val Effect(_, ann, _, _, tparams, ops, loc) = eff
    if (!a.accept(loc)) {
      return
    }

    c.consumeEff(eff)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    ops.foreach(visitOp)
  }

  private def visitOp(op: Op)(implicit a: Acceptor, c: Consumer): Unit = {
    val Op(_, spec, loc) = op
    if (!a.accept(loc)) {
      return
    }

    c.consumeOp(op)

    visitSpec(spec)
  }

  private def visitDef(defn: Def)(implicit a: Acceptor, c: Consumer): Unit = {
    val Def(_, spec, exp, loc) = defn
    if (!a.accept(loc)) {
      return
    }

    c.consumeDef(defn)

    visitSpec(spec)
    visitExpr(exp)
  }

  private def visitSpec(spec: Spec)(implicit a: Acceptor, c: Consumer): Unit = {
    val Spec(_, ann, _, tparams, fparams, _, retTpe, eff, tconstrs, econstrs) = spec

    // NB: Specs should not be consumed and when they occur, their corresponding
    // Def, Sig or Op is visited, they should always be visited as well.

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    fparams.foreach(visitFormalParam)
    visitType(retTpe)
    visitType(eff)
    tconstrs.foreach(visitTraitConstraint)
    econstrs.foreach(visitEqualityConstraint)
  }

  private def visitEqualityConstraint(ec: EqualityConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val EqualityConstraint(symUse, tpe1, tpe2, loc) = ec
    if (!a.accept(loc)) {
      return
    }

    c.consumeEqualityConstraint(ec)

    visitAssocTypeSymUse(symUse)
    visitType(tpe1)
    visitType(tpe2)
  }

  private def visitTypeAlias(alias: TypeAlias)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeAlias(_, ann, _, _, tparams, tpe, loc) = alias
    if (!a.accept(loc)) {
      return
    }

    c.consumeTypeAlias(alias)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    visitType(tpe)
  }

  private def visitTypeParam(tparam: TypeParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeParam(_, _, loc) = tparam
    if (!a.accept(loc)) {
      return
    }

    c.consumeTypeParam(tparam)
  }

  private def visitExpr(exp: Exp)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(exp.loc)) {
      return
    }

    c.consumeExpr(exp)

    exp match {
      case Exp.Cst(_, _, _) => ()
      case Exp.Var(_, _, _) => ()
      case Exp.Hole(_, _, _, _, _) => ()

      case Exp.HoleWithExp(exp, _, _, _, _) =>
        visitExpr(exp)

      case Exp.OpenAs(_, _, _, _) => () // Not visited, unsupported feature.

      case Exp.Use(_, _, exp, _) =>
        visitExpr(exp)

      case Exp.Lambda(fparam, exp, _, _) =>
        visitFormalParam(fparam)
        visitExpr(exp)

      case Exp.ApplyClo(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.ApplyDef(symUse, exps, _, _, _, _, _) =>
        visitDefSymUse(symUse)
        exps.foreach(visitExpr)

      case Exp.ApplyLocalDef(symUse, exps, _, _, _, _) =>
        visitLocalDefSymUse(symUse)
        exps.foreach(visitExpr)

      case Exp.ApplyOp(op, exps, _, _, _) =>
        visitOpSymUse(op)
        exps.foreach(visitExpr)

      case Exp.ApplySig(symUse, exps, _, _, _, _, _, _) =>
        visitSigSymUse(symUse)
        exps.foreach(visitExpr)

      case Exp.Unary(_, exp, _, _, _) =>
        visitExpr(exp)

      case Exp.Binary(_, exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.Let(bnd, exp1, exp2, _, _, _) =>
        visitBinder(bnd)
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.LocalDef(bnd, fparams, exp1, exp2, _, _, _) =>
        visitBinder(bnd)
        fparams.foreach(visitFormalParam)
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.Region(bnd, _, exp, _, _, _) =>
        visitBinder(bnd)
        visitExpr(exp)

      case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Exp.Stm(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.Discard(exp, _, _) =>
        visitExpr(exp)

      case Exp.Match(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitMatchRule)

      case Exp.TypeMatch(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitTypeMatchRule)

      case Exp.RestrictableChoose(_, _, _, _, _, _) => () // Not visited, unsupported feature.

      case Exp.ExtMatch(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitExtMatchRule)

      case Exp.Tag(symUse, exps, _, _, _) =>
        visitCaseSymUse(symUse)
        exps.foreach(visitExpr)

      case Exp.RestrictableTag(_, _, _, _, _) => () // Not visited, unsupported feature.

      case Exp.ExtTag(_, exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.Tuple(exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.RecordSelect(exp, _, _, _, _) =>
        visitExpr(exp)

      case Exp.RecordExtend(_, exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.RecordRestrict(_, exp, _, _, _) =>
        visitExpr(exp)

      case Exp.ArrayLit(exps, exp, _, _, _) =>
        visitExpr(exp)
        exps.foreach(visitExpr)

      case Exp.ArrayNew(exp1, exp2, exp3, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Exp.ArrayLoad(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.ArrayLength(exp, _, _) =>
        visitExpr(exp)

      case Exp.ArrayStore(exp1, exp2, exp3, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Exp.StructNew(_, fields, region, _, _, _) =>
        fields.foreach {
          case (symUse, exp) =>
            visitStructFieldSymUse(symUse)
            visitExpr(exp)
        }
        visitExpr(region)

      case Exp.StructGet(exp, symUse, _, _, _) =>
        visitExpr(exp)
        visitStructFieldSymUse(symUse)

      case Exp.StructPut(exp1, symUse, exp2, _, _, _) =>
        visitExpr(exp1)
        visitStructFieldSymUse(symUse)
        visitExpr(exp2)

      case Exp.VectorLit(exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.VectorLoad(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.VectorLength(exp, _) =>
        visitExpr(exp)

      case Exp.Ascribe(exp, _, _, _, _, _) =>
        visitExpr(exp)

      case Exp.InstanceOf(exp, _, _) =>
        visitExpr(exp)

      case Exp.CheckedCast(_, exp, _, _, _) =>
        visitExpr(exp)

      case Exp.UncheckedCast(exp, declaredType, declaredEff, _, _, _) =>
        visitExpr(exp)
        declaredType.foreach(visitType)
        declaredEff.foreach(visitType)

      case Exp.Unsafe(exp, runEff, _, _, _) =>
        // runEff is first syntactically
        visitType(runEff)
        visitExpr(exp)

      case Exp.Without(exp, symUse, _, _, _) =>
        visitExpr(exp)
        visitEffSymUse(symUse)

      case Exp.TryCatch(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitCatchRule)

      case Exp.Throw(exp, _, _, _) =>
        visitExpr(exp)

      case Exp.Handler(symUse, rules, _, _, _, _, _) =>
        visitEffSymUse(symUse)
        rules.foreach(visitHandlerRule)

      case Exp.RunWith(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.InvokeConstructor(_, exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.InvokeMethod(_, exp, exps, _, _, _) =>
        visitExpr(exp)
        exps.foreach(visitExpr)

      case Exp.InvokeStaticMethod(_, exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.GetField(_, exp, _, _, _) =>
        visitExpr(exp)

      case Exp.PutField(_, exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.GetStaticField(_, _, _, _) => ()

      case Exp.PutStaticField(_, exp, _, _, _) =>
        visitExpr(exp)

      case Exp.NewObject(_, _, _, _, methods, _) =>
        methods.foreach(visitJvmMethod)

      case Exp.NewChannel(exp, _, _, _) =>
        visitExpr(exp)

      case Exp.GetChannel(exp, _, _, _) =>
        visitExpr(exp)

      case Exp.PutChannel(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.SelectChannel(rules, default, _, _, _) =>
        rules.foreach(visitSelectChannelRule)
        default.foreach(visitExpr)

      case Exp.Spawn(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.ParYield(frags, exp, _, _, _) =>
        visitExpr(exp)
        frags.foreach(visitParYieldFrag)

      case Exp.Lazy(exp, _, _) =>
        visitExpr(exp)

      case Exp.Force(exp, _, _, _) =>
        visitExpr(exp)

      case Exp.FixpointConstraintSet(cs, _, _) =>
        cs.foreach(visitConstraint)

      case Exp.FixpointLambda(pparams, exp, _, _, _) =>
        pparams.foreach(visitPredicateParam)
        visitExpr(exp)

      case Exp.FixpointMerge(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Exp.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
        exps.foreach(visitExpr)
        visitPredicate(select)

      case Exp.FixpointQueryWithSelect(exps, queryExp, selects, from, where, _, _, _, _) =>
        exps.foreach(visitExpr)
        visitExpr(queryExp)
        selects.foreach(visitExpr)
        from.foreach(visitPredicate)
        where.foreach(visitExpr)

      case Exp.FixpointSolveWithProject(exps, _, _, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.FixpointInjectInto(exps, _, _, _, _) =>
        exps.foreach(visitExpr)

      case Exp.Error(_, _, _) => ()
    }
  }

  private def visitBinder(bnd: Binder)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(bnd.sym.loc)) {
      return
    }

    c.consumeBinder(bnd)
  }

  private def visitSigSymUse(symUse: SigSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val SigSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeSigSymUse(symUse)
  }

  private def visitLocalDefSymUse(symUse: LocalDefSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val LocalDefSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeLocalDefSym(symUse)
  }

  private def visitPredicateParam(pparam: PredicateParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val PredicateParam(_, _, loc) = pparam
    if (!a.accept(loc)) {
      return
    }

    c.consumePredicateParam(pparam)
  }

  private def visitStructFieldSymUse(symUse: StructFieldSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val StructFieldSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeStructFieldSymUse(symUse)
  }

  private def visitEffSymUse(effUse: EffSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val EffSymUse(_, qname) = effUse
    if (!a.accept(qname.loc)) {
      return
    }

    c.consumeEffSymUse(effUse)
  }

  private def visitJvmMethod(method: JvmMethod)(implicit a: Acceptor, c: Consumer): Unit = {
    val JvmMethod(_, fparams, exp, retTpe, _, loc) = method
    if (!a.accept(loc)) {
      return
    }

    c.consumeJvmMethod(method)

    fparams.foreach(visitFormalParam)
    visitExpr(exp)
    visitType(retTpe)
  }

  private def visitDefSymUse(symUse: DefSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val DefSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeDefSymUse(symUse)
  }

  private def visitSelectChannelRule(rule: SelectChannelRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val SelectChannelRule(bnd, chan, exp, loc) = rule
    if (!a.accept(loc)) {
      return
    }

    c.consumeSelectChannelRule(rule)

    visitBinder(bnd)
    visitExpr(chan)
    visitExpr(exp)
  }

  private def visitFormalParam(fparam: FormalParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val FormalParam(bnd, tpe, _, loc) = fparam
    if (!a.accept(loc)) {
      return
    }

    c.consumeFormalParam(fparam)

    visitBinder(bnd)
    visitType(tpe)
  }

  private def visitHandlerRule(rule: HandlerRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val HandlerRule(op, fparams, exp, loc) = rule
    if (!a.accept(loc)) {
      return
    }

    c.consumeHandlerRule(rule)

    visitOpSymUse(op)
    fparams.foreach(visitFormalParam)
    visitExpr(exp)
  }

  private def visitOpSymUse(symUse: OpSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val OpSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }

    c.consumeOpSymUse(symUse)
  }

  private def visitParYieldFrag(frag: ParYieldFragment)(implicit a: Acceptor, c: Consumer): Unit = {
    val ParYieldFragment(pat, exp, loc) = frag
    if (!a.accept(loc)) {
      return
    }

    c.consumeParYieldFragment(frag)

    visitPattern(pat)
    visitExpr(exp)
  }

  private def visitMatchRule(rule: MatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val MatchRule(pat, guard, exp, loc) = rule
    if (!a.accept(loc)) {
      return
    }

    c.consumeMatchRule(rule)

    visitPattern(pat)
    guard.foreach(visitExpr)
    visitExpr(exp)
  }

  private def visitExtMatchRule(rule: ExtMatchRule)(implicit a: Acceptor, c: Consumer): Unit = rule match {
    case ExtMatchRule(pat, exp, loc) =>
      if (!a.accept(loc)) {
        return
      }

      c.consumeExtMatchRule(rule)

      visitExtPattern(pat)
      visitExpr(exp)
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeMatchRule(bnd, tpe, exp, loc) = rule
    if (!a.accept(loc)) {
      return
    }

    c.consumeTypeMatchRule(rule)

    visitBinder(bnd)
    visitType(tpe)
    visitExpr(exp)
  }

  private def visitType(tpe: Type)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tpe.loc)) {
      return
    }

    c.consumeType(tpe)

    tpe match {
      case Type.Var(_, _) => ()
      case Type.Cst(_, _) => ()
      case Type.Apply(t1, t2, _) =>
        visitType(t1)
        visitType(t2)
      case Type.Alias(_, args, _, _) => args.foreach(visitType)
      case Type.AssocType(_, t, _, _) => visitType(t)
      case Type.JvmToType(t, _) => visitType(t)
      case Type.JvmToEff(t, _) => visitType(t)
      case Type.UnresolvedJvmType(_, _) => ()
    }
  }

  private def visitAnnotations(anns: Annotations)(implicit a: Acceptor, c: Consumer): Unit = {
    val Annotations(ls) = anns

    ls.foreach(visitAnnotation)
  }

  private def visitAnnotation(ann: Annotation)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(ann.loc)) {
      return
    }

    c.consumeAnnotation(ann)
  }

  private def visitCatchRule(rule: CatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val CatchRule(bnd, _, exp, loc) = rule
    if (!a.accept(loc)) {
      return
    }

    c.consumeCatchRule(rule)

    visitBinder(bnd)
    visitExpr(exp)
  }

  private def visitConstraint(cst: Constraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val Constraint(cparams, head, body, loc) = cst
    if (!a.accept(loc)) {
      return
    }

    c.consumeConstraint(cst)

    cparams.foreach(visitConstraintParam)
    visitPredicate(head)
    body.foreach(visitPredicate)
  }

  private def visitConstraintParam(cparam: ConstraintParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val ConstraintParam(bnd, _, loc) = cparam
    if (!a.accept(loc)) {
      return
    }

    c.consumeConstraintParam(cparam)

    visitBinder(bnd)
  }

  private def visitPredicate(p: Predicate)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(p.loc)) {
      return
    }

    c.consumePredicate(p)

    p match {
      case Predicate.Head.Atom(_, _, terms, _, _) => terms.foreach(visitExpr)
      case Predicate.Body.Atom(_, _, _, _, terms, _, _) => terms.foreach(visitPattern)
      case Predicate.Body.Functional(_, exp, _) => visitExpr(exp)
      case Predicate.Body.Guard(exp, _) => visitExpr(exp)
    }
  }

  private def visitPattern(pat: Pattern)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(pat.loc)) {
      return
    }

    c.consumePattern(pat)

    pat match {
      case Wild(_, _) => ()
      case Var(bnd, _, _) => visitBinder(bnd)
      case Cst(_, _, _) => ()
      case Tag(symUse, pats, _, _) =>
        visitCaseSymUse(symUse)
        pats.foreach(visitPattern)
      case Tuple(pats, _, _) =>
        pats.foreach(visitPattern)
      case Record(pats, pat1, _, _) =>
        pats.foreach(visitRecordLabelPattern)
        visitPattern(pat1)
      case Pattern.Error(_, _) =>
    }
  }

  private def visitExtPattern(pat: ExtPattern)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(pat.loc)) {
      return
    }

    c.consumeExtPattern(pat)

    pat match {
      case ExtPattern.Default(_) => ()
      case ExtPattern.Tag(_, pats, _) =>
        pats.foreach {
          case ExtTagPattern.Var(bnd, _, _) => visitBinder(bnd)
          case _ => ()
        }
      case ExtPattern.Error(_) => ()
    }
  }

  private def visitRecordLabelPattern(pat: RecordLabelPattern)(implicit a: Acceptor, c: Consumer): Unit = {
    val RecordLabelPattern(_, p, _, loc) = pat

    if (!a.accept(loc)) {
      return
    }

    c.consumeRecordLabelPattern(pat)

    visitPattern(p)
  }

  private def visitCaseSymUse(symUse: CaseSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val CaseSymUse(_, loc) = symUse
    if (!a.accept(loc)) {
      return
    }
    c.consumeCaseSymUse(symUse)
  }

  /**
    * Returns `true` if the position `pos` within the file given by path `uri` is contained within `loc`.
    * Returns `false` otherwise.
    *
    * @param uri the path of the file that `pos` is within.
    * @param pos the position that we want to know whether is within `loc`.
    * @param loc the `SourceLocation` that want to know if `pos` is within.
    * @return `true` if `pos` in file at path `uri` is within `loc`. `false` otherwise.
    */
  def inside(uri: String, pos: Position)(loc: SourceLocation): Boolean = {
    val sameSource = uri == loc.source.name
    if (!sameSource) {
      return false
    }

    val afterStart = loc.beginLine < pos.line ||
      (loc.beginLine == pos.line && loc.beginCol <= pos.character)
    if (!afterStart) {
      return false
    }


    val beforeEnd = pos.line < loc.endLine ||
      (pos.line == loc.endLine && pos.character < loc.endCol)
    if (!beforeEnd) {
      return false
    }

    true
  }
}
