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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.Position
import ca.uwaterloo.flix.language.ast.Ast.*
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.*
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.TypedAst.{AssocTypeDef, Instance, *}
import ca.uwaterloo.flix.language.ast.shared.{Annotation, Annotations}
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Type}

object Visitor {

  trait Consumer {
    def consumeAnnotation(ann: Annotation): Unit = ()
    def consumeAnnotations(anns: Annotations): Unit = ()
    def consumeAssocTypeConstructor(tcst: AssocTypeConstructor): Unit = ()
    def consumeAssocTypeDef(tdefn: AssocTypeDef): Unit = ()
    def consumeAssocTypeSig(tsig: AssocTypeSig): Unit = ()
    def consumeAssocTypeSymUse(symUse: AssocTypeSymUse): Unit = ()
    def consumeCase(cse: Case): Unit = ()
    def consumeCaseSymUse(sym: CaseSymUse): Unit = ()
    def consumeCatchRule(rule: CatchRule): Unit = ()
    def consumeConstraint(c: Constraint): Unit = ()
    def consumeConstraintParam(cparam: ConstraintParam): Unit = ()
    def consumeDef(defn: Def): Unit = ()
    def consumeDefSymUse(sym: DefSymUse): Unit = ()
    def consumeDerivation(derive: Derivation): Unit = ()
    def consumeDerivations(derives: Derivations): Unit = ()
    def consumeEff(eff: Effect): Unit = ()
    def consumeEnum(enm: Enum): Unit = ()
    def consumeEqConstraint(ec: EqualityConstraint): Unit = ()
    def consumeExpr(exp: Expr): Unit = ()
    def consumeFormalParam(fparam: FormalParam): Unit = ()
    def consumeParYieldFragment(frag: ParYieldFragment): Unit = ()
    def consumeHandlerRule(rule: HandlerRule): Unit = ()
    def consumeInstance(ins: Instance): Unit = ()
    def consumeMatchRule(rule: MatchRule): Unit = ()
    def consumeOp(op: Op): Unit = ()
    def consumeOpSymUse(sym: OpSymUse): Unit = ()
    def consumePattern(pat: Pattern): Unit = ()
    def consumePredicate(p: Predicate): Unit = ()
    def consumeRecordLabelPattern(pat: RecordLabelPattern): Unit = ()
    def consumeSelectChannelRule(rule: SelectChannelRule): Unit = ()
    def consumeSig(sig: Sig): Unit = ()
    def consumeSpec(spec: Spec): Unit = ()
    def consumeStruct(struct: Struct): Unit = ()
    def consumeStructField(field: StructField): Unit = ()
    def consumeTypeMatchRule(rule: TypeMatchRule): Unit = ()
    def consumeTrait(traitt: Trait): Unit = ()
    def consumeTraitConstraint(tc: TraitConstraint): Unit = ()
    def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = ()
    def consumeTraitSymUse(symUse: TraitSymUse): Unit= ()
    def consumeType(tpe: Type): Unit = ()
    def consumeTypeAlias(alias: TypeAlias): Unit = ()
    def consumeTypeParam(tparam: TypeParam): Unit = ()
  }

  trait Acceptor {
    def accept(loc: SourceLocation): Boolean;
  }

  case object allAcceptor extends Acceptor {
    def accept(loc: SourceLocation): Boolean = true
  }

  case class fileAcceptor(uri: String) extends Acceptor {
    def accept(loc: SourceLocation): Boolean = uri == loc.source.name
  }

  case class insideAcceptor(uri: String, pos: Position) extends Acceptor {
    def accept(loc: SourceLocation): Boolean = inside(uri, pos)(loc)
  }

  /**
    * Visits the root AST node and recursively visits
    * all children that are accepted by the acceptor,
    * "consuming" each node visited (including the root).
    *
    * By "consuming" what is meant is that the node is used
    * as the input to a function of the consumer associated with
    * the AST node type. Such consumer funcitons have no output,
    * but can have effects. For instance, the consumer can be defined such that each
    * expression containing a specific variable is collected in a list, via
    * mutation a variable.
    *
    * Note that if a node is not accepted, none of the nodes in it's
    * subtree will be visited either. For instance, if a `def` is not
    * accepted, none of the expressions in the body will be visited eiter,
    * even if they would otherwise be accepted by the acceptor.
    *
    * @param root      The AST root node.
    * @param consumer  A consumer which defines what to do when visiting different types of AST nodes.
    * @param acceptor  An acceptor which defines the the criteria for whether an AST node should be visited.
    */
  def visitRoot(root: Root, consumer: Consumer, acceptor: Acceptor): Unit = {

    implicit val c: Consumer = consumer
    implicit val a: Acceptor = acceptor

    root.defs.foreach{ case (_, defn) => visitDef(defn) }

    root.effects.foreach{ case (_, eff) => visitEffect(eff) }

    // root.entryPoint.map{ case v => visitEntryPoint(visit, accept)(v) }

    root.enums.foreach{ case (_, e) => visitEnum(e) }

    root.instances.foreach{ case (_, l) => l.foreach(visitInstance) }

    // root.modules.map { case (_, v) => visitModule(v, ???, ???) }

    root.sigs.foreach{ case (_, sig) => visitSig(sig) }

    root.structs.foreach{ case (_, struct) => visitStruct(struct) }

    // root.traitEnv.map{ case (_, v) => visitTraitEnv(???, ???)(v) };

    root.traits.foreach{ case (_, traitt) => visitTrait(traitt) }

    root.typeAliases.foreach{ case (_, alias) => visitTypeAlias(alias) }

    // root.uses
  }

  def inside(uri: String, pos: Position)(loc: SourceLocation): Boolean = {
    val (x, y) = pos.toZeroIndexed

    val posLine = x + 1
    val posCol = y + 1

    (uri == loc.source.name) &&
    (posLine >= loc.beginLine) &&
    (posLine <= loc.endLine) &&
    (!(posLine == loc.beginLine && posCol < loc.beginCol)) &&
    (!(posLine == loc.endLine && posCol >= loc.endCol)) // geq because end column is exclusive
  }

  private def visitEnum(enm: Enum)(implicit a: Acceptor, c: Consumer): Unit = {
    val Enum(_, ann, _, _, tparams, derives, cases, loc) = enm
    if (!a.accept(loc)) { return }

    c.consumeEnum(enm)

    visitAnnotations(ann)
    // TODO visit modifiers?
    tparams.foreach(visitTypeParam)
    visitDeriveList(derives)
    cases.values.foreach(visitCase)
  }

  private def visitDeriveList(derivations: Derivations)(implicit a: Acceptor, c: Consumer): Unit = {
    val Derivations(traits, loc) = derivations
    if (!a.accept(loc)) { return }

    c.consumeDerivations(derivations)

    traits.foreach(visitDerive)
  }

  private def visitDerive(derive: Derivation)(implicit a: Acceptor, c: Consumer): Unit = {
    val Derivation(_, loc) = derive
    if (!a.accept(loc)) { return }

    c.consumeDerivation(derive)
  }

  private def visitCase(cse: Case)(implicit a: Acceptor, c: Consumer): Unit = {
    val Case(_, _, _, loc) = cse
    if (!a.accept(loc)) { return }

    c.consumeCase(cse)
  }

  private def visitInstance(ins: Instance)(implicit a: Acceptor, c: Consumer): Unit = {
    val Instance(_, ann, _, trt, _, tconstrs, assocs, defs, _, loc) = ins
    if (!a.accept(loc)) { return }

    c.consumeInstance(ins)

    visitAnnotations(ann)
    visitTraitSymUse(trt)
    tconstrs.foreach(visitTraitConstraint)
    assocs.foreach(visitAssocTypeDef)
    defs.foreach(visitDef)
  }

  private def visitTraitSymUse(symUse: TraitSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeTraitSymUse(symUse)
  }

  private def visitTraitConstraint(tc: TraitConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitConstraint(head, arg, loc) = tc
    if (!a.accept(loc)) { return }

    c.consumeTraitConstraint(tc)

    visitTraitConstraintHead(head)
    visitType(arg)
  }

  private def visitTraitConstraintHead(tcHead: TraitConstraint.Head)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitConstraint.Head(_, loc) = tcHead
    if (!a.accept(loc)) { return }

    c.consumeTraitConstraintHead(tcHead)
  }

  private def visitAssocTypeDef(tdefn: AssocTypeDef)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeDef(_, _, symUse, arg, _, loc) = tdefn
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeDef(tdefn)

    visitAssocTypeSymUse(symUse)
    visitType(arg)
  }

  private def visitAssocTypeSymUse(symUse: AssocTypeSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeSymUse(symUse)
  }

  private def visitSig(sig: Sig)(implicit a: Acceptor, c: Consumer): Unit = {
    val Sig(_, spec, exp) = sig
    // TODO `insideSig` is a hack and should eventually be removed. This hack is necessary because a signature currently does not have a location.
    val insideSig = a.accept(sig.spec.loc) || sig.exp.exists(e => a.accept(e.loc))
    if (!insideSig) { return }

    c.consumeSig(sig)

    visitSpec(spec)
    exp.foreach(visitExpr)
  }

  private def visitStruct(struct: Struct)(implicit a: Acceptor, c: Consumer): Unit = {
    val Struct(doc, ann, mod, sym, tparams, sc, fields, loc) = struct
    if (!a.accept(loc)) { return }

    c.consumeStruct(struct)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    fields.values.foreach(visitStructField)
  }

  private def visitStructField(field: StructField)(implicit a: Acceptor, c: Consumer): Unit = {
    val StructField(sym, tpe, loc) = field
    if (!a.accept(loc)) { return }

    c.consumeStructField(field)

    visitType(tpe)
  }

  private def visitTrait(t: Trait)(implicit a: Acceptor, c: Consumer): Unit = {
    val Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc) = t
    if (!a.accept(loc)) { return }

    c.consumeTrait(t)

    visitAnnotations(ann)
    visitTypeParam(tparam)
    superTraits.foreach(visitTraitConstraint)
    assocs.foreach(visitAssocTypeSig)
    sigs.foreach(visitSig)
    laws.foreach(visitDef)
  }

  private def visitAssocTypeSig(assoc: AssocTypeSig)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc) = assoc
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeSig(assoc)

    visitTypeParam(tparam)
    visitKind(kind)
    tpe.foreach(visitType)

  }

  private def visitKind(kind: Kind)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO it doesn't have a location, so does this even make sense?
  }

  private def visitEffect(eff: Effect)(implicit a: Acceptor, c: Consumer): Unit = {
    val Effect(doc, ann, mod, sym, ops, loc) = eff
    if (!a.accept(loc)) { return }

    c.consumeEff(eff)

    visitAnnotations(ann)
    ops.foreach(visitOp)
  }

  private def visitOp(op: Op)(implicit a: Acceptor, c: Consumer): Unit = {
    val Op(sym, spec) = op
    // TODO: hack that should eventually be fixed. Really it should be `op.loc`, but since op decls don't have locations, this hack is necessary for now
    if (!a.accept(spec.loc)) { return }

    c.consumeOp(op)

    visitSpec(spec)
  }

  private def visitDef(defn: Def)(implicit a: Acceptor, c: Consumer): Unit = {
    val Def(sym, spec, exp) = defn
    // TODO `insideDef` is a hack and should be removed eventually. Necessary for now since Defs  don't have locations
    val insideDef = a.accept(spec.loc) || a.accept(exp.loc) || a.accept(sym.loc)
    if (!insideDef) { return }

    c.consumeDef(defn)

    visitSpec(defn.spec)
    visitExpr(defn.exp)
  }

  private def visitSpec(spec: Spec)(implicit a: Acceptor, c: Consumer): Unit = {
    val Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, econstrs, loc) = spec
    if (!a.accept(loc)) { return }

    c.consumeSpec(spec)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    fparams.foreach(visitFormalParam)
    visitType(retTpe)
    visitType(eff)
    tconstrs.foreach(visitTraitConstraint)
    econstrs.foreach(visitEqualityConstraint)
  }

  private def visitEqualityConstraint(ec: EqualityConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val EqualityConstraint(cst, tpe1, tpe2, loc) = ec
    if (!a.accept(loc)) { return }

    c.consumeEqConstraint(ec)

    visitAssocTypeConstructor(cst)
    visitType(tpe1)
    visitType(tpe2)
  }

  private def visitAssocTypeConstructor(tcst: AssocTypeConstructor)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeConstructor(sym, loc) = tcst
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeConstructor(tcst)
  }

  private def visitTypeAlias(alias: TypeAlias)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeAlias(doc, ann, mod, sym, tparams, tpe, loc) = alias
    if (!a.accept(loc)) { return }

    c.consumeTypeAlias(alias)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    visitType(tpe)
  }

  private def visitTypeParam(tparam: TypeParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeParam(name, sym, loc) = tparam
    if (!a.accept(loc)) { return }

    c.consumeTypeParam(tparam)
  }

  private def visitExpr(expr: Expr)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO: handle mutually recursive calls to other visit functions

    if (!(a.accept(expr.loc))) { return }

    c.consumeExpr(expr)

    expr match {
      case Expr.Cst(cst, tpe, loc) => ()
      case Expr.Var(sym, tpe, loc) => ()
      case Expr.Sig(sym, tpe, loc) => ()
      case Expr.Hole(sym, tpe, eff, loc) => ()

      case Expr.HoleWithExp(exp, tpe, eff, loc) => {
        visitExpr(exp)
      }

      // we do nothing here, because open as is a restrictable enum feature and thus experimental
      case Expr.OpenAs(symUse, exp, tpe, loc) => ()

      case Expr.Use(sym, alias, exp, loc) => {
        visitExpr(exp)
      }

      case Expr.Lambda(fparam, exp, tpe, loc) => {
        visitFormalParam(fparam)
        visitExpr(exp)
      }

      case Expr.Apply(exp, exps, tpe, eff, loc) => {
        visitExpr(exp)
        exps.foreach(visitExpr)
      }

      case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        visitDefSymUse(sym)
        exps.foreach(visitExpr)

      case Expr.Unary(sop, exp, tpe, eff, loc) => {
        visitExpr(exp)
      }

      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
        visitAnnotations(ann)
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.Region(tpe, loc) => ()

      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Expr.Stm(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.Discard(exp, eff, loc) =>
        visitExpr(exp)

      case Expr.Match(exp, rules, tpe, eff, loc) =>
        visitExpr(exp)
        rules.foreach(visitMatchRule)

      case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
        visitExpr(exp)
        rules.foreach(visitTypeMatchRule)

      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
        // Does nothing because feature is experimental

      case Expr.Tag(sym, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.Tuple(exps, tpe, eff, loc) =>
        exps.foreach(visitExpr)

      case Expr.RecordEmpty(tpe, loc) => ()

      case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
        visitExpr(exp)
        exps.foreach(visitExpr)

      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.ArrayLength(exp, eff, loc) =>
        visitExpr(exp)

      case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
        fields.map(_._2).foreach(visitExpr)
        visitExpr(region)

      case Expr.StructGet(exp, sym, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.VectorLit(exps, tpe, eff, loc) =>
        exps.foreach(visitExpr)

      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.VectorLength(exp, loc) =>
        visitExpr(exp)

      case Expr.Ascribe(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.InstanceOf(exp, clazz, loc) =>
        visitExpr(exp)

      case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
        visitExpr(exp)
        // TODO maybe we need to visit `cast` (`CheckTypeCast`)?

      case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        visitExpr(exp)
        declaredType.foreach(visitType)
        declaredEff.foreach(visitType)

      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.Without(exp, effUse, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        visitExpr(exp)
        rules.foreach(visitCatchRule)

      case Expr.Throw(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        visitExpr(exp)
        rules.foreach(visitHandlerRule)

      case Expr.Do(op, exps, tpe, eff, loc) =>
        exps.foreach(visitExpr)

      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
        exps.foreach(visitExpr)

      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
        visitExpr(exp)
        exps.foreach(visitExpr)

      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
        exps.foreach(visitExpr)

      case Expr.GetField(field, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.GetStaticField(field, tpe, eff, loc) => ()

      case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ()

      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.GetChannel(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
        rules.foreach(visitSelectChannelRule)
        default.foreach(visitExpr)

      case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.ParYield(frags, exp, tpe, eff, loc) =>
        visitExpr(exp)
        frags.foreach(visitParYieldFrag)

      case Expr.Lazy(exp, tpe, loc) =>
        visitExpr(exp)

      case Expr.Force(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.FixpointConstraintSet(cs, tpe, loc) =>
        cs.foreach(visitConstraint)

      case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.FixpointSolve(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.Error(m, tpe, eff) => ()
    }
  }

  private def visitDefSymUse(symUse: DefSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val DefSymUse(sym, loc) = symUse
    if(!a.accept(loc)) { return }

    c.consumeDefSymUse(symUse)
  }

  private def visitSelectChannelRule(rule: SelectChannelRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val SelectChannelRule(sym, chan, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since SelectChannelRule don't have locations
    val insideRule = a.accept(chan.loc) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeSelectChannelRule(rule)

    visitExpr(chan)
    visitExpr(exp)
  }

  private def visitFormalParam(fparam: FormalParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val FormalParam(sym, mod, tpe, src, loc) = fparam
    if (!a.accept(loc)) { return }

    c.consumeFormalParam(fparam)

    visitType(tpe)
  }

  private def visitHandlerRule(rule: HandlerRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val HandlerRule(op, fparams, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since HandlerRules don't have locations
    val insideRule = a.accept(op.loc) || fparams.map(_.loc).exists(a.accept) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeHandlerRule(rule)

    visitOpSymUse(op)
    fparams.foreach(visitFormalParam)
    visitExpr(exp)
  }

  private def visitOpSymUse(symUse: OpSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val OpSymUse(sym, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeOpSymUse(symUse)
  }

  private def visitParYieldFrag(frag: ParYieldFragment)(implicit a: Acceptor, c: Consumer): Unit = {
    val ParYieldFragment(pat, exp, loc) = frag
    if (!a.accept(loc)) { return }

    c.consumeParYieldFragment(frag)

    visitPattern(pat)
    visitExpr(exp)
  }

  private def visitMatchRule(rule: MatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val MatchRule(pat, guard, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since MatchRules don't have locations
    val insideRule = a.accept(pat.loc) || guard.map(_.loc).exists(a.accept) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeMatchRule(rule)

    visitPattern(pat)
    guard.foreach(visitExpr)
    visitExpr(exp)
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeMatchRule(sym, tpe, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since TypeMatchRules don't have locations
    val insideRule = a.accept(sym.loc) || a.accept(tpe.loc) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeTypeMatchRule(rule)

    visitType(tpe)
    visitExpr(exp)
  }

  private def visitType(tpe: Type)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tpe.loc)) { return }
    c.consumeType(tpe)
  }

  private def visitAnnotations(anns: Annotations)(implicit a: Acceptor, c: Consumer): Unit = {
    val Annotations(ls) = anns
    // TODO `insideAnns` is a hack, should be removed eventually. Necessary for now since Annotations (not to be confused with Annotation) don't have locations
    val insideAnns = ls.map(_.loc).exists(a.accept)
    if (!insideAnns) { return }

    c.consumeAnnotations(anns)

    ls.foreach(visitAnnotation)
  }

  private def visitAnnotation(ann: Annotation)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(ann.loc)) { return }
    c.consumeAnnotation(ann)
  }

  private def visitCatchRule(rule: CatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val CatchRule(sym, clazz, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since CatchRules don't have locations
    val insideRule = a.accept(sym.loc) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeCatchRule(rule)

    visitExpr(exp)
  }

  private def visitConstraint(cst: Constraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val Constraint(cparams, head, body, loc) = cst
    if (!a.accept(loc)) { return }

    c.consumeConstraint(cst)

    cparams.foreach(visitConstraintParam)
    visitPredicate(head)
    body.foreach(visitPredicate)
  }

  private def visitConstraintParam(cparam: ConstraintParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val ConstraintParam(sym, tpe, loc) = cparam
    if (!a.accept(loc)) { return }

    c.consumeConstraintParam(cparam)
  }

  private def visitPredicate(p: Predicate)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(p.loc)) { return }

    c.consumePredicate(p)

    p match {
    	case Predicate.Head.Atom(pred, den, terms, tpe, loc) => terms.foreach(visitExpr)
    	case Predicate.Body.Atom(pred, den, polarity, fixity, terms, tpe, loc) => terms.foreach(visitPattern)
    	case Predicate.Body.Functional(outVars, exp, loc) => visitExpr(exp)
    	case Predicate.Body.Guard(exp, loc) => visitExpr(exp)
    }
  }

  private def visitPattern(pat: Pattern)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(pat.loc)) { return }

    c.consumePattern(pat)

    pat match {
    	case Wild(tpe, loc) => ()
    	case Var(sym, tpe, loc) => ()
    	case Cst(cst, tpe, loc) => ()
    	case Tag(sym, pat, tpe, loc) =>
    	  visitCaseSymUse(sym)
        visitPattern(pat)
    	case Tuple(pats, tpe, loc) =>
    	  pats.foreach(visitPattern)
    	case Record(pats, pat, tpe, loc) =>
    	  pats.foreach(visitRecordLabelPattern)
    	  visitPattern(pat)
    	case RecordEmpty(tpe, loc) =>
    	case Pattern.Error(tpe, loc) =>
    }
  }

  private def visitRecordLabelPattern(pat: RecordLabelPattern)(implicit a: Acceptor, c: Consumer): Unit = {
    val RecordLabelPattern(label, tpe, p, loc) = pat
    if (!a.accept(loc)) { return }
    c.consumeRecordLabelPattern(pat)
    visitPattern(p)
  }

  private def visitCaseSymUse(symUse: CaseSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val CaseSymUse(sym, loc) = symUse
    if (!a.accept(loc)) { return }
    c.consumeCaseSymUse(symUse)
  }
}
