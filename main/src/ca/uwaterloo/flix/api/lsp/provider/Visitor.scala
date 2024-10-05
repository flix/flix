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

import ca.uwaterloo.flix.api.lsp.Entity
import ca.uwaterloo.flix.language.ast.TypedAst.{
  Root, 
  Def, 
  Expr, 
  Effect, 
  Enum,
  FormalParam,
  Constraint, 
  ConstraintParam,
  Pattern, 
  Predicate,
  SelectChannelRule,
  StructField,
  Case,
  Instance,
  Sig,
  Spec,
  Struct,
  Trait,
  TypeAlias,
  TypeParam,
  MatchRule,
  TypeMatchRule,
  CatchRule,
  HandlerRule,
  ParYieldFragment
}

import ca.uwaterloo.flix.language.ast.shared.{
  Annotations,
  Annotation
}
import ca.uwaterloo.flix.language.ast.Ast.UseOrImport
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.Ast.OpSymUse
import ca.uwaterloo.flix.language.ast.Ast.CaseSymUse
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Ast.EqualityConstraint
import ca.uwaterloo.flix.language.ast.Kind
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Scheme
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.Ast.Derivations
import ca.uwaterloo.flix.api.lsp.Position
import ca.uwaterloo.flix.language.ast.Ast.Derivation
import ca.uwaterloo.flix.language.ast.Ast.TraitConstraint
import ca.uwaterloo.flix.language.ast.Ast.TraitSymUse
import ca.uwaterloo.flix.language.ast.TypedAst.AssocTypeDef
import ca.uwaterloo.flix.language.ast.Ast.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.Ast.RestrictableEnumSymUse
import ca.uwaterloo.flix.language.ast.TypedAst.Op
import ca.uwaterloo.flix.language.ast.TypedAst.AssocTypeSig
import ca.uwaterloo.flix.language.ast.Ast.AssocTypeConstructor
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head.Atom
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.Atom
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.Functional
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.Guard
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Wild
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Var
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Cst
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Tag
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Tuple
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.RecordEmpty

object Visitor {

  trait Consumer {
    def consumeAnn(ann: Annotation): Unit = ()
    def consumeAnns(anns: Annotations): Unit = ()
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
    def consumeDerive(derive: Derivation): Unit = ()
    def consumeDeriveList(deriveList: Derivations): Unit = ()
    def consumeEff(eff: Effect): Unit = ()
    def consumeEnum(enm: Enum): Unit = ()
    def consumeEqConstraint(ec: EqualityConstraint): Unit = ()
    def consumeExpr(exp: Expr): Unit = ()
    def consumeFParam(fparam: FormalParam): Unit = ()
    def consumeFrag(frag: ParYieldFragment): Unit = ()
    def consumeHandlerRule(rule: HandlerRule): Unit = ()
    def consumeInstance(ins: Instance): Unit = ()
    def consumeMatchRule(rule: MatchRule): Unit = ()
    def consumeOp(op: Op): Unit = ()
    def consumeOpSymUse(sym: OpSymUse): Unit = ()
    def consumePat(pat: Pattern): Unit = ()
    def consumePredicate(p: Predicate): Unit = ()
    def consumeRecLabelPat(pat: Pattern.Record.RecordLabelPattern): Unit = ()
    def consumeRoot(root: Root): Unit = ()
    def consumeSelectChannelRule(rule: SelectChannelRule): Unit = ()
    def consumeSig(sig: Sig): Unit = ()
    def consumeSpec(spec: Spec): Unit = ()
    def consumeStruct(struct: Struct): Unit = ()
    def consumeStructField(field: StructField): Unit = ()
    def consumeTMatchRule(rule: TypeMatchRule): Unit = ()
    def consumeTrait(traitt: Trait): Unit = ()
    def consumeTraitConstraint(tc: TraitConstraint): Unit = ()
    def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = ()
    def consumeTraitSymUse(symUse: TraitSymUse): Unit= ()
    def consumeType(tpe: Type): Unit = ()
    def consumeTypeAlias(alias: TypeAlias): Unit = ()
    def consumeTypeParam(tparam: TypeParam): Unit = ()
    def consumeUseOrImport(use: UseOrImport): Unit = ()
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

    implicit val c = consumer
    implicit val a = acceptor

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

    // sp1 and sp2, by invariant, has the same source, so we can use either
    (uri == loc.sp1.source.name) &&
    (posLine >= loc.beginLine) &&
    (posLine <= loc.endLine) &&
    (!(posLine == loc.beginLine && posCol < loc.beginCol)) &&
    (!(posLine == loc.endLine && posCol >= loc.endCol)) // geq because end column is exclusive
  }

  def inside(loc1: SourceLocation, loc2: SourceLocation): Boolean = {
    loc1.source == loc2.source &&
    (loc1.beginLine >= loc2.beginLine) &&
    (loc1.endLine <= loc2.endLine) &&
    !(loc2.beginLine == loc1.beginLine && loc2.beginCol > loc1.beginCol) &&
    !(loc1.endLine == loc2.endLine && loc1.endCol > loc2.endCol)
  }

  private def visitEnum(enm: Enum)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(enm.loc)) { return }

    c.consumeEnum(enm)

    visitAnnotations(enm.ann)
    // TODO visit modifiers?
    enm.tparams.foreach(visitTypeParam)
    visitDeriveList(enm.derives)
    enm.cases.map(_._2).foreach(visitCase)
  }

  private def visitDeriveList(deriveList: Derivations)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(deriveList.loc)) { return }
    c.consumeDeriveList(deriveList)
    deriveList.traits.foreach(visitDerive)
  }

  private def visitDerive(derive: Derivation)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(derive.loc)) { return }
    c.consumeDerive(derive)
  }

  private def visitCase(cse: Case)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(cse.loc)) { return }
    c.consumeCase(cse)
    // TODO visit scheme?
  }

  private def visitInstance(ins: Instance)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(ins.loc)) { return }

    visitAnnotations(ins.ann)
    visitTraitSymUse(ins.trt)
    ins.tconstrs.foreach(visitTraitConstraint)
    ins.assocs.foreach(visitAssocTypeDef)
    ins.defs.foreach(visitDef)
  }

  private def visitTraitSymUse(symUse: TraitSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(symUse.loc)) { return }
    c.consumeTraitSymUse(symUse)
  }

  private def visitTraitConstraint(tc: TraitConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tc.loc)) { return }

    c.consumeTraitConstraint(tc)
    visitTraitConstraintHead(tc.head)
    visitType(tc.arg)
  }

  private def visitTraitConstraintHead(tcHead: TraitConstraint.Head)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tcHead.loc)) { return }
    c.consumeTraitConstraintHead(tcHead)
  }

  private def visitAssocTypeDef(tdefn: AssocTypeDef)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tdefn.loc)) { return }
    c.consumeAssocTypeDef(tdefn)
    visitAssocTypeSymUse(tdefn.sym)
    visitType(tdefn.arg)
  }

  private def visitAssocTypeSymUse(symUse: AssocTypeSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(symUse.loc)) { return }
    c.consumeAssocTypeSymUse(symUse)
  }

  private def visitSig(sig: Sig)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO fix hack. This hack is necessary because a signature currently does not have a location.
    val insideSig = a.accept(sig.spec.loc) || sig.exp.exists(e => a.accept(e.loc))
    if (!insideSig) { return }

    c.consumeSig(sig)
    sig.exp.foreach(visitExpr)
  }

  private def visitStruct(struct: Struct)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(struct.loc)) { return }
    c.consumeStruct(struct)

    visitAnnotations(struct.ann)
    struct.tparams.foreach(visitTypeParam)
    // TODO visit scheme?
    struct.fields.map(_._2).foreach(visitStructField)
  }

  private def visitStructField(field: StructField)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(field.loc)) { return }
    c.consumeStructField(field)
  }

  private def visitTrait(t: Trait)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(t.loc)) { return }

    c.consumeTrait(t)

    visitAnnotations(t.ann)
    visitTypeParam(t.tparam)
    t.superTraits.foreach(visitTraitConstraint)
    t.assocs.foreach(visitAssocTypeSig)
    t.sigs.foreach(visitSig)
    t.laws.foreach(visitDef)
  }

  private def visitAssocTypeSig(assoc: AssocTypeSig)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(assoc.loc)) { return }

    c.consumeAssocTypeSig(assoc)

    visitTypeParam(assoc.tparam)
    visitKind(assoc.kind)
    assoc.tpe.foreach(visitType)
    
  }

  private def visitKind(kind: Kind)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO it doesn't have a location, so does this even make sense?
  }
  
  private def visitEffect(eff: Effect)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!(a.accept(eff.loc))) { return }
    c.consumeEff(eff)

    visitAnnotations(eff.ann)
    eff.ops.foreach(visitOp)
  }

  private def visitOp(op: Op)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO: hack that should eventually be fixed. Really it should be `op.loc`, but since op decls don't have locations, this hack is necessary for now
    if (!a.accept(op.spec.loc)) { return }
    c.consumeOp(op)
    visitSpec(op.spec)
  }

  private def visitDef(defn: Def)(implicit a: Acceptor, c: Consumer): Unit = {
    val insideDefn = a.accept(defn.spec.loc) || a.accept(defn.exp.loc) || a.accept(defn.sym.loc)
    if (!insideDefn) { return }

    c.consumeDef(defn)

    visitSpec(defn.spec)
    visitExpr(defn.exp)
  }

  private def visitSpec(spec: Spec)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(spec.loc)) { return }

    c.consumeSpec(spec)

    visitAnnotations(spec.ann)
    spec.tparams.foreach(visitTypeParam)
    spec.fparams.foreach(visitFormalParam)
    // TODO visit scheme
    visitType(spec.retTpe)
    visitType(spec.eff)
    spec.tconstrs.foreach(visitTraitConstraint)
    spec.econstrs.foreach(visitEqualityConstraint)
  }

  private def visitEqualityConstraint(ec: EqualityConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(ec.loc)) { return }

    c.consumeEqConstraint(ec)

    visitAssocTypeConstructor(ec.cst)
    visitType(ec.tpe1)
    visitType(ec.tpe2)
  }

  private def visitAssocTypeConstructor(tcst: AssocTypeConstructor)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tcst.loc)) { return }
    c.consumeAssocTypeConstructor(tcst)
  }

  private def visitTypeAlias(alias: TypeAlias)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(alias.loc)) { return }

    c.consumeTypeAlias(alias)

    visitAnnotations(alias.ann)
    alias.tparams.foreach(visitTypeParam)
    visitType(alias.tpe)
  }

  private def visitTypeParam(tparam: TypeParam)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tparam.loc)) { return }
    c.consumeTypeParam(tparam)
  }

  private def visitUse(use: UseOrImport)(implicit a: Acceptor, c: Consumer): Unit = {
    val loc = use match {
      case UseOrImport.Use(_, _, loc) => loc
      case UseOrImport.Import(_, _, loc) => loc
    }

    if (!a.accept(loc)) { return }

    c.consumeUseOrImport(use)
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

  private def visitSelectChannelRule(rule: SelectChannelRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO `insideRule` is hack, should be removed eventually. Necessary for now since SelectChannelRule don't have locations
    val insideRule = a.accept(rule.chan.loc) || a.accept(rule.exp.loc)
    if (!insideRule) { return }

    c.consumeSelectChannelRule(rule)

    visitExpr(rule.chan)
    visitExpr(rule.exp)
  }

  private def visitFormalParam(fparam: FormalParam)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(fparam.loc)) { return }
    c.consumeFParam(fparam)
    visitType(fparam.tpe)
  }

  private def visitHandlerRule(rule: HandlerRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO `insideRule` is hack, should be removed eventually. Necessary for now since HandlerRules don't have locations
    val insideRule = a.accept(rule.op.loc) || rule.fparams.map(_.loc).exists(a.accept) || a.accept(rule.exp.loc)
    if (!insideRule) { return }

    c.consumeHandlerRule(rule)

    visitOpSymUse(rule.op)
    rule.fparams.foreach(visitFormalParam)
    visitExpr(rule.exp)
  }

  private def visitOpSymUse(sym: OpSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(sym.loc)) { return }
    c.consumeOpSymUse(sym)
  }

  private def visitParYieldFrag(frag: ParYieldFragment)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(frag.loc)) { return }

    c.consumeFrag(frag)

    visitPattern(frag.pat)
    visitExpr(frag.exp)
  }

  private def visitMatchRule(rule: MatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO `insideRule` is hack, should be removed eventually. Necessary for now since MatchRules don't have locations
    val insideRule = a.accept(rule.pat.loc) || rule.guard.map(_.loc).exists(a.accept) || a.accept(rule.exp.loc)
    if (!insideRule) { return }

    c.consumeMatchRule(rule)

    visitPattern(rule.pat)
    rule.guard.foreach(visitExpr)
    visitExpr(rule.exp)
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO `insideRule` is hack, should be removed eventually. Necessary for now since TypeMatchRules don't have locations
    val insideRule = a.accept(rule.sym.loc) || a.accept(rule.tpe.loc) || a.accept(rule.exp.loc) 
    if (!insideRule) { return }

    c.consumeTMatchRule(rule)

    visitType(rule.tpe)
    visitExpr(rule.exp)
  }

  private def visitType(tpe: Type)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tpe.loc)) { return }
    c.consumeType(tpe)
  }

  private def visitAnnotations(anns: Annotations)(implicit a: Acceptor, c: Consumer): Unit = {
    val ls = anns match {
      case Annotations(ls) => ls
    }

    // TODO `insideAnns` is hack, should be removed eventually. Necessary for now since Annotations (not to be confused with Annotation) don't have locations
    val insideAnns = ls.map(_.loc).exists(a.accept)
    if (!insideAnns) { return }

    c.consumeAnns(anns)

    ls.foreach(visitAnnotation)
  }

  private def visitAnnotation(ann: Annotation)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(ann.loc)) { return }
    c.consumeAnn(ann)
  }

  private def visitCatchRule(rule: CatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val insideRule = a.accept(rule.sym.loc) || a.accept(rule.exp.loc)
    if (!insideRule) { return }

    c.consumeCatchRule(rule)

    visitExpr(rule.exp)
  }

  private def visitConstraint(cst: Constraint)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(cst.loc)) { return }

    c.consumeConstraint(cst)

    cst.cparams.foreach(visitConstraintParam)
    visitPredicate(cst.head)
    cst.body.foreach(visitPredicate)
  }

  private def visitConstraintParam(cparam: ConstraintParam)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(cparam.loc)) { return }
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

    c.consumePat(pat)

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

  private def visitRecordLabelPattern(pat: Pattern.Record.RecordLabelPattern)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(pat.loc)) { return }
    c.consumeRecLabelPat(pat)
    visitPattern(pat.pat)
  }

  private def visitCaseSymUse(sym: CaseSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(sym.loc)) { return }
    c.consumeCaseSymUse(sym)
  }
}
