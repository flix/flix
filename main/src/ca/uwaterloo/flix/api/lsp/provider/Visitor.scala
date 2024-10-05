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
  Pattern, 
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
  Annotations
}
import ca.uwaterloo.flix.language.ast.Ast.UseOrImport
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.Type
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

object Visitor {

  trait Consumer {
    def consumeAnns(anns: Annotations): Unit = ()
    def consumeAssocTypeDef(tdefn: AssocTypeDef): Unit = ()
    def consumeAssocTypeSymUse(symUse: AssocTypeSymUse): Unit = ()
    def consumeCase(cse: Case): Unit = ()
    def consumeCatchRule(rule: CatchRule): Unit = ()
    def consumeConstraint(c: Constraint): Unit = ()
    def consumeDef(defn: Def): Unit = ()
    def consumeDerive(derive: Derivation): Unit = ()
    def consumeDeriveList(deriveList: Derivations): Unit = ()
    def consumeEff(eff: Effect): Unit = ()
    def consumeEnum(enm: Enum): Unit = ()
    def consumeExpr(exp: Expr): Unit = ()
    def consumeFParam(fparam: FormalParam): Unit = ()
    def consumeFrag(frag: ParYieldFragment): Unit = ()
    def consumeHandlerRule(rule: HandlerRule): Unit = ()
    def consumeInstance(ins: Instance): Unit = ()
    def consumeMatchRule(rule: MatchRule): Unit = ()
    def consumePat(pat: Pattern): Unit = ()
    def consumeRoot(root: Root): Unit = ()
    def consumeSig(sig: Sig): Unit = ()
    def consumeSpec(spec: Spec): Unit = ()
    def consumeStruct(struct: Struct): Unit = ()
    def consumeTMatchRule(rule: TypeMatchRule): Unit = ()
    def consumeTrait(traitt: Trait): Unit = ()
    def consumeTraitConstraint(tc: TraitConstraint): Unit = ()
    def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = ()
    def consumeTraitSymUse(symUse: TraitSymUse): Unit= ()
    def consumeType(tpe: Type): Unit = ()
    def consumeTypeAlias(alias: TypeAlias): Unit = ()
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

    // TODO visit symbols?
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
    // TODO
    // visit(sig)
    // if (accept(sig.spec.loc)) {
    //   ??? // TODO
    // }

    // sig.exp.foreach(exp =>
    //   if (accept(exp.loc)) {
    //     visitExpr(exp, ???, accept)
    //   }
    // )
  }

  private def visitStruct(struct: Struct)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
    // visit(struct)
    // struct.fields.foreach{case (_, field) => {
    //   if (accept(field.loc) ) { 
    //     visitStructField(field, ???, accept) 
    //   }
    // }}
  }

  private def visitStructField(field: StructField)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
    // visit(field)
  }

  private def visitTrait(t: Trait)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
    // visit(t)
    // t.laws.foreach(law => visitDef(law, ???, accept))
    // t.sigs.foreach(sig => visitSig(sig, ???, accept))
  }
  
  private def visitEffect(eff: Effect)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!(a.accept(eff.loc))) { return }
    // TODO
    // visit(eff)
    // ???
  }

  private def visitDef(defn: Def)(implicit a: Acceptor, c: Consumer): Unit = {
    val insideDefn = a.accept(defn.spec.loc) || a.accept(defn.exp.loc) || a.accept(defn.sym.loc)
    if (!insideDefn) { return }

    c.consumeDef(defn)

    visitSpec(defn.spec)
    visitExpr(defn.exp)
  }

  private def visitSpec(spec: Spec)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitTypeAlias(alias: TypeAlias)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
    // visit(alias)
    // alias.tparams.map(tp => if (accept(tp.loc)) { visitTypeParam(tp, ???, accept) })
  }

  private def visitTypeParam(tparam: TypeParam)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
    // visit(tparam)
  }

  private def visitUse(use: UseOrImport)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
    // visit(use)
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

      case Expr.OpenAs(symUse, exp, tpe, loc) => {
        visitResEnumSymUse(symUse)
        visitExpr(exp) 
      }

      case Expr.Use(sym, alias, exp, loc) => {
        visitExpr(exp) 
      }

      case Expr.Lambda(fparam, exp, tpe, loc) => {
        visitFParam(fparam)
        visitExpr(exp) 
      }

      case Expr.Apply(exp, exps, tpe, eff, loc) => {
        visitExpr(exp) 
        exps.foreach(e => visitExpr(e))
      }

      case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e))

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
        rules.foreach(rule => visitMatchRule(rule))

      case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
        visitExpr(exp) 
        rules.foreach(rule => visitTypeMatchRule(rule))

      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
        // Does nothing because feature is experimental

      case Expr.Tag(sym, exp, tpe, eff, loc) =>
        visitExpr(exp) 

      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
        visitExpr(exp) 

      case Expr.Tuple(exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e))

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
        exps.foreach(e => visitExpr(e))

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
        fields.foreach{ case (_, e) => { 
          visitExpr(e)  
        }}

        visitExpr(region)

        visitExpr(region) 

      case Expr.StructGet(exp, sym, tpe, eff, loc) =>
        visitExpr(exp) 

      case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) =>
        visitExpr(exp1) 
        visitExpr(exp2) 

      case Expr.VectorLit(exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e))

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
        declaredType.foreach(t => visitType(t))
        declaredEff.foreach(t => visitType(t))

      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.Without(exp, effUse, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        visitExpr(exp)
        rules.foreach(rule => visitCatchRule(rule))

      case Expr.Throw(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        visitExpr(exp)
        rules.foreach(rule => visitHandlerRule(rule))

      case Expr.Do(op, exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e))

      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e))

      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
        visitExpr(exp)
        exps.foreach(e => visitExpr(e))

      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e))

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
        rules.foreach(rule => {
          visitExpr(rule.chan)
          visitExpr(rule.exp)
        })

        default.foreach(e => visitExpr(e))

      case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1) 
        visitExpr(exp2) 

      case Expr.ParYield(frags, exp, tpe, eff, loc) =>
        visitExpr(exp)
        frags.foreach(frag => visitParYieldFrag(frag))

      case Expr.Lazy(exp, tpe, loc) =>
        visitExpr(exp)

      case Expr.Force(exp, tpe, eff, loc) =>
        visitExpr(exp)

      case Expr.FixpointConstraintSet(cs, tpe, loc) =>
        cs.foreach(con => visitConstraint(con))

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

  private def visitResEnumSymUse(symUse: RestrictableEnumSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitFParam(fparam: FormalParam)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitHandlerRule(rule: HandlerRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitParYieldFrag(frag: ParYieldFragment)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitMatchRule(rule: MatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitType(tpe: Type)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitAnnotations(anns: Annotations)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitCatchRule(rule: CatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitConstraint(con: Constraint)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }

  private def visitPattern(pat: Pattern)(implicit a: Acceptor, c: Consumer): Unit = {
    // TODO
  }
}
