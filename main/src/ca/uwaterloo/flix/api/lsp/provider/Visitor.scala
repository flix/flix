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
  RestrictableEnum,
  RestrictableCase,
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
import ca.uwaterloo.flix.api.lsp.Position

/**
  * A collection of visit functions that can apply a set of functions
  * to an TypedAst node and all of it's children recursively.
  */
object Visitor {
  /**
    * Applies a `seenRoot` to the root AST node and recursively visits
    * all children, applying the corresponding `seenX` functions.
    *
    * @param root    The AST root node.
    * @param seenX   The `seen` function that is to be applied to X type AST nodes.
    * @param accept  A predicate determining whether to visit a child node.
    */
  def visitRoot(root: Root, 
                seenAnnos: Annotations => Unit,
                seenCatchRule: CatchRule => Unit,
                seenConstraint: Constraint => Unit,
                seenDef: Def => Unit,
                seenEff: Effect => Unit,
                seenEnum: Enum => Unit,
                seenExpr: Expr => Unit,
                seenFParam: FormalParam => Unit,
                seenFrag: ParYieldFragment => Unit,
                seenHandlerRule: HandlerRule => Unit,
                seenInstance: Instance => Unit,
                seenMatchRule: MatchRule => Unit,
                seenPat: Pattern => Unit,
                seenResEnum: RestrictableEnum => Unit,
                seenRoot: Root => Unit,
                seenSig: Sig => Unit,
                seenSpec: Spec => Unit,
                seenStruct: Struct => Unit,
                seenTMatchRule: TypeMatchRule => Unit,
                seenTrait: Trait => Unit,
                seenType: Type => Unit,
                seenTypeAlias: TypeAlias => Unit,
                accept: SourceLocation => Boolean): Unit = {

    root.defs.foreach{ case (_, defn) => visitDef(defn, seenAnnos, seenCatchRule, seenConstraint, seenDef, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenSpec, seenTMatchRule, seenType, accept) }

    root.effects.foreach{ case (_, eff) => visitEffect(eff, seenEff, accept) }

    // root.entryPoint.map{ case v => visitEntryPoint(visit, accept)(v) }

    root.enums.foreach{ case (_, e) => visitEnum(e, seenEnum, accept) }

    root.instances.foreach{ case (_, l) => l.foreach(ins => visitInstance(ins, seenInstance, accept)) }

    // root.modules.map { case (_, v) => visitModule(v, ???, ???) }

    root.restrictableEnums.foreach{ case (_, e) => visitResEnum(e, seenResEnum, accept) } // experimental, maybe should be removed? 

    root.sigs.foreach{ case (_, sig) => visitSig(sig, seenSig, accept) }

    root.structs.foreach{ case (_, struct) => visitStruct(struct, seenStruct, accept) }

    // root.traitEnv.map{ case (_, v) => visitTraitEnv(???, ???)(v) };

    root.traits.foreach{ case (_, t) => visitTrait(t, seenTrait, accept) }

    root.typeAliases.foreach{ case (_, alias) => visitTypeAlias(alias, seenTypeAlias, accept) }

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

  private def visitEnum(e: Enum, seen: Enum => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(e)
    // e.cases
    //  .map{case (_, c) => c}
    //  .foreach(c => if (accept(c.loc)) { visitCase(c, ???, accept) })
  }

  private def visitCase(c: Case, seen: Case => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(c)
  }

  private def visitResEnum(e: RestrictableEnum, seen: RestrictableEnum => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(e)
    // e.cases
    //  .map{case (_, c) => c}
    //  .foreach(c => if (accept(c.loc)) { visitResCase(c, ???, accept) })
  }

  private def visitResCase(c: RestrictableCase, seen: RestrictableCase => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(c)
  }

  private def visitInstance(ins: Instance, seen: Instance => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(ins)
    // ins.defs.foreach(defn => visitDef(defn, ???, accept))
  }

  private def visitSig(sig: Sig, seen: Sig => Unit, accept: SourceLocation => Boolean): Unit = {
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

  private def visitStruct(struct: Struct, seen: Struct => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(struct)
    // struct.fields.foreach{case (_, field) => {
    //   if (accept(field.loc) ) { 
    //     visitStructField(field, ???, accept) 
    //   }
    // }}
  }

  private def visitStructField(field: StructField, seen: StructField => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(field)
  }

  private def visitTrait(t: Trait, seen: Trait => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(t)
    // t.laws.foreach(law => visitDef(law, ???, accept))
    // t.sigs.foreach(sig => visitSig(sig, ???, accept))
  }
  
  private def visitEffect(eff: Effect, seen: Effect => Unit, accept: SourceLocation => Boolean): Unit = {
    if (!(accept(eff.loc))) { return }
    // TODO
    // visit(eff)
    // ???
  }

  private def visitDef(defn: Def, 
                       seenAnnos: Annotations => Unit,
                       seenCatchRule: CatchRule => Unit,
                       seenConstraint: Constraint => Unit,
                       seenDef: Def => Unit, 
                       seenExpr: Expr => Unit, 
                       seenFParam: FormalParam => Unit,
                       seenFrag: ParYieldFragment => Unit,
                       seenHandlerRule: HandlerRule => Unit,
                       seenMatchRule: MatchRule => Unit,
                       seenPat: Pattern => Unit,
                       seenSpec: Spec => Unit,
                       seenTMatchRule: TypeMatchRule => Unit,
                       seenType: Type => Unit,
                       accept: SourceLocation => Boolean): Unit = {
    val insideDefn = accept(defn.spec.loc) || accept(defn.exp.loc) || accept(defn.sym.loc)
    if (!insideDefn) { return }

    seenDef(defn)

    visitSpec(defn.spec, seenSpec, accept)

    visitExpr(defn.exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
  }

  private def visitSpec(spec: Spec, seen: Spec => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitTypeAlias(alias: TypeAlias, seen: TypeAlias => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(alias)
    // alias.tparams.map(tp => if (accept(tp.loc)) { visitTypeParam(tp, ???, accept) })
  }

  private def visitTypeParam(tparam: TypeParam, seen: TypeParam => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(tparam)
  }

  private def visitUse(use: UseOrImport, seen: UseOrImport => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(use)
  }

  private def visitExpr(expr: Expr, 
                        seenAnnos: Annotations => Unit,
                        seenCatchRule: CatchRule => Unit,
                        seenConstraint: Constraint => Unit,
                        seenExpr: Expr => Unit, 
                        seenFParam: FormalParam => Unit,
                        seenFrag: ParYieldFragment => Unit,
                        seenHandlerRule: HandlerRule => Unit,
                        seenMatchRule: MatchRule => Unit,
                        seenPat: Pattern => Unit,
                        seenTMatchRule: TypeMatchRule => Unit,
                        seenType: Type => Unit,
                        accept: SourceLocation => Boolean): Unit = {
    // TODO: handle mutually recursive calls to other visit functions

    if (!(accept(expr.loc))) { return }

    seenExpr(expr)

    expr match {
      case Expr.Cst(cst, tpe, loc) => ()
      case Expr.Var(sym, tpe, loc) => ()
      case Expr.Def(sym, tpe, loc) => ()
      case Expr.Sig(sym, tpe, loc) => ()
      case Expr.Hole(sym, tpe, eff, loc) => ()

      case Expr.HoleWithExp(exp, tpe, eff, loc) => {
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
      }

      case Expr.OpenAs(symUse, exp, tpe, loc) => {
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
      }

      case Expr.Use(sym, alias, exp, loc) => {
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
      }

      case Expr.Lambda(fparam, exp, tpe, loc) => {
        visitFParam(fparam, seenFParam, accept)
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
      }

      case Expr.Apply(exp, exps, tpe, eff, loc) => {
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))
      }

      case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.Unary(sop, exp, tpe, eff, loc) => {
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
      }

      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
        visitAnnotations(ann, seenAnnos, accept)
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.Region(tpe, loc) => ()

      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp3, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.Stm(exp1, exp2, _, _, _) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.Discard(exp, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.Match(exp, rules, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        rules.foreach(rule => visitMatchRule(rule, seenMatchRule, accept))

      case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        rules.foreach(rule => visitTypeMatchRule(rule, seenTMatchRule, accept))

      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
        // Does nothing because feature is experimental

      case Expr.Tag(sym, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.Tuple(exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.RecordEmpty(tpe, loc) => ()

      case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp3, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.ArrayLength(exp, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp3, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
        fields.foreach{ case (_, e) => { 
          visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)  
        }}

        visitExpr(region, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

        visitExpr(region, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.StructGet(exp, sym, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.VectorLit(exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.VectorLength(exp, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.Ascribe(exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.InstanceOf(exp, clazz, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        // TODO maybe we need to visit `cast` (`CheckTypeCast`)?

      case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        declaredType.foreach(t => visitType(t, seenType, accept))
        declaredEff.foreach(t => visitType(t, seenType, accept))

      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.Without(exp, effUse, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        rules.foreach(rule => visitCatchRule(rule, seenCatchRule, accept))

      case Expr.Throw(exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        rules.foreach(rule => visitHandlerRule(rule, seenHandlerRule, accept))

      case Expr.Do(op, exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
        exps.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.GetField(field, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.GetStaticField(field, tpe, eff, loc) => ()

      case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ()

      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.GetChannel(exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
        rules.foreach(rule => {
          visitExpr(rule.chan, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
          visitExpr(rule.exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        })

        default.foreach(e => visitExpr(e, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept))

      case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.ParYield(frags, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)
        frags.foreach(frag => visitParYieldFrag(frag, seenFrag, accept))

      case Expr.Lazy(exp, tpe, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.Force(exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.FixpointConstraintSet(cs, tpe, loc) =>
        cs.foreach(con => visitConstraint(con, seenConstraint, accept))

      case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
        visitExpr(exp1, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 
        visitExpr(exp2, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept) 

      case Expr.FixpointSolve(exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
        visitExpr(exp, seenAnnos, seenCatchRule, seenConstraint, seenExpr, seenFParam, seenFrag, seenHandlerRule, seenMatchRule, seenPat, seenTMatchRule, seenType, accept)

      case Expr.Error(m, tpe, eff) => ()
    }
  }

  private def visitFParam(fparam: FormalParam, seen: FormalParam => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitHandlerRule(rule: HandlerRule, seen: HandlerRule => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitParYieldFrag(frag: ParYieldFragment, seen: ParYieldFragment => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitMatchRule(rule: MatchRule, seen: MatchRule => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitTypeMatchRule(rule: TypeMatchRule, seen: TypeMatchRule => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitType(tpe: Type, seen: Type => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitAnnotations(ann: Annotations, seen: Annotations => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitCatchRule(rule: CatchRule, seen: CatchRule => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitConstraint(con: Constraint, visit: Constraint => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  private def visitPattern(pat: Pattern, visit: Pattern => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }
}
