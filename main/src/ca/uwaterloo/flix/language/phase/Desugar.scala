/*
 * Copyright 2023 Jakob Schneider Villumsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.DesugaredAst.Expr
import ca.uwaterloo.flix.language.ast.WeededAst.Predicate
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, DesugaredAst, Name, SourceLocation, Type, WeededAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugDesugaredAst
import ca.uwaterloo.flix.util.ParOps

object Desugar {

  /**
    * Performs desugaring on `root`.
    */
  def run(root: WeededAst.Root, oldRoot: DesugaredAst.Root, changeSet: ChangeSet)(implicit flix: Flix): DesugaredAst.Root = flix.phase("Desugar") {
    // Compute the stale and fresh sources.
    val (stale, fresh) = changeSet.partition(root.units, oldRoot.units)

    val units = ParOps.parMapValues(stale)(visitUnit)
    val allUnits = units ++ fresh
    DesugaredAst.Root(allUnits, root.entryPoint, root.names)
  }

  /**
    * Desugars the given [[WeededAst.CompilationUnit]] `unit`.
    */
  private def visitUnit(unit: WeededAst.CompilationUnit)(implicit flix: Flix): DesugaredAst.CompilationUnit = unit match {
    case WeededAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val decls = decls0.map(visitDecl)
      DesugaredAst.CompilationUnit(usesAndImports, decls, loc)
  }

  /**
    * Maps `useOrImport0` to a corresponding [[DesugaredAst.UseOrImport]].
    */
  private def visitUseOrImport(useOrImport0: WeededAst.UseOrImport): DesugaredAst.UseOrImport = useOrImport0 match {
    case WeededAst.UseOrImport.Use(qname, alias, loc) =>
      DesugaredAst.UseOrImport.Use(qname, alias, loc)

    case WeededAst.UseOrImport.Import(name, alias, loc) =>
      DesugaredAst.UseOrImport.Import(name, alias, loc)
  }

  /**
    * Compiles `decl0` to a [[DesugaredAst.Declaration]].
    */
  private def visitDecl(decl0: WeededAst.Declaration)(implicit flix: Flix): DesugaredAst.Declaration = decl0 match {
    case WeededAst.Declaration.Namespace(ident, usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val decls = decls0.map(visitDecl)
      DesugaredAst.Declaration.Namespace(ident, usesAndImports, decls, loc)

    case d: WeededAst.Declaration.Trait => visitTrait(d)
    case d: WeededAst.Declaration.Instance => visitInstance(d)
    case d: WeededAst.Declaration.Def => visitDef(d)
    case d: WeededAst.Declaration.Law => visitLaw(d)
    case d: WeededAst.Declaration.Enum => visitEnum(d)
    case d: WeededAst.Declaration.RestrictableEnum => visitRestrictableEnum(d)
    case d: WeededAst.Declaration.TypeAlias => visitTypeAlias(d)
    case d: WeededAst.Declaration.Effect => visitEffect(d)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Trait]] `trait0`.
    */
  private def visitTrait(trait0: WeededAst.Declaration.Trait)(implicit flix: Flix): DesugaredAst.Declaration.Trait = trait0 match {
    case WeededAst.Declaration.Trait(doc, ann, mod, ident, tparam0, superTraits0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superTraits = superTraits0.map(visitTypeConstraint)
      val assocs = assocs0.map(visitAssocTypeSig)
      val sigs = sigs0.map(visitSig)
      val laws = laws0.map(visitDef)
      DesugaredAst.Declaration.Trait(doc, ann, mod, ident, tparam, superTraits, assocs, sigs, laws, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Instance]] `instance0`.
    */
  private def visitInstance(instance0: WeededAst.Declaration.Instance)(implicit flix: Flix): DesugaredAst.Declaration.Instance = instance0 match {
    case WeededAst.Declaration.Instance(doc, ann, mod, clazz, tpe0, tconstrs0, assocs0, defs0, loc) =>
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      val assocs = assocs0.map(visitAssocTypeDef)
      val defs = defs0.map(visitDef)
      DesugaredAst.Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, assocs, defs, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Def]] `def0`.
    */
  private def visitDef(def0: WeededAst.Declaration.Def)(implicit flix: Flix): DesugaredAst.Declaration.Def = def0 match {
    case WeededAst.Declaration.Def(doc, ann, mod, ident, tparams0, fparams0, exp0, tpe0, eff0, tconstrs0, constrs0, loc) =>
      flix.subtask(ident.name, sample = true)
      val tparams = visitKindedTypeParams(tparams0)
      val fparams = visitFormalParams(fparams0)
      val exp = visitExp(exp0)
      val tpe = visitType(tpe0)
      val eff = eff0.map(visitType)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      val constrs = constrs0.map(visitEqualityConstraint)
      DesugaredAst.Declaration.Def(doc, ann, mod, ident, tparams, fparams, exp, tpe, eff, tconstrs, constrs, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Law]] `law0`.
    */
  private def visitLaw(law0: WeededAst.Declaration.Law)(implicit flix: Flix): DesugaredAst.Declaration.Law = law0 match {
    case WeededAst.Declaration.Law(doc, ann, mod, ident, tparams0, fparams0, exp0, tpe0, eff0, tconstrs0, loc) =>
      val tparams = visitKindedTypeParams(tparams0)
      val fparams = visitFormalParams(fparams0)
      val exp = visitExp(exp0)
      val tpe = visitType(tpe0)
      val eff = visitType(eff0)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      DesugaredAst.Declaration.Law(doc, ann, mod, ident, tparams, fparams, exp, tpe, eff, tconstrs, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Enum]] `enum0`.
    */
  private def visitEnum(enum0: WeededAst.Declaration.Enum): DesugaredAst.Declaration.Enum = enum0 match {
    case WeededAst.Declaration.Enum(doc, ann, mod, ident, tparams0, derives0, cases0, loc) =>
      val tparams = visitTypeParams(tparams0)
      val derives = visitDerivations(derives0)
      val cases = cases0.map(visitCase)
      DesugaredAst.Declaration.Enum(doc, ann, mod, ident, tparams, derives, cases, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.RestrictableEnum]] `restrictableEnum0`.
    */
  private def visitRestrictableEnum(restrictableEnum0: WeededAst.Declaration.RestrictableEnum): DesugaredAst.Declaration.RestrictableEnum = restrictableEnum0 match {
    case WeededAst.Declaration.RestrictableEnum(doc, ann, mod, ident, index0, tparams0, derives0, cases0, loc) =>
      val index = visitTypeParam(index0)
      val tparams = visitTypeParams(tparams0)
      val derives = visitDerivations(derives0)
      val cases = cases0.map(visitRestrictableCase)
      DesugaredAst.Declaration.RestrictableEnum(doc, ann, mod, ident, index, tparams, derives, cases, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.TypeAlias]] `typeAlias0`.
    */
  private def visitTypeAlias(typeAlias0: WeededAst.Declaration.TypeAlias): DesugaredAst.Declaration.TypeAlias = typeAlias0 match {
    case WeededAst.Declaration.TypeAlias(doc, ann, mod, ident, tparams0, tpe0, loc) =>
      val tparams = visitTypeParams(tparams0)
      val tpe = visitType(tpe0)
      DesugaredAst.Declaration.TypeAlias(doc, ann, mod, ident, tparams, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Effect]] `eff0`.
    */
  private def visitEffect(eff0: WeededAst.Declaration.Effect): DesugaredAst.Declaration.Effect = eff0 match {
    case WeededAst.Declaration.Effect(doc, ann, mod, ident, ops0, loc) =>
      val ops = ops0.map(visitOp)
      DesugaredAst.Declaration.Effect(doc, ann, mod, ident, ops, loc)
  }

  /**
    * Desugars the given [[WeededAst.TypeParam]] `tparam0`.
    */
  private def visitTypeParam(tparam0: WeededAst.TypeParam): DesugaredAst.TypeParam = tparam0 match {
    case WeededAst.TypeParam.Unkinded(ident) => DesugaredAst.TypeParam.Unkinded(ident)
    case WeededAst.TypeParam.Kinded(ident, kind0) =>
      val kind = visitKind(kind0)
      DesugaredAst.TypeParam.Kinded(ident, kind)
  }

  /**
    * Desugars the given [[WeededAst.TypeConstraint]] `tconstr0`.
    */
  private def visitTypeConstraint(tconstr0: WeededAst.TypeConstraint): DesugaredAst.TypeConstraint = tconstr0 match {
    case WeededAst.TypeConstraint(clazz, tpe0, loc) =>
      val tpe = visitType(tpe0)
      DesugaredAst.TypeConstraint(clazz, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.AssocTypeSig]] `assoc0`.
    */
  private def visitAssocTypeSig(assoc0: WeededAst.Declaration.AssocTypeSig): DesugaredAst.Declaration.AssocTypeSig = assoc0 match {
    case WeededAst.Declaration.AssocTypeSig(doc, mod, ident, tparam0, kind0, tpe0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val kind = visitKind(kind0)
      val tpe = tpe0.map(visitType)
      DesugaredAst.Declaration.AssocTypeSig(doc, mod, ident, tparam, kind, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Sig]] `sig0`.
    */
  private def visitSig(sig0: WeededAst.Declaration.Sig)(implicit flix: Flix): DesugaredAst.Declaration.Sig = sig0 match {
    case WeededAst.Declaration.Sig(doc, ann, mod, ident, tparams0, fparams0, exp0, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      val tparams = visitKindedTypeParams(tparams0)
      val fparams = visitFormalParams(fparams0)
      val exp = exp0.map(visitExp)
      val tpe = visitType(tpe0)
      val eff = eff0.map(visitType)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      val econstrs = econstrs0.map(visitEqualityConstraint)
      DesugaredAst.Declaration.Sig(doc, ann, mod, ident, tparams, fparams, exp, tpe, eff, tconstrs, econstrs, loc)
  }

  /**
    * Desugars the given [[WeededAst.Type]] `tpe0`.
    */
  private def visitType(tpe0: WeededAst.Type): DesugaredAst.Type = tpe0 match {
    case WeededAst.Type.Var(ident, loc) =>
      DesugaredAst.Type.Var(ident, loc)

    case WeededAst.Type.Ambiguous(qname, loc) =>
      DesugaredAst.Type.Ambiguous(qname, loc)

    case WeededAst.Type.Unit(loc) =>
      DesugaredAst.Type.Unit(loc)

    case WeededAst.Type.Tuple(elms, loc) =>
      val ts = elms.map(visitType)
      DesugaredAst.Type.Tuple(ts, loc)

    case WeededAst.Type.RecordRowEmpty(loc) =>
      DesugaredAst.Type.RecordRowEmpty(loc)

    case WeededAst.Type.RecordRowExtend(label, tpe, rest, loc) =>
      val t = visitType(tpe)
      val r = visitType(rest)
      DesugaredAst.Type.RecordRowExtend(label, t, r, loc)

    case WeededAst.Type.Record(row, loc) =>
      DesugaredAst.Type.Record(visitType(row), loc)

    case WeededAst.Type.SchemaRowEmpty(loc) =>
      DesugaredAst.Type.SchemaRowEmpty(loc)

    case WeededAst.Type.SchemaRowExtendByAlias(qname, targs, rest, loc) =>
      val targs1 = targs.map(visitType)
      val r = visitType(rest)
      DesugaredAst.Type.SchemaRowExtendByAlias(qname, targs1, r, loc)

    case WeededAst.Type.SchemaRowExtendByTypes(name, den, tpes, rest, loc) =>
      val ts = tpes.map(visitType)
      val r = visitType(rest)
      DesugaredAst.Type.SchemaRowExtendByTypes(name, den, ts, r, loc)

    case WeededAst.Type.Schema(row, loc) =>
      val r = visitType(row)
      DesugaredAst.Type.Schema(r, loc)

    case WeededAst.Type.Native(fqn, loc) =>
      DesugaredAst.Type.Native(fqn, loc)

    case WeededAst.Type.Arrow(tparams, eff, tresult, loc) =>
      val tparams1 = tparams.map(visitType)
      val eff1 = eff.map(visitType)
      val t = visitType(tresult)
      DesugaredAst.Type.Arrow(tparams1, eff1, t, loc)

    case WeededAst.Type.Apply(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.Apply(t1, t2, loc)

    case WeededAst.Type.True(loc) =>
      DesugaredAst.Type.True(loc)

    case WeededAst.Type.False(loc) =>
      DesugaredAst.Type.False(loc)

    case WeededAst.Type.Not(tpe, loc) =>
      val t = visitType(tpe)
      DesugaredAst.Type.Not(t, loc)

    case WeededAst.Type.And(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.And(t1, t2, loc)

    case WeededAst.Type.Or(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.Or(t1, t2, loc)

    case WeededAst.Type.Complement(tpe, loc) =>
      val t = visitType(tpe)
      DesugaredAst.Type.Complement(t, loc)

    case WeededAst.Type.Union(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.Union(t1, t2, loc)

    case WeededAst.Type.Intersection(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.Intersection(t1, t2, loc)

    case WeededAst.Type.Pure(loc) =>
      DesugaredAst.Type.Pure(loc)

    case WeededAst.Type.CaseSet(cases, loc) =>
      DesugaredAst.Type.CaseSet(cases, loc)

    case WeededAst.Type.CaseUnion(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.CaseUnion(t1, t2, loc)

    case WeededAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      DesugaredAst.Type.CaseIntersection(t1, t2, loc)

    case WeededAst.Type.CaseComplement(tpe, loc) =>
      val t = visitType(tpe)
      DesugaredAst.Type.CaseComplement(t, loc)

    case WeededAst.Type.Ascribe(tpe, kind, loc) =>
      val t = visitType(tpe)
      val k = visitKind(kind)
      DesugaredAst.Type.Ascribe(t, k, loc)

    case WeededAst.Type.Error(loc) =>
      DesugaredAst.Type.Error(loc)
  }

  /**
    * Desugars the given list of [[WeededAst.Type]] `tpes0`.
    */
  private def visitTypes(tpes0: List[WeededAst.Type]): List[DesugaredAst.Type] =
    tpes0.map(visitType)

  /**
    * Desugars the given [[WeededAst.Declaration.AssocTypeDef]] `assoc0`.
    */
  private def visitAssocTypeDef(assoc0: WeededAst.Declaration.AssocTypeDef): DesugaredAst.Declaration.AssocTypeDef = assoc0 match {
    case WeededAst.Declaration.AssocTypeDef(doc, mod, ident, arg0, tpe0, loc) =>
      val arg = visitType(arg0)
      val tpe = visitType(tpe0)
      DesugaredAst.Declaration.AssocTypeDef(doc, mod, ident, arg, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.KindedTypeParams]] `tparams0`.
    */
  private def visitKindedTypeParams(tparams0: WeededAst.KindedTypeParams): DesugaredAst.KindedTypeParams = tparams0 match {
    case WeededAst.TypeParams.Elided => DesugaredAst.TypeParams.Elided
    case WeededAst.TypeParams.Kinded(tparams1) =>
      val tparams = tparams1.map(visitTypeParam).collect { case t: DesugaredAst.TypeParam.Kinded => t }
      DesugaredAst.TypeParams.Kinded(tparams)
  }

  /**
    * Desugars the given list of [[WeededAst.FormalParam]] `fparams0`.
    */
  private def visitFormalParams(fparams0: List[WeededAst.FormalParam]): List[DesugaredAst.FormalParam] =
    fparams0.map(visitFormalParam)

  /**
    * Desugars the given [[WeededAst.FormalParam]] `fparam0`.
    */
  private def visitFormalParam(fparam0: WeededAst.FormalParam): DesugaredAst.FormalParam = fparam0 match {
    case WeededAst.FormalParam(ident, mod, tpe0, loc) =>
      val tpe = tpe0.map(visitType)
      DesugaredAst.FormalParam(ident, mod, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.EqualityConstraint]] `econstr0`.
    */
  private def visitEqualityConstraint(econstr0: WeededAst.EqualityConstraint): DesugaredAst.EqualityConstraint = econstr0 match {
    case WeededAst.EqualityConstraint(qname, tpe01, tpe02, loc) =>
      val tpe1 = visitType(tpe01)
      val tpe2 = visitType(tpe02)
      DesugaredAst.EqualityConstraint(qname, tpe1, tpe2, loc)
  }

  /**
    * Desugars the given [[WeededAst.TypeParams]] `tparams0`.
    */
  private def visitTypeParams(tparams0: WeededAst.TypeParams): DesugaredAst.TypeParams = tparams0 match {
    case params: WeededAst.KindedTypeParams => visitKindedTypeParams(params)
    case WeededAst.TypeParams.Unkinded(tparams1) =>
      val tparams = tparams1.map(visitTypeParam).collect { case t: DesugaredAst.TypeParam.Unkinded => t }
      DesugaredAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Desugars the given [[WeededAst.Derivations]] `derives0`.
    */
  private def visitDerivations(derives0: WeededAst.Derivations): DesugaredAst.Derivations = derives0 match {
    case WeededAst.Derivations(traits, loc) =>
      DesugaredAst.Derivations(traits, loc)
  }

  /**
    * Desugars the given [[WeededAst.Case]] `case0`.
    */
  private def visitCase(case0: WeededAst.Case): DesugaredAst.Case = case0 match {
    case WeededAst.Case(ident, tpe0, loc) =>
      val tpe = visitType(tpe0)
      DesugaredAst.Case(ident, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.RestrictableCase]] `case0`.
    */
  private def visitRestrictableCase(case0: WeededAst.RestrictableCase): DesugaredAst.RestrictableCase = case0 match {
    case WeededAst.RestrictableCase(ident, tpe0, loc) =>
      val tpe = visitType(tpe0)
      DesugaredAst.RestrictableCase(ident, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Op]] `op0`.
    */
  private def visitOp(op0: WeededAst.Declaration.Op): DesugaredAst.Declaration.Op = op0 match {
    case WeededAst.Declaration.Op(doc, ann, mod, ident, fparams0, tpe0, tconstrs0, loc) =>
      val fparams = visitFormalParams(fparams0)
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      DesugaredAst.Declaration.Op(doc, ann, mod, ident, fparams, tpe, tconstrs, loc)
  }

  /**
    * Desugars the given [[WeededAst.Kind]] `kind0`.
    */
  private def visitKind(kind0: WeededAst.Kind): DesugaredAst.Kind = kind0 match {
    case WeededAst.Kind.Ambiguous(qname, loc) =>
      DesugaredAst.Kind.Ambiguous(qname, loc)

    case WeededAst.Kind.Arrow(kind1, kind2, loc) =>
      val k1 = visitKind(kind1)
      val k2 = visitKind(kind2)
      DesugaredAst.Kind.Arrow(k1, k2, loc)
  }

  /**
    * Desugars the given [[WeededAst.Expr]] `exp0`.
    */
  private def visitExp(exp0: WeededAst.Expr)(implicit flix: Flix): DesugaredAst.Expr = exp0 match {
    case WeededAst.Expr.Ambiguous(qname, loc) =>
      Expr.Ambiguous(qname, loc)

    case WeededAst.Expr.Open(qname, loc) =>
      Expr.Open(qname, loc)

    case WeededAst.Expr.OpenAs(qname, exp, loc) =>
      val e = visitExp(exp)
      Expr.OpenAs(qname, e, loc)

    case WeededAst.Expr.Hole(name, loc) =>
      Expr.Hole(name, loc)

    case WeededAst.Expr.HoleWithExp(exp, loc) =>
      val e = visitExp(exp)
      Expr.HoleWithExp(e, loc)

    case WeededAst.Expr.Use(uses, exp, loc) =>
      val u1 = uses.map(visitUseOrImport)
      val e = visitExp(exp)
      Expr.Use(u1, e, loc)

    case WeededAst.Expr.Cst(cst, loc) =>
      Expr.Cst(cst, loc)

    case WeededAst.Expr.Apply(exp, exps, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      Expr.Apply(e, es, loc)

    case WeededAst.Expr.Infix(exp1, exp2, exp3, loc) =>
      desugarInfix(exp1, exp2, exp3, loc)

    case WeededAst.Expr.Lambda(fparam, exp, loc) =>
      val fparam1 = visitFormalParam(fparam)
      val e = visitExp(exp)
      Expr.Lambda(fparam1, e, loc)

    case WeededAst.Expr.LambdaMatch(pat, exp, loc) =>
      val p = visitPattern(pat)
      val e = visitExp(exp)
      mkLambdaMatch(p, e, loc)

    case WeededAst.Expr.Unary(sop, exp, loc) =>
      val e = visitExp(exp)
      Expr.Unary(sop, e, loc)

    case WeededAst.Expr.Binary(sop, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Binary(sop, e1, e2, loc)

    case WeededAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.IfThenElse(e1, e2, e3, loc)

    case WeededAst.Expr.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Stm(e1, e2, loc)

    case WeededAst.Expr.Discard(exp, loc) =>
      val e = visitExp(exp)
      Expr.Discard(e, loc)

    case WeededAst.Expr.Let(ident, mod, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Let(ident, mod, e1, e2, loc)

    case WeededAst.Expr.LetRec(ident, ann, mod, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.LetRec(ident, ann, mod, e1, e2, loc)

    case WeededAst.Expr.LetImport(op, exp, loc) =>
      desugarLetImport(op, exp, loc)

    case WeededAst.Expr.Scope(ident, exp, loc) =>
      val e = visitExp(exp)
      Expr.Scope(ident, e, loc)

    case WeededAst.Expr.Match(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      Expr.Match(e, rs, loc)

    case WeededAst.Expr.TypeMatch(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitTypeMatchRule)
      Expr.TypeMatch(e, rs, loc)

    case WeededAst.Expr.RestrictableChoose(star, exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitRestrictableChooseRule)
      Expr.RestrictableChoose(star, e, rs, loc)

    case WeededAst.Expr.ApplicativeFor(frags, exp, loc) =>
      desugarApplicativeFor(frags, exp, loc)

    case WeededAst.Expr.ForEach(frags, exp, loc) =>
      desugarForEach(frags, exp, loc)

    case WeededAst.Expr.MonadicFor(frags, exp, loc) =>
      desugarMonadicFor(frags, exp, loc)

    case WeededAst.Expr.ForEachYield(frags, exp, loc) =>
      desugarForEachYield(frags, exp, loc)

    case WeededAst.Expr.LetMatch(pat, mod, tpe, exp1, exp2, loc) =>
      desugarLetMatch(pat, mod, tpe, exp1, exp2, loc)

    case WeededAst.Expr.Tuple(exps, loc) =>
      desugarTuple(exps, loc)

    case WeededAst.Expr.RecordEmpty(loc) =>
      Expr.RecordEmpty(loc)

    case WeededAst.Expr.RecordSelect(exp, label, loc) =>
      val e = visitExp(exp)
      Expr.RecordSelect(e, label, loc)

    case WeededAst.Expr.RecordExtend(label, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.RecordExtend(label, e1, e2, loc)

    case WeededAst.Expr.RecordRestrict(label, exp, loc) =>
      val e = visitExp(exp)
      Expr.RecordRestrict(label, e, loc)

    case WeededAst.Expr.ArrayLit(exps, exp, loc) =>
      val es = visitExps(exps)
      val e = visitExp(exp)
      Expr.ArrayLit(es, e, loc)

    case WeededAst.Expr.ArrayNew(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.ArrayNew(e1, e2, e3, loc)

    case WeededAst.Expr.ArrayLoad(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.ArrayLoad(e1, e2, loc)

    case WeededAst.Expr.ArrayLength(exp, loc) =>
      val e = visitExp(exp)
      Expr.ArrayLength(e, loc)

    case WeededAst.Expr.ArrayStore(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.ArrayStore(e1, e2, e3, loc)

    case WeededAst.Expr.VectorLit(exps, loc) =>
      val e = visitExps(exps)
      Expr.VectorLit(e, loc)

    case WeededAst.Expr.VectorLoad(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.VectorLoad(e1, e2, loc)

    case WeededAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Expr.VectorLength(e, loc)

    case WeededAst.Expr.FCons(exp1, exp2, loc) =>
      desugarFCons(exp1, exp2, loc)

    case WeededAst.Expr.FAppend(exp1, exp2, loc) =>
      desugarFAppend(exp1, exp2, loc)

    case WeededAst.Expr.ListLit(exps, loc) =>
      desugarListLit(exps, loc)

    case WeededAst.Expr.SetLit(exps, loc) =>
      desugarSetLit(exps, loc)

    case WeededAst.Expr.MapLit(exps, loc) =>
      desugarMapLit(exps, loc)

    case WeededAst.Expr.Ref(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Ref(e1, e2, loc)

    case WeededAst.Expr.Deref(exp, loc) =>
      val e = visitExp(exp)
      Expr.Deref(e, loc)

    case WeededAst.Expr.Assign(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Assign(e1, e2, loc)

    case WeededAst.Expr.Ascribe(exp, expectedType, expectedEff, loc) =>
      val e = visitExp(exp)
      val ts = expectedType.map(visitType)
      val effs = expectedEff.map(visitType)
      Expr.Ascribe(e, ts, effs, loc)

    case WeededAst.Expr.InstanceOf(exp, className, loc) =>
      val e = visitExp(exp)
      Expr.InstanceOf(e, className, loc)

    case WeededAst.Expr.CheckedCast(cast, exp, loc) =>
      val e = visitExp(exp)
      Expr.CheckedCast(cast, e, loc)

    case WeededAst.Expr.UncheckedCast(exp, declaredType, declaredEff, loc) =>
      val e = visitExp(exp)
      val t = declaredType.map(visitType)
      val eff = declaredEff.map(visitType)
      Expr.UncheckedCast(e, t, eff, loc)

    case WeededAst.Expr.UncheckedMaskingCast(exp, loc) =>
      val e = visitExp(exp)
      Expr.UncheckedMaskingCast(e, loc)

    case WeededAst.Expr.Without(exp, eff, loc) =>
      val e = visitExp(exp)
      Expr.Without(e, eff, loc)

    case WeededAst.Expr.TryCatch(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      Expr.TryCatch(e, rs, loc)

    case WeededAst.Expr.TryWith(exp, handlers, loc) =>
      val e = visitExp(exp)
      handlers.foldLeft(e) {
        case (acc, handler) =>
          val rs = handler.rules.map(visitHandlerRule)
          Expr.TryWith(acc, handler.eff, rs, loc)
      }

    case WeededAst.Expr.Do(op, exps, loc) =>
      val es = visitExps(exps)
      Expr.Do(op, es, loc)

    case WeededAst.Expr.InvokeConstructor(className, exps, sig, loc) =>
      val es = visitExps(exps)
      val ts = sig.map(visitType)
      Expr.InvokeConstructor(className, es, ts, loc)

    case WeededAst.Expr.InvokeMethod(className, methodName, exp, exps, sig, retTpe, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      val ts = sig.map(visitType)
      val rt = visitType(retTpe)
      Expr.InvokeMethod(className, methodName, e, es, ts, rt, loc)

    case WeededAst.Expr.InvokeStaticMethod(className, methodName, exps, sig, retTpe, loc) =>
      val es = visitExps(exps)
      val ts = sig.map(visitType)
      val rt = visitType(retTpe)
      Expr.InvokeStaticMethod(className, methodName, es, ts, rt, loc)

    case WeededAst.Expr.GetField(className, fieldName, exp, loc) =>
      val e = visitExp(exp)
      Expr.GetField(className, fieldName, e, loc)

    case WeededAst.Expr.PutField(className, fieldName, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.PutField(className, fieldName, e1, e2, loc)

    case WeededAst.Expr.GetStaticField(className, fieldName, loc) =>
      Expr.GetStaticField(className, fieldName, loc)

    case WeededAst.Expr.PutStaticField(className, fieldName, exp, loc) =>
      val e = visitExp(exp)
      Expr.PutStaticField(className, fieldName, e, loc)

    case WeededAst.Expr.NewObject(tpe, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod)
      Expr.NewObject(t, ms, loc)

    case WeededAst.Expr.Static(loc) =>
      val tpe = Type.mkRegion(Type.IO, loc)
      DesugaredAst.Expr.Region(tpe, loc)

    case WeededAst.Expr.NewChannel(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.NewChannel(e1, e2, loc)

    case WeededAst.Expr.GetChannel(exp, loc) =>
      val e = visitExp(exp)
      Expr.GetChannel(e, loc)

    case WeededAst.Expr.PutChannel(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.PutChannel(e1, e2, loc)

    case WeededAst.Expr.SelectChannel(rules, exp, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val es = exp.map(visitExp)
      Expr.SelectChannel(rs, es, loc)

    case WeededAst.Expr.Spawn(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Spawn(e1, e2, loc)

    case WeededAst.Expr.ParYield(frags, exp, loc) =>
      val fs = frags.map(visitParYieldFragment)
      val e = visitExp(exp)
      Expr.ParYield(fs, e, loc)

    case WeededAst.Expr.Lazy(exp, loc) =>
      val e = visitExp(exp)
      Expr.Lazy(e, loc)

    case WeededAst.Expr.Force(exp, loc) =>
      val e = visitExp(exp)
      Expr.Force(e, loc)

    case WeededAst.Expr.FixpointConstraintSet(cs, loc) =>
      val cs1 = cs.map(visitConstraint)
      Expr.FixpointConstraintSet(cs1, loc)

    case WeededAst.Expr.FixpointLambda(pparams, exp, loc) =>
      val ps = pparams.map(visitPredicateParam)
      val e = visitExp(exp)
      Expr.FixpointLambda(ps, e, loc)

    case WeededAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.FixpointMerge(e1, e2, loc)

    case WeededAst.Expr.FixpointSolve(exp, loc) =>
      val e = visitExp(exp)
      Expr.FixpointSolve(e, loc)

    case WeededAst.Expr.FixpointFilter(pred, exp, loc) =>
      val e = visitExp(exp)
      Expr.FixpointFilter(pred, e, loc)

    case WeededAst.Expr.FixpointInject(exp, pred, loc) =>
      val e = visitExp(exp)
      Expr.FixpointInject(e, pred, loc)

    case WeededAst.Expr.FixpointInjectInto(exps, idents, loc) =>
      desugarFixpointInjectInto(exps, idents, loc)

    case WeededAst.Expr.FixpointSolveWithProject(exps, optIdents, loc) =>
      desugarFixpointSolveWithProject(exps, optIdents, loc)

    case WeededAst.Expr.FixpointQueryWithSelect(exps0, selects0, from0, where0, loc) =>
      desugarFixpointQueryWithSelect(exps0, selects0, from0, where0, loc)

    case WeededAst.Expr.FixpointProject(pred, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.FixpointProject(pred, e1, e2, loc)

    case WeededAst.Expr.Debug(exp, kind, loc) =>
      desugarDebug(exp, kind, loc)

    case WeededAst.Expr.Error(m) =>
      DesugaredAst.Expr.Error(m)
  }

  /**
    * Desugars the given list of [[WeededAst.Expr]] `exps0`.
    */
  private def visitExps(exps0: List[WeededAst.Expr])(implicit flix: Flix): List[DesugaredAst.Expr] =
    exps0.map(visitExp)

  /**
    * Desugars the given [[WeededAst.MatchRule]] `rule0`.
    */
  private def visitMatchRule(rule0: WeededAst.MatchRule)(implicit flix: Flix): DesugaredAst.MatchRule = rule0 match {
    case WeededAst.MatchRule(pat, exp1, exp2) =>
      val p = visitPattern(pat)
      val e1 = exp1.map(visitExp)
      val e2 = visitExp(exp2)
      DesugaredAst.MatchRule(p, e1, e2)
  }

  /**
    * Desugars the given [[WeededAst.Pattern]] `pat0`.
    */
  private def visitPattern(pat0: WeededAst.Pattern): DesugaredAst.Pattern = pat0 match {
    case WeededAst.Pattern.Wild(loc) =>
      DesugaredAst.Pattern.Wild(loc)

    case WeededAst.Pattern.Var(ident, loc) =>
      DesugaredAst.Pattern.Var(ident, loc)

    case WeededAst.Pattern.Cst(cst, loc) =>
      DesugaredAst.Pattern.Cst(cst, loc)

    case WeededAst.Pattern.Tag(qname, pat, loc) =>
      val p = visitPattern(pat)
      DesugaredAst.Pattern.Tag(qname, p, loc)

    case WeededAst.Pattern.Tuple(elms, loc) =>
      val es = elms.map(visitPattern)
      DesugaredAst.Pattern.Tuple(es, loc)

    case WeededAst.Pattern.Record(pats, pat, loc) =>
      val ps = pats.map(visitRecordLabelPattern)
      val p = visitPattern(pat)
      DesugaredAst.Pattern.Record(ps, p, loc)

    case WeededAst.Pattern.RecordEmpty(loc) =>
      DesugaredAst.Pattern.RecordEmpty(loc)

    case WeededAst.Pattern.Error(loc) =>
      DesugaredAst.Pattern.Error(loc)
  }

  /**
    * Desugars the given [[WeededAst.TypeMatchRule]] `rule0`.
    */
  private def visitTypeMatchRule(rule0: WeededAst.TypeMatchRule)(implicit flix: Flix): DesugaredAst.TypeMatchRule = rule0 match {
    case WeededAst.TypeMatchRule(ident, tpe, exp) =>
      val t = visitType(tpe)
      val e = visitExp(exp)
      DesugaredAst.TypeMatchRule(ident, t, e)
  }

  /**
    * Desugars the given [[WeededAst.RestrictableChooseRule]] `rule0`.
    */
  private def visitRestrictableChooseRule(rule0: WeededAst.RestrictableChooseRule)(implicit flix: Flix): DesugaredAst.RestrictableChooseRule = rule0 match {
    case WeededAst.RestrictableChooseRule(pat, exp) =>
      val p = visitRestrictableChoosePattern(pat)
      val e = visitExp(exp)
      DesugaredAst.RestrictableChooseRule(p, e)
  }

  /**
    * Desugars the given [[WeededAst.RestrictableChoosePattern]] `pat0`.
    */
  private def visitRestrictableChoosePattern(pat0: WeededAst.RestrictableChoosePattern): DesugaredAst.RestrictableChoosePattern = {
    def visitVarOrWild(varOrWild0: WeededAst.RestrictableChoosePattern.VarOrWild): DesugaredAst.RestrictableChoosePattern.VarOrWild =
      varOrWild0 match {
        case WeededAst.RestrictableChoosePattern.Wild(loc) =>
          DesugaredAst.RestrictableChoosePattern.Wild(loc)

        case WeededAst.RestrictableChoosePattern.Var(ident, loc) =>
          DesugaredAst.RestrictableChoosePattern.Var(ident, loc)
      }

    pat0 match {
      case WeededAst.RestrictableChoosePattern.Tag(qname, pat, loc) =>
        val p = pat.map(visitVarOrWild)
        DesugaredAst.RestrictableChoosePattern.Tag(qname, p, loc)
    }
  }

  /**
    * Desugars the given [[WeededAst.CatchRule]] `rule0`.
    */
  private def visitCatchRule(rule0: WeededAst.CatchRule)(implicit flix: Flix): DesugaredAst.CatchRule = rule0 match {
    case WeededAst.CatchRule(ident, className, exp) =>
      val e = visitExp(exp)
      DesugaredAst.CatchRule(ident, className, e)
  }

  /**
    * Desugars the given [[WeededAst.HandlerRule]] `rule0`.
    */
  private def visitHandlerRule(rule0: WeededAst.HandlerRule)(implicit flix: Flix): DesugaredAst.HandlerRule = rule0 match {
    case WeededAst.HandlerRule(op, fparams, exp) =>
      val fps = visitFormalParams(fparams)
      val e = visitExp(exp)
      DesugaredAst.HandlerRule(op, fps, e)
  }

  /**
    * Desugars the given [[WeededAst.JvmMethod]] `method0`.
    */
  private def visitJvmMethod(method0: WeededAst.JvmMethod)(implicit flix: Flix): DesugaredAst.JvmMethod = method0 match {
    case WeededAst.JvmMethod(ident, fparams, exp, tpe, eff, loc) =>
      val fps = visitFormalParams(fparams)
      val e = visitExp(exp)
      val t = visitType(tpe)
      val ef = eff.map(visitType)
      DesugaredAst.JvmMethod(ident, fps, e, t, ef, loc)
  }

  /**
    * Desugars the given [[WeededAst.SelectChannelRule]] `rule0`.
    */
  private def visitSelectChannelRule(rule0: WeededAst.SelectChannelRule)(implicit flix: Flix): DesugaredAst.SelectChannelRule = rule0 match {
    case WeededAst.SelectChannelRule(ident, exp1, exp2) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      DesugaredAst.SelectChannelRule(ident, e1, e2)
  }

  /**
    * Desugars the given [[WeededAst.ParYieldFragment]] `frag0`.
    */
  private def visitParYieldFragment(frag0: WeededAst.ParYieldFragment)(implicit flix: Flix): DesugaredAst.ParYieldFragment = frag0 match {
    case WeededAst.ParYieldFragment(pat, exp, loc) =>
      val p = visitPattern(pat)
      val e = visitExp(exp)
      DesugaredAst.ParYieldFragment(p, e, loc)
  }

  /**
    * Desugars the given [[WeededAst.Constraint]] `constraint0`.
    */
  private def visitConstraint(constraint0: WeededAst.Constraint)(implicit flix: Flix): DesugaredAst.Constraint = {
    def visitHead(head0: WeededAst.Predicate.Head): DesugaredAst.Predicate.Head = head0 match {
      case WeededAst.Predicate.Head.Atom(pred, den, exps, loc) =>
        val e = visitExps(exps)
        DesugaredAst.Predicate.Head.Atom(pred, den, e, loc)
    }

    constraint0 match {
      case WeededAst.Constraint(head, body, loc) =>
        val h = visitHead(head)
        val b = body.map(visitPredicateBody)
        DesugaredAst.Constraint(h, b, loc)
    }
  }

  /**
    * Desugars the given [[WeededAst.Predicate.Body]] `body0`.
    */
  private def visitPredicateBody(body0: WeededAst.Predicate.Body)(implicit flix: Flix): DesugaredAst.Predicate.Body = body0 match {
    case WeededAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
      val ts = terms.map(visitPattern)
      DesugaredAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc)

    case WeededAst.Predicate.Body.Functional(idents, exp, loc) =>
      val e = visitExp(exp)
      DesugaredAst.Predicate.Body.Functional(idents, e, loc)

    case WeededAst.Predicate.Body.Guard(exp, loc) =>
      val e = visitExp(exp)
      DesugaredAst.Predicate.Body.Guard(e, loc)
  }

  /**
    * Desugars the given list of [[WeededAst.Predicate.Body]] `bodies0`.
    */
  private def visitPredicateBodies(bodies0: List[WeededAst.Predicate.Body])(implicit flix: Flix): List[DesugaredAst.Predicate.Body] =
    bodies0.map(visitPredicateBody)

  /**
    * Desugars the given [[WeededAst.PredicateParam]] `param0`.
    */
  private def visitPredicateParam(param0: WeededAst.PredicateParam): DesugaredAst.PredicateParam = param0 match {
    case WeededAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      DesugaredAst.PredicateParam.PredicateParamUntyped(pred, loc)

    case WeededAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      val ts = tpes.map(visitType)
      DesugaredAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
  }

  /**
    * Desugars the given [[WeededAst.Pattern.Record.RecordLabelPattern]] `pat0`.
    */
  private def visitRecordLabelPattern(pat0: WeededAst.Pattern.Record.RecordLabelPattern): DesugaredAst.Pattern.Record.RecordLabelPattern = pat0 match {
    case WeededAst.Pattern.Record.RecordLabelPattern(label, pat, loc) =>
      val p = pat.map(visitPattern)
      DesugaredAst.Pattern.Record.RecordLabelPattern(label, p, loc)
  }

  /**
    * Rewrites an infix expression into a normal function application.
    * {{{
    *   a `f` b
    * }}}
    * desugars to
    * {{{
    *   f(a, b)
    * }}}
    */
  private def desugarInfix(exp1: WeededAst.Expr, exp2: WeededAst.Expr, exp3: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr.Apply = {
    val e1 = visitExp(exp1)
    val e2 = visitExp(exp2)
    val e3 = visitExp(exp3)
    Expr.Apply(e2, List(e1, e3), loc0)
  }

  /**
    * Rewrites the let-import into a let-bound lambda.
    * The specific implementation of the lambda depends on `op`.
    */
  private def desugarLetImport(op0: WeededAst.JvmOp, exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = op0 match {
    case WeededAst.JvmOp.Constructor(fqn, sig, tpe, eff, ident) =>
      desugarJvmOpConstructor(fqn, sig, tpe, eff, ident, exp0, loc0)

    case WeededAst.JvmOp.Method(fqn, sig, tpe, eff, identOpt) =>
      desugarJvmOpMethod(fqn, sig, tpe, eff, identOpt, exp0, loc0)

    case WeededAst.JvmOp.StaticMethod(fqn, sig, tpe, eff, identOpt) =>
      desugarJvmOpStaticMethod(fqn, sig, tpe, eff, identOpt, exp0, loc0)

    case WeededAst.JvmOp.GetField(fqn, tpe, eff, ident) =>
      desugarJvmOpGetField(fqn, tpe, eff, ident, exp0, loc0)

    case WeededAst.JvmOp.PutField(fqn, tpe, eff, ident) =>
      desugarJvmOpPutField(fqn, tpe, eff, ident, exp0, loc0)

    case WeededAst.JvmOp.GetStaticField(fqn, tpe, eff, ident) =>
      desugarJvmOpGetStaticField(fqn, tpe, eff, ident, exp0, loc0)

    case WeededAst.JvmOp.PutStaticField(fqn, tpe, eff, ident) =>
      desugarJvmOpPutStaticField(fqn, tpe, eff, ident, exp0, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * (args...) -> InvokeConstructor(args) as tpe \ eff
    * }}}
    */
  private def desugarJvmOpConstructor(fqn0: Name.JavaName, sig0: List[WeededAst.Type], tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Name.Ident, exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val e = visitExp(exp0)
    val ts = visitTypes(sig0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    // Compute the class name.
    val className = fqn0.toString

    ts match {
      case Nil => // Case 1: No arguments.
        val fparam = DesugaredAst.FormalParam(Name.Ident(loc0.sp1, "_", loc0.sp2), Ast.Modifiers.Empty, Some(DesugaredAst.Type.Unit(loc0)), loc0)
        val call = DesugaredAst.Expr.InvokeConstructor(className, Nil, Nil, loc0)
        val lambdaBody = jvmCast(call, tpe, eff, loc0)
        val e1 = DesugaredAst.Expr.Lambda(fparam, lambdaBody, loc0)
        DesugaredAst.Expr.Let(ident0, Ast.Modifiers.Empty, e1, e, loc0)

      case _ =>
        // Introduce a formal parameter (of appropriate type) for each declared argument.
        val fs = ts.zipWithIndex.map {
          case (tpe, index) =>
            val id = Name.Ident(loc0.sp1, "a" + index, loc0.sp2)
            DesugaredAst.FormalParam(id, Ast.Modifiers.Empty, Some(tpe), loc0)
        }

        // Compute the argument to the method call.
        val as = ts.zipWithIndex.map {
          case (_, index) =>
            val ident = Name.Ident(loc0.sp1, "a" + index, loc0.sp2)
            DesugaredAst.Expr.Ambiguous(Name.mkQName(ident), loc0)
        }

        // Assemble the lambda expression.
        val call = DesugaredAst.Expr.InvokeConstructor(className, as, ts, loc0)
        val lambdaBody = jvmCast(call, tpe, eff, loc0)
        val e1 = mkCurried(fs, lambdaBody, loc0)
        DesugaredAst.Expr.Let(ident0, Ast.Modifiers.Empty, e1, e, loc0)
    }
  }

  /**
    * Returns an unchecked cast to the given type and effect, avoiding redundant
    * effect casts to syntactic IO.
    */
  private def jvmCast(exp: Expr, tpe: DesugaredAst.Type, eff: Option[DesugaredAst.Type], loc0: SourceLocation): Expr = {
    // ignore redundant effect casts to IO
    val correctedEff = eff match {
      case None => None
      case Some(DesugaredAst.Type.Ambiguous(Name.QName(_, Name.NName(_, Nil, _), Name.Ident(_, "IO", _), _), _)) => None
      case Some(v) => Some(v)
    }
    DesugaredAst.Expr.UncheckedCast(exp, Some(tpe), correctedEff, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * (obj, args...) -> InvokeMethod(obj, args) as tpe \ eff
    * }}}
    */
  private def desugarJvmOpMethod(fqn0: WeededAst.JavaClassMember, sig0: List[WeededAst.Type], tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Option[Name.Ident], exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val (className, methodName) = splitClassAndMember(fqn0)
    val e = visitExp(exp0)
    val ts = visitTypes(sig0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    // Compute the name of the let-bound variable.
    val ident = ident0.getOrElse(Name.Ident(fqn0.loc.sp1, methodName, fqn0.loc.sp2))

    val receiverType = DesugaredAst.Type.Native(className, loc0)

    // Introduce a formal parameter for the object argument.
    val objId = Name.Ident(loc0.sp1, "obj" + Flix.Delimiter, loc0.sp2)
    val objParam = DesugaredAst.FormalParam(objId, Ast.Modifiers.Empty, Some(receiverType), loc0)
    val objExp = DesugaredAst.Expr.Ambiguous(Name.mkQName(objId), loc0)

    // Introduce a formal parameter (of appropriate type) for each declared argument.
    val fs = objParam :: ts.zipWithIndex.map {
      case (tpe, index) =>
        val ident = Name.Ident(loc0.sp1, "a" + index + Flix.Delimiter, loc0.sp2)
        DesugaredAst.FormalParam(ident, Ast.Modifiers.Empty, Some(tpe), loc0)
    }

    // Compute the argument to the method call.
    val as = objExp :: ts.zipWithIndex.map {
      case (_, index) =>
        val ident = Name.Ident(loc0.sp1, "a" + index + Flix.Delimiter, loc0.sp2)
        DesugaredAst.Expr.Ambiguous(Name.mkQName(ident), loc0)
    }

    // Assemble the lambda expression.
    val call = DesugaredAst.Expr.InvokeMethod(className, methodName, as.head, as.tail, ts, tpe, loc0)
    val lambdaBody = jvmCast(call, tpe, eff, loc0)
    val e1 = mkCurried(fs, lambdaBody, loc0)
    DesugaredAst.Expr.Let(ident, Ast.Modifiers.Empty, e1, e, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * (args...) -> InvokeStaticMethod(args) as tpe \ eff
    * }}}
    */
  private def desugarJvmOpStaticMethod(fqn0: WeededAst.JavaClassMember, sig0: List[WeededAst.Type], tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Option[Name.Ident], exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val (className, methodName) = splitClassAndMember(fqn0)
    val e = visitExp(exp0)
    val ts = visitTypes(sig0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    // Compute the name of the let-bound variable.
    val ident = ident0.getOrElse(Name.Ident(fqn0.loc.sp1, methodName, fqn0.loc.sp2))

    ts match {
      case Nil => // Case 1: No arguments.
        val fparam = DesugaredAst.FormalParam(Name.Ident(loc0.sp1, "_", loc0.sp2), Ast.Modifiers.Empty, Some(DesugaredAst.Type.Unit(loc0)), loc0)
        val call = DesugaredAst.Expr.InvokeStaticMethod(className, methodName, Nil, Nil, tpe, loc0)
        val lambdaBody = jvmCast(call, tpe, eff, loc0)
        val e1 = DesugaredAst.Expr.Lambda(fparam, lambdaBody, loc0)
        DesugaredAst.Expr.Let(ident, Ast.Modifiers.Empty, e1, e, loc0)

      case _ =>
        // Introduce a formal parameter (of appropriate type) for each declared argument.
        val fs = ts.zipWithIndex.map {
          case (tpe, index) =>
            val id = Name.Ident(loc0.sp1, "a" + index + Flix.Delimiter, loc0.sp2)
            DesugaredAst.FormalParam(id, Ast.Modifiers.Empty, Some(tpe), loc0)
        }

        // Compute the argument to the method call.
        val as = ts.zipWithIndex.map {
          case (_, index) =>
            val ident = Name.Ident(loc0.sp1, "a" + index + Flix.Delimiter, loc0.sp2)
            DesugaredAst.Expr.Ambiguous(Name.mkQName(ident), loc0)
        }

        // Assemble the lambda expression.
        val call = DesugaredAst.Expr.InvokeStaticMethod(className, methodName, as, ts, tpe, loc0)
        val lambdaBody = jvmCast(call, tpe, eff, loc0)
        val e1 = mkCurried(fs, lambdaBody, loc0)
        DesugaredAst.Expr.Let(ident, Ast.Modifiers.Empty, e1, e, loc0)
    }
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * o -> GetField(o) as tpe \ eff
    * }}}
    */
  private def desugarJvmOpGetField(fqn0: WeededAst.JavaClassMember, tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Name.Ident, exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val (className, fieldName) = splitClassAndMember(fqn0)
    val e = visitExp(exp0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    val objectId = Name.Ident(loc0.sp1, "o" + Flix.Delimiter, loc0.sp2)
    val objectExp = DesugaredAst.Expr.Ambiguous(Name.mkQName(objectId), loc0)
    val objectParam = DesugaredAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc0)
    val call = DesugaredAst.Expr.GetField(className, fieldName, objectExp, loc0)
    val lambdaBody = jvmCast(call, tpe, eff, loc0)
    val e1 = DesugaredAst.Expr.Lambda(objectParam, lambdaBody, loc0)
    DesugaredAst.Expr.Let(ident0, Ast.Modifiers.Empty, e1, e, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * (o, v) -> PutField(o, v) as tpe \ eff
    * }}}
    */
  private def desugarJvmOpPutField(fqn0: WeededAst.JavaClassMember, tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Name.Ident, exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val (className, fieldName) = splitClassAndMember(fqn0)
    val e = visitExp(exp0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    val objectId = Name.Ident(loc0.sp1, "o" + Flix.Delimiter, loc0.sp2)
    val valueId = Name.Ident(loc0.sp1, "v" + Flix.Delimiter, loc0.sp2)
    val objectExp = DesugaredAst.Expr.Ambiguous(Name.mkQName(objectId), loc0)
    val valueExp = DesugaredAst.Expr.Ambiguous(Name.mkQName(valueId), loc0)
    val objectParam = DesugaredAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc0)
    val valueParam = DesugaredAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc0)
    val call = DesugaredAst.Expr.PutField(className, fieldName, objectExp, valueExp, loc0)
    val lambdaBody = jvmCast(call, tpe, eff, loc0)
    val e1 = mkCurried(objectParam :: valueParam :: Nil, lambdaBody, loc0)
    DesugaredAst.Expr.Let(ident0, Ast.Modifiers.Empty, e1, e, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * _: Unit -> GetStaticField
    * }}}
    */
  private def desugarJvmOpGetStaticField(fqn0: WeededAst.JavaClassMember, tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Name.Ident, exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val (className, fieldName) = splitClassAndMember(fqn0)
    val e = visitExp(exp0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    val unitId = Name.Ident(loc0.sp1, "_", loc0.sp2)
    val unitParam = DesugaredAst.FormalParam(unitId, Ast.Modifiers.Empty, Some(DesugaredAst.Type.Unit(loc0)), loc0)
    val call = DesugaredAst.Expr.GetStaticField(className, fieldName, loc0)
    val lambdaBody = jvmCast(call, tpe, eff, loc0)
    val e1 = DesugaredAst.Expr.Lambda(unitParam, lambdaBody, loc0)
    DesugaredAst.Expr.Let(ident0, Ast.Modifiers.Empty, e1, e, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.LetImport]] to a let-bound lambda:
    * {{{
    * x -> PutStaticField(x)
    * }}}
    */
  private def desugarJvmOpPutStaticField(fqn0: WeededAst.JavaClassMember, tpe0: WeededAst.Type, eff0: Option[WeededAst.Type], ident0: Name.Ident, exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val (className, fieldName) = splitClassAndMember(fqn0)
    val e = visitExp(exp0)
    val tpe = visitType(tpe0)
    val eff = eff0.map(visitType)

    val valueId = Name.Ident(loc0.sp1, "v" + Flix.Delimiter, loc0.sp2)
    val valueExp = DesugaredAst.Expr.Ambiguous(Name.mkQName(valueId), loc0)
    val valueParam = DesugaredAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc0)
    val call = DesugaredAst.Expr.PutStaticField(className, fieldName, valueExp, loc0)
    val lambdaBody = jvmCast(call, tpe, eff, loc0)
    val e1 = DesugaredAst.Expr.Lambda(valueParam, lambdaBody, loc0)
    DesugaredAst.Expr.Let(ident0, Ast.Modifiers.Empty, e1, e, loc0)
  }

  /**
    * Rewrites a `ForA` loop into a series of `Applicative.ap` calls.
    * {{{
    *   forA (
    *         x <- xs;
    *         y <- ys
    *     ) yield exp
    *
    * }}}
    * desugars to
    * {{{
    * Applicative.ap(Functor.map(x -> y -> exp, xs), ys)
    * }}}
    *
    */
  private def desugarApplicativeFor(frags0: List[WeededAst.ForFragment.Generator], exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val fqnAp = "Applicative.ap"
    val fqnMap = "Functor.map"
    val yieldExp = visitExp(exp0)

    // Make lambda for Functor.map(lambda, ...). This lambda uses all patterns from the for-fragments.
    val lambda = frags0.foldRight(yieldExp) {
      case (WeededAst.ForFragment.Generator(pat, _, loc1), acc) =>
        val p = visitPattern(pat)
        mkLambdaMatch(p, acc, loc1)
    }

    // Apply first fragment to Functor.map
    val xs = visitExp(frags0.head.exp)
    val baseExp = mkApplyFqn(fqnMap, List(lambda, xs), loc0)

    // Apply rest of fragments to Applicative.ap
    frags0.tail.foldLeft(baseExp) {
      case (acc, WeededAst.ForFragment.Generator(_, fexp, loc1)) =>
        val e = visitExp(fexp)
        mkApplyFqn(fqnAp, List(acc, e), loc1)
    }
  }

  /**
    * Rewrites a `ForEach` loop into a series of `Iterator.forEach` calls.
    * {{{
    *   foreach (
    *           x <- xs;
    *           if x > 0;
    *           y <- ys
    *       ) println(x + y)
    * }}}
    * desugars to (omitting regions)
    * {{{
    *   Iterator.foreach(x -> if (x > 0) Iterator.foreach(y -> println(x + y), Iterable.iterator(ys)) else (), Iterable.iterator(xs))
    * }}}
    */
  private def desugarForEach(frags0: List[WeededAst.ForFragment], exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val fqnForEach = "Iterator.forEach"
    val fqnIterator = "Iterable.iterator"
    val regIdent = Name.Ident(loc0.sp1, "reg" + Flix.Delimiter + flix.genSym.freshId(), loc0.sp2).asSynthetic
    val regVar = DesugaredAst.Expr.Ambiguous(Name.mkQName(regIdent), loc0.asSynthetic)

    val foreachExp = frags0.foldRight(visitExp(exp0)) {
      case (WeededAst.ForFragment.Generator(pat1, exp1, loc1), acc) =>
        val p1 = visitPattern(pat1)
        val e1 = visitExp(exp1)
        val lambda = mkLambdaMatch(p1, acc, loc1)
        val iterable = mkApplyFqn(fqnIterator, List(regVar, e1), e1.loc)
        val fparams = List(lambda, iterable)
        mkApplyFqn(fqnForEach, fparams, loc1.asSynthetic)

      case (WeededAst.ForFragment.Guard(exp1, loc1), acc) =>
        val e1 = visitExp(exp1)
        DesugaredAst.Expr.IfThenElse(e1, acc, DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc1.asSynthetic), loc1.asSynthetic)

      case (WeededAst.ForFragment.Let(pat1, exp1, loc1), acc) =>
        // Rewrite to pattern match
        val p1 = visitPattern(pat1)
        val e1 = visitExp(exp1)
        val matchRule = DesugaredAst.MatchRule(p1, None, acc)
        DesugaredAst.Expr.Match(e1, List(matchRule), loc1.asSynthetic)
    }
    DesugaredAst.Expr.Scope(regIdent, foreachExp, loc0)
  }

  /**
    * Rewrites a `MonadicFor` loop into a series of `Monad.flatMap` calls.
    * {{{
    *   forM (
    *           x <- xs;
    *           if x > 0;
    *           y <- ys
    *       ) yield x + y
    * }}}
    * desugars to
    * {{{
    *   Monad.flatMap(x -> if (x > 0) Monad.flatMap(y -> Applicative.point(x + y), ys) else MonadZero.empty(), xs)
    * }}}
    */
  private def desugarMonadicFor(frags0: List[WeededAst.ForFragment], exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): Expr = {
    val fqnFlatMap = "Monad.flatMap"
    val fqnPoint = "Applicative.point"
    val fqnZero = "MonadZero.empty"
    val e = visitExp(exp0)
    val yieldExp = mkApplyFqn(fqnPoint, List(e), loc0)
    frags0.foldRight(yieldExp) {
      case (WeededAst.ForFragment.Generator(pat1, exp1, loc1), acc) =>
        val p1 = visitPattern(pat1)
        val e1 = visitExp(exp1)
        val lambda = mkLambdaMatch(p1, acc, loc1)
        val fparams = List(lambda, e1)
        mkApplyFqn(fqnFlatMap, fparams, loc1)

      case (WeededAst.ForFragment.Guard(exp1, loc1), acc) =>
        val e1 = visitExp(exp1)
        val zero = mkApplyFqn(fqnZero, List(DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc1.asSynthetic)), loc1.asSynthetic)
        DesugaredAst.Expr.IfThenElse(e1, acc, zero, loc1.asSynthetic)

      case (WeededAst.ForFragment.Let(pat1, exp1, loc1), acc) =>
        // Rewrite to pattern match
        val p1 = visitPattern(pat1)
        val e1 = visitExp(exp1)
        val matchRule = DesugaredAst.MatchRule(p1, None, acc)
        DesugaredAst.Expr.Match(e1, List(matchRule), loc1.asSynthetic)
    }
  }

  /**
    *
    * Rewrites a `ForEachYield` loop into a series of iterators
    * wrapped in a `Collectable.collect` call:
    * {{{
    * foreach (x <- xs) yield x
    * }}}
    *
    * desugars to
    * {{{
    *     region rc {
    *         Collectable.collect(
    *             Iterator.flatMap(
    *                 match x -> Iterator.singleton(rc, x),
    *                 Iterable.iterator(rc, xs)
    *             )
    *         )
    *     }
    * }}}
    */
  private def desugarForEachYield(frags0: List[WeededAst.ForFragment], exp0: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): Expr = {
    // Declare functions
    val fqnEmpty = "Iterator.empty"
    val fqnSingleton = "Iterator.singleton"
    val fqnFlatMap = "Iterator.flatMap"
    val fqnIterator = "Iterable.iterator"
    val fqnCollect = "Collectable.collect"

    // Make region variable
    val regionSym = "forEachYieldIteratorRegion" + Flix.Delimiter + flix.genSym.freshId()
    val regionIdent = Name.Ident(loc0.sp1, regionSym, loc0.sp2).asSynthetic
    val regionVar = DesugaredAst.Expr.Ambiguous(Name.mkQName(regionIdent), loc0)

    // Desugar yield-exp
    //    ... yield x
    // Becomes
    //     Iterator.singleton(rc, x)
    val e = visitExp(exp0)
    val yieldExp = mkApplyFqn(fqnSingleton, List(regionVar, e), loc0)

    // Desugar loop
    val loop = frags0.foldRight(yieldExp) {
      case (WeededAst.ForFragment.Generator(pat1, exp1, loc1), acc) =>
        // Case 1: a generator fragment i.e. `pat <- exp`
        // This should be desugared into
        //     Iterator.flatMap(match pat -> accExp, Iterator.iterator(exp))
        val p1 = visitPattern(pat1)
        val e1 = visitExp(exp1)

        // 1. Create iterator from exp1
        val iter = mkApplyFqn(fqnIterator, List(regionVar, e1), loc1)

        // 2. Create match-lambda with pat1 as params and acc as body
        val lambda = mkLambdaMatch(p1, acc, loc1)

        // 3. Wrap in flatmap call
        val fparams = List(lambda, iter)
        mkApplyFqn(fqnFlatMap, fparams, loc1)

      case (WeededAst.ForFragment.Guard(exp1, loc1), acc) =>
        // Case 2: a guard fragment i.e. `if exp`
        // This should be desugared into
        //     if (exp) accExp else Iterator.empty(rc)
        val e1 = visitExp(exp1)

        // 1. Create empty iterator
        val empty = mkApplyFqn(fqnEmpty, List(regionVar), loc1)

        // 2. Wrap acc in if-then-else exp: if (exp1) acc else Iterator.empty(empty)
        DesugaredAst.Expr.IfThenElse(e1, acc, empty, loc1)

      case (WeededAst.ForFragment.Let(pat1, exp1, loc1), acc) =>
        // Rewrite to pattern match
        val p1 = visitPattern(pat1)
        val e1 = visitExp(exp1)
        val matchRule = DesugaredAst.MatchRule(p1, None, acc)
        DesugaredAst.Expr.Match(e1, List(matchRule), loc1.asSynthetic)
    }

    // Wrap in Collectable.collect function.
    // The nested calls to Iterator.flatMap are wrapped in
    // this function.
    val resultExp = mkApplyFqn(fqnCollect, List(loop), loc0)

    // Wrap in region
    DesugaredAst.Expr.Scope(regionIdent, resultExp, loc0)
  }

  /**
    * Rewrites a let-match to a regular let-binding or a full pattern match.
    */
  private def desugarLetMatch(pat0: WeededAst.Pattern, mod0: Ast.Modifiers, tpe0: Option[WeededAst.Type], exp1: WeededAst.Expr, exp2: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): Expr = {
    val p = visitPattern(pat0)
    val t = tpe0.map(visitType)
    val e1 = visitExp(exp1)
    val e2 = visitExp(exp2)
    p match {
      case DesugaredAst.Pattern.Var(ident, _) =>
        // No pattern match
        DesugaredAst.Expr.Let(ident, mod0, withAscription(e1, t), e2, loc0)
      case _ =>
        // Full pattern match
        val rule = DesugaredAst.MatchRule(p, None, e2)
        DesugaredAst.Expr.Match(withAscription(e1, t), List(rule), loc0)
    }
  }

  /**
    * Rewrites empty tuples to [[Ast.Constant.Unit]] and eliminate single-element tuples.
    */
  private def desugarTuple(exps0: List[WeededAst.Expr], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val es = visitExps(exps0)
    es match {
      case Nil => DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc0)
      case x :: Nil => x
      case xs => DesugaredAst.Expr.Tuple(xs, loc0)
    }
  }

  /**
    * Rewrites [[WeededAst.Expr.FCons]] (`x :: xs`) into a call to `List.Cons(x, xs)`.
    */
  private def desugarFCons(exp1: WeededAst.Expr, exp2: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val e1 = visitExp(exp1)
    val e2 = visitExp(exp2)
    mkApplyFqn("List.Cons", List(e1, e2), loc0)
  }

  /**
    * Rewrites  [[WeededAst.Expr.FAppend]] (`xs ++ ys`) into a call to `List.append`.
    */
  private def desugarFAppend(exp1: WeededAst.Expr, exp2: WeededAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    // NB: We painstakingly construct the qualified name
    // to ensure that source locations are available.
    val e1 = visitExp(exp1)
    val e2 = visitExp(exp2)
    mkApplyFqn("List.append", List(e1, e2), loc0)
  }

  /**
    * Rewrites [[WeededAst.Expr.ListLit]] (`1 :: 2 :: Nil`) expression into `List.Nil` with `List.Cons`.
    */
  private def desugarListLit(exps0: List[WeededAst.Expr], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val es = visitExps(exps0)
    val nil: Expr = DesugaredAst.Expr.Ambiguous(Name.mkQName("List.Nil"), loc0)
    es.foldRight(nil) {
      case (e, acc) => mkApplyFqn("List.Cons", List(e, acc), loc0)
    }
  }

  /**
    * Rewrites [[WeededAst.Expr.SetLit]] (`Set#{1, 2}`) into `Set.empty` and `Set.insert` calls.
    */
  private def desugarSetLit(exps0: List[WeededAst.Expr], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val es = visitExps(exps0)
    val empty = mkApplyFqn("Set.empty", List(DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc0)), loc0)
    es.foldLeft(empty) {
      case (acc, e) => mkApplyFqn("Set.insert", List(e, acc), loc0)
    }
  }

  /**
    * Rewrites [[WeededAst.Expr.MapLit]] (`Map#{1 => 2, 2 => 3}`) into `Map.empty` and a `Map.insert` calls.
    */
  private def desugarMapLit(exps0: List[(WeededAst.Expr, WeededAst.Expr)], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val empty = mkApplyFqn("Map.empty", List(DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc0)), loc0)
    exps0.map {
      case (k, v) => visitExp(k) -> visitExp(v)
    }.foldLeft(empty) {
      case (acc, (k, v)) => mkApplyFqn("Map.insert", List(k, v, acc), loc0)
    }
  }

  /**
    * Rewrites a [[WeededAst.Expr.FixpointInjectInto]] into a series of injects and merges.
    */
  private def desugarFixpointInjectInto(exps0: List[WeededAst.Expr], idents0: List[Name.Ident], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val es = visitExps(exps0)
    val init = DesugaredAst.Expr.FixpointConstraintSet(Nil, loc0)
    es.zip(idents0).foldRight(init: Expr) {
      case ((exp, ident), acc) =>
        val pred = Name.mkPred(ident)
        val innerExp = DesugaredAst.Expr.FixpointInject(exp, pred, loc0)
        DesugaredAst.Expr.FixpointMerge(innerExp, acc, loc0)
    }
  }

  /**
    * Rewrites a [[WeededAst.Expr.FixpointSolveWithProject]] into a series of solve and merges.
    *
    * E.g.,
    * {{{
    * solve e1, e2, e3 project P1, P2, P3
    * }}}
    * becomes
    * {{{
    *   let tmp%  solve (merge e1, 2, e3);
    *   merge (project P1 tmp%, project P2 tmp%, project P3 tmp%)
    * }}}
    */
  private def desugarFixpointSolveWithProject(exps0: List[WeededAst.Expr], idents0: Option[List[Name.Ident]], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val es = visitExps(exps0)

    // Introduce a tmp% variable that holds the minimal model of the merge of the exps.
    val freshVar = flix.genSym.freshId()
    val localVar = Name.Ident(SourceLocation.Unknown.sp1, s"tmp" + Flix.Delimiter + freshVar, SourceLocation.Unknown.sp2)

    // Merge all the exps into one Datalog program value.
    val mergeExp = es.reduceRight[DesugaredAst.Expr] {
      case (e, acc) => DesugaredAst.Expr.FixpointMerge(e, acc, loc0)
    }
    val modelExp = DesugaredAst.Expr.FixpointSolve(mergeExp, loc0)

    // Any projections?
    val bodyExp = idents0 match {
      case None =>
        // Case 1: No projections: Simply return the minimal model.
        DesugaredAst.Expr.Ambiguous(Name.mkQName(localVar), loc0)

      case Some(idents) =>
        // Case 2: A non-empty sequence of predicate symbols to project.

        // Construct a list of each projection.
        val projectExps = idents.map {
          case ident =>
            val varExp = DesugaredAst.Expr.Ambiguous(Name.mkQName(localVar), loc0)
            DesugaredAst.Expr.FixpointFilter(Name.Pred(ident.name, loc0), varExp, loc0)
        }

        // Merge all of the projections into one result.
        projectExps.reduceRight[DesugaredAst.Expr] {
          case (e, acc) => DesugaredAst.Expr.FixpointMerge(e, acc, loc0)
        }
    }

    // Bind the tmp% variable to the minimal model and combine it with the body expression.
    DesugaredAst.Expr.Let(localVar, Ast.Modifiers.Empty, modelExp, bodyExp, loc0.asReal)
  }

  /**
    * Rewrites a [[WeededAst.Expr.FixpointQueryWithSelect]] into a series of solves and merges.
    *
    * E.g.,
    * {{{
    * query e1, e2, e3 select (x, y, z) from A(x, y), B(z) where x > 0
    * }}}
    * becomes
    * {{{
    *   project out %Result from (solve (merge (merge e1, e2, e3) #{ #Result(x, y, z) :- A(x, y), B(y) if x > 0 } )
    *   merge (project P1 tmp%, project P2 tmp%, project P3 tmp%)
    * }}}
    * OBS: The last merge and solve is done in the typer because of trouble when `(merge e1, e2, e3)` is a closed row.
    */
  private def desugarFixpointQueryWithSelect(exps0: List[WeededAst.Expr], selects0: List[WeededAst.Expr], from0: List[Predicate.Body], where0: List[WeededAst.Expr], loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val exps = visitExps(exps0)
    val selects = visitExps(selects0)
    val from = visitPredicateBodies(from0)
    val where = visitExps(where0)

    // The fresh predicate name where to store the result of the query.
    val pred = Name.Pred(Flix.Delimiter + "Result", loc0)

    // The head of the pseudo-rule.
    val den = Ast.Denotation.Relational
    val head = DesugaredAst.Predicate.Head.Atom(pred, den, selects, loc0)

    // The body of the pseudo-rule.
    val guard = where.map(DesugaredAst.Predicate.Body.Guard(_, loc0))

    // Automatically fix all lattices atoms.
    val body = guard ::: from.map {
      case DesugaredAst.Predicate.Body.Atom(pred, Ast.Denotation.Latticenal, polarity, _, terms, loc) =>
        DesugaredAst.Predicate.Body.Atom(pred, Ast.Denotation.Latticenal, polarity, Ast.Fixity.Fixed, terms, loc)
      case pred => pred
    }

    // Construct the pseudo-query.
    val pseudoConstraint = DesugaredAst.Constraint(head, body, loc0)

    // Construct a constraint set that contains the single pseudo constraint.
    val queryExp = DesugaredAst.Expr.FixpointConstraintSet(List(pseudoConstraint), loc0)

    // Construct the merge of all the expressions.
    val dbExp = exps.reduceRight[Expr] {
      case (e, acc) => DesugaredAst.Expr.FixpointMerge(e, acc, loc0)
    }

    // Extract the tuples of the result predicate.
    DesugaredAst.Expr.FixpointProject(pred, queryExp, dbExp, loc0)
  }

  /**
    * Rewrites a [[WeededAst.Expr.Debug]] into a call to `Debug.debugWithPrefix`.
    */
  private def desugarDebug(exp0: WeededAst.Expr, kind0: WeededAst.DebugKind, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val e = visitExp(exp0)
    val prefix = mkDebugPrefix(e, kind0, loc0)
    val e1 = DesugaredAst.Expr.Cst(Ast.Constant.Str(prefix), loc0)
    val call = mkApplyFqn("Debug.debugWithPrefix", List(e1, e), loc0)
    DesugaredAst.Expr.UncheckedMaskingCast(call, loc0)
  }

  /**
    * Returns a prefix used by `Debug.debugWithPrefix` based on `kind0` and `exp0`.
    */
  private def mkDebugPrefix(exp0: DesugaredAst.Expr, kind0: WeededAst.DebugKind, loc0: SourceLocation): String = kind0 match {
    case WeededAst.DebugKind.Debug => ""
    case WeededAst.DebugKind.DebugWithLoc => s"[${loc0.formatWithLine}] "
    case WeededAst.DebugKind.DebugWithLocAndSrc =>
      val locPart = s"[${loc0.formatWithLine}]"
      val srcPart = exp0.loc.text.map(s => s" $s = ").getOrElse("")
      locPart + srcPart
  }

  /**
    * Returns a match lambda, i.e. a lambda with a pattern match on its arguments.
    *
    * This is also known as `ParsedAst.Expression.LambdaMatch`
    *
    * @param pat0 the pattern of the parameter
    * @param exp0 the body of the lambda
    * @param loc0 the [[SourceLocation]] of the lambda
    * @return A lambda that matches on its parameter i.e. a [[DesugaredAst.Expr.Lambda]] that has a pattern match in its body.
    */
  private def mkLambdaMatch(pat0: DesugaredAst.Pattern, exp0: DesugaredAst.Expr, loc0: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr.Lambda = {
    // The name of the lambda parameter.
    val ident = Name.Ident(loc0.sp1, "pat" + Flix.Delimiter + flix.genSym.freshId(), loc0.sp2).asSynthetic

    // Construct the body of the lambda expression.
    val varOrRef = DesugaredAst.Expr.Ambiguous(Name.mkQName(ident), loc0)
    val rule = DesugaredAst.MatchRule(pat0, None, exp0)

    val fparam = DesugaredAst.FormalParam(ident, Ast.Modifiers.Empty, None, loc0)
    val body = DesugaredAst.Expr.Match(varOrRef, List(rule), loc0)
    DesugaredAst.Expr.Lambda(fparam, body, loc0)
  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args0`.
    */
  private def mkApplyFqn(fqn0: String, args0: List[DesugaredAst.Expr], loc0: SourceLocation): DesugaredAst.Expr = {
    val l = loc0.asSynthetic
    val lambda = DesugaredAst.Expr.Ambiguous(Name.mkQName(fqn0), l)
    DesugaredAst.Expr.Apply(lambda, args0, l)
  }

  /**
    * Returns a curried version of the given expression `exp0` for each formal parameter in `fparams0`.
    */
  private def mkCurried(fparams0: List[DesugaredAst.FormalParam], exp0: DesugaredAst.Expr, loc0: SourceLocation): DesugaredAst.Expr = {
    val l = loc0.asSynthetic
    fparams0.foldRight(exp0) {
      case (fparam, acc) => DesugaredAst.Expr.Lambda(fparam, acc, l)
    }
  }

  /**
    * Returns the given expression `exp0` optionally wrapped in a type ascription if `tpe0` is `Some`.
    */
  private def withAscription(exp0: DesugaredAst.Expr, tpe0: Option[DesugaredAst.Type]): DesugaredAst.Expr = {
    val l = exp0.loc.asSynthetic
    tpe0 match {
      case None => exp0
      case Some(t) => DesugaredAst.Expr.Ascribe(exp0, Some(t), None, l)
    }
  }

  /**
    * Returns the class and member name constructed from the given `fqn0`
    */
  private def splitClassAndMember(fqn0: WeededAst.JavaClassMember): (String, String) = fqn0 match {
    case WeededAst.JavaClassMember(prefix, suffix, _) =>
      // The Parser ensures that suffix is non-empty.
      val className = prefix + "." + suffix.init.mkString(".")
      val memberName = suffix.last
      (className, memberName)
  }
}
