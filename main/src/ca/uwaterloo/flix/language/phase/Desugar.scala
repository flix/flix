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
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, DesugaredAst, Name, SourceLocation, WeededAst}
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

    case d: WeededAst.Declaration.Class => visitClass(d)
    case d: WeededAst.Declaration.Instance => visitInstance(d)
    case d: WeededAst.Declaration.Def => visitDef(d)
    case d: WeededAst.Declaration.Law => visitLaw(d)
    case d: WeededAst.Declaration.Enum => visitEnum(d)
    case d: WeededAst.Declaration.RestrictableEnum => visitRestrictableEnum(d)
    case d: WeededAst.Declaration.TypeAlias => visitTypeAlias(d)
    case d: WeededAst.Declaration.Effect => visitEffect(d)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Class]] `class0`.
    */
  private def visitClass(class0: WeededAst.Declaration.Class)(implicit flix: Flix): DesugaredAst.Declaration.Class = class0 match {
    case WeededAst.Declaration.Class(doc, ann, mod, ident, tparam0, superClasses0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superClasses = superClasses0.map(visitTypeConstraint)
      val assocs = assocs0.map(visitAssocTypeSig)
      val sigs = sigs0.map(visitSig)
      val laws = laws0.map(visitDef)
      DesugaredAst.Declaration.Class(doc, ann, mod, ident, tparam, superClasses, assocs, sigs, laws, loc)
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
    case WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams0, tpe0, loc) =>
      val tparams = visitTypeParams(tparams0)
      val tpe = visitType(tpe0)
      DesugaredAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, loc)
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
    case WeededAst.Declaration.AssocTypeSig(doc, mod, ident, tparam0, kind0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val kind = visitKind(kind0)
      DesugaredAst.Declaration.AssocTypeSig(doc, mod, ident, tparam, kind, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Sig]] `sig0`.
    */
  private def visitSig(sig0: WeededAst.Declaration.Sig)(implicit flix: Flix): DesugaredAst.Declaration.Sig = sig0 match {
    case WeededAst.Declaration.Sig(doc, ann, mod, ident, tparams0, fparams0, exp0, tpe0, eff0, tconstrs0, loc) =>
      val tparams = visitKindedTypeParams(tparams0)
      val fparams = visitFormalParams(fparams0)
      val exp = exp0.map(visitExp)
      val tpe = visitType(tpe0)
      val eff = eff0.map(visitType)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      DesugaredAst.Declaration.Sig(doc, ann, mod, ident, tparams, fparams, exp, tpe, eff, tconstrs, loc)
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
  }

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
    case WeededAst.Derivations(classes, loc) =>
      DesugaredAst.Derivations(classes, loc)
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

    case WeededAst.Expr.Region(tpe, loc) =>
      Expr.Region(tpe, loc)

    case WeededAst.Expr.Scope(ident, exp, loc) =>
      val e = visitExp(exp)
      Expr.Scope(ident, e, loc)

    case WeededAst.Expr.ScopeExit(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.ScopeExit(e1, e2, loc)

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
      val es = visitExps(exps)
      es match {
        case Nil => DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc)
        case x :: Nil => x
        case xs => DesugaredAst.Expr.Tuple(xs, loc)
      }

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

    case WeededAst.Expr.ListLit(exps, loc) =>
      // Rewrites a `FList` expression into `List.Nil` with `List.Cons`.
      val es = visitExps(exps)
      val nil: DesugaredAst.Expr = DesugaredAst.Expr.Ambiguous(Name.mkQName("List.Nil"), loc)
      es.foldRight(nil) {
        case (e, acc) => mkApplyFqn("List.Cons", List(e, acc), loc)
      }

    case WeededAst.Expr.SetLit(exps, loc) =>
      // Rewrites a `FSet` expression into `Set/empty` and a `Set/insert` calls.
      val es = visitExps(exps)
      val empty = mkApplyFqn("Set.empty", List(DesugaredAst.Expr.Cst(Ast.Constant.Unit, loc)), loc)
      es.foldLeft(empty) {
        case (acc, e) => mkApplyFqn("Set.insert", List(e, acc), loc)
      }

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

    case WeededAst.Expr.TryWith(exp, eff, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitHandlerRule)
      Expr.TryWith(e, eff, rs, loc)

    case WeededAst.Expr.Do(op, exps, loc) =>
      val es = visitExps(exps)
      Expr.Do(op, es, loc)

    case WeededAst.Expr.Resume(exp, loc) =>
      val e = visitExp(exp)
      Expr.Resume(e, loc)

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

    case WeededAst.Expr.FixpointProject(pred, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.FixpointProject(pred, e1, e2, loc)

    case WeededAst.Expr.Error(m) =>
      DesugaredAst.Expr.Error(m)
  }

  /**
    * Desugars the given list of [[WeededAst.Expr]] `exps`.
    */
  private def visitExps(exps: List[WeededAst.Expr])(implicit flix: Flix): List[DesugaredAst.Expr] =
    exps.map(visitExp)

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

    def visitBody(body0: WeededAst.Predicate.Body)(implicit flix: Flix): DesugaredAst.Predicate.Body = body0 match {
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

    constraint0 match {
      case WeededAst.Constraint(head, body, loc) =>
        val h = visitHead(head)
        val b = body.map(visitBody)
        DesugaredAst.Constraint(h, b, loc)
    }
  }

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
  private def desugarInfix(exp1: WeededAst.Expr, exp2: WeededAst.Expr, exp3: WeededAst.Expr, loc: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr.Apply = {
    val e1 = visitExp(exp1)
    val e2 = visitExp(exp2)
    val e3 = visitExp(exp3)
    Expr.Apply(e2, List(e1, e3), loc)
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
  private def desugarApplicativeFor(frags: List[WeededAst.ForFragment.Generator], exp: WeededAst.Expr, loc: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val fqnAp = "Applicative.ap"
    val fqnMap = "Functor.map"
    val yieldExp = visitExp(exp)

    // Make lambda for Functor.map(lambda, ...). This lambda uses all patterns from the for-fragments.
    val lambda = frags.foldRight(yieldExp) {
      case (WeededAst.ForFragment.Generator(pat, _, loc1), acc) =>
        val p = visitPattern(pat)
        mkLambdaMatch(p, acc, loc1)
    }

    // Apply first fragment to Functor.map
    val xs = visitExp(frags.head.exp)
    val baseExp = mkApplyFqn(fqnMap, List(lambda, xs), loc)

    // Apply rest of fragments to Applicative.ap
    frags.tail.foldLeft(baseExp) {
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
  private def desugarForEach(frags: List[WeededAst.ForFragment], exp: WeededAst.Expr, loc: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr = {
    val fqnForEach = "Iterator.forEach"
    val fqnIterator = "Iterable.iterator"
    val regIdent = Name.Ident(loc.sp1, "reg" + Flix.Delimiter + flix.genSym.freshId(), loc.sp2).asSynthetic
    val regVar = DesugaredAst.Expr.Ambiguous(Name.mkQName(regIdent), loc.asSynthetic)

    val foreachExp = frags.foldRight(visitExp(exp)) {
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
    }
    DesugaredAst.Expr.Scope(regIdent, foreachExp, loc)
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
  private def desugarMonadicFor(frags: List[WeededAst.ForFragment], exp: WeededAst.Expr, loc: SourceLocation)(implicit flix: Flix): Expr = {
    val fqnFlatMap = "Monad.flatMap"
    val fqnPoint = "Applicative.point"
    val fqnZero = "MonadZero.empty"
    val e = visitExp(exp)
    val yieldExp = mkApplyFqn(fqnPoint, List(e), loc)
    frags.foldRight(yieldExp) {
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
    }
  }

  /**
    *
    * Rewrites a `ForEachYield` loop into a series of iterators
    * wrapped in a `Collectable.collect`` call:
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
  private def desugarForEachYield(frags: List[WeededAst.ForFragment], exp: WeededAst.Expr, loc: SourceLocation)(implicit flix: Flix): Expr = {
    // Declare functions
    val fqnEmpty = "Iterator.empty"
    val fqnSingleton = "Iterator.singleton"
    val fqnFlatMap = "Iterator.flatMap"
    val fqnIterator = "Iterable.iterator"
    val fqnCollect = "Collectable.collect"

    // Make region variable
    val regionSym = "forEachYieldIteratorRegion" + Flix.Delimiter + flix.genSym.freshId()
    val regionIdent = Name.Ident(loc.sp1, regionSym, loc.sp2).asSynthetic
    val regionVar = DesugaredAst.Expr.Ambiguous(Name.mkQName(regionIdent), loc)

    // Desugar yield-exp
    //    ... yield x
    // Becomes
    //     Iterator.singleton(rc, x)
    val e = visitExp(exp)
    val yieldExp = mkApplyFqn(fqnSingleton, List(regionVar, e), loc)

    // Desugar loop
    val loop = frags.foldRight(yieldExp) {
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
    }

    // Wrap in Collectable.collect function.
    // The nested calls to Iterator.flatMap are wrapped in
    // this function.
    val resultExp = mkApplyFqn(fqnCollect, List(loop), loc)

    // Wrap in region
    DesugaredAst.Expr.Scope(regionIdent, resultExp, loc)
  }

  /**
    * Rewrites a let-match to a regular let-binding or a full pattern match.
    */
  private def desugarLetMatch(pat: WeededAst.Pattern, mod: Ast.Modifiers, tpe: Option[WeededAst.Type], exp1: WeededAst.Expr, exp2: WeededAst.Expr, loc: SourceLocation)(implicit flix: Flix): Expr = {
    val p = visitPattern(pat)
    val t = tpe.map(visitType)
    val e1 = visitExp(exp1)
    val e2 = visitExp(exp2)
    p match {
      case DesugaredAst.Pattern.Var(ident, _) =>
        // No pattern match
        DesugaredAst.Expr.Let(ident, mod, withAscription(e1, t), e2, loc)
      case _ =>
        // Full pattern match
        val rule = DesugaredAst.MatchRule(p, None, e2)
        DesugaredAst.Expr.Match(withAscription(e1, t), List(rule), loc)
    }
  }

  /**
    * Returns a match lambda, i.e. a lambda with a pattern match on its arguments.
    *
    * This is also known as `ParsedAst.Expression.LambdaMatch`
    *
    * @param p the pattern of the parameter
    * @param e the body of the lambda
    * @return A lambda that matches on its parameter i.e. a [[DesugaredAst.Expr.Lambda]] that has a pattern match in its body.
    */
  private def mkLambdaMatch(p: DesugaredAst.Pattern, e: DesugaredAst.Expr, loc: SourceLocation)(implicit flix: Flix): DesugaredAst.Expr.Lambda = {
    // The name of the lambda parameter.
    val ident = Name.Ident(loc.sp1, "pat" + Flix.Delimiter + flix.genSym.freshId(), loc.sp2).asSynthetic

    // Construct the body of the lambda expression.
    val varOrRef = DesugaredAst.Expr.Ambiguous(Name.mkQName(ident), loc)
    val rule = DesugaredAst.MatchRule(p, None, e)

    val fparam = DesugaredAst.FormalParam(ident, Ast.Modifiers.Empty, None, loc)
    val body = DesugaredAst.Expr.Match(varOrRef, List(rule), loc)
    DesugaredAst.Expr.Lambda(fparam, body, loc)
  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args`.
    */
  private def mkApplyFqn(fqn: String, args: List[DesugaredAst.Expr], loc: SourceLocation): DesugaredAst.Expr = {
    val l = loc.asSynthetic
    val lambda = DesugaredAst.Expr.Ambiguous(Name.mkQName(fqn), l)
    DesugaredAst.Expr.Apply(lambda, args, l)
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
}
