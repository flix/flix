package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.DesugaredAst.Expr
import ca.uwaterloo.flix.language.ast.{Ast, DesugaredAst, WeededAst}
import ca.uwaterloo.flix.util.ParOps

object Desugar {

  /**
    * Performs desugaring on `program`.
    */
  def run(program: WeededAst.Root)(implicit flix: Flix): DesugaredAst.Root = flix.phase("Desugar") {
    val units = ParOps.parMap(program.units) {
      case (k, v) => visitUnit(k, v)
    }

    val allUnits = units.foldLeft(Map.empty[Ast.Source, DesugaredAst.CompilationUnit]) {
      case (macc, (k, v)) => macc + (k -> v)
    }

    DesugaredAst.Root(allUnits, program.entryPoint, program.names)
  }

  /**
    * Desugars the given [[WeededAst.CompilationUnit]] `unit`.
    */
  private def visitUnit(src: Ast.Source, unit: WeededAst.CompilationUnit)(implicit flix: Flix): (Ast.Source, DesugaredAst.CompilationUnit) = unit match {
    case WeededAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val decls = decls0.map(visitDecl)
      src -> DesugaredAst.CompilationUnit(usesAndImports, decls, loc)
  }

  /**
    * Maps `useOrImport0` to a corresponding [[DesugaredAst.UseOrImport]].
    */
  private def visitUseOrImport(useOrImport0: WeededAst.UseOrImport): DesugaredAst.UseOrImport = useOrImport0 match {
    case WeededAst.UseOrImport.Use(qname, alias, loc) => DesugaredAst.UseOrImport.Use(qname, alias, loc)
    case WeededAst.UseOrImport.Import(name, alias, loc) => DesugaredAst.UseOrImport.Import(name, alias, loc)
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
  private def visitEnum(enum0: WeededAst.Declaration.Enum)(implicit flix: Flix): DesugaredAst.Declaration.Enum = enum0 match {
    case WeededAst.Declaration.Enum(doc, ann, mod, ident, tparams0, derives0, cases0, loc) =>
      val tparams = visitTypeParams(tparams0)
      val derives = visitDerivations(derives0)
      val cases = cases0.map(visitCase)
      DesugaredAst.Declaration.Enum(doc, ann, mod, ident, tparams, derives, cases, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.RestrictableEnum]] `restrictableEnum0`.
    */
  private def visitRestrictableEnum(restrictableEnum0: WeededAst.Declaration.RestrictableEnum)(implicit flix: Flix): DesugaredAst.Declaration.RestrictableEnum = restrictableEnum0 match {
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
  private def visitTypeAlias(typeAlias0: WeededAst.Declaration.TypeAlias)(implicit flix: Flix): DesugaredAst.Declaration.TypeAlias = typeAlias0 match {
    case WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams0, tpe0, loc) =>
      val tparams = visitTypeParams(tparams0)
      val tpe = visitType(tpe0)
      DesugaredAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Effect]] `eff0`.
    */
  private def visitEffect(eff0: WeededAst.Declaration.Effect)(implicit flix: Flix): DesugaredAst.Declaration.Effect = eff0 match {
    case WeededAst.Declaration.Effect(doc, ann, mod, ident, ops0, loc) =>
      val ops = ops0.map(visitOp)
      DesugaredAst.Declaration.Effect(doc, ann, mod, ident, ops, loc)
  }

  /**
    * Desugars the given [[WeededAst.TypeParam]] `tparam0`.
    */
  private def visitTypeParam(tparam0: WeededAst.TypeParam)(implicit flix: Flix): DesugaredAst.TypeParam = tparam0 match {
    case WeededAst.TypeParam.Unkinded(ident) => DesugaredAst.TypeParam.Unkinded(ident)
    case WeededAst.TypeParam.Kinded(ident, kind0) =>
      val kind = visitKind(kind0)
      DesugaredAst.TypeParam.Kinded(ident, kind)
  }

  /**
    * Desugars the given [[WeededAst.TypeConstraint]] `tconstr0`.
    */
  private def visitTypeConstraint(tconstr0: WeededAst.TypeConstraint)(implicit flix: Flix): DesugaredAst.TypeConstraint = tconstr0 match {
    case WeededAst.TypeConstraint(clazz, tpe0, loc) =>
      val tpe = visitType(tpe0)
      DesugaredAst.TypeConstraint(clazz, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.AssocTypeSig]] `assoc0`.
    */
  private def visitAssocTypeSig(assoc0: WeededAst.Declaration.AssocTypeSig)(implicit flix: Flix): DesugaredAst.Declaration.AssocTypeSig = assoc0 match {
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
  private def visitType(tpe0: WeededAst.Type)(implicit flix: Flix): DesugaredAst.Type = tpe0 match {
    case WeededAst.Type.Var(ident, loc) => DesugaredAst.Type.Var(ident, loc)
    case WeededAst.Type.Ambiguous(qname, loc) => DesugaredAst.Type.Ambiguous(qname, loc)
    case WeededAst.Type.Unit(loc) => DesugaredAst.Type.Unit(loc)
    case WeededAst.Type.Tuple(elms, loc) => DesugaredAst.Type.Tuple(elms.map(visitType), loc)
    case WeededAst.Type.RecordRowEmpty(loc) => DesugaredAst.Type.RecordRowEmpty(loc)
    case WeededAst.Type.RecordRowExtend(label, tpe, rest, loc) =>
      DesugaredAst.Type.RecordRowExtend(label, visitType(tpe), visitType(rest), loc)

    case WeededAst.Type.Record(row, loc) => DesugaredAst.Type.Record(visitType(row), loc)
    case WeededAst.Type.SchemaRowEmpty(loc) => DesugaredAst.Type.SchemaRowEmpty(loc)
    case WeededAst.Type.SchemaRowExtendByAlias(qname, targs, rest, loc) =>
      DesugaredAst.Type.SchemaRowExtendByAlias(qname, targs.map(visitType), visitType(rest), loc)

    case WeededAst.Type.SchemaRowExtendByTypes(name, den, tpes, rest, loc) =>
      DesugaredAst.Type.SchemaRowExtendByTypes(name, den, tpes.map(visitType), visitType(rest), loc)

    case WeededAst.Type.Schema(row, loc) => DesugaredAst.Type.Schema(visitType(row), loc)
    case WeededAst.Type.Native(fqn, loc) => DesugaredAst.Type.Native(fqn, loc)
    case WeededAst.Type.Arrow(tparams, eff, tresult, loc) =>
      DesugaredAst.Type.Arrow(tparams.map(visitType), eff.map(visitType), visitType(tresult), loc)

    case WeededAst.Type.Apply(tpe1, tpe2, loc) => DesugaredAst.Type.Apply(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.True(loc) => DesugaredAst.Type.True(loc)
    case WeededAst.Type.False(loc) => DesugaredAst.Type.False(loc)
    case WeededAst.Type.Not(tpe, loc) => DesugaredAst.Type.Not(visitType(tpe), loc)
    case WeededAst.Type.And(tpe1, tpe2, loc) => DesugaredAst.Type.And(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.Or(tpe1, tpe2, loc) => DesugaredAst.Type.Or(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.Complement(tpe, loc) => DesugaredAst.Type.Complement(visitType(tpe), loc)
    case WeededAst.Type.Union(tpe1, tpe2, loc) => DesugaredAst.Type.Union(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.Intersection(tpe1, tpe2, loc) =>
      DesugaredAst.Type.Intersection(visitType(tpe1), visitType(tpe2), loc)

    case WeededAst.Type.Pure(loc) => DesugaredAst.Type.Pure(loc)
    case WeededAst.Type.CaseSet(cases, loc) => DesugaredAst.Type.CaseSet(cases, loc)
    case WeededAst.Type.CaseUnion(tpe1, tpe2, loc) => DesugaredAst.Type.CaseUnion(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
      DesugaredAst.Type.CaseIntersection(visitType(tpe1), visitType(tpe2), loc)

    case WeededAst.Type.CaseComplement(tpe, loc) => DesugaredAst.Type.CaseComplement(visitType(tpe), loc)
    case WeededAst.Type.Ascribe(tpe, kind, loc) => DesugaredAst.Type.Ascribe(visitType(tpe), visitKind(kind), loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.AssocTypeDef]] `assoc0`.
    */
  private def visitAssocTypeDef(assoc0: WeededAst.Declaration.AssocTypeDef)(implicit flix: Flix): DesugaredAst.Declaration.AssocTypeDef = assoc0 match {
    case WeededAst.Declaration.AssocTypeDef(doc, mod, ident, arg0, tpe0, loc) =>
      val arg = visitType(arg0)
      val tpe = visitType(tpe0)
      DesugaredAst.Declaration.AssocTypeDef(doc, mod, ident, arg, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.KindedTypeParams]] `tparams0`.
    */
  private def visitKindedTypeParams(tparams0: WeededAst.KindedTypeParams)(implicit flix: Flix): DesugaredAst.KindedTypeParams = tparams0 match {
    case WeededAst.TypeParams.Elided => DesugaredAst.TypeParams.Elided
    case WeededAst.TypeParams.Kinded(tparams1) =>
      val tparams = tparams1.map(visitTypeParam).collect { case t: DesugaredAst.TypeParam.Kinded => t }
      DesugaredAst.TypeParams.Kinded(tparams)
  }

  /**
    * Desugars the given list of [[WeededAst.FormalParam]] `fparams0`.
    */
  private def visitFormalParams(fparams0: List[WeededAst.FormalParam])(implicit flix: Flix): List[DesugaredAst.FormalParam] =
    fparams0.map(visitFormalParam)

  /**
    * Desugars the given [[WeededAst.FormalParam]] `fparam0`.
    */
  private def visitFormalParam(fparam0: WeededAst.FormalParam)(implicit flix: Flix): DesugaredAst.FormalParam = fparam0 match {
    case WeededAst.FormalParam(ident, mod, tpe0, loc) =>
      val tpe = tpe0.map(visitType)
      DesugaredAst.FormalParam(ident, mod, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.EqualityConstraint]] `econstr0`.
    */
  private def visitEqualityConstraint(econstr0: WeededAst.EqualityConstraint)(implicit flix: Flix): DesugaredAst.EqualityConstraint = econstr0 match {
    case WeededAst.EqualityConstraint(qname, tpe01, tpe02, loc) =>
      val tpe1 = visitType(tpe01)
      val tpe2 = visitType(tpe02)
      DesugaredAst.EqualityConstraint(qname, tpe1, tpe2, loc)
  }

  /**
    * Desugars the given [[WeededAst.TypeParams]] `tparams0`.
    */
  private def visitTypeParams(tparams0: WeededAst.TypeParams)(implicit flix: Flix): DesugaredAst.TypeParams = tparams0 match {
    case params: WeededAst.KindedTypeParams => visitKindedTypeParams(params)
    case WeededAst.TypeParams.Unkinded(tparams1) =>
      val tparams = tparams1.map(visitTypeParam).collect { case t: DesugaredAst.TypeParam.Unkinded => t }
      DesugaredAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Desugars the given [[WeededAst.Derivations]] `derives0`.
    */
  private def visitDerivations(derives0: WeededAst.Derivations): DesugaredAst.Derivations = derives0 match {
    case WeededAst.Derivations(classes, loc) => DesugaredAst.Derivations(classes, loc)
  }

  /**
    * Desugars the given [[WeededAst.Case]] `case0`.
    */
  private def visitCase(case0: WeededAst.Case)(implicit flix: Flix): DesugaredAst.Case = case0 match {
    case WeededAst.Case(ident, tpe0, loc) =>
      val tpe = visitType(tpe0)
      DesugaredAst.Case(ident, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.RestrictableCase]] `case0`.
    */
  private def visitRestrictableCase(case0: WeededAst.RestrictableCase)(implicit flix: Flix): DesugaredAst.RestrictableCase = case0 match {
    case WeededAst.RestrictableCase(ident, tpe0, loc) =>
      val tpe = visitType(tpe0)
      DesugaredAst.RestrictableCase(ident, tpe, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Op]] `op0`.
    */
  private def visitOp(op0: WeededAst.Declaration.Op)(implicit flix: Flix): DesugaredAst.Declaration.Op = op0 match {
    case WeededAst.Declaration.Op(doc, ann, mod, ident, fparams0, tpe0, tconstrs0, loc) =>
      val fparams = visitFormalParams(fparams0)
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTypeConstraint)
      DesugaredAst.Declaration.Op(doc, ann, mod, ident, fparams, tpe, tconstrs, loc)
  }

  /**
    * Desugars the given [[WeededAst.Kind]] `kind0`.
    */
  private def visitKind(kind0: WeededAst.Kind)(implicit flix: Flix): DesugaredAst.Kind = kind0 match {
    case WeededAst.Kind.Ambiguous(qname, loc) => DesugaredAst.Kind.Ambiguous(qname, loc)
    case WeededAst.Kind.Arrow(k1, k2, loc) => DesugaredAst.Kind.Arrow(visitKind(k1), visitKind(k2), loc)
  }

  /**
    * Desugars the given [[WeededAst.Expr]] `exp0`.
    */
  private def visitExp(exp0: WeededAst.Expr)(implicit flix: Flix): DesugaredAst.Expr = exp0 match {
    case WeededAst.Expr.Ambiguous(qname, loc) => Expr.Ambiguous(qname, loc)
    case WeededAst.Expr.Open(qname, loc) => Expr.Open(qname, loc)
    case WeededAst.Expr.OpenAs(qname, exp, loc) => Expr.OpenAs(qname, visitExp(exp), loc)
    case WeededAst.Expr.Hole(name, loc) => Expr.Hole(name, loc)
    case WeededAst.Expr.HoleWithExp(exp, loc) => Expr.HoleWithExp(visitExp(exp), loc)
    case WeededAst.Expr.Use(uses, exp, loc) => Expr.Use(uses.map(visitUseOrImport), visitExp(exp), loc)
    case WeededAst.Expr.Cst(cst, loc) => Expr.Cst(cst, loc)
    case WeededAst.Expr.Apply(exp, exps, loc) => Expr.Apply(visitExp(exp), visitExps(exps), loc)
    case WeededAst.Expr.Lambda(fparam, exp, loc) => Expr.Lambda(visitFormalParam(fparam), visitExp(exp), loc)
    case WeededAst.Expr.Unary(sop, exp, loc) => Expr.Unary(sop, visitExp(exp), loc)
    case WeededAst.Expr.Binary(sop, exp1, exp2, loc) => Expr.Binary(sop, visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.IfThenElse(exp1, exp2, exp3, loc) => Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), loc)
    case WeededAst.Expr.Stm(exp1, exp2, loc) => Expr.Stm(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.Discard(exp, loc) => Expr.Discard(visitExp(exp), loc)
    case WeededAst.Expr.Let(ident, mod, exp1, exp2, loc) => Expr.Let(ident, mod, visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.LetRec(ident, mod, exp1, exp2, loc) => Expr.LetRec(ident, mod, visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.Region(tpe, loc) => Expr.Region(tpe, loc)
    case WeededAst.Expr.Scope(ident, exp, loc) => Expr.Scope(ident, visitExp(exp), loc)
    case WeededAst.Expr.ScopeExit(exp1, exp2, loc) => Expr.ScopeExit(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.Match(exp, rules, loc) => Expr.Match(visitExp(exp), rules.map(visitMatchRule), loc)
    case WeededAst.Expr.TypeMatch(exp, rules, loc) => Expr.TypeMatch(visitExp(exp), rules.map(visitTypeMatchRule), loc)
    case WeededAst.Expr.RestrictableChoose(star, exp, rules, loc) =>
      Expr.RestrictableChoose(star, visitExp(exp), rules.map(visitRestrictableChooseRule), loc)

    case WeededAst.Expr.Tuple(exps, loc) => Expr.Tuple(visitExps(exps), loc)
    case WeededAst.Expr.RecordEmpty(loc) => Expr.RecordEmpty(loc)
    case WeededAst.Expr.RecordSelect(exp, label, loc) => Expr.RecordSelect(visitExp(exp), label, loc)
    case WeededAst.Expr.RecordExtend(label, exp1, exp2, loc) => Expr.RecordExtend(label, visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.RecordRestrict(label, exp, loc) => Expr.RecordRestrict(label, visitExp(exp), loc)
    case WeededAst.Expr.ArrayLit(exps, exp, loc) => Expr.ArrayLit(visitExps(exps), visitExp(exp), loc)
    case WeededAst.Expr.ArrayNew(exp1, exp2, exp3, loc) => Expr.ArrayNew(visitExp(exp1), visitExp(exp2), visitExp(exp3), loc)
    case WeededAst.Expr.ArrayLoad(exp1, exp2, loc) => Expr.ArrayLoad(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.ArrayLength(exp, loc) => Expr.ArrayLength(visitExp(exp), loc)
    case WeededAst.Expr.ArrayStore(exp1, exp2, exp3, loc) => Expr.ArrayStore(visitExp(exp1), visitExp(exp2), visitExp(exp3), loc)
    case WeededAst.Expr.VectorLit(exps, loc) => Expr.VectorLit(visitExps(exps), loc)
    case WeededAst.Expr.VectorLoad(exp1, exp2, loc) => Expr.VectorLoad(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.VectorLength(exp, loc) => Expr.VectorLength(visitExp(exp), loc)
    case WeededAst.Expr.Ref(exp1, exp2, loc) => Expr.Ref(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.Deref(exp, loc) => Expr.Deref(visitExp(exp), loc)
    case WeededAst.Expr.Assign(exp1, exp2, loc) => Expr.Assign(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.Ascribe(exp, expectedType, expectedEff, loc) =>
      Expr.Ascribe(visitExp(exp), expectedType.map(visitType), expectedEff.map(visitType), loc)

    case WeededAst.Expr.InstanceOf(exp, className, loc) => Expr.InstanceOf(visitExp(exp), className, loc)
    case WeededAst.Expr.CheckedCast(cast, exp, loc) => Expr.CheckedCast(cast, visitExp(exp), loc)
    case WeededAst.Expr.UncheckedCast(exp, declaredType, declaredEff, loc) =>
      Expr.UncheckedCast(visitExp(exp), declaredType.map(visitType), declaredEff.map(visitType), loc)

    case WeededAst.Expr.UncheckedMaskingCast(exp, loc) => Expr.UncheckedMaskingCast(visitExp(exp), loc)
    case WeededAst.Expr.Without(exp, eff, loc) => Expr.Without(visitExp(exp), eff, loc)
    case WeededAst.Expr.TryCatch(exp, rules, loc) => Expr.TryCatch(visitExp(exp), rules.map(visitCatchRule), loc)
    case WeededAst.Expr.TryWith(exp, eff, rules, loc) => Expr.TryWith(visitExp(exp), eff, rules.map(visitHandlerRule), loc)
    case WeededAst.Expr.Do(op, exps, loc) => Expr.Do(op, visitExps(exps), loc)
    case WeededAst.Expr.Resume(exp, loc) => Expr.Resume(visitExp(exp), loc)
    case WeededAst.Expr.InvokeConstructor(className, exps, sig, loc) =>
      Expr.InvokeConstructor(className, visitExps(exps), sig.map(visitType), loc)

    case WeededAst.Expr.InvokeMethod(className, methodName, exp, exps, sig, retTpe, loc) =>
      Expr.InvokeMethod(className, methodName, visitExp(exp), visitExps(exps), sig.map(visitType), visitType(retTpe), loc)

    case WeededAst.Expr.InvokeStaticMethod(className, methodName, exps, sig, retTpe, loc) =>
      Expr.InvokeStaticMethod(className, methodName, visitExps(exps), sig.map(visitType), visitType(retTpe), loc)

    case WeededAst.Expr.GetField(className, fieldName, exp, loc) =>
      Expr.GetField(className, fieldName, visitExp(exp), loc)

    case WeededAst.Expr.PutField(className, fieldName, exp1, exp2, loc) =>
      Expr.PutField(className, fieldName, visitExp(exp1), visitExp(exp2), loc)

    case WeededAst.Expr.GetStaticField(className, fieldName, loc) => Expr.GetStaticField(className, fieldName, loc)
    case WeededAst.Expr.PutStaticField(className, fieldName, exp, loc) =>
      Expr.PutStaticField(className, fieldName, visitExp(exp), loc)

    case WeededAst.Expr.NewObject(tpe, methods, loc) =>
      Expr.NewObject(visitType(tpe), methods.map(visitJvmMethod), loc)

    case WeededAst.Expr.NewChannel(exp1, exp2, loc) => Expr.NewChannel(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.GetChannel(exp, loc) => Expr.GetChannel(visitExp(exp), loc)
    case WeededAst.Expr.PutChannel(exp1, exp2, loc) => Expr.PutChannel(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.SelectChannel(rules, exp, loc) =>
      Expr.SelectChannel(rules.map(visitSelectChannelRule), exp.map(visitExp), loc)

    case WeededAst.Expr.Spawn(exp1, exp2, loc) => Expr.Spawn(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.ParYield(frags, exp, loc) =>
      Expr.ParYield(frags.map(visitParYieldFragment), visitExp(exp), loc)

    case WeededAst.Expr.Lazy(exp, loc) => Expr.Lazy(visitExp(exp), loc)
    case WeededAst.Expr.Force(exp, loc) => Expr.Force(visitExp(exp), loc)
    case WeededAst.Expr.FixpointConstraintSet(cs, loc) => Expr.FixpointConstraintSet(cs.map(visitConstraint), loc)
    case WeededAst.Expr.FixpointLambda(pparams, exp, loc) =>
      Expr.FixpointLambda(pparams.map(visitPredicateParam), visitExp(exp), loc)

    case WeededAst.Expr.FixpointMerge(exp1, exp2, loc) => Expr.FixpointMerge(visitExp(exp1), visitExp(exp2), loc)
    case WeededAst.Expr.FixpointSolve(exp, loc) => Expr.FixpointSolve(visitExp(exp), loc)
    case WeededAst.Expr.FixpointFilter(pred, exp, loc) => Expr.FixpointFilter(pred, visitExp(exp), loc)
    case WeededAst.Expr.FixpointInject(exp, pred, loc) => Expr.FixpointInject(visitExp(exp), pred, loc)
    case WeededAst.Expr.FixpointProject(pred, exp1, exp2, loc) =>
      Expr.FixpointProject(pred, visitExp(exp1), visitExp(exp2), loc)

    case WeededAst.Expr.Error(m) => DesugaredAst.Expr.Error(m)
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
    case WeededAst.MatchRule(pat, exp1, exp2) => DesugaredAst.MatchRule(visitPattern(pat), exp1.map(visitExp), visitExp(exp2))
  }

  /**
    * Desugars the given [[WeededAst.Pattern]] `pat0`.
    */
  private def visitPattern(pat0: WeededAst.Pattern)(implicit flix: Flix): DesugaredAst.Pattern = ???

  /**
    * Desugars the given [[WeededAst.TypeMatchRule]] `rule0`.
    */
  private def visitTypeMatchRule(rule0: WeededAst.TypeMatchRule)(implicit flix: Flix): DesugaredAst.TypeMatchRule = rule0 match {
    case WeededAst.TypeMatchRule(ident, tpe, exp) => DesugaredAst.TypeMatchRule(ident, visitType(tpe), visitExp(exp))
  }

  /**
    * Desugars the given [[WeededAst.RestrictableChooseRule]] `rule0`.
    */
  private def visitRestrictableChooseRule(rule0: WeededAst.RestrictableChooseRule)(implicit flix: Flix): DesugaredAst.RestrictableChooseRule = rule0 match {
    case WeededAst.RestrictableChooseRule(pat, exp) => DesugaredAst.RestrictableChooseRule(visitRestrictableChoosePattern(pat), visitExp(exp))
  }

  /**
    * Desugars the given [[WeededAst.RestrictableChoosePattern]] `pat0`.
    */
  private def visitRestrictableChoosePattern(pat0: WeededAst.RestrictableChoosePattern)(implicit flix: Flix): DesugaredAst.RestrictableChoosePattern = {
    def visitVarOrWild(varOrWild0: WeededAst.RestrictableChoosePattern.VarOrWild): DesugaredAst.RestrictableChoosePattern.VarOrWild =
      varOrWild0 match {
        case WeededAst.RestrictableChoosePattern.Wild(loc) => DesugaredAst.RestrictableChoosePattern.Wild(loc)
        case WeededAst.RestrictableChoosePattern.Var(ident, loc) => DesugaredAst.RestrictableChoosePattern.Var(ident, loc)
      }

    pat0 match {
      case WeededAst.RestrictableChoosePattern.Tag(qname, pat, loc) => DesugaredAst.RestrictableChoosePattern.Tag(qname, pat.map(visitVarOrWild), loc)
    }
  }

  /**
    * Desugars the given [[WeededAst.CatchRule]] `rule0`.
    */
  private def visitCatchRule(rule0: WeededAst.CatchRule)(implicit flix: Flix): DesugaredAst.CatchRule = rule0 match {
    case WeededAst.CatchRule(ident, className, exp) => DesugaredAst.CatchRule(ident, className, visitExp(exp))
  }

  /**
    * Desugars the given [[WeededAst.HandlerRule]] `rule0`.
    */
  private def visitHandlerRule(rule0: WeededAst.HandlerRule)(implicit flix: Flix): DesugaredAst.HandlerRule = rule0 match {
    case WeededAst.HandlerRule(op, fparams, exp) => DesugaredAst.HandlerRule(op, visitFormalParams(fparams), visitExp(exp))
  }

  /**
    * Desugars the given [[WeededAst.JvmMethod]] `method0`.
    */
  private def visitJvmMethod(method0: WeededAst.JvmMethod)(implicit flix: Flix): DesugaredAst.JvmMethod = method0 match {
    case WeededAst.JvmMethod(ident, fparams, exp, tpe, eff, loc) =>
      DesugaredAst.JvmMethod(ident, visitFormalParams(fparams), visitExp(exp), visitType(tpe), eff.map(visitType), loc)
  }

  /**
    * Desugars the given [[WeededAst.SelectChannelRule]] `rule0`.
    */
  private def visitSelectChannelRule(rule0: WeededAst.SelectChannelRule)(implicit flix: Flix): DesugaredAst.SelectChannelRule = rule0 match {
    case WeededAst.SelectChannelRule(ident, exp1, exp2) => DesugaredAst.SelectChannelRule(ident, visitExp(exp1), visitExp(exp2))
  }

  /**
    * Desugars the given [[WeededAst.ParYieldFragment]] `frag0`.
    */
  private def visitParYieldFragment(frag0: WeededAst.ParYieldFragment)(implicit flix: Flix): DesugaredAst.ParYieldFragment = frag0 match {
    case WeededAst.ParYieldFragment(pat, exp, loc) => DesugaredAst.ParYieldFragment(visitPattern(pat), visitExp(exp), loc)
  }

  /**
    * Desugars the given [[WeededAst.Constraint]] `constraint0`.
    */
  private def visitConstraint(constraint0: WeededAst.Constraint)(implicit flix: Flix): DesugaredAst.Constraint = {
    def visitHead(head0: WeededAst.Predicate.Head): DesugaredAst.Predicate.Head = head0 match {
      case WeededAst.Predicate.Head.Atom(pred, den, exps, loc) => DesugaredAst.Predicate.Head.Atom(pred, den, visitExps(exps), loc)
    }

    def visitBody(body0: WeededAst.Predicate.Body): DesugaredAst.Predicate.Body = body0 match {
      case WeededAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
        DesugaredAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms.map(visitPattern), loc)
      case WeededAst.Predicate.Body.Functional(idents, exp, loc) =>
        DesugaredAst.Predicate.Body.Functional(idents, visitExp(exp), loc)
      case WeededAst.Predicate.Body.Guard(exp, loc) =>
        DesugaredAst.Predicate.Body.Guard(visitExp(exp), loc)
    }

    constraint0 match {
      case WeededAst.Constraint(head, body, loc) => DesugaredAst.Constraint(visitHead(head), body.map(visitBody), loc)
    }
  }

  /**
    * Desugars the given [[WeededAst.PredicateParam]] `param0`.
    */
  private def visitPredicateParam(param0: WeededAst.PredicateParam)(implicit flix: Flix): DesugaredAst.PredicateParam = param0 match {
    case WeededAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      DesugaredAst.PredicateParam.PredicateParamUntyped(pred, loc)
    case WeededAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      DesugaredAst.PredicateParam.PredicateParamWithType(pred, den, tpes.map(visitType), loc)
  }
}
