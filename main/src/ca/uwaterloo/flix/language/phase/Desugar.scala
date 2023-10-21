package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
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
    case WeededAst.Type.Intersection(tpe1, tpe2, loc) => DesugaredAst.Type.Intersection(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.Pure(loc) => DesugaredAst.Type.Pure(loc)
    case WeededAst.Type.CaseSet(cases, loc) => DesugaredAst.Type.CaseSet(cases, loc)
    case WeededAst.Type.CaseUnion(tpe1, tpe2, loc) => DesugaredAst.Type.CaseUnion(visitType(tpe1), visitType(tpe2), loc)
    case WeededAst.Type.CaseIntersection(tpe1, tpe2, loc) => DesugaredAst.Type.CaseIntersection(visitType(tpe1), visitType(tpe2), loc)
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
  private def visitFormalParams(fparams0: List[WeededAst.FormalParam])(implicit flix: Flix): List[DesugaredAst.FormalParam] = fparams0.map {
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
  private def visitDerivations(derives0: WeededAst.Derivations)(implicit flix: Flix): DesugaredAst.Derivations = derives0 match {
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
  private def visitExp(exp0: WeededAst.Expr)(implicit flix: Flix): DesugaredAst.Expr = ???

}
