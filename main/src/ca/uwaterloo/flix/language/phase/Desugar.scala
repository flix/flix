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
    * Desugars the given [[WeededAst.Declaration.Class]] `clazz0`.
    */
  private def visitClass(clazz0: WeededAst.Declaration.Class)(implicit flix: Flix): DesugaredAst.Declaration.Class = clazz0 match {
    case WeededAst.Declaration.Class(doc, ann, mod, ident, tparam0, superClasses0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superClasses = superClasses0.map(visitTypeConstraint)
      val assocs = assocs0.map(visitAssocTypeSig)
      val sigs = sigs0.map(visitSig)
      val laws = laws0.map(visitDef)
      DesugaredAst.Declaration.Class(doc, ann, mod, ident, tparam, superClasses, assocs, sigs, laws, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.Instance]] `d`.
    */
  private def visitInstance(d: WeededAst.Declaration.Instance)(implicit flix: Flix): DesugaredAst.Declaration.Instance = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Def]] `d`.
    */
  private def visitDef(d: WeededAst.Declaration.Def)(implicit flix: Flix): DesugaredAst.Declaration.Def = d match {
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
    * Desugars the given [[WeededAst.Declaration.Law]] `d`.
    */
  private def visitLaw(d: WeededAst.Declaration.Law)(implicit flix: Flix): DesugaredAst.Declaration.Law = d match {
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
    * Desugars the given [[WeededAst.Declaration.Enum]] `d`.
    */
  private def visitEnum(d: WeededAst.Declaration.Enum)(implicit flix: Flix): DesugaredAst.Declaration.Enum = d match {
    case WeededAst.Declaration.Enum(doc, ann, mod, ident, tparams0, derives0, cases0, loc) =>
      val tparams = visitTypeParams(tparams0)
      val derives = visitDerivations(derives0)
      val cases = cases0.map(visitCase)
      DesugaredAst.Declaration.Enum(doc, ann, mod, ident, tparams, derives, cases, loc)
  }

  /**
    * Desugars the given [[WeededAst.Declaration.RestrictableEnum]] `d`.
    */
  private def visitRestrictableEnum(d: WeededAst.Declaration.RestrictableEnum)(implicit flix: Flix): DesugaredAst.Declaration.RestrictableEnum = ???

  /**
    * Desugars the given [[WeededAst.Declaration.TypeAlias]] `d`.
    */
  private def visitTypeAlias(d: WeededAst.Declaration.TypeAlias)(implicit flix: Flix): DesugaredAst.Declaration.TypeAlias = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Effect]] `d`.
    */
  private def visitEffect(d: WeededAst.Declaration.Effect)(implicit flix: Flix): DesugaredAst.Declaration.Effect = ???

  /**
    * Desugars the given [[WeededAst.TypeParam]] `tparam0`.
    */
  private def visitTypeParam(tparam0: WeededAst.TypeParam)(implicit flix: Flix): DesugaredAst.TypeParam = ???

  /**
    * Desugars the given [[WeededAst.Declaration.AssocTypeSig]] `assoc0`.
    */
  private def visitAssocTypeSig(assoc0: WeededAst.Declaration.AssocTypeSig)(implicit flix: Flix): DesugaredAst.Declaration.AssocTypeSig = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Sig]] `assoc`.
    */
  private def visitSig(sig0: WeededAst.Declaration.Sig)(implicit flix: Flix): DesugaredAst.Declaration.Sig = ???

  /**
    * Desugars the given [[WeededAst.KindedTypeParams]] `tparams`.
    */
  private def visitKindedTypeParams(tparams: WeededAst.KindedTypeParams)(implicit flix: Flix): DesugaredAst.KindedTypeParams = ???

  /**
    * Desugars the given [[WeededAst.TypeParams]] `tparams`.
    */
  private def visitTypeParams(tparams: WeededAst.TypeParams)(implicit flix: Flix): DesugaredAst.TypeParams = ???

  /**
    * Desugars the given list of [[WeededAst.FormalParam]] `fparams`.
    */
  private def visitFormalParams(fparams: List[WeededAst.FormalParam])(implicit flix: Flix): List[DesugaredAst.FormalParam] = ???

  /**
    * Desugars the given [[WeededAst.Type]] `tpe`.
    */
  private def visitType(tpe: WeededAst.Type)(implicit flix: Flix): DesugaredAst.Type = ???

  /**
    * Desugars the given [[WeededAst.TypeConstraint]] `tconstr`.
    */
  private def visitTypeConstraint(tconstr: WeededAst.TypeConstraint)(implicit flix: Flix): DesugaredAst.TypeConstraint = ???

  /**
    * Desugars the given [[WeededAst.EqualityConstraint]] `econstr`.
    */
  private def visitEqualityConstraint(econstr: WeededAst.EqualityConstraint)(implicit flix: Flix): DesugaredAst.EqualityConstraint = ???

  /**
    * Desugars the given [[WeededAst.Derivations]] `derives0`.
    */
  private def visitDerivations(derives0: WeededAst.Derivations)(implicit flix: Flix): DesugaredAst.Derivations = ???

  /**
    * Desugars the given [[WeededAst.Case]] `case0`.
    */
  private def visitCase(case0: WeededAst.Case)(implicit flix: Flix): DesugaredAst.Case = ???

  /**
    * Desugars the given [[WeededAst.Expr]] `exp0`.
    */
  private def visitExp(exp0: WeededAst.Expr)(implicit flix: Flix): DesugaredAst.Expr = ???

}
