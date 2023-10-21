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
    * Desugars the given compilation unit `unit`.
    */
  private def visitUnit(src: Ast.Source, unit: WeededAst.CompilationUnit)(implicit flix: Flix): (Ast.Source, DesugaredAst.CompilationUnit) = unit match {
    case WeededAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val decls = decls0.map(visitDecl)
      src -> DesugaredAst.CompilationUnit(usesAndImports, decls, loc)
  }

  /**
    * Maps `useOrImport` to a corresponding [[DesugaredAst.UseOrImport]].
    */
  private def visitUseOrImport(useOrImport: WeededAst.UseOrImport): DesugaredAst.UseOrImport = useOrImport match {
    case WeededAst.UseOrImport.Use(qname, alias, loc) => DesugaredAst.UseOrImport.Use(qname, alias, loc)
    case WeededAst.UseOrImport.Import(name, alias, loc) => DesugaredAst.UseOrImport.Import(name, alias, loc)
  }

  /**
    * Compiles `decl` to a [[DesugaredAst.Declaration]].
    */
  private def visitDecl(decl: WeededAst.Declaration)(implicit flix: Flix): DesugaredAst.Declaration = decl match {
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
    * Desugars the given [[WeededAst.Declaration.Class]] `d`.
    */
  private def visitClass(d: WeededAst.Declaration.Class)(implicit flix: Flix): DesugaredAst.Declaration.Class = ???

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
  private def visitLaw(d: WeededAst.Declaration.Law)(implicit flix: Flix): DesugaredAst.Declaration.Law = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Enum]] `d`.
    */
  private def visitEnum(d: WeededAst.Declaration.Enum)(implicit flix: Flix): DesugaredAst.Declaration.Enum = ???

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
    * Desugars the given [[WeededAst.KindedTypeParams]] `tparams`.
    */
  private def visitKindedTypeParams(tparams: WeededAst.KindedTypeParams)(implicit flix: Flix): DesugaredAst.KindedTypeParams = ???

  /**
    * Desugars the given list of [[WeededAst.FormalParam]] `fparams`.
    */
  private def visitFormalParams(fparams: List[WeededAst.FormalParam])(implicit flix: Flix): List[DesugaredAst.FormalParam] = ???

  /**
    * Desugars the given [[WeededAst.Expr]] `exp0`.
    */
  private def visitExp(exp0: WeededAst.Expr)(implicit flix: Flix): DesugaredAst.Expr = ???

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

}
