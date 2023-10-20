package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, DesugaredAst, WeededAst}
import ca.uwaterloo.flix.util.ParOps

object Desugar {

  /**
    * Performs desugaring on the entire program.
    */
  def run(program: WeededAst.Root)(implicit flix: Flix): DesugaredAst.Root = flix.phase("Desugar") {
    val unitsVal = ParOps.parMap(program.units) {
      case (k, v) => visitUnit(k, v)
    }

    val desugaredCompilationUnits = unitsVal.foldLeft(Map.empty[Ast.Source, DesugaredAst.CompilationUnit]) {
      case (macc, (k, v)) => macc + (k -> v)
    }

    DesugaredAst.Root(desugaredCompilationUnits, program.entryPoint, program.names)
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
    * Maps `u0` to a corresponding [[DesugaredAst.UseOrImport]].
    */
  private def visitUseOrImport(u0: WeededAst.UseOrImport): DesugaredAst.UseOrImport = u0 match {
    case WeededAst.UseOrImport.Use(qname, alias, loc) => DesugaredAst.UseOrImport.Use(qname, alias, loc)
    case WeededAst.UseOrImport.Import(name, alias, loc) => DesugaredAst.UseOrImport.Import(name, alias, loc)
  }

  /**
    * Compiles `decl` to a list of [[DesugaredAst.Declaration]].
    */
  private def visitDecl(decl: WeededAst.Declaration)(implicit flix: Flix): DesugaredAst.Declaration = decl match {
    case WeededAst.Declaration.Namespace(ident, usesAndImports, decls, loc) =>
      val usesAndImportsVal = usesAndImports.map(visitUseOrImport)
      val declsVal = decls.map(visitDecl)
      DesugaredAst.Declaration.Namespace(ident, usesAndImportsVal, declsVal, loc)

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
  private def visitClass(d: WeededAst.Declaration.Class): DesugaredAst.Declaration.Class = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Instance]] `d`.
    */
  private def visitInstance(d: WeededAst.Declaration.Instance): DesugaredAst.Declaration.Instance = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Def]] `d`.
    */
  private def visitDef(d: WeededAst.Declaration.Def): DesugaredAst.Declaration.Def = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Law]] `d`.
    */
  private def visitLaw(d: WeededAst.Declaration.Law): DesugaredAst.Declaration.Law = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Enum]] `d`.
    */
  private def visitEnum(d: WeededAst.Declaration.Enum): DesugaredAst.Declaration.Enum = ???

  /**
    * Desugars the given [[WeededAst.Declaration.RestrictableEnum]] `d`.
    */
  private def visitRestrictableEnum(d: WeededAst.Declaration.RestrictableEnum): DesugaredAst.Declaration.RestrictableEnum = ???

  /**
    * Desugars the given [[WeededAst.Declaration.TypeAlias]] `d`.
    */
  private def visitTypeAlias(d: WeededAst.Declaration.TypeAlias): DesugaredAst.Declaration.TypeAlias = ???

  /**
    * Desugars the given [[WeededAst.Declaration.Effect]] `d`.
    */
  private def visitEffect(d: WeededAst.Declaration.Effect): DesugaredAst.Declaration.Effect = ???

}
