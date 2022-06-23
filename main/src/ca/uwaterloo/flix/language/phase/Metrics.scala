package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.ParsedAst
import ca.uwaterloo.flix.language.ast.ParsedAst.Declaration
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

object Metrics {

  /* Questions:

    Should typeclasses and instances be included?

   */

  case class Metrics(lines: Int, functions: Int, pureFunctions: Int, impureFunctions: Int, regionFunctions: Int,
                     oneRegionFunctions: Int, twoRegionFunctions: Int, threePlusRegionFunctions: Int) {
    override def toString: String = {
      def mkS(tag: String, data: Int): String = f"${tag+":"}%20s$data%4d"
      val outputLines =
        mkS("lines", lines) ::
        mkS("total pub functions", functions) ::
        mkS("pure functions", pureFunctions) ::
        mkS("impure functions", impureFunctions) ::
        mkS("region functions", regionFunctions) ::
        mkS("1 region functions", oneRegionFunctions) ::
        mkS("2 region functions", twoRegionFunctions) ::
        mkS("3+ region functions", threePlusRegionFunctions) ::
        Nil
      outputLines.mkString("\n")
    }
  }

  private val pubDefReg = "pub\\s+def".r

  private def pubDefCount(decl: ParsedAst.Declaration): Int = decl match {
    case ns: Declaration.Namespace => ns.decls.map(pubDefCount).sum
    case ddef: Declaration.Def => if (ddef.mod.exists(mod => mod.name == "pub")) 1 else 0
    case Declaration.Law(doc, ann, mod, sp1, ident, tparams, fparams, tconstrs, exp, sp2) => 0
    case Declaration.Enum(doc, ann, mod, sp1, ident, tparams, tpe, derives, cases, sp2) => 0
    case Declaration.TypeAlias(doc, mod, sp1, ident, tparams, tpe, sp2) => 0
    case Declaration.Relation(doc, mod, sp1, ident, tparams, attr, sp2) => 0
    case Declaration.Lattice(doc, mod, sp1, ident, tparams, attr, sp2) => 0
    case clazz: Declaration.Class => clazz.lawsAndSigs.map {
      case sig: Declaration.Sig => if (sig.mod.exists(mod => mod.name == "pub")) 1 else 0
      case _: Declaration.Law => 0
    }.sum
    case instance: Declaration.Instance => instance.defs.map(pubDefCount).sum
    case Declaration.Effect(doc, ann, mod, sp1, ident, tparams, ops, sp2) => 0
  }

  def run(root: ParsedAst.Root)(implicit flix: Flix): Validation[Map[Source, Metrics], CompilationMessage] =
    root.units.map{
      case (src, ParsedAst.CompilationUnit(_, _, decls, _)) =>
        val lines = src.data.count(_ == '\n')
        val functions = decls.map(pubDefCount).sum

        val metrics: Metrics = Metrics(lines, functions, 0, 0, 0, 0, 0, 0)
        (src, metrics)
    }.toSuccess

}
