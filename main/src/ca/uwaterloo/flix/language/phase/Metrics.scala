package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.ParsedAst.{Declaration, Effect, EffectSet, Type}
import ca.uwaterloo.flix.language.ast.{Name, ParsedAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

object Metrics {

  /* TODO Questions:
      * Should type-classes and instances be included?
      * Should private defs be counted?
   */

  case class Metrics(lines: Int, functions: Int, pureFunctions: Int, impureFunctions: Int, polyFunctions: Int, regionFunctions: Int,
                     oneRegionFunctions: Int, twoRegionFunctions: Int, threePlusRegionFunctions: Int) {

    override def toString: String = {
      def mkData(data: Int): String = f"$data%5d"

      def mkS(tag: String, data: Int): String = f"${tag + ":"}%25s$data%5d"

      def mkSPercent(tag: String, data: Int, outOf: Int): String = f"${mkS(tag, data)}%s (${(100 * data) / (1.0 * outOf)}%3.0f${"%"}%s)"

      def mkSPercentNoName(data: Int, outOf: Int): String = f"$data%5d (${(100 * data) / (1.0 * (if (outOf == 0) 1 else outOf))}%03.0f${"\\%"}%s)"

      val terminalOutputLines =
        mkS("lines", lines) ::
          mkS("total pub functions", functions) ::
          mkSPercent("pure functions", pureFunctions, functions) ::
          mkSPercent("impure functions", impureFunctions, functions) ::
          mkSPercent("effect poly functions", polyFunctions, functions) ::
          mkSPercent("region functions", regionFunctions, functions) ::
          mkSPercent("1 region functions", oneRegionFunctions, regionFunctions) ::
          mkSPercent("2 region functions", twoRegionFunctions, regionFunctions) ::
          mkSPercent("+3 region functions", threePlusRegionFunctions, regionFunctions) ::
          Nil
      val terminalOutput = terminalOutputLines.mkString("\n")

      val latexOutputLines =
        mkData(lines) ::
          mkData(functions) ::
          mkSPercentNoName(pureFunctions, functions) ::
          mkSPercentNoName(impureFunctions, functions) ::
          mkSPercentNoName(polyFunctions, functions) ::
          mkSPercentNoName(regionFunctions, functions) ::
          mkSPercentNoName(oneRegionFunctions, regionFunctions) ::
          mkSPercentNoName(twoRegionFunctions, regionFunctions) ::
          mkSPercentNoName(threePlusRegionFunctions, regionFunctions) ::
          Nil
      val latexOutput = latexOutputLines.mkString(" & ") + "\\\\\\hline"

      latexOutput
    }

    def sanityCheck(): Int = {
      if (functions != pureFunctions + impureFunctions + polyFunctions + regionFunctions) 1
      else if (regionFunctions != oneRegionFunctions + twoRegionFunctions + threePlusRegionFunctions) 2
      else 0
    }
  }

  def latexHeader(): String =
    s"""
       |\\begin{tabular}{|l|l|r|r|r|r|r|r|r|r|}
       |  \\hline
       |  file & lines & total & pure & impure & efpoly & regef & regef1 & regef2 & regef3+ \\\\\\hline
       |""".stripMargin

  def latexEnd(): String =
    s"""
       |\n\\end{tabular}
       |""".stripMargin

  case class DefIsh(name: String, tpe: ParsedAst.Type, purAndEff: ParsedAst.PurityAndEffect)

  private def getPubDefs(decl: ParsedAst.Declaration): Seq[DefIsh] = decl match {
    case ns: Declaration.Namespace => ns.decls.flatMap(getPubDefs)
    case ddef: Declaration.Def => if (ddef.mod.exists(mod => mod.name == "pub")) List(DefIsh(ddef.ident.name, ddef.tpe, ddef.purAndEff)) else Nil
    case _: Declaration.Law => Nil
    case _: Declaration.Enum => Nil
    case _: Declaration.TypeAlias => Nil
    case _: Declaration.Relation => Nil
    case _: Declaration.Lattice => Nil
    case clazz: Declaration.Class => clazz.lawsAndSigs.flatMap {
      case sig: Declaration.Sig => if (sig.mod.exists(mod => mod.name == "pub")) List(DefIsh(sig.ident.name, sig.tpe, sig.purAndEff)) else Nil
      case _: Declaration.Law => Nil
    }
    case instance: Declaration.Instance => instance.defs.flatMap(getPubDefs)
    case _: Declaration.Effect => Nil
  }

  private def isPure(defIsh: DefIsh): Boolean = {
    val purPart = defIsh.purAndEff.pur match {
      case Some(value) => value match {
        case Type.True(_, _) => true
        case Type.False(_, _) => false
        case _ => false
      }
      case None => true
    }
    val efPart = defIsh.purAndEff.eff match {
      case Some(value) => value match {
        case EffectSet.Singleton(_, _, _) => false
        case EffectSet.Pure(_, _) => true
        case EffectSet.Set(_, _, _) => false
      }
      case None => true
    }
    purPart && efPart
  }

  private def isImpure(defIsh: DefIsh): Boolean = {
    val purPart = defIsh.purAndEff.pur match {
      case Some(value) => value match {
        case Type.True(_, _) => Some(false)
        case Type.False(_, _) => Some(true)
        case _ => Some(false)
      }
      case None => None
    }
    val efPart = defIsh.purAndEff.eff match {
      case Some(value) => value match {
        case EffectSet.Singleton(_, eff, _) => eff match {
          case Effect.Impure(_, _) => Some(true)
          case _ => Some(false)
        }
        case EffectSet.Pure(_, _) => Some(false)
        case EffectSet.Set(_, effs, _) => Some(effs.forall {
          case Effect.Impure(_, _) => true
          case _ => false
        })
      }
      case None => None
    }
    (purPart, efPart) match {
      case (Some(true), Some(true)) => true
      case (None, Some(true)) => true
      case (Some(true), None) => true
      case _ => false
    }
  }

  private def combineOpts[T](comb: (T, T) => T, opt1: Option[T], opt2: Option[T]): Option[T] = (opt1, opt2) match {
    case (Some(b1), Some(b2)) => Some(comb(b1, b2))
    case (Some(b1), None) => Some(b1)
    case (None, Some(b2)) => Some(b2)
    case _ => None
  }

  private def isRegionInvolved(defIsh: DefIsh): Set[Name.Ident] = {
    val purPart: Set[Name.Ident] = defIsh.purAndEff.pur match {
      case Some(value) =>
        def check(t: Type): Set[Name.Ident] = t match {
          case Type.Var(sp1, ident, sp2) => Set() // dont know
          case Type.Ambiguous(sp1, qname, sp2) => ???
          case Type.Tuple(sp1, elms, sp2) => ???
          case Type.Record(sp1, fields, rest, sp2) => ???
          case Type.RecordRow(sp1, fields, rest, sp2) => ???
          case Type.Schema(sp1, predicates, rest, sp2) => ???
          case Type.SchemaRow(sp1, predicates, rest, sp2) => ???
          case Type.UnaryPolymorphicArrow(tpe1, tpe2, purAndEff, sp2) => ???
          case Type.PolymorphicArrow(sp1, tparams, tresult, purAndEff, sp2) => ???
          case Type.Native(sp1, fqn, sp2) => ???
          case Type.Apply(base, tparams, sp2) => ???
          case Type.True(sp1, sp2) => Set()
          case Type.False(sp1, sp2) => Set()
          case Type.Not(sp1, tpe, sp2) => ???
          case Type.And(tpe1, tpe2, sp2) => check(tpe1) union check(tpe2)
          case Type.Or(tpe1, tpe2, sp2) => check(tpe1) union check(tpe2)
          case Type.Effect(sp1, eff, sp2) => ???
          case Type.Ascribe(tpe, kind, sp2) => ???
        }

        check(value)
      case None => Set()
    }
    val efPart: Set[Name.Ident] = defIsh.purAndEff.eff match {
      case Some(effSet) => effSet match {
        case EffectSet.Singleton(_, eff, _) => eff match {
          case Effect.Read(sp1, regs, sp2) => regs.toSet
          case Effect.Write(sp1, regs, sp2) => regs.toSet
          case Effect.Var(sp1, ident, sp2) => Set()
          case Effect.Impure(sp1, sp2) => ???
          case Effect.Eff(sp1, name, sp2) => ???
          case Effect.Complement(sp1, eff, sp2) => ???
          case Effect.Union(eff1, effs) => ???
          case Effect.Intersection(eff1, effs) => ???
          case Effect.Difference(eff1, effs) => ???
        }
        case EffectSet.Pure(_, _) => Set()
        case EffectSet.Set(sp1, effs, sp2) =>
          val lll: Seq[Set[Name.Ident]] = effs.map {
            case Effect.Read(sp1, regs, sp2) => regs.toSet
            case Effect.Write(sp1, regs, sp2) => regs.toSet
            case Effect.Var(sp1, ident, sp2) => Set()
            case Effect.Impure(sp1, sp2) => ???
            case Effect.Eff(sp1, name, sp2) => ???
            case Effect.Complement(sp1, eff, sp2) => ???
            case Effect.Union(eff1, effs) => ???
            case Effect.Intersection(eff1, effs) => ???
            case Effect.Difference(eff1, effs) => ???
          }
          lll.foldLeft(Set[Name.Ident]()) { case (acc, s) => acc union s }
      }
      case None => Set()
    }
    purPart union efPart
  }

  private def isEffPoly(defIsh: DefIsh): Boolean = {
    val purPart = defIsh.purAndEff.pur match {
      case Some(value) =>
        def check(t: Type): Option[Boolean] = t match {
          case Type.Var(sp1, ident, sp2) => Some(true)
          case Type.Not(sp1, tpe, sp2) => check(tpe)
          case Type.And(tpe1, tpe2, sp2) => combineOpts[Boolean](_ || _, check(tpe1), check(tpe2))
          case Type.Or(tpe1, tpe2, sp2) => combineOpts[Boolean](_ || _, check(tpe1), check(tpe2))
          case Type.True(sp1, sp2) => Some(false)
          case Type.False(sp1, sp2) => Some(false)
          case Type.Ambiguous(sp1, qname, sp2) => ???
          case Type.Apply(base, tparams, sp2) => ???
          case Type.Ascribe(tpe, kind, sp2) => ???
          case Type.Native(sp1, fqn, sp2) => ???
          case Type.PolymorphicArrow(sp1, tparams, tresult, purAndEff, sp2) => ???
          case Type.Record(sp1, fields, rest, sp2) => ???
          case Type.RecordRow(sp1, fields, rest, sp2) => ???
          case Type.Schema(sp1, predicates, rest, sp2) => ???
          case Type.SchemaRow(sp1, predicates, rest, sp2) => ???
          case Type.Tuple(sp1, elms, sp2) => ???
          case Type.UnaryPolymorphicArrow(tpe1, tpe2, purAndEff, sp2) => ???
          case Type.Effect(sp1, eff, sp2) => ???
        }

        check(value)
      case None => None
    }
    val efPart = defIsh.purAndEff.eff match {
      case Some(value) => value match {
        case EffectSet.Singleton(sp1, eff, sp2) => eff match {
          case Effect.Var(sp1, ident, sp2) => Some(true)
          case Effect.Read(sp1, regs, sp2) => Some(false)
          case Effect.Write(sp1, regs, sp2) => Some(false)
          case Effect.Impure(sp1, sp2) => Some(false)
          case Effect.Eff(sp1, name, sp2) => ???
          case Effect.Complement(sp1, eff, sp2) => ???
          case Effect.Union(eff1, effs) => ???
          case Effect.Intersection(eff1, effs) => ???
          case Effect.Difference(eff1, effs) => ???
        }
        case EffectSet.Pure(sp1, sp2) => Some(false)
        case EffectSet.Set(sp1, effs, sp2) =>
          def check(e: Effect): Boolean = e match {
            case Effect.Var(sp1, ident, sp2) => true
            case Effect.Read(sp1, regs, sp2) => false
            case Effect.Write(sp1, regs, sp2) => false
            case Effect.Impure(sp1, sp2) => false
            case Effect.Eff(sp1, name, sp2) => ???
            case Effect.Complement(sp1, eff, sp2) => ???
            case Effect.Union(eff1, effs) => effs.exists(check) || check(eff1)
            case Effect.Intersection(eff1, effs) => ???
            case Effect.Difference(eff1, effs) => ???
          }

          Some(effs.exists(check))
      }
      case None => None
    }
    combineOpts[Boolean](_ || _, purPart, efPart).getOrElse(false)
  }

  def run(root: ParsedAst.Root)(implicit flix: Flix): Validation[Map[Source, Metrics], CompilationMessage] =
    root.units.map {
      case (src, ParsedAst.CompilationUnit(_, _, decls, _)) =>
        val lines = src.data.count(_ == '\n')
        val pubDefs = decls.flatMap(getPubDefs)
        val functions = pubDefs.length
        val pureFunctions = pubDefs.count(isPure)
        val impureFunctions = pubDefs.count(isImpure)

        def regFunctions(f: Int => Boolean): Int = pubDefs.count(d => f(isRegionInvolved(d).size))

        val regionFunctions = regFunctions(i => i > 0)
        val effPolyFunctions = pubDefs.filterNot(d => isRegionInvolved(d).nonEmpty).count(isEffPoly)

        val metrics: Metrics = Metrics(
          lines, functions, pureFunctions, impureFunctions, effPolyFunctions,
          regionFunctions, regFunctions(_ == 1), regFunctions(_ == 2), regFunctions(_ >= 3))
        (src, metrics)
    }.toSuccess

}
