package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.ParsedAst._
import ca.uwaterloo.flix.language.ast.{Name, ParsedAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

object Metrics {

  case class Metrics(lines: Int, functions: Int, pureFunctions: Int, impureFunctions: Int, polyFunctions: Int, regionFunctions: Int,
                     oneRegionFunctions: Int, twoRegionFunctions: Int, threePlusRegionFunctions: Int, regionUses: Int) {

    def isEmpty: Boolean = /* lines == 0 && */ functions == 0 && pureFunctions == 0 && impureFunctions == 0 && polyFunctions == 0 && regionFunctions == 0 &&
      oneRegionFunctions == 0 && twoRegionFunctions == 0 && threePlusRegionFunctions == 0 && regionUses == 0

    override def toString: String = {
      def mkData(data: Int): String = f"$data%5d"

      def mkDataNoSpace(data: Int): String = f"$data%d"

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
          mkSPercent("region intros", regionUses, functions) ::
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
          mkSPercentNoName(regionUses, functions) ::
          Nil
      val latexOutput = latexOutputLines.mkString(" & ") + "\\\\\\hline"

      val csvOutputLines =
        mkDataNoSpace(lines) ::
          mkDataNoSpace(functions) ::
          mkDataNoSpace(pureFunctions) ::
          mkDataNoSpace(impureFunctions) ::
          mkDataNoSpace(polyFunctions) ::
          mkDataNoSpace(regionFunctions) ::
          mkDataNoSpace(oneRegionFunctions) ::
          mkDataNoSpace(twoRegionFunctions) ::
          mkDataNoSpace(threePlusRegionFunctions) ::
          mkDataNoSpace(regionUses) ::
          Nil
      val csvOutput = csvOutputLines.mkString(", ")

      csvOutput
    }

    def sanityCheck(): Int = {
      if (functions != pureFunctions + impureFunctions + polyFunctions + regionFunctions) 1
      else if (regionFunctions != oneRegionFunctions + twoRegionFunctions + threePlusRegionFunctions) 2
      else 0
    }
  }

  def latexHeader(): String =
    s"""
       |\\begin{tabular}{|l|l|r|r|r|r|r|r|r|r|r|}
       |  \\hline
       |  file & lines & total & pure & impure & efpoly & regef & regef1 & regef2 & regef3+ & region intros\\\\\\hline
       |""".stripMargin

  def csvHeader(): String =
    s"""
       |file, lines, total, pure, impure, efpoly, regef, regef1, regef2, regef3+, reg intro
       |""".stripMargin

  def latexEnd(): String =
    s"""
       |\n\\end{tabular}
       |""".stripMargin

  sealed case class DefIsh(name: String, exp: ParsedAst.Expression, tpe: ParsedAst.Type, purAndEff: ParsedAst.PurityAndEffect)

  private def getPubDefs(decl: ParsedAst.Declaration): Seq[DefIsh] = decl match {
    case ns: Declaration.Namespace => ns.decls.flatMap(getPubDefs)
    case ddef: Declaration.Def => if (ddef.mod.exists(mod => mod.name == "pub")) List(DefIsh(ddef.ident.name, ddef.exp, ddef.tpe, ddef.purAndEff)) else Nil
    case _: Declaration.Law => Nil
    case _: Declaration.Enum => Nil
    case _: Declaration.TypeAlias => Nil
    case _: Declaration.Relation => Nil
    case _: Declaration.Lattice => Nil
    case clazz: Declaration.Class => Nil
    //      clazz.lawsAndSigs.flatMap {
    //        case sig: Declaration.Sig => if (sig.mod.exists(mod => mod.name == "pub")) List(DefIsh(sig.ident.name, sig.tpe, sig.purAndEff)) else Nil
    //        case _: Declaration.Law => Nil
    //      }
    case instance: Declaration.Instance => Nil // instance.defs.flatMap(getPubDefs)
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
        case EffectSet.Set(_, effs, _) => Some(effs.exists {
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
            case Effect.Impure(sp1, sp2) => Set()
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

  private def introducesRegion(defIsh: DefIsh): Boolean = {
    def visitSelectChannelRule(value: ParsedAst.SelectChannelRule): Boolean =
      visitExp(value.chan) || visitExp(value.exp)

    def visitCatchRule(value: ParsedAst.CatchRule): Boolean =
      visitExp(value.exp)

    def visitHandlerRule(value: ParsedAst.HandlerRule): Boolean =
      visitExp(value.exp)

    def visitCatchOrHandler(value: ParsedAst.CatchOrHandler): Boolean = value match {
      case CatchOrHandler.Catch(rules) => rules.exists(visitCatchRule)
      case CatchOrHandler.Handler(eff, rules) => rules.exists(_.exists(visitHandlerRule))
    }

    def visitInterpolationPart(value: ParsedAst.InterpolationPart): Boolean = value match {
      case InterpolationPart.ExpPart(sp1, exp, sp2) => exp.exists(visitExp)
      case InterpolationPart.StrPart(sp1, chars, sp2) => false
    }

    def visitRecordOp(value: ParsedAst.RecordOp): Boolean = value match {
      case RecordOp.Extend(sp1, field, exp, sp2) => visitExp(exp)
      case RecordOp.Restrict(sp1, field, sp2) => false
      case RecordOp.Update(sp1, field, exp, sp2) => visitExp(exp)
    }

    def visitRecordField(value: ParsedAst.RecordField): Boolean =
      visitExp(value.value)

    def visitForeachFragment(value: ParsedAst.ForeachFragment): Boolean = value match {
      case ForeachFragment.ForEach(sp1, pat, exp, sp2) => visitExp(exp)
      case ForeachFragment.Guard(sp1, guard, sp2) => visitExp(guard)
    }

    def visitChoiceRule(value: ParsedAst.ChoiceRule): Boolean =
      visitExp(value.exp)

    def visitMatchRule(value: ParsedAst.MatchRule): Boolean =
      value.guard.exists(visitExp) || visitExp(value.exp)

    def visitArg(value: ParsedAst.Argument): Boolean = value match {
      case Argument.Named(name, exp, sp2) => visitExp(exp)
      case Argument.Unnamed(exp) => visitExp(exp)
    }

    def visitExp(exp: ParsedAst.Expression): Boolean = exp match {
      case Expression.Scope(sp1, ident, exp, sp2) => true

      case Expression.SName(sp1, name, sp2) => false
      case Expression.QName(sp1, name, sp2) => false
      case Expression.Hole(sp1, ident, sp2) => false
      case Expression.Use(sp1, use, exp, sp2) => false
      case Expression.Lit(sp1, lit, sp2) => false
      case Expression.Intrinsic(sp1, op, exps, sp2) => exps.exists(visitArg)
      case Expression.Apply(lambda, args, sp2) => visitExp(lambda) || args.exists(visitArg)
      case Expression.Infix(e1, name, e2, sp2) => visitExp(e1) || visitExp(e2)
      case Expression.Binary(exp1, op, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.Lambda(sp1, fparams, exp, sp2) => visitExp(exp)
      case Expression.LambdaMatch(sp1, pat, exp, sp2) => visitExp(exp)
      case Expression.Unary(sp1, op, exp, sp2) => visitExp(exp)
      case Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) => visitExp(exp1) || visitExp(exp2) || visitExp(exp3)
      case Expression.Stm(exp1, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.Discard(sp1, exp, sp2) => visitExp(exp)
      case Expression.LetMatch(sp1, mod, pat, tpe, exp1, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.LetMatchStar(sp1, pat, tpe, exp1, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.LetRecDef(sp1, ident, fparams, exp1, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.LetImport(sp1, op, exp, sp2) => visitExp(exp)
      case Expression.NewObject(sp1, fqn, sp2) => false
      case Expression.Static(sp1, sp2) => false
      case Expression.Match(sp1, exp, rules, sp2) => visitExp(exp) || rules.exists(visitMatchRule)
      case Expression.Choose(sp1, star, exps, rules, sp2) => exps.exists(visitExp) || rules.exists(visitChoiceRule)
      case Expression.ForEach(sp1, frags, exp, sp2) => frags.exists(visitForeachFragment) || visitExp(exp)
      case Expression.Tag(sp1, name, exp, sp2) => exp.exists(visitExp)
      case Expression.Tuple(sp1, elms, sp2) => elms.exists(visitExp)
      case Expression.RecordLit(sp1, fields, sp2) => fields.exists(visitRecordField)
      case Expression.RecordSelect(exp, field, sp2) => visitExp(exp)
      case Expression.RecordSelectLambda(sp1, field, sp2) => false
      case Expression.RecordOperation(sp1, ops, rest, sp2) => ops.exists(visitRecordOp) || visitExp(rest)
      case Expression.New(sp1, qname, exp, sp2) => exp.exists(visitExp)
      case Expression.ArrayLit(sp1, exps, exp, sp2) => exps.exists(visitExp) || exp.exists(visitExp)
      case Expression.ArrayNew(sp1, exp1, exp2, exp3, sp2) => visitExp(exp1) || visitExp(exp2) || exp3.exists(visitExp)
      case Expression.ArrayLoad(base, index, sp2) => visitExp(base) || visitExp(index)
      case Expression.ArrayStore(base, indexes, elm, sp2) => visitExp(base) || indexes.exists(visitExp) || visitExp(elm)
      case Expression.ArraySlice(base, beginIndex, endIndex, sp2) => visitExp(base) || beginIndex.exists(visitExp) || endIndex.exists(visitExp)
      case Expression.FNil(sp1, sp2) => false
      case Expression.FCons(exp1, sp1, sp2, exp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.FAppend(exp1, sp1, sp2, exp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.FSet(sp1, sp2, exps) => exps.exists(visitExp)
      case Expression.FMap(sp1, sp2, exps) => exps.exists(p => visitExp(p._1) || visitExp(p._2))
      case Expression.Interpolation(sp1, parts, sp2) => parts.exists(visitInterpolationPart)
      case Expression.Ref(sp1, exp1, exp2, sp2) => visitExp(exp1) || exp2.exists(visitExp)
      case Expression.Deref(sp1, exp, sp2) => visitExp(exp)
      case Expression.Assign(exp1, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.Ascribe(exp, tpe, purAndEff, sp2) => visitExp(exp)
      case Expression.Cast(exp, tpe, purAndEff, sp2) => visitExp(exp)
      case Expression.Without(exp, eff, sp2) => visitExp(exp)
      case Expression.Do(sp1, op, args, sp2) => args.exists(visitArg)
      case Expression.Resume(sp1, arg, sp2) => visitArg(arg)
      case Expression.Try(sp1, exp, catchOrHandler, sp2) => visitExp(exp) || visitCatchOrHandler(catchOrHandler)
      case Expression.NewChannel(sp1, tpe, exp, sp2) => visitExp(exp)
      case Expression.GetChannel(sp1, exp, sp2) => visitExp(exp)
      case Expression.PutChannel(exp1, exp2, sp2) => visitExp(exp1) || visitExp(exp2)
      case Expression.SelectChannel(sp1, rules, default, sp2) => rules.exists(visitSelectChannelRule) || default.exists(visitExp)
      case Expression.Spawn(sp1, exp, sp2) => visitExp(exp)
      case Expression.Lazy(sp1, exp, sp2) => visitExp(exp)
      case Expression.Force(sp1, exp, sp2) => visitExp(exp)
      case Expression.FixpointConstraint(sp1, con, sp2) => false // im lazy
      case Expression.FixpointConstraintSet(sp1, cs, sp2) => false
      case Expression.FixpointLambda(sp1, pparams, exp, sp2) => false
      case Expression.FixpointCompose(exp1, exp2, sp2) => false
      case Expression.FixpointInjectInto(sp1, exps, into, sp2) => false
      case Expression.FixpointSolveWithProject(sp1, exps, idents, sp2) => false
      case Expression.FixpointQueryWithSelect(sp1, exps, selects, from, whereExp, sp2) => false
      case Expression.Reify(sp1, t, sp2) => false
      case Expression.ReifyBool(sp1, t, sp2) => false
      case Expression.ReifyType(sp1, t, sp2) => false
      case Expression.ReifyPurity(sp1, exp1, ident, exp2, exp3, sp2) => visitExp(exp1) || visitExp(exp2) || visitExp(exp3)
    }

    visitExp(defIsh.exp)
  }

  def run(root: ParsedAst.Root)(implicit flix: Flix): Validation[(Map[Source, Metrics], Metrics), CompilationMessage] = {
    val metrics = root.units.map {
      case (src, ParsedAst.CompilationUnit(_, _, decls, _)) =>
        val lines = src.data.count(_ == '\n')
        val pubDefs = decls.flatMap(getPubDefs)
        val functions = pubDefs.length
        val pureFunctions = pubDefs.count(isPure)
        val impureFunctions = pubDefs.count(isImpure)
        val regionUses = pubDefs.count(introducesRegion)

        def regFunctions(f: Int => Boolean): Int = pubDefs.count(d => f(isRegionInvolved(d).size))

        val regionFunctions = regFunctions(i => i > 0)
        val effPolyFunctions = pubDefs.filterNot(d => isRegionInvolved(d).nonEmpty).count(isEffPoly)

        val metrics: Metrics = Metrics(
          lines, functions, pureFunctions, impureFunctions, effPolyFunctions,
          regionFunctions, regFunctions(_ == 1), regFunctions(_ == 2), regFunctions(_ >= 3), regionUses = regionUses)
        (src, metrics)
    }.filterNot { case (_, metrics) => metrics.isEmpty }
    val total = metrics.values.reduce((m1, m2) => {
      Metrics(
        lines = m1.lines + m2.lines,
        functions = m1.functions + m2.functions,
        pureFunctions = m1.pureFunctions + m2.pureFunctions,
        impureFunctions = m1.impureFunctions + m2.impureFunctions,
        polyFunctions = m1.polyFunctions + m2.polyFunctions,
        regionFunctions = m1.regionFunctions + m2.regionFunctions,
        oneRegionFunctions = m1.oneRegionFunctions + m2.oneRegionFunctions,
        twoRegionFunctions = m1.twoRegionFunctions + m2.twoRegionFunctions,
        threePlusRegionFunctions = m1.threePlusRegionFunctions + m2.threePlusRegionFunctions,
        regionUses = m1.regionUses + m2.regionUses
      )
    })

    (metrics, total).toSuccess
  }

}
