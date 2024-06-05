package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.util.ParOps

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedSet

object PaperStats {

  private val effVarCount: TrieMap[Symbol.DefnSym, Int] = TrieMap.empty

  def addEffCount(sym: Symbol.DefnSym, tc: List[TypeConstraint]): Unit = {
    if (effVarCount.contains(sym)) ??? // shouldn't happen
    else effVarCount.update(sym, effVars(tc).size)
  }

  private def combine(l: List[SortedSet[Type.Var]]): SortedSet[Type.Var] = {
    if (l.isEmpty) SortedSet.empty
    else l.reduce((s1, s2) => s1.union(s2))
  }

  private def effVars(tcs: List[TypeConstraint]): SortedSet[Type.Var] = {
    combine(tcs.map(effVars))
  }

  private def effVars(tc: TypeConstraint): SortedSet[Type.Var] = tc match {
    case TypeConstraint.Equality(tpe1, tpe2, _) =>
      effVars(tpe1).union(effVars(tpe2))
    case TypeConstraint.EqJvmConstructor(mvar, _, tpes, _) =>
      effVars(mvar).union(combine(tpes.map(effVars)))
    case TypeConstraint.EqJvmMethod(mvar, tpe0, _, tpes, _) =>
      effVars(mvar).union(effVars(tpe0)).union(combine(tpes.map(effVars)))
    case TypeConstraint.Trait(_, tpe, _) =>
      effVars(tpe)
    case TypeConstraint.Purification(_, eff1, eff2, _, nested) =>
      effVars(eff1).union(effVars(eff2)).union(effVars(nested))
  }

  /**
    * OBS: not counting associated types as variables.
    */
  private def effVars(tpe: Type): SortedSet[Type.Var] = tpe match {
    case v@Type.Var(_, _) if tpe.kind == Kind.Eff =>
      SortedSet(v)
    case Type.Var(_, _) =>
      SortedSet.empty
    case Type.Cst(_, _) =>
      SortedSet.empty
    case Type.Apply(tpe1, tpe2, _) =>
      effVars(tpe1).union(effVars(tpe2))
    case Type.Alias(_, args, tpe, _) =>
      combine(args.map(effVars)).union(effVars(tpe))
    case Type.AssocType(_, arg, _, _) =>
      effVars(arg)
  }

  def run(root: Root)(implicit flix: Flix): Unit = {
    val x = ParOps.parMapValues(root.defs)(visitDef)
    val stats = x.values.toList.sortWith((s1, s2) => s1.sym.toString < s2.sym.toString)
    val effCount = (eff: EffectType) => stats.count(_.effect == eff)
    val pureFunctionCount = effCount(EffectType.Pure)
    val ioFunctionCount = effCount(EffectType.IO)
    val polyFunctionCount = effCount(EffectType.Poly)
    val unknownFunctionCount = stats.count(_.effect.isUnknown)
    val avgVars = stats.map(_.effVars).sum / stats.length
    val nsStats = stats.groupBy(stat => stat.sym.namespace).values.map(
      _.map(s => NsStats(s.sym.namespace, 1, s.lines, Map(s.effect -> 1), s.lambdas, s.effVars)).reduce(
        (s1, s2) => {
          assert(s1.ns == s2.ns)
          NsStats(s1.ns, s1.functions + s2.functions, s1.lines + s2.lines, addMaps(s1.effects, s2.effects), s1.lambdas + s2.lambdas, s1.effVars + s2.effVars)
        }
      )
    ).toList
    println(mdNsStats(nsStats))
    println()
    println(s"[Pure/IO/Poly/Err] - [$pureFunctionCount/$ioFunctionCount/$polyFunctionCount/$unknownFunctionCount]")
    println(s"avg vars: $avgVars")
  }

  private def addMaps(m1: Map[EffectType, Int], m2: Map[EffectType, Int]): Map[EffectType, Int] = {
    import EffectType._
    List(
      Pure -> (m1.getOrElse(Pure, 0) + m2.getOrElse(Pure, 0)),
      IO -> (m1.getOrElse(IO, 0) + m2.getOrElse(IO, 0)),
      Poly -> (m1.getOrElse(Poly, 0) + m2.getOrElse(Poly, 0))
    ).toMap
  }

  private def mdStats(stats: Iterable[Stats]): String = {
    val sb = new StringBuilder()
    sb.append("|")
    sb.append(s" Name |")
    sb.append(s" Lines |")
    sb.append(s" Effect |")
    sb.append(s" Lambdas |")
    sb.append(s" Effect Vars |")
    sb.append("\n")
    sb.append(s"| --- | --- | --- | --- | --- |\n")
    for (stat <- stats) {
      sb.append("|")
      sb.append(s" ${stat.sym.toString} |")
      sb.append(s" ${stat.lines} |")
      sb.append(s" ${stat.effect} |")
      sb.append(s" ${stat.lambdas} |")
      sb.append(s" ${stat.effVars} |\n")
    }
    sb.toString()
  }

  private def mdNsStats(stats: Iterable[NsStats]): String = {
    val sb = new StringBuilder()
    sb.append("|")
    sb.append(s" Name |")
    sb.append(s" Functions |")
    sb.append(s" Lines |")
    sb.append(s" Pure |")
    sb.append(s" IO |")
    sb.append(s" Poly |")
    sb.append(s" Lambdas |")
    sb.append(s" Effect Vars |")
    sb.append("\n")
    sb.append(s"| --- | --- | --- | --- | --- | --- | --- | --- |\n")
    for (stat <- stats) {
      sb.append("|")
      sb.append(s" ${stat.ns.mkString(".")} |")
      sb.append(s" ${stat.functions} |")
      sb.append(s" ${stat.lines} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.Pure, 0)} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.IO, 0)} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.Poly, 0)} |")
      sb.append(s" ${stat.lambdas} |")
      sb.append(s" ${stat.effVars} |\n")
    }
    sb.toString()
  }

  private def lineCount(loc: SourceLocation): Int = loc.endLine - loc.beginLine + 1

  private def lineCount(defn: Def): Int = {
    lineCount(defn.exp.loc)
  }

  private sealed trait EffectType {
    override def toString: String = this match {
      case EffectType.IO => "IO"
      case EffectType.Pure => "Pure"
      case EffectType.Poly => "Poly"
    }

    def isUnknown: Boolean = this match {
      case EffectType.IO => false
      case EffectType.Pure => false
      case EffectType.Poly => false
    }
  }

  private object EffectType {
    case object IO extends EffectType

    case object Pure extends EffectType

    case object Poly extends EffectType
  }

  // Assumptions:
  // - IO is used directly, not e.g. IO + IO or IOAlias
  // - effect variables are use meaningfully, not e.g. x and not x
  // - Pure is written directly, not e.g. IO and not IO
  private def effectType(defn: Def): EffectType = {
    val effect = defn.spec.eff
    effect match {
      case Type.Cst(TypeConstructor.Pure, _) => EffectType.Pure
      case Type.Cst(TypeConstructor.Effect(Symbol.IO), _) => EffectType.IO
      case _ if effect.typeVars.nonEmpty => EffectType.Poly
      case _ => ??? // unknown
    }
  }

  private case class NsStats(ns: List[String], functions: Int, lines: Int, effects: Map[EffectType, Int], lambdas: Int, effVars: Int)

  private case class Stats(sym: Symbol.DefnSym, lines: Int, effect: EffectType, lambdas: Int, effVars: Int) {
    override def toString: String = {
      s"$sym, $lines, $effect, $lambdas, $effVars"
    }
  }

  private def visitDef(defn: Def): Stats = {
    Stats(
      sym = defn.sym,
      lines = lineCount(defn),
      effect = effectType(defn),
      lambdas = lambdaCount(defn),
      effVars = effVarCount.getOrElse(defn.sym, -1)
    )
  }

  /**
    * Returns the number of syntactic lambdas in the function body.
    *
    * OBS: newObject are not counted as a lambda.
    */
  private def lambdaCount(defn: Def): Int = {
    def visit(e: Expr): Int = e match {
      case Expr.Cst(cst, tpe, loc) => 0
      case Expr.Var(sym, tpe, loc) => 0
      case Expr.Def(sym, tpe, loc) => 0
      case Expr.Sig(sym, tpe, loc) => 0
      case Expr.Hole(sym, tpe, loc) => -0
      case Expr.HoleWithExp(exp, tpe, eff, loc) => visit(exp)
      case Expr.OpenAs(symUse, exp, tpe, loc) => visit(exp)
      case Expr.Use(sym, alias, exp, loc) => visit(exp)
      case Expr.Lambda(fparam, exp, tpe, loc) => 1 + visit(exp)
      case Expr.Apply(exp, exps, tpe, eff, loc) => (exp :: exps).map(visit).sum
      case Expr.Unary(sop, exp, tpe, eff, loc) => visit(exp)
      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.Region(tpe, loc) => 0
      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => visit(exp)
      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visit(exp1) + visit(exp2) + visit(exp3)
      case Expr.Stm(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.Discard(exp, eff, loc) => visit(exp)
      case Expr.Match(exp, rules, tpe, eff, loc) => (exp :: rules.flatMap(r => r.exp :: r.guard.toList)).map(visit).sum
      case Expr.TypeMatch(exp, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(visit).sum
      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(visit).sum
      case Expr.Tag(sym, exp, tpe, eff, loc) => visit(exp)
      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => visit(exp)
      case Expr.Tuple(elms, tpe, eff, loc) => elms.map(visit).sum
      case Expr.RecordEmpty(tpe, loc) => 0
      case Expr.RecordSelect(exp, label, tpe, eff, loc) => visit(exp)
      case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.RecordRestrict(label, exp, tpe, eff, loc) => visit(exp)
      case Expr.ArrayLit(exps, exp, tpe, eff, loc) => (exp :: exps).map(visit).sum
      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => visit(exp1) + visit(exp2) + visit(exp3)
      case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.ArrayLength(exp, eff, loc) => visit(exp)
      case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => visit(exp1) + visit(exp2) + visit(exp3)
      case Expr.VectorLit(exps, tpe, eff, loc) => exps.map(visit).sum
      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.VectorLength(exp, loc) => visit(exp)
      case Expr.Ref(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.Deref(exp, tpe, eff, loc) => visit(exp)
      case Expr.Assign(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.Ascribe(exp, tpe, eff, loc) => visit(exp)
      case Expr.InstanceOf(exp, clazz, loc) => visit(exp)
      case Expr.CheckedCast(cast, exp, tpe, eff, loc) => visit(exp)
      case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => visit(exp)
      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => visit(exp)
      case Expr.Without(exp, effUse, tpe, eff, loc) => visit(exp)
      case Expr.TryCatch(exp, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(visit).sum
      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(visit).sum
      case Expr.Do(op, exps, tpe, eff, loc) => exps.map(visit).sum
      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => exps.map(visit).sum
      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => (exp :: exps).map(visit).sum
      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => exps.map(visit).sum
      case Expr.GetField(field, exp, tpe, eff, loc) => visit(exp)
      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.GetStaticField(field, tpe, eff, loc) => 0
      case Expr.PutStaticField(field, exp, tpe, eff, loc) => visit(exp)
      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => methods.map(_.exp).map(visit).sum
      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.GetChannel(exp, tpe, eff, loc) => visit(exp)
      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.SelectChannel(rules, default, tpe, eff, loc) => (default.toList ++ rules.flatMap(r => List(r.exp, r.chan))).map(visit).sum
      case Expr.Spawn(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.ParYield(frags, exp, tpe, eff, loc) => (exp :: frags.map(_.exp)).map(visit).sum
      case Expr.Lazy(exp, tpe, loc) => visit(exp)
      case Expr.Force(exp, tpe, eff, loc) => visit(exp)
      case Expr.FixpointConstraintSet(cs, tpe, loc) => cs.flatMap(_.head match {
        case Head.Atom(pred, den, terms, tpe, loc) => terms
      }).map(visit).sum
      case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => visit(exp)
      case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => visit(exp1) + visit(exp2)
      case Expr.FixpointSolve(exp, tpe, eff, loc) => visit(exp)
      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => visit(exp)
      case Expr.FixpointInject(exp, pred, tpe, eff, loc) => visit(exp)
      case Expr.FixpointProject(pred, exp, tpe, eff, loc) => visit(exp)
      case Expr.Error(m, tpe, eff) => 0
    }

    visit(defn.exp)
  }
}
