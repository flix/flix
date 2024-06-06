package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.Ast.{Input, Source}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint

import java.nio.file.Files
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedSet

object PaperStats {

  def run(root: Root): Unit = {
    val defs = allDefs(root, defs = true, instances = true)
    val stats = getStats(defs)
    val effCount = (eff: EffectType) => stats.count(_.effect == eff)
    val pureFunctionCount = effCount(EffectType.Pure)
    val ioFunctionCount = effCount(EffectType.IO)
    val polyFunctionCount = effCount(EffectType.Poly)
    val unknownFunctionCount = stats.count(_.effect.isUnknown)
    val avgVars = stats.map(_.effVars).sum / stats.length
    val namespaceStats = getFileStats(stats)
    println(mdFileStats(namespaceStats))
    println()
    println(s"[Pure/IO/Poly/Err] - [$pureFunctionCount/$ioFunctionCount/$polyFunctionCount/$unknownFunctionCount]")
    println(s"avg vars: $avgVars")
  }

  private def getStats(defs: Iterable[Def]): List[Stats] = {
    ??? //defs.map(visitDef).toList.sortWith((s1, s2) => s1.sym.toString < s2.sym.toString)
  }

  private def allDefs(root: Root, defs: Boolean, instances: Boolean): Iterable[Def] = {
    val defdefs = if (defs) root.defs.values else Nil
    val instanceDefs = if (instances) root.instances.values.toList.flatten.flatMap(_.defs) else Nil
    defdefs ++ instanceDefs
  }

  private def getNamespaceStats(stats: List[Stats]): List[NsStats] = {
    stats.groupBy(stat => stat.sym.namespace).values.map(
      _.map(statsToNsStats).reduce(combineNsStats)
    ).toList.sortBy(_.ns.mkString("."))
  }

  private def getFileStats(stats: List[Stats]): List[FileStats] = {
    stats.groupBy(stat => stat.sym.src).values.map(
      _.map(statsToFileStats).reduce(combineFileStats)
    ).toList.sortBy(_.src.name)
  }

  private def statsToNsStats(stats: Stats): NsStats = {
    NsStats(stats.sym.namespace, Set(stats.sym.loc.source), 1, Map(stats.effect -> 1), stats.lambdas, stats.effVars)
  }

  private def statsToFileStats(stats: Stats): FileStats = {
    FileStats(stats.sym.src, Set(stats.sym.namespace), 1, Map(stats.effect -> 1), stats.lambdas, stats.effVars)
  }

  private def combineNsStats(s1: NsStats, s2: NsStats): NsStats = {
    assert(s1.ns == s2.ns)
    NsStats(s1.ns, s1.src.union(s2.src), s1.functions + s2.functions, addMaps(s1.effects, s2.effects), s1.lambdas + s2.lambdas, s1.effVars + s2.effVars)
  }

  private def combineFileStats(s1: FileStats, s2: FileStats): FileStats = {
    assert(s1.src == s2.src)
    FileStats(s1.src, s1.ns.union(s2.ns), s1.functions + s2.functions, addMaps(s1.effects, s2.effects), s1.lambdas + s2.lambdas, s1.effVars + s2.effVars)
  }

  // Sum of touching files
  private def lineCount(s: NsStats): Long = {
    s.src.map(src => lineCount(src.input)).sum
  }

  private def lineCount(i: Input): Long = {
    i match {
      case Input.Text(_, text, _) => text.lines().count()
      case Input.TxtFile(path) =>
        Files.lines(path).count()
      case Input.PkgFile(_) =>
        ???
    }
  }

  private def lineCount(s: FileStats): Long = {
    lineCount(s.src.input)
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
    sb.append(s" Files |")
    sb.append(s" Lines |")
    sb.append(s" Functions |")
    sb.append(s" Pure |")
    sb.append(s" IO |")
    sb.append(s" Poly |")
    sb.append(s" Lambdas |")
    sb.append(s" Effect Vars |")
    sb.append("\n")
    sb.append(s"| --- | --- | --- | --- | --- | --- | --- | --- | --- |\n")
    for (stat <- stats) {
      sb.append("|")
      sb.append(s" ${rootPad(stat.ns).mkString(".")} |")
      sb.append(s" ${stat.src.size} |")
      sb.append(s" ${lineCount(stat)} |")
      sb.append(s" ${stat.functions} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.Pure, 0)} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.IO, 0)} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.Poly, 0)} |")
      sb.append(s" ${stat.lambdas} |")
      sb.append(s" ${stat.effVars} |\n")
    }
    sb.toString()
  }

  private def mdFileStats(stats: Iterable[FileStats]): String = {
    val sb = new StringBuilder()
    sb.append("|")
    sb.append(s" File |")
    sb.append(s" namespaces |")
    sb.append(s" Lines |")
    sb.append(s" Functions |")
    sb.append(s" Pure |")
    sb.append(s" IO |")
    sb.append(s" Poly |")
    sb.append(s" Lambdas |")
    sb.append(s" Effect Vars |")
    sb.append("\n")
    sb.append(s"| --- | --- | --- | --- | --- | --- | --- | --- | --- |\n")
    for (stat <- stats) {
      sb.append("|")
      sb.append(s" ${stat.src.name} |")
      sb.append(s" ${stat.ns.size} |")
      sb.append(s" ${lineCount(stat)} |")
      sb.append(s" ${stat.functions} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.Pure, 0)} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.IO, 0)} |")
      sb.append(s" ${stat.effects.getOrElse(EffectType.Poly, 0)} |")
      sb.append(s" ${stat.lambdas} |")
      sb.append(s" ${stat.effVars} |\n")
    }
    sb.toString()
  }

  private def rootPad(l: List[String]): List[String] = l match {
    case Nil => List("Root")
    case _ => l
  }

  private def lineCount(defn: Def): Int = {
    val beginLine = defn.sym.loc.beginLine min defn.exp.loc.beginLine min defn.spec.loc.beginLine
    val endLine = defn.sym.loc.endLine min defn.exp.loc.endLine min defn.spec.loc.endLine
    endLine - beginLine + 1
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

  private case class NsStats(ns: List[String], src: Set[Source], functions: Int, effects: Map[EffectType, Int], lambdas: Int, effVars: Int)

  private case class FileStats(src: Source, ns: Set[List[String]], functions: Int, effects: Map[EffectType, Int], lambdas: Int, effVars: Int)

  private case class Stats(sym: Symbol.DefnSym, lines: Int, effect: EffectType, lambdas: Int, effVars: Int) {
    override def toString: String = {
      s"$sym, $lines, $effect, $lambdas, $effVars"
    }
  }
}
