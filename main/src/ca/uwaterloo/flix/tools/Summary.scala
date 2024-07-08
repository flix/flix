/*
 * Copyright 2023 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, SourcePosition, Type, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable.ListBuffer

object Summary {

  def printMarkdownFileSummary(root: Root, nsDepth: Option[Int] = None, minLines: Option[Int] = None): Unit = {
    val table = fileSummaryTable(root, nsDepth, minLines)

    def rowString(row: List[String]) = row.mkString("| ", " | ", " |")

    table match {
      case headers :: rows =>
        val line = headers.map(s => "-" * s.length)
        println(rowString(headers))
        println(rowString(line))
        rows.foreach(row => println(rowString(row)))
      case Nil => println("Empty")
    }
  }

  def printLatexFileSummary(root: Root, nsDepth: Option[Int] = None, minLines: Option[Int] = None): Unit = {
    val table = fileSummaryTable(root, nsDepth, minLines)
    table.foreach(row => println(row.mkString("", " & ", " \\\\")))
  }

  private def fileSummaryTable(root: Root, nsDepth: Option[Int], minLines: Option[Int]): List[List[String]] = {
    val allSums = groupedFileSummaries(fileSummaries(root), nsDepth)
    val totals = fileTotals(allSums)
    val printedSums = minLines match {
      case Some(min) => allSums.filter(_.data.lines >= min)
      case None => allSums
    }
    val dots = printedSums.lengthIs < allSums.length
    val builder = new RowBuilder()
    builder.addRow(FileSummary.header)
    printedSums.sortBy(_.src.name).map(_.toRow).foreach(builder.addRow)
    if (dots) builder.addRepeatedRow("...")
    builder.addRow("Totals" :: totals.toRow)
    builder.getRows
  }

  private def defSummary(defn: TypedAst.Def, isInstance: Boolean): DefSummary = {
    val fun = if (isInstance) Function.InstanceFun(defn.sym) else Function.Def(defn.sym)
    val eff = resEffect(defn.spec.eff)
    DefSummary(fun, eff)
  }

  private def defSummary(sig: TypedAst.Sig): DefSummary = {
    val fun = Function.TraitFunWithExp(sig.sym)
    val eff = resEffect(sig.spec.eff)
    DefSummary(fun, eff)
  }

  private def defSummaries(root: Root): List[DefSummary] = {
    val defs = root.defs.values.map(defSummary(_, isInstance = false))
    val instances = root.instances.values.flatten.flatMap(i => i.defs.map(defSummary(_, isInstance = true)))
    val traits = root.traits.values.flatMap(t => t.sigs.filter(_.exp.isDefined).map(defSummary))
    (defs ++ instances ++ traits).toList
  }

  private def fileData(sum: DefSummary)(implicit root: Root): FileData = {
    val src = sum.fun.loc.source
    val srcLoc = root.sources.getOrElse(src, SourceLocation(isReal = false, SourcePosition(unknownSource, 0, 0), SourcePosition(unknownSource, 0, 0)))
    val pureDefs = if (sum.eff == ResEffect.Pure) 1 else 0
    val justIODefs = if (sum.eff == ResEffect.JustIO) 1 else 0
    val polyDefs = if (sum.eff == ResEffect.Poly) 1 else 0
    FileData(Some(src), srcLoc.endLine, defs = 1, pureDefs, justIODefs, polyDefs)
  }

  private def fileData(sums: List[DefSummary])(implicit root: Root): FileData = {
    FileData.combine(sums.map(fileData))
  }

  private def fileSummaries(root: Root): List[FileSummary] = {
    val defSums = defSummaries(root)
    defSums.groupBy(_.src).map { case (src, sums) => FileSummary(src, fileData(sums)(root)) }.toList
  }

  /**
    * nsDepth=1 means that `Something/One.flix` and `Something/Two.flix` are counted
    * together under `Something/...`. nsDepth=2 would keep them separate but
    * collect files a level deeper.
    *
    * nsDepth < 1 means all files are kept separate
    */
  private def groupedFileSummaries(sums: List[FileSummary], nsDepth: Option[Int]): List[FileSummary] = {
    def comb(x: FileSummary, y: FileSummary): FileSummary = {
      FileSummary(x.src, x.data.naiveSum(y.data))
    }

    def zero(name: String): FileSummary = FileSummary(Ast.Source(Ast.Input.Text(name, "", stable = true), Array.emptyCharArray, stable = true), FileData.zero)

    sums.groupBy(sum => prefixFileName(sum.src.name, nsDepth)).map {
      case (name, sums) => sums.foldLeft(zero(name))(comb).copy(src = zero(name).src)
    }.toList
  }

  private def prefixFileName(name: String, nsDepth: Option[Int]): String = {
    nsDepth match {
      case None => name
      case Some(depth) =>
        // Note: the separator disagrees with File.separator
        val fileSep = '/'
        name.split(fileSep).toList match {
          case parts if depth > 0 && parts.length > depth =>
            (parts.take(depth) :+ "...").mkString(fileSep.toString)
          case _ => name
        }
    }
  }

  private def fileTotals(l: List[FileSummary]): FileData = {
    FileData.naiveSum(l.map(_.data))
  }

  /**
    * Assumes that IO and Pure are represented simply, i.e. no `{} + {}`,
    * `IO + {}`, or `x - x`.
    */
  private def resEffect(eff: Type): ResEffect = eff match {
    case Type.Pure => ResEffect.Pure
    case Type.IO => ResEffect.JustIO
    case _ if eff.kind == Kind.Eff => ResEffect.Poly
    case _ => throw InternalCompilerException(s"Not an effect: '$eff'", eff.loc)
  }

  private val unknownSource = {
    Ast.Source(Ast.Input.Text("generated", "", stable = true), Array.emptyCharArray, stable = true)
  }

  /** debugSrc is just for consistency checking exceptions */
  private sealed case class FileData(debugSrc: Option[Ast.Source], lines: Int, defs: Int, pureDefs: Int, justIODefs: Int, polyDefs: Int) {
    if (defs != pureDefs + justIODefs + polyDefs) {
      val src = debugSrc.getOrElse(unknownSource)
      throw InternalCompilerException(
        s"${(defs, pureDefs, justIODefs, polyDefs)} does not sum for $src",
        SourceLocation(isReal = true, SourcePosition(src, 0, 0), SourcePosition(src, 0, 0))
      )
    }

    def combine(other: FileData): FileData = {
      if (lines != other.lines) {
        val src = debugSrc.getOrElse(unknownSource)
        throw InternalCompilerException(s"lines '$lines' and '${other.lines}' in $debugSrc",
          SourceLocation(isReal = true, SourcePosition(src, 0, 0), SourcePosition(src, 0, 0))
        )
      }
      FileData(debugSrc.orElse(other.debugSrc), lines, defs + other.defs, pureDefs + other.pureDefs, justIODefs + other.justIODefs, polyDefs + other.polyDefs)
    }

    def naiveSum(other: FileData): FileData = {
      FileData(debugSrc.orElse(other.debugSrc), lines + other.lines, defs + other.defs, pureDefs + other.pureDefs, justIODefs + other.justIODefs, polyDefs + other.polyDefs)
    }

    def toRow: List[String] = List(lines, defs, pureDefs, justIODefs, polyDefs).map(format)
  }

  private object FileData {
    val zero: FileData = FileData(None, 0, 0, 0, 0, 0)

    def combine(l: List[FileData]): FileData = if (l.nonEmpty) l.reduce(_.combine(_)) else zero

    def naiveSum(l: List[FileData]): FileData = if (l.nonEmpty) l.reduce(_.naiveSum(_)) else zero

    def header: List[String] = List("lines", "defs", "Pure", "IO", "Eff. Poly.")
  }

  private sealed case class FileSummary(src: Ast.Source, data: FileData) {
    def toRow: List[String] = List(src.name) ++ data.toRow
  }

  private object FileSummary {
    def header: List[String] = List("Module") ++ FileData.header
  }

  private sealed case class DefSummary(fun: Function, eff: ResEffect) {
    def src: Ast.Source = loc.source

    def loc: SourceLocation = fun.loc
  }

  private sealed trait ResEffect

  private object ResEffect {
    case object Pure extends ResEffect

    case object JustIO extends ResEffect

    case object Poly extends ResEffect
  }

  private sealed trait Function {
    def loc: SourceLocation
  }

  private object Function {

    import ca.uwaterloo.flix.language.ast.Symbol

    case class Def(sym: Symbol.DefnSym) extends Function {
      val loc: SourceLocation = sym.loc
    }

    case class TraitFunWithExp(sym: Symbol.SigSym) extends Function {
      val loc: SourceLocation = sym.loc
    }

    case class InstanceFun(sym: Symbol.DefnSym) extends Function {
      val loc: SourceLocation = sym.loc
    }
  }

  /** Formats the given number `n`. */
  private def format(n: Int): String = f"$n%,d".replace(".", ",")

  /** Right-pads the given string `s` to length `l`. */
  private def padR(s: String, l: Int): String = s.padTo(l, ' ')

  /** Left-pads the given string `s` to length `l`. */
  private def padL(s: String, l: Int): String = {
    if (s.length >= l) {
      return s
    }
    val sb = new StringBuilder
    while (sb.length < l - s.length) {
      sb.append(' ')
    }
    sb.append(s)
    sb.toString
  }

  /** Keeps track of max lengths in columns */
  private class RowBuilder() {
    private val rows: ListBuffer[List[String]] = ListBuffer.empty
    /** has the length of the longest list in rows */
    private val maxLens: ListBuffer[Int] = ListBuffer.empty

    def addRow(row: List[String]): Unit = {
      for ((s, i) <- row.iterator.zipWithIndex) {
        if (i >= maxLens.size) maxLens.append(0)
        maxLens(i) = maxLens(i) max s.length
      }
      rows.append(row)
    }

    def addRepeatedRow(content: String): Unit = {
      addRow(maxLens.toList.map(_ => content))
    }

    def getRows: List[List[String]] = {
      rows.map(row => {
        row.iterator.zipWithIndex.map {
          case (s, i) => padL(s, maxLens(i))
        }.toList
      }).toList
    }
  }

}
