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
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable.ListBuffer

object Summary {

  /**
    * Returns a table of the file data of the root
    *
    * Example with markdown rendering (just a single data row):
    * {{{
    * |               Module |  lines |  defs |  Pure |  IO | Eff. Poly. |
    * | -------------------- | ------ | ----- | ----- | --- | ---------- |
    * |         Adaptor.flix |    242 |    21 |     8 |   5 |          8 |
    * |                  ... |    ... |   ... |   ... | ... |        ... |
    * |               Totals | 37,877 | 3,519 | 1,998 | 149 |      1,372 |
    * }}}
    *
    * @param root the root to create data for
    * @param nsDepth after this folder depth, files will be summarized under the
    *                folder
    * @param minLines all files with less lines than this will not be in the
    *                 table but it will still be reflected in the total row
    */
  def fileSummaryTable(root: Root, nsDepth: Option[Int], minLines: Option[Int]): Table = {
    val allSums = groupedFileSummaries(fileSummaries(root), nsDepth)
    val totals = fileTotals(allSums)
    val printedSums = minLines match {
      case Some(min) => allSums.filter(_.data.lines >= min)
      case None => allSums
    }
    val dots = printedSums.lengthIs < allSums.length
    val table = new Table()
    table.addRow(FileSummary.header)
    printedSums.sortBy(_.src.name).map(_.toRow).foreach(table.addRow)
    if (dots) table.addRepeatedRow("...")
    table.addRow("Totals" :: totals.toRow)
    table
  }

  /** Returns a function summary for a def or an instance, depending on the flag */
  private def defSummary(defn: TypedAst.Def, isInstance: Boolean): DefSummary = {
    val fun = if (isInstance) FunctionSym.InstanceFun(defn.sym) else FunctionSym.Def(defn.sym)
    val eff = resEffect(defn.spec.eff)
    DefSummary(fun, eff)
  }

  /** Returns a function summary for a signature, if it has implementation */
  private def defSummary(sig: TypedAst.Sig): Option[DefSummary] = sig.exp match {
    case None => None
    case Some(_) =>
      val fun = FunctionSym.TraitFunWithExp(sig.sym)
      val eff = resEffect(sig.spec.eff)
      Some(DefSummary(fun, eff))
  }

  /** Returns a function summary for every function */
  private def defSummaries(root: Root): List[DefSummary] = {
    val defs = root.defs.values.map(defSummary(_, isInstance = false))
    val instances = root.instances.values.flatten.flatMap(_.defs.map(defSummary(_, isInstance = true)))
    val traits = root.traits.values.flatMap(_.sigs.flatMap(defSummary))
    (defs ++ instances ++ traits).toList
  }

  /**
    * Converts a function summary into file data.
    * Root is used to find file length.
    */
  private def fileData(sum: DefSummary)(implicit root: Root): FileData = {
    val src = sum.fun.loc.source
    val srcLoc = root.sources.getOrElse(src, SourceLocation(isReal = false, SourcePosition(unknownSource, 0, 0), SourcePosition(unknownSource, 0, 0)))
    val pureDefs = if (sum.eff == ResEffect.Pure) 1 else 0
    val justIODefs = if (sum.eff == ResEffect.GroundNonPure) 1 else 0
    val polyDefs = if (sum.eff == ResEffect.Poly) 1 else 0
    FileData(Some(src), srcLoc.endLine, defs = 1, pureDefs, justIODefs, polyDefs)
  }

  /** Combines function summaries into file data. */
  private def fileData(sums: List[DefSummary])(implicit root: Root): FileData = {
    FileData.combine(sums.map(fileData))
  }

  /** Returns a file summary of each individual file. */
  private def fileSummaries(root: Root): List[FileSummary] = {
    val defSums = defSummaries(root)
    defSums.groupBy(_.src).map { case (src, sums) => FileSummary(src, fileData(sums)(root)) }.toList
  }

  /**
    * Returns the given summaries grouped by their folder structure up to the
    * given depth if any. If depth 1 is given, then top level folders are
    * summarized by a single summary.
    *
    * Sources are converted to faux sources to reflect the groupings.
    *
    * nsDepth=1 means that `Something/One.flix` and `Something/Two.flix` are
    * counted together under `Something/...`. nsDepth=2 would keep them separate
    * but collect files a level deeper.
    *
    * nsDepth < 1 means all files are kept separate.
    */
  private def groupedFileSummaries(sums: List[FileSummary], nsDepth: Option[Int]): List[FileSummary] = {
    def comb(x: FileSummary, y: FileSummary): FileSummary = {
      FileSummary(x.src, x.data.naiveSum(y.data))
    }

    def zero(name: String): FileSummary =
      FileSummary(Source(Input.Text(name, "", stable = true, SecurityContext.AllPermissions), Array.emptyCharArray), FileData.zero)

    sums.groupBy(sum => prefixFileName(sum.src.name, nsDepth)).map {
      case (name, sums) => sums.foldLeft(zero(name))(comb).copy(src = zero(name).src)
    }.toList
  }

  /**
    *   - prefixFileName("a/b", None) = "a/b"
    *   - prefixFileName("a/b", Some(1)) = "a/..."
    *   - prefixFileName("a/b", Some(2)) = "a/b"
    *   - prefixFileName("a/b/c", Some(2)) = "a/b/..."
    *   - prefixFileName("a/b", Some(0) = "a/b"
    *   - prefixFileName("a/b", Some(-1) = "a/b"
    */
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
    * Returns the [[ResEffect]] representation of an effect. It is assumed that effects are written
    * "sensibly", e.g. not `Pure + (ef - ef)` or `not IO`.
    */
  private def resEffect(eff: Type): ResEffect = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => ResEffect.Pure
    case _ if eff.typeVars.nonEmpty => ResEffect.Poly
    case _ => ResEffect.GroundNonPure
  }

  private val unknownSource = {
    Source(Input.Text("generated", "", stable = true, SecurityContext.AllPermissions), Array.emptyCharArray)
  }

  /** debugSrc is just for consistency checking exceptions */
  private sealed case class FileData(debugSrc: Option[Source], lines: Int, defs: Int, pureDefs: Int, groundNonPureDefs: Int, polyDefs: Int) {
    if (defs != pureDefs + groundNonPureDefs + polyDefs) {
      val src = debugSrc.getOrElse(unknownSource)
      throw InternalCompilerException(
        s"${(defs, pureDefs, groundNonPureDefs, polyDefs)} does not sum for $src",
        SourceLocation(isReal = true, SourcePosition(src, 0, 0), SourcePosition(src, 0, 0))
      )
    }

    /**
      * Combines two partial FileData from the same file. Line count is asserted
      * to be equal for both data and is left unchanged. The remaining fields
      * are summed.
      */
    private def combine(other: FileData): FileData = {
      if (lines != other.lines) {
        val src = debugSrc.getOrElse(unknownSource)
        throw InternalCompilerException(s"lines '$lines' and '${other.lines}' in $debugSrc",
          SourceLocation(isReal = true, SourcePosition(src, 0, 0), SourcePosition(src, 0, 0))
        )
      }
      FileData(debugSrc.orElse(other.debugSrc), lines, defs + other.defs, pureDefs + other.pureDefs, groundNonPureDefs + other.groundNonPureDefs, polyDefs + other.polyDefs)
    }

    /**
      * Returns new data where each field is summed. This is used for data of
      * different files to compute a total of a folder fx.
      */
    def naiveSum(other: FileData): FileData = {
      FileData(debugSrc.orElse(other.debugSrc), lines + other.lines, defs + other.defs, pureDefs + other.pureDefs, groundNonPureDefs + other.groundNonPureDefs, polyDefs + other.polyDefs)
    }

    def toRow: List[String] = List(lines, defs, pureDefs, groundNonPureDefs, polyDefs).map(format)
  }

  private object FileData {
    val zero: FileData = FileData(None, 0, 0, 0, 0, 0)

    /**
      * Combines a list of partial FileData from the same file. Line count is
      * asserted to be equal for all data and is left unchanged. The remaining
      * fields are summed.
      */
    def combine(l: List[FileData]): FileData = if (l.nonEmpty) l.reduce(_.combine(_)) else zero

    /**
      * Returns new data where each field is summed. This is used for data of
      * different files to compute a total of a folder fx.
      */
    def naiveSum(l: List[FileData]): FileData = if (l.nonEmpty) l.reduce(_.naiveSum(_)) else zero

    def header: List[String] = List("lines", "defs", "Pure", "Ground Eff.", "Eff. Poly.")
  }

  private sealed case class FileSummary(src: Source, data: FileData) {
    def toRow: List[String] = List(src.name) ++ data.toRow
  }

  private object FileSummary {
    def header: List[String] = List("Module") ++ FileData.header
  }

  private sealed case class DefSummary(fun: FunctionSym, eff: ResEffect) {
    def src: Source = loc.source

    def loc: SourceLocation = fun.loc
  }

  /**
    * Represents the direct effect of a function
    *   - `def f(x: Int32): Int32` is `Pure`
    *   - `def f(x: Int32): Unit \ IO` is `GroundNonPure`
    *   - `def f(x: Int32): Unit \ IO + Crash` is `GroundNonPure`
    *   - `def f(x: Array[Int32, r]): IO + r` is `Poly`
    */
  private sealed trait ResEffect

  private object ResEffect {
    case object Pure extends ResEffect

    case object GroundNonPure extends ResEffect

    case object Poly extends ResEffect
  }

  /**
    * This type is used to differentiate between
    *   - normal defs
    *   - instance defs, and
    *   - trait defs with implementation
    */
  private sealed trait FunctionSym {
    def loc: SourceLocation
  }

  private object FunctionSym {

    import ca.uwaterloo.flix.language.ast.Symbol

    case class Def(sym: Symbol.DefnSym) extends FunctionSym {
      val loc: SourceLocation = sym.loc
    }

    case class TraitFunWithExp(sym: Symbol.SigSym) extends FunctionSym {
      val loc: SourceLocation = sym.loc
    }

    case class InstanceFun(sym: Symbol.DefnSym) extends FunctionSym {
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
  class Table() {

    /** The rows collected so far */
    private val rows: ListBuffer[List[String]] = ListBuffer.empty

    /**
      * Has the length of the longest list in rows. Each integer contains the
      * max length of any string in that column.
      */
    private val maxLens: ListBuffer[Int] = ListBuffer.empty

    /** Adds a row to the builder. The rows can have different lengths */
    def addRow(row: List[String]): Unit = insertRow(rows.length, row)

    private def insertRow(idx: Int, row: List[String]): Unit = {
      for ((s, i) <- row.iterator.zipWithIndex) {
        if (i >= maxLens.size) maxLens.append(0)
        maxLens(i) = maxLens(i) max s.length
      }
      rows.insert(idx, row)
    }

    /**
      * Adds a row with the given content in each column. The number of columns
      * is the max length of the previous columns.
      *
      * OBS: if this is the first row, it will have zero columns.
      */
    def addRepeatedRow(content: String): Unit = {
      addRow(maxLens.toList.map(_ => content))
    }

    /**
      * Returns the built rows where all strings are left padded to have
      * consistent column lengths, i.e, all strings in a column is padded to the
      * length of the longest string in the column.
      */
    def getRows: List[List[String]] = {
      rows.map(row => {
        row.iterator.zipWithIndex.map {
          case (s, i) => padL(s, maxLens(i))
        }.toList
      }).toList
    }

    /** Returns the table as a list of lines with latex formatting. */
    def getLatexLines: List[String] = {
      // avoid common illegal character % in latex syntax
      def sanitize(s: String): String = s.replace("%", "\\%")

      def latexLine(l: List[String]): String = l.mkString("", " & ", " \\\\")

      getRows.map(_.map(sanitize)).map(latexLine)
    }

    /** Returns the table as a list of lines with markdown formatting */
    def getMarkdownLines: List[String] = {
      // add | --- | --- | --- | header separation
      if (rows.length >= 2) {
        insertRow(1, maxLens.toList.map(len => "-" * (len max 3)))
      }

      def markdownLine(l: List[String]): String = l.mkString("| ", " | ", " |")

      getRows.map(markdownLine)
    }
  }

}
