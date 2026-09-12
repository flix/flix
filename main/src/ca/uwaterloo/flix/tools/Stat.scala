/*
 * Copyright 2026 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.shared.{Input, Source}
import ca.uwaterloo.flix.language.ast.{SourceLocation, TokenKind, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.Lexer

import java.util.Locale

/**
  * Computes and formats statistics about the source files of a project.
  *
  * The output looks like:
  *
  * {{{
  * my-project 0.3.0
  *
  * 20 files, 4,615 lines: 3,557 code, 555 comment, 503 blank.
  * 18 modules, 281 defs: 220 pure, 19 effectful, 42 effect polymorphic.
  * 19 types, 5 traits, 22 instances, 5 effects.
  * }}}
  */
object Stat {

  /**
    * Statistics about the source files of a project.
    *
    * Only sources that are files on disk are counted, which excludes the standard library and files inside packages.
    *
    * @param lines the line statistics.
    * @param defs  the def statistics.
    * @param decls the statistics for the remaining declaration kinds.
    */
  case class ProjectStat(lines: LineStat, defs: DefStat, decls: DeclStat)

  /**
    * Line statistics for a set of source files.
    *
    * Every line is either blank, code, or comment. A line with both code and a comment counts as code.
    *
    * @param files   the number of source files.
    * @param code    the number of lines on which a non-comment token starts or ends.
    * @param comment the number of non-blank lines without any non-comment token.
    * @param blank   the number of lines that contain only whitespace.
    */
  case class LineStat(files: Int, code: Int, comment: Int, blank: Int) {
    /** The total number of lines. */
    def lines: Int = code + comment + blank

    /** Returns the element-wise sum of `this` and `that`. */
    def +(that: LineStat): LineStat =
      LineStat(files + that.files, code + that.code, comment + that.comment, blank + that.blank)
  }

  /**
    * Statistics about the modules and defs of a project.
    *
    * Only top-level and module-level defs are counted, not instance defs or trait signatures.
    * The three def counts partition the defs by their declared effect.
    *
    * @param modules     the number of modules.
    * @param pure        the number of defs whose effect is `Pure`.
    * @param effectful   the number of defs whose effect is ground and not `Pure`, e.g. `IO`.
    * @param polymorphic the number of defs whose effect contains an effect variable, e.g. `ef` or `IO + ef`.
    */
  case class DefStat(modules: Int, pure: Int, effectful: Int, polymorphic: Int) {
    /** The total number of defs. */
    def defs: Int = pure + effectful + polymorphic
  }

  /**
    * Counts of the remaining declaration kinds of a project.
    *
    * @param types     the number of enums, structs, and restrictable enums.
    * @param traits    the number of traits.
    * @param instances the number of instances.
    * @param effects   the number of effects.
    */
  case class DeclStat(types: Int, traits: Int, instances: Int, effects: Int)

  /**
    * Returns statistics for the project sources in `root`.
    */
  def compute(root: Root): ProjectStat =
    ProjectStat(lineStat(root), defStat(root), declStat(root))

  /**
    * Returns the statistics as text, preceded by `header` (typically the project name and version) if present.
    */
  def format(header: Option[String], s: ProjectStat): String = {
    val lines = List(
      s"${plural(s.lines.files, "file")}, ${plural(s.lines.lines, "line")}: ${fmt(s.lines.code)} code, ${fmt(s.lines.comment)} comment, ${fmt(s.lines.blank)} blank.",
      s"${plural(s.defs.modules, "module")}, ${plural(s.defs.defs, "def")}: ${fmt(s.defs.pure)} pure, ${fmt(s.defs.effectful)} effectful, ${fmt(s.defs.polymorphic)} effect polymorphic.",
      s"${plural(s.decls.types, "type")}, ${plural(s.decls.traits, "trait")}, ${plural(s.decls.instances, "instance")}, ${plural(s.decls.effects, "effect")}."
    )
    val all = header.map(h => List(h, "")).getOrElse(Nil) ::: lines
    all.mkString(System.lineSeparator())
  }

  /** Returns the line statistics of the project sources in `root`. */
  private def lineStat(root: Root): LineStat =
    root.sources.keys.filter(isRealFile).map(countLines).foldLeft(LineStat(0, 0, 0, 0))(_ + _)

  /** Returns the module and def statistics of the project sources in `root`. */
  private def defStat(root: Root): DefStat = {
    val defs = root.defs.values.filter(d => isProject(d.loc)).toList
    val pure = defs.count(d => isPure(d.spec.eff))
    val polymorphic = defs.count(d => isPoly(d.spec.eff))
    DefStat(
      modules = root.modules.values.count(m => isProject(m.loc)),
      pure = pure,
      effectful = defs.length - pure - polymorphic,
      polymorphic = polymorphic
    )
  }

  /** Returns the declaration statistics of the project sources in `root`. */
  private def declStat(root: Root): DeclStat =
    DeclStat(
      types = root.enums.values.count(e => isProject(e.loc)) +
        root.structs.values.count(s => isProject(s.loc)) +
        root.restrictableEnums.values.count(e => isProject(e.loc)),
      traits = root.traits.values.count(t => isProject(t.loc)),
      instances = root.instances.values.count(i => isProject(i.loc)),
      effects = root.effects.values.count(e => isProject(e.loc))
    )

  /**
    * Classifies every line of `src` as blank, code, or comment.
    *
    * The source is lexed again, since the tokens kept in the typed root are only the semantic ones.
    * A trailing newline does not start a new line.
    */
  private def countLines(src: Source): LineStat = {
    val data = src.data
    val isBlank = scala.collection.mutable.ArrayBuffer.empty[Boolean]
    var blank = true
    var i = 0
    while (i < data.length) {
      val c = data(i)
      if (c == '\n') {
        isBlank += blank
        blank = true
      } else if (!Character.isWhitespace(c)) {
        blank = false
      }
      i += 1
    }
    if (data.nonEmpty && data(data.length - 1) != '\n') {
      isBlank += blank
    }

    val numLines = isBlank.length
    val hasCode = new Array[Boolean](numLines)
    val (tokens, _) = Lexer.lex(src)
    for (t <- tokens if t.kind != TokenKind.Eof && !t.kind.isComment) {
      var l = math.max(t.start.lineOneIndexed - 1, 0)
      val end = math.min(t.end.lineOneIndexed - 1, numLines - 1)
      while (l <= end) {
        hasCode(l) = true
        l += 1
      }
    }

    var code = 0
    var comment = 0
    var blanks = 0
    for (l <- 0 until numLines) {
      if (isBlank(l)) blanks += 1
      else if (hasCode(l)) code += 1
      else comment += 1
    }
    LineStat(files = 1, code = code, comment = comment, blank = blanks)
  }

  /** Returns `true` if the given location is in a project source. */
  private def isProject(loc: SourceLocation): Boolean = isRealFile(loc.source)

  /** Returns `true` if the given source is a file on disk. */
  private def isRealFile(src: Source): Boolean = src.input match {
    case Input.RealFile(_, _) => true
    case _ => false
  }

  /** Returns `true` if `eff` is the `Pure` effect. */
  private def isPure(eff: Type): Boolean = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => true
    case _ => false
  }

  /** Returns `true` if `eff` is not pure and contains an effect variable. */
  private def isPoly(eff: Type): Boolean = !isPure(eff) && eff.typeVars.nonEmpty

  /** Formats `n` with thousands separators. */
  private def fmt(n: Int): String = "%,d".formatLocal(Locale.US, n)

  /** Formats `n` followed by `noun`, pluralized with an `s` unless `n` is one. */
  private def plural(n: Int, noun: String): String =
    if (n == 1) s"1 $noun" else s"${fmt(n)} ${noun}s"

}
