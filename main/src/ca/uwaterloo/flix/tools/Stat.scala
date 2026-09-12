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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Token, TokenKind, Type, TypeConstructor}
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
    * Statistics about a set of source files.
    *
    *   - `lines` is the sum of `code`, `comment`, and `blank`.
    *   - `defs` counts top-level and module-level `def` declarations (not instance defs or trait signatures).
    *   - `pureDefs`, `effectfulDefs`, and `polyDefs` partition `defs` by declared effect: `Pure`, a ground
    *     effect such as `IO`, or an effect containing a variable.
    *   - `types` is the number of enums and structs.
    */
  case class ProjectStat(files: Int,
                         lines: Int,
                         code: Int,
                         comment: Int,
                         blank: Int,
                         modules: Int,
                         defs: Int,
                         pureDefs: Int,
                         effectfulDefs: Int,
                         polyDefs: Int,
                         types: Int,
                         traits: Int,
                         instances: Int,
                         effects: Int)

  /**
    * Returns `true` if the given source is a file on disk.
    *
    * This excludes the standard library and files inside packages.
    */
  def isRealFile(src: Source): Boolean = src.input match {
    case Input.RealFile(_, _) => true
    case _ => false
  }

  /**
    * Returns statistics for the sources in `root` that satisfy `include`.
    */
  def compute(root: Root, include: Source => Boolean): ProjectStat = {
    def included(loc: SourceLocation): Boolean = include(loc.source)

    val sources = root.sources.keys.filter(include).toList
    // The tokens kept in `root` are only the semantic ones, so each source is lexed again to get them all.
    val lineCounts = sources.map(src => countLines(src, Lexer.lex(src)._1))
    val defs = root.defs.values.filter(d => included(d.loc)).toList
    val pureDefs = defs.count(d => isPure(d.spec.eff))
    val polyDefs = defs.count(d => isPoly(d.spec.eff))

    ProjectStat(
      files = sources.length,
      lines = lineCounts.map(_.total).sum,
      code = lineCounts.map(_.code).sum,
      comment = lineCounts.map(_.comment).sum,
      blank = lineCounts.map(_.blank).sum,
      modules = root.modules.values.count(m => included(m.loc)),
      defs = defs.length,
      pureDefs = pureDefs,
      effectfulDefs = defs.length - pureDefs - polyDefs,
      polyDefs = polyDefs,
      types = root.enums.values.count(e => included(e.loc)) +
        root.structs.values.count(s => included(s.loc)) +
        root.restrictableEnums.values.count(e => included(e.loc)),
      traits = root.traits.values.count(t => included(t.loc)),
      instances = root.instances.values.count(i => included(i.loc)),
      effects = root.effects.values.count(e => included(e.loc))
    )
  }

  /**
    * Returns the statistics as text, preceded by `header` (typically the project name and version) if present.
    */
  def format(header: Option[String], s: ProjectStat): String = {
    val lines = List(
      s"${plural(s.files, "file")}, ${plural(s.lines, "line")}: ${fmt(s.code)} code, ${fmt(s.comment)} comment, ${fmt(s.blank)} blank.",
      s"${plural(s.modules, "module")}, ${plural(s.defs, "def")}: ${fmt(s.pureDefs)} pure, ${fmt(s.effectfulDefs)} effectful, ${fmt(s.polyDefs)} effect polymorphic.",
      s"${plural(s.types, "type")}, ${plural(s.traits, "trait")}, ${plural(s.instances, "instance")}, ${plural(s.effects, "effect")}."
    )
    val all = header.map(h => List(h, "")).getOrElse(Nil) ::: lines
    all.mkString(System.lineSeparator())
  }

  /**
    * The number of code, comment, and blank lines in a single source.
    */
  private case class LineCount(code: Int, comment: Int, blank: Int) {
    def total: Int = code + comment + blank
  }

  /**
    * Classifies every line of `src` as blank, code, or comment.
    *
    *   - A line is blank if it contains only whitespace.
    *   - A line is code if a non-comment token starts or ends on it.
    *   - Any other line is a comment.
    *
    * A line with both code and a comment counts as code. A trailing newline does not start a new line.
    */
  private def countLines(src: Source, tokens: Array[Token]): LineCount = {
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
    LineCount(code, comment, blanks)
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
