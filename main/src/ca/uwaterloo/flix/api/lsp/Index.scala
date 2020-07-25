/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import java.nio.file.Path

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.collection.MultiMap

object Index {
  /**
    * Represents the empty reverse index.
    */
  val empty: Index = Index(Map.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty)

  /**
    * Returns an index for the given expression `exp0`.
    */
  def of(exp0: Expression): Index = empty + exp0

  /**
    * Returns an index for the given pattern `pat00`.
    */
  def of(pat0: Pattern): Index = empty + pat0

  /**
    * Returns an index for the given expression `exp0`.
    */
  def of(enum0: Enum): Index = empty + enum0

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.DefnSym, loc: SourceLocation): Index = Index.empty.copy(defUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.EnumSym, loc: SourceLocation): Index = Index.empty.copy(enumUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.VarSym, loc: SourceLocation): Index = Index.empty.copy(varUses = MultiMap.singleton(sym, loc))
}

/**
  * Represents a reserve index from documents to line numbers to expressions.
  */
case class Index(m: Map[(Path, Int), List[Entity]],
                 defUses: MultiMap[Symbol.DefnSym, SourceLocation],
                 enumUses: MultiMap[Symbol.EnumSym, SourceLocation],
                 varUses: MultiMap[Symbol.VarSym, SourceLocation]) {

  /**
    * Optionally returns the expression in the document at the given `uri` at the given position `pos`.
    */
  // TODO: Add support for multi-line expressions.
  def query(uri: Path, pos: Position): Option[Entity] = {
    // A key consists of a uri and a line number.
    val key = (uri, pos.line)
    m.get(key).flatMap {
      case candidates =>
        // We have all expressions on that uri and on that line.

        // Step 1: Compute all whole range overlap with the given position.
        val filtered = candidates.filter(e => e.loc.beginCol <= pos.character && pos.character <= e.loc.endCol)

        // Step 2: Sort the expressions by their span (i.e. their length).
        val sorted = filtered.sortBy(e => span(e.loc))

        // Print all candidates.
        // println(sorted.map(_.loc.format).mkString("\n"))

        // Step 3: Return the candidate with the smallest span.
        sorted.headOption
    }
  }

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.DefnSym): Set[SourceLocation] = defUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.EnumSym): Set[SourceLocation] = enumUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.VarSym): Set[SourceLocation] = varUses(sym)

  /**
    * Adds the given expression `exp0` to `this` index.
    */
  def +(exp0: Expression): Index = this + Entity.Exp(exp0)

  /**
    * Adds the given pattern `pat0` to `this` index.
    */
  def +(pat0: Pattern): Index = this + Entity.Pat(pat0)

  /**
    * Adds the given enum `enum0` to `this` index.
    */
  def +(enum0: Enum): Index = this + Entity.Enum(enum0)

  /**
    * Adds the given entity `exp0` to `this` index.
    */
  private def +(entity: Entity): Index = {
    // Compute the uri, line, and column of the expression.
    val uri = Path.of(entity.loc.source.name)
    val beginLine = entity.loc.beginLine
    val beginCol = entity.loc.beginCol

    // Compute the other expressions already on that uri and line.
    val otherEntities = m.getOrElse((uri, beginLine), Nil)

    // Prepend the current expression to the other expressions on that uri and line.
    val newEntities = entity :: otherEntities

    // Returns an updated map.
    copy(m = m + ((uri, beginLine) -> newEntities))
  }

  /**
    * Merges two indexes.
    */
  def ++(that: Index): Index = {
    val m3 = that.m.foldLeft(this.m) {
      case (macc, (line, exps1)) =>
        val exps2 = macc.getOrElse(line, Nil)
        val result = exps1 ::: exps2
        macc + (line -> result)
    }
    Index(m3, this.defUses ++ that.defUses, this.enumUses ++ that.enumUses, this.varUses ++ that.varUses)
  }

  /**
    * Returns the span (i.e. length) of the given source location `loc`.
    */
  private def span(loc: SourceLocation): Int =
    if (loc.beginLine == loc.endLine)
      loc.endCol - loc.beginCol
    else
      1000 // TODO: Add support for multi-line expressions.

}