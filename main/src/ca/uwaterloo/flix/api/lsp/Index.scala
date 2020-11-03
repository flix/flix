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

import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.MultiMap

object Index {
  /**
    * Represents the empty reverse index.
    */
  val empty: Index = Index(Map.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty)

  // TODO: Better division between uses and definitions.

  /**
    * Returns an index for the given case `case0`.
    */
  def of(case0: Case): Index = empty + Entity.Case(case0)

  /**
    * Returns an index for the given definition `def0`.
    */
  def of(def0: Def): Index = empty + Entity.Def(def0)

  /**
    * Returns an index for the given expression `exp0`.
    */
  def of(enum0: Enum): Index = empty + Entity.Enum(enum0)

  /**
    * Returns an index for the given expression `exp0`.
    */
  def of(exp0: Expression): Index = empty + Entity.Exp(exp0)

  /**
    * Returns an index for the formal parameter `fparam0`.
    */
  def of(fparam0: FormalParam): Index = empty + Entity.FormalParam(fparam0)

  /**
    * Returns an index for the given pattern `pat0`.
    */
  def of(pat0: Pattern): Index = empty + Entity.Pattern(pat0)

  /**
    * Returns an index for the given field `f0`.
    */
  def occurrenceOf(field: Name.Field): Index = empty + Entity.Field(field)

  /**
    * Returns an index for the given atom `a0`.
    */
  def of(a0: Predicate.Head.Atom): Index = empty + Entity.Pred(a0.pred)

  /**
    * Returns an index for the given atom `a0`.
    */
  def of(b0: Predicate.Body.Atom): Index = empty + Entity.Pred(b0.pred)

  /**
    * Returns an index for the given local variable definition `sym0`.
    */
  def of(sym0: Symbol.VarSym, tpe0: Type): Index = empty + Entity.LocalVar(sym0, tpe0)

  /**
    * Returns an index with the symbol 'sym' used at location 'loc'.
    */
  def useOf(sym: Symbol.ClassSym, loc: SourceLocation): Index = Index.empty.copy(classUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol 'sym' used at location 'loc'.
    */
  def useOf(sym: Symbol.SigSym, loc: SourceLocation): Index = Index.empty.copy(sigUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.DefnSym, loc: SourceLocation): Index = Index.empty.copy(defUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with a use of the predicate `pred` with the given denotation `den` and type `tpe`.
    */
  def useOf(pred: Name.Pred): Index = Index.empty.copy(predUses = MultiMap.singleton(pred, pred.loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.EnumSym, loc: SourceLocation): Index = Index.empty.copy(enumUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` and `tag` used at location `loc.`
    */
  def useOf(sym: Symbol.EnumSym, tag: Name.Tag): Index = Index.empty.copy(tagUses = MultiMap.singleton((sym, tag), tag.loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.VarSym, loc: SourceLocation): Index = Index.empty.copy(varUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with a def of the given `field`.
    */
  def defOf(field: Name.Field): Index = Index.empty.copy(fieldDefs = MultiMap.singleton(field, field.loc))

  /**
    * Returns an index with a use of the given `field`.
    */
  def useOf(field: Name.Field): Index = Index.empty.copy(fieldUses = MultiMap.singleton(field, field.loc))

}

/**
  * Represents a reserve index from documents to line numbers to expressions.
  */
case class Index(m: Map[(String, Int), List[Entity]],
                 classUses: MultiMap[Symbol.ClassSym, SourceLocation],
                 sigUses: MultiMap[Symbol.SigSym, SourceLocation],
                 defUses: MultiMap[Symbol.DefnSym, SourceLocation],
                 enumUses: MultiMap[Symbol.EnumSym, SourceLocation],
                 tagUses: MultiMap[(Symbol.EnumSym, Name.Tag), SourceLocation],
                 fieldUses: MultiMap[Name.Field, SourceLocation],
                 fieldDefs: MultiMap[Name.Field, SourceLocation],
                 predUses: MultiMap[Name.Pred, SourceLocation],
                 varUses: MultiMap[Symbol.VarSym, SourceLocation]) {

  /**
    * Optionally returns the expression in the document at the given `uri` at the given position `pos`.
    */
  // TODO: Add support for multi-line expressions.
  def query(uri: String, pos: Position): Option[Entity] = {
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
  def usesOf(sym: Symbol.ClassSym): Set[SourceLocation] = classUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.SigSym): Set[SourceLocation] = sigUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.DefnSym): Set[SourceLocation] = defUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.EnumSym): Set[SourceLocation] = enumUses(sym)

  /**
    * Returns all uses of the given predicate `pred`.
    */
  def usesOf(pred: Name.Pred): Set[SourceLocation] = predUses(pred)

  /**
    * Returns all uses of the given symbol `sym` and `tag`.
    */
  def usesOf(sym: Symbol.EnumSym, tag: Name.Tag): Set[SourceLocation] = tagUses((sym, tag))

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.VarSym): Set[SourceLocation] = varUses(sym)

  /**
    * Returns all defs of the given `field`.
    */
  def defsOf(field: Name.Field): Set[SourceLocation] = fieldDefs(field)

  /**
    * Returns all uses of the given `field`.
    */
  def usesOf(field: Name.Field): Set[SourceLocation] = fieldUses(field)

  /**
    * Adds the given entity `exp0` to `this` index.
    */
  private def +(entity: Entity): Index = {
    // Compute the uri, line, and column of the expression.
    val uri = entity.loc.source.name
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
    Index(
      m3,
      this.classUses ++ that.classUses,
      this.sigUses ++ that.sigUses,
      this.defUses ++ that.defUses,
      this.enumUses ++ that.enumUses,
      this.tagUses ++ that.tagUses,
      this.fieldUses ++ that.fieldUses,
      this.fieldDefs ++ that.fieldDefs,
      this.predUses ++ that.predUses,
      this.varUses ++ that.varUses
    )
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