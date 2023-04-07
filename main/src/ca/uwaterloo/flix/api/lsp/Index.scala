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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.collection.mutable.ArrayBuffer

object Index {
  /**
    * Represents the empty reverse index.
    */
  val empty: Index = Index(Map.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty,
    MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty, MultiMap.empty,
    MultiMap.empty, MultiMap.empty, MultiMap.empty)

  /**
    * Returns an index for the given `class0`.
    */
  def occurrenceOf(class0: TypedAst.Class): Index = empty + Entity.Class(class0)

  /**
    * Returns an index for the given `case0`.
    */
  def occurrenceOf(case0: Case): Index = empty + Entity.Case(case0)

  /**
    * Returns an index for the given `defn0`.
    */
  def occurrenceOf(defn0: Def): Index = empty + Entity.Def(defn0)

  /**
    * Returns an index for the given `sig0`.
    */
  def occurrenceOf(sig0: Sig): Index = empty + Entity.Sig(sig0)

  /**
    * Returns an index for the given `enum0`.
    */
  def occurrenceOf(enum0: Enum): Index = empty + Entity.Enum(enum0)

  /**
    * Returns an index for the given `alias0`.
    */
  def occurrenceOf(alias0: TypeAlias): Index = empty + Entity.TypeAlias(alias0)

  /**
    * Returns an index for the given `alias0`.
    */
  def occurrenceOf(assoc: AssociatedTypeSig): Index = empty + Entity.AssocType(assoc)

  /**
    * Returns an index for the given `exp0`.
    */
  def occurrenceOf(exp0: Expression): Index = empty + Entity.Exp(exp0)

  /**
    * Returns an index for the given `fparam0`.
    */
  def occurrenceOf(fparam0: FormalParam): Index = empty + Entity.FormalParam(fparam0)

  /**
    * Returns an index for the given `pat0`.
    */
  def occurrenceOf(pat0: Pattern): Index = empty + Entity.Pattern(pat0)

  /**
    * Returns an index for the given field `f0`.
    */
  def occurrenceOf(field: Name.Field): Index = empty + Entity.Field(field)

  /**
    * Returns an index for the given atom `a0`.
    */
  def occurrenceOf(pred: Name.Pred, tpe0: Type): Index = empty + Entity.Pred(pred, tpe0)

  /**
    * Returns an index for the given type `t`.
    */
  def occurrenceOf(t: Type): Index = empty + Entity.Type(t)

  /**
    * Returns an index for the given local variable `sym0`.
    */
  def occurrenceOf(sym: Symbol.VarSym, tpe0: Type): Index = empty + Entity.LocalVar(sym, tpe0)

  /**
    * Returns an index for the given local variable `sym0`.
    */
  def occurrenceOf(sym: Symbol.KindedTypeVarSym): Index = empty + Entity.TypeVar(sym)

  /**
    * Returns an index for the given effect `sym`.
    */
  def occurrenceOf(eff: Effect): Index = empty + Entity.Effect(eff)

  /**
    * Returns an index for the given effect operation `sym`.
    */
  def occurrenceOf(op: Op): Index = empty + Entity.Op(op)

  /**
    * Returns an index with the symbol 'sym' used at location 'loc'.
    */
  def useOf(sym: Symbol.ClassSym, loc: SourceLocation): Index = Index.empty.copy(classUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol 'sym' used at location 'loc'.
    */
  def useOf(sym: Symbol.SigSym, loc: SourceLocation, parent: Entity): Index =
    Index.empty.copy(sigUses = MultiMap.singleton(sym, loc)) + Entity.SigUse(sym, loc, parent)

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.DefnSym, loc: SourceLocation, parent: Entity): Index =
    Index.empty.copy(defUses = MultiMap.singleton(sym, loc)) + Entity.DefUse(sym, loc, parent)

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.EnumSym, loc: SourceLocation): Index = Index.empty.copy(enumUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.CaseSym, loc: SourceLocation, parent: Entity): Index = Index.empty.copy(tagUses = MultiMap.singleton(sym, loc)) + Entity.CaseUse(sym, loc, parent)

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.TypeAliasSym, loc: SourceLocation): Index = Index.empty.copy(aliasUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.AssocTypeSym, loc: SourceLocation): Index = Index.empty.copy(assocUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.VarSym, loc: SourceLocation, parent: Entity): Index =
    Index.empty.copy(varUses = MultiMap.singleton(sym, loc)) + Entity.VarUse(sym, loc, parent)

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.KindedTypeVarSym, loc: SourceLocation): Index = Index.empty.copy(tvarUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.EffectSym, loc: SourceLocation): Index = Index.empty.copy(effUses = MultiMap.singleton(sym, loc))

  /**
    * Returns an index with the symbol `sym` used at location `loc.`
    */
  def useOf(sym: Symbol.OpSym, loc: SourceLocation, parent: Entity): Index = Index.empty.copy(opUses = MultiMap.singleton(sym, loc)) + Entity.OpUse(sym, loc, parent)

  /**
    * Returns an index with a def of the given `field`.
    */
  def defOf(field: Name.Field): Index = Index.empty.copy(fieldDefs = MultiMap.singleton(field, field.loc))

  /**
    * Returns an index with a use of the given `field`.
    */
  def useOf(field: Name.Field): Index = Index.empty.copy(fieldUses = MultiMap.singleton(field, field.loc))

  /**
    * Returns an index with a def of the predicate `pred`.
    */
  def defOf(pred: Name.Pred): Index = Index.empty.copy(predDefs = MultiMap.singleton(pred, pred.loc))

  /**
    * Returns an index with a use of the predicate `pred`.
    */
  def useOf(pred: Name.Pred): Index = Index.empty.copy(predUses = MultiMap.singleton(pred, pred.loc))

  /**
    * Applies `f` to each element in `xs` and merges the result into a single index.
    */
  def traverse[T](xs: Iterable[T])(f: T => Index): Index = {
    xs.foldLeft(Index.empty) {
      case (acc, idx) => acc ++ f(idx)
    }
  }

}

/**
  * Represents a reserve index from documents to line numbers to expressions.
  */
case class Index(m: Map[(String, Int), List[Entity]],
                 classUses: MultiMap[Symbol.ClassSym, SourceLocation],
                 sigUses: MultiMap[Symbol.SigSym, SourceLocation],
                 defUses: MultiMap[Symbol.DefnSym, SourceLocation],
                 enumUses: MultiMap[Symbol.EnumSym, SourceLocation],
                 aliasUses: MultiMap[Symbol.TypeAliasSym, SourceLocation],
                 assocUses: MultiMap[Symbol.AssocTypeSym, SourceLocation],
                 tagUses: MultiMap[Symbol.CaseSym, SourceLocation],
                 fieldDefs: MultiMap[Name.Field, SourceLocation],
                 fieldUses: MultiMap[Name.Field, SourceLocation],
                 predDefs: MultiMap[Name.Pred, SourceLocation],
                 predUses: MultiMap[Name.Pred, SourceLocation],
                 varUses: MultiMap[Symbol.VarSym, SourceLocation],
                 tvarUses: MultiMap[Symbol.KindedTypeVarSym, SourceLocation],
                 effUses: MultiMap[Symbol.EffectSym, SourceLocation],
                 opUses: MultiMap[Symbol.OpSym, SourceLocation]
                ) {

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

        // Step 2: Get the min expression,
        // primarily by the span (i.e. the length)
        // and secondarily by the precision level of the entity
        filtered.minOption(
          Ordering.by((e: Entity) => span(e.loc))
            .orElse(Ordering.by((e: Entity) => e.precision).reverse))
    }
  }

  /**
    * Returns the expressions in the document at the given `uri` in the given range `range`.
    */
  def queryByRange(uri: String, range: Range): List[Entity] = {
    (range.start.line to range.end.line).flatMap { line =>
      m.getOrElse((uri, line), Nil)
    }.filter { entity => entity.isInRange(range) }.toList
  }


  /**
    * Returns all entities in the document at the given `uri`.
    */
  def query(uri: String): Iterable[Entity] = {
    val res = new ArrayBuffer[Entity]()
    for (((entitiesUri, _), entities) <- m) {
      if (entitiesUri == uri) {
        res.addAll(entities)
      }
    }
    res
  }

  /**
    * Returns all entities in the document at the given `uri` which appear at most
    * `beforeLine`s before `queryLine` and at most `afterLine`s after `queryLine`.
    */
  def queryWithRange(uri: String, queryLine: Int, beforeLine: Int, afterLine: Int): Iterable[Entity] = {
    query(uri).filter {
      case entity =>
        val currentLine = entity.loc.beginLine
        val delta = queryLine - currentLine
        if (delta < 0)
          (-delta) < beforeLine
        else
          delta < afterLine
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
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.TypeAliasSym): Set[SourceLocation] = aliasUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.AssocTypeSym): Set[SourceLocation] = assocUses(sym)

  /**
    * Returns all uses of the given symbol `sym` and `tag`.
    */
  def usesOf(sym: Symbol.CaseSym): Set[SourceLocation] = tagUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.VarSym): Set[SourceLocation] = varUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.KindedTypeVarSym): Set[SourceLocation] = tvarUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.EffectSym): Set[SourceLocation] = effUses(sym)

  /**
    * Returns all uses of the given symbol `sym`.
    */
  def usesOf(sym: Symbol.OpSym): Set[SourceLocation] = opUses(sym)

  /**
    * Returns all defs of the given `field`.
    */
  def defsOf(field: Name.Field): Set[SourceLocation] = fieldDefs(field)

  /**
    * Returns all uses of the given `field`.
    */
  def usesOf(field: Name.Field): Set[SourceLocation] = fieldUses(field)

  /**
    * Returns all defs of the given predicate `pred`.
    */
  def defsOf(pred: Name.Pred): Set[SourceLocation] = predDefs(pred)

  /**
    * Returns all uses of the given predicate `pred`.
    */
  def usesOf(pred: Name.Pred): Set[SourceLocation] = predUses(pred)

  /**
    * Adds the given entity `exp0` to `this` index.
    */
  private def +(entity: Entity): Index = {
    // Do not index synthetic source locations.
    if (entity.loc.isSynthetic) {
      return this
    }

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
      this.aliasUses ++ that.aliasUses,
      this.assocUses ++ that.assocUses,
      this.tagUses ++ that.tagUses,
      this.fieldDefs ++ that.fieldDefs,
      this.fieldUses ++ that.fieldUses,
      this.predDefs ++ that.predDefs,
      this.predUses ++ that.predUses,
      this.varUses ++ that.varUses,
      this.tvarUses ++ that.tvarUses,
      this.effUses ++ that.effUses,
      this.opUses ++ that.opUses
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
