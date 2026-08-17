/*
 * Copyright 2026 Werner Stein
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
package ca.uwaterloo.flix.language.ast

/**
  * The disambiguator suffix appended to a symbol's rendered name, kept apart from the
  * source-level namespace and text that names it.
  *
  * A symbol whose id is `None` renders under its bare text alone; `Some(id)` appends
  * `id.render` after the symbol's own delimiter. Keeping [[Counter]] and [[Hash]] as
  * distinct cases, rather than both being plain `Int`/`String`, means a symbol can never
  * hold an id that is ambiguous about which kind of disambiguator it is: a reader (or the
  * type checker) does not have to guess from context, or from the value's shape, whether a
  * given id is a GenSym allocation counter or a hash of a caller-supplied key.
  */
sealed trait SymId {

  /** Returns this id as it appears in a generated name. */
  def render: String = this match {
    case SymId.Counter(value) => value.toString
    case SymId.Hash(value) => value
  }

}

object SymId {

  /**
    * A GenSym-minted, order-dependent disambiguator: unique per allocation, but its value
    * depends on how many ids happened to be minted before it.
    */
  case class Counter(value: Int) extends SymId

  /**
    * A hash of a caller-supplied key, already rendered to its display form: stable across
    * runs as long as the key is, and never dependent on a global GenSym allocation counter.
    *
    * That does not make every `Hash` fully content-addressed in the strongest sense --
    * it is only as position-independent as the key it was computed from.
    * [[ca.uwaterloo.flix.language.ast.Symbol.specializedDefnSym]]'s key is purely
    * structural (the definition and the type it is specialized at), but
    * [[ca.uwaterloo.flix.language.ast.Symbol.liftedDefnSym]]'s and
    * [[ca.uwaterloo.flix.language.ast.Symbol.specializedAnonClassSym]]'s keys both include
    * an occurrence index counted within their enclosing definition, so those two are
    * stable against edits to *other* definitions, not against reordering lambdas or
    * anonymous classes *within* the same one.
    */
  case class Hash(value: String) extends SymId

}
