/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast.shared

sealed trait TraitUsageKind

/**
  * Represents the kind of trait use.
  * Used to indicate the context of an undefined trait
  */
object TraitUsageKind {
  /**
    * Represents a trait use in an expression
    * e.g. let res = E...
    */
  case object Expr extends TraitUsageKind

  /**
    * Represents a trait use in a constraint
    * e.g. def f(a: t) : Unit with E...
    * e.g. trait Foo[t] with E...
    */
  case object Constraint extends TraitUsageKind

  /**
    * Represents a trait use in a derivation
    * e.g. enum Color with E...
    */
  case object Derivation extends TraitUsageKind

  /**
    * Represents a trait use in an instance declaration
    * e.g. instance E...
    */
  case object Implementation extends TraitUsageKind
}
