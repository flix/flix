/*
 * Copyright 2021 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}

/**
  * A common super-type for code hints.
  */
trait CodeHint {
  def summary: String

  def severity: Severity

  def loc: SourceLocation
}

object CodeHint {

  /**
    * A code hint that indicates a deprecation.
    *
    * @param loc the location of the expression.
    */
  case class Deprecated(loc: SourceLocation) extends CodeHint {
    def summary: String = s"Deprecated."

    def severity: Severity = Severity.Info
  }

  /**
    * A code hint that indicates an experimental feature.
    *
    * @param loc the location of the expression.
    */
  case class Experimental(loc: SourceLocation) extends CodeHint {
    def summary: String = s"Experimental feature: may be changed or removed without warning!"

    def severity: Severity = Severity.Info
  }

  /**
    * A code hint that indicates laziness.
    *
    * @param loc the location of the expression.
    */
  case class Lazy(loc: SourceLocation) extends CodeHint {
    def summary: String = s"Uses lazy evaluation."

    def severity: Severity = Severity.Hint
  }

  /**
    * A code hint that indicates that a purity reflective operation is lazy.
    *
    * @param sym the symbol of the operation that is lazy.
    * @param loc the location associated with the code hint.
    */
  case class LazyEvaluation(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = s"Lazy: The operation uses lazy evaluation (due to purity reflection)."

    def severity: Severity = Severity.Hint
  }

  /**
    * A code hint that indicates parallelism.
    *
    * @param loc the location of the expression.
    */
  case class Parallel(loc: SourceLocation) extends CodeHint {
    def summary: String = s"Uses parallel evaluation."

    def severity: Severity = Severity.Hint
  }

  /**
    * A code hint that indicates that a purity reflective operation is parallel.
    *
    * @param sym the symbol of the operation that is parallel.
    * @param loc the location associated with the code hint.
    */
  case class ParallelEvaluation(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = s"Parallel: The operation uses parallel evaluation (due to purity reflection)."

    def severity: Severity = Severity.Hint
  }

  /**
    * A code hint that indicates that an operation could be lazy if given a pure function.
    *
    * @param sym the symbol of the operation that could be lazy.
    * @param loc the location associated with the code hint.
    */
  case class SuggestPurityForLazyEvaluation(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = "Eager: Use a pure function to enable lazy evaluation (see purity reflection)."

    def severity: Severity = Severity.Hint
  }

  /**
    * A code hint that indicates that an operation could be parallel if given a pure function.
    *
    * @param sym the symbol of the operation that could be parallel.
    * @param loc the location associated with the code hint.
    */
  case class SuggestPurityForParallelEvaluation(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = "Sequential: Use a pure function to enable parallel evaluation (see purity reflection)."

    def severity: Severity = Severity.Hint
  }

}
