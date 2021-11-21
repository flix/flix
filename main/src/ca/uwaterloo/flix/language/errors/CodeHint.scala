/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for code hints.
  */
trait CodeHint extends CompilationMessage {
  val kind: String = "Code Hint"
}

object CodeHint {

  /**
    * A code hint that indicates that an operation is parallel.
    *
    * @param sym the symbol of the operation that is parallel.
    * @param loc the location associated with the code hint.
    */
  case class IsParallel(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = s"Is Lazy" // TODO

    override def severity: Severity = Severity.Hint // TODO

    def message(formatter: Formatter): String = { // TODO
      import formatter._
      s"""${line(kind, source.format)}
         |>> Is Lazy
         |
         |${code(loc, "lazy")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * A code hint that indicates that an operation is lazy.
    *
    * @param sym the symbol of the operation that is lazy.
    * @param loc the location associated with the code hint.
    */
  case class IsLazy(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = s"Is Lazy" // TODO

    override def severity: Severity = Severity.Hint // TODO

    def message(formatter: Formatter): String = { // TODO
      import formatter._
      s"""${line(kind, source.format)}
         |>> Is Lazy
         |
         |${code(loc, "lazy")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * A code hint that indicates that an operation could be lazy if given a pure function.
    *
    * @param sym the symbol of the operation that could be lazy.
    * @param loc the location associated with the code hint.
    */
  case class SuggestPurityForLazyEvaluation(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    def summary: String = s"Use of impure function prevents lazy evaluation."

    override def severity: Severity = Severity.Hint

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Use of impure function prevents lazy evaluation.
         |
         |${code(loc, "use of impure function.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * A code hint that indicates that an operation could be parallel if given a pure function.
    *
    * @param sym the symbol of the operation that could be parallel.
    * @param loc the location associated with the code hint.
    */
  case class SuggestPurityForParallelEvaluation(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    override def summary: String = s"Use of impure function prevents parallel evaluation."

    override def severity: Severity = Severity.Hint

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Use of impure function prevents parallel evaluation.
         |
         |${code(loc, "use of impure function.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * A code hint that indicates that an expression has a non-trivial effect.
    *
    * @param loc the location of the expression.
    */
  case class NonTrivialEffect(loc: SourceLocation) extends CodeHint {
    override def summary: String = s"Expression has a non-trivial effect."

    override def severity: Severity = Severity.Info

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Expression has a non-trivial effect.
         |
         |${code(loc, "non-trivial effect.")}
         |""".stripMargin

    }

    def explain(formatter: Formatter): Option[String] = None
  }
}
