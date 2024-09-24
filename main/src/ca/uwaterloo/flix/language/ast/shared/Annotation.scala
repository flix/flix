/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.SourceLocation

/** A common super type for AST nodes that represent annotations. */
trait Annotation {
  def loc: SourceLocation
}

object Annotation {

  /**
    * An annotation that marks a construct as deprecated.
    *
    * @param loc the source location of the annotation.
    */
  case class Deprecated(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Deprecated"
  }

  /**
    * An annotation that marks a construct as experimental.
    *
    * @param loc the source location of the annotation.
    */
  case class Experimental(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Experimental"
  }

  /**
    * An annotation that marks a function to exported.
    *
    * @param loc the source location of the annotation.
    */
  case class Export(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Export"
  }

  /**
    * An annotation that marks a construct as internal.
    *
    * @param loc the source location of the annotation.
    */
  case class Internal(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Internal"
  }

  /**
    * An annotation that marks a function definition as using parallel evaluation.
    *
    * @param loc the source location of the annotation.
    */
  case class Parallel(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Parallel"
  }

  /**
    * An annotation that marks a function definition as using parallel evaluation when given a pure function argument.
    *
    * @param loc the source location of the annotation.
    */
  case class ParallelWhenPure(loc: SourceLocation) extends Annotation {
    override def toString: String = "@ParallelWhenPure"
  }

  /**
    * An annotation that marks a function definition as using lazy evaluation.
    *
    * @param loc the source location of the annotation.
    */
  case class Lazy(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Lazy"
  }

  /**
    * An annotation that marks a function definition as using lazy evaluation when given a pure function argument.
    *
    * @param loc the source location of the annotation.
    */
  case class LazyWhenPure(loc: SourceLocation) extends Annotation {
    override def toString: String = "@LazyWhenPure"
  }

  /**
    * An annotation that marks a type as must-use.
    *
    * @param loc the source location of the annotation.
    */
  case class MustUse(loc: SourceLocation) extends Annotation {
    override def toString: String = "@MustUse"
  }

  /**
    * An AST node that represents a `@Skip` annotation.
    *
    * A function marked with `Skip` is skipped by the test framework.
    *
    * @param loc the source location of the annotation.
    */
  case class Skip(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Skip"
  }

  /**
    * An AST node that represents a `@Test` annotation.
    *
    * A function marked with `Test` is evaluated as part of the test framework.
    *
    * @param loc the source location of the annotation.
    */
  case class Test(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Test"
  }

  /**
    * An AST node that represents a `@TailRec` annotation.
    *
    * A function marked with `@TailRec` is guaranteed to be tail recursive by the compiler.
    *
    * @param loc the source location of the annotation.
    */
  case class TailRecursive(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Tailrec"
  }

  /**
    * An AST node that represents an undefined (i.e. erroneous) annotation.
    *
    * @param name the name of the annotation.
    * @param loc  the source location of the annotation.
    */
  case class Error(name: String, loc: SourceLocation) extends Annotation {
    override def toString: String = "@" + name
  }

}
