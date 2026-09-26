/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * A common super type for AST nodes that represent annotations.
  */
trait Annotation {
  def loc: SourceLocation
}

object Annotation {

  /**
    * An AST node that represents a `@CompileTest` annotation.
    *
    * A function marked with `CompileTest` is compiled, but not evaluated.
    *
    * @param loc the source location of the annotation.
    */
  case class CompileTest(loc: SourceLocation) extends Annotation {
    override def toString: String = "@CompileTest"
  }

  /**
    * An annotation that marks a function as a default handler for an effect.
    *
    * @param loc the source location of the annotation.
    */
  case class DefaultHandler(loc: SourceLocation) extends Annotation {
    override def toString: String = "@DefaultHandler"
  }

  /**
    * An annotation that marks a construct as deprecated.
    *
    * @param loc the source location of the annotation.
    */
  case class Deprecated(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Deprecated"
  }

  /**
    * An AST node that represents a `@DontInline` annotation.
    *
    * A function marked with `@DontInline` is guaranteed to never be inlined by the compiler.
    *
    * @param loc the source location of the annotation.
    */
  case class DontInline(loc: SourceLocation) extends Annotation {
    override def toString: String = "@DontInline"
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
    * An AST node that represents an `@Inline` annotation.
    *
    * A function marked with `@Inline` is guaranteed to be inlined by the compiler.
    * If it is recursive, it may be unrolled any number of times but never inside
    * its own definition.
    *
    * @param loc the source location of the annotation.
    */
  case class Inline(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Inline"
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
    * An annotation that marks a function definition as a target for the Channel lowering.
    *
    * A function annotated with `LoweringTargetChannel` may not be discarded until lowering has
    * been performed if the program uses Channels.
    *
    * @param loc the source location of the annotation.
    */
  case class LoweringTargetChannel(loc: SourceLocation) extends Annotation {
    override def toString: String = "@LoweringTargetChannel"
  }

  /**
    * An annotation that marks a function definition as a target for the Datalog lowering.
    *
    * A function annotated with `LoweringTargetDatalog` may not be discarded until lowering has
    * been performed if the program uses the Datalog subset of the language.
    *
    * @param loc the source location of the annotation.
    */
  case class LoweringTargetDatalog(loc: SourceLocation) extends Annotation {
    override def toString: String = "@LoweringTargetDatalog"
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
    * An AST node that represents a `@Tailrec` annotation.
    *
    * A function marked with `@Tailrec` is guaranteed to be tail recursive by the compiler.
    *
    * @param loc the source location of the annotation.
    */
  case class TailRecursive(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Tailrec"
  }

  /**
    * An AST node that represents a `@Terminates` annotation.
    *
    * A function marked with `@Terminates` is verified to be structurally recursive by the compiler.
    *
    * @param loc the source location of the annotation.
    */
  case class Terminates(loc: SourceLocation) extends Annotation {
    override def toString: String = "@Terminates"
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
