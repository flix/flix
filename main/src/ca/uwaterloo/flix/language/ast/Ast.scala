/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.api.InvokableUnsafe

/**
  * A collection of AST nodes that are shared across multiple ASTs.
  */
object Ast {

  /**
    * A common super type for AST nodes that represent annotations.
    */
  trait Annotation

  object Annotation {

    /**
      * An AST node that represents a `@benchmark` annotation.
      *
      * A function marked with `benchmark` is evaluated as part of the benchmark framework.
      *
      * @param loc the source location of the annotation.
      */
    case class Benchmark(loc: SourceLocation) extends Annotation {
      override def toString: String = "@benchmark"
    }

    /**
      * An AST node that represents an `@internal` annotation.
      *
      * An `internal` function is a non-public function hidden from view.
      *
      * @param loc the source location of the annotation.
      */
    case class Internal(loc: SourceLocation) extends Annotation {
      override def toString: String = "@internal"
    }

    /**
      * An AST node that represents a `@law` annotation.
      *
      * A `law` function is a property (theorem) about the behaviour of one or more functions.
      *
      * @param loc the source location of the annotation.
      */
    case class Law(loc: SourceLocation) extends Annotation {
      override def toString: String = "@law"
    }

    /**
      * An AST node that represents a `@test` annotation.
      *
      * A function marked with `test` is evaluated as part of the test framework.
      *
      * @param loc the source location of the annotation.
      */
    case class Test(loc: SourceLocation) extends Annotation {
      override def toString: String = "@test"
    }

    /**
      * An AST node that represents an `@unchecked` annotation.
      *
      * The properties of a function marked `@unchecked` are not checked by the verifier.
      *
      * E.g. if a function is marked @commutative and @unchecked then
      * no attempt is made to check that the function is actually commutative.
      * However, the compiler and run-time is still permitted to assume that the
      * function is commutative.
      *
      * @param loc the source location of the annotation.
      */
    case class Unchecked(loc: SourceLocation) extends Annotation {
      override def toString: String = "@unchecked"
    }

    /**
      * An AST node that represents an `@unsafe` annotation.
      *
      * A function marked `@unsafe` is permitted to use unsafe operations.
      *
      * @param loc the source location of the annotation.
      */
    case class Unsafe(loc: SourceLocation) extends Annotation {
      override def toString: String = "@unsafe"
    }

  }

  /**
    * Companion object of [[Annotations]].
    */
  object Annotations {
    /**
      * The empty sequence of annotations.
      */
    val Empty: Annotations = Annotations(Nil)
  }

  /**
    * A sequence of annotations.
    */
  case class Annotations(annotations: List[Annotation]) {

    /**
      * Returns `true` if `this` sequence contains the `@benchmark` annotation.
      */
    def isBenchmark: Boolean = annotations exists (_.isInstanceOf[Annotation.Benchmark])

    /**
      * Returns `true` if `this` sequence contains the `@internal` annotation.
      */
    def isInternal: Boolean = annotations exists (_.isInstanceOf[Annotation.Internal])

    /**
      * Returns `true` if `this` sequence contains the `@law` annotation.
      */
    def isLaw: Boolean = annotations exists (_.isInstanceOf[Annotation.Law])

    /**
      * Returns `true` if `this` sequence contains the `@test` annotation.
      */
    def isTest: Boolean = annotations exists (_.isInstanceOf[Annotation.Test])

    /**
      * Returns `true` if `this` sequence contains the `@unchecked` annotation.
      */
    def isUnchecked: Boolean = annotations exists (_.isInstanceOf[Annotation.Unchecked])

    /**
      * Returns `true` if `this` sequence contains the `@unsafe` annotation.
      */
    def isUnsafe: Boolean = annotations exists (_.isInstanceOf[Annotation.Unsafe])
  }

  /**
    * Documentation.
    *
    * @param text the text of the documentation.
    * @param loc  the source location of the text.
    */
  case class Documentation(text: String, loc: SourceLocation)


  /**
    * A common super-type for hooks.
    */
  sealed trait Hook {
    /**
      * Returns the symbol of the hook.
      */
    def sym: Symbol.DefnSym

    /**
      * Returns the type of the hook.
      */
    def tpe: Type
  }

  object Hook {

    /**
      * A reference to an implementation of the [[InvokableUnsafe]] interface.
      *
      * @param sym the symbol of the hook.
      * @param inv the functional object.
      * @param tpe the type of the function.
      */
    case class Unsafe(sym: Symbol.DefnSym, inv: InvokableUnsafe, tpe: Type) extends Hook

  }

  /**
    * Companion object of [[Modifiers]].
    */
  object Modifiers {
    /**
      * The empty sequence of modifiers.
      */
    val Empty: Modifiers = Modifiers(Nil)
  }

  /**
    * A sequence of modifiers.
    */
  case class Modifiers(mod: List[Modifier]) {
    /**
      * Returns `true` if these modifiers contain the inline modifier.
      */
    def isInline: Boolean = mod contains Modifier.Inline

    /**
      * Returns `true` if these modifiers contain the synthetic modifier.
      */
    def isSynthetic: Boolean = mod contains Modifier.Synthetic
  }

  /**
    * A common super-type for modifiers.
    */
  sealed trait Modifier

  object Modifier {

    /**
      * The inline modifier.
      */
    case object Inline extends Modifier

    /**
      * The synthetic modifier.
      */
    case object Synthetic extends Modifier

  }

  /**
    * A common super-type for the polarity of an atom.
    */
  sealed trait Polarity

  object Polarity {

    /**
      * The atom is positive.
      */
    case object Positive extends Polarity

    /**
      * The atom is negative.
      */
    case object Negative extends Polarity

  }

  /**
    * A hole context consists of a hole symbol and a set of local variables in scope.
    */
  case class HoleContext(sym: Symbol.HoleSym, tpe: Type, env: Set[(Symbol.VarSym, Type)])

  /**
    * Represents that the annotated element is introduced by the class `clazz`.
    */
  case class IntroducedBy(clazz: java.lang.Class[_]) extends scala.annotation.StaticAnnotation

  /**
    * Represents that the annotated element is eliminated by the class `clazz`.
    */
  case class EliminatedBy(clazz: java.lang.Class[_]) extends scala.annotation.StaticAnnotation

}
