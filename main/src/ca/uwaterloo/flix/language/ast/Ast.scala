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

import ca.uwaterloo.flix.api.{Invokable, InvokableUnsafe}

/**
  * A collection of AST nodes that are shared across multiple ASTs.
  */
object Ast {

  /**
    * A common super type for AST nodes that represent annotations.
    */
  sealed trait Annotation

  object Annotation {

    /**
      * An AST node that represents an `@associative` annotation.
      *
      * An `associative` function is a function that satisfies the associative property, e.g:
      *
      * 1 + (2 + 3) === (1 + 2) + 3.
      *
      * @param loc the source location of the annotation.
      */
    case class Associative(loc: SourceLocation) extends Annotation {
      override def toString: String = "@associative"
    }

    /**
      * An AST node that represents a `@commutative` annotation.
      *
      * A `commutative` function is a function that satisfies the commutative property, e.g:
      *
      * f(1, 2) === f(2, 1).
      *
      * @param loc the source location of the annotation.
      */
    case class Commutative(loc: SourceLocation) extends Annotation {
      override def toString: String = "@commutative"
    }

    /**
      * An AST node that represents an `@internal` annotation.
      *
      * An `internal` function is a non-public function hidden from view.
      *
      * @param loc the source location of the annotation.
      */
    case class Internal(loc: SourceLocation) extends Annotation {
      override def toString: String = "@commutative"
    }

    /**
      * An AST node that represents a `@monotone` annotation.
      *
      * A `monotone` function is an order-preserving function between lattice elements.
      *
      * @param loc the source location of the annotation.
      */
    case class Monotone(loc: SourceLocation) extends Annotation {
      override def toString: String = "@monotone"
    }

    /**
      * An AST node that represents a `@strict` annotation.
      *
      * A `strict` function is a function that when applied to (any) bottom element yields bottom.
      *
      * @param loc the source location of the annotation.
      */
    case class Strict(loc: SourceLocation) extends Annotation {
      override def toString: String = "@strict"
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
    * A sequence of annotations.
    *
    * @param annotations the annotations.
    */
  case class Annotations(annotations: List[Annotation]) {

    /**
      * Returns `true` if `this` sequence contains the `@associative` annotation.
      */
    def isAssociative: Boolean = annotations exists (_.isInstanceOf[Annotation.Associative])

    /**
      * Returns `true` if `this` sequence contains the `@commutative` annotation.
      */
    def isCommutative: Boolean = annotations exists (_.isInstanceOf[Annotation.Commutative])

    /**
      * Returns `true` if `this` sequence contains the `@monotone` annotation.
      */
    def isMonotone: Boolean = annotations exists (_.isInstanceOf[Annotation.Monotone])

    /**
      * Returns `true` if `this` sequence contains the `@strict` annotation.
      */
    def isStrict: Boolean = annotations exists (_.isInstanceOf[Annotation.Strict])

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
    * @param text the text of the documentation comment.
    * @param loc  the source location of the comment.
    */
  case class Documentation(text: String, loc: SourceLocation)

  /**
    * A common super-type for hooks.
    */
  sealed trait Hook {
    /**
      * Returns the fully qualified name of the hook.
      */
    def name: Symbol.Resolved

    /**
      * Returns the type of the hook.
      */
    def tpe: Type
  }

  object Hook {

    /**
      * A reference to an implementation of the [[Invokable]] interface.
      *
      * @param name the fully qualified name.
      * @param inv  the functional object.
      * @param tpe  the type of the function.
      */
    case class Safe(name: Symbol.Resolved, inv: Invokable, tpe: Type) extends Hook

    /**
      * A reference to an implementation of the [[InvokableUnsafe]] interface.
      *
      * @param name the fully qualified name.
      * @param inv  the functional object.
      * @param tpe  the type of the function.
      */
    case class Unsafe(name: Symbol.Resolved, inv: InvokableUnsafe, tpe: Type) extends Hook

  }


}
