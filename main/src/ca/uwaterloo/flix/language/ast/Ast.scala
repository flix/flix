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
    * Attribute (column of a relation or lattice).
    *
    * @param ident the name of the attribute.
    * @param tpe   the type of the attribute.
    */
  case class Attribute(ident: Name.Ident, tpe: Type)

  /**
    * Formal Parameter.
    *
    * @param ident the name of the argument.
    * @param tpe   the type of the argument.
    */
  case class FormalParam(ident: Name.Ident, tpe: Type)

  /**
    * A common super-type for hooks.
    */
  sealed trait Hook {
    /**
      * Returns the fully qualified name of the hook.
      */
    def name: Symbol.Resolved

    /**
      * Returns true if the hook is Safe; false otherwise.
      */
    def isSafe: Boolean

    /**
      * Returns the type of the hook.
      */
    def tpe: Type.Lambda
  }

  object Hook {

    /**
      * A reference to an implementation of the [[Invokable]] interface.
      *
      * @param name the fully qualified name.
      * @param inv  the functional object.
      * @param tpe  the type of the function.
      */
    case class Safe(name: Symbol.Resolved, inv: Invokable, tpe: Type.Lambda) extends Hook {
      def isSafe = true
    }

    /**
      * A reference to an implementation of the [[InvokableUnsafe]] interface.
      *
      * @param name the fully qualified name.
      * @param inv  the functional object.
      * @param tpe  the type of the function.
      */
    case class Unsafe(name: Symbol.Resolved, inv: InvokableUnsafe, tpe: Type.Lambda) extends Hook {
      def isSafe = false
    }

  }


}
