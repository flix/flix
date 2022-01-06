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

import java.nio.file.Path
import java.util.Objects

/**
  * A collection of AST nodes that are shared across multiple ASTs.
  */
object Ast {

  /**
    * A common super-type for inputs.
    */
  sealed trait Input

  object Input {

    /**
      * A source that is backed by an internal resource.
      *
      * A source is stable if it cannot change after being loaded (e.g. the standard library, etc).
      */
    case class Text(name: String, text: String, stable: Boolean) extends Input {
      override def hashCode(): Int = name.hashCode

      override def equals(obj: Any): Boolean = obj match {
        case that: Text => this.name == that.name
        case _ => false
      }
    }

    /**
      * A source that is backed by a regular file.
      */
    case class TxtFile(path: Path) extends Input

    /**
      * A source that is backed by flix package file.
      */
    case class PkgFile(path: Path) extends Input

  }

  /**
    * A source is a name and an array of character data.
    *
    * A source is stable if it cannot change after being loaded (e.g. the standard library, etc).
    */
  case class Source(input: Input, data: Array[Char], stable: Boolean) extends Sourceable {

    def name: String = input match {
      case Input.Text(name, _, _) => name
      case Input.TxtFile(path) => path.toString
      case Input.PkgFile(path) => path.toString
    }

    def src: Source = this

    override def equals(o: scala.Any): Boolean = o match {
      case that: Source => this.input == that.input
    }

    override def hashCode(): Int = input.hashCode()
  }

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
    // NB: Deprecated
    case class Benchmark(loc: SourceLocation) extends Annotation {
      override def toString: String = "@benchmark"
    }

    /**
      * An AST node that represents a `@test` annotation.
      *
      * A function marked with `test` is evaluated as part of the test framework.
      *
      * @param loc the source location of the annotation.
      */
    // NB: Deprecated
    case class Test(loc: SourceLocation) extends Annotation {
      override def toString: String = "@test"
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
      * An annotation that marks a construct as experimental.
      *
      * @param loc the source location of the annotation.
      */
    case class Experimental(loc: SourceLocation) extends Annotation {
      override def toString: String = "@Experimental"
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
      * An annotation that indicates the space complexity of a function definition.
      *
      * @param loc the source location of the annotation.
      */
    case class Space(loc: SourceLocation) extends Annotation {
      override def toString: String = "@Space"
    }

    /**
      * An annotation that indicates the time complexity of a function definition.
      *
      * @param loc the source location of the annotation.
      */
    case class Time(loc: SourceLocation) extends Annotation {
      override def toString: String = "@Time"
    }

    /**
      * An annotation that marks a function definition as being inherently unsafe.
      *
      * @param loc the source location of the annotation.
      */
    case class Unsafe(loc: SourceLocation) extends Annotation {
      override def toString: String = "@Unsafe"
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
      * Returns `true` if `this` sequence contains the `@test` annotation.
      */
    def isTest: Boolean = annotations exists (_.isInstanceOf[Annotation.Test])

  }

  /**
    * Documentation.
    *
    * @param lines the lines of the comments.
    * @param loc   the source location of the text.
    */
  case class Doc(lines: List[String], loc: SourceLocation) {
    def text: String = lines.
      dropWhile(_.trim.isEmpty).
      map(_.trim).
      mkString("\n")
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
      * Returns `true` if these modifiers contain the lawless modifier.
      */
    def isLawless: Boolean = mod contains Modifier.Lawless

    /**
      * Returns `true` if these modifiers contain the override modifier.
      */
    def isOverride: Boolean = mod contains Modifier.Override

    /**
      * Returns `true` if these modifiers contain the public modifier.
      */
    def isPublic: Boolean = mod contains Modifier.Public

    /**
      * Returns `true` if these modifiers contain the scoped modifier.
      */
    def isScoped: Boolean = mod contains Modifier.Scoped

    /**
      * Returns `true` if these modifiers contain the sealed modifier.
      */
    def isSealed: Boolean = mod contains Modifier.Sealed

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
      * The lawless modifier.
      */
    case object Lawless extends Modifier

    /**
      * The override modifier.
      */
    case object Override extends Modifier

    /**
      * The public modifier.
      */
    case object Public extends Modifier

    /**
      * The scoped modifier.
      */
    case object Scoped extends Modifier

    /**
      * The sealed modifier.
      */
    case object Sealed extends Modifier

    /**
      * The synthetic modifier.
      */
    case object Synthetic extends Modifier

  }

  /**
    * A common super-type for the denotation of an atom.
    */
  sealed trait Denotation

  object Denotation {

    /**
      * The atom has a relational denotation.
      */
    case object Relational extends Denotation

    /**
      * The atom has a latticenal denotation.
      */
    case object Latticenal extends Denotation

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
    * Represents a positive or negative labelled dependency edge.
    * The labels represent predicate nodes that must co-occur for the dependency to be relevant.
    */
  case class LabelledEdge(head: Name.Pred, p: Polarity, labels: Vector[Label], body: Name.Pred, loc: SourceLocation)

  /**
    * Represents a label in the labelled graph.
    */
  case class Label(pred: Name.Pred, arity: Int)

  object LabelledGraph {
    /**
      * The empty labelled graph.
      */
    val empty: LabelledGraph = LabelledGraph(Set.empty)
  }

  /**
    * Represents a labelled graph; the dependency graph with additional labels
    * on the edges allowing more accurate filtering. The rule `A :- not B, C` would
    * add dependency edges `B -x> A` and `C -> A`. The labelled graph can then
    * add labels that allow the two edges to be filtered out together. If we
    * look at a program consisting of A, B, and D. then the rule `C -> A`
    * cannot be relevant, but by remembering that B occurred together with A,
    * we can also rule out `B -x> A`. The labelled edges would be `B -[C]-x> A`
    * and `C -[B]-> A`.
    */
  case class LabelledGraph(edges: Set[LabelledEdge]) {
    /**
      * Returns a labelled graph with all labelled edges in `this` and `that` labelled graph.
      */
    def +(that: LabelledGraph): LabelledGraph = {
      if (this eq LabelledGraph.empty)
        that
      else if (that eq LabelledGraph.empty)
        this
      else
        LabelledGraph(this.edges ++ that.edges)
    }

    /**
      * Returns `this` labelled graph including only the edges where all
      * its labels are in `syms` and the arity matches.
      * A rule like
      * `A(ta) :- B(tb), not C(tc).` is represented by `edge(A, pos, {(A, arity(ta)), (B, arity(tb)), (C, arity(tc))}, (B, arity(tb)))` etc.
      * and is only included in the output if `syms` contains all of `A, B, C` and `syms(A) == arity(ta)` etc.
      */
    def restrict(syms: Map[Name.Pred, Int]): LabelledGraph = {
      def check(l: Label): Boolean =
        syms.get(l.pred).contains(l.arity)

      LabelledGraph(edges.filter {
        case LabelledEdge(_, _, labels, _, _) =>
          labels.forall(check)
      })
    }
  }

  object Stratification {
    /**
      * Represents the empty stratification.
      */
    val Empty: Stratification = Stratification(Map.empty)
  }

  /**
    * Represents a stratification that maps every predicate symbol to its stratum.
    */
  case class Stratification(m: Map[Name.Pred, Int])

  /**
    * A hole context consists of a hole symbol and its type together with the local environment.
    */
  case class HoleContext(sym: Symbol.HoleSym, tpe: Type, env: Map[Symbol.VarSym, Type])

  /**
    * Represents that the annotated element is introduced by the class `clazz`.
    */
  case class IntroducedBy(clazz: java.lang.Class[_]) extends scala.annotation.StaticAnnotation

  /**
    * Represents that the annotated element is eliminated by the class `clazz`.
    */
  case class EliminatedBy(clazz: java.lang.Class[_]) extends scala.annotation.StaticAnnotation

  /**
    * Represents that the type `arg` must belong to class `sym`.
    */
  case class TypeConstraint(sym: Symbol.ClassSym, arg: Type, loc: SourceLocation) {
    override def equals(o: Any): Boolean = o match {
      case that: TypeConstraint =>
        this.sym == that.sym && this.arg == that.arg
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym, arg)
  }

  /**
    * Represents that an instance on type `tpe` has the type constraints `tconstrs`.
    */
  case class Instance(tpe: Type, tconstrs: List[Ast.TypeConstraint])

  /**
    * Represents the super classes and instances available for a particular class.
    */
  case class ClassContext(superClasses: List[Symbol.ClassSym], instances: List[Ast.Instance])

  /**
    * Represents a derivation on an enum (e.g. `enum E with Eq`).
    */
  case class Derivation(clazz: Symbol.ClassSym, loc: SourceLocation)

  /**
    * Represents the way a variable is bound.
    */
  sealed trait BoundBy

  object BoundBy {

    /**
      * Represents a variable that is bound by a formal parameter.
      */
    case object FormalParam extends BoundBy

    /**
      * Represents a variable that is bound by a let-binding.
      */
    case object Let extends BoundBy

    /**
      * Represents a variable that is bound by a pattern.
      */
    case object Pattern extends BoundBy

    /**
      * Represents a variable that is bound by a select.
      */
    case object SelectRule extends BoundBy

    /**
      * Represents a variable that is bound by a catch rule.
      */
    case object CatchRule extends BoundBy

    /**
      * Represents a variable that is bound by a constraint.
      */
    case object Constraint extends BoundBy

  }

}
