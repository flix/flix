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

import ca.uwaterloo.flix.language.ast.shared.{Annotation, Denotation, Fixity, Polarity}
import ca.uwaterloo.flix.language.errors.ResolutionError

import java.util.Objects

/**
  * A collection of AST nodes that are shared across multiple ASTs.
  */
object Ast {

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
      * Returns a new modifier sequence with `pub` added.
      */
    def asPublic: Modifiers = if (isPublic) this else Modifiers(Modifier.Public :: mod)

    /**
      * Returns `true` if these modifiers contain the lawful modifier.
      */
    def isLawful: Boolean = mod contains Modifier.Lawful

   /**
     * Returns `true` if these modifiers contain the mutable modifier.
     */
    def isMutable: Boolean = mod contains Modifier.Mutable

    /**
      * Returns `true` if these modifiers contain the override modifier.
      */
    def isOverride: Boolean = mod contains Modifier.Override

    /**
      * Returns `true` if these modifiers contain the public modifier.
      */
    def isPublic: Boolean = mod contains Modifier.Public

    /**
      * Returns `true` if these modifiers contain the sealed modifier.
      */
    def isSealed: Boolean = mod contains Modifier.Sealed

    /**
      * Returns `true` if these modifiers contain the synthetic modifier.
      */
    def isSynthetic: Boolean = mod contains Modifier.Synthetic

    /**
      * Returns a string representation that hides the internals.
      */
    override def toString: String = "Modifiers(...)"

  }

  /**
    * A common super-type for modifiers.
    */
  sealed trait Modifier

  object Modifier {

    /**
      * The lawful modifier.
      */
    case object Lawful extends Modifier

   /**
     * The mutable modifier.
     */

    case object Mutable extends Modifier

    /**
      * The override modifier.
      */
    case object Override extends Modifier

    /**
      * The public modifier.
      */
    case object Public extends Modifier

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
    * Represents a positive or negative labelled dependency edge.
    *
    * The labels represent predicate nodes that must co-occur for the dependency to be relevant.
    */
  case class LabelledEdge(head: Name.Pred, polarity: Polarity, fixity: Fixity, labels: Vector[Label], body: Name.Pred, loc: SourceLocation)

  /**
    * Represents a label in the labelled graph.
    */
  case class Label(pred: Name.Pred, den: Denotation, arity: Int, terms: List[Type])

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
  object LabelledPrecedenceGraph {
    /**
      * The empty labelled graph.
      */
    val empty: LabelledPrecedenceGraph = LabelledPrecedenceGraph(Vector.empty)
  }

  case class LabelledPrecedenceGraph(edges: Vector[LabelledEdge]) {
    /**
      * Returns a labelled graph with all labelled edges in `this` and `that` labelled graph.
      */
    def +(that: LabelledPrecedenceGraph): LabelledPrecedenceGraph = {
      if (this eq LabelledPrecedenceGraph.empty)
        that
      else if (that eq LabelledPrecedenceGraph.empty)
        this
      else
        LabelledPrecedenceGraph(this.edges ++ that.edges)
    }

    /**
      * Returns `this` labelled graph including only the edges where all its labels are in
      * `syms` and the labels match according to `'`labelEq`'`.
      *
      * A rule like `A(ta) :- B(tb), not C(tc).` is represented by `edge(A, pos, {la, lb, lc}, B)` etc.
      * and is only included in the output if `syms` contains all of `la.pred, lb.pred, lc.pred` and `labelEq(syms(A), la)` etc.
      */
    def restrict(syms: Map[Name.Pred, Label], labelEq: (Label, Label) => Boolean): LabelledPrecedenceGraph = {
      def include(l: Label): Boolean = syms.get(l.pred).exists(l2 => labelEq(l, l2))

      LabelledPrecedenceGraph(edges.filter {
        case LabelledEdge(_, _, _, labels, _, _) => labels.forall(include)
      })
    }
  }

  object Stratification {
    /**
      * Represents the empty stratification.
      */
    val empty: Stratification = Stratification(Map.empty)
  }

  /**
    * Represents a stratification that maps every predicate symbol to its stratum.
    */
  case class Stratification(m: Map[Name.Pred, Int])

  /**
    * Represents that the annotated element is introduced by the class `clazz`.
    */
  case class IntroducedBy(clazz: java.lang.Class[?]) extends scala.annotation.StaticAnnotation

  /**
    * Represents that the annotated element is eliminated by the class `clazz`.
    */
  case class EliminatedBy(clazz: java.lang.Class[?]) extends scala.annotation.StaticAnnotation

  case object TraitConstraint {
    /**
      * Represents the head (located class) of a type constraint.
      */
    case class Head(sym: Symbol.TraitSym, loc: SourceLocation)
  }

  /**
    * Represents that the type `arg` must belong to trait `sym`.
    */
  case class TraitConstraint(head: TraitConstraint.Head, arg: Type, loc: SourceLocation) {
    override def equals(o: Any): Boolean = o match {
      case that: TraitConstraint =>
        this.head.sym == that.head.sym && this.arg == that.arg
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(head.sym, arg)
  }

  /**
    * Represents that `cst[tpe1]` and `tpe2` are equivalent types.
    */
  case class EqualityConstraint(cst: Ast.AssocTypeConstructor, tpe1: Type, tpe2: Type, loc: SourceLocation)

  /**
    *
    * Represents that `tpe1` and `tpe2` are equivalent types.
    */
  case class BroadEqualityConstraint(tpe1: Type, tpe2: Type) // TODO ASSOC-TYPES not really an AST feature

  /**
    * Represents a use of an effect sym.
    */
  case class EffectSymUse(sym: Symbol.EffectSym, loc: SourceLocation)

  /**
    * Represents a use of an effect operation sym.
    */
  case class OpSymUse(sym: Symbol.OpSym, loc: SourceLocation)

  /**
    * Represents a use of an enum case sym.
    */
  case class CaseSymUse(sym: Symbol.CaseSym, loc: SourceLocation)

  /**
    * Represents a use of a struct field sym.
    */
  case class StructFieldSymUse(sym: Symbol.StructFieldSym, loc: SourceLocation)

  /**
    * Represents a use of a restrictable enum case sym.
    */
  case class RestrictableCaseSymUse(sym: Symbol.RestrictableCaseSym, loc: SourceLocation)

  /**
    * Represents a use of a restrictable enum sym.
    */
  case class RestrictableEnumSymUse(sym: Symbol.RestrictableEnumSym, loc: SourceLocation)

  /**
    * Represents a use of a defn sym.
    */
  case class DefSymUse(sym: Symbol.DefnSym, loc: SourceLocation)

  /**
    * Represents a use of a class sym.
    */
  case class TraitSymUse(sym: Symbol.TraitSym, loc: SourceLocation)

  /**
    * Represents a use of an associated type sym.
    */
  case class AssocTypeSymUse(sym: Symbol.AssocTypeSym, loc: SourceLocation)

  /**
    * Represents that an instance on type `tpe` has the type constraints `tconstrs`.
    */
  case class Instance(tpe: Type, tconstrs: List[Ast.TraitConstraint])

  /**
    * Represents the super traits and instances available for a particular traits.
    */
  case class TraitContext(superTraits: List[Symbol.TraitSym], instances: List[Ast.Instance])

  /**
    * Represents the definition of an associated type.
    * If this associated type is named `Assoc`, then
    * Assoc[arg] = ret.
    */
  case class AssocTypeDef(arg: Type, ret: Type)

  /**
    * Represents a derivation on an enum (e.g. `enum E with Eq`).
    */
  case class Derivation(trt: Symbol.TraitSym, loc: SourceLocation)

  /**
    * Represents a list of derivations with a source location.
    *
    * The source location spans the entire `with X, Y, Z` clause.
    *
    * If there is no `with`-clause then the source location has zero
    * length and is positioned right after the enum type. For example,
    * if the enum is `enum Color {` then the source position would point
    * to the position right after `r` and have zero width.
    */
  case class Derivations(traits: List[Derivation], loc: SourceLocation)

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

  /**
    * Represents the text of a variable.
    */
  sealed trait VarText {

    /**
      * A measure of precision of the text.
      */
    private def precision: Int = this match {
      case VarText.Absent => 0
      case VarText.SourceText(_) => 2
    }

    /**
      * Returns true if `this` VarText is less precise than `that` VarText.
      *
      * More precise text should be preferred when choosing a text to use when substituting.
      *
      */
    def isStrictlyLessPreciseThan(that: VarText): Boolean = this.precision < that.precision
  }

  object VarText {
    /**
      * The variable has no associated text.
      */
    case object Absent extends VarText

    /**
      * The variable is associated with the string `s` taken directly from the source code.
      */
    case class SourceText(s: String) extends VarText
  }

  /**
    * Enum representing whether a type is ascribed or inferred.
    */
  sealed trait TypeSource

  object TypeSource {
    /**
      * The type is ascribed (present in the source code).
      */
    case object Ascribed extends TypeSource

    /**
      * The type is inferred (absent in the source code).
      */
    case object Inferred extends TypeSource
  }

  /**
    * A constructor for a type alias. (Not a valid type by itself).
    */
  case class AliasConstructor(sym: Symbol.TypeAliasSym, loc: SourceLocation)

  /**
    * A constructor for an associated type. (Not a valid type by itself).
    */
  case class AssocTypeConstructor(sym: Symbol.AssocTypeSym, loc: SourceLocation)

  /**
    * A use of a Flix symbol or import of a Java class.
    */
  sealed trait UseOrImport

  object UseOrImport {

    /**
      * A use of a Flix declaration symbol.
      */
    case class Use(sym: Symbol, alias: Name.Ident, loc: SourceLocation) extends UseOrImport

    /**
      * An import of a Java class.
      */
    case class Import(clazz: Class[?], alias: Name.Ident, loc: SourceLocation) extends UseOrImport
  }

  /**
    * A common super-type for syntactic contexts.
    *
    * A syntactic context is an estimate of the syntactic construct a specific source position is inside.
    */
  sealed trait SyntacticContext

  object SyntacticContext {

    sealed trait Decl extends SyntacticContext

    object Decl {
      case object Enum extends Decl

      case object Instance extends Decl

      case object Module extends Decl

      case object Struct extends Decl

      case object Trait extends Decl

      case object Type extends Decl
    }

    sealed trait Expr extends SyntacticContext

    object Expr {
      case object Constraint extends Expr

      case object Do extends Expr

      case class InvokeMethod(tpe: ca.uwaterloo.flix.language.ast.Type, name: Name.Ident) extends Expr

      case object New extends Expr

      case class StaticFieldOrMethod(e: ResolutionError.UndefinedJvmStaticField) extends Expr

      case class StructAccess(e: ResolutionError.UndefinedStructField) extends Expr

      case object OtherExpr extends Expr
    }

    case object Import extends SyntacticContext

    sealed trait Pat extends SyntacticContext

    object Pat {
      case object OtherPat extends Pat
    }

    sealed trait Type extends SyntacticContext

    object Type {
      case object Eff extends Type

      case object OtherType extends Type
    }

    case object Use extends SyntacticContext

    case object WithClause extends SyntacticContext

    case object Unknown extends SyntacticContext

  }

}
