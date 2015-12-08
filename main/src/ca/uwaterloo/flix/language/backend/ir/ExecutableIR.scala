package ca.uwaterloo.flix.language.backend.ir

import java.lang.reflect.{Method, Field}
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast._

import scala.collection.mutable

sealed trait ExecutableIR

object ExecutableIR {

  case class Root(functions: Map[Name.Resolved, ExecutableIR.Definition.Function],
                  directives: ExecutableIR.Directives,
                  lattices: Map[Type, ExecutableIR.Definition.Lattice],
                  collections: Map[Name.Resolved, ExecutableIR.Collection],
                  indexes: Map[Name.Resolved, ExecutableIR.Definition.Index],
                  facts: Array[ExecutableIR.Constraint.Fact],
                  rules: Array[ExecutableIR.Constraint.Rule],
                  time: Time) extends ExecutableIR {

    val dependenciesOf: Map[Name.Resolved, mutable.Set[(Constraint.Rule, TypedAst.Predicate.Body.Collection)]] = ???

  }

  sealed trait Definition

  object Definition {

    case class Function(name: Name.Resolved, exp: ExecutableIR.Expression, tpe: Type, loc: SourceLocation) extends ExecutableIR.Definition

    case class Lattice(tpe: Type,
                       bot: ExecutableIR.Expression,
                       top: ExecutableIR.Expression,
                       leq: ExecutableIR.Expression,
                       lub: ExecutableIR.Expression,
                       glb: ExecutableIR.Expression,
                       loc: SourceLocation) extends ExecutableIR.Definition

    case class Index(name: Name.Resolved, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends ExecutableIR.Definition

  }

  sealed trait Collection

  object Collection {

    case class Relation(name: Name.Resolved, attributes: Array[ExecutableIR.Attribute], loc: SourceLocation) extends ExecutableIR.Collection

    case class Lattice(name: Name.Resolved, keys: Array[ExecutableIR.Attribute], values: Array[ExecutableIR.Attribute], loc: SourceLocation) extends ExecutableIR.Collection

  }


  sealed trait Constraint extends ExecutableIR {


  }

  object Constraint {

    case class Fact(head: ExecutableIR.Predicate.Head) extends ExecutableIR.Constraint

    case class Rule(head: ExecutableIR.Predicate.Head, body: Array[ExecutableIR.Predicate.Body]) extends ExecutableIR.Constraint {

      val collections: Array[TypedAst.Predicate.Body.Collection] = body collect {
        case p: TypedAst.Predicate.Body.Collection => p
      }

      val loops: Array[TypedAst.Predicate.Body.Loop] = body collect {
        case p: TypedAst.Predicate.Body.Loop => p
      }

      val filters: Array[TypedAst.Predicate.Body.Function] = body collect {
        case p: TypedAst.Predicate.Body.Function => p
      }

      val disjoint: Array[TypedAst.Predicate.Body.NotEqual] = body collect {
        case p: TypedAst.Predicate.Body.NotEqual => p
      }

      var elapsedTime: Long = 0

      var hitcount: Int = 0
    }

  }

  case class Directives(directives: Array[ExecutableIR.Directive]) extends ExecutableIR {
    /**
      * A collection fact assertions in the program.
      */
    val assertedFacts: Array[TypedAst.Directive.AssertFact] = directives collect {
      case d: TypedAst.Directive.AssertFact => d
    }

    /**
      * A collection of rule assertions in the program.
      */
    val assertedRules: Array[TypedAst.Directive.AssertRule] = directives collect {
      case d: TypedAst.Directive.AssertRule => d
    }

    /**
      * A collection print directives in the program.
      */
    val prints: Array[TypedAst.Directive.Print] = directives collect {
      case d: TypedAst.Directive.Print => d
    }
  }

  sealed trait Directive

  object Directive {

    case class AssertFact(fact: ExecutableIR.Constraint.Fact, loc: SourceLocation) extends ExecutableIR.Directive

    case class AssertRule(rule: ExecutableIR.Constraint.Rule, loc: SourceLocation) extends ExecutableIR.Directive

    case class Print(name: Name.Resolved, loc: SourceLocation) extends ExecutableIR.Directive

  }

  sealed trait Literal extends ExecutableIR {
    def tpe: Type

    def loc: SourceLocation
  }

  object Literal {

    case class Unit(loc: SourceLocation) extends ExecutableIR.Literal {
      final val tpe = Type.Unit
    }

    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends ExecutableIR.Literal {
      final val tpe = Type.Bool
    }

    case class Int(lit: scala.Int, loc: SourceLocation) extends ExecutableIR.Literal {
      final val tpe = Type.Int
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ExecutableIR.Literal {
      final val tpe = Type.Str
    }

    case class Tag(enum: Name.Resolved, tag: Name.Ident, lit: ExecutableIR.Literal, tpe: Type.Enum, loc: SourceLocation) extends ExecutableIR.Literal

    case class Tuple(elms: Array[ExecutableIR.Literal], tpe: Type.Tuple, loc: SourceLocation) extends ExecutableIR.Literal

    case class Set(elms: Array[ExecutableIR.Literal], tpe: Type.Set, loc: SourceLocation) extends ExecutableIR.Literal

  }

  sealed trait Expression extends ExecutableIR {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Interp(e: CodeGenIR.Expression)

    case class Compiled(args: Array[ExecutableIR.FormalArg], body: ExecutableIR.Expression, tpe: Type.Lambda, loc: SourceLocation) extends ExecutableIR.Expression {
      var compiled: Any = ??? // TODO: Link with generated code.
    }

    case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends ExecutableIR.Expression

    case class NativeMethod(method: Method, tpe: Type, loc: SourceLocation) extends ExecutableIR.Expression

  }

  sealed trait Predicate extends ExecutableIR {
    def tpe: Type

    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ExecutableIR.Predicate

    object Head {

      case class Relation(name: Name.Resolved, terms: Array[ExecutableIR.Term.Head], tpe: Type.Predicate, loc: SourceLocation) extends ExecutableIR.Predicate.Head

      case class Error(terms: Array[ExecutableIR.Term.Head], tpe: Type, loc: SourceLocation) extends ExecutableIR.Predicate.Head

    }


    sealed trait Body extends ExecutableIR.Predicate

    object Body {

      case class Collection(name: Name.Resolved, terms: Array[ExecutableIR.Term.Body], tpe: Type.Predicate, loc: SourceLocation) extends ExecutableIR.Predicate.Body

      case class Function(name: Name.Resolved, terms: Array[ExecutableIR.Term.Body], tpe: Type.Lambda, loc: SourceLocation) extends ExecutableIR.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableIR.Predicate.Body

      case class Loop(ident: Name.Ident, term: ExecutableIR.Term.Head, tpe: Type, loc: SourceLocation) extends ExecutableIR.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends ExecutableIR {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Head

      case class Lit(literal: ExecutableIR.Literal, tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Head

      case class Apply(name: Name.Resolved, args: Array[ExecutableIR.Term.Head], tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Head

      case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Head

    }

    sealed trait Body extends ExecutableIR {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Body

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Body

      case class Lit(lit: ExecutableIR.Literal, tpe: Type, loc: SourceLocation) extends ExecutableIR.Term.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends ExecutableIR

  case class FormalArg(ident: Name.Ident, tpe: Type) extends ExecutableIR

}
