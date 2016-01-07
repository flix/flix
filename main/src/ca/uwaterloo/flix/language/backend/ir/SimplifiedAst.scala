package ca.uwaterloo.flix.language.backend.ir

import java.lang.reflect.{Method, Field}
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast._

sealed trait SimplifiedAst

object SimplifiedAst {

  case class Root(constants: Map[Name.Resolved, SimplifiedAst.Definition.Constant],
                  directives: SimplifiedAst.Directives,
                  lattices: Map[Type, SimplifiedAst.Definition.Lattice],
                  collections: Map[Name.Resolved, SimplifiedAst.Collection],
                  indexes: Map[Name.Resolved, SimplifiedAst.Definition.Index],
                  facts: List[SimplifiedAst.Constraint.Fact],
                  rules: List[SimplifiedAst.Constraint.Rule],
                  time: Time) extends SimplifiedAst {

  }

  sealed trait Definition

  object Definition {

    case class Constant(name: Name.Resolved, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Definition

    case class Lattice(tpe: Type,
                       bot: SimplifiedAst.Expression,
                       top: SimplifiedAst.Expression,
                       leq: SimplifiedAst.Expression,
                       lub: SimplifiedAst.Expression,
                       glb: SimplifiedAst.Expression,
                       loc: SourceLocation) extends SimplifiedAst.Definition

    case class Index(name: Name.Resolved, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends SimplifiedAst.Definition

  }

  sealed trait Collection

  object Collection {

    case class Relation(name: Name.Resolved, attributes: List[SimplifiedAst.Attribute], loc: SourceLocation) extends SimplifiedAst.Collection

    case class Lattice(name: Name.Resolved, keys: List[SimplifiedAst.Attribute], values: List[SimplifiedAst.Attribute], loc: SourceLocation) extends SimplifiedAst.Collection

  }


  sealed trait Constraint extends SimplifiedAst

  object Constraint {

    case class Fact(head: SimplifiedAst.Predicate.Head) extends SimplifiedAst.Constraint

    case class Rule(head: SimplifiedAst.Predicate.Head, body: List[SimplifiedAst.Predicate.Body]) extends SimplifiedAst.Constraint

  }

  case class Directives(directives: List[SimplifiedAst.Directive]) extends SimplifiedAst

  sealed trait Directive

  object Directive {

    case class AssertFact(fact: SimplifiedAst.Constraint.Fact, loc: SourceLocation) extends SimplifiedAst.Directive

    case class AssertRule(rule: SimplifiedAst.Constraint.Rule, loc: SourceLocation) extends SimplifiedAst.Directive

    case class Print(name: Name.Resolved, loc: SourceLocation) extends SimplifiedAst.Directive

  }


  sealed trait Expression extends SimplifiedAst {
    def tpe: Type
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe = TypedAst.Type.Unit
    }

    case class True(loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe = TypedAst.Type.Bool
    }

    case class False(loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe = TypedAst.Type.Bool
    }

    case class Int(lit: scala.Int, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe = TypedAst.Type.Int
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe = TypedAst.Type.Str
    }

    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Ref(name: Name.Resolved, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Lambda(args: List[SimplifiedAst.FormalArg], body: SimplifiedAst.Expression, tpe: Type.Lambda, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Apply(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Unary(op: UnaryOperator, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Binary(op: BinaryOperator, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class IfThenElse(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Let(ident: Name.Ident, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tag(enum: Name.Resolved, tag: Name.Ident, exp: SimplifiedAst.Expression, tpe: Type.Enum, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tuple(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Set(elms: List[SimplifiedAst.Expression], tpe: Type.Set, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Error(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

  }

  sealed trait Predicate extends SimplifiedAst {
    def tpe: Type

    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedAst.Predicate

    object Head {

      case class Relation(name: Name.Resolved, terms: List[SimplifiedAst.Term.Head], tpe: Type.Predicate, loc: SourceLocation) extends SimplifiedAst.Predicate.Head

    }


    sealed trait Body extends SimplifiedAst.Predicate

    object Body {

      case class Collection(name: Name.Resolved, terms: List[SimplifiedAst.Term.Body], tpe: Type.Predicate, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Function(name: Name.Resolved, terms: List[SimplifiedAst.Term.Body], tpe: Type.Lambda, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: SimplifiedAst.Term.Head, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends SimplifiedAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class Lit(literal: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      case class Apply(name: Name.Resolved, args: List[SimplifiedAst.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

    }

    sealed trait Body extends SimplifiedAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class Exp(e: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends SimplifiedAst

  case class FormalArg(ident: Name.Ident, tpe: Type) extends SimplifiedAst

}
