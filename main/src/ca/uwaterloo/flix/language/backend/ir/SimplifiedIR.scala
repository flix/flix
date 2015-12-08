package ca.uwaterloo.flix.language.backend.ir

import java.lang.reflect.{Method, Field}
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast._

sealed trait SimplifiedIR

// TODO:
// Number everything.
// TODO: Write simplifier to eliminate:
// - match
// - switch


object SimplifiedIR {

  case class Root(functions: Map[Name.Resolved, SimplifiedIR.Definition.Function],
                  directives: SimplifiedIR.Directives,
                  lattices: Map[Type, SimplifiedIR.Definition.Lattice],
                  collections: Map[Name.Resolved, SimplifiedIR.Collection],
                  indexes: Map[Name.Resolved, SimplifiedIR.Definition.Index],
                  facts: List[SimplifiedIR.Constraint.Fact],
                  rules: List[SimplifiedIR.Constraint.Rule],
                  time: Time) extends SimplifiedIR {

  }

  sealed trait Definition

  object Definition {

    case class Function(name: Name.Resolved, exp: SimplifiedIR.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Definition

    case class Lattice(tpe: Type,
                       bot: SimplifiedIR.Expression,
                       top: SimplifiedIR.Expression,
                       leq: SimplifiedIR.Expression,
                       lub: SimplifiedIR.Expression,
                       glb: SimplifiedIR.Expression,
                       loc: SourceLocation) extends SimplifiedIR.Definition

    case class Index(name: Name.Resolved, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends SimplifiedIR.Definition

  }

  sealed trait Collection

  object Collection {

    case class Relation(name: Name.Resolved, attributes: List[SimplifiedIR.Attribute], loc: SourceLocation) extends SimplifiedIR.Collection

    case class Lattice(name: Name.Resolved, keys: List[SimplifiedIR.Attribute], values: List[SimplifiedIR.Attribute], loc: SourceLocation) extends SimplifiedIR.Collection

  }


  sealed trait Constraint extends SimplifiedIR

  object Constraint {

    case class Fact(head: SimplifiedIR.Predicate.Head) extends SimplifiedIR.Constraint

    case class Rule(head: SimplifiedIR.Predicate.Head, body: List[SimplifiedIR.Predicate.Body]) extends SimplifiedIR.Constraint

  }

  case class Directives(directives: List[SimplifiedIR.Directive]) extends SimplifiedIR

  sealed trait Directive

  object Directive {

    case class AssertFact(fact: SimplifiedIR.Constraint.Fact, loc: SourceLocation) extends SimplifiedIR.Directive

    case class AssertRule(rule: SimplifiedIR.Constraint.Rule, loc: SourceLocation) extends SimplifiedIR.Directive

    case class Print(name: Name.Resolved, loc: SourceLocation) extends SimplifiedIR.Directive

  }

  sealed trait Literal extends SimplifiedIR {
    def tpe: Type

    def loc: SourceLocation
  }

  object Literal {

    case class Unit(loc: SourceLocation) extends SimplifiedIR.Literal {
      final val tpe = Type.Unit
    }

    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends SimplifiedIR.Literal {
      final val tpe = Type.Bool
    }

    case class Int(lit: scala.Int, loc: SourceLocation) extends SimplifiedIR.Literal {
      final val tpe = Type.Int
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends SimplifiedIR.Literal {
      final val tpe = Type.Str
    }

    case class Tag(enum: Name.Resolved, tag: Name.Ident, lit: SimplifiedIR.Literal, tpe: Type.Enum, loc: SourceLocation) extends SimplifiedIR.Literal

    case class Tuple(elms: List[SimplifiedIR.Literal], tpe: Type.Tuple, loc: SourceLocation) extends SimplifiedIR.Literal

    case class Set(elms: List[SimplifiedIR.Literal], tpe: Type.Set, loc: SourceLocation) extends SimplifiedIR.Literal

  }

  sealed trait Expression extends SimplifiedIR {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case class Lit(literal: SimplifiedIR.Literal, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Ref(name: Name.Resolved, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Lambda(args: List[SimplifiedIR.FormalArg], body: SimplifiedIR.Expression, tpe: Type.Lambda, loc: SourceLocation) extends SimplifiedIR.Expression 
    
    case class Apply(exp: SimplifiedIR.Expression, args: List[SimplifiedIR.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression 

    case class Unary(op: UnaryOperator, exp: SimplifiedIR.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Binary(op: BinaryOperator, exp1: SimplifiedIR.Expression, exp2: SimplifiedIR.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class IfThenElse(exp1: SimplifiedIR.Expression, exp2: SimplifiedIR.Expression, exp3: SimplifiedIR.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Let(ident: Name.Ident, exp1: SimplifiedIR.Expression, exp2: SimplifiedIR.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Tag(name: Name.Resolved, ident: Name.Ident, exp: SimplifiedIR.Expression, tpe: Type.Enum, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Tuple(elms: List[SimplifiedIR.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Set(elms: List[SimplifiedIR.Expression], tpe: Type.Set, loc: SourceLocation) extends SimplifiedIR.Expression

    case class Error(tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

    case class NativeMethod(method: Method, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Expression

  }

  sealed trait Predicate extends SimplifiedIR {
    def tpe: Type

    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedIR.Predicate

    object Head {

      case class Relation(name: Name.Resolved, terms: List[SimplifiedIR.Term.Head], tpe: Type.Predicate, loc: SourceLocation) extends SimplifiedIR.Predicate.Head 
      
      case class Error(terms: List[SimplifiedIR.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedIR.Predicate.Head

    }


    sealed trait Body extends SimplifiedIR.Predicate

    object Body {
      case class Collection(name: Name.Resolved, terms: List[SimplifiedIR.Term.Body], tpe: Type.Predicate, loc: SourceLocation) extends SimplifiedIR.Predicate.Body

      case class Function(name: Name.Resolved, terms: List[SimplifiedIR.Term.Body], tpe: Type.Lambda, loc: SourceLocation) extends SimplifiedIR.Predicate.Body 

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Predicate.Body

      case class Loop(ident: Name.Ident, term: SimplifiedIR.Term.Head, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Predicate.Body
    }

  }

  object Term {

    sealed trait Head extends SimplifiedIR {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {
      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Head

      case class Lit(literal: SimplifiedIR.Literal, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Head

      case class Apply(name: Name.Resolved, args: List[SimplifiedIR.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Head

      case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Head

    }

    sealed trait Body extends SimplifiedIR {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {
      case class Wildcard(tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Body

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Body

      case class Lit(lit: SimplifiedIR.Literal, tpe: Type, loc: SourceLocation) extends SimplifiedIR.Term.Body
    }
  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends SimplifiedIR

  case class FormalArg(ident: Name.Ident, tpe: Type) extends SimplifiedIR

}
