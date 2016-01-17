package ca.uwaterloo.flix.language.ast

import java.lang.reflect.{Field, Method}

import ca.uwaterloo.flix.language.ast.TypedAst.Type

import scala.collection.mutable

sealed trait ExecutableAst

object ExecutableAst {

  case class Root(functions: Map[Name.Resolved, ExecutableAst.Definition.Constant],
                  directives: ExecutableAst.Directives,
                  lattices: Map[Type, ExecutableAst.Definition.BoundedLattice],
                  collections: Map[Name.Resolved, ExecutableAst.Collection],
                  indexes: Map[Name.Resolved, ExecutableAst.Definition.Index],
                  facts: Array[ExecutableAst.Constraint.Fact],
                  rules: Array[ExecutableAst.Constraint.Rule],
                  time: Time) extends ExecutableAst {

    val dependenciesOf: Map[Name.Resolved, mutable.Set[(Constraint.Rule, ExecutableAst.Predicate.Body.Collection)]] = {
      val result = mutable.Map.empty[Name.Resolved, mutable.Set[(Constraint.Rule, ExecutableAst.Predicate.Body.Collection)]]

      for (rule <- rules) {
        rule.head match {
          case ExecutableAst.Predicate.Head.Relation(name, _, _, _) => result.update(name, mutable.Set.empty)
          case _ => // nop
        }
      }

      for (outerRule <- rules) {
        for (innerRule <- rules) {
          for (body <- innerRule.body) {
            (outerRule.head, body) match {
              case (outer: ExecutableAst.Predicate.Head.Relation, inner: ExecutableAst.Predicate.Body.Collection) =>
                if (outer.name == inner.name) {
                  val deps = result(outer.name)
                  deps += ((innerRule, inner))
                }
              case _ => // nop
            }
          }
        }
      }
      result.toMap
    }

  }

  sealed trait Definition

  object Definition {

    case class Constant(name: Name.Resolved, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Definition

    case class BoundedLattice(tpe: Type,
                              bot: ExecutableAst.Expression,
                              top: ExecutableAst.Expression,
                              leq: ExecutableAst.Expression,
                              lub: ExecutableAst.Expression,
                              glb: ExecutableAst.Expression,
                              loc: SourceLocation) extends ExecutableAst.Definition

    case class Index(name: Name.Resolved, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends ExecutableAst.Definition

  }

  sealed trait Collection

  object Collection {

    case class Relation(name: Name.Resolved, attributes: Array[ExecutableAst.Attribute], loc: SourceLocation) extends ExecutableAst.Collection

    case class Lattice(name: Name.Resolved, keys: Array[ExecutableAst.Attribute], values: Array[ExecutableAst.Attribute], loc: SourceLocation) extends ExecutableAst.Collection

  }


  sealed trait Constraint extends ExecutableAst

  object Constraint {

    case class Fact(head: ExecutableAst.Predicate.Head) extends ExecutableAst.Constraint

    case class Rule(head: ExecutableAst.Predicate.Head, body: Array[ExecutableAst.Predicate.Body]) extends ExecutableAst.Constraint {

      val collections: Array[ExecutableAst.Predicate.Body.Collection] = body collect {
        case p: ExecutableAst.Predicate.Body.Collection => p
      }

      val loops: Array[ExecutableAst.Predicate.Body.Loop] = body collect {
        case p: ExecutableAst.Predicate.Body.Loop => p
      }

      val filters: Array[ExecutableAst.Predicate.Body.Function] = body collect {
        case p: ExecutableAst.Predicate.Body.Function => p
      }

      val disjoint: Array[ExecutableAst.Predicate.Body.NotEqual] = body collect {
        case p: ExecutableAst.Predicate.Body.NotEqual => p
      }

      var elapsedTime: Long = 0

      var hitcount: Int = 0
    }

  }

  case class Directives(directives: Array[ExecutableAst.Directive]) extends ExecutableAst {
    /**
      * A collection fact assertions in the program.
      */
    val assertedFacts: Array[ExecutableAst.Directive.AssertFact] = directives collect {
      case d: ExecutableAst.Directive.AssertFact => d
    }

    /**
      * A collection of rule assertions in the program.
      */
    val assertedRules: Array[ExecutableAst.Directive.AssertRule] = directives collect {
      case d: ExecutableAst.Directive.AssertRule => d
    }

    /**
      * A collection print directives in the program.
      */
    val prints: Array[ExecutableAst.Directive.Print] = directives collect {
      case d: ExecutableAst.Directive.Print => d
    }
  }

  sealed trait Directive

  object Directive {

    case class AssertFact(fact: ExecutableAst.Constraint.Fact, loc: SourceLocation) extends ExecutableAst.Directive

    case class AssertRule(rule: ExecutableAst.Constraint.Rule, loc: SourceLocation) extends ExecutableAst.Directive

    case class Print(name: Name.Resolved, loc: SourceLocation) extends ExecutableAst.Directive

  }

  sealed trait Expression extends ExecutableAst {
    def tpe: Type
  }

  object Expression {

    case object Unit extends ExecutableAst.Expression {
      final val tpe = TypedAst.Type.Unit
    }

    case object True extends ExecutableAst.Expression {
      final val tpe = TypedAst.Type.Bool
    }

    case object False extends ExecutableAst.Expression {
      final val tpe = TypedAst.Type.Bool
    }

    case class Int(lit: scala.Int) extends ExecutableAst.Expression {
      final val tpe = TypedAst.Type.Int
    }

    case class Str(lit: java.lang.String) extends ExecutableAst.Expression {
      final val tpe = TypedAst.Type.Str
    }

    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Ref(name: Name.Resolved, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Lambda(annotations: Ast.Annotations, args: List[ExecutableAst.FormalArg], body: ExecutableAst.Expression, tpe: Type.Lambda, loc: SourceLocation) extends ExecutableAst.Expression {
      // TODO: Move
      val argsAsArray: Array[ExecutableAst.FormalArg] = args.toArray
    }

    case class Apply(exp: ExecutableAst.Expression, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression {
      // TODO: Move
      val argsAsArray: Array[ExecutableAst.Expression] = args.toArray
    }

    case class Unary(op: UnaryOperator, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Binary(op: BinaryOperator, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class IfThenElse(exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, exp3: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Let(ident: Name.Ident, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Tag(name: Name.Resolved, ident: Name.Ident, exp: ExecutableAst.Expression, tpe: Type.Enum, loc: SourceLocation) extends ExecutableAst.Expression

    case class Tuple(elms: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression {
      // TODO: Move
      val asArray: Array[ExecutableAst.Expression] = elms.toArray
    }

    case class Set(elms: List[ExecutableAst.Expression], tpe: Type.Set, loc: SourceLocation) extends ExecutableAst.Expression

    case class Error(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class NativeMethod(method: Method, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

  }

  sealed trait Predicate extends ExecutableAst {
    def tpe: Type

    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ExecutableAst.Predicate

    object Head {

      case class Relation(name: Name.Resolved, terms: Array[ExecutableAst.Term.Head], tpe: Type.Predicate, loc: SourceLocation) extends ExecutableAst.Predicate.Head {
        /**
          * Returns the arity of the predicate.
          */
        val arity: Int = terms.length

        /**
          * Returns the terms as an array.
          */
        // TODO: Move this into a more appropiate IR.
        val termsArray: Array[ExecutableAst.Term.Head] = terms.toArray
      }

      case class Error(terms: Array[ExecutableAst.Term.Head], tpe: Type, loc: SourceLocation) extends ExecutableAst.Predicate.Head

    }


    sealed trait Body extends ExecutableAst.Predicate {

    }

    object Body {

      case class Collection(name: Name.Resolved, terms: Array[ExecutableAst.Term.Body], tpe: Type.Predicate, loc: SourceLocation) extends ExecutableAst.Predicate.Body {
        /**
          * Returns the arity of this collection predicate.
          */
        val arity: Int = terms.length

        /**
          * Returns the terms as an array.
          */
        // TODO: Move this into a more appropiate IR.
        val termsArray: Array[ExecutableAst.Term.Body] = terms.toArray

        // TODO: Move this into a more appropiate IR.
        val index2var: Array[String] = {
          val r = new Array[String](terms.length)
          var i = 0
          while (i < r.length) {
            terms(i) match {
              case ExecutableAst.Term.Body.Var(ident, _, _) =>
                r(i) = ident.name
              case _ => // nop
            }
            i = i + 1
          }
          r
        }
      }

      case class Function(name: Name.Resolved, terms: Array[ExecutableAst.Term.Body], tpe: Type.Lambda, loc: SourceLocation) extends ExecutableAst.Predicate.Body {
        // TODO: Move
        val termsAsArray: Array[ExecutableAst.Term.Body] = terms.toArray
      }

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: ExecutableAst.Term.Head, tpe: Type, loc: SourceLocation) extends ExecutableAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends ExecutableAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class Apply(name: Name.Resolved, args: Array[ExecutableAst.Term.Head], tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head {
        // TODO: Move
        val argsAsArray: Array[ExecutableAst.Term.Head] = args.toArray
      }

      case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

    }

    sealed trait Body extends ExecutableAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      // TODO: Lit/Exp
    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends ExecutableAst

  case class FormalArg(ident: Name.Ident, tpe: Type) extends ExecutableAst

}
