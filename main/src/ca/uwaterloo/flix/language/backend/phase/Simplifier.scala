package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.backend.ir.SimplifiedAst

// TODO: This phase:
// - Eliminates switch and match.
// - Introduce TestTag, extract etc.
// - Inlines literals into Exp?
// - Numbers every argument etc.

/**
  * A phase that simplifies a Typed AST by:
  *
  * -
  */
object Simplifier {

  def simplify(tast: TypedAst.Root): SimplifiedAst.Root = {
    val constants = tast.constants.mapValues(Definition.simplify)
    val directives = Directives.simplify(tast.directives)
    val lattices = tast.lattices.mapValues(Definition.simplify)
    val collections = tast.collections.mapValues(Collection.simplify)
    val indexes = tast.indexes.mapValues(Definition.simplify)
    val facts = tast.facts.map(Constraint.simplify)
    val rules = tast.rules.map(Constraint.simplify)
    val time = tast.time

    SimplifiedAst.Root(constants, directives, lattices, collections, indexes, facts, rules, time)
  }

  object Collection {
    def simplify(tast: TypedAst.Collection): SimplifiedAst.Collection = tast match {
      case TypedAst.Collection.Relation(name, attributes, loc) =>
        SimplifiedAst.Collection.Relation(name, attributes.map(Simplifier.simplify), loc)
      case TypedAst.Collection.Lattice(name, keys, values, loc) =>
        SimplifiedAst.Collection.Lattice(name, keys.map(Simplifier.simplify), values.map(Simplifier.simplify), loc)
    }
  }

  object Constraint {
    def simplify(tast: TypedAst.Constraint.Fact): SimplifiedAst.Constraint.Fact =
      SimplifiedAst.Constraint.Fact(Predicate.Head.simplify(tast.head))

    def simplify(tast: TypedAst.Constraint.Rule): SimplifiedAst.Constraint.Rule = {
      val head = Predicate.Head.simplify(tast.head)
      val body = tast.body.map(Predicate.Body.simplify)
      SimplifiedAst.Constraint.Rule(head, body)
    }
  }

  object Definition {
    def simplify(tast: TypedAst.Definition.BoundedLattice): SimplifiedAst.Definition.Lattice = tast match {
      case TypedAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        import Expression.{simplify => s}
        SimplifiedAst.Definition.Lattice(tpe, s(bot), s(top), s(leq), s(lub), s(glb), loc)
    }

    def simplify(tast: TypedAst.Definition.Constant): SimplifiedAst.Definition.Constant =
      SimplifiedAst.Definition.Constant(tast.name, Expression.simplify(tast.exp), tast.tpe, tast.loc)

    def simplify(tast: TypedAst.Definition.Index): SimplifiedAst.Definition.Index =
      SimplifiedAst.Definition.Index(tast.name, tast.indexes, tast.loc)

  }

  object Directives {
    def simplify(tast: TypedAst.Directives): SimplifiedAst.Directives = ???
  }

  object Expression {
    def simplify(tast: TypedAst.Expression): SimplifiedAst.Expression = tast match {
      case TypedAst.Expression.Lit(lit, tpe, loc) => ???
      case TypedAst.Expression.Var(ident, tpe, loc) => SimplifiedAst.Expression.Var(ident, tpe, loc)
      case TypedAst.Expression.Ref(name, tpe, loc) => SimplifiedAst.Expression.Ref(name, tpe, loc)
    }
  }

  object Literal {
    // TODO: or inline?
    def simplify(tast: TypedAst.Literal): SimplifiedAst.Expression = tast match {
      case TypedAst.Literal.Unit(loc) => SimplifiedAst.Expression.Unit(loc)
      //
    }

  }

  object Predicate {

    object Head {
      def simplify(tast: TypedAst.Predicate.Head): SimplifiedAst.Predicate.Head = ???
    }

    object Body {
      def simplify(tast: TypedAst.Predicate.Body): SimplifiedAst.Predicate.Body = ???
    }

  }

  def simplify(tast: TypedAst.Attribute): SimplifiedAst.Attribute = ???

}
