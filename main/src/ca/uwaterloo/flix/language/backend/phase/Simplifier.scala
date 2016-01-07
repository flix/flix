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
    def simplify(tast: TypedAst.Directives): SimplifiedAst.Directives =
      SimplifiedAst.Directives(tast.directives map simplify)

    def simplify(tast: TypedAst.Directive): SimplifiedAst.Directive = tast match {
      case TypedAst.Directive.AssertFact(fact, loc) => throw new UnsupportedOperationException // TODO: To be removed?
      case TypedAst.Directive.AssertRule(fact, loc) => throw new UnsupportedOperationException // TODO: To be removed?
      case TypedAst.Directive.Print(fact, loc) => throw new UnsupportedOperationException // TODO: To be removed?
    }
  }

  object Expression {
    def simplify(tast: TypedAst.Expression): SimplifiedAst.Expression = tast match {
      case TypedAst.Expression.Lit(lit, tpe, loc) => Literal.simplify(lit)
      case TypedAst.Expression.Var(ident, tpe, loc) => SimplifiedAst.Expression.Var(ident, tpe, loc)
      case TypedAst.Expression.Ref(name, tpe, loc) => SimplifiedAst.Expression.Ref(name, tpe, loc)
      case TypedAst.Expression.Lambda(annotations, args, body, tpe, loc) => ??? // TODO
      case TypedAst.Expression.Apply(exp, args, tpe, loc) =>
        ???
      case TypedAst.Expression.Unary(op, exp, tpe, loc) =>
        ???
      case TypedAst.Expression.Binary(op, e1, e2, tpe, loc) =>
        ???

      case TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc) => ???

      case TypedAst.Expression.Let(ident, e1, e2, tpe, loc) => ???

      case TypedAst.Expression.Match(exp, rules, tpe, loc) => ???

      case TypedAst.Expression.Tag(enum, tag, exp, tpe, loc) => ???

      case TypedAst.Expression.Tuple(elms, tpe, loc) => ???

      case TypedAst.Expression.Set(elms, tpe, loc) => ???

      case TypedAst.Expression.Error(tpe, loc) => ???

      case TypedAst.Expression.NativeField(field, tpe, loc) => throw new UnsupportedOperationException // TODO: To be removed?

      case TypedAst.Expression.NativeMethod(method, tpe, loc) => throw new UnsupportedOperationException // TODO: To be removed?
    }
  }

  object Literal {
    def simplify(tast: TypedAst.Literal): SimplifiedAst.Expression = tast match {
      case TypedAst.Literal.Unit(loc) => SimplifiedAst.Expression.Unit(loc)
      case TypedAst.Literal.Bool(b, loc) =>
        if (b) SimplifiedAst.Expression.True(loc) else SimplifiedAst.Expression.False(loc)
      case TypedAst.Literal.Int(i, loc) => SimplifiedAst.Expression.Int(i, loc)
      case TypedAst.Literal.Str(s, loc) => SimplifiedAst.Expression.Str(s, loc)
      case TypedAst.Literal.Tag(enum, tag, lit, tpe, loc) => SimplifiedAst.Expression.Tag(enum, tag, simplify(lit), tpe, loc)
      case TypedAst.Literal.Tuple(elms, tpe, loc) => SimplifiedAst.Expression.Tuple(elms map simplify, tpe, loc)
      case TypedAst.Literal.Set(elms, tpe, loc) => SimplifiedAst.Expression.Set(elms map simplify, tpe, loc)
    }
  }

  object Predicate {

    object Head {
      def simplify(tast: TypedAst.Predicate.Head): SimplifiedAst.Predicate.Head = tast match {
        case TypedAst.Predicate.Head.Relation(name, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Head.Relation(name, terms map Term.simplify, tpe, loc)
        case TypedAst.Predicate.Head.Error(terms, tpe, loc) => throw new UnsupportedOperationException // TODO: To be removed?
        case TypedAst.Predicate.Head.Trace(terms, tpe, loc) => throw new UnsupportedOperationException // TODO: To be removed?
        case TypedAst.Predicate.Head.Write(terms, path, tpe, loc) => ???
      }
    }

    object Body {
      def simplify(tast: TypedAst.Predicate.Body): SimplifiedAst.Predicate.Body = tast match {
        case TypedAst.Predicate.Body.Collection(name, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Body.Collection(name, terms map Term.simplify, tpe, loc)
        case TypedAst.Predicate.Body.Function(name, terms, tpe, loc) =>
          SimplifiedAst.Predicate.Body.Function(name, terms map Term.simplify, tpe, loc)
        case TypedAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc) =>
          SimplifiedAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc)
        case TypedAst.Predicate.Body.Loop(ident, term, tpe, loc) =>
          SimplifiedAst.Predicate.Body.Loop(ident, Term.simplify(term), tpe, loc)
        case TypedAst.Predicate.Body.Read(terms, path, tpe, loc) => throw new UnsupportedOperationException // TODO: to be removed?
      }
    }

  }

  object Term {
    def simplify(tast: TypedAst.Term.Head): SimplifiedAst.Term.Head = tast match {
      case TypedAst.Term.Head.Var(ident, tpe, loc) => SimplifiedAst.Term.Head.Var(ident, tpe, loc)
      case TypedAst.Term.Head.Lit(lit, tpe, loc) => SimplifiedAst.Term.Head.Lit(???, tpe, loc) // TODO
      case TypedAst.Term.Head.Apply(name, args, tpe, loc) => SimplifiedAst.Term.Head.Apply(name, ???, tpe, loc) // TODO
      case TypedAst.Term.Head.NativeField(field, tpe, loc) => throw new UnsupportedOperationException // TODO: to be removed?
    }

    def simplify(tast: TypedAst.Term.Body): SimplifiedAst.Term.Body = tast match {
      case TypedAst.Term.Body.Wildcard(tpe, loc) => SimplifiedAst.Term.Body.Wildcard(tpe, loc)
      case TypedAst.Term.Body.Var(ident, tpe, loc) => SimplifiedAst.Term.Body.Var(ident, tpe, loc)
      case TypedAst.Term.Body.Lit(lit, tpe, loc) => ??? // TODO
    }
  }

  def simplify(tast: TypedAst.Attribute): SimplifiedAst.Attribute =
    SimplifiedAst.Attribute(tast.ident, tast.tpe)

}
