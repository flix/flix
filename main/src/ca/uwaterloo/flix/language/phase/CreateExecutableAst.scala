package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.mutable

/**
  * A phase that transforms a SimplifiedAst into an ExecutableAst.
  * Essentially the identity transform, with a few differences:
  *
  * - Lists are copied to arrays
  */
object CreateExecutableAst {

  def toExecutable(sast: SimplifiedAst.Root): ExecutableAst.Root = {
    val constants = sast.constants.mapValues(Definition.toExecutable)
    val directives = Directives.toExecutable(sast.directives)
    val lattices = sast.lattices.mapValues(Definition.toExecutable)
    val collections = sast.collections.mapValues(Collection.toExecutable)
    val indexes = sast.indexes.mapValues(Definition.toExecutable)
    val facts = sast.facts.map(Constraint.toExecutable).toArray
    val rules = sast.rules.map(Constraint.toExecutable).toArray
    val time = sast.time

    val dependenciesOf: Map[Name.Resolved, mutable.Set[(ExecutableAst.Constraint.Rule, ExecutableAst.Predicate.Body.Collection)]] = {
      val result = mutable.Map.empty[Name.Resolved, mutable.Set[(ExecutableAst.Constraint.Rule, ExecutableAst.Predicate.Body.Collection)]]

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

    ExecutableAst.Root(constants, directives, lattices, collections, indexes, facts, rules, time, dependenciesOf)
  }

  object Definition {
    def toExecutable(sast: SimplifiedAst.Definition.Constant): ExecutableAst.Definition.Constant =
      ExecutableAst.Definition.Constant(sast.name, Expression.toExecutable(sast.exp), sast.tpe, sast.loc)

    def toExecutable(sast: SimplifiedAst.Definition.Lattice): ExecutableAst.Definition.Lattice = sast match {
      case SimplifiedAst.Definition.Lattice(tpe, bot, top, leq, lub, glb, loc) =>
        import Expression.{toExecutable => t}
        ExecutableAst.Definition.Lattice(tpe, t(bot), t(top), t(leq), t(lub), t(glb), loc)
    }

    def toExecutable(sast: SimplifiedAst.Definition.Index): ExecutableAst.Definition.Index =
      ExecutableAst.Definition.Index(sast.name, sast.indexes, sast.loc)

    // TODO: Compile SimplifiedAst.Definition.Function?
  }

  object Collection {
    def toExecutable(sast: SimplifiedAst.Collection): ExecutableAst.Collection = sast match {
      case SimplifiedAst.Collection.Relation(name, attributes, loc) =>
        val attributesArray = attributes.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Collection.Relation(name, attributesArray, loc)
      case SimplifiedAst.Collection.Lattice(name, keys, values, loc) =>
        val keysArray = keys.map(CreateExecutableAst.toExecutable).toArray
        val valuesArray = values.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Collection.Lattice(name, keysArray, valuesArray, loc)
    }
  }

  object Constraint {
    def toExecutable(sast: SimplifiedAst.Constraint.Fact): ExecutableAst.Constraint.Fact =
      ExecutableAst.Constraint.Fact(Predicate.Head.toExecutable(sast.head))

    def toExecutable(sast: SimplifiedAst.Constraint.Rule): ExecutableAst.Constraint.Rule = {
      val head = Predicate.Head.toExecutable(sast.head)
      val body = sast.body.map(Predicate.Body.toExecutable).toArray
      val collections = body.collect { case p: ExecutableAst.Predicate.Body.Collection => p }
      val filters = body.collect { case p: ExecutableAst.Predicate.Body.Function => p }
      val disjoint = body.collect { case p: ExecutableAst.Predicate.Body.NotEqual => p }
      val loops = body.collect { case p: ExecutableAst.Predicate.Body.Loop => p }
      ExecutableAst.Constraint.Rule(head, body, collections, filters, disjoint, loops)
    }
  }

  object Directives {
    def toExecutable(sast: SimplifiedAst.Directives): ExecutableAst.Directives = {
      val directives = sast.directives.map(toExecutable).toArray
      val assertedFacts = directives.collect { case d: ExecutableAst.Directive.AssertFact => d }
      val assertedRules = directives.collect { case d: ExecutableAst.Directive.AssertRule => d }
      val prints = directives.collect { case d: ExecutableAst.Directive.Print => d }
      ExecutableAst.Directives(directives, assertedFacts, assertedRules, prints)
    }

    def toExecutable(sast: SimplifiedAst.Directive): ExecutableAst.Directive = sast match {
      case SimplifiedAst.Directive.AssertFact(fact, loc) => throw new UnsupportedOperationException // TODO: To be removed?
      case SimplifiedAst.Directive.AssertRule(rule, loc) => throw new UnsupportedOperationException // TODO: To be removed?
      case SimplifiedAst.Directive.Print(name, loc) => throw new UnsupportedOperationException // TODO: To be removed?
    }
  }

  object Expression {
    def toExecutable(sast: SimplifiedAst.Expression): ExecutableAst.Expression = sast match {
      case SimplifiedAst.Expression.Unit => ExecutableAst.Expression.Unit
      case SimplifiedAst.Expression.True => ExecutableAst.Expression.True
      case SimplifiedAst.Expression.False => ExecutableAst.Expression.False
      case SimplifiedAst.Expression.Int(lit) => ExecutableAst.Expression.Int(lit)
      case SimplifiedAst.Expression.Int8(lit) => ExecutableAst.Expression.Int8(lit)
      case SimplifiedAst.Expression.Int16(lit) => ExecutableAst.Expression.Int16(lit)
      case SimplifiedAst.Expression.Int32(lit) => ExecutableAst.Expression.Int32(lit)
      case SimplifiedAst.Expression.Int64(lit) => ExecutableAst.Expression.Int64(lit)
      case SimplifiedAst.Expression.Str(lit) => ExecutableAst.Expression.Str(lit)
      case SimplifiedAst.Expression.LoadBool(e, offset) => ExecutableAst.Expression.LoadBool(toExecutable(e), offset)
      case SimplifiedAst.Expression.LoadInt8(e, offset) => ExecutableAst.Expression.LoadInt8(toExecutable(e), offset)
      case SimplifiedAst.Expression.LoadInt16(e, offset) => ExecutableAst.Expression.LoadInt16(toExecutable(e), offset)
      case SimplifiedAst.Expression.LoadInt32(e, offset) => ExecutableAst.Expression.LoadInt32(toExecutable(e), offset)
      case SimplifiedAst.Expression.StoreBool(e, offset, v) =>
        ExecutableAst.Expression.StoreBool(toExecutable(e), offset, toExecutable(v))
      case SimplifiedAst.Expression.StoreInt8(e, offset, v) =>
        ExecutableAst.Expression.StoreInt8(toExecutable(e), offset, toExecutable(v))
      case SimplifiedAst.Expression.StoreInt16(e, offset, v) =>
        ExecutableAst.Expression.StoreInt16(toExecutable(e), offset, toExecutable(v))
      case SimplifiedAst.Expression.StoreInt32(e, offset, v) =>
        ExecutableAst.Expression.StoreInt32(toExecutable(e), offset, toExecutable(v))
      case SimplifiedAst.Expression.Var(ident, offset, tpe, loc) =>
        ExecutableAst.Expression.Var(ident, offset, tpe, loc)
      case SimplifiedAst.Expression.Ref(name, tpe, loc) => ExecutableAst.Expression.Ref(name, tpe, loc)
      case SimplifiedAst.Expression.Lambda(annotations, args, body, tpe, loc) =>
        val argsArray = args.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Expression.Lambda(annotations, argsArray, toExecutable(body), tpe, loc)
      case SimplifiedAst.Expression.Closure(args, body, env, tpe, loc) =>
        val argsArray = args.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Expression.Closure(argsArray, toExecutable(body), env.mapValues(toExecutable), tpe, loc)
      case SimplifiedAst.Expression.Apply(name, args, tpe, loc) =>
        val argsArray = args.map(toExecutable).toArray
        ExecutableAst.Expression.Apply(name, argsArray, tpe, loc)
      case SimplifiedAst.Expression.Apply3(lambda, args, tpe, loc) =>
        val argsArray = args.map(toExecutable).toArray
        ExecutableAst.Expression.Apply3(toExecutable(lambda), argsArray, tpe, loc)
      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
        ExecutableAst.Expression.Unary(op, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.Binary(op, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        ExecutableAst.Expression.IfThenElse(toExecutable(exp1), toExecutable(exp2), toExecutable(exp3), tpe, loc)
      case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.Let(ident, offset, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.CheckTag(tag, exp, loc) =>
        ExecutableAst.Expression.CheckTag(tag, toExecutable(exp), loc)
      case SimplifiedAst.Expression.GetTagValue(exp, tpe, loc) =>
        ExecutableAst.Expression.GetTagValue(toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        ExecutableAst.Expression.Tag(enum, tag, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.GetTupleIndex(base, offset, tpe, loc) =>
        ExecutableAst.Expression.GetTupleIndex(toExecutable(base), offset, tpe, loc)
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val elmsArray = elms.map(toExecutable).toArray
        ExecutableAst.Expression.Tuple(elmsArray, tpe, loc)
      case SimplifiedAst.Expression.CheckNil(exp, loc) => ExecutableAst.Expression.CheckNil(toExecutable(exp), loc)
      case SimplifiedAst.Expression.CheckCons(exp, loc) => ExecutableAst.Expression.CheckCons(toExecutable(exp), loc)
      case SimplifiedAst.Expression.Set(elms, tpe, loc) =>
        val elmsArray = elms.map(toExecutable).toArray
        ExecutableAst.Expression.Set(elmsArray, tpe, loc)
      case SimplifiedAst.Expression.Error(tpe, loc) => ExecutableAst.Expression.Error(tpe, loc)
      case SimplifiedAst.Expression.MatchError(tpe, loc) => ExecutableAst.Expression.MatchError(tpe, loc)
    }
  }

  object Predicate {

    object Head {
      def toExecutable(sast: SimplifiedAst.Predicate.Head): ExecutableAst.Predicate.Head = sast match {
        case SimplifiedAst.Predicate.Head.Relation(name, terms, tpe, loc) =>
          ExecutableAst.Predicate.Head.Relation (name, terms.map(Term.toExecutable).toArray, tpe, loc)
      }
    }

    object Body {
      def toExecutable(sast: SimplifiedAst.Predicate.Body): ExecutableAst.Predicate.Body = sast match {
        case SimplifiedAst.Predicate.Body.Collection(n, terms, tpe, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          val index2var: Array[String] = {
            val r = new Array[String](termsArray.length)
            var i = 0
            while (i < r.length) {
              termsArray(i) match {
                case ExecutableAst.Term.Body.Var(ident, _, _, _) =>
                  r(i) = ident.name
                case _ => // nop
              }
              i = i + 1
            }
            r
          }
          ExecutableAst.Predicate.Body.Collection(n, termsArray, index2var, tpe, loc)
        case SimplifiedAst.Predicate.Body.Function(name, terms, tpe, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Body.Function(name, termsArray, tpe, loc)
        case SimplifiedAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc) =>
          ExecutableAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc)
        case SimplifiedAst.Predicate.Body.Loop(ident, term, tpe, loc) =>
          ExecutableAst.Predicate.Body.Loop(ident, Term.toExecutable(term), tpe, loc)
      }
    }

  }

  object Term {
    def toExecutable(sast: SimplifiedAst.Term.Head): ExecutableAst.Term.Head = sast match {
      case SimplifiedAst.Term.Head.Var(ident, tpe, loc) => ExecutableAst.Term.Head.Var(ident, tpe, loc)
      case SimplifiedAst.Term.Head.Exp(literal, tpe, loc) =>
        ExecutableAst.Term.Head.Exp(Expression.toExecutable(literal), tpe, loc)
      case SimplifiedAst.Term.Head.Apply(name, args, tpe, loc) =>
        val argsArray = args.map(Term.toExecutable).toArray
        ExecutableAst.Term.Head.Apply(name, argsArray, tpe, loc)
    }

    def toExecutable(sast: SimplifiedAst.Term.Body): ExecutableAst.Term.Body = sast match {
      case SimplifiedAst.Term.Body.Wildcard(tpe, loc) => ExecutableAst.Term.Body.Wildcard(tpe, loc)
      case SimplifiedAst.Term.Body.Var(ident, v, tpe, loc) => ExecutableAst.Term.Body.Var(ident, v, tpe, loc)
      case SimplifiedAst.Term.Body.Exp(e, tpe, loc) => ExecutableAst.Term.Body.Exp(Expression.toExecutable(e), tpe, loc)
    }
  }

  def toExecutable(sast: SimplifiedAst.Attribute): ExecutableAst.Attribute =
    ExecutableAst.Attribute(sast.ident, sast.tpe)

  def toExecutable(sast: SimplifiedAst.FormalArg): ExecutableAst.FormalArg =
    ExecutableAst.FormalArg(sast.ident, sast.tpe)
}
