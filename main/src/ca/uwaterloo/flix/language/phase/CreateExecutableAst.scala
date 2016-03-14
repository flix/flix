package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.mutable

/**
  * A phase that transforms a SimplifiedAst into an ExecutableAst.
  * Essentially the identity transform, with a few differences:
  *
  * - Lists are copied to arrays
  */
// TODO: Better name
object CreateExecutableAst {

  def toExecutable(sast: SimplifiedAst.Root): ExecutableAst.Root = {
    val constants = sast.constants.map { case (k, v) => k -> Definition.toExecutable(v) }
    val lattices = sast.lattices.map { case (k, v) => k -> Definition.toExecutable(v) }
    val tables = sast.tables.map { case (k, v) => k -> Table.toExecutable(v) }
    val indexes = sast.indexes.map { case (k, v) => k -> Definition.toExecutable(v) }
    val facts = sast.facts.map(Constraint.toExecutable).toArray
    val rules = sast.rules.map(Constraint.toExecutable).toArray
    val time = sast.time

    val dependenciesOf: Map[Symbol.TableSym, mutable.Set[(ExecutableAst.Constraint.Rule, ExecutableAst.Predicate.Body.Table)]] = {
      val result = mutable.Map.empty[Symbol.TableSym, mutable.Set[(ExecutableAst.Constraint.Rule, ExecutableAst.Predicate.Body.Table)]]

      for (rule <- rules) {
        rule.head match {
          case ExecutableAst.Predicate.Head.Table(sym, _, _, _) => result.update(sym, mutable.Set.empty)
          case _ => // nop
        }
      }

      for (outerRule <- rules) {
        for (innerRule <- rules) {
          for (body <- innerRule.body) {
            (outerRule.head, body) match {
              case (outer: ExecutableAst.Predicate.Head.Table, inner: ExecutableAst.Predicate.Body.Table) =>
                if (outer.sym == inner.sym) {
                  val deps = result(outer.sym)
                  deps += ((innerRule, inner))
                }
              case _ => // nop
            }
          }
        }
      }
      result.toMap
    }

    ExecutableAst.Root(constants, lattices, tables, indexes, facts, rules, time, dependenciesOf)
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
      ExecutableAst.Definition.Index(sast.sym, sast.indexes, sast.loc)

    // TODO: Compile SimplifiedAst.Definition.Function?
  }

  object Table {
    def toExecutable(sast: SimplifiedAst.Table): ExecutableAst.Table = sast match {
      case SimplifiedAst.Table.Relation(symbol, attributes, loc) =>
        val attributesArray = attributes.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Table.Relation(symbol, attributesArray, loc)
      case SimplifiedAst.Table.Lattice(symbol, keys, value, loc) =>
        val keysArray = keys.map(CreateExecutableAst.toExecutable).toArray
        val valuesArray = Array(CreateExecutableAst.toExecutable(value))
        ExecutableAst.Table.Lattice(symbol, keysArray, valuesArray, loc)
    }
  }

  object Constraint {
    def toExecutable(sast: SimplifiedAst.Constraint.Fact): ExecutableAst.Constraint.Fact =
      ExecutableAst.Constraint.Fact(Predicate.Head.toExecutable(sast.head))

    def toExecutable(sast: SimplifiedAst.Constraint.Rule): ExecutableAst.Constraint.Rule = {
      val head = Predicate.Head.toExecutable(sast.head)
      // TODO(magnus): Convert lists to arrays (and refactor Solver)
      val body = sast.body.map(Predicate.Body.toExecutable)
      val collections = body.collect { case p: ExecutableAst.Predicate.Body.Table => p }
      val filters = body.collect { case p: ExecutableAst.Predicate.Body.ApplyFilter => p }
      val hookFilters = body.collect { case p: ExecutableAst.Predicate.Body.ApplyHookFilter => p }
      val disjoint = body.collect { case p: ExecutableAst.Predicate.Body.NotEqual => p }
      val loops = body.collect { case p: ExecutableAst.Predicate.Body.Loop => p }
      ExecutableAst.Constraint.Rule(head, body, collections, filters, hookFilters, disjoint, loops)
    }
  }

  object Expression {
    def toExecutable(sast: SimplifiedAst.Expression): ExecutableAst.Expression = sast match {
      case SimplifiedAst.Expression.Unit => ExecutableAst.Expression.Unit
      case SimplifiedAst.Expression.True => ExecutableAst.Expression.True
      case SimplifiedAst.Expression.False => ExecutableAst.Expression.False
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
      case SimplifiedAst.Expression.Hook(hook, tpe, loc) => ExecutableAst.Expression.Hook(hook, tpe, loc)
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
      case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
        ExecutableAst.Expression.GetTagValue(tag, toExecutable(exp), tpe, loc)
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
      case SimplifiedAst.Expression.UserError(tpe, loc) => ExecutableAst.Expression.Error(tpe, loc)
      case SimplifiedAst.Expression.MatchError(tpe, loc) => ExecutableAst.Expression.MatchError(tpe, loc)
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => ExecutableAst.Expression.SwitchError(tpe, loc)
    }
  }

  object Predicate {

    object Head {
      def toExecutable(sast: SimplifiedAst.Predicate.Head): ExecutableAst.Predicate.Head = sast match {
        case SimplifiedAst.Predicate.Head.Table(name, terms, tpe, loc) =>
          ExecutableAst.Predicate.Head.Table(name, terms.map(Term.toExecutable).toArray, tpe, loc)
      }
    }

    object Body {
      // TODO: Should we move this to the Indexer (the only place that accesses freeVars)?
      // Also, figure out the actual implementation for Predicate.Body.Loop
      private def freeVars(terms: List[SimplifiedAst.Term.Body]): Set[String] = terms.foldLeft(Set.empty[String]) {
        case (xs, t: SimplifiedAst.Term.Body.Wildcard) => xs
        case (xs, t: SimplifiedAst.Term.Body.Var) => xs + t.ident.name
        case (xs, t: SimplifiedAst.Term.Body.Exp) => xs
      }

      def toExecutable(sast: SimplifiedAst.Predicate.Body): ExecutableAst.Predicate.Body = sast match {
        case SimplifiedAst.Predicate.Body.Table(sym, terms, tpe, loc) =>
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
          ExecutableAst.Predicate.Body.Table(sym, termsArray, index2var, freeVars(terms), tpe, loc)
        case SimplifiedAst.Predicate.Body.ApplyFilter(name, terms, tpe, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Body.ApplyFilter(name, termsArray, freeVars(terms), tpe, loc)
        case SimplifiedAst.Predicate.Body.ApplyHookFilter(hook, terms, tpe, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Body.ApplyHookFilter(hook, termsArray, freeVars(terms), tpe, loc)
        case SimplifiedAst.Predicate.Body.NotEqual(ident1, ident2, tpe, loc) =>
          val freeVars = Set(ident1.name, ident2.name)
          ExecutableAst.Predicate.Body.NotEqual(ident1, ident2, freeVars, tpe, loc)
        case SimplifiedAst.Predicate.Body.Loop(ident, term, tpe, loc) =>
          val freeVars = Set.empty[String] // TODO
          ExecutableAst.Predicate.Body.Loop(ident, Term.toExecutable(term), freeVars, tpe, loc)
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
      case SimplifiedAst.Term.Head.ApplyHook(hook, args, tpe, loc) =>
        val argsArray = args.map(Term.toExecutable).toArray
        ExecutableAst.Term.Head.ApplyHook(hook, argsArray, tpe, loc)
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
