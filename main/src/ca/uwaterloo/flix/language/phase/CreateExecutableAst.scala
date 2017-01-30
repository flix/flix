/*
 * Copyright 2015-2016 Ming-Ho Yee
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

/**
  * A phase that transforms a SimplifiedAst into an ExecutableAst.
  * Essentially the identity transform, with a few differences:
  *
  * - Lists are copied to arrays
  * - Certain nodes no longer exist in SimplifiedAst and thus do not exist in ExecutableAst
  */
// TODO: Better name
object CreateExecutableAst {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, ExecutableAst.Definition.Constant]

  def toExecutable(sast: SimplifiedAst.Root)(implicit genSym: GenSym): ExecutableAst.Root = {
    // A mutable map to hold top-level definitions created by lifting lattice expressions.
    val m: TopLevel = mutable.Map.empty

    val constants = sast.definitions.map { case (k, v) => k -> Definition.toExecutable(v) }

    val enums = sast.enums.map {
      case (sym, SimplifiedAst.Definition.Enum(_, cases0, loc)) =>
        val cases = cases0.map {
          case (tag, SimplifiedAst.Case(enumName, tagName, tpe)) => tag -> ExecutableAst.Case(enumName, tagName, tpe)
        }
        sym -> ExecutableAst.Definition.Enum(sym, cases, loc)
    }

    // Converting lattices to ExecutableAst will create new top-level definitions in the map `m`.
    val lattices = sast.lattices.map { case (k, v) => k -> Definition.toExecutable(v, m) }
    val tables = sast.tables.map { case (k, v) => k -> Table.toExecutable(v) }
    val indexes = sast.indexes.map { case (k, v) => k -> Definition.toExecutable(v) }
    val facts = sast.strata.head.constraints.filter(_.body.isEmpty).map(Constraint.toFact).toArray
    // TODO: Assumes one stratum
    val rules = sast.strata.head.constraints.filter(_.body.nonEmpty).map(Constraint.toRule).toArray
    // TODO: Assumes one stratum
    val properties = sast.properties.map(p => toExecutable(p))
    val time = sast.time

    val dependenciesOf: Map[Symbol.TableSym, Set[(ExecutableAst.Constraint.Rule, ExecutableAst.Predicate.Body.Positive)]] = {
      val result = mutable.Map.empty[Symbol.TableSym, Set[(ExecutableAst.Constraint.Rule, ExecutableAst.Predicate.Body.Positive)]]

      for (rule <- rules) {
        rule.head match {
          case ExecutableAst.Predicate.Head.Positive(sym, _, _) => result.update(sym, Set.empty)
          case _ => // nop
        }
      }

      for (outerRule <- rules) {
        for (innerRule <- rules) {
          for (body <- innerRule.body) {
            (outerRule.head, body) match {
              case (outer: ExecutableAst.Predicate.Head.Positive, inner: ExecutableAst.Predicate.Body.Positive) =>
                if (outer.sym == inner.sym) {
                  val deps = result(outer.sym)
                  result(outer.sym) = deps + ((innerRule, inner))
                }
              case _ => // nop
            }
          }
        }
      }
      result.toMap
    }

    ExecutableAst.Root(constants ++ m, enums, lattices, tables, indexes, facts, rules, properties, time, dependenciesOf)
  }

  object Definition {
    def toExecutable(sast: SimplifiedAst.Definition.Constant): ExecutableAst.Definition.Constant = {
      val formals = sast.formals.map {
        case SimplifiedAst.FormalParam(sym, tpe) => ExecutableAst.FormalParam(sym, tpe)
      }.toArray

      ExecutableAst.Definition.Constant(sast.ann, sast.sym, formals, Expression.toExecutable(sast.exp), sast.isSynthetic, sast.tpe, sast.loc)
    }

    def toExecutable(sast: SimplifiedAst.Definition.Lattice, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Definition.Lattice = sast match {
      case SimplifiedAst.Definition.Lattice(tpe, bot, top, leq, lub, glb, loc) =>
        import Expression.{toExecutable => t}

        /**
          * In `SimplifiedAst.Definition.Lattice`, bot/top/leq/lub/glb are `SimplifiedAst.Expression`s.
          * For `ExecutableAst.Definition.Lattice`, they are `Symbol.Resolved`s.
          *
          * bot/top are arbitrary expressions, so we lift them to top-level definitions.
          * We assume that leq/lub/glb are `Expression.Ref`s, so we do a cast and extract the symbols.
          *
          * Note that all of this code will eventually be replaced by typeclasses.
          */

        val botSym = Symbol.freshDefnSym("bot")
        val topSym = Symbol.freshDefnSym("top")

        val botConst = ExecutableAst.Definition.Constant(Ast.Annotations(Nil), botSym, formals = Array(), t(bot), isSynthetic = true, bot.tpe, bot.loc)
        val topConst = ExecutableAst.Definition.Constant(Ast.Annotations(Nil), topSym, formals = Array(), t(top), isSynthetic = true, top.tpe, top.loc)

        // Update the map of definitions
        m ++= Map(botSym -> botConst, topSym -> topConst)

        // Extract the symbols for leq/lub/glb
        val leqSym = t(leq).asInstanceOf[ExecutableAst.Expression.Ref].sym
        val lubSym = t(lub).asInstanceOf[ExecutableAst.Expression.Ref].sym
        val glbSym = t(glb).asInstanceOf[ExecutableAst.Expression.Ref].sym

        ExecutableAst.Definition.Lattice(tpe, botSym, topSym, leqSym, lubSym, glbSym, loc)
    }

    def toExecutable(sast: SimplifiedAst.Definition.Index): ExecutableAst.Definition.Index =
      ExecutableAst.Definition.Index(sast.sym, sast.indexes, sast.loc)
  }

  object Table {
    def toExecutable(sast: SimplifiedAst.Table): ExecutableAst.Table = sast match {
      case SimplifiedAst.Table.Relation(symbol, attributes, loc) =>
        val attributesArray = attributes.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Table.Relation(symbol, attributesArray, loc)
      case SimplifiedAst.Table.Lattice(symbol, keys, value, loc) =>
        val keysArray = keys.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Table.Lattice(symbol, keysArray, CreateExecutableAst.toExecutable(value), loc)
    }
  }

  object Constraint {
    def toFact(sast: SimplifiedAst.Constraint): ExecutableAst.Constraint.Fact =
      ExecutableAst.Constraint.Fact(Predicate.Head.toExecutable(sast.head))

    def toRule(sast: SimplifiedAst.Constraint): ExecutableAst.Constraint.Rule = {
      val head = Predicate.Head.toExecutable(sast.head)
      val body = sast.body.map(Predicate.Body.toExecutable)
      val cparams = sast.cparams.map {
        case SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc) => ExecutableAst.ConstraintParam.HeadParam(sym, tpe, loc)
        case SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc) => ExecutableAst.ConstraintParam.RuleParam(sym, tpe, loc)
      }

      val collections = body.collect {
        case p: ExecutableAst.Predicate.Body.Positive => p
        case p: ExecutableAst.Predicate.Body.Negative => p
      }
      val filters = body.collect { case p: ExecutableAst.Predicate.Body.ApplyFilter => p }
      val hookFilters = body.collect { case p: ExecutableAst.Predicate.Body.ApplyHookFilter => p }
      val loops = body.collect { case p: ExecutableAst.Predicate.Body.Loop => p }
      ExecutableAst.Constraint.Rule(cparams, head, body, collections, filters, hookFilters, loops)
    }
  }

  object Expression {
    def toExecutable(sast: SimplifiedAst.Expression): ExecutableAst.Expression = sast match {
      case SimplifiedAst.Expression.Unit => ExecutableAst.Expression.Unit
      case SimplifiedAst.Expression.True => ExecutableAst.Expression.True
      case SimplifiedAst.Expression.False => ExecutableAst.Expression.False
      case SimplifiedAst.Expression.Char(lit) => ExecutableAst.Expression.Char(lit)
      case SimplifiedAst.Expression.Float32(lit) => ExecutableAst.Expression.Float32(lit)
      case SimplifiedAst.Expression.Float64(lit) => ExecutableAst.Expression.Float64(lit)
      case SimplifiedAst.Expression.Int8(lit) => ExecutableAst.Expression.Int8(lit)
      case SimplifiedAst.Expression.Int16(lit) => ExecutableAst.Expression.Int16(lit)
      case SimplifiedAst.Expression.Int32(lit) => ExecutableAst.Expression.Int32(lit)
      case SimplifiedAst.Expression.Int64(lit) => ExecutableAst.Expression.Int64(lit)
      case SimplifiedAst.Expression.BigInt(lit) => ExecutableAst.Expression.BigInt(lit)
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
      case SimplifiedAst.Expression.Var(sym, tpe, loc) =>
        ExecutableAst.Expression.Var(sym, tpe, loc)
      case SimplifiedAst.Expression.Ref(name, tpe, loc) => ExecutableAst.Expression.Ref(name, tpe, loc)
      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        throw InternalCompilerException("Lambdas should have been converted to closures and lifted.")
      case SimplifiedAst.Expression.Hook(hook, tpe, loc) =>
        throw InternalCompilerException("Hooks should have been inlined into ApplyHooks or wrapped inside lambdas.")
      case SimplifiedAst.Expression.MkClosure(lambda, freeVars, tpe, loc) =>
        throw InternalCompilerException("MkClosure should have been replaced by MkClosureRef after lambda lifting.")
      case SimplifiedAst.Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
        val e = toExecutable(ref)
        val fvs = freeVars.map(CreateExecutableAst.toExecutable).toArray
        ExecutableAst.Expression.MkClosureRef(e.asInstanceOf[ExecutableAst.Expression.Ref], fvs, tpe, loc)
      case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) =>
        val argsArray = args.map(toExecutable).toArray
        ExecutableAst.Expression.ApplyRef(name, argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyTail(name, formals, actuals, tpe, loc) =>
        ExecutableAst.Expression.ApplyTail(name, formals.map(CreateExecutableAst.toExecutable), actuals.map(toExecutable), tpe, loc)
      case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) =>
        val argsArray = args.map(toExecutable).toArray
        ExecutableAst.Expression.ApplyHook(hook, argsArray, tpe, loc)
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        val argsArray = args.map(toExecutable).toArray
        ExecutableAst.Expression.ApplyClosure(toExecutable(exp), argsArray, tpe, loc)
      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) =>
        ExecutableAst.Expression.Unary(op, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.Binary(op, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        ExecutableAst.Expression.IfThenElse(toExecutable(exp1), toExecutable(exp2), toExecutable(exp3), tpe, loc)
      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.Let(sym, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.Is(exp, tag, loc) =>
        ExecutableAst.Expression.Is(toExecutable(exp), tag, loc)
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        ExecutableAst.Expression.Tag(enum, tag, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Untag(tag, exp, tpe, loc) =>
        ExecutableAst.Expression.Untag(tag, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Index(base, offset, tpe, loc) =>
        ExecutableAst.Expression.Index(toExecutable(base), offset, tpe, loc)
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val elmsArray = elms.map(toExecutable).toArray
        ExecutableAst.Expression.Tuple(elmsArray, tpe, loc)
      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Existential(p, toExecutable(exp), loc)
      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Universal(p, toExecutable(exp), loc)
      case SimplifiedAst.Expression.UserError(tpe, loc) => ExecutableAst.Expression.UserError(tpe, loc)
      case SimplifiedAst.Expression.MatchError(tpe, loc) => ExecutableAst.Expression.MatchError(tpe, loc)
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => ExecutableAst.Expression.SwitchError(tpe, loc)
    }
  }

  object Predicate {

    object Head {
      def toExecutable(sast: SimplifiedAst.Predicate.Head): ExecutableAst.Predicate.Head = sast match {
        case SimplifiedAst.Predicate.Head.True(loc) => ExecutableAst.Predicate.Head.True(loc)
        case SimplifiedAst.Predicate.Head.False(loc) => ExecutableAst.Predicate.Head.False(loc)

        case SimplifiedAst.Predicate.Head.Positive(name, terms, loc) =>
          val ts = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Head.Positive(name, ts, loc)

        case SimplifiedAst.Predicate.Head.Negative(name, terms, loc) =>
          val ts = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Head.Negative(name, ts, loc)
      }
    }

    object Body {
      // TODO: Should we move this to the Indexer (the only place that accesses freeVars)?
      // Also, figure out the actual implementation for Predicate.Body.Loop
      // TODO: Should not return strings!
      private def freeVars(terms: List[SimplifiedAst.Term.Body]): Set[String] = terms.foldLeft(Set.empty[String]) {
        case (xs, t: SimplifiedAst.Term.Body.Wildcard) => xs
        case (xs, t: SimplifiedAst.Term.Body.Var) => xs + t.sym.toString
        case (xs, t: SimplifiedAst.Term.Body.Exp) => xs
      }

      def toExecutable(sast: SimplifiedAst.Predicate.Body): ExecutableAst.Predicate.Body = sast match {
        case SimplifiedAst.Predicate.Body.Positive(sym, terms, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          val index2var: Array[String] = {
            val r = new Array[String](termsArray.length)
            var i = 0
            while (i < r.length) {
              termsArray(i) match {
                case ExecutableAst.Term.Body.Var(ident, _, _, _) =>
                  r(i) = ident.toString
                case _ => // nop
              }
              i = i + 1
            }
            r
          }
          ExecutableAst.Predicate.Body.Positive(sym, termsArray, index2var, freeVars(terms), loc)

        case SimplifiedAst.Predicate.Body.Negative(sym, terms, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          val index2var: Array[String] = {
            val r = new Array[String](termsArray.length)
            var i = 0
            while (i < r.length) {
              termsArray(i) match {
                case ExecutableAst.Term.Body.Var(ident, _, _, _) =>
                  r(i) = ident.toString
                case _ => // nop
              }
              i = i + 1
            }
            r
          }
          ExecutableAst.Predicate.Body.Negative(sym, termsArray, index2var, freeVars(terms), loc)


        case SimplifiedAst.Predicate.Body.ApplyFilter(name, terms, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Body.ApplyFilter(name, termsArray, freeVars(terms), loc)
        case SimplifiedAst.Predicate.Body.ApplyHookFilter(hook, terms, loc) =>
          val termsArray = terms.map(Term.toExecutable).toArray
          ExecutableAst.Predicate.Body.ApplyHookFilter(hook, termsArray, freeVars(terms), loc)
        case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) =>
          val freeVars = Set.empty[String] // TODO
          ExecutableAst.Predicate.Body.Loop(sym, Term.toExecutable(term), freeVars, loc)
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
    ExecutableAst.Attribute(sast.name, sast.tpe)

  def toExecutable(sast: SimplifiedAst.FormalParam): ExecutableAst.FormalParam =
    ExecutableAst.FormalParam(sast.sym, sast.tpe)

  def toExecutable(sast: SimplifiedAst.FreeVar): ExecutableAst.FreeVar =
    ExecutableAst.FreeVar(sast.sym, sast.tpe)

  def toExecutable(sast: SimplifiedAst.Property): ExecutableAst.Property =
    ExecutableAst.Property(sast.law, sast.defn, Expression.toExecutable(sast.exp))

}
