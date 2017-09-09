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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.ByteCodes
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.runtime.Interpreter
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

// TODO: This class is pretty ugly and could use a rewrite.

object CreateExecutableAst extends Phase[SimplifiedAst.Root, ExecutableAst.Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, ExecutableAst.Def]

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    // A mutable map to hold top-level definitions created by lifting lattice expressions.
    val m: TopLevel = mutable.Map.empty

    val constants = root.defs.map { case (k, v) => k -> toExecutable(v) }

    val enums = root.enums.map {
      case (sym, SimplifiedAst.Enum(_, cases0, loc)) =>
        val cases = cases0.map {
          case (tag, SimplifiedAst.Case(enumName, tagName, tpe)) => tag -> ExecutableAst.Case(enumName, tagName, tpe)
        }
        sym -> ExecutableAst.Enum(sym, cases, loc)
    }

    // Converting lattices to ExecutableAst will create new top-level definitions in the map `m`.
    val lattices = root.lattices.map { case (k, v) => k -> toExecutable(v, m) }
    val tables = root.tables.map { case (k, v) => k -> Table.toExecutable(v) }
    val indexes = root.indexes.map { case (k, v) => k -> toExecutable(v) }
    // TODO: Assumes one stratum
    val constraints = root.strata.head.constraints.map(c => Constraint.toConstraint(c, m))
    val properties = root.properties.map(p => toExecutable(p))
    val specialOps = root.specialOps
    val reachable = root.reachable
    val time = root.time

    val dependenciesOf: Map[Symbol.TableSym, Set[(ExecutableAst.Constraint, ExecutableAst.Predicate.Body.Positive)]] = {
      val result = mutable.Map.empty[Symbol.TableSym, Set[(ExecutableAst.Constraint, ExecutableAst.Predicate.Body.Positive)]]

      for (rule <- constraints) {
        rule.head match {
          case ExecutableAst.Predicate.Head.Positive(sym, _, _) => result.update(sym, Set.empty)
          case _ => // nop
        }
      }

      for (outerRule <- constraints if outerRule.isRule) {
        for (innerRule <- constraints if innerRule.isRule) {
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

    ExecutableAst.Root(constants ++ m, enums, lattices, tables, indexes, constraints, properties, specialOps,
      reachable, ByteCodes(Map(), Map(), Map(), Map(), Map(), Map()), time, dependenciesOf).toSuccess
  }

  def toExecutable(sast: SimplifiedAst.Def): ExecutableAst.Def = {
    val formals = sast.formals.map {
      case SimplifiedAst.FormalParam(sym, mod, tpe, loc) => ExecutableAst.FormalParam(sym, tpe)
    }.toArray

    ExecutableAst.Def(sast.ann, sast.sym, formals, Expression.toExecutable(sast.exp), sast.isSynthetic, sast.tpe, sast.loc)
  }

  def toExecutable(sast: SimplifiedAst.Lattice, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Lattice = sast match {
    case SimplifiedAst.Lattice(tpe, bot, top, equ, leq, lub, glb, loc) =>
      import Expression.{toExecutable => t}

      /**
        * In `SimplifiedAst.Definition.Lattice`, bot/top/eq/leq/lub/glb are `SimplifiedAst.Expression`s.
        * For `ExecutableAst.Definition.Lattice`, they are `Symbol.Resolved`s.
        *
        * bot/top are arbitrary expressions, so we lift them to top-level definitions.
        * We assume that eq/leq/lub/glb are `Expression.Ref`s, so we do a cast and extract the symbols.
        *
        * Note that all of this code will eventually be replaced by typeclasses.
        */

      val botSym = Symbol.freshDefnSym("bot")
      val topSym = Symbol.freshDefnSym("top")

      val botConst = ExecutableAst.Def(Ast.Annotations(Nil), botSym, formals = Array(), t(bot), isSynthetic = true, bot.tpe, bot.loc)
      val topConst = ExecutableAst.Def(Ast.Annotations(Nil), topSym, formals = Array(), t(top), isSynthetic = true, top.tpe, top.loc)

      // Update the map of definitions
      m ++= Map(botSym -> botConst, topSym -> topConst)

      // Extract the symbols for eq/leq/lub/glb
      val equSym = equ.asInstanceOf[SimplifiedAst.Expression.Def].sym
      val leqSym = leq.asInstanceOf[SimplifiedAst.Expression.Def].sym
      val lubSym = lub.asInstanceOf[SimplifiedAst.Expression.Def].sym
      val glbSym = glb.asInstanceOf[SimplifiedAst.Expression.Def].sym

      ExecutableAst.Lattice(tpe, botSym, topSym, equSym, leqSym, lubSym, glbSym, loc)
  }

  def toExecutable(sast: SimplifiedAst.Index): ExecutableAst.Index =
    ExecutableAst.Index(sast.sym, sast.indexes, sast.loc)

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
    def toConstraint(sast: SimplifiedAst.Constraint, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Constraint = {
      val head = Predicate.Head.toExecutable(sast.head, m)
      val body = sast.body.map(b => Predicate.Body.toExecutable(b, m))
      val cparams = sast.cparams.map {
        case SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc) => ExecutableAst.ConstraintParam.HeadParam(sym, tpe, loc)
        case SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc) => ExecutableAst.ConstraintParam.RuleParam(sym, tpe, loc)
      }
      ExecutableAst.Constraint(cparams, head, body)
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
      case SimplifiedAst.Expression.Var(sym, tpe, loc) =>
        ExecutableAst.Expression.Var(sym, tpe, loc)
      case SimplifiedAst.Expression.Def(name, tpe, loc) => ???
      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        throw InternalCompilerException("Lambdas should have been converted to closures and lifted.")
      case SimplifiedAst.Expression.Hook(hook, tpe, loc) =>
        throw InternalCompilerException("Hooks should have been inlined into ApplyHooks or wrapped inside lambdas.")
      case SimplifiedAst.Expression.LambdaClosure(lambda, freeVars, tpe, loc) =>
        throw InternalCompilerException("MkClosure should have been replaced by MkClosureRef after lambda lifting.")
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        throw InternalCompilerException("Apply should have been replaced by ClosureConv.") // TODO: Doc
      case SimplifiedAst.Expression.Closure(exp, freeVars, tpe, loc) =>
        val fvs = freeVars.map(CreateExecutableAst.toExecutable)
        val d = exp.asInstanceOf[SimplifiedAst.Expression.Def]
        ExecutableAst.Expression.Closure(d.sym, fvs, d.tpe, tpe, loc)
      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyClo(toExecutable(exp), argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyDef(name, argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyCloTail(toExecutable(exp), argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyDefTail(name, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyDefTail(name, argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
        ExecutableAst.Expression.ApplySelfTail(name, formals.map(CreateExecutableAst.toExecutable), actuals.map(toExecutable), tpe, loc)
      case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyHook(hook, argsArray, tpe, loc)
      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        ExecutableAst.Expression.Unary(sop, op, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.Binary(sop, op, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        ExecutableAst.Expression.IfThenElse(toExecutable(exp1), toExecutable(exp2), toExecutable(exp3), tpe, loc)
      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = toExecutable(exp)
        val bs = branches map {
          case (sym, br) => sym -> toExecutable(br)
        }
        ExecutableAst.Expression.Branch(e, bs, tpe, loc)
      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        ExecutableAst.Expression.JumpTo(sym, tpe, loc)
      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.Let(sym, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        ExecutableAst.Expression.LetRec(sym, toExecutable(exp1), toExecutable(exp2), tpe, loc)
      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        ExecutableAst.Expression.Is(sym, tag, toExecutable(exp), loc)
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        ExecutableAst.Expression.Tag(enum, tag, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        ExecutableAst.Expression.Untag(sym, tag, toExecutable(exp), tpe, loc)
      case SimplifiedAst.Expression.Index(base, offset, tpe, loc) =>
        ExecutableAst.Expression.Index(toExecutable(base), offset, tpe, loc)
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val elmsArray = elms.map(toExecutable).toArray
        ExecutableAst.Expression.Tuple(elmsArray, tpe, loc)
      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        val e = toExecutable(exp)
        ExecutableAst.Expression.Ref(e, tpe, loc)
      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        val e = toExecutable(exp)
        ExecutableAst.Expression.Deref(e, tpe, loc)
      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = toExecutable(exp1)
        val e2 = toExecutable(exp2)
        ExecutableAst.Expression.Assign(e1, e2, tpe, loc)
      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Existential(p, toExecutable(exp), loc)
      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Universal(p, toExecutable(exp), loc)
      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args.map(e => toExecutable(e))
        ExecutableAst.Expression.NativeConstructor(constructor, es, tpe, loc)
      case SimplifiedAst.Expression.NativeField(field, tpe, loc) => ExecutableAst.Expression.NativeField(field, tpe, loc)
      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args.map(e => toExecutable(e))
        ExecutableAst.Expression.NativeMethod(method, es, tpe, loc)
      case SimplifiedAst.Expression.UserError(tpe, loc) => ExecutableAst.Expression.UserError(tpe, loc)
      case SimplifiedAst.Expression.MatchError(tpe, loc) => ExecutableAst.Expression.MatchError(tpe, loc)
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => ExecutableAst.Expression.SwitchError(tpe, loc)
    }
  }

  object Patterns {

    def toExecutable(pat0: SimplifiedAst.Pattern): ExecutableAst.Pattern = pat0 match {
      case SimplifiedAst.Pattern.Wild(tpe, loc) => ExecutableAst.Pattern.Wild(tpe, loc)
      case SimplifiedAst.Pattern.Var(sym, tpe, loc) => ExecutableAst.Pattern.Var(sym, tpe, loc)
      case SimplifiedAst.Pattern.Unit(loc) => ExecutableAst.Pattern.Unit(loc)
      case SimplifiedAst.Pattern.True(loc) => ExecutableAst.Pattern.True(loc)
      case SimplifiedAst.Pattern.False(loc) => ExecutableAst.Pattern.False(loc)
      case SimplifiedAst.Pattern.Char(lit, loc) => ExecutableAst.Pattern.Char(lit, loc)
      case SimplifiedAst.Pattern.Float32(lit, loc) => ExecutableAst.Pattern.Float32(lit, loc)
      case SimplifiedAst.Pattern.Float64(lit, loc) => ExecutableAst.Pattern.Float64(lit, loc)
      case SimplifiedAst.Pattern.Int8(lit, loc) => ExecutableAst.Pattern.Int8(lit, loc)
      case SimplifiedAst.Pattern.Int16(lit, loc) => ExecutableAst.Pattern.Int16(lit, loc)
      case SimplifiedAst.Pattern.Int32(lit, loc) => ExecutableAst.Pattern.Int32(lit, loc)
      case SimplifiedAst.Pattern.Int64(lit, loc) => ExecutableAst.Pattern.Int64(lit, loc)
      case SimplifiedAst.Pattern.BigInt(lit, loc) => ExecutableAst.Pattern.BigInt(lit, loc)
      case SimplifiedAst.Pattern.Str(lit, loc) => ExecutableAst.Pattern.Str(lit, loc)
      case SimplifiedAst.Pattern.Tag(sym, tag, pat, tpe, loc) => ExecutableAst.Pattern.Tag(sym, tag, toExecutable(pat), tpe, loc)
      case SimplifiedAst.Pattern.Tuple(elms, tpe, loc) =>
        val es = elms map toExecutable
        ExecutableAst.Pattern.Tuple(es, tpe, loc)
    }

  }

  object Predicate {

    object Head {
      def toExecutable(sast: SimplifiedAst.Predicate.Head, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Predicate.Head = sast match {
        case SimplifiedAst.Predicate.Head.True(loc) => ExecutableAst.Predicate.Head.True(loc)
        case SimplifiedAst.Predicate.Head.False(loc) => ExecutableAst.Predicate.Head.False(loc)

        case SimplifiedAst.Predicate.Head.Positive(name, terms, loc) =>
          val ts = terms.map(t => Terms.translate(t, m)).toArray
          ExecutableAst.Predicate.Head.Positive(name, ts, loc)

        case SimplifiedAst.Predicate.Head.Negative(name, terms, loc) =>
          val ts = terms.map(t => Terms.translate(t, m)).toArray
          ExecutableAst.Predicate.Head.Negative(name, ts, loc)
      }
    }

    object Body {
      // TODO: Should we move this to the Indexer (the only place that accesses freeVars)?
      // Also, figure out the actual implementation for Predicate.Body.Loop
      // TODO: Should not return strings!
      private def freeVars(terms: List[SimplifiedAst.Term.Body]): Set[String] = terms.foldLeft(Set.empty[String]) {
        case (xs, t: SimplifiedAst.Term.Body.Wild) => xs
        case (xs, t: SimplifiedAst.Term.Body.Var) => xs + t.sym.toString
        case (xs, t: SimplifiedAst.Term.Body.Lit) => xs
        case (xs, t: SimplifiedAst.Term.Body.Pat) => xs // TODO ????
      }

      def toExecutable(sast: SimplifiedAst.Predicate.Body, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Predicate.Body = sast match {
        case SimplifiedAst.Predicate.Body.Positive(sym, terms, loc) =>
          val termsArray = terms.map(t => Terms.Body.translate(t, m)).toArray
          val index2var: Array[Symbol.VarSym] = {
            val r = new Array[Symbol.VarSym](termsArray.length)
            var i = 0
            while (i < r.length) {
              termsArray(i) match {
                case ExecutableAst.Term.Body.Var(sym, _, _) =>
                  r(i) = sym
                case _ => // nop
              }
              i = i + 1
            }
            r
          }
          ExecutableAst.Predicate.Body.Positive(sym, termsArray, index2var, freeVars(terms), loc)

        case SimplifiedAst.Predicate.Body.Negative(sym, terms, loc) =>
          val termsArray = terms.map(t => Terms.Body.translate(t, m)).toArray
          val index2var: Array[Symbol.VarSym] = {
            val r = new Array[Symbol.VarSym](termsArray.length)
            var i = 0
            while (i < r.length) {
              termsArray(i) match {
                case ExecutableAst.Term.Body.Var(sym, _, _) =>
                  r(i) = sym
                case _ => // nop
              }
              i = i + 1
            }
            r
          }
          ExecutableAst.Predicate.Body.Negative(sym, termsArray, index2var, freeVars(terms), loc)


        case SimplifiedAst.Predicate.Body.Filter(name, terms, loc) =>
          val termsArray = terms.map(t => Terms.Body.translate(t, m)).toArray
          ExecutableAst.Predicate.Body.Filter(name, termsArray, freeVars(terms), loc)
        case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) =>
          val freeVars = Set.empty[String] // TODO
          ExecutableAst.Predicate.Body.Loop(sym, Terms.translate(term, m), freeVars, loc)
      }
    }

  }

  object Terms {

    /**
      * Returns the given simplified head term `t0` as an executable head term.
      */
    def translate(t0: SimplifiedAst.Term.Head, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Term.Head = t0 match {
      case SimplifiedAst.Term.Head.Var(sym, tpe, loc) => ExecutableAst.Term.Head.Var(sym, tpe, loc)
      case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => toValueOpt(lit) match {
        case Some(value) => ExecutableAst.Term.Head.Lit(value, tpe, loc)
        case None => ExecutableAst.Term.Head.Cst(lit2sym(lit, m), tpe, loc)
      }
      case SimplifiedAst.Term.Head.App(name, args, tpe, loc) =>
        ExecutableAst.Term.Head.App(name, args.toArray, tpe, loc)
    }

    object Body {
      /**
        * Returns the given simplified body term `t0` as an executable body term.
        */
      def translate(t0: SimplifiedAst.Term.Body, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Term.Body = t0 match {
        case SimplifiedAst.Term.Body.Wild(tpe, loc) => ExecutableAst.Term.Body.Wild(tpe, loc)
        case SimplifiedAst.Term.Body.Var(sym, tpe, loc) => ExecutableAst.Term.Body.Var(sym, tpe, loc)
        case SimplifiedAst.Term.Body.Lit(lit, tpe, loc) => toValueOpt(lit) match {
          case Some(value) => ExecutableAst.Term.Body.Lit(value, tpe, loc)
          case None => ExecutableAst.Term.Body.Cst(lit2sym(lit, m), tpe, loc)
        }
        case SimplifiedAst.Term.Body.Pat(pat, tpe, loc) => ExecutableAst.Term.Body.Pat(Patterns.toExecutable(pat), tpe, loc)
      }
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

  /**
    * Optionally returns the given expression `exp0` as a value reference.
    */
  private def toValueOpt(exp0: SimplifiedAst.Expression): Option[AnyRef] = exp0 match {
    case SimplifiedAst.Expression.True => Some(java.lang.Boolean.TRUE)
    case SimplifiedAst.Expression.False => Some(java.lang.Boolean.FALSE)
    case SimplifiedAst.Expression.Char(lit) => Some(new java.lang.Character(lit))
    case SimplifiedAst.Expression.Float32(lit) => Some(new java.lang.Float(lit))
    case SimplifiedAst.Expression.Float64(lit) => Some(new java.lang.Double(lit))
    case SimplifiedAst.Expression.Int8(lit) => Some(new java.lang.Byte(lit))
    case SimplifiedAst.Expression.Int16(lit) => Some(new java.lang.Short(lit))
    case SimplifiedAst.Expression.Int32(lit) => Some(new java.lang.Integer(lit))
    case SimplifiedAst.Expression.Int64(lit) => Some(new java.lang.Long(lit))
    case SimplifiedAst.Expression.BigInt(lit) => Some(lit)
    case SimplifiedAst.Expression.Str(lit) => Some(lit)
    case _ => None
  }

  private def lit2sym(exp0: SimplifiedAst.Expression, m: TopLevel)(implicit genSym: GenSym): Symbol.DefnSym = {
    // Generate a top-level function for the constant.
    val sym = Symbol.freshDefnSym("lit")
    val lit = Expression.toExecutable(exp0)
    val defn = ExecutableAst.Def(Ast.Annotations(Nil), sym, formals = Array(), lit, isSynthetic = true, exp0.tpe, exp0.loc)
    m += (sym -> defn)
    sym
  }

}
