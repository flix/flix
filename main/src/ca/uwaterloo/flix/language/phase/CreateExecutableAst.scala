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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.runtime.datastore.ProxyObject
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

// TODO: This class is pretty ugly and could use a rewrite.

object CreateExecutableAst extends Phase[SimplifiedAst.Root, ExecutableAst.Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, ExecutableAst.Def]

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = flix.phase("CreateExecutableAst") {
    implicit val _ = flix.genSym

    // A mutable map to hold top-level definitions created by lifting lattice expressions.
    val m: TopLevel = mutable.Map.empty

    val constants = root.defs.map { case (k, v) => k -> toExecutable(v) }

    val effs = root.effs.map { case (k, v) => k -> visitEff(v) }
    val handlers = root.handlers.map { case (k, v) => k -> visitHandler(v) }

    val enums = root.enums.map {
      case (sym, SimplifiedAst.Enum(mod, _, cases0, tpe, loc)) =>
        val cases = cases0.map {
          case (tag, SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)) => tag -> ExecutableAst.Case(enumSym, tagName, tagType, tagLoc)
        }
        sym -> ExecutableAst.Enum(mod, sym, cases, tpe, loc)
    }

    // Converting lattices to ExecutableAst will create new top-level definitions in the map `m`.
    val lattices = root.lattices.map { case (k, v) => k -> toExecutable(v, m) }
    val tables = root.tables.map { case (k, v) => k -> Table.toExecutable(v) }
    val indexes = root.indexes.map { case (k, v) => k -> toExecutable(v) }
    val strata = root.strata.map(s => ExecutableAst.Stratum(s.constraints.map(c => Constraint.toConstraint(c, m))))
    val properties = root.properties.map(p => toExecutable(p))
    val specialOps = root.specialOps
    val reachable = root.reachable

    ExecutableAst.Root(constants ++ m, effs, handlers, enums, lattices, tables, indexes, strata, properties, specialOps, reachable).toSuccess
  }

  def toExecutable(sast: SimplifiedAst.Def): ExecutableAst.Def = {
    val formals = sast.fparams.map {
      case SimplifiedAst.FormalParam(sym, mod, tpe, loc) => ExecutableAst.FormalParam(sym, tpe)
    }.toArray

    ExecutableAst.Def(sast.ann, sast.mod, sast.sym, formals, Expression.toExecutable(sast.exp), sast.tpe, sast.loc)
  }

  def visitEff(eff0: SimplifiedAst.Eff): ExecutableAst.Eff = {
    val fparams = eff0.fparams.map(toExecutable)
    ExecutableAst.Eff(eff0.ann, eff0.mod, eff0.sym, fparams, eff0.tpe, eff0.loc)
  }

  def visitHandler(handler0: SimplifiedAst.Handler): ExecutableAst.Handler = {
    val fparams = handler0.fparams.map(toExecutable)
    val exp = Expression.toExecutable(handler0.exp)
    ExecutableAst.Handler(handler0.ann, handler0.mod, handler0.sym, fparams, exp, handler0.tpe, handler0.loc)
  }

  def toExecutable(sast: SimplifiedAst.Lattice, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Lattice = sast match {
    case SimplifiedAst.Lattice(tpe, bot, top, equ, leq, lub, glb, loc) =>
      ExecutableAst.Lattice(tpe, bot, top, equ, leq, lub, glb, loc)
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
      case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
        throw InternalCompilerException("Lambdas should have been converted to closures and lifted.")
      case SimplifiedAst.Expression.LambdaClosure(lambda, freeVars, tpe, loc) =>
        throw InternalCompilerException("MkClosure should have been replaced by MkClosureRef after lambda lifting.")
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        throw InternalCompilerException("Apply should have been replaced by ClosureConv.") // TODO: Doc
      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars.map(CreateExecutableAst.toExecutable)

        // TODO: Temporary fix to compute the function interface type (as opposed to the closure interface type).
        // In the future this "computation" should not be performed here.
        val base = tpe.typeConstructor
        val targs = tpe.typeArguments
        val freeArgs = fvs.map(_.tpe)
        val fnType = Type.mkArrow(freeArgs ::: targs.init, targs.last)

        ExecutableAst.Expression.Closure(sym, fvs, fnType, tpe, loc)
      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyClo(toExecutable(exp), argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        val as = args.map(toExecutable)
        ExecutableAst.Expression.ApplyDef(name, as, tpe, loc)
      case SimplifiedAst.Expression.ApplyEff(sym, args, tpe, loc) =>
        val as = args.map(toExecutable)
        ExecutableAst.Expression.ApplyEff(sym, as, tpe, loc)
      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyCloTail(toExecutable(exp), argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyDefTail(sym, argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe, loc) =>
        val argsArray = args.map(toExecutable)
        ExecutableAst.Expression.ApplyEffTail(sym, argsArray, tpe, loc)
      case SimplifiedAst.Expression.ApplySelfTail(name, formals, actuals, tpe, loc) =>
        ExecutableAst.Expression.ApplySelfTail(name, formals.map(CreateExecutableAst.toExecutable), actuals.map(toExecutable), tpe, loc)
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
      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val elmsArray = elms.map(toExecutable).toArray
        ExecutableAst.Expression.ArrayLit(elmsArray, tpe, loc)
      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = toExecutable(elm)
        val ln = toExecutable(len)
        ExecutableAst.Expression.ArrayNew(e, ln, tpe, loc)
      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = toExecutable(base)
        val i = toExecutable(index)
        ExecutableAst.Expression.ArrayLoad(b, i, tpe, loc)
      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = toExecutable(base)
        val i = toExecutable(index)
        val e = toExecutable(elm)
        ExecutableAst.Expression.ArrayStore(b, i, e, tpe, loc)
      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = toExecutable(base)
        ExecutableAst.Expression.ArrayLength(b, tpe, loc)
      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = toExecutable(base)
        val i1 = toExecutable(startIndex)
        val i2 = toExecutable(endIndex)
        ExecutableAst.Expression.ArraySlice(b, i1, i2, tpe, loc)
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
      case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = toExecutable(exp)
        val bs = bindings map {
          case SimplifiedAst.HandlerBinding(sym, body) => ExecutableAst.HandlerBinding(sym, toExecutable(body))
        }
        ExecutableAst.Expression.HandleWith(e, bs, tpe, loc)
      case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Existential(p, toExecutable(exp), loc)
      case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
        val p = ExecutableAst.FormalParam(fparam.sym, fparam.tpe)
        ExecutableAst.Expression.Universal(p, toExecutable(exp), loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = toExecutable(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = toExecutable(body)
            ExecutableAst.CatchRule(sym, clazz, b)
        }
        ExecutableAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args.map(e => toExecutable(e))
        ExecutableAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case SimplifiedAst.Expression.NativeField(field, tpe, loc) => ExecutableAst.Expression.NativeField(field, tpe, loc)

      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args.map(e => toExecutable(e))
        ExecutableAst.Expression.NativeMethod(method, es, tpe, loc)

      case SimplifiedAst.Expression.UserError(tpe, loc) => ExecutableAst.Expression.UserError(tpe, loc)
      case SimplifiedAst.Expression.HoleError(sym, tpe, eff, loc) => ExecutableAst.Expression.HoleError(sym, tpe, loc)
      case SimplifiedAst.Expression.MatchError(tpe, loc) => ExecutableAst.Expression.MatchError(tpe, loc)
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => ExecutableAst.Expression.SwitchError(tpe, loc)
      case SimplifiedAst.Expression.Def(sym, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$sast'.")
      case SimplifiedAst.Expression.Eff(sym, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '$sast'.")
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

        case SimplifiedAst.Predicate.Head.Atom(name, terms, loc) =>
          val ts = terms.map(t => Terms.translate(t, m))
          ExecutableAst.Predicate.Head.Atom(name, ts, loc)
      }
    }

    object Body {
      // Also, figure out the actual implementation for Predicate.Body.Loop

      def toExecutable(sast: SimplifiedAst.Predicate.Body, m: TopLevel)(implicit genSym: GenSym): ExecutableAst.Predicate.Body = sast match {
        case SimplifiedAst.Predicate.Body.Atom(sym, polarity, terms, loc) =>
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
          ExecutableAst.Predicate.Body.Atom(sym, polarity, termsArray, index2var, loc)

        case SimplifiedAst.Predicate.Body.Filter(name, terms, loc) =>
          val termsArray = terms.map(t => Terms.Body.translate(t, m)).toArray
          ExecutableAst.Predicate.Body.Filter(name, termsArray, loc)
        case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) =>
          ExecutableAst.Predicate.Body.Loop(sym, Terms.translate(term, m), loc)
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
  private def toValueOpt(exp0: SimplifiedAst.Expression): Option[ProxyObject] = exp0 match {
    case SimplifiedAst.Expression.True => Some(new ProxyObject(java.lang.Boolean.TRUE, null, null, null))
    case SimplifiedAst.Expression.False => Some(new ProxyObject(java.lang.Boolean.FALSE, null, null, null))
    case SimplifiedAst.Expression.Char(lit) => Some(new ProxyObject(new java.lang.Character(lit), null, null, null))
    case SimplifiedAst.Expression.Float32(lit) => Some(new ProxyObject(new java.lang.Float(lit), null, null, null))
    case SimplifiedAst.Expression.Float64(lit) => Some(new ProxyObject(new java.lang.Double(lit), null, null, null))
    case SimplifiedAst.Expression.Int8(lit) => Some(new ProxyObject(new java.lang.Byte(lit), null, null, null))
    case SimplifiedAst.Expression.Int16(lit) => Some(new ProxyObject(new java.lang.Short(lit), null, null, null))
    case SimplifiedAst.Expression.Int32(lit) => Some(new ProxyObject(new java.lang.Integer(lit), null, null, null))
    case SimplifiedAst.Expression.Int64(lit) => Some(new ProxyObject(new java.lang.Long(lit), null, null, null))
    case SimplifiedAst.Expression.BigInt(lit) => Some(new ProxyObject(lit, null, null, null))
    case SimplifiedAst.Expression.Str(lit) => Some(new ProxyObject(lit, null, null, null))
    case _ => None
  }

  private def lit2sym(exp0: SimplifiedAst.Expression, m: TopLevel)(implicit genSym: GenSym): Symbol.DefnSym = {
    // Generate a top-level function for the constant.
    val sym = Symbol.freshDefnSym("lit")
    val lit = Expression.toExecutable(exp0)
    val ann = Ast.Annotations.Empty
    val mod = Ast.Modifiers(List(Ast.Modifier.Synthetic))
    val varX = Symbol.freshVarSym("_unit")
    varX.setStackOffset(0)
    val fparam = ExecutableAst.FormalParam(varX, Type.Unit)
    val fs = Array(fparam)
    val tpe = Type.mkArrow(Type.Unit, exp0.tpe)
    val defn = ExecutableAst.Def(ann, mod, sym, fs, lit, tpe, exp0.loc)
    m += (sym -> defn)
    sym
  }

}
