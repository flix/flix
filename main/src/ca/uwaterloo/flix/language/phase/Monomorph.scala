/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.ReificationError
import ca.uwaterloo.flix.language.phase.unification.{Substitution, Unification}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import java.math.BigInteger
import scala.collection.mutable

/**
  * Monomorphization is a whole-program compilation strategy that replaces every reference to a parametric function with
  * a reference to a non-parametric version (of that function) specialized to the concrete types of the reference.
  *
  * For example, the polymorphic program:
  *
  * -   def fst[a, b](p: (a, b)): a = let (x, y) = p ; x
  * -   def f: Bool = fst((true, 'a'))
  * -   def g: Int32 = fst((42, "foo"))
  *
  * is, roughly speaking, translated to:
  *
  * -   def fst$1(p: (Bool, Char)): Bool = let (x, y) = p ; x
  * -   def fst$2(p: (Int32, Str)): Int32 = let (x, y) = p ; x
  * -   def f: Bool = fst$1((true, 'a'))
  * -   def g: Bool = fst$2((42, "foo"))
  *
  * At a high-level, monomorphization works as follows:
  *
  * 1. We maintain a queue of functions and the concrete types they must be specialized to.
  * 2. We populate the queue by specialization of non-parametric function definitions and other top-level expressions.
  * 3. We iteratively extract a function from the queue and specialize it:
  *    a. We replace every type variable appearing anywhere in the definition by its concrete type.
  *       b. We create new fresh local variable symbols (since the function is effectively being copied).
  *       c. We enqueue (or re-used) other functions referenced by the current function which require specialization.
  *       4. We reconstruct the AST from the specialized functions and remove all parametric functions.
  */
object Monomorph {

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are replaced by the
    * Unit type. In other words, when performing a type substitution if there is no requirement on a polymorphic type
    * we assume it to be Unit. This is safe since otherwise the type would not be polymorphic after type-inference.
    */
  private case class StrictSubstitution(s: Substitution)(implicit flix: Flix) {
    /**
      * Returns `true` if this substitution is empty.
      */
    def isEmpty: Boolean = s.isEmpty

    /**
      * Applies `this` substitution to the given type `tpe`.
      *
      * NB: Applies the substitution first, then replaces every type variable with the unit type.
      */
    def apply(tpe0: Type): Type = {
      val t = s(tpe0)

      t.map {
        case Type.KindedVar(sym, loc) if sym.kind == Kind.Bool =>
          // TODO: In strict mode we demand that there are no free (uninstantiated) Boolean variables.
          // TODO: In the future we need to decide what should actually happen if such variables occur.
          // TODO: In particular, it seems there are two cases.
          // TODO: A. Variables that occur inside the specialized types (those we can erase?)
          // TODO: B. Variables that occur inside an expression but nowhere else really.
          if (flix.options.xstrictmono)
            throw UnexpectedNonConstBool(tpe0, loc)
          else
            Type.True
        case Type.KindedVar(sym, _) if sym.kind == Kind.RecordRow => Type.RecordRowEmpty
        case Type.KindedVar(sym, _) if sym.kind == Kind.SchemaRow => Type.SchemaRowEmpty
        case Type.KindedVar(sym, _) if sym.kind == Kind.Effect => Type.Empty
        case _ => Type.Unit
      }
    }
  }

  /**
    * An exception raised to indicate that a Boolean type cannot be reified.
    *
    * @param tpe the type that cannot be reified.
    * @param loc the location of the type.
    */
  case class ReifyBoolException(tpe: Type, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised to indicate that a regular type cannot be reified.
    *
    * @param tpe the type that cannot be reified.
    * @param loc the location of the type.
    */
  case class ReifyTypeException(tpe: Type, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised to indicate that the Monomorpher encountered an unexpected non-constant Boolean.
    *
    * @param tpe the non-constant Boolean type.
    * @param loc the location of the type.
    */
  // TODO: Possibly this one should be removed.
  case class UnexpectedNonConstBool(tpe: Type, loc: SourceLocation) extends RuntimeException


  // TODO: Monomorph: We use exceptions here as a temporary stop-gap. We should consider to restructure and use Validation.

  /**
    * A function-local queue of pending (fresh symbol, function definition, and substitution)-triples.
    *
    * For example, if the queue contains the entry:
    *
    * -   (f$1, f, [a -> Int])
    *
    * it means that the function definition f should be specialized w.r.t. the map [a -> Int] under the fresh name f$1.
    */
  private type DefQueue = mutable.Set[(Symbol.DefnSym, Def, StrictSubstitution)]

  /**
    * A function-local map from a symbol and a concrete type to the fresh symbol for the specialized version of that function.
    *
    * For example, if the function:
    *
    * -   def fst[a, b](x: a, y: b): a = ...
    *
    * has been specialized w.r.t. to `Int` and `Str` then this map will contain an entry:
    *
    * -   (fst, (Int, Str) -> Int) -> fst$1
    */
  private type Def2Def = mutable.Map[(Symbol.DefnSym, Type), Symbol.DefnSym]

  /**
    * Enqueues the element `x` in `xs`.
    */
  private def enqueue[A](x: A, xs: mutable.Set[A]): Unit = xs += x

  /**
    * Dequeues an element from a non-empty `xs`.
    */
  private def dequeue[A](xs: mutable.Set[A]): A = {
    val elm = xs.head
    xs -= elm
    elm
  }

  /**
    * Performs monomorphization of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Monomorph") {

    implicit val r: Root = root

    val defQueue: DefQueue = mutable.Set.empty

    val def2def: Def2Def = mutable.Map.empty

    /*
     * A map used to collect specialized definitions, etc.
     */
    val specializedDefns: mutable.Map[Symbol.DefnSym, TypedAst.Def] = mutable.Map.empty

    /*
     * Collect all non-parametric function definitions.
     */
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.spec.tparams.isEmpty
    }

    try {
      /*
       * Perform specialization of all non-parametric function definitions.
       */
      for ((sym, defn) <- nonParametricDefns) {
        // Get a substitution from the inferred scheme to the declared scheme.
        // This is necessary because the inferred scheme may be more generic than the declared scheme.
        val subst = infallibleUnify(defn.spec.declaredScheme.base, defn.impl.inferredScheme.base)

        // Specialize the formal parameters to obtain fresh local variable symbols for them.
        val (fparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

        // Specialize the body expression.
        val body = specialize(defn.impl.exp, env0, subst, def2def, defQueue)

        // Specialize the inferred scheme
        val base = Type.mkUncurriedArrowWithEffect(fparams.map(_.tpe), body.pur, Type.freshVar(Kind.Effect, body.loc.asSynthetic), body.tpe, sym.loc.asSynthetic) // TODO use eff
        val tvars = base.typeVars.map(_.sym).toList
        val tconstrs = Nil // type constraints are not used after monomorph
        val scheme = Scheme(tvars, tconstrs, base)

        // Reassemble the definition.
        specializedDefns.put(sym, defn.copy(spec = defn.spec.copy(fparams = fparams), impl = defn.impl.copy(exp = body, inferredScheme = scheme)))
      }

      /*
       * Performs function specialization until both queues are empty.
       */
      while (defQueue.nonEmpty) {
        // Extract a function from the queue and specializes it w.r.t. its substitution.
        val (freshSym, defn, subst) = dequeue(defQueue)

        flix.subtask(freshSym.toString, sample = true)

        // Specialize the formal parameters and introduce fresh local variable symbols.
        val (fparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

        // Specialize the body expression.
        val specializedExp = specialize(defn.impl.exp, env0, subst, def2def, defQueue)

        // Reassemble the definition.
        // NB: Removes the type parameters as the function is now monomorphic.
        val specializedDefn = defn.copy(sym = freshSym, spec = defn.spec.copy(fparams = fparams, tparams = Nil), impl = TypedAst.Impl(specializedExp, Scheme(Nil, List.empty, subst(defn.impl.inferredScheme.base))))

        // Save the specialized function.
        specializedDefns.put(freshSym, specializedDefn)
      }

      // Reassemble the AST.
      root.copy(
        defs = specializedDefns.toMap
      ).toSuccess
    } catch {
      case ReifyBoolException(tpe, loc) => ReificationError.IllegalReifiedBool(tpe, loc).toFailure
      case ReifyTypeException(tpe, loc) => ReificationError.IllegalReifiedType(tpe, loc).toFailure
      case UnexpectedNonConstBool(tpe, loc) => ReificationError.UnexpectedNonConstBool(tpe, loc).toFailure
    }
  }

  /**
    * Performs specialization of the given expression `exp0` under the environment `env0` w.r.t. the given substitution `subst0`.
    *
    * Replaces every reference to a parametric function with a reference to its specialized version.
    *
    * Replaces every local variable symbol with a fresh local variable symbol.
    *
    * If a specialized version of a function does not yet exists, a fresh symbol is created for it, and the
    * definition and substitution is enqueued.
    */
  private def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: StrictSubstitution, def2def: Def2Def, defQueue: DefQueue)(implicit root: Root, flix: Flix): Expression = {

    // TODO: Monomorph: Must apply subst to all effects.

    /**
      * Specializes the given expression `e0` under the environment `env0`. w.r.t. the current substitution.
      */
    def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = e0 match {
      case Expression.Wild(tpe, loc) => Expression.Wild(subst0(tpe), loc)

      case Expression.Var(sym, tpe, loc) =>
        Expression.Var(env0(sym), subst0(tpe), loc)

      case Expression.Def(sym, tpe, loc) =>
        /*
         * !! This is where all the magic happens !!
         */
        val newSym = specializeDefSym(sym, subst0(tpe), def2def, defQueue)
        Expression.Def(newSym, subst0(tpe), loc)

      case Expression.Sig(sym, tpe, loc) =>
        val newSym = specializeSigSym(sym, subst0(tpe), def2def, defQueue)
        Expression.Def(newSym, subst0(tpe), loc)

      case Expression.Hole(sym, tpe, loc) => Expression.Hole(sym, subst0(tpe), loc)

      case Expression.Unit(loc) => Expression.Unit(loc)

      case Expression.Null(tpe, loc) => Expression.Null(subst0(tpe), loc)

      case Expression.True(loc) => Expression.True(loc)

      case Expression.False(loc) => Expression.False(loc)

      case Expression.Char(lit, loc) => Expression.Char(lit, loc)

      case Expression.Float32(lit, loc) => Expression.Float32(lit, loc)

      case Expression.Float64(lit, loc) => Expression.Float64(lit, loc)

      case Expression.Int8(lit, loc) => Expression.Int8(lit, loc)

      case Expression.Int16(lit, loc) => Expression.Int16(lit, loc)

      case Expression.Int32(lit, loc) => Expression.Int32(lit, loc)

      case Expression.Int64(lit, loc) => Expression.Int64(lit, loc)

      case Expression.BigInt(lit, loc) => Expression.BigInt(lit, loc)

      case Expression.Str(lit, loc) => Expression.Str(lit, loc)

      case Expression.Default(tpe, loc) =>
        //
        // Replace a default literal by the actual default value based on its type.
        //
        subst0(tpe).typeConstructor match {
          case None =>
            throw ReifyTypeException(tpe, loc)

          case Some(tc) => tc match {
            case TypeConstructor.Unit => Expression.Unit(loc)
            case TypeConstructor.Bool => Expression.False(loc)
            case TypeConstructor.Char => Expression.Char('0', loc)
            case TypeConstructor.Float32 => Expression.Float32(0, loc)
            case TypeConstructor.Float64 => Expression.Float64(0, loc)
            case TypeConstructor.Int8 => Expression.Int8(0, loc)
            case TypeConstructor.Int16 => Expression.Int16(0, loc)
            case TypeConstructor.Int32 => Expression.Int32(0, loc)
            case TypeConstructor.Int64 => Expression.Int64(0, loc)
            case TypeConstructor.BigInt => Expression.BigInt(BigInteger.ZERO, loc)
            case TypeConstructor.Str => Expression.Str("", loc)
            case _ => Expression.Null(subst0(tpe), loc)
          }
        }

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        val (p, env1) = specializeFormalParam(fparam, subst0)
        val e = visitExp(exp, env0 ++ env1)
        Expression.Lambda(p, e, subst0(tpe), loc)

      case Expression.Apply(exp, exps, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        val es = exps.map(visitExp(_, env0))
        Expression.Apply(e, es, subst0(tpe), pur, eff, loc)

      case Expression.Unary(sop, exp, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp, env0)
        Expression.Unary(sop, e1, subst0(tpe), pur, eff, loc)

      /*
       * Other Binary Expression.
       */
      case Expression.Binary(sop, exp1, exp2, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Binary(sop, e1, e2, subst0(tpe), pur, eff, loc)

      case Expression.Let(sym, mod, exp1, exp2, tpe, pur, eff, loc) =>
        // Generate a fresh symbol for the let-bound variable.
        val freshSym = Symbol.freshVarSym(sym)
        val env1 = env0 + (sym -> freshSym)
        Expression.Let(freshSym, mod, visitExp(exp1, env0), visitExp(exp2, env1), subst0(tpe), pur, eff, loc)

      case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, eff, loc) =>
        // Generate a fresh symbol for the let-bound variable.
        val freshSym = Symbol.freshVarSym(sym)
        val env1 = env0 + (sym -> freshSym)
        Expression.LetRec(freshSym, mod, visitExp(exp1, env1), visitExp(exp2, env1), subst0(tpe), pur, eff, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        val e3 = visitExp(exp3, env0)
        Expression.IfThenElse(e1, e2, e3, subst0(tpe), pur, eff, loc)

      case Expression.Stm(exp1, exp2, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Stm(e1, e2, subst0(tpe), pur, eff, loc)

      case Expression.Discard(exp, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.Discard(e, pur, eff, loc)

      case Expression.Match(exp, rules, tpe, pur, eff, loc) =>
        val rs = rules map {
          case MatchRule(pat, guard, body) =>
            val (p, env1) = visitPat(pat)
            val extendedEnv = env0 ++ env1
            val g = visitExp(guard, extendedEnv)
            val b = visitExp(body, extendedEnv)
            MatchRule(p, g, b)
        }
        Expression.Match(visitExp(exp, env0), rs, subst0(tpe), pur, eff, loc)

      case Expression.Choose(exps, rules, tpe, pur, eff, loc) =>
        val es = exps.map(visitExp(_, env0))
        val rs = rules.map {
          case ChoiceRule(pat, exp) =>
            val patAndEnv = pat.map {
              case ChoicePattern.Wild(loc) => (ChoicePattern.Wild(loc), Map.empty)
              case ChoicePattern.Absent(loc) => (ChoicePattern.Absent(loc), Map.empty)
              case ChoicePattern.Present(sym, tpe1, loc) =>
                val freshVar = Symbol.freshVarSym(sym)
                (ChoicePattern.Present(freshVar, subst0(tpe1), loc), Map(sym -> freshVar))
            }
            val p = patAndEnv.map(_._1)
            val env1 = patAndEnv.map(_._2).foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
              case (acc, m) => acc ++ m
            }
            val e = visitExp(exp, env0 ++ env1)
            ChoiceRule(p, e)
        }
        Expression.Choose(es, rs, tpe, pur, eff, loc)

      case Expression.Tag(sym, tag, exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.Tag(sym, tag, e, subst0(tpe), pur, eff, loc)

      case Expression.Tuple(elms, tpe, pur, eff, loc) =>
        val es = elms.map(e => visitExp(e, env0))
        Expression.Tuple(es, subst0(tpe), pur, eff, loc)

      case Expression.RecordEmpty(tpe, loc) =>
        Expression.RecordEmpty(subst0(tpe), loc)

      case Expression.RecordSelect(base, field, tpe, pur, eff, loc) =>
        val b = visitExp(base, env0)
        Expression.RecordSelect(b, field, subst0(tpe), pur, eff, loc)

      case Expression.RecordExtend(field, value, rest, tpe, pur, eff, loc) =>
        val v = visitExp(value, env0)
        val r = visitExp(rest, env0)
        Expression.RecordExtend(field, v, r, subst0(tpe), pur, eff, loc)

      case Expression.RecordRestrict(field, rest, tpe, pur, eff, loc) =>
        val r = visitExp(rest, env0)
        Expression.RecordRestrict(field, r, subst0(tpe), pur, eff, loc)

      case Expression.ArrayLit(exps, exp, tpe, pur, eff, loc) =>
        val es = exps.map(visitExp(_, env0))
        val e = visitExp(exp, env0)
        Expression.ArrayLit(es, e, subst0(tpe), pur, eff, loc)

      case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        val e3 = visitExp(exp3, env0)
        Expression.ArrayNew(e1, e2, e3, subst0(tpe), pur, eff, loc)

      case Expression.ArrayLoad(base, index, tpe, pur, eff, loc) =>
        val b = visitExp(base, env0)
        val i = visitExp(index, env0)
        Expression.ArrayLoad(b, i, subst0(tpe), pur, eff, loc)

      case Expression.ArrayStore(base, index, elm, eff, loc) =>
        val b = visitExp(base, env0)
        val i = visitExp(index, env0)
        val e = visitExp(elm, env0)
        Expression.ArrayStore(b, i, e, eff, loc)

      case Expression.ArrayLength(base, pur, eff, loc) =>
        val b = visitExp(base, env0)
        Expression.ArrayLength(b, pur, eff, loc)

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, eff, loc) =>
        val b = visitExp(base, env0)
        val i1 = visitExp(startIndex, env0)
        val i2 = visitExp(endIndex, env0)
        Expression.ArraySlice(b, i1, i2, subst0(tpe), eff, loc)

      case Expression.Ref(exp1, exp2, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Ref(e1, e2, subst0(tpe), pur, eff, loc)

      case Expression.Deref(exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.Deref(e, subst0(tpe), pur, eff, loc)

      case Expression.Assign(exp1, exp2, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Assign(e1, e2, subst0(tpe), pur, eff, loc)

      case Expression.Ascribe(exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.Ascribe(e, subst0(tpe), pur, eff, loc)

      case Expression.Cast(exp, _, _, _, tpe, pur, eff, loc) =>
        // We drop the declaredType and declaredEff here.
        val e = visitExp(exp, env0)
        Expression.Cast(e, None, None, None, subst0(tpe), pur, eff, loc)

      case Expression.TryCatch(exp, rules, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            // Generate a fresh symbol.
            val freshSym = Symbol.freshVarSym(sym)
            val env1 = env0 + (sym -> freshSym)
            val b = visitExp(body, env1)
            CatchRule(freshSym, clazz, b)
        }
        Expression.TryCatch(e, rs, subst0(tpe), pur, eff, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, pur, eff, loc) =>
        val as = args.map(visitExp(_, env0))
        Expression.InvokeConstructor(constructor, as, subst0(tpe), pur, eff, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        val as = args.map(visitExp(_, env0))
        Expression.InvokeMethod(method, e, as, tpe, pur, eff, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, pur, eff, loc) =>
        val as = args.map(visitExp(_, env0))
        Expression.InvokeStaticMethod(method, as, tpe, pur, eff, loc)

      case Expression.GetField(field, exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.GetField(field, e, tpe, pur, eff, loc)

      case Expression.PutField(field, exp1, exp2, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.PutField(field, e1, e2, tpe, pur, eff, loc)

      case Expression.GetStaticField(field, tpe, pur, eff, loc) =>
        Expression.GetStaticField(field, tpe, pur, eff, loc)

      case Expression.PutStaticField(field, exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.PutStaticField(field, e, tpe, pur, eff, loc)

      case Expression.NewObject(clazz, tpe, pur, eff, loc) =>
        Expression.NewObject(clazz, subst0(tpe), pur, eff, loc)

      case Expression.NewChannel(exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.NewChannel(e, subst0(tpe), pur, eff, loc)

      case Expression.GetChannel(exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.GetChannel(e, subst0(tpe), pur, eff, loc)

      case Expression.PutChannel(exp1, exp2, tpe, pur, eff, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.PutChannel(e1, e2, subst0(tpe), pur, eff, loc)

      case Expression.SelectChannel(rules, default, tpe, pur, eff, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) =>
            val freshSym = Symbol.freshVarSym(sym)
            val env1 = env0 + (sym -> freshSym)
            val c = visitExp(chan, env1)
            val e = visitExp(exp, env1)
            SelectChannelRule(freshSym, c, e)
        }

        val d = default.map(visitExp(_, env0))

        Expression.SelectChannel(rs, d, subst0(tpe), pur, eff, loc)

      case Expression.Spawn(exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.Spawn(e, subst0(tpe), pur, eff, loc)

      case Expression.Lazy(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Lazy(e, subst0(tpe), loc)

      case Expression.Force(exp, tpe, pur, eff, loc) =>
        val e = visitExp(exp, env0)
        Expression.Force(e, subst0(tpe), pur, eff, loc)

      case Expression.Region(_, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.Scope(_, _, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointConstraintSet(_, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointLambda(_, _, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointMerge(_, _, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointSolve(_, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointFilter(_, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointInject(_, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointProject(_, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.Reify(t, _, _, _, loc) =>
        // Magic!
        val isTrue = subst0(t) match {
          case Type.Cst(TypeConstructor.True, _) => true
          case Type.Cst(TypeConstructor.False, _) => false
          case other => throw ReifyBoolException(other, loc)
        }

        if (isTrue)
          Expression.True(loc)
        else
          Expression.False(loc)

      case Expression.ReifyType(t, k, _, _, _, loc) =>
        k match {
          case Kind.Bool => reifyBool(subst0(t), loc)
          case Kind.Star => reifyType(subst0(t), loc)
          case _ => throw InternalCompilerException(s"Unexpected kind: $k.")
        }

      case Expression.ReifyEff(sym, exp1, exp2, exp3, _, _, _, loc) =>
        // Magic!
        val isPure = subst0(exp1.tpe).arrowPurityType match {
          case Type.Cst(TypeConstructor.True, _) => true
          case _ => false
        }

        if (isPure) {
          // Generate a fresh symbol for the let-bound variable.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)

          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env1)
          Expression.Let(freshSym, Modifiers.Empty, e1, e2, e2.tpe, e2.pur, e2.eff, loc)
        } else {
          visitExp(exp3, env0)
        }
    }

    /**
      * Specializes the given pattern `p0` w.r.t. the current substitution.
      *
      * Returns the new pattern and a mapping from variable symbols to fresh variable symbols.
      */
    def visitPat(p0: Pattern): (Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = p0 match {
      case Pattern.Wild(tpe, loc) => (Pattern.Wild(subst0(tpe), loc), Map.empty)
      case Pattern.Var(sym, tpe, loc) =>
        // Generate a fresh variable symbol for the pattern-bound variable.
        val freshSym = Symbol.freshVarSym(sym)
        (Pattern.Var(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
      case Pattern.Unit(loc) => (Pattern.Unit(loc), Map.empty)
      case Pattern.True(loc) => (Pattern.True(loc), Map.empty)
      case Pattern.False(loc) => (Pattern.False(loc), Map.empty)
      case Pattern.Char(lit, loc) => (Pattern.Char(lit, loc), Map.empty)
      case Pattern.Float32(lit, loc) => (Pattern.Float32(lit, loc), Map.empty)
      case Pattern.Float64(lit, loc) => (Pattern.Float64(lit, loc), Map.empty)
      case Pattern.Int8(lit, loc) => (Pattern.Int8(lit, loc), Map.empty)
      case Pattern.Int16(lit, loc) => (Pattern.Int16(lit, loc), Map.empty)
      case Pattern.Int32(lit, loc) => (Pattern.Int32(lit, loc), Map.empty)
      case Pattern.Int64(lit, loc) => (Pattern.Int64(lit, loc), Map.empty)
      case Pattern.BigInt(lit, loc) => (Pattern.BigInt(lit, loc), Map.empty)
      case Pattern.Str(lit, loc) => (Pattern.Str(lit, loc), Map.empty)
      case Pattern.Tag(sym, tag, pat, tpe, loc) =>
        val (p, env1) = visitPat(pat)
        (Pattern.Tag(sym, tag, p, subst0(tpe), loc), env1)
      case Pattern.Tuple(elms, tpe, loc) =>
        val (ps, envs) = elms.map(p => visitPat(p)).unzip
        (Pattern.Tuple(ps, subst0(tpe), loc), envs.reduce(_ ++ _))
      case Pattern.Array(elms, tpe, loc) =>
        val (ps, envs) = elms.map(p => visitPat(p)).unzip
        (Pattern.Array(ps, subst0(tpe), loc), if (envs.isEmpty) Map.empty else envs.reduce(_ ++ _))
      case Pattern.ArrayTailSpread(elms, sym, tpe, loc) =>
        val freshSym = Symbol.freshVarSym(sym)
        val (ps, envs) = elms.map(p => visitPat(p)).unzip
        (Pattern.ArrayTailSpread(ps, freshSym, subst0(tpe), loc),
          if (envs.isEmpty) Map(sym -> freshSym)
          else envs.reduce(_ ++ _) ++ Map(sym -> freshSym))
      case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) =>
        val freshSym = Symbol.freshVarSym(sym)
        val (ps, envs) = elms.map(p => visitPat(p)).unzip
        (Pattern.ArrayHeadSpread(freshSym, ps, subst0(tpe), loc),
          if (envs.isEmpty) Map(sym -> freshSym)
          else envs.reduce(_ ++ _) ++ Map(sym -> freshSym))
    }

    visitExp(exp0, env0)
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  private def specializeDefSym(sym: Symbol.DefnSym, tpe: Type, def2def: Def2Def, defQueue: DefQueue)(implicit root: Root, flix: Flix): Symbol.DefnSym = {
    // Lookup the definition and its declared type.
    val defn = root.defs(sym)

    // Compute the erased type.
    val erasedType = eraseType(tpe)

    // Check if the function is non-polymorphic.
    if (defn.spec.tparams.isEmpty) {
      defn.sym
    } else {
      specializeDef(defn, erasedType, def2def, defQueue)
    }
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  private def specializeSigSym(sym: Symbol.SigSym, tpe: Type, def2def: Def2Def, defQueue: DefQueue)(implicit root: Root, flix: Flix): Symbol.DefnSym = {
    val sig = root.sigs(sym)

    // lookup the instance corresponding to this type
    val instances = root.instances(sig.sym.clazz)

    val defns = instances.flatMap {
      inst =>
        inst.defs.find {
          defn =>
            defn.sym.name == sig.sym.name && Unification.unifiesWith(defn.spec.declaredScheme.base, tpe, RigidityEnv.empty)
        }
    }

    (sig.impl, defns) match {
      // Case 1: An instance implementation exists. Use it.
      case (_, defn :: Nil) => specializeDef(defn, tpe, def2def, defQueue)
      // Case 2: No instance implementation, but a default implementation exists. Use it.
      case (Some(impl), Nil) => specializeDef(sigToDef(sig.sym, sig.spec, impl), tpe, def2def, defQueue)
      // Case 3: Multiple matching defs. Should have been caught previously.
      case (_, _ :: _ :: _) => throw InternalCompilerException(s"Expected at most one matching definition for '$sym', but found ${defns.size} signatures.")
      // Case 4: No matching defs and no default. Should have been caught previously.
      case (None, Nil) => throw InternalCompilerException(s"No default or matching definition found for '$sym'.")
    }
  }

  /**
    * Converts a signature with an implementation into the equivalent definition.
    */
  private def sigToDef(sigSym: Symbol.SigSym, spec: TypedAst.Spec, impl: TypedAst.Impl): TypedAst.Def = {
    TypedAst.Def(sigSymToDefnSym(sigSym), spec, impl)
  }

  /**
    * Converts a SigSym into the equivalent DefnSym.
    */
  private def sigSymToDefnSym(sigSym: Symbol.SigSym): Symbol.DefnSym = {
    val ns = sigSym.clazz.namespace :+ sigSym.clazz.name
    new Symbol.DefnSym(None, ns, sigSym.name, sigSym.loc)
  }

  /**
    * Returns the def symbol corresponding to the specialized def `defn` w.r.t. to the type `tpe`.
    */
  private def specializeDef(defn: TypedAst.Def, tpe: Type, def2def: Def2Def, defQueue: DefQueue)(implicit flix: Flix): Symbol.DefnSym = {
    // Unify the declared and actual type to obtain the substitution map.
    val subst = infallibleUnify(defn.impl.inferredScheme.base, tpe)

    // Check whether the function definition has already been specialized.
    def2def.get((defn.sym, tpe)) match {
      case None =>
        // Case 1: The function has not been specialized.
        // Generate a fresh specialized definition symbol.
        val freshSym = Symbol.freshDefnSym(defn.sym)

        // Register the fresh symbol (and actual type) in the symbol2symbol map.
        def2def.put((defn.sym, tpe), freshSym)

        // Enqueue the fresh symbol with the definition and substitution.
        enqueue((freshSym, defn, subst), defQueue)

        // Now simply refer to the freshly generated symbol.
        freshSym
      case Some(specializedSym) =>
        // Case 2: The function has already been specialized.
        // Simply refer to the already existing specialized symbol.
        specializedSym
    }

  }

  /**
    * Specializes the given formal parameters `fparams0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
    */
  private def specializeFormalParams(fparams0: List[FormalParam], subst0: StrictSubstitution)(implicit flix: Flix): (List[FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    // Return early if there are no formal parameters.
    if (fparams0.isEmpty)
      return (Nil, Map.empty)

    // Specialize each formal parameter and recombine the results.
    val (params, envs) = fparams0.map(p => specializeFormalParam(p, subst0)).unzip
    (params, envs.reduce(_ ++ _))
  }

  /**
    * Specializes the given formal parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  private def specializeFormalParam(fparam0: FormalParam, subst0: StrictSubstitution)(implicit flix: Flix): (FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
    val FormalParam(sym, mod, tpe, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    (FormalParam(freshSym, mod, subst0(tpe), loc), Map(sym -> freshSym))
  }

  /**
    * Specializes the given constraint parameters `cparams0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
    */
  private def specializeConstraintParams(cparams0: List[ConstraintParam], subst0: StrictSubstitution)(implicit flix: Flix): (List[ConstraintParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    // Return early if there are no formal parameters.
    if (cparams0.isEmpty)
      return (Nil, Map.empty)

    // Specialize each constraint parameter and recombine the results.
    val (params, envs) = cparams0.map(p => specializeConstraintParam(p, subst0)).unzip
    (params, envs.reduce(_ ++ _))
  }

  /**
    * Specializes the given constraint parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new constraint parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  private def specializeConstraintParam(cparam0: ConstraintParam, subst0: StrictSubstitution)(implicit flix: Flix): (ConstraintParam, Map[Symbol.VarSym, Symbol.VarSym]) = cparam0 match {
    case ConstraintParam.HeadParam(sym, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      (ConstraintParam.HeadParam(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
    case ConstraintParam.RuleParam(sym, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      (ConstraintParam.RuleParam(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
  }

  /**
    * Returns an expression that evaluates to a ReifiedBool for the given type `tpe`.
    */
  private def reifyBool(tpe: Type, loc: SourceLocation): Expression = {
    val sym = Symbol.mkEnumSym("ReifiedBool")
    val resultTpe = Type.mkEnum(sym, Kind.Star, loc)
    val resultEff = Type.Pure

    tpe.typeConstructor match {
      case None =>
        throw ReifyTypeException(tpe, loc)

      case Some(tc) => tc match {
        case TypeConstructor.True =>
          val tag = Name.Tag("ReifiedTrue", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.False =>
          val tag = Name.Tag("ReifiedFalse", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case _ =>
          val tag = Name.Tag("ErasedBool", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)
      }
    }
  }

  /**
    * Returns an expression that evaluates to a ReifiedType for the given type `tpe`.
    */
  private def reifyType(tpe: Type, loc: SourceLocation): Expression = {
    val sym = Symbol.mkEnumSym("ReifiedType")
    val resultTpe = Type.mkEnum(sym, Kind.Star, loc)
    val resultEff = Type.Pure

    def visit(t0: Type): Expression = t0.typeConstructor match {
      case None =>
        throw ReifyTypeException(tpe, loc)

      case Some(tc) => tc match {
        case TypeConstructor.Unit =>
          val tag = Name.Tag("ReifiedUnit", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Bool =>
          val tag = Name.Tag("ReifiedBool", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Char =>
          val tag = Name.Tag("ReifiedChar", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Float32 =>
          val tag = Name.Tag("ReifiedFloat32", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Float64 =>
          val tag = Name.Tag("ReifiedFloat64", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Int8 =>
          val tag = Name.Tag("ReifiedInt8", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Int16 =>
          val tag = Name.Tag("ReifiedInt16", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Int32 =>
          val tag = Name.Tag("ReifiedInt32", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Int64 =>
          val tag = Name.Tag("ReifiedInt64", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

        case TypeConstructor.Array =>
          val tag = Name.Tag("ReifiedArray", loc)
          val innerTpe = Type.eraseAliases(t0).typeArguments.head
          val innerExp = visit(innerTpe)
          Expression.Tag(sym, tag, innerExp, resultTpe, resultEff, loc)

        case _ =>
          val tag = Name.Tag("ErasedType", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc)

      }
    }

    visit(tpe)
  }

  /**
    * Unifies `tpe1` and `tpe2` which must be unifiable.
    */
  private def infallibleUnify(tpe1: Type, tpe2: Type)(implicit flix: Flix): StrictSubstitution = {
    Unification.unifyTypes(tpe1, tpe2, RigidityEnv.empty) match {
      case Result.Ok(subst) =>
        StrictSubstitution(subst)
      case Result.Err(_) =>
        throw InternalCompilerException(s"Unable to unify: '$tpe1' and '$tpe2'.")
    }
  }

  /**
    * Performs type erasure on the given type `tpe`.
    *
    * Flix does not erase normal types, but it does erase Boolean formulas.
    */
  private def eraseType(tpe: Type)(implicit flix: Flix): Type = tpe match {
    case Type.KindedVar(_, loc) =>
      if (flix.options.xstrictmono)
        throw UnexpectedNonConstBool(tpe, loc)
      else {
        // TODO: We should return Type.ErasedBool or something.
        Type.True
      }

    case Type.Cst(_, _) => tpe

    case Type.Apply(tpe1, tpe2, loc) =>
      val t1 = eraseType(tpe1)
      val t2 = eraseType(tpe2)
      Type.Apply(t1, t2, loc)

    case Type.Alias(sym, args, tpe, loc) =>
      val as = args.map(eraseType)
      val t = eraseType(tpe)
      Type.Alias(sym, as, t, loc)

    case Type.UnkindedVar(_, loc) => throw InternalCompilerException(s"Unexpected type at: ${loc.format}")

    case Type.UnkindedArrow(_, _, loc) => throw InternalCompilerException(s"Unexpected type at: ${loc.format}")

    case Type.ReadWrite(_, loc) => throw InternalCompilerException(s"Unexpected type at: ${loc.format}")

    case Type.Ascribe(_, _, loc) => throw InternalCompilerException(s"Unexpected type at: ${loc.format}")
  }

}
