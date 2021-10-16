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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Kind, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
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
object Monomorph extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are replaced by the
    * Unit type. In other words, when performing a type substitution if there is no requirement on a polymorphic type
    * we assume it to be Unit. This is safe since otherwise the type would not be polymorphic after type-inference.
    */
  case class StrictSubstitution(s: Substitution)(implicit flix: Flix) {
    /**
      * Returns `true` if this substitution is empty.
      */
    def isEmpty: Boolean = s.isEmpty

    /**
      * Applies `this` substitution to the given type `tpe`.
      *
      * NB: Applies the substitution first, then replaces every type variable with the unit type.
      */
    def apply(tpe0: Type): Validation[Type, CompilationError] = {
      val t = s(tpe0)

      transformType(t) {
        case Type.KindedVar(_, Kind.Bool, loc, _, _) =>
          // TODO: In strict mode we demand that there are no free (uninstantiated) Boolean variables.
          // In the future we need to decide what should actually happen if such variables occur.
          if (flix.options.xstrictmono) {
            ReificationError.UnexpectedNonConstBool(tpe0, loc).toFailure
          } else
            Type.True.toSuccess
        case Type.KindedVar(_, Kind.RecordRow, _, _, _) => Type.RecordRowEmpty.toSuccess
        case Type.KindedVar(_, Kind.SchemaRow, _, _, _) => Type.SchemaRowEmpty.toSuccess
        case _ => Type.Unit.toSuccess
      }
    }

    private def transformType[E](tpe: Type)(f: Type.KindedVar => Validation[Type, E]): Validation[Type, E] = tpe match {
      case tvar: Type.Var => f(tvar.asKinded)
      case Type.Cst(_, _) => tpe.toSuccess
      case Type.Lambda(tvar, tpe, loc) => transformType(tpe)(f).map(Type.Lambda(tvar, _, loc))
      case Type.Apply(tpe1, tpe2, loc) => mapN(transformType(tpe1)(f), transformType(tpe2)(f)) {
        (ntpe1, ntpe2) => Type.Apply(ntpe1, ntpe2, loc)
      }
      case Type.Ascribe(tpe, kind, loc) => transformType(tpe)(f).map(Type.Ascribe(_, kind, loc))
    }
  }

  private type DefQueue = mutable.Queue[(Symbol.DefnSym, Def, StrictSubstitution)]

  private type Def2Def = mutable.Map[(Symbol.DefnSym, Type), Symbol.DefnSym]

  /**
    * Performs monomorphization of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Monomorph") {

    /**
      * A function-local queue of pending (fresh symbol, function definition, and substitution)-triples.
      *
      * For example, if the queue contains the entry:
      *
      * -   (f$1, f, [a -> Int])
      *
      * it means that the function definition f should be specialized w.r.t. the map [a -> Int] under the fresh name f$1.
      */
    implicit val defQueue: DefQueue = mutable.Queue.empty

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
    implicit val def2def: Def2Def = mutable.Map.empty

    implicit val r: Root = root

    // A map used to collect specialized definitions, etc.
    val specializedDefns: mutable.Map[Symbol.DefnSym, TypedAst.Def] = mutable.Map.empty

    // Collect all non-parametric function definitions.
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.spec.tparams.isEmpty
    }

    // Perform specialization of all non-parametric function definitions.
    for ((sym, defn) <- nonParametricDefns) {
      // Get a substitution from the inferred scheme to the declared scheme.
      // This is necessary because the inferred scheme may be more generic than the declared scheme.
      val subst = Unification.unifyTypes(defn.spec.declaredScheme.base, defn.impl.inferredScheme.base) match {
        case Result.Ok(subst1) => subst1
        // This should not happen, since the Typer guarantees that the schemes unify
        case Result.Err(_) => throw InternalCompilerException("Failed to unify declared and inferred schemes.")
      }
      val subst0 = StrictSubstitution(subst)

      // Specialize the formal parameters to obtain fresh local variable symbols for them.
      val (fparams, env0) = specializeFormalParams(defn.spec.fparams, subst0) match {
        case Success(t) => t
        case Failure(errors) => return Failure(errors)
      }

      // Specialize the body expression.
      val body = specialize(defn.impl.exp, env0, subst0) match {
        case Success(t) => t
        case Failure(errors) => return Failure(errors)
      }

      // Reassemble the definition.
      specializedDefns.put(sym, defn.copy(spec = defn.spec.copy(fparams = fparams), impl = defn.impl.copy(exp = body)))
    }

    // Performs function specialization until both queues are empty.
    while (defQueue.nonEmpty) {

      // Performs function specialization until the queue is empty.
      while (defQueue.nonEmpty) {
        // Extract a function from the queue and specializes it w.r.t. its substitution.
        val (freshSym, defn, subst) = defQueue.dequeue()

        flix.subtask(freshSym.toString, sample = true)

        // Specialize the formal parameters and introduce fresh local variable symbols.
        val (fparams, env0) = specializeFormalParams(defn.spec.fparams, subst) match {
          case Success(t) => t
          case Failure(errors) => return Failure(errors)
        }

        // Specialize the body expression.
        val specializedExp = specialize(defn.impl.exp, env0, subst) match {
          case Success(t) => t
          case Failure(errors) => return Failure(errors)
        }

        subst(defn.impl.inferredScheme.base) match {
          case Failure(errors) => Failure(errors)
          case Success(base) =>
            // Reassemble the definition.
            // NB: Removes the type parameters as the function is now monomorphic.
            val specializedDefn = defn.copy(
              sym = freshSym,
              spec = defn.spec.copy(fparams = fparams, tparams = Nil),
              impl = TypedAst.Impl(specializedExp, Scheme(Nil, List.empty, base)))

            // Save the specialized function.
            specializedDefns.put(freshSym, specializedDefn)
        }

      }

    }

    // Reassemble the AST.
    root.copy(
      defs = specializedDefns.toMap
    ).toSuccess
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
  def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: StrictSubstitution)(implicit root: Root, flix: Flix, def2def: Def2Def, defQueue: DefQueue): Validation[Expression, CompilationError] = {

    /**
      * Specializes the given expression `e0` under the environment `env0`. w.r.t. the current substitution.
      */
    def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Validation[Expression, CompilationError] = e0 match {
      case Expression.Wild(tpe, loc) => subst0(tpe).map(Expression.Wild(_, loc))

      case Expression.Var(sym, tpe, loc) => subst0(tpe).map(Expression.Var(env0(sym), _, loc))

      case Expression.Def(sym, tpe, loc) =>
        // !! This is where all the magic happens !!
        subst0(tpe).map(t => Expression.Def(specializeDefSym(sym, t), t, loc))

      case Expression.Sig(sym, tpe, loc) =>
        subst0(tpe).map(t => Expression.Def(specializeSigSym(sym, t), t, loc))

      case Expression.Hole(sym, tpe, loc) => subst0(tpe).map(Expression.Hole(sym, _, loc))

      case Expression.Unit(loc) => Expression.Unit(loc).toSuccess

      case Expression.Null(tpe, loc) => subst0(tpe).map(Expression.Null(_, loc))

      case Expression.True(loc) => Expression.True(loc).toSuccess

      case Expression.False(loc) => Expression.False(loc).toSuccess

      case Expression.Char(lit, loc) => Expression.Char(lit, loc).toSuccess

      case Expression.Float32(lit, loc) => Expression.Float32(lit, loc).toSuccess

      case Expression.Float64(lit, loc) => Expression.Float64(lit, loc).toSuccess

      case Expression.Int8(lit, loc) => Expression.Int8(lit, loc).toSuccess

      case Expression.Int16(lit, loc) => Expression.Int16(lit, loc).toSuccess

      case Expression.Int32(lit, loc) => Expression.Int32(lit, loc).toSuccess

      case Expression.Int64(lit, loc) => Expression.Int64(lit, loc).toSuccess

      case Expression.BigInt(lit, loc) => Expression.BigInt(lit, loc).toSuccess

      case Expression.Str(lit, loc) => Expression.Str(lit, loc).toSuccess

      case Expression.Default(tpe, loc) =>
        // Replace a default literal by the actual default value based on its type.
        subst0(tpe).map(_.typeConstructor) flatMap {
          case None =>
            ReificationError.IllegalReifiedType(tpe, loc).toFailure

          case Some(tc) => tc match {
            case TypeConstructor.Unit => Expression.Unit(loc).toSuccess
            case TypeConstructor.Bool => Expression.False(loc).toSuccess
            case TypeConstructor.Char => Expression.Char('0', loc).toSuccess
            case TypeConstructor.Float32 => Expression.Float32(0, loc).toSuccess
            case TypeConstructor.Float64 => Expression.Float64(0, loc).toSuccess
            case TypeConstructor.Int8 => Expression.Int8(0, loc).toSuccess
            case TypeConstructor.Int16 => Expression.Int16(0, loc).toSuccess
            case TypeConstructor.Int32 => Expression.Int32(0, loc).toSuccess
            case TypeConstructor.Int64 => Expression.Int64(0, loc).toSuccess
            case TypeConstructor.BigInt => Expression.BigInt(BigInteger.ZERO, loc).toSuccess
            case TypeConstructor.Str => Expression.Str("", loc).toSuccess
            case _ => subst0(tpe).map(Expression.Null(_, loc))
          }
        }

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        for {
          pair <- specializeFormalParam(fparam, subst0)
          (p, env1) = pair
          e <- visitExp(exp, env0 ++ env1)
          t <- subst0(tpe)
        } yield Expression.Lambda(p, e, t, loc)

      case Expression.Apply(exp, exps, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          es <- traverse(exps)(visitExp(_, env0))
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Apply(e, es, t, ef, loc)

      case Expression.Unary(sop, exp, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Unary(sop, e1, t, ef, loc)

      // Other Binary Expression.
      case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp1, env0)
          e2 <- visitExp(exp2, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Binary(sop, e1, e2, t, ef, loc)

      case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        // Generate a fresh symbol for the let-bound variable.
        val freshSym = Symbol.freshVarSym(sym)
        val env1 = env0 + (sym -> freshSym)
        for {
          e1 <- visitExp(exp1, env1)
          e2 <- visitExp(exp2, env1)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Let(freshSym, mod, e1, e2, t, ef, loc)

      case Expression.LetRegion(sym, exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.LetRegion(sym, e, t, ef, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp1, env0)
          e2 <- visitExp(exp2, env0)
          e3 <- visitExp(exp3, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.IfThenElse(e1, e2, e3, t, ef, loc)

      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp1, env0)
          e2 <- visitExp(exp2, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Stm(e1, e2, t, ef, loc)

      case Expression.Match(exp, rules, tpe, eff, loc) =>
        for {
          rs <- traverse(rules) {
            case MatchRule(pat, guard, body) =>
              for {
                pair <- visitPat(pat)
                (p, env1) = pair
                extendedEnv = env0 ++ env1
                g <- visitExp(guard, extendedEnv)
                b <- visitExp(body, extendedEnv)
              } yield MatchRule(p, g, b)
          }
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Match(e, rs, t, ef, loc)

      case Expression.Choose(exps, rules, tpe, eff, loc) =>
        for {
          es <- traverse(exps)(visitExp(_, env0))
          rs <- traverse(rules) {
            case ChoiceRule(pat, exp) =>
              for {
                patAndEnv <- traverse(pat) {
                  case ChoicePattern.Wild(loc) => (ChoicePattern.Wild(loc), Map.empty).toSuccess
                  case ChoicePattern.Absent(loc) => (ChoicePattern.Absent(loc), Map.empty).toSuccess
                  case ChoicePattern.Present(sym, tpe1, loc) =>
                    val freshVar = Symbol.freshVarSym(sym)
                    subst0(tpe1).map(t => (ChoicePattern.Present(freshVar, t, loc), Map(sym -> freshVar)))
                }
                p = patAndEnv.map(_._1)
                env1 = patAndEnv.map(_._2).foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
                  case (acc, m) => acc ++ m
                }
                e <- visitExp(exp, env0 ++ env1)
              } yield ChoiceRule(p, e)
          }
          // TODO dont subst type?
        } yield Expression.Choose(es, rs, tpe, eff, loc)

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Tag(sym, tag, e, t, ef, loc)

      case Expression.Tuple(elms, tpe, eff, loc) =>
        for {
          es <- traverse(elms)(visitExp(_, env0))
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Tuple(es, t, ef, loc)

      case Expression.RecordEmpty(tpe, loc) =>
        subst0(tpe).map(Expression.RecordEmpty(_, loc))

      case Expression.RecordSelect(base, field, tpe, eff, loc) =>
        for {
          b <- visitExp(base, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.RecordSelect(b, field, t, ef, loc)

      case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
        for {
          v <- visitExp(value, env0)
          r <- visitExp(rest, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.RecordExtend(field, v, r, t, ef, loc)

      case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
        for {
          r <- visitExp(rest, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.RecordRestrict(field, r, t, ef, loc)

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        for {
          es <- traverse(elms)(visitExp(_, env0))
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.ArrayLit(es, t, ef, loc)

      case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        for {
          e <- visitExp(elm, env0)
          ln <- visitExp(len, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.ArrayNew(e, ln, t, ef, loc)

      case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        for {
          b <- visitExp(base, env0)
          i <- visitExp(index, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.ArrayLoad(b, i, t, ef, loc)

      case Expression.ArrayStore(base, index, elm, loc) =>
        for {
          b <- visitExp(base, env0)
          i <- visitExp(index, env0)
          e <- visitExp(elm, env0)
        } yield Expression.ArrayStore(b, i, e, loc)

      case Expression.ArrayLength(base, eff, loc) =>
        for {
          b <- visitExp(base, env0)
          ef <- subst0(eff)
        } yield Expression.ArrayLength(b, ef, loc)

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        for {
          b <- visitExp(base, env0)
          i1 <- visitExp(startIndex, env0)
          i2 <- visitExp(endIndex, env0)
          t <- subst0(tpe)
        } yield Expression.ArraySlice(b, i1, i2, t, loc)

      case Expression.Ref(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Ref(e, t, ef, loc)

      case Expression.Deref(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Deref(e, t, ef, loc)

      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp1, env0)
          e2 <- visitExp(exp2, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Assign(e1, e2, t, ef, loc)

      case Expression.Existential(fparam, exp, loc) =>
        for {
          pair <- specializeFormalParam(fparam, subst0)
          (param, env1) = pair
          e <- visitExp(exp, env0 ++ env1)
        } yield Expression.Existential(param, e, loc)

      case Expression.Universal(fparam, exp, loc) =>
        for {
          pair <- specializeFormalParam(fparam, subst0)
          (param, env1) = pair
          e <- visitExp(exp, env0 ++ env1)
        } yield Expression.Universal(param, e, loc)

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Ascribe(e, t, ef, loc)

      case Expression.Cast(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Cast(e, t, ef, loc)

      case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          rs <- traverse(rules) {
            case CatchRule(sym, clazz, body) =>
              // Generate a fresh symbol.
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              visitExp(body, env1).map(
                CatchRule(freshSym, clazz, _)
              )
          }
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.TryCatch(e, rs, t, ef, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        for {
          as <- traverse(args)(visitExp(_, env0))
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.InvokeConstructor(constructor, as, t, ef, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          as <- traverse(args)(visitExp(_, env0))
          // TODO dont subst type?
        } yield Expression.InvokeMethod(method, e, as, tpe, eff, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        for {
          as <- traverse(args)(visitExp(_, env0))
          // TODO dont subst type?
        } yield Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

      case Expression.GetField(field, exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          // TODO dont subst type?
        } yield Expression.GetField(field, e, tpe, eff, loc)

      case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp1, env0)
          e2 <- visitExp(exp2, env0)
          // TODO dont subst type?
        } yield Expression.PutField(field, e1, e2, tpe, eff, loc)

      case Expression.GetStaticField(field, tpe, eff, loc) =>
        // TODO dont subst type?
        Expression.GetStaticField(field, tpe, eff, loc).toSuccess

      case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          // TODO dont subst type?
        } yield Expression.PutStaticField(field, e, tpe, eff, loc)

      case Expression.NewChannel(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.NewChannel(e, t, ef, loc)

      case Expression.GetChannel(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.GetChannel(e, t, ef, loc)

      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        for {
          e1 <- visitExp(exp1, env0)
          e2 <- visitExp(exp2, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.PutChannel(e1, e2, t, ef, loc)

      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        for {
          rs <- traverse(rules) {
            case SelectChannelRule(sym, chan, exp) =>
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              for {
                c <- visitExp(chan, env1)
                e <- visitExp(exp, env1)
              } yield SelectChannelRule(freshSym, c, e)
          }
          d <- default.map(visitExp(_, env0)) match {
            case Some(value) => value.map(Some(_))
            case None => None.toSuccess
          }
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.SelectChannel(rs, d, t, ef, loc)

      case Expression.Spawn(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Spawn(e, t, ef, loc)

      case Expression.Lazy(exp, tpe, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
        } yield Expression.Lazy(e, t, loc)

      case Expression.Force(exp, tpe, eff, loc) =>
        for {
          e <- visitExp(exp, env0)
          t <- subst0(tpe)
          ef <- subst0(eff)
        } yield Expression.Force(e, t, ef, loc)

      case Expression.FixpointConstraintSet(_, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointMerge(_, _, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointSolve(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointFilter(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointProjectIn(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.FixpointProjectOut(_, _, _, _, loc) =>
        throw InternalCompilerException(s"Unexpected expression near: ${loc.format}.")

      case Expression.Reify(t, _, _, loc) =>
        // Magic!
        for {
          tpe <- subst0(t)
          isTrue <- tpe match {
            case Type.Cst(TypeConstructor.True, _) => true.toSuccess
            case Type.Cst(TypeConstructor.False, _) => false.toSuccess
            case other => ReificationError.IllegalReifiedBool(other, loc).toFailure
          }
          res = if (isTrue) Expression.True(loc) else Expression.False(loc)
        } yield res

      case Expression.ReifyType(t, _, _, loc) => subst0(t).flatMap(reifyType(_, loc))
    }

    /**
      * Specializes the given pattern `p0` w.r.t. the current substitution.
      *
      * Returns the new pattern and a mapping from variable symbols to fresh variable symbols.
      */
    def visitPat(p0: Pattern): Validation[(Pattern, Map[Symbol.VarSym, Symbol.VarSym]), CompilationError] = {
      p0 match {
        case Pattern.Wild(tpe, loc) => subst0(tpe).map(t => (Pattern.Wild(t, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]))
        case Pattern.Var(sym, tpe, loc) =>
          // Generate a fresh variable symbol for the pattern-bound variable.
          val freshSym = Symbol.freshVarSym(sym)
          subst0(tpe).map(t => (Pattern.Var(freshSym, t, loc), Map(sym -> freshSym)))
        case Pattern.Unit(loc) => (Pattern.Unit(loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.True(loc) => (Pattern.True(loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.False(loc) => (Pattern.False(loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Char(lit, loc) => (Pattern.Char(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Float32(lit, loc) => (Pattern.Float32(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Float64(lit, loc) => (Pattern.Float64(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Int8(lit, loc) => (Pattern.Int8(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Int16(lit, loc) => (Pattern.Int16(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Int32(lit, loc) => (Pattern.Int32(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Int64(lit, loc) => (Pattern.Int64(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.BigInt(lit, loc) => (Pattern.BigInt(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Str(lit, loc) => (Pattern.Str(lit, loc), Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess
        case Pattern.Tag(sym, tag, pat, tpe, loc) =>
          for {
            pair <- visitPat(pat)
            (p, env1) = pair
            t <- subst0(tpe)
          } yield (Pattern.Tag(sym, tag, p, t, loc), env1)
        case Pattern.Tuple(elms, tpe, loc) =>
          for {
            pair <- traverse(elms)(p => visitPat(p)).map(_.unzip)
            (ps, envs) = pair
            t <- subst0(tpe)
          } yield (Pattern.Tuple(ps, t, loc), envs.reduce(_ ++ _))
        case Pattern.Array(elms, tpe, loc) =>
          for {
            pair <- traverse(elms)(visitPat).map(_.unzip)
            (ps, envs) = pair
            t <- subst0(tpe)
          } yield (Pattern.Array(ps, t, loc), if (envs.isEmpty) Map.empty[Symbol.VarSym, Symbol.VarSym] else envs.reduce(_ ++ _))
        case Pattern.ArrayTailSpread(elms, sym, tpe, loc) =>
          val freshSym = Symbol.freshVarSym(sym)
          for {
            pair <- traverse(elms)(visitPat).map(_.unzip)
            (ps, envs) = pair
            t <- subst0(tpe)
          } yield (Pattern.ArrayTailSpread(ps, freshSym, t, loc),
            if (envs.isEmpty) Map(sym -> freshSym)
            else envs.reduce(_ ++ _) ++ Map(sym -> freshSym))
        case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) =>
          val freshSym = Symbol.freshVarSym(sym)
          for {
            pair <- traverse(elms)(visitPat).map(_.unzip)
            (ps, envs) = pair
            t <- subst0(tpe)
          } yield (Pattern.ArrayHeadSpread(freshSym, ps, t, loc),
            if (envs.isEmpty) Map(sym -> freshSym)
            else envs.reduce(_ ++ _) ++ Map(sym -> freshSym))
      }
    }

    visitExp(exp0, env0)
  }


  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  def specializeDefSym(sym: Symbol.DefnSym, tpe: Type)(implicit root: Root, flix: Flix, def2def: Def2Def, defQueue: DefQueue): Symbol.DefnSym = {
    // Lookup the definition and its declared type.
    val defn = root.defs(sym)

    // Check if the function is non-polymorphic.
    if (defn.spec.tparams.isEmpty) {
      defn.sym
    } else {
      specializeDef(defn, tpe)
    }
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  def specializeSigSym(sym: Symbol.SigSym, tpe: Type)(implicit root: Root, flix: Flix, def2def: Def2Def, defQueue: DefQueue): Symbol.DefnSym = {
    val sig = root.sigs(sym)

    // lookup the instance corresponding to this type
    val instances = root.instances(sig.sym.clazz)

    val defns = instances.flatMap {
      inst =>
        inst.defs.find {
          defn =>
            defn.sym.name == sig.sym.name && Unification.unifiesWith(defn.spec.declaredScheme.base, tpe)
        }
    }

    (sig.impl, defns) match {
      // Case 1: An instance implementation exists. Use it.
      case (_, defn :: Nil) => specializeDef(defn, tpe)
      // Case 2: No instance implementation, but a default implementation exists. Use it.
      case (Some(impl), Nil) => specializeDef(sigToDef(sig.sym, sig.spec, impl), tpe)
      // Case 3: Multiple matching defs. Should have been caught previously.
      case (_, _ :: _ :: _) => throw InternalCompilerException(s"Expected at most one matching definition for '$sym', but found ${defns.size} signatures.")
      // Case 4: No matching defs and no default. Should have been caught previously.
      case (None, Nil) => throw InternalCompilerException(s"No default or matching definition found for '$sym'.")
    }
  }

  /**
    * Converts a signature with an implementation into the equivalent definition.
    */
  def sigToDef(sigSym: Symbol.SigSym, spec: TypedAst.Spec, impl: TypedAst.Impl): TypedAst.Def = {
    TypedAst.Def(sigSymToDefnSym(sigSym), spec, impl)
  }

  /**
    * Converts a SigSym into the equivalent DefnSym.
    */
  def sigSymToDefnSym(sigSym: Symbol.SigSym): Symbol.DefnSym = {
    val ns = sigSym.clazz.namespace :+ sigSym.clazz.name
    new Symbol.DefnSym(None, ns, sigSym.name, sigSym.loc)
  }

  /**
    * Returns the def symbol corresponding to the specialized def `defn` w.r.t. to the type `tpe`.
    */
  def specializeDef(defn: TypedAst.Def, tpe: Type)(implicit flix: Flix, def2def: Def2Def, defQueue: DefQueue): Symbol.DefnSym = {
    // Unify the declared and actual type to obtain the substitution map.
    val subst = StrictSubstitution(Unification.unifyTypes(defn.impl.inferredScheme.base, tpe).get)

    // Check whether the function definition has already been specialized.
    def2def.get((defn.sym, tpe)) match {
      case None =>
        // Case 1: The function has not been specialized.
        // Generate a fresh specialized definition symbol.
        val freshSym = Symbol.freshDefnSym(defn.sym)

        // Register the fresh symbol (and actual type) in the symbol2symbol map.
        def2def.put((defn.sym, tpe), freshSym)

        // Enqueue the fresh symbol with the definition and substitution.
        defQueue.enqueue((freshSym, defn, subst))

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
  def specializeFormalParams(fparams0: List[FormalParam], subst0: StrictSubstitution)(implicit flix: Flix): Validation[(List[FormalParam], Map[Symbol.VarSym, Symbol.VarSym]), CompilationError] = {
    // Return early if there are no formal parameters.
    if (fparams0.isEmpty)
      return (Nil: List[FormalParam], Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess

    // Specialize each formal parameter and recombine the results.
    traverse(fparams0)(p => specializeFormalParam(p, subst0)).map(_.unzip).map {
      case (params, envs) => (params, envs.reduce(_ ++ _))
    }

  }

  /**
    * Specializes the given formal parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  def specializeFormalParam(fparam0: FormalParam, subst0: StrictSubstitution)(implicit flix: Flix): Validation[(FormalParam, Map[Symbol.VarSym, Symbol.VarSym]), CompilationError] = {
    val FormalParam(sym, mod, tpe, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    subst0(tpe).map(t => (FormalParam(freshSym, mod, t, loc), Map(sym -> freshSym)))
  }

  /**
    * Specializes the given constraint parameters `cparams0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
    */
  def specializeConstraintParams(cparams0: List[ConstraintParam], subst0: StrictSubstitution)(implicit flix: Flix): Validation[(List[ConstraintParam], Map[Symbol.VarSym, Symbol.VarSym]), CompilationError] = {
    // Return early if there are no formal parameters.
    if (cparams0.isEmpty)
      return (Nil: List[ConstraintParam], Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess

    // Specialize each constraint parameter and recombine the results.
    traverse(cparams0)(p => specializeConstraintParam(p, subst0)).map(_.unzip).map {
      case (params, envs) => (params, envs.reduce(_ ++ _))
    }

  }

  /**
    * Specializes the given constraint parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new constraint parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  def specializeConstraintParam(cparam0: ConstraintParam, subst0: StrictSubstitution)(implicit flix: Flix): Validation[(ConstraintParam, Map[Symbol.VarSym, Symbol.VarSym]), CompilationError] = cparam0 match {
    case ConstraintParam.HeadParam(sym, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      subst0(tpe).map(t => (ConstraintParam.HeadParam(freshSym, t, loc), Map(sym -> freshSym)))
    case ConstraintParam.RuleParam(sym, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      subst0(tpe).map(t => (ConstraintParam.RuleParam(freshSym, t, loc), Map(sym -> freshSym)))
  }

  /**
    * Returns an expression that evaluates to a ReifiedType for the given type `tpe`.
    */
  private def reifyType(tpe: Type, loc: SourceLocation): Validation[Expression, CompilationError] = {
    val sym = Symbol.mkEnumSym("ReifiedType")
    val resultTpe = Type.mkEnum(sym, Kind.Star, loc)
    val resultEff = Type.Pure

    def visit(t0: Type): Validation[Expression, CompilationError] = t0.typeConstructor match {
      case None =>
        ReificationError.IllegalReifiedType(tpe, loc).toFailure

      case Some(tc) => tc match {
        case TypeConstructor.Bool =>
          val tag = Name.Tag("ReifiedBool", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Char =>
          val tag = Name.Tag("ReifiedChar", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Float32 =>
          val tag = Name.Tag("ReifiedFloat32", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Float64 =>
          val tag = Name.Tag("ReifiedFloat64", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Int8 =>
          val tag = Name.Tag("ReifiedInt8", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Int16 =>
          val tag = Name.Tag("ReifiedInt16", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Int32 =>
          val tag = Name.Tag("ReifiedInt32", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Int64 =>
          val tag = Name.Tag("ReifiedInt64", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Str =>
          val tag = Name.Tag("ErasedType", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.Array =>
          val tag = Name.Tag("ReifiedArray", loc)
          val innerTpe = t0.typeArguments.head
          visit(innerTpe).map(Expression.Tag(sym, tag, _, resultTpe, resultEff, loc))

        case _ => ReificationError.IllegalReifiedType(tpe, loc).toFailure
      }
    }

    visit(tpe)
  }

}
