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
  private case class StrictSubstitution(s: Substitution)(implicit flix: Flix) {
    /**
      * Applies `this` substitution to the given type `tpe`.
      *
      * NB: Applies the substitution first, then replaces every type variable with the unit type.
      */
    def apply(tpe0: Type): Validation[Type, CompilationMessage] = {
      val t = s(tpe0)

      t.mapV {
        case Type.KindedVar(_, Kind.Bool, loc, _, _) =>
          // TODO: In strict mode we demand that there are no free (uninstantiated) Boolean variables.
          // TODO: In the future we need to decide what should actually happen if such variables occur.
          // TODO: In particular, it seems there are two cases.
          // TODO: A. Variables that occur inside the specialized types (those we can erase?)
          // TODO: B. Variables that occur inside an expression but nowhere else really.
          if (flix.options.xstrictmono) {
            ReificationError.UnexpectedNonConstBool(tpe0, loc).toFailure[Type, CompilationMessage]
          } else
            Type.True.toSuccess
        case Type.KindedVar(_, Kind.RecordRow, _, _, _) => Type.RecordRowEmpty.toSuccess
        case Type.KindedVar(_, Kind.SchemaRow, _, _, _) => Type.SchemaRowEmpty.toSuccess
        case _ => Type.Unit.toSuccess
      }
    }
  }

  /**
    * An exception raised to indicate that a Boolean type cannot be reified.
    *
    * @param tpe the type that cannot be reified.
    * @param loc the location of the type.
    */
  case class RReifyBoolException(tpe: Type, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised to indicate that a regular type cannot be reified.
    *
    * @param tpe the type that cannot be reified.
    * @param loc the location of the type.
    */
  case class RReifyTypeException(tpe: Type, loc: SourceLocation) extends RuntimeException

  /**
    * An exception raised to indicate that the Monomorpher encountered an unexpected non-constant Boolean.
    *
    * @param tpe the non-constant Boolean type.
    * @param loc the location of the type.
    */
  // TODO: Possibly this one should be removed.
  case class UUnexpectedNonConstBool(tpe: Type, loc: SourceLocation) extends RuntimeException


  /**
    * A function-local queue of pending (fresh symbol, function definition, and substitution)-triples.
    *
    * For example, if the queue contains the entry:
    *
    * -   (f$1, f, [a -> Int])
    *
    * it means that the function definition f should be specialized w.r.t. the map [a -> Int] under the fresh name f$1.
    */
  private type DefQueue = mutable.Queue[(Symbol.DefnSym, Def, StrictSubstitution)]

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
    * Performs monomorphization of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Monomorph") {

    implicit val r: Root = root

    val defQueue: DefQueue = mutable.Queue.empty

    val def2def: Def2Def = mutable.Map.empty

    /*
     * Collect all non-parametric function definitions.
     */
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.spec.tparams.isEmpty
    }

    /*
     * Perform specialization of all non-parametric function definitions.
     */
    val specializedDefnsValidation = traverse(nonParametricDefns) { case (sym, defn) =>
      // Get a substitution from the inferred scheme to the declared scheme.
      // This is necessary because the inferred scheme may be more generic than the declared scheme.
      val subst = Unification.unifyTypes(defn.spec.declaredScheme.base, defn.impl.inferredScheme.base) match {
        case Result.Ok(subst1) => subst1
        // This should not happen, since the Typer guarantees that the schemes unify
        case Result.Err(_) => throw InternalCompilerException("Failed to unify declared and inferred schemes.")
      }
      val subst0 = StrictSubstitution(subst)

      // Specialize the formal parameters to obtain fresh local variable symbols for them.
      flatMapN(specializeFormalParams(defn.spec.fparams, subst0)) {
        case (fparams, env0) =>
          // Specialize the body expression.
          mapN(specialize(defn.impl.exp, env0, subst0, def2def, defQueue)) {
            body => (sym, defn.copy(spec = defn.spec.copy(fparams = fparams), impl = defn.impl.copy(exp = body)))
          }
      }
    }

    /*
    * A map used to collect specialized definitions, etc.
    */
    val specializedDefns: mutable.Map[Symbol.DefnSym, TypedAst.Def] = specializedDefnsValidation match {
      case Success(t) => mutable.Map.from(t)
      case Failure(errors) => return Failure(errors)
    }


    /*
     * Performs function specialization until both queues are empty.
     */
    while (defQueue.nonEmpty) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val (freshSym, defn, subst) = defQueue.dequeue()

      flix.subtask(freshSym.toString, sample = true)

      // Specialize the formal parameters and introduce fresh local variable symbols.
      val (fparams, specializedExp, newBase) = flatMapN(specializeFormalParams(defn.spec.fparams, subst)) {
        case (fparams, env0) =>
          mapN(
            // Specialize the body expression.
            specialize(defn.impl.exp, env0, subst, def2def, defQueue),
            subst(defn.impl.inferredScheme.base)
          ) {
            case (specializedExp, newBase) => (fparams, specializedExp, newBase)
          }
      } match {
        case Success(t) => t
        case Failure(errors) => return Failure(errors)
      }


      // Reassemble the definition.
      // NB: Removes the type parameters as the function is now monomorphic.
      val specializedDefn = defn.copy(
        sym = freshSym,
        spec = defn.spec.copy(fparams = fparams, tparams = Nil),
        impl = TypedAst.Impl(specializedExp, Scheme(Nil, List.empty, newBase))
      )

      // Save the specialized function.
      specializedDefns.put(freshSym, specializedDefn)
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
  private def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: StrictSubstitution, def2def: Def2Def, defQueue: DefQueue)(implicit root: Root, flix: Flix): Validation[Expression, CompilationMessage] = {

    // TODO: Monomorph: Must apply subst to all effects.

    /**
      * Specializes the given expression `e0` under the environment `env0`. w.r.t. the current substitution.
      */
    def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Validation[Expression, CompilationMessage] = e0 match {
      case Expression.Wild(tpe, loc) =>
        mapN(subst0(tpe)) {
          t => Expression.Wild(t, loc)
        }

      case Expression.Var(sym, tpe, loc) =>
        mapN(subst0(tpe)) {
          t => Expression.Var(env0(sym), t, loc)
        }

      case Expression.Def(sym, tpe, loc) =>
        /*
         * !! This is where all the magic happens !!
         */
        flatMapN(subst0(tpe)) {
          t =>
            mapN(specializeDefSym(sym, t, def2def, defQueue), subst0(tpe)) {
              case (newSym, t) =>
                Expression.Def(newSym, t, loc)
            }
        }

      case Expression.Sig(sym, tpe, loc) =>
        flatMapN(subst0(tpe)) {
          t =>
            val newSym = specializeSigSym(sym, t, def2def, defQueue)
            mapN(subst0(tpe)) {
              t => Expression.Def(newSym, t, loc)
            }
        }

      case Expression.Hole(sym, tpe, loc) =>
        mapN(subst0(tpe)) {
          t => Expression.Hole(sym, t, loc)
        }

      case Expression.Unit(loc) => Expression.Unit(loc).toSuccess

      case Expression.Null(tpe, loc) =>
        mapN(subst0(tpe)) {
          t => Expression.Null(t, loc)
        }

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
        //
        // Replace a default literal by the actual default value based on its type.
        //
        flatMapN(subst0(tpe).map(_.typeConstructor)) {
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
            case _ => mapN(subst0(tpe)) { t => Expression.Null(t, loc) }
          }
        }

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        flatMapN(specializeFormalParam(fparam, subst0)) {
          case (p, env1) =>
            mapN(visitExp(exp, env0 ++ env1), subst0(tpe)) {
              case (e, t) =>
                Expression.Lambda(p, e, t, loc)
            }
        }

      case Expression.Apply(exp, exps, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), traverse(exps)(visitExp(_, env0)), subst0(tpe)) {
          case (e, es, t) =>
            Expression.Apply(e, es, t, eff, loc)
        }

      case Expression.Unary(sop, exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Unary(sop, e, t, eff, loc)
        }

      /*
       * Other Binary Expression.
       */
      case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        mapN(visitExp(exp1, env0), visitExp(exp2, env0), subst0(tpe)) {
          case (e1, e2, t) =>
            Expression.Binary(sop, e1, e2, t, eff, loc)
        }

      case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        // Generate a fresh symbol for the let-bound variable.
        val freshSym = Symbol.freshVarSym(sym)
        val env1 = env0 + (sym -> freshSym)
        mapN(visitExp(exp1, env1), visitExp(exp2, env1), subst0(tpe)) {
          case (e1, e2, t) =>
            Expression.Let(freshSym, mod, e1, e2, t, eff, loc)
        }

      case Expression.LetRegion(sym, exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.LetRegion(sym, e, t, eff, loc)
        }

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        mapN(visitExp(exp1, env0), visitExp(exp2, env0), visitExp(exp3, env0), subst0(tpe)) {
          case (e1, e2, e3, t) =>
            Expression.IfThenElse(e1, e2, e3, t, eff, loc)
        }

      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        mapN(visitExp(exp1, env0), visitExp(exp2, env0), subst0(tpe)) {
          case (e1, e2, t) =>
            Expression.Stm(e1, e2, t, eff, loc)
        }

      case Expression.Match(exp, rules, tpe, eff, loc) =>
        mapN(
          traverse(rules) {
            case MatchRule(pat, guard, body) =>
              flatMapN(visitPat(pat)) {
                case (p, env1) =>
                  val extendedEnv = env0 ++ env1
                  mapN(visitExp(guard, extendedEnv), visitExp(body, extendedEnv)) {
                    case (g, b) => MatchRule(p, g, b)
                  }
              }
          },
          visitExp(exp, env0),
          subst0(tpe)
        ) {
          case (rs, e, t) =>
            Expression.Match(e, rs, t, eff, loc)
        }

      case Expression.Choose(exps, rules, tpe, eff, loc) =>
        mapN(
          traverse(exps)(visitExp(_, env0)),
          traverse(rules) {
            case ChoiceRule(pat, exp) =>
              flatMapN(traverse(pat) {
                case ChoicePattern.Wild(loc) => (ChoicePattern.Wild(loc), Map.empty).toSuccess
                case ChoicePattern.Absent(loc) => (ChoicePattern.Absent(loc), Map.empty).toSuccess
                case ChoicePattern.Present(sym, tpe1, loc) =>
                  val freshVar = Symbol.freshVarSym(sym)
                  mapN(subst0(tpe1)) { t => (ChoicePattern.Present(freshVar, t, loc), Map(sym -> freshVar)) }
              }) {
                patAndEnv =>
                  val p = patAndEnv.map(_._1)
                  val env1 = patAndEnv.map(_._2).foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
                    case (acc, m) => acc ++ m
                  }
                  mapN(visitExp(exp, env0 ++ env1)) { e => ChoiceRule(p, e) }
              }
          }
        ) {
          case (es, rs) =>
            Expression.Choose(es, rs, tpe, eff, loc)
        }

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Tag(sym, tag, e, t, eff, loc)
        }

      case Expression.Tuple(elms, tpe, eff, loc) =>
        mapN(traverse(elms)(visitExp(_, env0)), subst0(tpe)) {
          case (es, t) =>
            Expression.Tuple(es, t, eff, loc)
        }

      case Expression.RecordEmpty(tpe, loc) =>
        mapN(subst0(tpe)) {
          t => Expression.RecordEmpty(t, loc)
        }

      case Expression.RecordSelect(base, field, tpe, eff, loc) =>
        mapN(visitExp(base, env0), subst0(tpe)) {
          case (b, t) =>
            Expression.RecordSelect(b, field, t, eff, loc)
        }

      case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
        mapN(visitExp(value, env0), visitExp(rest, env0), subst0(tpe)) {
          case (v, r, t) =>
            Expression.RecordExtend(field, v, r, t, eff, loc)
        }

      case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
        mapN(visitExp(rest, env0), subst0(tpe)) {
          case (r, t) =>
            Expression.RecordRestrict(field, r, t, eff, loc)
        }

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        mapN(traverse(elms)(visitExp(_, env0)), subst0(tpe)) {
          case (es, t) =>
            Expression.ArrayLit(es, t, eff, loc)
        }

      case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        mapN(visitExp(elm, env0), visitExp(len, env0), subst0(tpe)) {
          case (e, ln, t) =>
            Expression.ArrayNew(e, ln, t, eff, loc)
        }

      case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        mapN(visitExp(base, env0), visitExp(index, env0), subst0(tpe)) {
          case (b, i, t) =>
            Expression.ArrayLoad(b, i, t, eff, loc)
        }

      case Expression.ArrayStore(base, index, elm, loc) =>
        mapN(visitExp(base, env0), visitExp(index, env0), visitExp(elm, env0)) {
          case (b, i, e) =>
            Expression.ArrayStore(b, i, e, loc)
        }

      case Expression.ArrayLength(base, eff, loc) =>
        mapN(visitExp(base, env0)) {
          b => Expression.ArrayLength(b, eff, loc)
        }

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        mapN(visitExp(base, env0), visitExp(startIndex, env0), visitExp(endIndex, env0), subst0(tpe)) {
          case (b, i1, i2, t) =>
            Expression.ArraySlice(b, i1, i2, t, loc)
        }

      case Expression.Ref(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Ref(e, t, eff, loc)
        }

      case Expression.Deref(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Deref(e, t, eff, loc)
        }

      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        mapN(visitExp(exp1, env0), visitExp(exp2, env0), subst0(tpe)) {
          case (e1, e2, t) =>
            Expression.Assign(e1, e2, t, eff, loc)
        }

      case Expression.Existential(fparam, exp, loc) =>
        flatMapN(specializeFormalParam(fparam, subst0)) {
          case (param, env1) =>
            mapN(visitExp(exp, env0 ++ env1)) {
              e => Expression.Existential(param, e, loc)
            }
        }

      case Expression.Universal(fparam, exp, loc) =>
        flatMapN(specializeFormalParam(fparam, subst0)) {
          case (param, env1) =>
            mapN(visitExp(exp, env0 ++ env1)) {
              e => Expression.Universal(param, e, loc)
            }
        }

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Ascribe(e, t, eff, loc)
        }

      case Expression.Cast(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Cast(e, t, eff, loc)
        }

      case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        mapN(
          visitExp(exp, env0),
          traverse(rules) {
            case CatchRule(sym, clazz, body) =>
              // Generate a fresh symbol.
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              mapN(visitExp(body, env1)) {
                CatchRule(freshSym, clazz, _)
              }
          },
          subst0(tpe)
        ) {
          case (e, rs, t) =>
            Expression.TryCatch(e, rs, t, eff, loc)
        }

      case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        mapN(traverse(args)(visitExp(_, env0)), subst0(tpe)) {
          case (as, t) =>
            Expression.InvokeConstructor(constructor, as, t, eff, loc)
        }

      case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), traverse(args)(visitExp(_, env0))) {
          case (e, as) =>
            Expression.InvokeMethod(method, e, as, tpe, eff, loc)
        }

      case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        mapN(traverse(args)(visitExp(_, env0))) {
          as => Expression.InvokeStaticMethod(method, as, tpe, eff, loc)
        }

      case Expression.GetField(field, exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0)) {
          e => Expression.GetField(field, e, tpe, eff, loc)
        }

      case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        mapN(visitExp(exp1, env0), visitExp(exp2, env0)) {
          case (e1, e2) =>
            Expression.PutField(field, e1, e2, tpe, eff, loc)
        }

      case Expression.GetStaticField(field, tpe, eff, loc) =>
        Expression.GetStaticField(field, tpe, eff, loc).toSuccess

      case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0)) {
          e => Expression.PutStaticField(field, e, tpe, eff, loc)
        }

      case Expression.NewChannel(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.NewChannel(e, t, eff, loc)
        }

      case Expression.GetChannel(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.GetChannel(e, t, eff, loc)
        }

      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        mapN(visitExp(exp1, env0), visitExp(exp2, env0), subst0(tpe)) {
          case (e1, e2, t) =>
            Expression.PutChannel(e1, e2, t, eff, loc)
        }

      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        mapN(
          traverse(rules) {
            case SelectChannelRule(sym, chan, exp) =>
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              mapN(visitExp(chan, env1), visitExp(exp, env1)) {
                case (c, e) => SelectChannelRule(freshSym, c, e)
              }
          },
          default.map(visitExp(_, env0)) match {
            case Some(value) => value.map(Some(_))
            case None => None.toSuccess
          },
          subst0(tpe)
        ) {
          case (rs, d, t) =>
            Expression.SelectChannel(rs, d, t, eff, loc)
        }

      case Expression.Spawn(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Spawn(e, t, eff, loc)
        }

      case Expression.Lazy(exp, tpe, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Lazy(e, t, loc)
        }

      case Expression.Force(exp, tpe, eff, loc) =>
        mapN(visitExp(exp, env0), subst0(tpe)) {
          case (e, t) =>
            Expression.Force(e, t, eff, loc)
        }

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
        flatMapN(subst0(t)) {
          case Type.Cst(TypeConstructor.True, _) => Expression.True(loc).toSuccess
          case Type.Cst(TypeConstructor.False, _) => Expression.False(loc).toSuccess
          case other => ReificationError.IllegalReifiedBool(other, loc).toFailure
        }

      case Expression.ReifyType(t0, k, _, _, loc) =>
        k match {
          case Kind.Bool => flatMapN(subst0(t0)) { t => reifyBool(t, loc) }
          case Kind.Star => flatMapN(subst0(t0)) { t => reifyType(t, loc) }
          case _ => throw InternalCompilerException(s"Unexpected kind: $k.")
        }

      case Expression.ReifyEff(sym, exp1, exp2, exp3, _, _, loc) =>
        // Magic!
        flatMapN(subst0(exp1.tpe).map(_.arrowEffectType)) {
          case Type.Cst(TypeConstructor.True, _) =>
            // Generate a fresh symbol for the let-bound variable.
            val freshSym = Symbol.freshVarSym(sym)
            val env1 = env0 + (sym -> freshSym)

            mapN(visitExp(exp1, env0), visitExp(exp2, env1)) {
              case (e1, e2) =>
                Expression.Let(freshSym, Modifiers.Empty, e1, e2, e2.tpe, e2.eff, loc)
            }
          case _ =>
            visitExp(exp3, env0)
        }

    }

    /**
      * Specializes the given pattern `p0` w.r.t. the current substitution.
      *
      * Returns the new pattern and a mapping from variable symbols to fresh variable symbols.
      */
    def visitPat(p0: Pattern): Validation[(Pattern, Map[Symbol.VarSym, Symbol.VarSym]), CompilationMessage] = p0 match {
      case Pattern.Wild(tpe, loc) =>
        mapN(subst0(tpe)) {
          t => (Pattern.Wild(t, loc), Map.empty)
        }

      case Pattern.Var(sym, tpe, loc) =>
        // Generate a fresh variable symbol for the pattern-bound variable.
        val freshSym = Symbol.freshVarSym(sym)
        mapN(subst0(tpe)) {
          t => (Pattern.Var(freshSym, t, loc), Map(sym -> freshSym))
        }

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
        mapN(visitPat(pat), subst0(tpe)) {
          case ((p, env1), t) => (Pattern.Tag(sym, tag, p, t, loc), env1)
        }

      case Pattern.Tuple(elms, tpe, loc) =>
        mapN(traverse(elms)(visitPat).map(_.unzip), subst0(tpe)) {
          case ((ps, envs), t) => (Pattern.Tuple(ps, t, loc), envs.reduce(_ ++ _))
        }

      case Pattern.Array(elms, tpe, loc) =>
        mapN(traverse(elms)(visitPat).map(_.unzip), subst0(tpe)) {
          case ((ps, envs), t) =>
            (Pattern.Array(ps, t, loc), if (envs.isEmpty) Map.empty else envs.reduce(_ ++ _))
        }


      case Pattern.ArrayTailSpread(elms, sym, tpe, loc) =>
        val freshSym = Symbol.freshVarSym(sym)
        mapN(traverse(elms)(visitPat).map(_.unzip), subst0(tpe)) {
          case ((ps, envs), t) =>
            (Pattern.ArrayTailSpread(ps, freshSym, t, loc),
              if (envs.isEmpty) Map(sym -> freshSym)
              else envs.reduce(_ ++ _) ++ Map(sym -> freshSym))
        }

      case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) =>
        val freshSym = Symbol.freshVarSym(sym)
        mapN(traverse(elms)(visitPat).map(_.unzip), subst0(tpe)) {
          case ((ps, envs), t) => (Pattern.ArrayHeadSpread(freshSym, ps, t, loc),
            if (envs.isEmpty) Map(sym -> freshSym)
            else envs.reduce(_ ++ _) ++ Map(sym -> freshSym))
        }
    }

    visitExp(exp0, env0)
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  private def specializeDefSym(sym: Symbol.DefnSym, tpe: Type, def2def: Def2Def, defQueue: DefQueue)(implicit root: Root, flix: Flix): Validation[Symbol.DefnSym, CompilationMessage] = {
    // Lookup the definition and its declared type.
    val defn = root.defs(sym)

    // Compute the erased type.
    mapN(eraseType(tpe)) {
      erasedType =>
        // Check if the function is non-polymorphic.
        if (defn.spec.tparams.isEmpty) {
          defn.sym
        } else {
          specializeDef(defn, erasedType, def2def, defQueue)
        }
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
            defn.sym.name == sig.sym.name && Unification.unifiesWith(defn.spec.declaredScheme.base, tpe)
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
  private def specializeFormalParams(fparams0: List[FormalParam], subst0: StrictSubstitution)(implicit flix: Flix): Validation[(List[FormalParam], Map[Symbol.VarSym, Symbol.VarSym]), CompilationMessage] = {
    // Return early if there are no formal parameters.
    if (fparams0.isEmpty)
      return (Nil: List[FormalParam], Map.empty[Symbol.VarSym, Symbol.VarSym]).toSuccess

    // Specialize each formal parameter and recombine the results.
    mapN(traverse(fparams0)(p => specializeFormalParam(p, subst0)).map(_.unzip)) {
      case (params, envs) => (params, envs.reduce(_ ++ _))
    }
  }

  /**
    * Specializes the given formal parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  private def specializeFormalParam(fparam0: FormalParam, subst0: StrictSubstitution)(implicit flix: Flix): Validation[(FormalParam, Map[Symbol.VarSym, Symbol.VarSym]), CompilationMessage] = {
    val FormalParam(sym, mod, tpe, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    mapN(subst0(tpe)) {
      t => (FormalParam(freshSym, mod, t, loc), Map(sym -> freshSym))
    }
  }
  //
  //  /**
  //    * Specializes the given constraint parameters `cparams0` w.r.t. the given substitution `subst0`.
  //    *
  //    * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
  //    */
  //  private def specializeConstraintParams(cparams0: List[ConstraintParam], subst0: StrictSubstitution)(implicit flix: Flix): (List[ConstraintParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
  //    // Return early if there are no formal parameters.
  //    if (cparams0.isEmpty)
  //      return (Nil, Map.empty)
  //
  //    // Specialize each constraint parameter and recombine the results.
  //    val (params, envs) = cparams0.map(p => specializeConstraintParam(p, subst0)).unzip
  //    (params, envs.reduce(_ ++ _))
  //  }
  //
  //  /**
  //    * Specializes the given constraint parameter `fparam0` w.r.t. the given substitution `subst0`.
  //    *
  //    * Returns the new constraint parameter and an environment mapping the variable symbol to a fresh variable symbol.
  //    */
  //  private def specializeConstraintParam(cparam0: ConstraintParam, subst0: StrictSubstitution)(implicit flix: Flix): Validation[(ConstraintParam, Map[Symbol.VarSym, Symbol.VarSym]), CompilationMessage] = cparam0 match {
  //    case ConstraintParam.HeadParam(sym, tpe, loc) =>
  //      val freshSym = Symbol.freshVarSym(sym)
  //      mapN(subst0(tpe)){
  //        t => (ConstraintParam.HeadParam(freshSym, t, loc), Map(sym -> freshSym))
  //      }
  //    case ConstraintParam.RuleParam(sym, tpe, loc) =>
  //      val freshSym = Symbol.freshVarSym(sym)
  //      mapN(subst0(tpe)){
  //        t => (ConstraintParam.RuleParam(freshSym, t, loc), Map(sym -> freshSym))
  //      }
  //  }

  /**
    * Returns an expression that evaluates to a ReifiedBool for the given type `tpe`.
    */
  private def reifyBool(tpe: Type, loc: SourceLocation): Validation[Expression, CompilationMessage] = {
    val sym = Symbol.mkEnumSym("ReifiedBool")
    val resultTpe = Type.mkEnum(sym, Kind.Star, loc)
    val resultEff = Type.Pure

    tpe.typeConstructor match {
      case None =>
        ReificationError.IllegalReifiedType(tpe, loc).toFailure

      case Some(tc) => tc match {
        case TypeConstructor.True =>
          val tag = Name.Tag("ReifiedTrue", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case TypeConstructor.False =>
          val tag = Name.Tag("ReifiedFalse", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

        case _ =>
          val tag = Name.Tag("ErasedBool", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess
      }
    }
  }

  /**
    * Returns an expression that evaluates to a ReifiedType for the given type `tpe`.
    */
  private def reifyType(tpe: Type, loc: SourceLocation): Validation[Expression, CompilationMessage] = {
    val sym = Symbol.mkEnumSym("ReifiedType")
    val resultTpe = Type.mkEnum(sym, Kind.Star, loc)
    val resultEff = Type.Pure

    def visit(t0: Type): Validation[Expression, CompilationMessage] = t0.typeConstructor match {
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

        case TypeConstructor.Array =>
          val tag = Name.Tag("ReifiedArray", loc)
          val innerTpe = t0.typeArguments.head
          mapN(visit(innerTpe)) {
            innerExp => Expression.Tag(sym, tag, innerExp, resultTpe, resultEff, loc)
          }


        case _ =>
          val tag = Name.Tag("ErasedType", loc)
          Expression.Tag(sym, tag, Expression.Unit(loc), resultTpe, resultEff, loc).toSuccess

      }
    }

    visit(tpe)
  }

  /**
    * Performs type erasure on the given type `tpe`.
    *
    * Flix does not erase normal types, but it does erase Boolean formulas.
    */
  private def eraseType(tpe: Type)(implicit flix: Flix): Validation[Type, CompilationMessage] = tpe match {
    case Type.KindedVar(_, _, loc, _, _) =>
      if (flix.options.xstrictmono) {
        ReificationError.UnexpectedNonConstBool(tpe, loc).toFailure
      } else {
        // TODO: We should return Type.ErasedBool or something.
        Type.True.toSuccess
      }

    case Type.Cst(_, _) => tpe.toSuccess

    case Type.Apply(tpe1, tpe2, loc) =>
      mapN(eraseType(tpe1), eraseType(tpe2)) {
        case (t1, t2) => Type.Apply(t1, t2, loc)
      }

    case Type.Alias(sym, args, tpe, loc) =>
      mapN(traverse(args)(eraseType), eraseType(tpe)) {
        case (as, t) => Type.Alias(sym, as, t, loc)
      }

    case Type.UnkindedVar(_, loc, _, _) => throw InternalCompilerException(s"Unexpected type at: ${loc.format}")

    case Type.Ascribe(_, _, loc) => throw InternalCompilerException(s"Unexpected type at: ${loc.format}")
  }

}
