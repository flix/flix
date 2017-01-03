/*
 * Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.TypedAst.Expression.UserError
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Declaration, Expression, Pattern, Root}

import scala.collection.mutable

/**
  * TODO: Describe implementation:
  *
  * Discuss how there is a work list and fixed point computation.
  *
  * - Find all refs
  * - Unify infered and declared type
  * - specialize defn.
  */
object Monomorph {

  /**
    * Performs monomorphization of the given AST `root`.
    */
  def monomorph(root: Root)(implicit genSym: GenSym): TypedAst.Root = {

    /**
      * A function-local queue of pending (fresh symbol, function definition, and substitution)-triples.
      *
      * For example, if the queue contains the entry:
      *
      * -   (f$1, f, [a -> Int])
      *
      * it means that the function definition f should be specialized w.r.t. the map [a -> Int] under the fresh name f$1.
      */
    val queue: mutable.Queue[(Symbol.DefnSym, Declaration.Definition, Unification.Substitution)] = mutable.Queue.empty

    /**
      * A function-local map from symbols and a specialized type to the symbol for the corresponding specialized version.
      *
      * For example, if the function:
      *
      * -   def fst[a, b](x: a, y: b): a = ...
      *
      * has been specialized w.r.t. to `Int` and `Str` then this map will contain an entry:
      *
      * -   (fst, (Int, Str) -> Int) -> fst$1
      */
    val symbol2symbol: mutable.Map[(Symbol.DefnSym, Type), Symbol.DefnSym] = mutable.Map.empty

    /**
      * Performs specialization of the given expression `exp0` w.r.t. the given substitution `subst0`.
      *
      * Replaces every call to a parametric function with a call to its specialized version.
      *
      * If the specialized version does not yet exist, a fresh symbol is created, and the definition is enqueue.
      */
    def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: Unification.Substitution): Expression = {

      /**
        * Specializes the given expression `e0` w.r.t. the current substitution.
        */
      def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = e0 match {
        case Expression.Unit(loc) => Expression.Unit(loc)
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
        case Expression.Wild(tpe, loc) => Expression.Wild(subst0(tpe), loc)
        case Expression.Var(sym, tpe, loc) => Expression.Var(env0(sym), subst0(tpe), loc)

        case Expression.Ref(sym, tpe, loc) =>
          // Specialize the actual inferred type according to the substitution.
          val actualType = subst0(tpe)

          // Lookup the definition of the symbol.
          val defn = root.definitions(sym)
          val declaredType = defn.tpe

          // Unify the type of the definition with the actual type of the reference.
          val subst = Unification.unify(declaredType, actualType).get

          // Check if the substitution is empty in which case there is no need for specialization.
          if (subst.isEmpty) {
            return Expression.Ref(sym, subst0(tpe), loc)
          }

          // Check if we have already specialized this function.
          symbol2symbol.get((sym, actualType)) match {
            case None =>
              // Case 1: The function has not yet been specialized.
              // Generate a fresh symbol and enqueue the function with its specialized type.
              val freshSym = Symbol.freshDefnSym(sym)

              queue.enqueue((freshSym, defn, subst))

              // Refer to the specialized function.
              Expression.Ref(freshSym, subst0(actualType), loc)
            case Some(specializedSym) =>
              // Case 2: The function has already been specialized. Use the new function.
              Expression.Ref(specializedSym, subst0(actualType), loc)
          }

        case Expression.Hook(hook, tpe, loc) => Expression.Hook(hook, subst0(tpe), loc)

        case Expression.Lambda(fparams, body, tpe, loc) =>
          val (fs, env1) = specializeFormalParams(fparams, subst0)
          Expression.Lambda(fs, visitExp(body, env0 ++ env1), subst0(tpe), loc)

        case Expression.Apply(exp, args, tpe, loc) =>
          val e = visitExp(exp, env0)
          val as = args.map(a => visitExp(a, env0))
          Expression.Apply(e, as, subst0(tpe), loc)

        case Expression.Unary(op, exp, tpe, loc) =>
          val e1 = visitExp(exp, env0)
          Expression.Unary(op, e1, subst0(tpe), loc)

        case Expression.Binary(op, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.Binary(op, e1, e2, subst0(tpe), loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Generate a fresh symbol.
          val freshSym = Symbol.freshVarSym(sym)
          val extendedEnv = env0 + (sym -> freshSym)
          Expression.Let(freshSym, visitExp(exp1, extendedEnv), visitExp(exp2, extendedEnv), subst0(tpe), loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          val e3 = visitExp(exp3, env0)
          Expression.IfThenElse(e1, e2, e3, subst0(tpe), loc)

        case Expression.Match(exp, rules, tpe, loc) =>
          val rs = rules map {
            case (pat, exp) =>
              val (p, e) = visitPat(pat)
              (p, visitExp(exp, env0 ++ e))
          }
          Expression.Match(visitExp(exp, env0), rs, subst0(tpe), loc)

        case Expression.Switch(rules, tpe, loc) =>
          val rs = rules map {
            case (e1, e2) => (visitExp(e1, env0), visitExp(e2, env0))
          }
          Expression.Switch(rs, subst0(tpe), loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = visitExp(exp, env0)
          Expression.Tag(sym, tag, e, subst0(tpe), loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms.map(e => visitExp(e, env0))
          Expression.Tuple(es, subst0(tpe), loc)

        case Expression.Existential(fparams, e, loc) =>
          val (fs, env1) = specializeFormalParams(fparams, subst0)
          Expression.Existential(fs, visitExp(e, env0 ++ env1), loc)

        case Expression.Universal(fparams, e, loc) =>
          val (fs, env1) = specializeFormalParams(fparams, subst0)
          Expression.Universal(fs, visitExp(e, env0 ++ env1), loc)

        case Expression.UserError(tpe, loc) => UserError(subst0(tpe), loc)
      }

      /**
        * Specializes the given pattern `p0` w.r.t. the current substitution.
        */
      def visitPat(p0: Pattern): (Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = p0 match {
        case Pattern.Wild(tpe, loc) => (Pattern.Wild(subst0(tpe), loc), Map.empty)
        case Pattern.Var(sym, tpe, loc) =>
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
          val es = elms.map(p => visitPat(p))
          val ps = es.map(_._1)
          val env1 = es.map(_._2).reduce(_ ++ _)
          (Pattern.Tuple(ps, subst0(tpe), loc), env1)

        case Pattern.FSet(_, _, _, _) => ??? // TODO: Unsupported
        case Pattern.FMap(_, _, _, _) => ??? // TODO: Unsupported
      }

      visitExp(exp0, env0)
    }

    /**
      * Specializes the given formal parameters `fparams0` w.r.t. the given substitution `subst0`.
      *
      * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
      */
    def specializeFormalParams(fparams0: List[TypedAst.FormalParam], subst0: Unification.Substitution): (List[TypedAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
      // Return early if there is no formal parameters.
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
    def specializeFormalParam(fparam0: TypedAst.FormalParam, subst0: Unification.Substitution): (TypedAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
      val TypedAst.FormalParam(sym, tpe, loc) = fparam0
      val freshSym = Symbol.freshVarSym(sym)
      (TypedAst.FormalParam(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
    }

    /*
     * A map to collect all specialized function definitions.
     */
    val specialized: mutable.Map[Symbol.DefnSym, Declaration.Definition] = mutable.Map.empty

    /*
     * Collect all non-parametric function definitions.
     */
    val nonParametricDefns = root.definitions.filter {
      case (_, defn) => defn.tparams.isEmpty
    }

    /*
     * Perform specialization of all non-parametric function definitions.
     */
    for ((sym, defn) <- nonParametricDefns) {
      // Specialize the expression of the function definition.

      // The net effect is to replace calls to parametric functions with calls to their specialized versions.
      // If a specialized version does not yet exist, it is added to the queue.
      val subst0 = Unification.Substitution.empty
      val (fs, env) = specializeFormalParams(defn.formals, subst0)
      specialized.put(sym, defn.copy(formals = fs, exp = specialize(defn.exp, env, subst0)))

      // TODO: Specialize expressions appearing in other locations.
    }

    /*
     * Performs specialization until the queue is empty.
     */
    while (queue.nonEmpty) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val (freshSym, defn, subst) = queue.dequeue()
      val (fs, env) = specializeFormalParams(defn.formals, subst)
      val specializedExp = specialize(defn.exp, env, subst)
      val specializedDefn = defn.copy(sym = freshSym, formals = fs, exp = specializedExp, tpe = subst(defn.tpe), tparams = Nil)
      specialized.put(freshSym, specializedDefn)

      println(s"  Specialize ${defn.sym} w.r.t. ${subst.m}")
    }

    println()

    for ((sym, defn) <- specialized.toList.sortBy(_._1.loc)) {
      println(s"${sym} - ${defn.tpe}")
    }

    println()

    root.copy(
      definitions = specialized.toMap
    )
  }

}
