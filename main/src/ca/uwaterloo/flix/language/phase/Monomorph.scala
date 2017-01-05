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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.TypedAst.Expression.UserError
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Declaration, Expression, Pattern, Root}

import scala.collection.mutable

/**
  * Monomorphization is a whole-program compilation strategy that replaces every reference to a parametric function with
  * a call to a non-parametric version (of that function) specialized to the types of the arguments at the call.
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
  * 1. We maintain a queue of functions and the type it must be specialized to.
  * 2. We populate the queue by specialization of non-parametric function definitions and other top-level expressions.
  * 3. We iteratively extract a function from the queue and specialize it.
  *    a. We replace every type variable appearing anywhere in the definition by its concrete type.
  *    b. We create new local variable symbols (since the function is being copied).
  *    c. We enqueue (or re-used) other functions called by the function which require specialization.
  * 4. We reconstruct the AST from the specialized functions and remove all parametric functions.
  */
object Monomorph {

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are automatically
    * replaced by the Unit type. In other words, when performing a type substitution if there is no requirement
    * on a polymorphic type we assume it to be Unit. This is safe since otherwise the type would not be polymorphic.
    */
  case class StrictSubstitution(s: Unification.Substitution) {
    /**
      * Returns `true` if this substitution is empty.
      */
    def isEmpty: Boolean = s.isEmpty

    /**
      * Applies `this` substitution to the given type `tpe`.
      */
    def apply(tpe: Type): Type = s(tpe) match {
      case Type.Var(_, _) => Type.Unit
      case result => result
    }
  }

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
    val queue: mutable.Queue[(Symbol.DefnSym, Declaration.Definition, StrictSubstitution)] = mutable.Queue.empty

    /**
      * A function-local map from a symbol and a type to the fresh symbol for the specialized version of that function.
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
      * Performs specialization of the given expression `exp0` under the environment `env0` w.r.t. the given substitution `subst0`.
      *
      * Replaces every call to a parametric function with a call to its specialized version.
      *
      * Replaces every local variable symbol with a fresh local variable symbol.
      *
      * If a specialized version of a function does not yet exists, a fresh symbol is created for it, and the
      * definition and substitution is enqueued.
      */
    def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: StrictSubstitution): Expression = {

      /**
        * Specializes the given expression `e0` under the environment `env0`. w.r.t. the current substitution.
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
          /*
           * !! This is where all the magic happens !!
           */

          // Apply the current substitution to the type of the reference.
          // NB: This is to concretize any type variables already captured by the substitution.
          val actualType = subst0(tpe)

          // Lookup the definition and its declared type.
          val defn = root.definitions(sym)
          val declaredType = defn.tpe

          // Unify the declared and actual type to obtain the substitution map.
          val subst = StrictSubstitution(Unification.unify(declaredType, actualType).get)

          // Check if the substitution is empty, if so there is no need for specialization.
          if (subst.isEmpty) {
            return Expression.Ref(sym, subst0(tpe), loc)
          }

          // Check whether the function definition has already been specialized.
          symbol2symbol.get((sym, actualType)) match {
            case None =>
              // Case 1: The function has not been specialized.
              // Generate a fresh definition symbol.
              val freshSym = Symbol.freshDefnSym(sym)

              // Enqueue the fresh symbol with the definition and substitution.
              queue.enqueue((freshSym, defn, subst))

              // Now simply refer to the freshly generated symbol.
              Expression.Ref(freshSym, subst0(actualType), loc)
            case Some(specializedSym) =>
              // Case 2: The function has already been specialized.
              // Simply refer to the already existing specialized symbol.
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
          // Generate a fresh symbol for the let-bound variable.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          Expression.Let(freshSym, visitExp(exp1, env1), visitExp(exp2, env1), subst0(tpe), loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          val e3 = visitExp(exp3, env0)
          Expression.IfThenElse(e1, e2, e3, subst0(tpe), loc)

        case Expression.Match(matchExp, rules, tpe, loc) =>
          val rs = rules map {
            case (pat, exp) =>
              val (p, e) = visitPat(pat)
              (p, visitExp(exp, env0 ++ e))
          }
          Expression.Match(visitExp(matchExp, env0), rs, subst0(tpe), loc)

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

        case Expression.Existential(fparams, exp, loc) =>
          val (fs, env1) = specializeFormalParams(fparams, subst0)
          Expression.Existential(fs, visitExp(exp, env0 ++ env1), loc)

        case Expression.Universal(fparams, exp, loc) =>
          val (fs, env1) = specializeFormalParams(fparams, subst0)
          Expression.Universal(fs, visitExp(exp, env0 ++ env1), loc)

        case Expression.UserError(tpe, loc) => UserError(subst0(tpe), loc)
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
    def specializeFormalParams(fparams0: List[TypedAst.FormalParam], subst0: StrictSubstitution): (List[TypedAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
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
    def specializeFormalParam(fparam0: TypedAst.FormalParam, subst0: StrictSubstitution): (TypedAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
      val TypedAst.FormalParam(sym, tpe, loc) = fparam0
      val freshSym = Symbol.freshVarSym(sym)
      (TypedAst.FormalParam(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
    }

    /*
     * We can now use these helper functions to perform specialization of the whole program.
     */
    val t = System.nanoTime()

    /*
     * A map used to collect specialized definitions, etc.
     */
    val specializedDefns: mutable.Map[Symbol.DefnSym, Declaration.Definition] = mutable.Map.empty
    // TODO: Specialize expressions occurring in other places, e.g facts/rules/properties.

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
      // Specialize the function definition under the empty substitution (it has no type parameters).
      val subst0 = StrictSubstitution(Unification.Substitution.empty)

      // Specialize the formal parameters to obtain fresh local variable symbols for them.
      val (fparams, env0) = specializeFormalParams(defn.formals, subst0)

      // Specialize the body expression.
      val body = specialize(defn.exp, env0, subst0)

      // Reassemble the definition.
      specializedDefns.put(sym, defn.copy(formals = fparams, exp = body))
    }

    /*
     * Performs function specialization until the queue is empty.
     */
    while (queue.nonEmpty) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val (freshSym, defn, subst) = queue.dequeue()

      // Specialize the formal parameters and introduce fresh local variable symbols.
      val (fparams, env0) = specializeFormalParams(defn.formals, subst)

      // Specialize the body expression.
      val specializedExp = specialize(defn.exp, env0, subst)

      // Reassemble the definition.
      // NB: Removes the type parameters as the function is now monomorphic.
      val specializedDefn = defn.copy(sym = freshSym, formals = fparams, exp = specializedExp, tpe = subst(defn.tpe), tparams = Nil)

      // Save the specialized function.
      specializedDefns.put(freshSym, specializedDefn)
    }

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = specializedDefns.toMap,
      time = root.time.copy(monomorph = e)
    )
  }

}
