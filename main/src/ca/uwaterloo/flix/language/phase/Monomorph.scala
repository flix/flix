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
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, HandlerBinding, Pattern, Root}
import ca.uwaterloo.flix.language.ast.{BinaryOperator, Symbol, Type, TypedAst, UnaryOperator}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

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
  *    b. We create new fresh local variable symbols (since the function is effectively being copied).
  *    c. We enqueue (or re-used) other functions referenced by the current function which require specialization.
  * 4. We reconstruct the AST from the specialized functions and remove all parametric functions.
  */
object Monomorph extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are replaced by the
    * Unit type. In other words, when performing a type substitution if there is no requirement on a polymorphic type
    * we assume it to be Unit. This is safe since otherwise the type would not be polymorphic after type-inference.
    */
  case class StrictSubstitution(s: Unification.Substitution) {
    /**
      * Returns `true` if this substitution is empty.
      */
    def isEmpty: Boolean = s.isEmpty

    /**
      * Applies `this` substitution to the given type `tpe`.
      *
      * NB: Applies the substitution first, then replaces every type variable with the unit type.
      */
    def apply(tpe: Type): Type = {
      /**
        * Recursively replaces every type variable with the unit type.
        */
      def visit(t: Type): Type = t match {
        case Type.Var(_, _) => Type.Unit
        case Type.Unit => Type.Unit
        case Type.Bool => Type.Bool
        case Type.Char => Type.Char
        case Type.Float32 => Type.Float32
        case Type.Float64 => Type.Float64
        case Type.Int8 => Type.Int8
        case Type.Int16 => Type.Int16
        case Type.Int32 => Type.Int32
        case Type.Int64 => Type.Int64
        case Type.BigInt => Type.BigInt
        case Type.Str => Type.Str
        case Type.Array => Type.Array
        case Type.Vector => Type.Vector
        case Type.Native => Type.Native
        case Type.Ref => Type.Ref
        case Type.Arrow(l) => Type.Arrow(l)
        case Type.Tuple(l) => Type.Tuple(l)
        case Type.Zero => Type.Zero
        case Type.Succ(n, t) => Type.Succ(n, t)
        case Type.Enum(name, kind) => Type.Enum(name, kind)
        case Type.Apply(tpe1, tpe2) => Type.Apply(apply(tpe1), apply(tpe2))
      }

      visit(s(tpe))
    }
  }

  /**
    * Performs monomorphization of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    /**
      * A function-local queue of pending (fresh symbol, function definition, and substitution)-triples.
      *
      * For example, if the queue contains the entry:
      *
      * -   (f$1, f, [a -> Int])
      *
      * it means that the function definition f should be specialized w.r.t. the map [a -> Int] under the fresh name f$1.
      */
    val defQueue: mutable.Queue[(Symbol.DefnSym, TypedAst.Def, StrictSubstitution)] = mutable.Queue.empty
    val effQueue: mutable.Queue[(Symbol.EffSym, TypedAst.Handler, StrictSubstitution)] = mutable.Queue.empty

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
    val def2def: mutable.Map[(Symbol.DefnSym, Type), Symbol.DefnSym] = mutable.Map.empty
    val eff2eff: mutable.Map[(Symbol.EffSym, Type), Symbol.EffSym] = mutable.Map.empty

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
    def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: StrictSubstitution): Expression = {

      /**
        * Specializes the given expression `e0` under the environment `env0`. w.r.t. the current substitution.
        */
      def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = e0 match {
        case Expression.Wild(tpe, eff, loc) => Expression.Wild(subst0(tpe), eff, loc)
        case Expression.Var(sym, tpe, eff, loc) => Expression.Var(env0(sym), subst0(tpe), eff, loc)

        case Expression.Def(sym, tpe, eff, loc) =>
          /*
           * !! This is where all the magic happens !!
           */
          val newSym = specializeDefSym(sym, subst0(tpe))
          Expression.Def(newSym, subst0(tpe), eff, loc)

        case Expression.Eff(sym, tpe, eff, loc) =>
          /*
           * !! This is where all the magic happens !!
           */
          val newSym = specializeEffSym(sym, subst0(tpe))
          Expression.Eff(newSym, subst0(tpe), eff, loc)

        case Expression.Hole(sym, tpe, eff, loc) => Expression.Hole(sym, tpe, eff, loc)

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

        case Expression.Lambda(fparams, body, tpe, eff, loc) =>
          val (fs, env1) = specializeFormalParams(fparams, subst0)
          Expression.Lambda(fs, visitExp(body, env0 ++ env1), subst0(tpe), eff, loc)

        case Expression.Apply(exp, args, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          val as = args.map(a => visitExp(a, env0))
          Expression.Apply(e, as, subst0(tpe), eff, loc)

        case Expression.Unary(op, exp, tpe, eff, loc) =>
          val e1 = visitExp(exp, env0)
          Expression.Unary(op, e1, subst0(tpe), eff, loc)

        /*
         * Equality / Inequality Check.
         */
        case Expression.Binary(op@(BinaryOperator.Equal | BinaryOperator.NotEqual), exp1, exp2, tpe, eff, loc) =>
          // Perform specialization on the left and right sub-expressions.
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)

          // The type of the values of exp1 and exp2. NB: The typer guarantees that exp1 and exp2 have the same type.
          val valueType = subst0(exp1.tpe)

          // The expected type of an equality function: a -> a -> bool.
          val eqType = Type.mkArrow(List(valueType, valueType), Type.Bool)

          // Look for any function named `eq` with the expected type.
          // Returns `Some(sym)` if there is exactly one such function.
          lookup("eq", eqType) match {
            case None =>
              // No equality function found. Use a regular equality / inequality expression.
              if (op == BinaryOperator.Equal) {
                Expression.Binary(BinaryOperator.Equal, e1, e2, Type.Bool, eff, loc)
              } else {
                Expression.Binary(BinaryOperator.NotEqual, e1, e2, Type.Bool, eff, loc)
              }
            case Some(eqSym) =>
              // Equality function found. Specialize and generate a call to it.
              val newSym = specializeDefSym(eqSym, eqType)
              val ref = Expression.Def(newSym, eqType, eff, loc)
              // Check whether the whether the operator is equality or inequality.
              if (op == BinaryOperator.Equal) {
                // Call the equality function.
                Expression.Apply(ref, List(e1, e2), tpe, eff, loc)
              } else {
                // Call the equality function and negate the result.
                Expression.Unary(UnaryOperator.LogicalNot, Expression.Apply(ref, List(e1, e2), tpe, eff, loc), Type.Bool, eff, loc)
              }
          }

        /*
         * Other Binary Expression.
         */
        case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.Binary(op, e1, e2, subst0(tpe), eff, loc)

        case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
          // Generate a fresh symbol for the let-bound variable.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          Expression.Let(freshSym, visitExp(exp1, env1), visitExp(exp2, env1), subst0(tpe), eff, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
          // Generate a fresh symbol for the letrec-bound variable.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          Expression.LetRec(freshSym, visitExp(exp1, env1), visitExp(exp2, env1), subst0(tpe), eff, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          val e3 = visitExp(exp3, env0)
          Expression.IfThenElse(e1, e2, e3, subst0(tpe), eff, loc)

        case Expression.Match(exp, rules, tpe, eff, loc) =>
          val rs = rules map {
            case TypedAst.MatchRule(pat, guard, body) =>
              val (p, env1) = visitPat(pat)
              val extendedEnv = env0 ++ env1
              val g = visitExp(guard, extendedEnv)
              val b = visitExp(body, extendedEnv)
              TypedAst.MatchRule(p, g, b)
          }
          Expression.Match(visitExp(exp, env0), rs, subst0(tpe), eff, loc)

        case Expression.Switch(rules, tpe, eff, loc) =>
          val rs = rules map {
            case (e1, e2) => (visitExp(e1, env0), visitExp(e2, env0))
          }
          Expression.Switch(rs, subst0(tpe), eff, loc)

        case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Tag(sym, tag, e, subst0(tpe), eff, loc)

        case Expression.Tuple(elms, tpe, eff, loc) =>
          val es = elms.map(e => visitExp(e, env0))
          Expression.Tuple(es, subst0(tpe), eff, loc)

        case Expression.ArrayLit(elms, tpe, eff, loc) =>
          val es = elms.map(e => visitExp(e, env0))
          Expression.ArrayLit(es, subst0(tpe), eff, loc)

        case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
          val e = visitExp(elm, env0)
          val ln = visitExp(len, env0)
          Expression.ArrayNew(e, ln, tpe, eff, loc)

        case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          val i = visitExp(index, env0)
          Expression.ArrayLoad(b, i, tpe, eff, loc)

        case Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          val i = visitExp(index, env0)
          val e = visitExp(elm, env0)
          Expression.ArrayStore(b, i, e, tpe, eff, loc)

        case Expression.ArrayLength(base, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          Expression.ArrayLength(b, tpe, eff, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          val i1 = visitExp(startIndex, env0)
          val i2 = visitExp(endIndex, env0)
          Expression.ArraySlice(b, i1, i2, tpe, eff, loc)

        case Expression.VectorLit(elms, tpe, eff, loc) =>
          val es = elms.map(e => visitExp(e, env0))
          Expression.VectorLit(es, subst0(tpe), eff, loc)

        case Expression.VectorNew(elm, len, tpe, eff, loc) =>
          val e = visitExp(elm, env0)
          Expression.VectorNew(e, len, tpe, eff, loc)

        case Expression.VectorLoad(base, index, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          Expression.VectorLoad(b, index, tpe, eff, loc)

        case Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          val e = visitExp(elm, env0)
          Expression.VectorStore(b, index, e, tpe, eff, loc)

        case Expression.VectorLength(base, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          Expression.VectorLength(b, tpe, eff, loc)

        case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          val i2 = visitExp(endIndex, env0)
          Expression.VectorSlice(b, startIndex, i2, tpe, eff, loc)

        case Expression.Unique(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Unique(e, tpe, eff, loc)

        case Expression.Ref(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Ref(e, tpe, eff, loc)

        case Expression.Deref(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Deref(e, tpe, eff, loc)

        case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.Assign(e1, e2, tpe, eff, loc)

        case Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          val bs = bindings map {
            case HandlerBinding(sym, handler) =>
              val specializedSym = specializeEffSym(sym, handler.tpe)
              val e = visitExp(handler, env0)
              TypedAst.HandlerBinding(specializedSym, e)
          }
          Expression.HandleWith(e, bs, tpe, eff, loc)

        case Expression.Existential(fparam, exp, eff, loc) =>
          val (param, env1) = specializeFormalParam(fparam, subst0)
          Expression.Existential(param, visitExp(exp, env0 ++ env1), eff, loc)

        case Expression.Universal(fparam, exp, eff, loc) =>
          val (param, env1) = specializeFormalParam(fparam, subst0)
          Expression.Universal(param, visitExp(exp, env0 ++ env1), eff, loc)

        case Expression.Ascribe(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Ascribe(e, subst0(tpe), eff, loc)

        case Expression.Cast(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Cast(e, subst0(tpe), eff, loc)

        case Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
          val es = args.map(e => visitExp(e, env0))
          Expression.NativeConstructor(constructor, es, subst0(tpe), eff, loc)

        case Expression.NativeField(field, tpe, eff, loc) =>
          Expression.NativeField(field, subst0(tpe), eff, loc)

        case Expression.NativeMethod(method, args, tpe, eff, loc) =>
          val es = args.map(e => visitExp(e, env0))
          Expression.NativeMethod(method, es, subst0(tpe), eff, loc)

        case Expression.UserError(tpe, eff, loc) => Expression.UserError(subst0(tpe), eff, loc)
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
      }

      visitExp(exp0, env0)
    }

    /**
      * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
      */
    def specializeDefSym(sym: Symbol.DefnSym, tpe: Type): Symbol.DefnSym = {
      // Lookup the definition and its declared type.
      val defn = root.defs(sym)
      val declaredType = defn.tpe

      // Unify the declared and actual type to obtain the substitution map.
      val subst = StrictSubstitution(Unification.unify(declaredType, tpe).get)

      // Check if the substitution is empty, if so there is no need for specialization.
      if (subst.isEmpty) {
        return sym
      }

      // Check whether the function definition has already been specialized.
      def2def.get((sym, tpe)) match {
        case None =>
          // Case 1: The function has not been specialized.
          // Generate a fresh specialized definition symbol.
          val freshSym = Symbol.freshDefnSym(sym)

          // Register the fresh symbol (and actual type) in the symbol2symbol map.
          def2def.put((sym, tpe), freshSym)

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
      * Returns the eff symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
      */
    def specializeEffSym(sym: Symbol.EffSym, tpe: Type): Symbol.EffSym = {
      // Lookup the eff and its declared type.
      val eff = root.handlers(sym)
      val declaredType = eff.tpe

      // Unify the declared and actual type to obtain the substitution map.
      val subst = StrictSubstitution(Unification.unify(declaredType, tpe).get)

      // Check if the substitution is empty, if so there is no need for specialization.
      if (subst.isEmpty) {
        return sym
      }

      // Check whether the effect handler has already been specialized.
      eff2eff.get((sym, tpe)) match {
        case None =>
          // Case 1: The handler has not been specialized.
          // Generate a fresh specialized symbol.
          val freshSym = Symbol.freshEffSym(sym)

          // Register the fresh symbol (and actual type) in the symbol2symbol map.
          eff2eff.put((sym, tpe), freshSym)

          // Enqueue the fresh symbol with the definition and substitution.
          effQueue.enqueue((freshSym, eff, subst))

          // Now simply refer to the freshly generated symbol.
          freshSym
        case Some(specializedSym) =>
          // Case 2: The handler has already been specialized.
          // Simply refer to the already existing specialized symbol.
          specializedSym
      }
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
      val TypedAst.FormalParam(sym, mod, tpe, loc) = fparam0
      val freshSym = Symbol.freshVarSym(sym)
      (TypedAst.FormalParam(freshSym, mod, subst0(tpe), loc), Map(sym -> freshSym))
    }

    /**
      * Optionally returns the symbol of a function with the given `name` whose declared types unifies with the given type `tpe`.
      *
      * Returns `None` if no such function exists or more than one such function exist.
      */
    def lookup(name: String, tpe: Type): Option[Symbol.DefnSym] = {
      // A set of matching symbols.
      val matches = mutable.Set.empty[Symbol.DefnSym]

      // Iterate through each definition and collect the matching symbols.
      for ((sym, defn) <- root.defs) {
        // Check the function name.
        if (name == sym.name) {
          // Check whether the type unifies.
          if (Unification.unify(defn.tpe, tpe).isOk) {
            // Match found!
            matches += sym
          }
        }
      }

      // Returns the result if there is exactly one match.
      if (matches.size == 1) {
        return Some(matches.head)
      }
      // Otherwise return None.
      return None
    }

    /*
     * We can now use these helper functions to perform specialization of the whole program.
     */

    // Start the timer.
    val t = System.nanoTime()

    /*
     * A map used to collect specialized definitions, etc.
     */
    val specializedDefns: mutable.Map[Symbol.DefnSym, TypedAst.Def] = mutable.Map.empty
    val specializedHandlers: mutable.Map[Symbol.EffSym, TypedAst.Handler] = mutable.Map.empty
    val specializedProperties: mutable.ListBuffer[TypedAst.Property] = mutable.ListBuffer.empty
    // TODO: Specialize expressions occurring in other places, e.g facts/rules/properties.

    /*
     * Collect all non-parametric function definitions.
     */
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.tparams.isEmpty
    }

    /*
     * Perform specialization of all non-parametric function definitions.
     */
    for ((sym, defn) <- nonParametricDefns) {
      // Specialize the function definition under the empty substitution (it has no type parameters).
      val subst0 = StrictSubstitution(Unification.Substitution.empty)

      // Specialize the formal parameters to obtain fresh local variable symbols for them.
      val (fparams, env0) = specializeFormalParams(defn.fparams, subst0)

      // Specialize the body expression.
      val body = specialize(defn.exp, env0, subst0)

      // Reassemble the definition.
      specializedDefns.put(sym, defn.copy(fparams = fparams, exp = body))
    }

    /*
     * Perform specialization of all properties.
     */
    for (TypedAst.Property(law, defn, exp0, loc) <- root.properties) {
      // Specialize the property under the empty substitution.
      val subst0 = StrictSubstitution(Unification.Substitution.empty)

      // A property has no formal parameters and hence the initial environment is empty.
      val env0 = Map.empty[Symbol.VarSym, Symbol.VarSym]

      // Specialize the expression.
      val exp = specialize(exp0, env0, subst0)

      // Reassemble the property.
      specializedProperties += TypedAst.Property(law, defn, exp, loc)
    }

    /*
     * Performs function specialization until the queue is empty.
     */
    while (defQueue.nonEmpty) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val (freshSym, defn, subst) = defQueue.dequeue()

      // Specialize the formal parameters and introduce fresh local variable symbols.
      val (fparams, env0) = specializeFormalParams(defn.fparams, subst)

      // Specialize the body expression.
      val specializedExp = specialize(defn.exp, env0, subst)

      // Reassemble the definition.
      // NB: Removes the type parameters as the function is now monomorphic.
      val specializedDefn = defn.copy(sym = freshSym, fparams = fparams, exp = specializedExp, tpe = subst(defn.tpe), tparams = Nil)

      // Save the specialized function.
      specializedDefns.put(freshSym, specializedDefn)
    }

    /*
     * Performs effect handler specialization until the queue is empty.
     */
    while (effQueue.nonEmpty) {
      // Extract an effect from the queue and specializes it w.r.t. its substitution.
      val (freshSym, handler, subst) = effQueue.dequeue()

      // Specialize the formal parameters and introduce fresh local variable symbols.
      val (fparams, env0) = specializeFormalParams(handler.fparams, subst)

      // Specialize the body expression.
      val specializedExp = specialize(handler.exp, env0, subst)

      // Reassemble the definition.
      // NB: Removes the type parameters as the function is now monomorphic.
      val specializedHandler = handler.copy(sym = freshSym, fparams = fparams, exp = specializedExp, tpe = subst(handler.tpe), tparams = Nil)

      // Save the specialized handler.
      specializedHandlers.put(freshSym, specializedHandler)
    }

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      defs = specializedDefns.toMap,
      handlers = specializedHandlers.toMap,
      properties = specializedProperties.toList,
      time = root.time.copy(monomorph = e)
    ).toSuccess
  }

}
