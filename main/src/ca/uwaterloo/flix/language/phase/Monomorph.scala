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
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{BinaryOperator, Scheme, Symbol, Type, TypeConstructor, TypedAst, UnaryOperator}
import ca.uwaterloo.flix.language.phase.unification.{Substitution, Unification}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

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
  case class StrictSubstitution(s: Substitution) {
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
        case Type.Var(_, _, _) => Type.Unit
        case Type.Cst(_) => t
        case Type.Arrow(l, eff) => Type.Arrow(l, visit(eff))
        case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(label)), tpe), rest) => rest match {
          case Type.Var(_, _, _) => Type.mkRecordExtend(label, visit(tpe), Type.RecordEmpty)
          case _ => Type.mkRecordExtend(label, visit(tpe), visit(rest))
        }
        case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(sym)), tpe), rest) => rest match {
          case Type.Var(_, _, _) => Type.mkSchemaExtend(sym, visit(tpe), Type.SchemaEmpty)
          case _ => Type.mkSchemaExtend(sym, visit(tpe), visit(rest))
        }
        case Type.Apply(tpe1, tpe2) => Type.Apply(apply(tpe1), apply(tpe2))
        case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type: '$t'.")
      }

      visit(s(tpe))
    }
  }

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
    val defQueue: mutable.Queue[(Symbol.DefnSym, Def, StrictSubstitution)] = mutable.Queue.empty

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

    /**
      * A function-local set of all equality functions (all functions named `__eq`).
      */
    val eqDefs: mutable.ListBuffer[Def] = mutable.ListBuffer.empty

    /**
      * A function-local set of all comparator functions (all functions named `__cmp`).
      */
    val cmpDefs: mutable.ListBuffer[Def] = mutable.ListBuffer.empty

    // Populate eqDefs and cmpDefs.
    for ((sym, defn) <- root.defs) {
      if (sym.name == "__eq") {
        eqDefs += defn
      }
      if (sym.name == "__cmp") {
        cmpDefs += defn
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
    def specialize(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym], subst0: StrictSubstitution): Expression = {

      /**
        * Specializes the given expression `e0` under the environment `env0`. w.r.t. the current substitution.
        */
      def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = e0 match {
        case Expression.Wild(tpe, loc) => Expression.Wild(subst0(tpe), loc)
        case Expression.Var(sym, tpe, loc) => Expression.Var(env0(sym), subst0(tpe), loc)

        case Expression.Def(sym, tpe, loc) =>
          /*
           * !! This is where all the magic happens !!
           */
          val newSym = specializeDefSym(sym, subst0(tpe))
          Expression.Def(newSym, subst0(tpe), loc)

        case Expression.Hole(sym, tpe, eff, loc) => Expression.Hole(sym, subst0(tpe), eff, loc)

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

        case Expression.Lambda(fparam, exp, tpe, loc) =>
          val (p, env1) = specializeFormalParam(fparam, subst0)
          val e = visitExp(exp, env0 ++ env1)
          Expression.Lambda(p, e, subst0(tpe), loc)

        case Expression.Apply(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.Apply(e1, e2, subst0(tpe), eff, loc)

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
          val eqType = Type.mkArrow(List(valueType, valueType), Type.Pure, Type.Bool)

          // Look for an equality function with the expected type.
          // Returns `Some(sym)` if there is exactly one such function.
          lookupEq(eqType) match {
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
              val base = Expression.Def(newSym, eqType, loc)

              // Call the equality function.
              val inner = Expression.Apply(base, e1, Type.mkPureArrow(valueType, Type.Bool), eff, loc)
              val outer = Expression.Apply(inner, e2, Type.Bool, eff, loc)

              // Check whether the whether the operator is equality or inequality.
              if (op == BinaryOperator.Equal) {
                outer
              } else {
                Expression.Unary(UnaryOperator.LogicalNot, outer, Type.Bool, eff, loc)
              }
          }

        /*
         * Spaceship
         */
        case Expression.Binary(BinaryOperator.Spaceship, exp1, exp2, tpe, eff, loc) =>
          // Perform specialization on the left and right sub-expressions.
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)

          // The type of the values of exp1 and exp2. NB: The typer guarantees that exp1 and exp2 have the same type.
          val valueType = subst0(exp1.tpe)

          // The expected type of a comparator function: a -> a -> int.
          val cmpType = Type.mkArrow(List(valueType, valueType), Type.Pure, Type.Int32)

          // Look for a comparator function with the expected type.
          // Returns `Some(sym)` if there is exactly one such function.
          lookupCmp(cmpType) match {
            case None =>
              // No comparator function found. Return the same expression.
              Expression.Binary(BinaryOperator.Spaceship, e1, e2, Type.Int32, eff, loc)

            case Some(cmpSym) =>
              // Comparator function found. Specialize and generate a call to it.
              val newSym = specializeDefSym(cmpSym, cmpType)
              val base = Expression.Def(newSym, cmpType, loc)

              // Call the equality function.
              val inner = Expression.Apply(base, e1, Type.mkPureArrow(valueType, Type.Int32), eff, loc)
              Expression.Apply(inner, e2, Type.Int32, eff, loc)
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

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          val e3 = visitExp(exp3, env0)
          Expression.IfThenElse(e1, e2, e3, subst0(tpe), eff, loc)

        case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.Stm(e1, e2, subst0(tpe), eff, loc)

        case Expression.Match(exp, rules, tpe, eff, loc) =>
          val rs = rules map {
            case MatchRule(pat, guard, body) =>
              val (p, env1) = visitPat(pat)
              val extendedEnv = env0 ++ env1
              val g = visitExp(guard, extendedEnv)
              val b = visitExp(body, extendedEnv)
              MatchRule(p, g, b)
          }
          Expression.Match(visitExp(exp, env0), rs, subst0(tpe), eff, loc)

        case Expression.MatchNull(sym, exp1, exp2, exp3, tpe, eff, loc) =>
          // Generate a fresh symbol for the bound variable.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)

          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          val e3 = visitExp(exp3, env1)
          Expression.MatchNull(sym, e1, e2, e3, tpe, eff, loc)

        case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Tag(sym, tag, e, subst0(tpe), eff, loc)

        case Expression.Tuple(elms, tpe, eff, loc) =>
          val es = elms.map(e => visitExp(e, env0))
          Expression.Tuple(es, subst0(tpe), eff, loc)

        case Expression.RecordEmpty(tpe, loc) =>
          Expression.RecordEmpty(subst0(tpe), loc)

        case Expression.RecordSelect(base, label, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          Expression.RecordSelect(b, label, subst0(tpe), eff, loc)

        case Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
          val v = visitExp(value, env0)
          val r = visitExp(rest, env0)
          Expression.RecordExtend(label, v, r, subst0(tpe), eff, loc)

        case Expression.RecordRestrict(label, rest, tpe, eff, loc) =>
          val r = visitExp(rest, env0)
          Expression.RecordRestrict(label, r, subst0(tpe), eff, loc)

        case Expression.ArrayLit(elms, tpe, eff, loc) =>
          val es = elms.map(e => visitExp(e, env0))
          Expression.ArrayLit(es, subst0(tpe), eff, loc)

        case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
          val e = visitExp(elm, env0)
          val ln = visitExp(len, env0)
          Expression.ArrayNew(e, ln, subst0(tpe), eff, loc)

        case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
          val b = visitExp(base, env0)
          val i = visitExp(index, env0)
          Expression.ArrayLoad(b, i, subst0(tpe), eff, loc)

        case Expression.ArrayStore(base, index, elm, loc) =>
          val b = visitExp(base, env0)
          val i = visitExp(index, env0)
          val e = visitExp(elm, env0)
          Expression.ArrayStore(b, i, e, loc)

        case Expression.ArrayLength(base, eff, loc) =>
          val b = visitExp(base, env0)
          Expression.ArrayLength(b, eff, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = visitExp(base, env0)
          val i1 = visitExp(startIndex, env0)
          val i2 = visitExp(endIndex, env0)
          Expression.ArraySlice(b, i1, i2, subst0(tpe), loc)

        case Expression.Ref(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Ref(e, subst0(tpe), eff, loc)

        case Expression.Deref(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Deref(e, subst0(tpe), eff, loc)

        case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.Assign(e1, e2, subst0(tpe), eff, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val (param, env1) = specializeFormalParam(fparam, subst0)
          Expression.Existential(param, visitExp(exp, env0 ++ env1), loc)

        case Expression.Universal(fparam, exp, loc) =>
          val (param, env1) = specializeFormalParam(fparam, subst0)
          Expression.Universal(param, visitExp(exp, env0 ++ env1), loc)

        case Expression.Ascribe(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Ascribe(e, subst0(tpe), eff, loc)

        case Expression.Cast(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Cast(e, subst0(tpe), eff, loc)

        case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              // Generate a fresh symbol.
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              val b = visitExp(body, env1)
              CatchRule(freshSym, clazz, b)
          }
          Expression.TryCatch(e, rs, subst0(tpe), eff, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
          val as = args.map(visitExp(_, env0))
          Expression.InvokeConstructor(constructor, as, subst0(tpe), eff, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          val as = args.map(visitExp(_, env0))
          Expression.InvokeMethod(method, e, as, tpe, eff, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
          val as = args.map(visitExp(_, env0))
          Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

        case Expression.GetField(field, exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.GetField(field, e, tpe, eff, loc)

        case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.PutField(field, e1, e2, tpe, eff, loc)

        case Expression.GetStaticField(field, tpe, eff, loc) =>
          Expression.GetStaticField(field, tpe, eff, loc)

        case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.PutStaticField(field, e, tpe, eff, loc)

        case Expression.NewChannel(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.NewChannel(e, subst0(tpe), eff, loc)

        case Expression.GetChannel(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.GetChannel(e, subst0(tpe), eff, loc)

        case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.PutChannel(e1, e2, subst0(tpe), eff, loc)

        case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              val c = visitExp(chan, env1)
              val e = visitExp(exp, env1)
              SelectChannelRule(freshSym, c, e)
          }

          val d = default.map(visitExp(_, env0))

          Expression.SelectChannel(rs, d, subst0(tpe), eff, loc)

        case Expression.Spawn(exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.Spawn(e, subst0(tpe), eff, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) =>
          val cs = cs0.map(visitConstraint(_, env0))
          Expression.FixpointConstraintSet(cs, subst0(tpe), loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.FixpointCompose(e1, e2, subst0(tpe), eff, loc)

        case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.FixpointSolve(e, stf, subst0(tpe), eff, loc)

        case Expression.FixpointProject(name, exp, tpe, eff, loc) =>
          val e = visitExp(exp, env0)
          Expression.FixpointProject(name, e, subst0(tpe), eff, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          Expression.FixpointEntails(e1, e2, subst0(tpe), eff, loc)

        case Expression.FixpointFold(name, exp1, exp2, exp3, tpe, eff, loc) =>
          val e1 = visitExp(exp1, env0)
          val e2 = visitExp(exp2, env0)
          val e3 = visitExp(exp3, env0)
          Expression.FixpointFold(name, e1, e2, e3, subst0(tpe), eff, loc)
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

      /**
        * Specializes the given constraint `c0` w.r.t. the given environment and current substitution.
        */
      def visitConstraint(c0: Constraint, env0: Map[Symbol.VarSym, Symbol.VarSym]): Constraint = {
        val Constraint(cparams0, head0, body0, loc) = c0
        val (cparams, env1) = specializeConstraintParams(cparams0, subst0)
        val head = visitHeadPredicate(head0, env0 ++ env1)
        val body = body0.map(b => visitBodyPredicate(b, env0 ++ env1))
        Constraint(cparams, head, body, loc)
      }

      /**
        * Specializes the given head predicate `h0` w.r.t. the given environment and current substitution.
        */
      def visitHeadPredicate(h0: Predicate.Head, env0: Map[Symbol.VarSym, Symbol.VarSym]): Predicate.Head = h0 match {
        case Predicate.Head.Atom(name, den, terms, tpe, loc) =>
          val ts = terms.map(t => visitExp(t, env0))
          Predicate.Head.Atom(name, den, ts, subst0(tpe), loc)

        case Predicate.Head.Union(exp, tpe, loc) =>
          val e = visitExp(exp, env0)
          Predicate.Head.Union(e, subst0(tpe), loc)
      }

      /**
        * Specializes the given body predicate `b0` w.r.t. the given environment and current substitution.
        */
      def visitBodyPredicate(b0: Predicate.Body, env0: Map[Symbol.VarSym, Symbol.VarSym]): Predicate.Body = {
        // TODO: We should change what is allowed here. Probably only variables and constants should be allowed?
        def visitPatTemporaryToBeRemoved(pat: Pattern): Pattern = pat match {
          case Pattern.Wild(tpe, loc) => Pattern.Wild(subst0(tpe), loc)
          case Pattern.Var(sym, tpe, loc) => Pattern.Var(env0(sym), subst0(tpe), loc)
          case Pattern.Unit(loc) => Pattern.Unit(loc)
          case Pattern.True(loc) => Pattern.True(loc)
          case Pattern.False(loc) => Pattern.False(loc)
          case Pattern.Char(lit, loc) => Pattern.Char(lit, loc)
          case Pattern.Float32(lit, loc) => Pattern.Float32(lit, loc)
          case Pattern.Float64(lit, loc) => Pattern.Float64(lit, loc)
          case Pattern.Int8(lit, loc) => Pattern.Int8(lit, loc)
          case Pattern.Int16(lit, loc) => Pattern.Int16(lit, loc)
          case Pattern.Int32(lit, loc) => Pattern.Int32(lit, loc)
          case Pattern.Int64(lit, loc) => Pattern.Int64(lit, loc)
          case Pattern.BigInt(lit, loc) => Pattern.BigInt(lit, loc)
          case Pattern.Str(lit, loc) => Pattern.Str(lit, loc)
          case Pattern.Tag(sym, tag, p, tpe, loc) => Pattern.Tag(sym, tag, visitPatTemporaryToBeRemoved(p), subst0(tpe), loc)
          case _ => throw InternalCompilerException(s"Pattern not allowed here $pat.")
        }

        b0 match {
          case Predicate.Body.Atom(name, den, polarity, terms, tpe, loc) =>
            val ts = terms map visitPatTemporaryToBeRemoved
            Predicate.Body.Atom(name, den, polarity, ts, subst0(tpe), loc)

          case Predicate.Body.Guard(exp, loc) =>
            val e = visitExp(exp, env0)
            Predicate.Body.Guard(e, loc)
        }
      }

      visitExp(exp0, env0)
    }

    /**
      * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
      */
    def specializeDefSym(sym: Symbol.DefnSym, tpe: Type): Symbol.DefnSym = {
      // Lookup the definition and its declared type.
      val defn = root.defs(sym)

      // Check if the function is non-polymorphic.
      if (defn.tparams.isEmpty) {
        return sym
      }

      // Unify the declared and actual type to obtain the substitution map.
      val subst = StrictSubstitution(Unification.unifyTypes(defn.inferredScheme.base, tpe).get)

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
      * Specializes the given formal parameters `fparams0` w.r.t. the given substitution `subst0`.
      *
      * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
      */
    def specializeFormalParams(fparams0: List[FormalParam], subst0: StrictSubstitution): (List[FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
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
    def specializeFormalParam(fparam0: FormalParam, subst0: StrictSubstitution): (FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
      val FormalParam(sym, mod, tpe, loc) = fparam0
      val freshSym = Symbol.freshVarSym(sym)
      (FormalParam(freshSym, mod, subst0(tpe), loc), Map(sym -> freshSym))
    }

    /**
      * Specializes the given constraint parameters `cparams0` w.r.t. the given substitution `subst0`.
      *
      * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
      */
    def specializeConstraintParams(cparams0: List[ConstraintParam], subst0: StrictSubstitution): (List[ConstraintParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
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
    def specializeConstraintParam(cparam0: ConstraintParam, subst0: StrictSubstitution): (ConstraintParam, Map[Symbol.VarSym, Symbol.VarSym]) = cparam0 match {
      case ConstraintParam.HeadParam(sym, tpe, loc) =>
        val freshSym = Symbol.freshVarSym(sym)
        (ConstraintParam.HeadParam(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
      case ConstraintParam.RuleParam(sym, tpe, loc) =>
        val freshSym = Symbol.freshVarSym(sym)
        (ConstraintParam.RuleParam(freshSym, subst0(tpe), loc), Map(sym -> freshSym))
    }

    /**
      * Optionally returns the symbol of a function with the `__eq` name whose declared types unifies with the given type `tpe`.
      *
      * Returns `None` if no such function exists or more than one such function exist.
      */
    def lookupEq(tpe: Type): Option[Symbol.DefnSym] = tpe match {
      // Equality cannot be overriden for primitive types.
      case Type.Cst(TypeConstructor.Unit) => None
      case Type.Cst(TypeConstructor.Bool) => None
      case Type.Cst(TypeConstructor.Char) => None
      case Type.Cst(TypeConstructor.Float32) => None
      case Type.Cst(TypeConstructor.Float64) => None
      case Type.Cst(TypeConstructor.Int8) => None
      case Type.Cst(TypeConstructor.Int16) => None
      case Type.Cst(TypeConstructor.Int32) => None
      case Type.Cst(TypeConstructor.Int64) => None
      case Type.Cst(TypeConstructor.BigInt) => None
      case Type.Cst(TypeConstructor.Str) => None
      case _ => lookupIn("__eq", tpe, eqDefs)
    }

    /**
      * Optionally returns the symbol of a function with the `__cmp` name whose declared types unifies with the given type `tpe`.
      *
      * Returns `None` if no such function exists or more than one such function exist.
      */
    def lookupCmp(tpe: Type): Option[Symbol.DefnSym] = tpe match {
      // Ordering cannot be overriden for primitive types.
      case Type.Cst(TypeConstructor.Unit) => None
      case Type.Cst(TypeConstructor.Bool) => None
      case Type.Cst(TypeConstructor.Char) => None
      case Type.Cst(TypeConstructor.Float32) => None
      case Type.Cst(TypeConstructor.Float64) => None
      case Type.Cst(TypeConstructor.Int8) => None
      case Type.Cst(TypeConstructor.Int16) => None
      case Type.Cst(TypeConstructor.Int32) => None
      case Type.Cst(TypeConstructor.Int64) => None
      case Type.Cst(TypeConstructor.BigInt) => None
      case Type.Cst(TypeConstructor.Str) => None
      case _ => lookupIn("__cmp", tpe, cmpDefs)
    }

    /**
      * Optionally returns the symbol of the function with `name` whose declared types unifies with the given type `tpe` and appears in `defns`.
      */
    def lookupIn(name: String, tpe: Type, defns: mutable.Iterable[Def]): Option[Symbol.DefnSym] = {
      // A set of matching symbols.
      val matches = mutable.Set.empty[Symbol.DefnSym]

      // Iterate through each definition and collect the matching symbols.
      for (defn <- defns) {
        val sym = defn.sym
        if (name == sym.name) {
          val declaredType = Scheme.instantiate(defn.inferredScheme, InstantiateMode.Flexible)
          if (Unification.unifyTypes(declaredType, tpe).isInstanceOf[Result.Ok[_, _]]) {
            matches += sym
          }
        }
      }

      if (matches.size != 1) None else Some(matches.head)
    }

    /*
     * We can now use these helper functions to perform specialization of the whole program.
     */

    /*
     * A map used to collect specialized definitions, etc.
     */
    val specializedDefns: mutable.Map[Symbol.DefnSym, TypedAst.Def] = mutable.Map.empty
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
      val subst0 = StrictSubstitution(Substitution.empty)

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
      val subst0 = StrictSubstitution(Substitution.empty)

      // A property has no formal parameters and hence the initial environment is empty.
      val env0 = Map.empty[Symbol.VarSym, Symbol.VarSym]

      // Specialize the expression.
      val exp = specialize(exp0, env0, subst0)

      // Reassemble the property.
      specializedProperties += TypedAst.Property(law, defn, exp, loc)
    }

    /*
     * Performs function specialization until both queues are empty.
     */
    while (defQueue.nonEmpty) {

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
        val specializedDefn = defn.copy(sym = freshSym, fparams = fparams, exp = specializedExp, inferredScheme = Scheme(Nil, subst(defn.inferredScheme.base)), tparams = Nil)

        // Save the specialized function.
        specializedDefns.put(freshSym, specializedDefn)
      }

    }

    // Reassemble the AST.
    root.copy(
      defs = specializedDefns.toMap,
      properties = specializedProperties.toList
    ).toSuccess
  }

}
