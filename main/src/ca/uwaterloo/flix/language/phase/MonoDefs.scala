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
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.NormalType.toType
import ca.uwaterloo.flix.language.ast.{Ast, Kind, LoweredAst, LoweredNormalAst, NormalType, RigidityEnv, Scheme, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnvironment, Unification}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.{ListMap, MapOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/*
 * Goals:
 * - Ground types
 * - No aliases
 * - No associated types
 * - Simplified effects (Impure / Pure)
 *   - regions |-> Impure
 *   - concrete control effects |-> Impure
 * - Normalized types ({a = Int32, b = Int32} only, not {b = Int32, a = Int32})
 *   - todo everest: also for function arity?
 *
 * Methods:
 * - specialize functions on their instantiated type, not their instantiations
 * - resolve aliases in the substitution output?
 * - resolve associated types in the substitution output
 * - part of normalization
 * - normalize types before specialization (avoids redundancy and is required for MonoTypes)
 *
 * Q1: what about unconstrained variables?
 * -   def h(f: Bool -> Bool \ a + b): Bool \ a + b
 * -   h(x -> {println(x); x})
 * ->  def h$1(f: Bool -> Bool \ IO): Bool \ IO
 * -   with the substitution [a |-> a + !b, b |-> b]
 * -   where erasure maps [a |-> Pure, b |-> Pure]
 * Q2: What about region type variables?
 * - Regions variables are erased to Impure
 * Q3: Type normalization
 * Q3a: Effect Normalization
 * Q4: Erase aliases
 *
 * Invariants:
 * - Root.reachable and Root.entrypoint refers to functions that have no type parameters
 *   - otherwise they will be deleted and specialized into new symbols
 */

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
object MonoDefs {

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are replaced by the
    * Unit type (if using apply, not for applyWithoutErasure). In other words, when performing a type substitution if there is no requirement on a polymorphic type
    * we assume it to be Unit. This is safe since otherwise the type would not be polymorphic after type-inference.
    *
    * Result of apply is
    * - without aliases
    * - without associated types
    * - without variables
    * - has simplified effects (Pure or EffUniv only)
    * - NOT normalized (todo everest: will do later)
    *
    * applyWithoutErasure does not remove variables
    */
  private case class StrictSubstitution(m: Map[Symbol.KindedTypeVarSym, NormalType], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix) {

    private def default(tpe0: Type.Var): NormalType = tpe0.kind match {
      case Kind.Wild => NormalType.Cst(TypeConstructor.Unit) // todo everest: does this exist?
      case Kind.WildCaseSet => NormalType.Cst(TypeConstructor.Unit) // todo everest: does this exist?
      case Kind.Star => NormalType.Cst(TypeConstructor.Unit)
      case Kind.Eff =>
        // If an effect variable is free, we may assume its Pure due to the subst. lemma.
        NormalType.Cst(TypeConstructor.Pure)
      case Kind.Bool => NormalType.Cst(TypeConstructor.Unit) // todo everest: shouldn't this be true?
      case Kind.RecordRow => NormalType.Cst(TypeConstructor.RecordRowEmpty)
      case Kind.SchemaRow => NormalType.Cst(TypeConstructor.SchemaRowEmpty) // todo everest: does this exist?
      case Kind.Predicate => NormalType.Cst(TypeConstructor.Unit) // todo everest: does this exist?
      case Kind.CaseSet(sym) => NormalType.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym))
      case Kind.Arrow(_, _) => NormalType.Cst(TypeConstructor.Unit) // todo everest: what?
    }

    def apply(tpe0: Type): NormalType = {
      // NB: The order of cases has been determined by code coverage analysis.
      def visit(t: Type): NormalType =
        t match {
          case x: Type.Var => m.getOrElse(x.sym, default(x))
          case Type.Cst(TypeConstructor.Effect(_), _) => NormalType.Cst(TypeConstructor.EffUniv)
          case Type.Cst(tc, _) => NormalType.Cst(tc)
          case Type.Apply(t1, t2, _) =>
            val y = visit(t2)
            visit(t1) match {
              // Simplify boolean equations.
              case NormalType.Cst(TypeConstructor.Complement) =>
                NormalType.mkComplement(y)
              case NormalType.Apply(NormalType.Cst(TypeConstructor.Union), x) =>
                NormalType.mkUnion(x, y)
              case NormalType.Apply(NormalType.Cst(TypeConstructor.Intersection), x) =>
                NormalType.mkIntersection(x, y)

              // Simplify boolean equations.
              case NormalType.Cst(TypeConstructor.Not) => NormalType.mkNot(y)
              case NormalType.Apply(NormalType.Cst(TypeConstructor.Or), x) =>
                NormalType.mkOr(x, y)
              case NormalType.Apply(NormalType.Cst(TypeConstructor.And), x) =>
                NormalType.mkAnd(x, y)

              // Simplify set expressions
              case NormalType.Cst(TypeConstructor.CaseComplement(_)) =>
                NormalType.mkCaseComplement(y)
              case NormalType.Apply(NormalType.Cst(TypeConstructor.CaseIntersection(_)), x) =>
                NormalType.mkCaseIntersection(x, y)
              case NormalType.Apply(NormalType.Cst(TypeConstructor.CaseUnion(_)), x) =>
                NormalType.mkCaseUnion(x, y)

              // Else just apply
              case x => NormalType.Apply(x, y)
            }
          case Type.Alias(_, _, tpe0, _) =>
            visit(tpe0)
          case Type.AssocType(cst, arg0, _, loc) =>
            val arg = visit(arg0)
            EqualityEnvironment.reduceAssocType(cst, NormalType.toType(arg), eqEnv) match {
              case Ok(t) => visit(t)
              case Err(_) => throw InternalCompilerException("unexpected associated type reduction failure", loc)
            }
        }

      visit(tpe0)
    }

    def applyWithoutErasure(tpe0: Type): Type = {
      // NB: The order of cases has been determined by code coverage analysis.
      def visit(t: Type): Type =
        t match {
          case x: Type.Var => m.get(x.sym).map(NormalType.toType).getOrElse(x)
          case Type.Cst(TypeConstructor.Effect(_), loc) => Type.Cst(TypeConstructor.EffUniv, loc)
          case Type.Cst(tc, loc) => Type.Cst(tc, loc)
          case Type.Apply(t1, t2, loc) =>
            val y = visit(t2)
            visit(t1) match {
              // Simplify boolean equations.
              case Type.Cst(TypeConstructor.Complement, loc) =>
                Type.mkComplement(y, loc)
              case Type.Apply(Type.Cst(TypeConstructor.Union, _), x, loc) =>
                Type.mkUnion(x, y, loc)
              case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, loc) =>
                Type.mkIntersection(x, y, loc)

              // Simplify boolean equations.
              case Type.Cst(TypeConstructor.Not, loc) => Type.mkNot(y, loc)
              case Type.Apply(Type.Cst(TypeConstructor.Or, _), x, loc) =>
                Type.mkOr(x, y, loc)
              case Type.Apply(Type.Cst(TypeConstructor.And, _), x, loc) =>
                Type.mkAnd(x, y, loc)

              // Simplify set expressions
              case Type.Cst(TypeConstructor.CaseComplement(sym), loc) =>
                Type.mkCaseComplement(y, sym, loc)
              case Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, loc) =>
                Type.mkCaseIntersection(x, y, sym, loc)
              case Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, loc) =>
                Type.mkCaseUnion(x, y, sym, loc)

              // Else just apply
              case x => Type.Apply(x, y, loc)
            }
          case Type.Alias(_, _, tpe0, _) =>
            visit(tpe0)
          case Type.AssocType(cst, arg0, _, loc) =>
            val arg = visit(arg0)
            EqualityEnvironment.reduceAssocType(cst, arg, eqEnv) match {
              case Ok(t) => visit(t)
              case Err(_) => throw InternalCompilerException("unexpected associated type reduction failure", loc)
            }
        }

      visit(tpe0)
    }

    /**
      * Adds the given mapping to the substitution.
      */
    def +(kv: (Symbol.KindedTypeVarSym, NormalType)): StrictSubstitution = kv match {
      case (tvar, tpe) =>
        if (this.m.contains(tvar)) this
        else StrictSubstitution(this.m + (tvar -> tpe), this.eqEnv)
    }

  }

  /**
    * Holds the mutable data used throughout monomorphization.
    *
    * This class is thread-safe.
    */
  private class Context() {

    /**
      * A function-local queue of pending (fresh symbol, function definition, and substitution)-triples.
      *
      * For example, if the queue contains the entry:
      *
      * -   (f$1, f, [a -> Int])
      *
      * it means that the function definition f should be specialized w.r.t. the map [a -> Int] under the fresh name f$1.
      *
      * Note: We use a concurrent linked queue (which is non-blocking) so threads can enqueue items without contention.
      */
    private val defQueue: ConcurrentLinkedQueue[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)] = new ConcurrentLinkedQueue

    /**
      * Returns `true` if the queue is non-empty.
      *
      * Note: This is not synchronized.
      */
    def nonEmpty: Boolean = {
      !defQueue.isEmpty
    }

    /**
      * Enqueues the given symbol, def, and substitution triple.
      */
    def enqueue(sym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution): Unit = {
      defQueue.add((sym, defn, subst))
    }

    /**
      * Dequeues an element from the queue.
      *
      * Note: This is not synchronized.
      */
    def dequeueAll: Array[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)] = {
      val r = defQueue.toArray(Array.empty[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)])
      defQueue.clear()
      r
    }

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
    private val def2def: mutable.Map[(Symbol.DefnSym, NormalType), Symbol.DefnSym] = mutable.Map.empty

    /**
      * Optionally returns the specialized def symbol for the given symbol `sym` and type `tpe`.
      */
    def getDef2Def(sym: Symbol.DefnSym, tpe: NormalType): Option[Symbol.DefnSym] = synchronized {
      def2def.get((sym, tpe))
    }

    /**
      * Adds a new def2def binding for the given symbol `sym1` and type `tpe`.
      */
    def putDef2Def(sym1: Symbol.DefnSym, tpe: NormalType, sym2: Symbol.DefnSym): Unit = synchronized {
      def2def.put((sym1, tpe), sym2)
    }

    /**
      * A map used to collect specialized definitions, etc.
      */
    private val specializedDefns: mutable.Map[Symbol.DefnSym, LoweredNormalAst.Def] = mutable.Map.empty

    /**
      * Adds a new specialized definition for the given def symbol `sym`.
      */
    def putSpecializedDef(sym: Symbol.DefnSym, defn: LoweredNormalAst.Def): Unit = synchronized {
      specializedDefns.put(sym, defn)
    }

    /**
      * Returns the specialized definitions as an immutable map.
      */
    def toMap: Map[Symbol.DefnSym, LoweredNormalAst.Def] = synchronized {
      specializedDefns.toMap
    }
  }

  /**
    * Performs monomorphization of the given AST `root`.
    */
  def run(root: LoweredAst.Root)(implicit flix: Flix): LoweredNormalAst.Root = flix.phase("MonoDefs") {

    implicit val r: LoweredAst.Root = root
    implicit val ctx: Context = new Context()
    val empty = StrictSubstitution(Map.empty, root.eqEnv)

    /*
     * Collect all non-parametric function definitions.
     */
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.spec.tparams.isEmpty
    }

    /*
     * Perform specialization of all non-parametric function definitions.
     *
     * We perform specialization in parallel.
     *
     * This will enqueue additional functions for specialization.
     */
    ParOps.parMap(nonParametricDefns) {
      case (sym, defn) =>
        // We use an empty substitution because the defs are non-parametric.
        mkFreshDefn(sym, defn, empty)
    }

    /*
     * Perform function specialization until the queue is empty.
     *
     * We perform specialization in parallel along the frontier, i.e. each frontier is done in parallel.
     */
    while (ctx.nonEmpty) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val queue = ctx.dequeueAll
      ParOps.parMap(queue) {
        case (freshSym, defn, subst) => mkFreshDefn(freshSym, defn, subst)
      }
    }

    LoweredNormalAst.Root(
      ctx.toMap,
      root.enums,
      MapOps.mapValues(root.effects)(visitEffect),
      root.entryPoint,
      root.reachable,
      root.sources,
      root.eqEnv
    )
  }

  private def visitEffect(eff: LoweredAst.Effect)(implicit root: LoweredAst.Root, flix: Flix): LoweredNormalAst.Effect = eff match {
    case LoweredAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp)
      LoweredNormalAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  private def visitOp(op: LoweredAst.Op)(implicit root: LoweredAst.Root, flix: Flix): LoweredNormalAst.Op = op match {
    case LoweredAst.Op(sym, spec0) =>
      val LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, loc) = spec0
      assert(tparams.isEmpty)
      assert(declaredScheme.tconstrs.isEmpty)
      assert(declaredScheme.econstrs.isEmpty)
      assert(declaredScheme.quantifiers.isEmpty)
      assert(tconstrs.isEmpty)
      val rebuildFunctionType = Type.mkUncurriedArrowWithEffect(spec0.fparams.map(_.tpe), spec0.eff, spec0.retTpe, spec0.declaredScheme.base.loc)
      assert(spec0.declaredScheme.base == rebuildFunctionType, s"${spec0.declaredScheme.base}\n$rebuildFunctionType")
      val empty = StrictSubstitution(Map.empty, root.eqEnv)
      val spec = LoweredNormalAst.Spec(
        doc,
        ann,
        mod,
        specializeFormalParams(fparams, empty)._1,
        Scheme(Nil, Nil, Nil, toType(empty(declaredScheme.base))),
        empty(retTpe),
        empty(eff),
        loc
      )
      LoweredNormalAst.Op(sym, spec)
  }

  /**
    * Adds a specialized def for the given symbol `freshSym` and def `defn` with the given substitution `subst`.
    */
  private def mkFreshDefn(freshSym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Unit = {
    // Specialize the formal parameters and introduce fresh local variable symbols.
    val (fparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

    // Specialize the body expression.
    val specializedExp = visitExp(defn.exp, env0, subst)

    // Reassemble the definition.
    // NB: Removes the type parameters as the function is now monomorphic.
    val spec0 = defn.spec
    val spec = LoweredNormalAst.Spec(
      spec0.doc,
      spec0.ann,
      spec0.mod,
      fparams,
      Scheme(Nil, Nil, Nil, toType(subst(spec0.declaredScheme.base))),
      subst(spec0.retTpe),
      subst(spec0.eff),
      spec0.loc
    )
    val specializedDefn = LoweredNormalAst.Def(
      freshSym,
      spec,
      specializedExp
    )

    // Save the specialized function.
    ctx.putSpecializedDef(freshSym, specializedDefn)
  }

  /**
    * Replaces every reference to a parametric function with a reference to its specialized version.
    *
    * Replaces every local variable symbol with a fresh local variable symbol.
    *
    * Replaces every reference to a signature with a specialized instance function
    *
    * Resolves Type match
    *
    * Normalizes types
    *
    * If a specialized version of a function does not yet exists, a fresh symbol is created for it, and the
    * definition and substitution is enqueued.
    */
  private def visitExp(exp0: LoweredAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredNormalAst.Expr = exp0 match {
    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      LoweredNormalAst.Expr.Var(env0(sym), subst(tpe), loc)

    case LoweredAst.Expr.Def(sym0, tpe0, loc) =>
      /*
       * !! This is where all the magic happens !!
       */
      val tpe = subst(tpe0)
      val sym = specializeDefSym(sym0, tpe)
      LoweredNormalAst.Expr.Def(sym, tpe, loc)

    case LoweredAst.Expr.Sig(sym0, tpe0, loc) =>
      val tpe = subst(tpe0)
      val sym = specializeSigSym(sym0, tpe)
      LoweredNormalAst.Expr.Def(sym, tpe, loc)

    case LoweredAst.Expr.Cst(cst, tpe, loc) =>
      LoweredNormalAst.Expr.Cst(cst, subst(tpe), loc)

    case LoweredAst.Expr.Lambda(fparam0, exp0, tpe, loc) =>
      val (fparam, env1) = specializeFormalParam(fparam0, subst)
      val exp = visitExp(exp0, env0 ++ env1, subst)
      LoweredNormalAst.Expr.Lambda(fparam, exp, subst(tpe), loc)

    case LoweredAst.Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val es = exps.map(visitExp(_, env0, subst))
      LoweredNormalAst.Expr.Apply(e, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      LoweredNormalAst.Expr.ApplyAtomic(op, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      // Generate a fresh symbol for the let-bound variable.
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      LoweredNormalAst.Expr.Let(freshSym, mod, visitExp(exp1, env0, subst), visitExp(exp2, env1, subst), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      // Generate a fresh symbol for the let-bound variable.
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      LoweredNormalAst.Expr.LetRec(freshSym, mod, visitExp(exp1, env1, subst), visitExp(exp2, env1, subst), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      // forcedly mark the region variable as Impure inside the region
      val subst1 = StrictSubstitution(subst.m + (regionVar.sym -> NormalType.EffUniv), subst.eqEnv)
      LoweredNormalAst.Expr.Scope(freshSym, regionVar.sym, visitExp(exp, env1, subst1), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val e3 = visitExp(exp3, env0, subst)
      LoweredNormalAst.Expr.IfThenElse(e1, e2, e3, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      LoweredNormalAst.Expr.Stm(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      LoweredNormalAst.Expr.Discard(e, subst(eff), loc)

    case LoweredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val rs = rules map {
        case LoweredAst.MatchRule(pat0, guard, body) =>
          val (pat, env1) = visitPat(pat0, subst)
          val extendedEnv = env0 ++ env1
          val g = guard.map(visitExp(_, extendedEnv, subst))
          val b = visitExp(body, extendedEnv, subst)
          LoweredNormalAst.MatchRule(pat, g, b)
      }
      LoweredNormalAst.Expr.Match(visitExp(exp, env0, subst), rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TypeMatch(exp, rules, tpe, _, loc) =>
      // todo everest: relies on the erasure to non-existing types (i.e. a |-> NormalType.Erased)
      // todo everest: doesnt fix when _ escapes to the outside (outside |-> Erased, inside |-> Bool fx)
      val e = visitExp(exp, env0, subst)
      rules.iterator.flatMap {
        case LoweredAst.TypeMatchRule(sym, t, body0) =>
          // try to unify
          Unification.unifyTypes(toType(e.tpe), subst.applyWithoutErasure(t), RigidityEnv.empty) match {
            // Case 1: types don't unify; just continue
            case Result.Err(_) => None
            // Case 2: types unify; use the substitution in the body
            case Result.Ok((caseSubst, econstrs)) => // TODO ASSOC-TYPES consider econstrs
              // Generate a fresh symbol for the let-bound variable.
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              val strictCaseSubst = StrictSubstitution(MapOps.mapValues(caseSubst.m)(StrictSubstitution(Map.empty, subst.eqEnv).apply), subst.eqEnv)
              // This does not have to be proper composition because:
              // - The range of either substitution does not have variables
              //
              val subst1 = StrictSubstitution(subst.m ++ strictCaseSubst.m, subst.eqEnv)
              // visit the body under the extended environment
              val body = visitExp(body0, env1, subst1)
              val eff = NormalType.mkUnion(e.eff, body.eff)
              Some(LoweredNormalAst.Expr.Let(freshSym, Modifiers.Empty, e, body, subst1(tpe), eff, loc))
          }
      }.next() // We are safe to get next() because the last case will always match

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      LoweredNormalAst.Expr.VectorLit(es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      LoweredNormalAst.Expr.VectorLoad(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, env0, subst)
      LoweredNormalAst.Expr.VectorLength(e, loc)

    case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      LoweredNormalAst.Expr.Ascribe(e, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
      // We drop the declaredType and declaredEff here.
      val e = visitExp(exp, env0, subst)
      LoweredNormalAst.Expr.Cast(e, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.CatchRule(sym, clazz, body) =>
          // Generate a fresh symbol.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          val b = visitExp(body, env1, subst)
          LoweredNormalAst.CatchRule(freshSym, clazz, b)
      }
      LoweredNormalAst.Expr.TryCatch(e, rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TryWith(exp, effect, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.HandlerRule(op, fparams0, body0) =>
          val (fparams, fparamEnv) = specializeFormalParams(fparams0, subst)
          val env1 = env0 ++ fparamEnv
          val body = visitExp(body0, env1, subst)
          LoweredNormalAst.HandlerRule(op, fparams, body)
      }
      LoweredNormalAst.Expr.TryWith(e, effect, rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      LoweredNormalAst.Expr.Do(op, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod(_, env0, subst))
      LoweredNormalAst.Expr.NewObject(name, clazz, subst(tpe), subst(eff), methods, loc)

  }

  /**
    * Specializes the given pattern `p0` w.r.t. the current substitution.
    *
    * Returns the new pattern and a mapping from variable symbols to fresh variable symbols.
    */
  private def visitPat(p0: LoweredAst.Pattern, subst: StrictSubstitution)(implicit flix: Flix): (LoweredNormalAst.Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = p0 match {
    case LoweredAst.Pattern.Wild(tpe, loc) =>
      (LoweredNormalAst.Pattern.Wild(subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      // Generate a fresh variable symbol for the pattern-bound variable.
      val freshSym = Symbol.freshVarSym(sym)
      (LoweredNormalAst.Pattern.Var(freshSym, subst(tpe), loc), Map(sym -> freshSym))
    case LoweredAst.Pattern.Cst(cst, tpe, loc) =>
      (LoweredNormalAst.Pattern.Cst(cst, subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Tag(sym, pat, tpe, loc) =>
      val (p, env1) = visitPat(pat, subst)
      (LoweredNormalAst.Pattern.Tag(sym, p, subst(tpe), loc), env1)
    case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
      val (ps, envs) = elms.map(visitPat(_, subst)).unzip
      (LoweredNormalAst.Pattern.Tuple(ps, subst(tpe), loc), envs.reduce(_ ++ _))
    case LoweredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, envs) = pats.map {
        case LoweredAst.Pattern.Record.RecordLabelPattern(label, tpe1, pat1, loc1) =>
          val (p1, env1) = visitPat(pat1, subst)
          (LoweredNormalAst.Pattern.Record.RecordLabelPattern(label, subst(tpe1), p1, loc1), env1)
      }.unzip
      val (p, env1) = visitPat(pat, subst)
      val finalEnv = env1 :: envs
      (LoweredNormalAst.Pattern.Record(ps, p, subst(tpe), loc), finalEnv.reduce(_ ++ _))
    case LoweredAst.Pattern.RecordEmpty(tpe, loc) =>
      (LoweredNormalAst.Pattern.RecordEmpty(subst(tpe), loc), Map.empty)
  }

  /**
    * Specializes the given method `method` w.r.t. the current substitution.
    *
    * Returns the new method.
    */
  private def visitJvmMethod(method: LoweredAst.JvmMethod, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredNormalAst.JvmMethod = method match {
    case LoweredAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
      val (fparams, env1) = specializeFormalParams(fparams0, subst)
      val exp = visitExp(exp0, env0 ++ env1, subst)
      LoweredNormalAst.JvmMethod(ident, fparams, exp, subst(tpe), subst(eff), loc)
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  private def specializeDefSym(sym: Symbol.DefnSym, tpe: NormalType)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    // Lookup the definition and its declared type.
    val defn = root.defs(sym)

    // Check if the function is non-polymorphic.
    // this is important to keep root.reachable and root.entrypoint valid
    if (defn.spec.tparams.isEmpty) {
      // if there is no tparams then the initial loop over non-parametric defs
      // will have converted the function and since it maintains the symbol, we
      // use it unchanged here.
      defn.sym
    } else {
      specializeDef(defn, tpe)
    }
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    */
  private def specializeSigSym(sym: Symbol.SigSym, tpe: NormalType)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    val sig = root.sigs(sym)

    // lookup the instance corresponding to this type
    val instances = root.instances(sig.sym.clazz)

    val defns = instances.flatMap {
      inst =>
        inst.defs.find {
          defn =>
            defn.sym.text == sig.sym.name && Unification.unifiesWith(defn.spec.declaredScheme.base, toType(tpe), RigidityEnv.empty, root.eqEnv)
        }
    }

    (sig.exp, defns) match {
      // Case 1: An instance implementation exists. Use it.
      case (_, defn :: Nil) => specializeDef(defn, tpe)
      // Case 2: No instance implementation, but a default implementation exists. Use it.
      case (Some(impl), Nil) => specializeDef(sigToDef(sig.sym, sig.spec, impl), tpe)
      // Case 3: Multiple matching defs. Should have been caught previously.
      case (_, _ :: _ :: _) => throw InternalCompilerException(s"Expected at most one matching definition for '$sym', but found ${defns.size} signatures.", sym.loc)
      // Case 4: No matching defs and no default. Should have been caught previously.
      case (None, Nil) => throw InternalCompilerException(s"No default or matching definition found for '$sym'.", sym.loc)
    }
  }

  /**
    * Converts a signature with an implementation into the equivalent definition.
    */
  private def sigToDef(sigSym: Symbol.SigSym, spec: LoweredAst.Spec, exp: LoweredAst.Expr): LoweredAst.Def = {
    LoweredAst.Def(sigSymToDefnSym(sigSym), spec, exp)
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
  private def specializeDef(defn: LoweredAst.Def, tpe: NormalType)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    // Check whether the function definition has already been specialized.
    ctx.getDef2Def(defn.sym, tpe) match {
      case None =>
        // Case 1: The function has not been specialized.
        // Generate a fresh specialized definition symbol.
        val freshSym = Symbol.freshDefnSym(defn.sym)

        // Register the fresh symbol (and actual type) in the symbol2symbol map.
        ctx.putDef2Def(defn.sym, tpe, freshSym)

        // Unify the declared and actual type to obtain the substitution map.
        val subst = infallibleUnify(defn.spec.declaredScheme.base, toType(tpe))
        // Enqueue the fresh symbol with the definition and substitution.
        ctx.enqueue(freshSym, defn, subst)

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
  private def specializeFormalParams(fparams0: List[LoweredAst.FormalParam], subst0: StrictSubstitution)(implicit flix: Flix): (List[LoweredNormalAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    // Specialize each formal parameter and recombine the results.
    val (params, envs) = fparams0.map(p => specializeFormalParam(p, subst0)).unzip
    (params, envs.reduce(_ ++ _))
  }

  /**
    * Specializes the given formal parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  private def specializeFormalParam(fparam0: LoweredAst.FormalParam, subst0: StrictSubstitution)(implicit flix: Flix): (LoweredNormalAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
    val LoweredAst.FormalParam(sym, mod, tpe, src, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    (LoweredNormalAst.FormalParam(freshSym, mod, subst0(tpe), src, loc), Map(sym -> freshSym))
  }

  /**
    * Unifies `tpe1` and `tpe2` which must be unifiable.
    */
  private def infallibleUnify(tpe1: Type, tpe2: Type)(implicit root: LoweredAst.Root, flix: Flix): StrictSubstitution = {
    Unification.unifyTypes(tpe1, tpe2, RigidityEnv.empty) match {
      case Result.Ok((subst, econstrs)) => // TODO ASSOC-TYPES consider econstrs
        StrictSubstitution(MapOps.mapValues(subst.m)(StrictSubstitution(Map.empty, root.eqEnv).apply), root.eqEnv)
      case Result.Err(_) =>
        throw InternalCompilerException(s"Unable to unify: '$tpe1' and '$tpe2'.", tpe1.loc)
    }
  }

}
