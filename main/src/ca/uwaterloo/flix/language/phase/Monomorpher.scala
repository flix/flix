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
import ca.uwaterloo.flix.language.ast.LoweredAst.Instance
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Scope}
import ca.uwaterloo.flix.language.ast.{AtomicOp, Kind, LoweredAst, MonoAst, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, Progress, TypeReduction2}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.{CofiniteSet, ListMap, ListOps, MapOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
  * Monomorphization is a whole-program compilation strategy that replaces every reference to a
  * parametric function with a reference to a non-parametric version (of that function) specialized
  * to the concrete types of the reference.
  *
  * Additionally it eliminates all type variables and normalizes types. This means that types have
  * unique representations, without aliases and associated types.
  *
  * Additionally it resolves type class methods into actual function calls.
  *
  * For example, the polymorphic program:
  *
  *   - `def fst[a, b](p: (a, b)): a = let (x, y) = p ; x`
  *   - `def f: Bool = fst((true, 'a'))`
  *   - `def g: Int32 = fst((42, "foo"))`
  *
  * is, roughly speaking, translated to:
  *
  *   - `def fst$1(p: (Bool, Char)): Bool = let (x, y) = p ; x`
  *   - `def fst$2(p: (Int32, String)): Int32 = let (x, y) = p ; x`
  *   - `def f: Bool = fst$1((true, 'a'))`
  *   - `def g: Bool = fst$2((42, "foo"))`
  *
  * Additionally for things like record types and effect formulas, equivalent types are flattened
  * and ordered. This means that `{b = String, a = String}` becomes `{a = String, b = String}` and
  * that `Print + (Crash + Print)` becomes `Crash + Print`.
  *
  * Enums and structs can both express type-recursion which cannot be resolved without erasure. This
  * means that those definitions are left polymorphic, but the types are still resolved of
  * associated types fx. Throughout monomorphization, `visitX` is for conversion and `specializeX`
  * is for monomorphization.
  *
  * At a high-level, monomorphization works as follows:
  *
  *   - 1. We maintain a queue of functions and the concrete, normalized types they must be
  *      specialized to.
  *   - 2. We populate the queue by specialization of non-parametric function definitions.
  *   - 3. We iteratively extract a function from the queue and specialize it:
  *      - a. We replace every type variable appearing anywhere in the definition by its concrete
  *         type.
  *      - b. We create new fresh local variable symbols (since the function is effectively being
  *         copied).
  *      - c. We enqueue (or re-use) other functions referenced by the current function which require
  *         specialization.
  *   - 4. We reconstruct the AST from the specialized functions and remove all parametric functions.
  *
  * Type normalization details:
  *
  *   - Record fields are in alphabetical order
  *   - Schema fields are in alphabetical order
  *   - Effect formulas are flat unions of effects in alphabetical order or a complement thereof.
  *   - Case set formulas are a single CaseSet literal or a complement thererof.
  *
  */
object Monomorpher {

  /** The effect that all [[TypeConstructor.Region]] are instantiated to. */
  private val RegionInstantiation: TypeConstructor.Effect =
    TypeConstructor.Effect(Symbol.IO)

  /** Companion object for [[StrictSubstitution]]. */
  private object StrictSubstitution {

    /** The empty substitution. */
    val empty: StrictSubstitution = StrictSubstitution(Substitution.empty)

    /**
      * A smart constructor for [[StrictSubstitution]].
      *
      * The smart constructor ensures that all types in the substitution are grounded.
      */
    def mk(s: Substitution)(implicit root: LoweredAst.Root, flix: Flix): StrictSubstitution = {
      val m = s.m.map {
        case (sym, tpe) => sym -> simplify(tpe.map(default), isGround = true)
      }
      StrictSubstitution(Substitution(m))
    }
  }

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are
    * replaced by an appropriate default type. In other words, when performing a type substitution,
    * if there is no requirement on a polymorphic type, we assume it to be the default type of its
    * kind. This is safe since otherwise the type would not be polymorphic after type-inference.
    *
    * Properties of normalized types (which is returned by apply):
    *   - No type variables
    *   - No associated types
    *   - No type aliases
    *   - Equivalent types are uniquely represented (e.g. fields in records types are alphabetized)
    */
  private case class StrictSubstitution(s: Substitution) {

    /**
      * Applies `this` substitution to the given type `tpe`, returning a normalized type.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    def apply(tpe0: Type)(implicit root: LoweredAst.Root, flix: Flix): Type = tpe0 match {
      case v@Type.Var(sym, _) => s.m.get(sym) match {
        case None =>
          // Variable unbound. Use the default type.
          default(v)
        case Some(t) =>
          // Variable in subst. Note: All types in the *StrictSubstitution* are already normalized.
          t
      }

      case Type.Cst(TypeConstructor.Region(_), loc) =>
        Type.Cst(RegionInstantiation, loc)

      case cst@Type.Cst(_, _) =>
        // Maintain and exploit reference equality for performance.
        cst

      case app@Type.Apply(_, _, _) =>
        normalizeApply(apply, app, isGround = true)

      case Type.Alias(_, _, t, _) =>
        // Remove the Alias and continue.
        apply(t)

      case Type.AssocType(symUse, arg0, kind, loc) =>
        val arg = apply(arg0)
        val assoc = Type.AssocType(symUse, arg, kind, loc)
        val reducedType = reduceAssocType(assoc)
        // `reducedType` is ground, but might need normalization.
        simplify(reducedType, isGround = true)

      case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
      case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)
      case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
    }

    /**
      * Returns the non-strict version of this substitution.
      *
      * Mapping added must not contain type aliases or associated types. Variables in the image of
      * the substitution are considered unconstrained.
      */
    def nonStrict: Substitution = s
  }

  /**
    * The mutable data used throughout monomorphization.
    *
    * This class is thread-safe.
    */
  private class Context {

    /**
      * A queue of pending (fresh symbol, function definition, and substitution)-triples.
      *
      * If the queue contains the entry:
      *
      *   - `(f$1, f, [a -> Int32])`
      *
      * it means that the function definition `f` should be specialized w.r.t. the map
      * `[a -> Int32]` under the fresh name `f$1`.
      *
      * Note: [[ConcurrentLinkedQueue]] is non-blocking so threads can enqueue items without
      * contention.
      */
    private val defQueue: ConcurrentLinkedQueue[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)] =
      new ConcurrentLinkedQueue

    /** Returns `true` if the queue is non-empty. */
    def nonEmptySpecializationQueue: Boolean =
      synchronized {
        !defQueue.isEmpty
      }

    /**
      * Enqueues `defn` to be specialized according to `subst` with the new name `sym`.
      *
      * This should be used in combination with [[getSpecializedName]] and [[addSpecializedName]] to
      * avoid enqueuing duplicate specializations.
      */
    def enqueueSpecialization(sym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution): Unit =
      synchronized {
        defQueue.add((sym, defn, subst))
      }

    /** Dequeues all elements from the queue and clears it. */
    def dequeueAllSpecializations: Array[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)] =
      synchronized {
        val r = defQueue.toArray(Array.empty[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)])
        defQueue.clear()
        r
      }

    /**
      * A map from a symbol and a normalized type to the fresh symbol for the specialized version
      * of that function.
      *
      * For example, if the function:
      *
      *   - `def fst[a, b](x: a, y: b): a = ...`
      *
      * has been specialized w.r.t. to `Int32` and `String` then this map will contain an entry:
      *
      *   - `(fst, (Int32, String) -> Int32) -> fst$1`
      */
    private val specializedNames: mutable.Map[(Symbol.DefnSym, Type), Symbol.DefnSym] =
      mutable.Map.empty

    /** Returns the specialized def symbol for `sym` w.r.t. `tpe` if it exists. */
    def getSpecializedName(sym: Symbol.DefnSym, tpe: Type): Option[Symbol.DefnSym] =
      synchronized {
        specializedNames.get((sym, tpe))
      }

    /** Adds a specialized name, `specializedSym`, for `sym` w.r.t `tpe`. */
    def addSpecializedName(sym1: Symbol.DefnSym, tpe: Type, sym2: Symbol.DefnSym): Unit =
      synchronized {
        specializedNames.put((sym1, tpe), sym2)
      }

    /** A map of specialized definitions. */
    private val specializedDefns: mutable.Map[Symbol.DefnSym, MonoAst.Def] =
      mutable.Map.empty

    /** Add a new specialized definition. */
    def addSpecializedDef(sym: Symbol.DefnSym, defn: MonoAst.Def): Unit =
      synchronized {
        specializedDefns.put(sym, defn)
      }

    /** Returns the specialized definitions as an immutable map. */
    def getSpecializedDefs: Map[Symbol.DefnSym, MonoAst.Def] =
      synchronized {
        specializedDefns.toMap
      }
  }

  /**
    * Returns a sorted record, assuming that `rest` is sorted.
    *
    * labels of the same name are not reordered.
    *
    * N.B: `rest` must not contain [[Type.AssocType]] or [[Type.Alias]].
    */
  private def mkRecordExtendSorted(label: Name.Label, tpe: Type, rest: Type, loc: SourceLocation): Type = rest match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(l), loc1), t, loc2), r, loc3) if l.name < label.name =>
      // Push extend further.
      val newRest = mkRecordExtendSorted(label, tpe, r, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(l), loc1), t, loc2), newRest, loc3)
    case Type.Cst(_, _) | Type.Apply(_, _, _) | Type.Var(_, _) =>
      // Non-record related types or a record in correct order.
      Type.mkRecordRowExtend(label, tpe, rest, loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected alias '$rest'", rest.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected associated type '$rest'", rest.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected JVM eff '$rest'", rest.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
  }

  /**
    * Returns a sorted schema, assuming that `rest` is sorted.
    *
    * Sorting is stable on duplicate predicates.
    *
    * Assumes that rest does not contain variables, aliases, or associated types.
    */
  private def mkSchemaExtendSorted(label: Name.Pred, tpe: Type, rest: Type, loc: SourceLocation): Type = rest match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(l), loc1), t, loc2), r, loc3) if l.name < label.name =>
      // Push extend further.
      val newRest = mkSchemaExtendSorted(label, tpe, r, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(l), loc1), t, loc2), newRest, loc3)
    case Type.Cst(_, _) | Type.Apply(_, _, _) | Type.Var(_, _) =>
      // Non-record related types or a record in correct order.
      Type.mkSchemaRowExtend(label, tpe, rest, loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected alias '$rest'", rest.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected associated type '$rest'", rest.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected JVM eff '$rest'", rest.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
  }

  /** Performs monomorphization of the given AST `root`. */
  def run(root: LoweredAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Monomorpher") {
    implicit val r: LoweredAst.Root = root
    implicit val is: Map[(Symbol.TraitSym, TypeConstructor), Instance] = mkInstanceMap(root.instances)
    implicit val ctx: Context = new Context()

    // Collect all non-parametric function definitions.
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.spec.tparams.isEmpty
    }

    // Perform specialization of all non-parametric function definitions.
    // We perform specialization in parallel.
    // This will enqueue additional functions for specialization.
    ParOps.parMap(nonParametricDefns) {
      case (sym, defn) =>
        // We use an empty substitution because the defs are non-parametric.
        // It's important that non-parametric functions keep their symbol to not
        // invalidate the set of entryPoints functions.
        val specializedDefn = specializeDef(sym, defn, StrictSubstitution.empty)
        ctx.addSpecializedDef(sym, specializedDefn)
    }

    // Perform function specialization until the queue is empty.
    // Perform specialization in parallel along the frontier, i.e. each frontier is done in parallel.
    while (ctx.nonEmptySpecializationQueue) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val queue = ctx.dequeueAllSpecializations
      ParOps.parMap(queue) {
        case (freshSym, defn, subst) =>
          val specializedDefn = specializeDef(freshSym, defn, subst)
          ctx.addSpecializedDef(freshSym, specializedDefn)
      }
    }

    val effects = ParOps.parMapValues(root.effects) {
      case LoweredAst.Effect(doc, ann, mod, sym, ops0, loc) =>
        val ops = ops0.map(visitEffectOp)
        MonoAst.Effect(doc, ann, mod, sym, ops, loc)
    }

    val enums = ParOps.parMapValues(root.enums) {
      case LoweredAst.Enum(doc, ann, mod, sym, tparams0, _, cases, loc) =>
        val newCases = MapOps.mapValues(cases)(visitEnumCase)
        val tparams = tparams0.map(visitTypeParam)
        MonoAst.Enum(doc, ann, mod, sym, tparams, newCases, loc)
    }

    val structs = ParOps.parMapValues(root.structs) {
      case LoweredAst.Struct(doc, ann, mod, sym, tparams0, fields, loc) =>
        val newFields = fields.map(visitStructField)
        val tparams = tparams0.map(visitTypeParam)
        MonoAst.Struct(doc, ann, mod, sym, tparams, newFields, loc)
    }

    MonoAst.Root(
      ctx.getSpecializedDefs,
      enums,
      structs,
      effects,
      root.mainEntryPoint,
      root.entryPoints,
      root.sources
    )
  }

  /** Creates a table for fast lookup of instances. */
  private def mkInstanceMap(instances: ListMap[Symbol.TraitSym, Instance]): Map[(Symbol.TraitSym, TypeConstructor), Instance] = {
    instances.map {
      case (sym, inst) => ((sym, inst.tpe.typeConstructor.get), inst)
    }.toMap
  }

  /** Converts `field`, simplifying its polymorphic type. */
  def visitStructField(field: LoweredAst.StructField)(implicit root: LoweredAst.Root, flix: Flix): MonoAst.StructField = {
    field match {
      case LoweredAst.StructField(fieldSym, tpe, loc) =>
        MonoAst.StructField(fieldSym, simplify(tpe, isGround = false), loc)
    }
  }

  /** Converts `caze`, simplifying its polymorphic type. */
  def visitEnumCase(caze: LoweredAst.Case)(implicit root: LoweredAst.Root, flix: Flix): MonoAst.Case = {
    caze match {
      case LoweredAst.Case(sym, tpes, _, loc) =>
        MonoAst.Case(sym, tpes.map(simplify(_, isGround = false)), loc)
    }
  }

  /** Converts `tparam` directly. */
  private def visitTypeParam(tparam: LoweredAst.TypeParam): MonoAst.TypeParam = tparam match {
    case LoweredAst.TypeParam(name, sym, loc) => MonoAst.TypeParam(name, sym, loc)
  }

  /** Converts `op`, simplifying its type. */
  private def visitEffectOp(op: LoweredAst.Op)(implicit root: LoweredAst.Root, flix: Flix): MonoAst.Op =
    op match {
      case LoweredAst.Op(sym, LoweredAst.Spec(doc, ann, mod, _, fparams0, declaredScheme, retTpe, eff, _), loc) =>
        // Effect operations are monomorphic - they have no variables.
        // The substitution can be left empty.
        val fparams = fparams0.map {
          case LoweredAst.FormalParam(varSym, fparamMod, tpe, src, fpLoc) =>
            MonoAst.FormalParam(varSym, fparamMod, StrictSubstitution.empty(tpe), src, fpLoc)
        }
        val spec = MonoAst.Spec(doc, ann, mod, fparams, declaredScheme.base, StrictSubstitution.empty(retTpe), StrictSubstitution.empty(eff))
        MonoAst.Op(sym, spec, loc)
    }

  /** Returns a specialization of `defn` with the name `freshSym` according to `subst`. */
  private def specializeDef(freshSym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: LoweredAst.Root, flix: Flix): MonoAst.Def = {
    val (specializedFparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

    val specializedExp = specializeExp(defn.exp, env0, subst)

    val spec0 = defn.spec
    val spec = MonoAst.Spec(
      spec0.doc,
      spec0.ann,
      spec0.mod,
      specializedFparams,
      subst(defn.spec.declaredScheme.base),
      subst(spec0.retTpe),
      subst(spec0.eff)
    )
    MonoAst.Def(freshSym, spec, specializedExp, defn.loc)
  }

  /**
    * Specializes `exp0`, renaming variables according to `env0` and specializes
    * w.r.t. `subst`.
    *
    * Replaces every polymorphic function reference with a reference to a specialized version. If a
    * specialized version of a function does not yet exist, a fresh symbol is created for it,
    *
    * Replaces every local variable symbol with a fresh local variable symbol.
    */
  private def specializeExp(exp0: LoweredAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: LoweredAst.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      MonoAst.Expr.Var(env0(sym), subst(tpe), loc)

    case LoweredAst.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, subst(tpe), loc)

    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val (p, env1) = specializeFormalParam(fparam, subst)
      val e = specializeExp(exp, env0 ++ env1, subst)
      MonoAst.Expr.Lambda(p, e, subst(tpe), loc)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyAtomic(op, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      MonoAst.Expr.ApplyClo(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val it = subst(itpe)
      val newSym = specializeDefnSym(sym, it)
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, it, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.ApplySig(sym, exps, itpe, tpe, eff, loc) =>
      val it = subst(itpe)
      val newSym = specializeSigSym(sym, it)
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, it, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val newSym = env0(sym)
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      val ef = subst(eff)
      MonoAst.Expr.ApplyLocalDef(newSym, es, t, ef, loc)

    case LoweredAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env1, subst)
      MonoAst.Expr.Let(freshSym, e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      val (fps, env2) = specializeFormalParams(fparams, subst)
      val e1 = specializeExp(exp1, env1 ++ env2, subst)
      val e2 = specializeExp(exp2, env1, subst)
      val t = subst(tpe)
      val ef = subst(eff)
      MonoAst.Expr.LocalDef(freshSym, fps, e1, e2, t, ef, loc)

    case LoweredAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      MonoAst.Expr.Scope(freshSym, regionVar, specializeExp(exp, env1, subst), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val e3 = specializeExp(exp3, env0, subst)
      MonoAst.Expr.IfThenElse(e1, e2, e3, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      MonoAst.Expr.Stm(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      MonoAst.Expr.Discard(e, subst(eff), loc)

    case LoweredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val rs = rules map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val (p, env1) = specializePat(pat, subst)
          val extendedEnv = env0 ++ env1
          val g = guard.map(specializeExp(_, extendedEnv, subst))
          val b = specializeExp(body, extendedEnv, subst)
          MonoAst.MatchRule(p, g, b)
      }
      MonoAst.Expr.Match(specializeExp(exp, env0, subst), rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TypeMatch(exp, rules, tpe, _, loc) =>
      // Use the non-strict substitution to allow free type variables to match with anything.
      val expTpe = subst.nonStrict(exp.tpe)
      // Make the tvars in `exp`'s type rigid so that `Nil: List[x%123]` can only match `List[_]`
      val renv = expTpe.typeVars.foldLeft(RigidityEnv.empty) {
        case (acc, Type.Var(sym, _)) => acc.markRigid(sym)
      }
      ListOps.findMap(rules) {
        case LoweredAst.TypeMatchRule(sym, t, body0) =>
          // Try to unify.
          ConstraintSolver2.fullyUnify(expTpe, subst.nonStrict(t), Scope.Top, renv)(root.eqEnv, flix) match {
            // Types don't unify; just continue.
            case None => None
            // Types unify; use the substitution in the body.
            case Some(caseSubst) =>
              // Visit the base expression under the initial environment.
              val e = specializeExp(exp, env0, subst)
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              val subst1 = StrictSubstitution.mk(caseSubst @@ subst.nonStrict)
              // Visit the body under the extended environment.
              val body = specializeExp(body0, env1, subst1)
              val eff = Type.mkUnion(e.eff, body.eff, loc.asSynthetic)
              Some(MonoAst.Expr.Let(freshSym, e, body, subst1(tpe), subst1(eff), loc))
          }
      }.get // This is safe since the last case can always match.

    case LoweredAst.Expr.JvmReflection(exp, true, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val argType = e.tpe match {
        // Typing guarantees that `e.tpe` is `Proxy[?]`.
        case Type.Apply(Type.Cst(_, _), arg, _) => arg
        case _ => throw InternalCompilerException(s"Unexpected proxy type '${e.tpe}'", e.tpe.loc)
      }
      argType match {
        case Type.Char => mkJvmTypeTag("JvmChar", tpe, eff, loc)
        case Type.Bool => mkJvmTypeTag("JvmBool", tpe, eff, loc)
        case Type.Int8 => mkJvmTypeTag("JvmInt8", tpe, eff, loc)
        case Type.Int16 => mkJvmTypeTag("JvmInt16", tpe, eff, loc)
        case Type.Int32 => mkJvmTypeTag("JvmInt32", tpe, eff, loc)
        case Type.Int64 => mkJvmTypeTag("JvmInt64", tpe, eff, loc)
        case Type.Float32 => mkJvmTypeTag("JvmFloat32", tpe, eff, loc)
        case Type.Float64 => mkJvmTypeTag("JvmFloat64", tpe, eff, loc)
        case Type.Cst(_, _) => mkJvmTypeTag("JvmObject", tpe, eff, loc)
        case Type.Apply(_, _, _) => mkJvmTypeTag("JvmObject", tpe, eff, loc)
        case Type.Var(_, _) | Type.Alias(_, _, _, _) | Type.AssocType(_, _, _, _) |
             Type.JvmToType(_, _) | Type.JvmToEff(_, _) | Type.UnresolvedJvmType(_, _) =>
          throw InternalCompilerException(s"Unexpected type '${e.tpe}'", e.tpe.loc)
      }

    case LoweredAst.Expr.JvmReflection(exp, false, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      e.tpe match {
        case Type.Char => mkJvmValueTag("JvmChar", e, tpe, eff, loc)
        case Type.Bool => mkJvmValueTag("JvmBool", e, tpe, eff, loc)
        case Type.Int8 => mkJvmValueTag("JvmInt8", e, tpe, eff, loc)
        case Type.Int16 => mkJvmValueTag("JvmInt16", e, tpe, eff, loc)
        case Type.Int32 => mkJvmValueTag("JvmInt32", e, tpe, eff, loc)
        case Type.Int64 => mkJvmValueTag("JvmInt64", e, tpe, eff, loc)
        case Type.Float32 => mkJvmValueTag("JvmFloat32", e, tpe, eff, loc)
        case Type.Float64 => mkJvmValueTag("JvmFloat64", e, tpe, eff, loc)
        case Type.Cst(_, _) | Type.Apply(_, _, _) =>
          val castToObject = mkCast(e, Type.mkNative(classOf[java.lang.Object], loc), eff, loc)
          mkJvmValueTag("JvmObject", castToObject, tpe, eff, loc)
        case Type.Var(_, _) | Type.Alias(_, _, _, _) | Type.AssocType(_, _, _, _) |
             Type.JvmToType(_, _) | Type.JvmToEff(_, _) | Type.UnresolvedJvmType(_, _) =>
          throw InternalCompilerException(s"Unexpected type '${e.tpe}'", e.tpe.loc)
      }

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.VectorLit(es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      MonoAst.Expr.VectorLoad(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.VectorLength(exp, loc) =>
      val e = specializeExp(exp, env0, subst)
      MonoAst.Expr.VectorLength(e, loc)

    case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      MonoAst.Expr.Ascribe(e, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
      // Drop the declaredType and declaredEff.
      val e = specializeExp(exp, env0, subst)
      mkCast(e, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.CatchRule(sym, clazz, body) =>
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          val b = specializeExp(body, env1, subst)
          MonoAst.CatchRule(freshSym, clazz, b)
      }
      MonoAst.Expr.TryCatch(e, rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.RunWith(exp, effect, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.HandlerRule(op, fparams0, body0) =>
          val (fparams, fparamEnv) = specializeFormalParams(fparams0, subst)
          val env1 = env0 ++ fparamEnv
          val body = specializeExp(body0, env1, subst)
          MonoAst.HandlerRule(op, fparams, body)
      }
      MonoAst.Expr.RunWith(e, effect, rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.Do(op, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map(specializeJvmMethod(_, env0, subst))
      MonoAst.Expr.NewObject(name, clazz, subst(tpe), subst(eff), methods, loc)

  }

  private def mkJvmValueTag(tagName: String, e: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): MonoAst.Expr = {
    val jvmTypeSym = Symbol.mkEnumSym(Name.NName(Nil, loc), Name.Ident("JvmValue", loc))
    val jvmCharSym = Symbol.mkCaseSym(jvmTypeSym, Name.Ident(tagName, loc))
    val op = AtomicOp.Tag(jvmCharSym)
    MonoAst.Expr.ApplyAtomic(op, List(e), tpe, eff, loc)
  }

  private def mkJvmTypeTag(tagName: String, tpe: Type, eff: Type, loc: SourceLocation): MonoAst.Expr = {
    val jvmTypeSym = Symbol.mkEnumSym(Name.NName(Nil, loc), Name.Ident("JvmType", loc))
    val jvmCharSym = Symbol.mkCaseSym(jvmTypeSym, Name.Ident(tagName, loc))
    val op = AtomicOp.Tag(jvmCharSym)
    MonoAst.Expr.ApplyAtomic(op, Nil, tpe, eff, loc)
  }

  /**
    * Returns the cast of `e` to `tpe` and `eff`.
    *
    * If `exp` and `tpe` is bytecode incompatible, a runtime crash is inserted to appease the
    * bytecode verifier.
    */
  private def mkCast(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): MonoAst.Expr = {
    (exp.tpe, tpe) match {
      case (Type.Char, Type.Char) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Char, Type.Int32) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Bool, Type.Bool) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int8, Type.Int8) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int16, Type.Int16) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int32, Type.Int32) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int64, Type.Int64) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Float32, Type.Float32) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Float64, Type.Float64) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (x, y) if !isPrimType(x) && !isPrimType(y) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (x, y) =>
        val crash = MonoAst.Expr.ApplyAtomic(AtomicOp.CastError(erasedString(x), erasedString(y)), Nil, tpe, eff, loc)
        MonoAst.Expr.Stm(exp, crash, tpe, eff, loc)
    }
  }

  /**
    * Returns `true` if `tpe` is a primitive type.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def isPrimType(tpe: Type): Boolean = tpe match {
    case Type.Char => true
    case Type.Bool => true
    case Type.Int8 => true
    case Type.Int16 => true
    case Type.Int32 => true
    case Type.Int64 => true
    case Type.Float32 => true
    case Type.Float64 => true
    case Type.Cst(_, _) => false
    case Type.Apply(_, _, _) => false
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
  }

  /**
    * Returns the erased string representation of `tpe`
    *
    * N.B.: `tpe` must be normalized.
    */
  private def erasedString(tpe: Type): String = tpe match {
    case Type.Char => "Char"
    case Type.Bool => "Bool"
    case Type.Int8 => "Int8"
    case Type.Int16 => "Int16"
    case Type.Int32 => "Int32"
    case Type.Int64 => "Int64"
    case Type.Float32 => "Float32"
    case Type.Float64 => "Float64"
    case Type.Cst(_, _) => "Object"
    case Type.Apply(_, _, _) => "Object"
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
  }

  /**
    * Specializes `p0` w.r.t. `subst` and returns a mapping from variable symbols to fresh variable
    * symbols.
    */
  private def specializePat(p0: LoweredAst.Pattern, subst: StrictSubstitution)(implicit root: LoweredAst.Root, flix: Flix): (MonoAst.Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = p0 match {
    case LoweredAst.Pattern.Wild(tpe, loc) =>
      (MonoAst.Pattern.Wild(subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      (MonoAst.Pattern.Var(freshSym, subst(tpe), loc), Map(sym -> freshSym))
    case LoweredAst.Pattern.Cst(cst, tpe, loc) => (MonoAst.Pattern.Cst(cst, subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, envs) = pats.map(specializePat(_, subst)).unzip
      (MonoAst.Pattern.Tag(sym, ps, subst(tpe), loc), combineEnvs(envs))
    case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
      val (ps, envs) = elms.map(specializePat(_, subst)).unzip
      (MonoAst.Pattern.Tuple(ps, subst(tpe), loc), combineEnvs(envs))
    case LoweredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, envs) = pats.map {
        case LoweredAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val (p1, env1) = specializePat(pat1, subst)
          (MonoAst.Pattern.Record.RecordLabelPattern(label, p1, subst(tpe1), loc1), env1)
      }.unzip
      val (p, env1) = specializePat(pat, subst)
      val finalEnv = env1 :: envs
      (MonoAst.Pattern.Record(ps, p, subst(tpe), loc), combineEnvs(finalEnv))
  }

  /** Specializes `method` w.r.t. `subst`. */
  private def specializeJvmMethod(method: LoweredAst.JvmMethod, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: LoweredAst.Root, flix: Flix): MonoAst.JvmMethod = method match {
    case LoweredAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
      val (fparams, env1) = specializeFormalParams(fparams0, subst)
      val exp = specializeExp(exp0, env0 ++ env1, subst)
      MonoAst.JvmMethod(ident, fparams, exp, subst(tpe), subst(eff), loc)
  }

  /**
    * Specializes `sym` w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def specializeDefnSym(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    val defn = root.defs(sym)

    if (defn.spec.tparams.isEmpty) {
      defn.sym
    } else {
      specializeDefCallsite(defn, tpe)
    }
  }

  /**
    * Resolves and specializes `sym` w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def specializeSigSym(sym: Symbol.SigSym, tpe: Type)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    val defn = resolveSigSym(sym, tpe)
    specializeDefCallsite(defn, tpe)
  }

  /**
    * Returns the concrete function that `sym` resolves to w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def resolveSigSym(sym: Symbol.SigSym, tpe: Type)(implicit instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: LoweredAst.Root, flix: Flix): LoweredAst.Def = {
    val sig = root.sigs(sym)
    val trt = root.traits(sym.trt)

    // Find out what instance to use by unifying with the sig type.
    val subst = ConstraintSolver2.fullyUnify(sig.spec.declaredScheme.base, tpe, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix).get
    val traitType = subst.m(trt.tparam.sym)
    val tyCon = traitType.typeConstructor.get

    val instance = instances((sym.trt, tyCon))
    val defns = instance.defs.filter(_.sym.text == sig.sym.name)

    (sig.exp, defns) match {
      // An instance implementation exists. Use it.
      case (_, defn :: Nil) => defn
      // No instance implementation, but a default implementation exists. Use it.
      case (Some(impl), Nil) =>
        val ns = sig.sym.trt.namespace :+ sig.sym.trt.name
        val defnSym = new Symbol.DefnSym(None, ns, sig.sym.name, sig.sym.loc)
        LoweredAst.Def(defnSym, sig.spec, impl, sig.loc)
      // Multiple matching defs. Should have been caught previously.
      case (_, _ :: _ :: _) => throw InternalCompilerException(s"Expected at most one matching definition for '$sym', but found ${defns.size} signatures.", sym.loc)
      // No matching defs and no default. Should have been caught previously.
      case (None, Nil) => throw InternalCompilerException(s"No default or matching definition found for '$sym'.", sym.loc)
    }
  }

  /**
    * Returns a function reference to the specialization of `defn` w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def specializeDefCallsite(defn: LoweredAst.Def, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    // Unify the declared and actual type to obtain the substitution map.
    val subst = infallibleUnify(defn.spec.declaredScheme.base, tpe, defn.sym)

    // Check whether the function definition has already been specialized.
    ctx synchronized {
      ctx.getSpecializedName(defn.sym, tpe) match {
        case None =>
          // The function has not been specialized.
          // Generate a fresh specialized definition symbol.
          val freshSym = Symbol.freshDefnSym(defn.sym)

          // Register the fresh symbol (and actual type).
          ctx.addSpecializedName(defn.sym, tpe, freshSym)

          // Enqueue the fresh symbol with the definition and substitution.
          ctx.enqueueSpecialization(freshSym, defn, subst)

          // Now simply refer to the freshly generated symbol.
          freshSym
        case Some(specializedSym) =>
          // The function has already been specialized.
          // Simply refer to the already existing specialized symbol.
          specializedSym
      }
    }

  }

  /**
    * Returns the combined map of `envs`.
    *
    * This is equivalent to `envs.reduce(_ ++ _)` without crashing on empty lists.
    */
  private def combineEnvs(envs: Iterable[Map[Symbol.VarSym, Symbol.VarSym]]): Map[Symbol.VarSym, Symbol.VarSym] =
    envs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)

  /**
    * Specializes `fparams0` w.r.t. `subst0` and returns a mapping from variable symbols to fresh
    * variable symbols.
    */
  private def specializeFormalParams(fparams0: List[LoweredAst.FormalParam], subst0: StrictSubstitution)(implicit root: LoweredAst.Root, flix: Flix): (List[MonoAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    // Specialize each formal parameter and recombine the results.
    val (params, envs) = fparams0.map(p => specializeFormalParam(p, subst0)).unzip
    (params, combineEnvs(envs))
  }

  /**
    * Specializes `fparam0` w.r.t. `subst0` and returns an environment mapping the variable symbol
    * to a fresh variable symbol.
    */
  private def specializeFormalParam(fparam0: LoweredAst.FormalParam, subst0: StrictSubstitution)(implicit root: LoweredAst.Root, flix: Flix): (MonoAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
    val LoweredAst.FormalParam(sym, mod, tpe, src, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    (MonoAst.FormalParam(freshSym, mod, subst0(tpe), src, loc), Map(sym -> freshSym))
  }

  /** Unifies `tpe1` and `tpe2` which must be unifiable. */
  private def infallibleUnify(tpe1: Type, tpe2: Type, sym: Symbol.DefnSym)(implicit root: LoweredAst.Root, flix: Flix): StrictSubstitution = {
    ConstraintSolver2.fullyUnify(tpe1, tpe2, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix) match {
      case Some(subst) =>
        StrictSubstitution.mk(subst)
      case None =>
        throw InternalCompilerException(s"Unable to unify: '$tpe1' and '$tpe2'.\nIn '$sym'", tpe1.loc)
    }
  }

  /** Reduces the given associated into its definition, will crash if not able to. */
  private def reduceAssocType(assoc: Type.AssocType)(implicit root: LoweredAst.Root, flix: Flix): Type = {
    // Since assoc is ground, `scope` will be unused.
    val scope = Scope.Top
    // Since assoc is ground, `renv` will be unused.
    val renv = RigidityEnv.empty
    val progress = Progress()

    val res = TypeReduction2.reduce(assoc, scope, renv)(progress, root.eqEnv, flix)

    if (progress.query()) res
    else throw InternalCompilerException(s"Could not reduce associated type $assoc", assoc.loc)
  }

  /**
    * Removes [[Type.Alias]] and [[Type.AssocType]], or crashes if some [[Type.AssocType]] is not
    * reducible.
    *
    * @param isGround If true, then `tpe` will be normalized.
    */
  private def simplify(tpe: Type, isGround: Boolean)(implicit root: LoweredAst.Root, flix: Flix): Type = tpe match {
    case v@Type.Var(_, _) => v
    case c@Type.Cst(_, _) => c
    case app@Type.Apply(_, _, _) => normalizeApply(simplify(_, isGround), app, isGround)
    case Type.Alias(_, _, t, _) => simplify(t, isGround)
    case Type.AssocType(symUse, arg0, kind, loc) =>
      val arg = simplify(arg0, isGround)
      val assoc = Type.AssocType(symUse, arg, kind, loc)
      val t = reduceAssocType(assoc)
      simplify(t, isGround)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
  }

  /**
    * Applies `normalize` on both sides of the application, then simplifies the remaining type.
    *
    * @param isGround If true then `app` will be normalized.
    * @param normalize Must not output [[Type.AssocType]] or [[Type.Alias]]. If `isGround` is true
    *                  then `normalize` should also normalize the type.
    */
  @inline
  private def normalizeApply(normalize: Type => Type, app: Type.Apply, isGround: Boolean): Type = {
    val Type.Apply(tpe1, tpe2, loc) = app
    (normalize(tpe1), normalize(tpe2)) match {
      // Simplify effect equations.
      case (x, y) if isGround && app.kind == Kind.Eff => canonicalEffect(Type.Apply(x, y, loc))
      case (Type.Cst(TypeConstructor.Complement, _), y) => Type.mkComplement(y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y) => Type.mkUnion(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y) => Type.mkIntersection(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y) => Type.mkDifference(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y) => Type.mkSymmetricDiff(x, y, loc)

      // Simplify case equations.
      case (Type.Cst(TypeConstructor.CaseComplement(sym), _), y) => Type.mkCaseComplement(y, sym, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, _), y) => Type.mkCaseIntersection(x, y, sym, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, _), y) => Type.mkCaseUnion(x, y, sym, loc)

      // Put records in alphabetical order.
      case (Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), tpe, _), rest) =>
        mkRecordExtendSorted(label, tpe, rest, loc)

      // Put schemas in alphabetical order.
      case (Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label), _), tpe, _), rest) =>
        mkSchemaExtendSorted(label, tpe, rest, loc)

      case (x, y) =>
        // Maintain and exploit reference equality for performance.
        app.renew(x, y, loc)
    }
  }

  /** Returns the canonical effect equivalent to `eff`. */
  private def canonicalEffect(eff: Type): Type =
    coSetToType(eval(eff), eff.loc)

  /**
    * Evaluates `eff`.
    *
    * N.B.: `eff` must be simplified and ground.
    */
  private def eval(eff: Type): CofiniteSet[Symbol.EffectSym] = eff match {
    case Type.Univ => CofiniteSet.universe
    case Type.Pure => CofiniteSet.empty
    case Type.Cst(TypeConstructor.Effect(sym), _) =>
      CofiniteSet.mkSet(sym)
    case Type.Cst(TypeConstructor.Region(_), _) =>
      CofiniteSet.mkSet(RegionInstantiation.sym)
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), y, _) =>
      CofiniteSet.complement(eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y, _) =>
      CofiniteSet.union(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y, _) =>
      CofiniteSet.intersection(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y, _) =>
      CofiniteSet.difference(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y, _) =>
      CofiniteSet.xor(eval(x), eval(y))
    case other => throw InternalCompilerException(s"Unexpected effect $other", other.loc)
  }

  /** Returns the [[Type]] representation of `set` with `loc`. */
  private def coSetToType(set: CofiniteSet[Symbol.EffectSym], loc: SourceLocation): Type = set match {
    case CofiniteSet.Set(s) => Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc)
    case CofiniteSet.Compl(s) => Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc), loc)
  }

  /**
    * Returns the default type of `tpe0`, depending on its kind.
    *
    * A default type is the choice we make for unconstrained type variables. Any choice will result
    * in a well-typed program since the typer did not constrain them.
    *
    * All defaults are normalized types.
    */
  private def default(tpe0: Type): Type = tpe0.kind match {
    case Kind.Wild => Type.mkAnyType(tpe0.loc)
    case Kind.WildCaseSet => Type.mkAnyType(tpe0.loc)
    case Kind.Star => Type.mkAnyType(tpe0.loc)
    case Kind.Eff => Type.Pure
    case Kind.Bool => Type.mkAnyType(tpe0.loc)
    case Kind.RecordRow => Type.RecordRowEmpty
    case Kind.SchemaRow => Type.SchemaRowEmpty
    case Kind.Predicate => Type.mkAnyType(tpe0.loc)
    case Kind.CaseSet(sym) => Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym), tpe0.loc)
    case Kind.Arrow(_, _) => Type.mkAnyType(tpe0.loc)
    case Kind.Jvm => throw InternalCompilerException(s"Unexpected type: '$tpe0'.", tpe0.loc)
    case Kind.Error => throw InternalCompilerException(s"Unexpected type '$tpe0'.", tpe0.loc)
  }

}
