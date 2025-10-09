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
import ca.uwaterloo.flix.language.ast.MonoAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.shared.{Constant, Scope}
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
  * Additionally it resolves trait methods into actual function calls.
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
  *     specialized to.
  *   - 2. We populate the queue by specialization of non-parametric function definitions.
  *   - 3. We iteratively extract a function from the queue and specialize it:
  *      - a. We replace every type variable appearing anywhere in the definition by its concrete
  *        type.
  *      - b. We create new fresh local variable symbols (since the function is effectively being
  *        copied).
  *      - c. We enqueue (or re-use) other functions referenced by the current function which require
  *        specialization.
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

  type Instances = Map[(Symbol.TraitSym, TypeConstructor), Instance]

  /** The effect that all [[TypeConstructor.Region]] are instantiated to. */
  private val RegionInstantiation: TypeConstructor.Effect =
    TypeConstructor.Effect(Symbol.IO, Kind.Eff)

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
    implicit val is: Instances = mkInstanceMap(root.instances)
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
  private def mkInstanceMap(instances: ListMap[Symbol.TraitSym, Instance]): Instances = {
    instances.map {
      case (sym, inst) => ((sym, inst.tpe.typeConstructor.get), inst)
    }.toMap
  }

  /** Converts `field`, simplifying its polymorphic type. */
  def visitStructField(field: LoweredAst.StructField)(implicit root: LoweredAst.Root, flix: Flix): MonoAst.StructField = {
    field match {
      case LoweredAst.StructField(fieldSym, tpe, loc) =>
        MonoAst.StructField(fieldSym, simplify(DatalogCodeGen.visitType(tpe), isGround = false), loc)
    }
  }

  /** Converts `caze`, simplifying its polymorphic type. */
  def visitEnumCase(caze: LoweredAst.Case)(implicit root: LoweredAst.Root, flix: Flix): MonoAst.Case = {
    caze match {
      case LoweredAst.Case(sym, tpes, _, loc) =>
        MonoAst.Case(sym, tpes.map(DatalogCodeGen.visitType).map(simplify(_, isGround = false)), loc)
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
          case LoweredAst.FormalParam(varSym, tpe, fpLoc) =>
            MonoAst.FormalParam(varSym, StrictSubstitution.empty(tpe), Occur.Unknown, fpLoc)
        }
        val spec = MonoAst.Spec(doc, ann, mod, fparams, declaredScheme.base, StrictSubstitution.empty(retTpe), StrictSubstitution.empty(eff), DefContext.Unknown)
        MonoAst.Op(sym, spec, loc)
    }

  /** Returns a specialization of `defn` with the name `freshSym` according to `subst`. */
  private def specializeDef(freshSym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Def = {
    val (specializedFparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

    val specializedExp = specializeExp(defn.exp, env0, subst)

    val spec0 = defn.spec
    val spec = MonoAst.Spec(
      spec0.doc,
      spec0.ann,
      spec0.mod,
      specializedFparams,
      DatalogCodeGen.visitType(subst(defn.spec.declaredScheme.base)),
      DatalogCodeGen.visitType(subst(spec0.retTpe)),
      subst(spec0.eff),
      DefContext.Unknown
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
  private def specializeExp(exp0: LoweredAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      MonoAst.Expr.Var(env0(sym), DatalogCodeGen.visitType(subst(tpe)), loc)

    case LoweredAst.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, subst(tpe), loc)

    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val (p, env1) = specializeFormalParam(fparam, subst)
      val e = specializeExp(exp, env0 ++ env1, subst)
      MonoAst.Expr.Lambda(p, e, DatalogCodeGen.visitType(subst(tpe)), loc)

    case LoweredAst.Expr.ApplyAtomic(AtomicOp.InstanceOf(clazz), exps, tpe, eff, loc) =>
      // In bytecode, instanceof can only be called on reference types
      val es = exps.map(specializeExp(_, env0, subst))
      val List(e) = es
      if (isPrimType(e.tpe)) {
        // If it's a primitive type, evaluate the expression but return false
        MonoAst.Expr.Stm(e, MonoAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc), Type.Bool, e.eff, loc)
      } else {
        // If it's a reference type, then do the instanceof check
        MonoAst.Expr.ApplyAtomic(AtomicOp.InstanceOf(clazz), es, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)
      }

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyAtomic(op, es, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      MonoAst.Expr.ApplyClo(e1, e2, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.ApplyDef(sym, exps, _, itpe, tpe, eff, loc) =>
      val it = subst(itpe)
      val newSym = specializeDefnSym(sym, it)
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, it, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val newSym = env0(sym)
      val es = exps.map(specializeExp(_, env0, subst))
      val t = DatalogCodeGen.visitType(subst(tpe))
      val ef = subst(eff)
      MonoAst.Expr.ApplyLocalDef(newSym, es, t, ef, loc)

    case LoweredAst.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyOp(sym, es, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.ApplySig(sym, exps, _, _, itpe, tpe, eff, loc) =>
      val it = subst(itpe)
      val newSym = specializeSigSym(sym, it)
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, DatalogCodeGen.visitType(it), DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env1, subst)
      MonoAst.Expr.Let(freshSym, e1, e2, DatalogCodeGen.visitType(subst(tpe)), subst(eff), Occur.Unknown, loc)

    case LoweredAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      val (fps, env2) = specializeFormalParams(fparams, subst)
      val e1 = specializeExp(exp1, env1 ++ env2, subst)
      val e2 = specializeExp(exp2, env1, subst)
      val t = DatalogCodeGen.visitType(subst(tpe))
      val ef = subst(eff)
      MonoAst.Expr.LocalDef(freshSym, fps, e1, e2, t, ef, Occur.Unknown, loc)

    case LoweredAst.Expr.Region(sym, regionVar, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      MonoAst.Expr.Region(freshSym, regionVar, specializeExp(exp, env1, subst), DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val e3 = specializeExp(exp3, env0, subst)
      MonoAst.Expr.IfThenElse(e1, e2, e3, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      MonoAst.Expr.Stm(e1, e2, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

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
      MonoAst.Expr.Match(specializeExp(exp, env0, subst), rs, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules.map {
        case LoweredAst.ExtMatchRule(pat, exp1, loc1) =>
          val (p, env1) = specializeExtPat(pat, subst)
          val extendedEnv = env0 ++ env1
          val e1 = specializeExp(exp1, extendedEnv, subst)
          MonoAst.ExtMatchRule(p, e1, loc1)
      }
      MonoAst.Expr.ExtMatch(e, rs, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

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
              Some(MonoAst.Expr.Let(freshSym, e, body, DatalogCodeGen.visitType(subst1(tpe)), subst1(eff), Occur.Unknown, loc))
          }
      }.get // This is safe since the last case can always match.

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      MonoAst.Expr.VectorLit(es, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      MonoAst.Expr.VectorLoad(e1, e2, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.VectorLength(exp, loc) =>
      val e = specializeExp(exp, env0, subst)
      MonoAst.Expr.VectorLength(e, loc)

    case LoweredAst.Expr.Ascribe(exp, _, _, _) =>
      specializeExp(exp, env0, subst)

    case LoweredAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
      // Drop the declaredType and declaredEff.
      val e = specializeExp(exp, env0, subst)
      mkCast(e, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.CatchRule(sym, clazz, body) =>
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          val b = specializeExp(body, env1, subst)
          MonoAst.CatchRule(freshSym, clazz, b)
      }
      MonoAst.Expr.TryCatch(e, rs, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.RunWith(exp, effSymUse, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.HandlerRule(opSymUse, fparams0, body0) =>
          val (fparams, fparamEnv) = specializeFormalParams(fparams0, subst)
          val env1 = env0 ++ fparamEnv
          val body = specializeExp(body0, env1, subst)
          MonoAst.HandlerRule(opSymUse, fparams, body)
      }
      MonoAst.Expr.RunWith(e, effSymUse, rs, DatalogCodeGen.visitType(subst(tpe)), subst(eff), loc)

    case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map(specializeJvmMethod(_, env0, subst))
      MonoAst.Expr.NewObject(name, clazz, DatalogCodeGen.visitType(subst(tpe)), subst(eff), methods, loc)

    case LoweredAst.Expr.FixpointConstraintSet(cs, _, loc) =>
      DatalogCodeGen.mkDatalog(cs, loc, env0, subst)

    case LoweredAst.Expr.FixpointLambda(pparams, exp, _, eff, loc) =>
      DatalogCodeGen.mkDatalogLambda(pparams, exp, subst(eff), loc, env0, subst)

    case LoweredAst.Expr.FixpointMerge(exp1, exp2, _, eff, loc) =>
      DatalogCodeGen.mkMerge(exp1, exp2, subst(eff), loc, env0, subst)

    case LoweredAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tpe, eff, loc) =>
      DatalogCodeGen.mkFixpointQueryWithProvenance(exps, select, withh, tpe, subst(eff), loc, env0, subst)

    case LoweredAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, _, _, pred, tpe, eff, loc) =>
      DatalogCodeGen.mkFixpointQueryWithSelect(exps, queryExp, selects, pred, tpe, subst(eff), loc, env0, subst)

    case LoweredAst.Expr.FixpointSolveWithProject(exps0, optPreds, mode, _, eff, loc) =>
      DatalogCodeGen.mkFixpointSolveWithProject(exps0, optPreds, mode, subst(eff), loc, env0, subst)

    case LoweredAst.Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
      DatalogCodeGen.mkFixpointInjectInto(exps, predsAndArities, loc, env0, subst)

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
      case (Type.Char, Type.Int16) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int16, Type.Char) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
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
      (MonoAst.Pattern.Var(freshSym, subst(tpe), Occur.Unknown, loc), Map(sym -> freshSym))
    case LoweredAst.Pattern.Cst(cst, tpe, loc) => (MonoAst.Pattern.Cst(cst, subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val (ps, envs) = pats.map(specializePat(_, subst)).unzip
      (MonoAst.Pattern.Tag(symUse, ps, subst(tpe), loc), combineEnvs(envs))
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

  /**
    * Specializes `pat0` w.r.t. `subst` and returns a mapping from variable symbols to fresh variable
    * symbols.
    */
  private def specializeExtPat(pat0: LoweredAst.ExtPattern, subst: StrictSubstitution)(implicit root: LoweredAst.Root, flix: Flix): (MonoAst.ExtPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case LoweredAst.ExtPattern.Default(loc) =>
      (MonoAst.ExtPattern.Default(loc), Map.empty)

    case LoweredAst.ExtPattern.Tag(label, pats, loc) =>
      val (ps, symMaps) = pats.map(specializeExtTagPat(_, subst)).unzip
      val env = symMaps.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (MonoAst.ExtPattern.Tag(label, ps, loc), env)
  }

  /**
    * Specializes `pat0` w.r.t. `subst` and returns a mapping from variable symbols to fresh variable
    * symbols.
    */
  private def specializeExtTagPat(pat0: LoweredAst.ExtTagPattern, subst: StrictSubstitution)(implicit root: LoweredAst.Root, flix: Flix): (MonoAst.ExtTagPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case LoweredAst.ExtTagPattern.Wild(tpe, loc) =>
      (MonoAst.ExtTagPattern.Wild(subst(tpe), loc), Map.empty)

    case LoweredAst.ExtTagPattern.Var(sym, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      (MonoAst.ExtTagPattern.Var(freshSym, subst(tpe), Occur.Unknown, loc), Map(sym -> freshSym))

    case LoweredAst.ExtTagPattern.Unit(tpe, loc) =>
      (MonoAst.ExtTagPattern.Unit(subst(tpe), loc), Map.empty)
  }

  /** Specializes `method` w.r.t. `subst`. */
  private def specializeJvmMethod(method: LoweredAst.JvmMethod, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.JvmMethod = method match {
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
  private def specializeSigSym(sym: Symbol.SigSym, tpe: Type)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    val defn = resolveSigSym(sym, tpe)
    specializeDefCallsite(defn, tpe)
  }

  /**
    * Returns the concrete function that `sym` resolves to w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def resolveSigSym(sym: Symbol.SigSym, tpe: Type)(implicit instances: Instances, root: LoweredAst.Root, flix: Flix): LoweredAst.Def = {
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
    val LoweredAst.FormalParam(sym, tpe, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    (MonoAst.FormalParam(freshSym, DatalogCodeGen.visitType(subst0(tpe)), Occur.Unknown, loc), Map(sym -> freshSym))
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
  private def eval(eff: Type): CofiniteSet[Symbol.EffSym] = eff match {
    case Type.Univ => CofiniteSet.universe
    case Type.Pure => CofiniteSet.empty
    case Type.Cst(TypeConstructor.Effect(sym, _), _) =>
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
  private def coSetToType(set: CofiniteSet[Symbol.EffSym], loc: SourceLocation): Type = set match {
    case CofiniteSet.Set(s) => Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc)), loc)
    case CofiniteSet.Compl(s) => Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc)), loc), loc)
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

  private object DatalogCodeGen {
    import ca.uwaterloo.flix.language.ast.ops.LoweredAstOps
    import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, DatalogDefs, Denotation, Fixity, Polarity, PredicateAndArity, SolveMode, SymUse}
    /**
      * Returns the definition associated with the given symbol `sym`.
      *
      * @param tpe must be subst. Can be visited, if the underlying function can handle that
      */
    private def lookup(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym =
      specializeDefnSym(sym, tpe)

    private object Enums {
      lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Datalog")
      lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Constraint")

      lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.HeadPredicate")
      lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.BodyPredicate")

      lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.HeadTerm")
      lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.BodyTerm")

      lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Shared.PredSym")
      lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.VarSym")

      lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Shared.Denotation")
      lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Polarity")
      lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Ast.Datalog.Fixity")

      lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${DatalogDefs.version}.Boxed")

      lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")
    }

    private object Types {
      //
      // Data Types
      //
      lazy val Datalog: Type = Type.mkEnum(Enums.Datalog, Nil, SourceLocation.Unknown)
      lazy val Constraint: Type = Type.mkEnum(Enums.Constraint, Nil, SourceLocation.Unknown)

      lazy val HeadPredicate: Type = Type.mkEnum(Enums.HeadPredicate, Nil, SourceLocation.Unknown)
      lazy val BodyPredicate: Type = Type.mkEnum(Enums.BodyPredicate, Nil, SourceLocation.Unknown)

      lazy val HeadTerm: Type = Type.mkEnum(Enums.HeadTerm, Nil, SourceLocation.Unknown)
      lazy val BodyTerm: Type = Type.mkEnum(Enums.BodyTerm, Nil, SourceLocation.Unknown)

      lazy val PredSym: Type = Type.mkEnum(Enums.PredSym, Nil, SourceLocation.Unknown)
      lazy val VarSym: Type = Type.mkEnum(Enums.VarSym, Nil, SourceLocation.Unknown)

      lazy val Denotation: Type = Type.mkEnum(Enums.Denotation, Boxed :: Nil, SourceLocation.Unknown)
      lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil, SourceLocation.Unknown)
      lazy val Fixity: Type = Type.mkEnum(Enums.Fixity, Nil, SourceLocation.Unknown)

      lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil, SourceLocation.Unknown)

      lazy val VectorOfBoxed: Type = Type.mkVector(Types.Boxed, SourceLocation.Unknown)

      def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)

      //
      // Function Types.
      //
      lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog, SourceLocation.Unknown)
      lazy val MergeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog, SourceLocation.Unknown)
      lazy val FilterType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog, SourceLocation.Unknown)
      lazy val RenameType: Type = Type.mkPureUncurriedArrow(List(mkList(PredSym, SourceLocation.Unknown), Datalog), Datalog, SourceLocation.Unknown)

      /**
        *
        * @param t must be subst
        * @return subst type
        */
      def mkProvenanceOf(t: Type, loc: SourceLocation): Type =
        Type.mkPureUncurriedArrow(
          List(
            PredSym,
            Type.mkVector(Boxed, loc),
            Type.mkVector(PredSym, loc),
            Type.mkPureCurriedArrow(List(PredSym, Type.mkVector(Boxed, loc)), t, loc),
            Datalog
          ),
          Type.mkVector(t, loc), loc
        )

    }

//    /** Unifies `tpe1` and `tpe2` which must be unifiable. */
//    private def infallibleUnifyDatalog(tpe1: Type, tpe2: Type)(implicit root: LoweredAst.Root, flix: Flix): Substitution = {
//      ConstraintSolver2.fullyUnify(tpe1, tpe2, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix) match {
//        case Some(subst) =>
//          subst
//        case None =>
//          throw InternalCompilerException(s"Unable to unify: '$tpe1' and '$tpe2'.", tpe1.loc)
//      }
//    }


    /**
      * Constructs a `Fixpoint/Ast/Datalog.Datalog` value from the given list of Datalog constraints `cs`.
      */
    def mkDatalog(cs: List[LoweredAst.Constraint], loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val factExps = cs.filter(c => c.body.isEmpty).map(c0 => visitConstraint(c0, env0, subst))
      val ruleExps = cs.filter(c => c.body.nonEmpty).map(c0 => visitConstraint(c0, env0, subst))

      val factListExp = mkVector(factExps, Types.Constraint, loc)
      val ruleListExp = mkVector(ruleExps, Types.Constraint, loc)

      val innerExp = List(factListExp, ruleListExp)
      mkTag(Enums.Datalog, "Datalog", innerExp, Types.Datalog, loc)
    }

    def mkMerge(exp1: LoweredAst.Expr, exp2: LoweredAst.Expr, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val defnSym = lookup(DatalogDefs.Merge, Types.MergeType)
      val argExps = specializeExp(exp1, env0, subst) :: specializeExp(exp2, env0, subst) :: Nil
      val resultType = Types.Datalog
      MonoAst.Expr.ApplyDef(defnSym, argExps, Types.MergeType, resultType, eff, loc)
    }

    def mkDatalogLambda(pparams: List[LoweredAst.PredicateParam], exp: LoweredAst.Expr, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val defnSym = lookup(DatalogDefs.Rename, Types.RenameType)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.PredSym, loc)
      val argExps = predExps :: specializeExp(exp, env0, subst) :: Nil
      val resultType = Types.Datalog
      MonoAst.Expr.ApplyDef(defnSym, argExps, Types.RenameType, resultType, eff, loc)
    }

    def mkFixpointQueryWithProvenance(exps: List[LoweredAst.Expr], select: LoweredAst.Predicate.Head, withh: List[Name.Pred], tpe: Type, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      // Create appropriate call to Fixpoint.Solver.provenanceOf. This requires creating a mapping, mkExtVar, from
      // PredSym and terms to an extensible variant.
      val mergedExp = mergeExps(exps.map(specializeExp(_, env0, subst)), loc)
      val (goalPredSym, goalTerms) = select match {
        case LoweredAst.Predicate.Head.Atom(pred, _, terms, _, loc1) =>
          val boxedTerms = terms.map(t => box(specializeExp(t, env0, subst), subst(t.tpe)))
          (mkPredSym(pred), mkVector(boxedTerms, Types.Boxed, loc1))
      }
      val withPredSyms = mkVector(withh.map(mkPredSym), Types.PredSym, loc)
      val extVarType = subst(unwrapVectorType(tpe, loc))
      val preds = predicatesOfExtVar(extVarType, loc)
      val lambdaExp = mkExtVarLambda(preds, visitType(extVarType), loc)
      val argExps = goalPredSym :: goalTerms :: withPredSyms :: lambdaExp :: mergedExp :: Nil
      val itpe = Types.mkProvenanceOf(extVarType, loc)
      // Note that `DatalogDefs.ProvenanceOf` is apparently fine with visited types
      val defnSym = lookup(DatalogDefs.ProvenanceOf, itpe)
      MonoAst.Expr.ApplyDef(defnSym, argExps, itpe, visitType(subst(tpe)), eff, loc)
    }

    def mkFixpointQueryWithSelect(exps: List[LoweredAst.Expr], queryExp: LoweredAst.Expr, selects: List[LoweredAst.Expr], pred: Name.Pred, tpe: Type, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
//      val substForExps = exps match {
//        case Nil => subst
//        case x :: xs => StrictSubstitution.mk(xs.foldLeft((x.tpe, subst.s)){
//          case ((tpe0, acc), exp) => (exp.tpe, acc ++ infallibleUnifyDatalog(exp.tpe, tpe0))
//        }._2)
//      }
//      println(s"${exps}")
      val loweredExps = exps.map(specializeExp(_, env0, subst))
      val loweredQueryExp = specializeExp(queryExp, env0, subst)

      // Compute the arity of the predicate symbol.
      val predArity = selects.length

      // Define the name and type of the appropriate factsX function in Solver.flix
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), subst(tpe), loc)
      // Note that `DatalogDefs.Facts` is *not* necessarily fine with a visited return type
      val sym = lookup(DatalogDefs.Facts(predArity), defTpe)

      // Merge and solve exps
      val mergedExp = mergeExps(loweredQueryExp :: loweredExps, loc)
      val solvedSym = lookup(DatalogDefs.Solve, Types.SolveType)
      val solvedExp = MonoAst.Expr.ApplyDef(solvedSym, mergedExp :: Nil, Types.SolveType, Types.Datalog, eff, loc)

      // Put everything together
      val argExps = mkPredSym(pred) :: solvedExp :: Nil
      MonoAst.Expr.ApplyDef(sym, argExps, visitType(defTpe), visitType(subst(tpe)), eff, loc)
    }

    def mkFixpointSolveWithProject(exps0: List[LoweredAst.Expr], optPreds: Option[List[Name.Pred]], mode: SolveMode, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      // Rewrites
      //     solve e₁, e₂, e₃ project P₁, P₂, P₃
      // to
      //     let tmp% = solve e₁ <+> e₂ <+> e₃;
      //     merge (project P₁ tmp%, project P₂ tmp%, project P₃ tmp%)
      //
      val defnSym = mode match {
        case SolveMode.Default => lookup(DatalogDefs.Solve, Types.SolveType)
        case SolveMode.WithProvenance => lookup(DatalogDefs.SolveWithProvenance, Types.Datalog)
      }
      val exps = exps0.map(specializeExp(_, env0, subst))
      val mergedExp = mergeExps(exps, loc)
      val argExps = mergedExp :: Nil
      val solvedExp = MonoAst.Expr.ApplyDef(defnSym, argExps, Types.SolveType, Types.Datalog, eff, loc)
      val tmpVarSym = Symbol.freshVarSym("tmp%", BoundBy.Let, loc)(Scope.Top, flix)
      val letBodyExp = optPreds match {
        case Some(preds) =>
          mergeExps(preds.map(pred => {
            val varExp = MonoAst.Expr.Var(tmpVarSym, Types.Datalog, loc)
            projectSym(mkPredSym(pred), varExp, loc)
          }), loc)
        case None => MonoAst.Expr.Var(tmpVarSym, Types.Datalog, loc)
      }
      MonoAst.Expr.Let(tmpVarSym, solvedExp, letBodyExp, Types.Datalog, eff, Occur.Unknown, loc)
    }

    def mkFixpointInjectInto(exps: List[LoweredAst.Expr], predsAndArities: List[PredicateAndArity], loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val loweredExps = exps.zip(predsAndArities).map {
        case (exp, PredicateAndArity(pred, _)) =>
          // Compute the types arguments of the functor F[(a, b, c)] or F[a].
          val (_, targs) = Type.eraseAliases(exp.tpe) match {
            case Type.Apply(tycon, innerType, _) => innerType.typeConstructor match {
              case Some(TypeConstructor.Tuple(_)) => (tycon, innerType.typeArguments)
              case Some(TypeConstructor.Unit) => (tycon, Nil)
              case _ => (tycon, List(innerType))
            }
            case _ => throw InternalCompilerException(s"Unexpected non-foldable type: '${exp.tpe}'.", loc)
          }

          val specialExp = specializeExp(exp, env0, subst)

          // Compute the symbol of the function.
          // The type of the function.
          val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, subst(exp.tpe)), Types.Datalog, loc)
          // Note that `DatalogDefs.ProjectInto` is *not* necessarily fine with a visited parameter, though the return type is fine.
          val sym = lookup(DatalogDefs.ProjectInto(targs.length), defTpe)


          // Put everything together.
          val argExps = mkPredSym(pred) :: specialExp :: Nil
          MonoAst.Expr.ApplyDef(sym, argExps, visitType(defTpe), Types.Datalog, specialExp.eff, loc)
      }
      mergeExps(loweredExps, loc)
    }
      /**
    * Lowers the given constraint `c0`.
    */
    private def visitConstraint(c0: LoweredAst.Constraint, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = c0 match {
      case LoweredAst.Constraint(cparams, head, body, loc) =>
        val headExp = visitHeadPred(cparams, env0, subst, head)
        val bodyExp = mkVector(body.map(visitBodyPred(cparams, env0, subst, _)), Types.BodyPredicate, loc)
        val innerExp = List(headExp, bodyExp)
        mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
    }

    /**
      * Lowers the given head predicate `p0`.
      */
    private def visitHeadPred(cparams0: List[LoweredAst.ConstraintParam], env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution, p0: LoweredAst.Predicate.Head)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = p0 match {
      case LoweredAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
        val predSymExp = mkPredSym(pred)
        val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc, subst)
        val termsExp = mkVector(terms.map(visitHeadTerm(cparams0, env0, subst, _)), Types.HeadTerm, loc)
        val innerExp = List(predSymExp, denotationExp, termsExp)
        mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
    }

    /**
      * Lowers the given body predicate `p0`.
      */
    private def visitBodyPred(cparams0: List[LoweredAst.ConstraintParam], env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution, p0: LoweredAst.Predicate.Body)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = p0 match {
      case LoweredAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
        val predSymExp = mkPredSym(pred)
        val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc, subst)
        val polarityExp = mkPolarity(polarity, loc)
        val fixityExp = mkFixity(fixity, loc)
        val termsExp = mkVector(terms.map(visitBodyTerm(cparams0, _, env0, subst)), Types.BodyTerm, loc)
        val innerExp = List(predSymExp, denotationExp, polarityExp, fixityExp, termsExp)
        mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

      case LoweredAst.Predicate.Body.Functional(outVars, exp0, loc) =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val inVars = quantifiedVars(cparams0, exp0).map {case (sym, tpe) => (sym, subst(tpe))}
        val newMappings = inVars.map {case (sym, _) => (sym, Symbol.freshVarSym(sym))}.toMap
        val exp = specializeExp(exp0, env0 ++ newMappings, subst)
        mkFunctional(outVars, inVars, exp, newMappings, loc)

      case LoweredAst.Predicate.Body.Guard(exp0, loc) =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val quantifiedFreeVars = quantifiedVars(cparams0, exp0).map {case (s, tpe) => (s, visitType(subst(tpe)))}
        val newMappings = quantifiedFreeVars.map {case (sym, _) => (sym, Symbol.freshVarSym(sym))}.toMap
        val exp = specializeExp(exp0, env0 ++ newMappings, subst)
        mkGuard(quantifiedFreeVars, newMappings, exp, loc)

    }

    /**
      * Lowers the given head term `exp0`.
      */
    private def visitHeadTerm(cparams0: List[LoweredAst.ConstraintParam], env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution, exp0: LoweredAst.Expr)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      //
      // We need to consider four cases:
      //
      // Case 1.1: The expression is quantified variable. We translate it to a Var.
      // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
      // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
      // Case 3: The expression contains quantified variables. We translate it to an application term.
      //
      exp0 match {
        case LoweredAst.Expr.Var(sym, _, _) =>
          // Case 1: Variable term.
          if (isQuantifiedVar(sym, cparams0)) {
            // Case 1.1: Quantified variable.
            mkHeadTermVar(sym)
          } else {
            // Case 1.2: Lexically bound variable.
            mkHeadTermLit(box(specializeExp(exp0, env0, subst), subst(exp0.tpe)))

          }
        case _ =>
          // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
          val quantifiedFreeVars = quantifiedVars(cparams0, exp0).map { case (sym, tpe0) => (sym, subst(tpe0))}

          if (quantifiedFreeVars.isEmpty) {
            // Case 2: No quantified variables. The expression can be reduced to a value.
            mkHeadTermLit(box(specializeExp(exp0, env0, subst), subst(exp0.tpe)))
          } else {
            // Case 3: Quantified variables. The expression is translated to an application term.
            mkAppTerm(quantifiedFreeVars, exp0, exp0.loc, env0, subst)
          }
      }
    }

    /**
      * Lowers the given body term `pat0`.
      */
    private def visitBodyTerm(cparams0: List[LoweredAst.ConstraintParam], pat0: LoweredAst.Pattern, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = pat0 match {
      case LoweredAst.Pattern.Wild(_, loc) =>
        mkBodyTermWild(loc)

      case LoweredAst.Pattern.Var(sym, tpe, loc) =>
        if (isQuantifiedVar(sym, cparams0)) {
          // Case 1: Quantified variable.
          mkBodyTermVar(sym)
        } else {
          // Case 2: Lexically bound variable *expression*.
          mkBodyTermLit(box(MonoAst.Expr.Var(env0(sym), subst(tpe), loc), subst(tpe)))
        }

      case LoweredAst.Pattern.Cst(cst, tpe, loc) =>
        mkBodyTermLit(box(MonoAst.Expr.Cst(cst, subst(tpe), loc), subst(tpe)))

      case LoweredAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

      case LoweredAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

      case LoweredAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Var` from the given variable symbol `sym`.
      */
    private def mkHeadTermVar(sym: Symbol.VarSym): MonoAst.Expr = {
      val innerExp = List(mkVarSym(sym))
      mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Lit` value which wraps the given expression `exp`.
      */
    private def mkHeadTermLit(exp: MonoAst.Expr): MonoAst.Expr = {
      mkTag(Enums.HeadTerm, "Lit", List(exp), Types.HeadTerm, exp.loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Wild` from the given source location `loc`.
      */
    private def mkBodyTermWild(loc: SourceLocation): MonoAst.Expr = {
      mkTag(Enums.BodyTerm, "Wild", Nil, Types.BodyTerm, loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Var` from the given variable symbol `sym`.
      */
    private def mkBodyTermVar(sym: Symbol.VarSym): MonoAst.Expr = {
      val innerExp = List(mkVarSym(sym))
      mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Lit` from the given expression `exp0`.
      */
    private def mkBodyTermLit(exp: MonoAst.Expr): MonoAst.Expr = {
      mkTag(Enums.BodyTerm, "Lit", List(exp), Types.BodyTerm, exp.loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Shared.Denotation` from the given denotation `d` and type `tpeOpt`
      * (which must be the optional type of the last term).
      */
    private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation, subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = d match {
      case Denotation.Relational =>
        mkTag(Enums.Denotation, "Relational", Nil, Types.Denotation, loc)

      case Denotation.Latticenal =>
        tpeOpt match {
          case None => throw InternalCompilerException("Unexpected nullary lattice predicate.", loc)
          case Some(tpe) =>
            val innerType = subst(tpe)
            // The type `Denotation[tpe]`.
            val unboxedDenotationType = Type.mkEnum(Enums.Denotation, innerType :: Nil, loc)

            // The type `Denotation[Boxed]`.
            val boxedDenotationType = Types.Denotation

            val latticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)
            val latticeSym: Symbol.DefnSym = lookup(DatalogDefs.lattice, latticeType)

            val boxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)
            val boxSym: Symbol.DefnSym = lookup(DatalogDefs.box, boxType)

            val innerApply = MonoAst.Expr.ApplyDef(latticeSym, List(MonoAst.Expr.Cst(Constant.Unit, Type.Unit, loc)), visitType(latticeType), unboxedDenotationType, Type.Pure, loc)
            MonoAst.Expr.ApplyDef(boxSym, List(innerApply), visitType(boxType), boxedDenotationType, Type.Pure, loc)
        }
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.Polarity` from the given polarity `p`.
      */
    private def mkPolarity(p: Polarity, loc: SourceLocation): MonoAst.Expr = p match {
      case Polarity.Positive =>
        mkTag(Enums.Polarity, "Positive", Nil, Types.Polarity, loc)

      case Polarity.Negative =>
        mkTag(Enums.Polarity, "Negative", Nil, Types.Polarity, loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.Fixity` from the given fixity `f`.
      */
    private def mkFixity(f: Fixity, loc: SourceLocation): MonoAst.Expr = f match {
      case Fixity.Loose =>
        mkTag(Enums.Fixity, "Loose", Nil, Types.Fixity, loc)

      case Fixity.Fixed =>
        mkTag(Enums.Fixity, "Fixed", Nil, Types.Fixity, loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Shared.PredSym` from the given predicate `pred`.
      */
    private def mkPredSym(pred: Name.Pred): MonoAst.Expr = pred match {
      case Name.Pred(sym, loc) =>
        val nameExp = MonoAst.Expr.Cst(Constant.Str(sym), Type.Str, loc)
        val idExp = MonoAst.Expr.Cst(Constant.Int64(0), Type.Int64, loc)
        val inner = List(nameExp, idExp)
        mkTag(Enums.PredSym, "PredSym", inner, Types.PredSym, loc)
    }

    /**
      * Constructs a `Fixpoint/Ast/Datalog.VarSym` from the given variable symbol `sym`.
      */
    private def mkVarSym(sym: Symbol.VarSym): MonoAst.Expr = {
      val nameExp = MonoAst.Expr.Cst(Constant.Str(sym.text), Type.Str, sym.loc)
      mkTag(Enums.VarSym, "VarSym", List(nameExp), Types.VarSym, sym.loc)
    }

    /**
      * Returns the given expression `exp` in a box.
      */

    /**
      * Returns the given expression `exp` in a box.
      *
      * @param exp: The expression to be boxed
      * @param tpe0: The substituted, unvisited type of `exp`
      * @return Expression boxing `exp`
      */
    private def box(exp: MonoAst.Expr, tpe0: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val loc = exp.loc
      val tpe = Type.mkPureArrow(tpe0, Types.Boxed, loc)
      val defnSym = lookup(DatalogDefs.Box, tpe)
      MonoAst.Expr.ApplyDef(defnSym, List(exp), visitType(tpe), Types.Boxed, Type.Pure, loc)
    }

    /**
      * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.GuardX`.
      */

    /**
      *
      * @param fvs: The types are expected to be substituted, but not visited.
      */
    private def mkGuard(fvs: List[(Symbol.VarSym, Type)], freshVars: Map[Symbol.VarSym, Symbol.VarSym], exp: MonoAst.Expr, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      // Compute the number of free variables.
      val arity = fvs.length

      // Check that we have <= 5 free variables.
      if (arity > 5) {
        throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
      }

      // Special case: No free variables.
      if (fvs.isEmpty) {
        val sym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, loc)(Scope.Top, flix)
        // Construct a lambda that takes the unit argument.
        val fparam = MonoAst.FormalParam(sym, Type.Unit, Occur.Unknown, loc)
        // Here it is okay to use the `monoExp.tpe` as it is only used to generate the `MonoAst.Expr`.
        val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
        val lambdaExp = MonoAst.Expr.Lambda(fparam, exp, tpe, loc)
        return mkTag(Enums.BodyPredicate, s"Guard0", List(lambdaExp), Types.BodyPredicate, loc)
      }

      // Curry `exp` in a lambda expression for each free variable.
      val lambdaExp = fvs.foldRight(exp) {
        case ((oldSym, tpe), acc) =>
          val freshSym = freshVars(oldSym)
          val fparam = MonoAst.FormalParam(freshSym, visitType(tpe), Occur.Unknown, loc)
          // Here it is okay to use the `monoExp.tpe` as it is only used to generate the `MonoAst.Expr`.
          val lambdaType = Type.mkPureArrow(visitType(tpe), acc.tpe, loc)
          MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
      }

      // Lift the lambda expression to operate on boxed values.
      val liftedExp = liftXb(lambdaExp, fvs.map(_._2))

      // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
      val varExps = fvs.map(kv => mkVarSym(kv._1))
      val innerExp = liftedExp :: varExps
      mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
    }

    /**
      * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.Functional`.
      */

    /**
      * @param inVars: Types must be subst, but not visited
      */
    private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, freshVars: Map[Symbol.VarSym, Symbol.VarSym], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      // Compute the number of in and out variables.
      val numberOfInVars = inVars.length
      val numberOfOutVars = outVars.length

      if (numberOfInVars > 5) {
        throw InternalCompilerException("Does not support more than 5 in variables.", loc)
      }
      if (numberOfOutVars == 0) {
        throw InternalCompilerException("Requires at least one out variable.", loc)
      }
      if (numberOfOutVars > 5) {
        throw InternalCompilerException("Does not support more than 5 out variables.", loc)
      }

      // Curry `freshExp` in a lambda expression for each free variable.
      val lambdaExp = inVars.foldRight(exp) {
        case ((oldSym, tpe), acc) =>
          val freshSym = freshVars(oldSym)
          val fparam = MonoAst.FormalParam(freshSym, visitType(tpe), Occur.Unknown, loc)
          // Here it is okay to use the `monoExp.tpe` as it is only used to generate the `MonoAst.Expr`.
          val lambdaType = Type.mkPureArrow(visitType(tpe), acc.tpe, loc)
          MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
      }

      // Lift the lambda expression to operate on boxed values.
      val liftedExp = liftXY(outVars, lambdaExp, inVars.map(t => visitType(t._2)), exp.tpe, exp.loc)

      // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
      val boundVarVector = mkVector(outVars.map(mkVarSym), Types.VarSym, loc)
      val freeVarVector = mkVector(inVars.map(kv => mkVarSym(kv._1)), Types.VarSym, loc)
      val innerExp = List(boundVarVector, liftedExp, freeVarVector)
      mkTag(Enums.BodyPredicate, s"Functional", innerExp, Types.BodyPredicate, loc)
    }

    /**
      * Returns a `Fixpoint/Ast/Datalog.HeadTerm.AppX`.
      */

    /**
      *
      * @param fvs the types in this must be subst, but not visited
      */
    private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Instances, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      // Compute the number of free variables.
      val arity = fvs.length

      // Check that we have <= 5 free variables.
      if (arity > 5) {
        throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
      }

      // Introduce a fresh variable for each free variable.
      val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
        case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
      }

      // Substitute every symbol in `exp` for its fresh equivalent.
      val freshExp = specializeExp(exp, env0 ++ freshVars, subst)

      // Curry `freshExp` in a lambda expression for each free variable.
      val lambdaExp = fvs.foldRight(freshExp) {
        case ((oldSym, tpe), acc) =>
          val freshSym = freshVars(oldSym)
          val fparam =  MonoAst.FormalParam(freshSym, tpe, Occur.Unknown, loc)
          val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
          MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
      }

      // Lift the lambda expression to operate on boxed values.
      val liftedExp = liftX(lambdaExp, fvs.map(_._2), subst(exp.tpe))

      // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
      val varExps = fvs.map(kv => mkVarSym(kv._1))
      val innerExp = liftedExp :: varExps
      mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
    }

    /**
      * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
      *
      * Note: liftX and liftXb are similar and should probably be maintained together.
      *
      * @param argTypes must be subst, but not visited
      * @param resultType must be subst, but not visited
      */
    private def liftX(exp0: MonoAst.Expr, argTypes: List[Type], resultType: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {

      //
      // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
      // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Boxed`.
      // That is, the function accepts a *curried* function and returns a *curried* function.
      //

      // The type of the function argument, i.e. a -> b -> c -> `resultType`.
      val argType = Type.mkPureCurriedArrow(argTypes, resultType, exp0.loc)

      // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Boxed.
      val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Types.Boxed, exp0.loc)

      // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> Boxed).
      val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

      // Compute the liftXb symbol.
      // Note that `DatalogDefs.liftX` is not necessarily fine with a visited parameter.
      val sym = lookup(DatalogDefs.liftX(argTypes.length), liftType)

      // Construct a call to the liftX function.
      MonoAst.Expr.ApplyDef(sym, List(exp0), visitType(liftType), visitType(returnType), Type.Pure, exp0.loc)
    }

    /**
      * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
      * @param argTypes must be subst, but not visited
      */
    private def liftXb(exp0: MonoAst.Expr, argTypes: List[Type])(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      // Compute the liftXb symbol.

      //
      // The liftX family of functions are of the form: a -> b -> c -> Bool and
      // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Bool.
      // That is, the function accepts a *curried* function and returns a *curried* function.
      //

      // The type of the function argument, i.e. a -> b -> c -> Bool.
      val argType = Type.mkPureCurriedArrow(argTypes, Type.Bool, exp0.loc)

      // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Bool.
      val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Type.Bool, exp0.loc)

      // The type of the overall liftXb function, i.e. (a -> b -> c -> Bool) -> (Boxed -> Boxed -> Boxed -> Bool).
      val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

      // Note that `DatalogDefs.liftXb` is *not* necessarily fine with a visited parameter, though the return type is fine
      val sym = lookup(DatalogDefs.liftXb(argTypes.length), liftType)

      // Construct a call to the liftXb function.
      MonoAst.Expr.ApplyDef(sym, List(exp0), visitType(liftType), returnType, Type.Pure, exp0.loc)
    }


    /**
      * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
      *
      * @param argTypes: Must be subst and visited.
      * @param resultType: Must be subst and visited.
      */
    private def liftXY(outVars: List[Symbol.VarSym], exp0: MonoAst.Expr, argTypes: List[Type], resultType: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {

      // Compute the liftXY symbol.
      // For example, lift3X2 is a function from three arguments to a Vector of pairs.

      //
      // The liftXY family of functions are of the form: i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)] and
      // returns a function of the form Vector[Boxed] -> Vector[Vector[Boxed]].
      // That is, the function accepts a *curried* function and an uncurried function that takes
      // its input as a boxed Vector and return its output as a vector of vectors.
      //
      //      val targs = argTypes ::: extractTuplishTypes(resultType)

      // The type of the function argument, i.e. i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)].
      val argType = Type.mkPureCurriedArrow(argTypes, resultType, loc)

      // The type of the returned function, i.e. Vector[Boxed] -> Vector[Vector[Boxed]].
      val returnType = Type.mkPureArrow(Type.mkVector(Types.Boxed, loc), Type.mkVector(Type.mkVector(Types.Boxed, loc), loc), loc)

      // The type of the overall liftXY function, i.e. (i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)]) -> (Vector[Boxed] -> Vector[Vector[Boxed]]).
      val liftType = Type.mkPureArrow(argType, returnType, loc)

      // Compute the number of bound ("output") and free ("input") variables.
      val numberOfInVars = argTypes.length
      val numberOfOutVars = outVars.length
      // Note that `DatalogDefs.liftXY` is fine with Datalog types.
      val sym = lookup(DatalogDefs.liftXY(numberOfInVars, numberOfOutVars), liftType)

      // Construct a call to the liftXY function.
      MonoAst.Expr.ApplyDef(sym, List(exp0), liftType, returnType, Type.Pure, loc)
    }

    /**
      * Returns a list expression constructed from the given `exps` with type list of `elmType`.
      */
    private def mkList(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation): MonoAst.Expr = {
      val nil = mkNil(elmType, loc)
      exps.foldRight(nil) {
        case (e, acc) => mkCons(e, acc, loc)
      }
    }

    /**
      * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
      */
    private def mkVector(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation): MonoAst.Expr = {
      MonoAst.Expr.VectorLit(exps, Type.mkVector(elmType, loc), Type.Pure, loc)
    }

    /**
      * Returns a `Nil` expression with type list of `elmType`.
      */
    private def mkNil(elmType: Type, loc: SourceLocation): MonoAst.Expr = {
      mkTag(Enums.FList, "Nil", Nil, Types.mkList(elmType, loc), loc)
    }

    /**
      * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
      */
    private def mkCons(hd: MonoAst.Expr, tail: MonoAst.Expr, loc: SourceLocation): MonoAst.Expr = {
      mkTag(Enums.FList, "Cons", List(hd, tail), tail.tpe, loc)
    }

    /**
      * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
      */
    private def mkTag(sym: Symbol.EnumSym, tag: String, exps: List[MonoAst.Expr], tpe: Type, loc: SourceLocation): MonoAst.Expr = {
      val caseSym = new Symbol.CaseSym(sym, tag, loc.asSynthetic)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, tpe, Type.Pure, loc)
    }

    /**
      * Returns an expression merging `exps` using `Defs.Merge`.
      */
    private def mergeExps(exps: List[MonoAst.Expr], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr =
      exps.reduceRight {
        (exp, acc) =>
          val itpe = Types.MergeType
          // Note that `DatalogDefs.Merge` is fine with Datalog types.
          val defnSym = lookup(DatalogDefs.Merge, itpe)
          val argExps = exp :: acc :: Nil
          val resultType = Types.Datalog
          MonoAst.Expr.ApplyDef(defnSym, argExps, itpe, resultType, exp.eff, loc)
      }

    /**
      * Returns a new `Datalog` from `datalogExp` containing only facts from the predicate given by the `PredSym` `predSymExp`
      * using `Defs.Filter`.
      */
    private def projectSym(predSymExp: MonoAst.Expr, datalogExp: MonoAst.Expr, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val argExps = predSymExp :: datalogExp :: Nil
      val resultType = Types.Datalog
      val itpe = Types.FilterType
      // Note that `DatalogDefs.Filter` is fine with Datalog types.
      val defnSym = lookup(DatalogDefs.Filter, itpe)
      MonoAst.Expr.ApplyDef(defnSym, argExps, itpe, resultType, datalogExp.eff, loc)
    }

    /**
      * Returns `t` from the Flix type `Vector[t]`.
      */
    private def unwrapVectorType(tpe: Type, loc: SourceLocation): Type = tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Vector, _), extType, _) => extType
      case t => throw InternalCompilerException(
        s"Expected Type.Apply(Type.Cst(TypeConstructor.Vector, _), _, _), but got ${t}",
        loc
      )
    }

    /**
      * Returns the pairs consisting of predicates and their term types from the extensible variant
      * type `tpe`.
      */
    private def predicatesOfExtVar(tpe: Type, loc: SourceLocation): List[(Name.Pred, List[Type])] = tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Extensible, _), tpe1, loc1) =>
        predicatesOfSchemaRow(tpe1, loc1)
      case t => throw InternalCompilerException(
        s"Expected Type.Apply(Type.Cst(TypeConstructor.Extensible, _), _, _), but got ${t}",
        loc
      )
    }

    /**
      * Returns the pairs consisting of predicates and their term types from the SchemaRow `row`.
      */
    private def predicatesOfSchemaRow(row: Type, loc: SourceLocation): List[(Name.Pred, List[Type])] = row match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), rel, loc2), tpe2, loc1) =>
        (pred, termTypesOfRelation(rel, loc2)) :: predicatesOfSchemaRow(tpe2, loc1)
      case Type.Var(_, _) => Nil
      case Type.SchemaRowEmpty => Nil
      case t => throw InternalCompilerException(s"Got unexpected ${t}", loc)
    }

    /**
      * Returns the types constituting a `Type.Relation`.
      */
    private def termTypesOfRelation(rel: Type, loc: SourceLocation): List[Type] = {
      def f(rel0: Type, loc0: SourceLocation): List[Type] = rel0 match {
        case Type.Cst(TypeConstructor.Relation(_), _) => Nil
        case Type.Apply(rest, t, loc1) => t :: f(rest, loc1)
        // The type has not been assigned. This is either due to an error in the compiler, or because it is free.
        // We assume the last and let it be of type Unit.
        // TODO: Didn't write this originally. Should this be Nil or Unit :: Nil?
        case _ if rel0.typeConstructor.contains(TypeConstructor.AnyType) => Nil
        case t => throw InternalCompilerException(s"Expected Type.Apply(_, _, _), but got ${t}", loc0)
      }

      f(rel, loc).reverse
    }

    /**
      * Returns the `LoweredAst` lambda expression
      * {{{
      *   predSym: PredSym -> terms: Vector[Boxed] -> match predSym {
      *     case PredSym.PredSym(name, _) => match name {
      *       case "P1" => xvar P1(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
      *       case "P2" => xvar P2(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
      *       ...
      *     }
      *   }
      * }}}
      * where `P1, P2, ...` are in `preds` with their respective term types.
      */
    // `tpe` must be subst and visited. Types in `preds` must be subst, but not visited.
    private def mkExtVarLambda(preds: List[(Name.Pred, List[Type])], tpe: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val predSymVar = Symbol.freshVarSym("predSym", BoundBy.FormalParam, loc)(Scope.Top, flix)
      val termsVar = Symbol.freshVarSym("terms", BoundBy.FormalParam, loc)(Scope.Top, flix)
      mkLambdaExp(predSymVar, Types.PredSym,
        mkLambdaExp(termsVar, Types.VectorOfBoxed,
          mkExtVarBody(preds, predSymVar, termsVar, tpe, loc),
          tpe, Type.Pure, loc
        ),
        Type.mkPureArrow(Types.VectorOfBoxed, tpe, loc), Type.Pure, loc
      )
    }

    /**
      * Returns the `LoweredAst` lambda expression
      * {{{
      *   paramName -> exp
      * }}}
      * where `"paramName" == param.text` and `exp` has type `expType` and effect `eff`.
      */
    private def mkLambdaExp(param: Symbol.VarSym, paramTpe: Type, exp: MonoAst.Expr, expTpe: Type, eff: Type, loc: SourceLocation): MonoAst.Expr =
      MonoAst.Expr.Lambda(
        MonoAst.FormalParam(param, paramTpe, Occur.Unknown, loc),
        exp,
        Type.mkArrowWithEffect(paramTpe, eff, expTpe, loc),
        loc
      )

    /**
      * Returns the `LoweredAst` match expression
      * {{{
      *   match predSym {
      *     case PredSym.PredSym(name, _) => match name {
      *       case "P1" => xvar P1(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
      *       case "P2" => xvar P2(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
      *       ...
      *     }
      *   }
      * }}}
      * where `P1, P2, ...` are in `preds` with their respective term types, `"predSym" == predSymVar.text`
      * and `"terms" == termsVar.text`.
      */
    // `tpe` must be subst and visited. Typed in `pred` must be subst, but not visited.
    private def mkExtVarBody(preds: List[(Name.Pred, List[Type])], predSymVar: Symbol.VarSym, termsVar: Symbol.VarSym, tpe: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val nameVar = Symbol.freshVarSym(Name.Ident("name", loc), BoundBy.Pattern)(Scope.Top, flix)
      MonoAst.Expr.Match(
        exp = MonoAst.Expr.Var(predSymVar, Types.PredSym, loc),
        rules = List(
          MonoAst.MatchRule(
            pat = MonoAst.Pattern.Tag(
              symUse = SymUse.CaseSymUse(Symbol.mkCaseSym(Enums.PredSym, Name.Ident("PredSym", loc)), loc),
              pats = List(
                MonoAst.Pattern.Var(nameVar, Type.Str, Occur.Unknown, loc),
                MonoAst.Pattern.Wild(Type.Int64, loc)
              ),
              tpe = Types.PredSym, loc = loc
            ),
            guard = None,
            exp = MonoAst.Expr.Match(
              exp = MonoAst.Expr.Var(nameVar, Type.Str, loc),
              rules = preds.map {
                case (p, types) => mkProvenanceMatchRule(termsVar, tpe, p, types, loc)
              },
              tpe = tpe, eff = Type.Pure, loc = loc
            ),
          )
        ),
        tpe = tpe, eff = Type.Pure, loc
      )
    }

    /**
      * Returns the pattern match rule
      * {{{
      *   case "P" => xvar P(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
      * }}}
      * where `"P" == p.name`
      */
    /**
      * @param tpe must be subst
      * @param types must be subst, but not visited
      * @return subst type
      */
    private def mkProvenanceMatchRule(termsVar: Symbol.VarSym, tpe: Type, p: Name.Pred, types: List[Type], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.MatchRule = {
      val termsExps = types.zipWithIndex.map {
        case (tpe1, i) => mkUnboxedTerm(termsVar, tpe1, i, loc)
      }
      MonoAst.MatchRule(
        pat = MonoAst.Pattern.Cst(Constant.Str(p.name), Type.Str, loc),
        guard = None,
        exp = MonoAst.Expr.ApplyAtomic(
          op = AtomicOp.ExtTag(Name.Label(p.name, loc)),
          exps = termsExps,
          tpe = tpe, eff = Type.Pure, loc = loc
        )
      )
    }

    /**
      * Returns the `LoweredAst` expression
      * {{{
      *   unbox(Vector.get(i, terms))
      * }}}
      * where `"terms" == termsVar.text`.
      */

    /**
      * @param tpe must be subst
      * @return subst type, but not visited
      */
    private def mkUnboxedTerm(termsVar: Symbol.VarSym, tpe: Type, i: Int, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
      val outerItpe = Type.mkPureUncurriedArrow(List(Types.Boxed), tpe, loc)
      val innerItpe = Type.mkPureUncurriedArrow(List(Type.Int32, Types.VectorOfBoxed), Types.Boxed, loc)
      MonoAst.Expr.ApplyDef(
        sym = lookup(DatalogDefs.Unbox, outerItpe),
        exps = List(
          MonoAst.Expr.ApplyDef(
            sym = lookup(Symbol.mkDefnSym(s"Vector.get"), innerItpe),
            exps = List(
              MonoAst.Expr.Cst(Constant.Int32(i), Type.Int32, loc),
              MonoAst.Expr.Var(termsVar, Types.VectorOfBoxed, loc)
            ),
            itpe = innerItpe,
            tpe = Types.Boxed, eff = Type.Pure, loc = loc
          )
        ),
        itpe = visitType(outerItpe),
        tpe = visitType(tpe), eff = Type.Pure, loc = loc
      )
    }

    /**
      * Return a list of quantified variables in the given expression `exp0`.
      *
      * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
      * but not in the constraint params `cparams0` of the constraint.
      */

    /**
      * @return not subst safe
      */
    private def quantifiedVars(cparams0: List[LoweredAst.ConstraintParam], exp0: LoweredAst.Expr): List[(Symbol.VarSym, Type)] = {
      LoweredAstOps.freeVars(exp0).toList.filter {
        case (sym, _) => isQuantifiedVar(sym, cparams0)
      }
    }

    /**
      * Returns `true` if the given variable symbol `sym` is a quantified variable according to the given constraint params `cparams0`.
      *
      * That is, the variable symbol is *NOT* lexically bound.
      */
    private def isQuantifiedVar(sym: Symbol.VarSym, cparams0: List[LoweredAst.ConstraintParam]): Boolean =
      cparams0.exists(p => p.sym == sym)

      /**
      * Lowers the given type `tpe0`.
      */
    def visitType(tpe0: Type)(implicit root: LoweredAst.Root, flix: Flix): Type = tpe0.typeConstructor match {
      case Some(TypeConstructor.Schema) =>
        // We replace any Schema type, no matter the number of polymorphic type applications, with the erased Datalog type.
        Types.Datalog
      case _ => visitTypeNonSchema(tpe0)
    }

    /**
      * Lowers the given type `tpe0` which must not be a schema type.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    private def visitTypeNonSchema(tpe0: Type)(implicit root: LoweredAst.Root, flix: Flix): Type = tpe0 match {
      case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

      case Type.Var(_, _) => tpe0

      case tp@Type.Apply(tpe1, tpe2, loc) =>
        tp.renew(visitType(tpe1), visitType(tpe2), loc)

      case Type.Alias(sym, args, t, loc) =>
        Type.Alias(sym, args.map(visitType), visitType(t), loc)

      case Type.AssocType(cst, args, kind, loc) =>
        Type.AssocType(cst, args.map(visitType), kind, loc) // TODO ASSOC-TYPES can't put lowered stuff on right side of assoc type def...

      case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

      case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)

      case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
    }

  }

}
