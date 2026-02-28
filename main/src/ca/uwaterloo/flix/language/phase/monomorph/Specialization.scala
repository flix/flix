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

package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Expr, Instance, StructField}
import ca.uwaterloo.flix.language.ast.shared.SymUse.{CaseSymUse, DefSymUse, LocalDefSymUse}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Kind, MonoAst, Name, RigidityEnv, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, Progress, TypeReduction2}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.{CofiniteSet, ListMap, ListOps, MapOps, Nel}
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
  *      - c. We lower the specialized function.
  *      - d. We enqueue (or re-use) other functions referenced by the current function which require
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
  * At a high level the relation between specialization and lowering is as follows
  *
  *   - First a function is specialized (step 3.a and 3.b)
  *   - Then a function is lowered (step 3.c)
  *   - Both specialization and lowering can lead to new functions (step 3.d)
  *   - Both lowering and specialization do a single traversal of an ast per specialization
  */
object Specialization {

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
    def mk(s: Substitution)(implicit root: TypedAst.Root, flix: Flix): StrictSubstitution = {
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
  protected[monomorph] case class StrictSubstitution(s: Substitution) {

    /**
      * Applies `this` substitution to the given type `tpe`, returning a normalized type.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    def apply(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = tpe0 match {
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
  protected[monomorph] class Context {

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
    private val defQueue: ConcurrentLinkedQueue[(Symbol.DefnSym, TypedAst.Def, StrictSubstitution)] =
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
    def enqueueSpecialization(sym: Symbol.DefnSym, defn: TypedAst.Def, subst: StrictSubstitution): Unit =
      synchronized {
        defQueue.add((sym, defn, subst))
      }

    /** Dequeues all elements from the queue and clears it. */
    def dequeueAllSpecializations: Array[(Symbol.DefnSym, TypedAst.Def, StrictSubstitution)] =
      synchronized {
        val r = defQueue.toArray(Array.empty[(Symbol.DefnSym, TypedAst.Def, StrictSubstitution)])
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
  def run(root: TypedAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Monomorpher") {
    implicit val r: TypedAst.Root = root
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
        val loweredDefn = Lowering.lowerDef(specializedDefn)
        ctx.addSpecializedDef(sym, loweredDefn)
    }

    // Perform function specialization until the queue is empty.
    // Perform specialization in parallel along the frontier, i.e. each frontier is done in parallel.
    while (ctx.nonEmptySpecializationQueue) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val queue = ctx.dequeueAllSpecializations
      ParOps.parMap(queue) {
        case (freshSym, defn, subst) =>
          val specializedDefn = specializeDef(freshSym, defn, subst)
          val loweredDefn = Lowering.lowerDef(specializedDefn)
          ctx.addSpecializedDef(freshSym, loweredDefn)
      }
    }

    val effects = ParOps.parMapValues(root.effects) {
      case TypedAst.Effect(doc, ann, mod, sym, targs, ops0, loc) =>
        val ops = ops0.map(visitEffectOp)
        val specializedEffect = TypedAst.Effect(doc, ann, mod, sym, targs, ops, loc)
        Lowering.lowerEffect(specializedEffect)
    }

    val enums = ParOps.parMapValues(root.enums) {
      case TypedAst.Enum(doc, ann, mod, sym, tparams0, derives, cases, loc) =>
        val newCases = MapOps.mapValues(cases)(visitEnumCase)
        val tparams = tparams0.map(visitTypeParam)
        val specializedEnum = TypedAst.Enum(doc, ann, mod, sym, tparams, derives, newCases, loc)
        Lowering.lowerEnum(specializedEnum)
    }

    val restrictableEnums = ParOps.parMapValues(root.restrictableEnums) {
      case TypedAst.RestrictableEnum(doc, ann, mod, sym, index, tparams0, derives, cases, loc) =>
        val newCases = MapOps.mapValues(cases)(visitRestrictableEnumCase)
        val tparams = tparams0.map(visitTypeParam)
        val specializedEnum = TypedAst.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, newCases, loc)
        Lowering.lowerRestrictableEnum(specializedEnum)
    }

    val structs = ParOps.parMapValues(root.structs) {
      case TypedAst.Struct(doc, ann, mod, sym, tparams0, sc, fields, loc) =>
        val newFields = MapOps.mapValues(fields)(visitStructField)
        val tparams = tparams0.map(visitTypeParam)
        val specializedStruct = TypedAst.Struct(doc, ann, mod, sym, tparams, sc, newFields, loc)
        Lowering.lowerStruct(specializedStruct)
    }

    val newEnums = enums ++ restrictableEnums.map {
      case (_, v) => v.sym -> v
    }

    MonoAst.Root(
      ctx.getSpecializedDefs,
      newEnums,
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
  def visitStructField(field: StructField)(implicit root: TypedAst.Root, flix: Flix): TypedAst.StructField = {
    field match {
      case TypedAst.StructField(fieldSym, tpe, loc) =>
        TypedAst.StructField(fieldSym, simplify(tpe, isGround = false), loc)
    }
  }

  /** Converts `caze`, simplifying its polymorphic type. */
  def visitEnumCase(caze: TypedAst.Case)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Case = {
    caze match {
      case TypedAst.Case(sym, tpes, sc, loc) =>
        TypedAst.Case(sym, tpes.map(simplify(_, isGround = false)), sc, loc)
    }
  }

  /** Converts `caze`, simplifying its polymorphic type. */
  def visitRestrictableEnumCase(caze: TypedAst.RestrictableCase)(implicit root: TypedAst.Root, flix: Flix): TypedAst.RestrictableCase = {
    caze match {
      case TypedAst.RestrictableCase(caseSym0, tpes, sc, loc) =>
        TypedAst.RestrictableCase(caseSym0, tpes.map(simplify(_, isGround = false)), sc, loc)
    }
  }

  /** Converts `tparam` directly. */
  private def visitTypeParam(tparam: TypedAst.TypeParam): TypedAst.TypeParam = tparam match {
    case TypedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /** Converts `op`, simplifying its type. */
  private def visitEffectOp(op: TypedAst.Op)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Op =
    op match {
      case TypedAst.Op(sym, TypedAst.Spec(doc, ann, mod, tparams, fparams0, declaredScheme, retTpe, eff, tconstrs, econstrs), loc) =>
        // Effect operations are monomorphic - they have no variables.
        // The substitution can be left empty.
        val fparams = fparams0.map {
          case TypedAst.FormalParam(varSym, tpe, src, decreasing, fpLoc) =>
            TypedAst.FormalParam(varSym, StrictSubstitution.empty(tpe), src, decreasing, fpLoc)
        }
        // `tparams` and `tconstrs` are ignored by `monomorph.Lowering`.
        // They are solely passed to adhere to the `TypedAst.spec`.
        // For `declaredScheme` we are only interested in the `base` attribute.
        val spec = TypedAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, StrictSubstitution.empty(retTpe), StrictSubstitution.empty(eff), tconstrs, econstrs)
        TypedAst.Op(sym, spec, loc)
    }

  /** Returns a specialization of `defn` with the name `freshSym` according to `subst`. */
  private def specializeDef(freshSym: Symbol.DefnSym, defn: TypedAst.Def, subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.Def = {
    val (specializedFparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

    val specializedExp = specializeExp(defn.exp, env0, subst)

    val spec0 = defn.spec
    val declaredScheme = spec0.declaredScheme.copy(base = subst(spec0.declaredScheme.base))
    // `tparams` and `tconstrs` are ignored by `monomorph.Lowering`.
    // They are solely passed to adhere to the `TypedAst.spec`.
    // For `declaredScheme` we are only interested in the `base` attribute.
    val spec = TypedAst.Spec(
      spec0.doc,
      spec0.ann,
      spec0.mod,
      spec0.tparams,
      specializedFparams,
      declaredScheme,
      subst(spec0.retTpe),
      subst(spec0.eff),
      spec0.tconstrs,
      spec0.econstrs
    )
    TypedAst.Def(freshSym, spec, specializedExp, defn.loc)
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
  private def specializeExp(exp0: TypedAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.Expr = exp0 match {
    case Expr.Var(sym, tpe, loc) =>
      Expr.Var(env0(sym), subst(tpe), loc)

    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, subst(tpe), loc)

    case Expr.Hole(sym, scp, tpe, eff, loc) =>
      val t = subst(tpe)
      Expr.Hole(sym, scp, t, subst(eff), loc)

    case Expr.HoleWithExp(exp, scp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.HoleWithExp(e, scp, t, subst(eff), loc)

    case Expr.OpenAs(symUse, exp, tpe, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.OpenAs(symUse, e, t, loc)

    case Expr.Use(symbol, alias, exp, loc) =>
      val e = specializeExp(exp, env0, subst)
      Expr.Use(symbol, alias, e, loc)

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val (p, env1) = specializeFormalParam(fparam, subst)
      val e = specializeExp(exp, env0 ++ env1, subst)
      Expr.Lambda(p, e, subst(tpe), loc)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, pos, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      Expr.ApplyClo(e1, e2, subst(tpe), subst(eff), pos, loc)

    case Expr.ApplyDef(symUse, exps, targs, itpe, tpe, eff, pos, loc) =>
      val it = subst(itpe)
      val newSym = specializeDefnSym(symUse.sym, it)
      val es = exps.map(specializeExp(_, env0, subst))
      Expr.ApplyDef(DefSymUse(newSym, symUse.loc), es, targs, it, subst(tpe), subst(eff), pos, loc)

    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, pos, loc) =>
      val newSym = env0(symUse.sym)
      val es = exps.map(specializeExp(_, env0, subst))
      val arrowT = subst(arrowTpe)
      val t = subst(tpe)
      val ef = subst(eff)
      Expr.ApplyLocalDef(LocalDefSymUse(newSym, symUse.loc), es, arrowT, t, ef, pos, loc)

    case Expr.ApplyOp(sym, exps, tpe, eff, pos, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      Expr.ApplyOp(sym, es, subst(tpe), subst(eff), pos, loc)

    case Expr.ApplySig(symUse, exps, _, targs, itpe, tpe, eff, pos, loc) =>
      val it = subst(itpe)
      val newSym = specializeSigSym(symUse.sym, it)
      val es = exps.map(specializeExp(_, env0, subst))
      Expr.ApplyDef(DefSymUse(newSym, symUse.loc), es, targs, it, subst(tpe), subst(eff), pos, loc)

    case Expr.Unary(sop, exp, tpe, eff, loc) => sop match {

      case SemanticOp.ReflectOp.ReflectEff =>
        val expTpe = subst(exp.tpe)
        val typeArg = expTpe.typeArguments.headOption.getOrElse(
          throw InternalCompilerException(s"Expected ProxyEff[ef] type, got $expTpe", loc)
        )
        val purityEnumSym = Symbols.Enums.Purity
        val caseName = typeArg match {
          case Type.Cst(TypeConstructor.Pure, _) => "Pure"
          case _                                 => "Impure"
        }
        val caseSym = Symbol.mkCaseSym(purityEnumSym, Name.Ident(caseName, loc))
        val symUse = CaseSymUse(caseSym, loc)
        val resultType = Type.mkEnum(purityEnumSym, Nil, loc)
        Expr.Tag(symUse, Nil, resultType, Type.Pure, loc)

      case SemanticOp.ReflectOp.ReflectType =>
        val expTpe = subst(exp.tpe)
        val typeArg = expTpe.typeArguments.headOption.getOrElse(
          throw InternalCompilerException(s"Expected Proxy[t] type, got $expTpe", loc)
        )
        val jvmTypeEnumSym = Symbols.Enums.JvmType
        val caseName = typeArg.baseType match {
          case Type.Cst(TypeConstructor.Bool, _)    => "JvmBool"
          case Type.Cst(TypeConstructor.Char, _)    => "JvmChar"
          case Type.Cst(TypeConstructor.Int8, _)    => "JvmInt8"
          case Type.Cst(TypeConstructor.Int16, _)   => "JvmInt16"
          case Type.Cst(TypeConstructor.Int32, _)   => "JvmInt32"
          case Type.Cst(TypeConstructor.Int64, _)   => "JvmInt64"
          case Type.Cst(TypeConstructor.Float32, _) => "JvmFloat32"
          case Type.Cst(TypeConstructor.Float64, _) => "JvmFloat64"
          case _                                    => "JvmObject"
        }
        val caseSym = Symbol.mkCaseSym(jvmTypeEnumSym, Name.Ident(caseName, loc))
        val symUse = CaseSymUse(caseSym, loc)
        val resultType = Type.mkEnum(jvmTypeEnumSym, Nil, loc)
        Expr.Tag(symUse, Nil, resultType, Type.Pure, loc)

      case SemanticOp.ReflectOp.ReflectValue =>
        val e = specializeExp(exp, env0, subst)
        val expTpe = subst(exp.tpe)
        val jvmValueEnumSym = Symbols.Enums.JvmValue
        val resultType = Type.mkEnum(jvmValueEnumSym, Nil, loc)
        val caseName = expTpe.baseType match {
          case Type.Cst(TypeConstructor.Bool, _)    => "JvmBool"
          case Type.Cst(TypeConstructor.Char, _)    => "JvmChar"
          case Type.Cst(TypeConstructor.Int8, _)    => "JvmInt8"
          case Type.Cst(TypeConstructor.Int16, _)   => "JvmInt16"
          case Type.Cst(TypeConstructor.Int32, _)   => "JvmInt32"
          case Type.Cst(TypeConstructor.Int64, _)   => "JvmInt64"
          case Type.Cst(TypeConstructor.Float32, _) => "JvmFloat32"
          case Type.Cst(TypeConstructor.Float64, _) => "JvmFloat64"
          case _                                    => "JvmObject"
        }
        val caseSym = Symbol.mkCaseSym(jvmValueEnumSym, Name.Ident(caseName, loc))
        val symUse = CaseSymUse(caseSym, loc)
        val tagArg = if (caseName == "JvmObject") {
          val objType = Type.mkNative(classOf[java.lang.Object], loc)
          Expr.UncheckedCast(e, Some(objType), None, objType, Type.Pure, loc)
        } else {
          e
        }
        Expr.Tag(symUse, List(tagArg), resultType, subst(eff), loc)

      case _ =>
        val e = specializeExp(exp, env0, subst)
        val t = subst(tpe)
        Expr.Unary(sop, e, t, subst(eff), loc)
    }

    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.Binary(sop, e1, e2, t, subst(eff), loc)

    case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env1, subst)
      Expr.Let(Binder(freshSym, subst(bnd.tpe)), e1, e2, subst(tpe), subst(eff), loc)

    case Expr.LocalDef(ann, bnd, fparams, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val (fps, env2) = specializeFormalParams(fparams, subst)
      val e1 = specializeExp(exp1, env1 ++ env2, subst)
      val e2 = specializeExp(exp2, env1, subst)
      val t = subst(tpe)
      val ef = subst(eff)
      Expr.LocalDef(ann, Binder(freshSym, subst(bnd.tpe)), fps, e1, e2, t, ef, loc)

    case Expr.Region(bnd, regionVar, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      Expr.Region(Binder(freshSym, subst(bnd.tpe)), regionVar, specializeExp(exp, env1, subst), subst(tpe), subst(eff), loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val e3 = specializeExp(exp3, env0, subst)
      Expr.IfThenElse(e1, e2, e3, subst(tpe), subst(eff), loc)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      Expr.Stm(e1, e2, subst(tpe), subst(eff), loc)

    case Expr.Discard(exp, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      Expr.Discard(e, subst(eff), loc)

    case Expr.Match(exp, rules, tpe, eff, loc0) =>
      val rs = rules map {
        case TypedAst.MatchRule(pat, guard, body, loc) =>
          val (p, env1) = specializePat(pat, Map(), subst)
          val extendedEnv = env0 ++ env1
          val g = guard.map(specializeExp(_, extendedEnv, subst))
          val b = specializeExp(body, extendedEnv, subst)
          TypedAst.MatchRule(p, g, b, loc)
      }
      Expr.Match(specializeExp(exp, env0, subst), rs, subst(tpe), subst(eff), loc0)

    case Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules.map {
        case TypedAst.ExtMatchRule(pat, exp1, loc1) =>
          val (p, env1) = specializeExtPat(pat, subst)
          val extendedEnv = env0 ++ env1
          val e1 = specializeExp(exp1, extendedEnv, subst)
          TypedAst.ExtMatchRule(p, e1, loc1)
      }
      Expr.ExtMatch(e, rs, subst(tpe), subst(eff), loc)

    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules.map(r => specializeRestrictableChooseRule(r, env0, subst))
      val t = subst(tpe)
      Expr.RestrictableChoose(star, e, rs, t, subst(eff), loc)

    case Expr.Tag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.Tag(symUse, es, t, subst(eff), loc)

    case Expr.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.RestrictableTag(symUse, es, t, subst(eff), loc)

    case Expr.ExtTag(label, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.ExtTag(label, es, t, subst(eff), loc)

    case Expr.Tuple(exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.Tuple(es, t, subst(eff), loc)

    case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.RecordSelect(e, label, t, subst(eff), loc)

    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.RecordExtend(label, e1, e2, t, subst(eff), loc)

    case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.RecordRestrict(label, e, t, subst(eff), loc)

    case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.ArrayLit(es, e, t, subst(eff), loc)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val e3 = specializeExp(exp3, env0, subst)
      val t = subst(tpe)
      Expr.ArrayNew(e1, e2, e3, t, subst(eff), loc)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.ArrayLoad(e1, e2, t, subst(eff), loc)

    case Expr.ArrayLength(exp, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      Expr.ArrayLength(e, subst(eff), loc)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val e3 = specializeExp(exp3, env0, subst)
      Expr.ArrayStore(e1, e2, e3, subst(eff), loc)

    case Expr.StructNew(sym, fields0, region0, tpe, eff, loc) =>
      val fields = fields0.map { case (k, v) => (k, specializeExp(v, env0, subst)) }
      val region = region0.map(r => specializeExp(r, env0, subst))
      val t = subst(tpe)
      Expr.StructNew(sym, fields, region, t, subst(eff), loc)

    case Expr.StructGet(exp, field, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.StructGet(e, field, t, subst(eff), loc)

    case Expr.StructPut(exp1, field, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.StructPut(e1, field, e2, t, subst(eff), loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      Expr.VectorLit(es, subst(tpe), subst(eff), loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      Expr.VectorLoad(e1, e2, subst(tpe), subst(eff), loc)

    case Expr.VectorLength(exp, loc) =>
      val e = specializeExp(exp, env0, subst)
      Expr.VectorLength(e, loc)

    case Expr.Ascribe(exp, expectedType, expectedEff, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val eType = expectedType.map(subst.apply)
      val eEff = expectedEff.map(subst.apply)
      val t = subst(tpe)
      Expr.Ascribe(e, eType, eEff, t, subst(eff), loc)

    case Expr.InstanceOf(exp, clazz, loc) =>
      val e = specializeExp(exp, env0, subst)
      Expr.InstanceOf(e, clazz, loc)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val dType = declaredType.map(subst.apply)
      val dEff = declaredEff.map(subst.apply)
      val t = subst(tpe)
      Expr.UncheckedCast(e, dType, dEff, t, subst(eff), loc)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.CheckedCast(cast, e, t, subst(eff), loc)

    case Expr.Unsafe(exp, runEff, asEff0, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      val asEff = asEff0.map(subst.apply)
      Expr.Unsafe(e, subst(runEff), asEff, t, subst(eff), loc)


    case Expr.TryCatch(exp, rules, tpe, eff, loc0) =>
      val e = specializeExp(exp, env0, subst)
      val rs = rules map {
        case TypedAst.CatchRule(bnd, clazz, body, loc) =>
          val freshSym = Symbol.freshVarSym(bnd.sym)
          val env1 = env0 + (bnd.sym -> freshSym)
          val b = specializeExp(body, env1, subst)
          TypedAst.CatchRule(Binder(freshSym, subst(bnd.tpe)), clazz, b, loc)
      }
      Expr.TryCatch(e, rs, subst(tpe), subst(eff), loc0)

    case Expr.Throw(exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.Throw(e, t, subst(eff), loc)

    case Expr.Handler(symUse0, rules0, bodyTpe, bodyEff, handledEff, tpe, loc0) =>
      val rules = rules0.map {
        case TypedAst.HandlerRule(symUse, fparams0, exp, loc) =>
          val (fparams, env1) = specializeFormalParams(fparams0, subst)
          val e = specializeExp(exp, env0 ++ env1, subst)
          TypedAst.HandlerRule(symUse, fparams, e, loc)
      }
      val bodyT = subst(bodyTpe)
      val bodyE = subst(bodyEff)
      val handledE = subst(handledEff)
      val t = subst(tpe)
      Expr.Handler(symUse0, rules, bodyT, bodyE, handledE, t, loc0)

    case Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.RunWith(e1, e2, t, subst(eff), loc)

    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.InvokeConstructor(constructor, es, t, subst(eff), loc)

    case Expr.InvokeSuperConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.InvokeSuperConstructor(constructor, es, t, subst(eff), loc)

    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.InvokeMethod(method, e, es, t, subst(eff), loc)

    case Expr.InvokeSuperMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.InvokeSuperMethod(method, es, t, subst(eff), loc)

    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.InvokeStaticMethod(method, es, t, subst(eff), loc)

    case Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.GetField(field, e, t, subst(eff), loc)

    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.PutField(field, e1, e2, t, subst(eff), loc)

    case Expr.GetStaticField(field, tpe, eff, loc) =>
      val t = subst(tpe)
      Expr.GetStaticField(field, t, subst(eff), loc)

    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.PutStaticField(field, e, t, subst(eff), loc)

    case Expr.NewObject(name, clazz, tpe, eff, constructors0, methods0, loc) =>
      val constructors = constructors0.map(specializeJvmConstructor(_, env0, subst))
      val methods = methods0.map(specializeJvmMethod(_, env0, subst))
      Expr.NewObject(name, clazz, subst(tpe), subst(eff), constructors, methods, loc)

    case Expr.NewChannel(innerExp, tpe, eff, loc) =>
      val e = specializeExp(innerExp, env0, subst)
      Expr.NewChannel(e, subst(tpe), subst(eff), loc)

    case Expr.GetChannel(innerExp, tpe, eff, loc) =>
      val e = specializeExp(innerExp, env0, subst)
      Expr.GetChannel(e, subst(tpe), subst(eff), loc)

    case Expr.PutChannel(innerExp1, innerExp2, tpe, eff, loc) =>
      val e1 = specializeExp(innerExp1, env0, subst)
      val e2 = specializeExp(innerExp2, env0, subst)
      Expr.PutChannel(e1, e2, subst(tpe), subst(eff), loc)

    case Expr.SelectChannel(rules0, default0, tpe, eff, loc0) =>
      val rules = rules0.map {
        case TypedAst.SelectChannelRule(bnd, chan0, exp, loc) =>
          val freshSym = Symbol.freshVarSym(bnd.sym)
          val env1 = env0 + (bnd.sym -> freshSym)
          val chan = specializeExp(chan0, env1, subst)
          val e = specializeExp(exp, env1, subst)
          TypedAst.SelectChannelRule(Binder(freshSym, subst(bnd.tpe)), chan, e, loc)
      }
      val default = default0.map { d => specializeExp(d, env0, subst) }
      Expr.SelectChannel(rules, default, subst(tpe), subst(eff), loc0)

    case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.Spawn(e1, e2, t, subst(eff), loc)

    case Expr.ParYield(frags, exp, tpe, eff, loc) =>
      var curEnv = env0
      val fs = frags.map {
        case TypedAst.ParYieldFragment(pat, fragExp, fragLoc) =>
          val (p, env1) = specializePat(pat, Map(), subst)
          curEnv ++= env1
          TypedAst.ParYieldFragment(p, specializeExp(fragExp, curEnv, subst), fragLoc)
      }
      val e = specializeExp(exp, curEnv, subst)
      Expr.ParYield(fs, e, subst(tpe), subst(eff), loc)

    case Expr.Lazy(exp, tpe, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.Lazy(e, t, loc)

    case Expr.Force(exp, tpe, eff, loc) =>
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.Force(e, t, subst(eff), loc)

    case Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      val cs = cs0.map(specializeConstraint(_, env0, subst))
      val t = subst(tpe)
      Expr.FixpointConstraintSet(cs, t, loc)

    case Expr.FixpointLambda(pparams0, exp, tpe, eff, loc) =>
      val pparams = pparams0.map {
        case TypedAst.PredicateParam(pred, tpe0, loc0) => TypedAst.PredicateParam(pred, subst(tpe0), loc0)
      }
      val e = specializeExp(exp, env0, subst)
      val t = subst(tpe)
      Expr.FixpointLambda(pparams, e, t, subst(eff), loc)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      val e1 = specializeExp(exp1, env0, subst)
      val e2 = specializeExp(exp2, env0, subst)
      val t = subst(tpe)
      Expr.FixpointMerge(e1, e2, t, subst(eff), loc)

    case Expr.FixpointQueryWithProvenance(exps0, select0, withh, tpe, eff, loc) =>
      val exps = exps0.map(specializeExp(_, env0, subst))
      val select = specializeHeadPred(select0, env0, subst)
      val t = subst(tpe)
      Expr.FixpointQueryWithProvenance(exps, select, withh, t, subst(eff), loc)

    // We do not care about `selects`, `from`, or `where`.
    case Expr.FixpointQueryWithSelect(exps0, queryExp0, selects0, from0, where0, pred, tpe, eff, loc) =>
      val exps = exps0.map(specializeExp(_, env0, subst))
      val queryExp = specializeExp(queryExp0, env0, subst)
      val t = subst(tpe)
      Expr.FixpointQueryWithSelect(exps, queryExp, selects0, from0, where0, pred, t, subst(eff), loc)

    case Expr.FixpointSolveWithProject(exps0, optPreds, mode, tpe, eff, loc) =>
      val exps = exps0.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.FixpointSolveWithProject(exps, optPreds, mode, t, subst(eff), loc)

    case Expr.FixpointInjectInto(exps0, predsAndArities, tpe, eff, loc) =>
      val exps = exps0.map(specializeExp(_, env0, subst))
      val t = subst(tpe)
      Expr.FixpointInjectInto(exps, predsAndArities, t, subst(eff), loc)

    case Expr.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)

  }

  /**
    * Specializes `p`.
    */
  private def specializeHeadPred(p: TypedAst.Predicate.Head, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.Predicate.Head = p match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
      val t = subst(tpe)
      val visitedTerms = terms.map(specializeExp(_, env0, subst))
      TypedAst.Predicate.Head.Atom(pred, den, visitedTerms, t, loc)
  }

  /**
    * Specializes the given body predicate `p0`.
    */
  private def specializeBodyPred(b0: TypedAst.Predicate.Body, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): (TypedAst.Predicate.Body, Map[Symbol.VarSym, Symbol.VarSym]) = b0 match {
    case TypedAst.Predicate.Body.Atom(pred0, den, polarity, fixity, terms0, tpe, loc) =>
      val (terms, env) = specializePats(terms0, env0, subst)
      val t = subst(tpe)
      (TypedAst.Predicate.Body.Atom(pred0, den, polarity, fixity, terms, t, loc), env)
    case TypedAst.Predicate.Body.Functional(outSyms0, exp, loc) =>
      val e = specializeExp(exp, env0, subst)
      val outSyms = outSyms0.map(bnd => Binder(env0(bnd.sym), subst(bnd.tpe)))
      (TypedAst.Predicate.Body.Functional(outSyms, e, loc), env0)
    case TypedAst.Predicate.Body.Guard(exp, loc) =>
      (TypedAst.Predicate.Body.Guard(specializeExp(exp, env0, subst), loc), env0)

  }

  private def specializePats(ps: List[TypedAst.Pattern], env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): (List[TypedAst.Pattern], Map[Symbol.VarSym, Symbol.VarSym]) = {
    ps.foldRight((Nil: List[TypedAst.Pattern], env0)) {
      case (pat0, (res, env1)) =>
        val (pat, env) = specializePat(pat0, env1, subst)
        (pat :: res, env)
    }
  }

  private def specializeBodies(bodies: List[TypedAst.Predicate.Body], env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): (List[TypedAst.Predicate.Body], Map[Symbol.VarSym, Symbol.VarSym]) = {
    bodies.foldRight((Nil: List[TypedAst.Predicate.Body], env0)) {
      case (body, (res, env1)) =>
        val (pat, env) = specializeBodyPred(body, env1, subst)
        (pat :: res, env)
    }
  }

  /**
    * Specializes the given constraint `c0`.
    */
  private def specializeConstraint(c0: TypedAst.Constraint, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.Constraint = c0 match {
    case TypedAst.Constraint(cparams0, head0, body0, loc0) =>
      // For every parameter of the constraint add it to `env0` with a new mapping if it has not been encountered before.
      val env = cparams0.foldLeft(env0) {
        case (env1, TypedAst.ConstraintParam(bnd, _, _)) =>
          if (env1.contains(bnd.sym)) {
            env1
          } else {
            val freshSym = Symbol.freshVarSym(bnd.sym)
            env1 + (bnd.sym -> freshSym)
          }
      }
      val cparams = cparams0.map {
        case TypedAst.ConstraintParam(bnd, tpe, loc) =>
          TypedAst.ConstraintParam(Binder(env(bnd.sym), subst(bnd.tpe)), subst(tpe), loc)
      }
      val (body, env2) = specializeBodies(body0, env, subst)
      val head = specializeHeadPred(head0, env2, subst)
      TypedAst.Constraint(cparams, head, body, loc0)
  }

  /**
    * Specializes the given restrictable choice rule `rule0` to a match rule.
    */
  private def specializeRestrictableChooseRule(rule0: TypedAst.RestrictableChooseRule, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.RestrictableChooseRule = rule0 match {
    case TypedAst.RestrictableChooseRule(pat, exp) =>
      pat match {
        case TypedAst.RestrictableChoosePattern.Tag(symUse, pat0, tpe, loc) =>
          val env = pat0.foldLeft(env0) {
            case (env1, TypedAst.RestrictableChoosePattern.Var(bnd, _, _)) =>
              val freshSym = Symbol.freshVarSym(bnd.sym)
              val env = env1 + (bnd.sym -> freshSym)
              env
            case (env1, TypedAst.RestrictableChoosePattern.Wild(_, _)) =>
              env1
            case (_, TypedAst.RestrictableChoosePattern.Error(_, errLoc)) => throw InternalCompilerException("unexpected restrictable choose variable", errLoc)
          }
          val pats = pat0.map {
            case TypedAst.RestrictableChoosePattern.Var(bnd, varTpe, varLoc) =>
              TypedAst.RestrictableChoosePattern.Var(Binder(env(bnd.sym), subst(bnd.tpe)), subst(varTpe), varLoc)
            case TypedAst.RestrictableChoosePattern.Wild(wildTpe, wildLoc) =>
              TypedAst.RestrictableChoosePattern.Wild(subst(wildTpe), wildLoc)
            case TypedAst.RestrictableChoosePattern.Error(_, errLoc) => throw InternalCompilerException("unexpected restrictable choose variable", errLoc)
          }
          val e = specializeExp(exp, env, subst)
          val p = TypedAst.RestrictableChoosePattern.Tag(symUse, pats, subst(tpe), loc)
          TypedAst.RestrictableChooseRule(p, e)
        case TypedAst.RestrictableChoosePattern.Error(_, loc) => throw InternalCompilerException("unexpected error restrictable choose pattern", loc)
      }
  }

  /**
    * Specializes `p0` w.r.t. `subst` and returns a mapping from variable symbols to fresh variable
    * symbols.
    */
  private def specializePat(p0: TypedAst.Pattern, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): (TypedAst.Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = p0 match {
    case TypedAst.Pattern.Wild(tpe, loc) =>
      (TypedAst.Pattern.Wild(subst(tpe), loc), env0)

    case TypedAst.Pattern.Var(bnd, tpe, loc) =>
      val env = if (env0.contains(bnd.sym)) {
        env0
      } else {
        val freshSym = Symbol.freshVarSym(bnd.sym)
        env0 + (bnd.sym -> freshSym)
      }
      (TypedAst.Pattern.Var(Binder(env(bnd.sym), subst(bnd.tpe)), subst(tpe), loc), env)

    case TypedAst.Pattern.Cst(cst, tpe, loc) => (TypedAst.Pattern.Cst(cst, subst(tpe), loc), env0)

    case TypedAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val (ps, env) = specializePats(pats, env0, subst)
      (TypedAst.Pattern.Tag(symUse, ps, subst(tpe), loc), env)

    case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
      val (ps, env) = specializePats(elms.toList, env0, subst)
      (TypedAst.Pattern.Tuple(Nel(ps.head, ps.tail), subst(tpe), loc), env)

    case TypedAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, envs) = pats.map {
        case TypedAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val (p1, env1) = specializePat(pat1, env0, subst)
          (TypedAst.Pattern.Record.RecordLabelPattern(label, p1, subst(tpe1), loc1), env1)
      }.unzip
      val (p, env1) = specializePat(pat, env0, subst)
      val finalEnv = env1 :: envs
      (TypedAst.Pattern.Record(ps, p, subst(tpe), loc), combineEnvs(finalEnv))

    case TypedAst.Pattern.Error(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$p0'.", loc)
  }

  /**
    * Specializes `pat0` w.r.t. `subst` and returns a mapping from variable symbols to fresh variable
    * symbols.
    */
  private def specializeExtPat(pat0: TypedAst.ExtPattern, subst: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): (TypedAst.ExtPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case TypedAst.ExtPattern.Default(loc) =>
      (TypedAst.ExtPattern.Default(loc), Map.empty)

    case TypedAst.ExtPattern.Tag(label, pats, loc) =>
      val (ps, symMaps) = pats.map(specializeExtTagPat(_, subst)).unzip
      val env = symMaps.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (TypedAst.ExtPattern.Tag(label, ps, loc), env)

    case TypedAst.ExtPattern.Error(loc) => throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  /**
    * Specializes `pat0` w.r.t. `subst` and returns a mapping from variable symbols to fresh variable
    * symbols.
    */
  private def specializeExtTagPat(pat0: TypedAst.ExtTagPattern, subst: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): (TypedAst.ExtTagPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case TypedAst.ExtTagPattern.Wild(tpe, loc) =>
      (TypedAst.ExtTagPattern.Wild(subst(tpe), loc), Map.empty)

    case TypedAst.ExtTagPattern.Var(bnd, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      (TypedAst.ExtTagPattern.Var(Binder(freshSym, subst(bnd.tpe)), subst(tpe), loc), Map(bnd.sym -> freshSym))

    case TypedAst.ExtTagPattern.Unit(tpe, loc) =>
      (TypedAst.ExtTagPattern.Unit(subst(tpe), loc), Map.empty)

    case TypedAst.ExtTagPattern.Error(_, loc) =>
      throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  /** Specializes `constructor` w.r.t. `subst`. */
  private def specializeJvmConstructor(constructor: TypedAst.JvmConstructor, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.JvmConstructor = constructor match {
    case TypedAst.JvmConstructor(exp0, tpe, eff, loc) =>
      val exp = specializeExp(exp0, env0, subst)
      TypedAst.JvmConstructor(exp, subst(tpe), subst(eff), loc)
  }

  /** Specializes `method` w.r.t. `subst`. */
  private def specializeJvmMethod(method: TypedAst.JvmMethod, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
      val (fparams, env1) = specializeFormalParams(fparams0, subst)
      val exp = specializeExp(exp0, env0 ++ env1, subst)
      TypedAst.JvmMethod(ident, fparams, exp, subst(tpe), subst(eff), loc)
  }

  /**
    * Specializes `sym` w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  protected[monomorph] def specializeDefnSym(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: TypedAst.Root, flix: Flix): Symbol.DefnSym = {
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
  private def specializeSigSym(sym: Symbol.SigSym, tpe: Type)(implicit ctx: Context, instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): Symbol.DefnSym = {
    val defn = resolveSigSym(sym, tpe)
    specializeDefCallsite(defn, tpe)
  }

  /**
    * Returns the concrete function that `sym` resolves to w.r.t. `tpe`.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def resolveSigSym(sym: Symbol.SigSym, tpe: Type)(implicit instances: Map[(Symbol.TraitSym, TypeConstructor), Instance], root: TypedAst.Root, flix: Flix): TypedAst.Def = {
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
        TypedAst.Def(defnSym, sig.spec, impl, sig.loc)
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
  private def specializeDefCallsite(defn: TypedAst.Def, tpe: Type)(implicit ctx: Context, root: TypedAst.Root, flix: Flix): Symbol.DefnSym = {
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
  private def specializeFormalParams(fparams0: List[TypedAst.FormalParam], subst0: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): (List[TypedAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    // Specialize each formal parameter and recombine the results.
    val (params, envs) = fparams0.map(p => specializeFormalParam(p, subst0)).unzip
    (params, combineEnvs(envs))
  }

  /**
    * Specializes `fparam0` w.r.t. `subst0` and returns an environment mapping the variable symbol
    * to a fresh variable symbol.
    */
  private def specializeFormalParam(fparam0: TypedAst.FormalParam, subst0: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): (TypedAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
    val TypedAst.FormalParam(bnd, tpe, src, decreasing, loc) = fparam0
    val freshSym = Symbol.freshVarSym(bnd.sym)
    (TypedAst.FormalParam(Binder(freshSym, subst0(bnd.tpe)), subst0(tpe), src, decreasing, loc), Map(bnd.sym -> freshSym))
  }

  /** Unifies `tpe1` and `tpe2` which must be unifiable. */
  private def infallibleUnify(tpe1: Type, tpe2: Type, sym: Symbol.DefnSym)(implicit root: TypedAst.Root, flix: Flix): StrictSubstitution = {
    ConstraintSolver2.fullyUnify(tpe1, tpe2, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix) match {
      case Some(subst) =>
        StrictSubstitution.mk(subst)
      case None =>
        throw InternalCompilerException(s"Unable to unify: '$tpe1' and '$tpe2'.\nIn '$sym'", tpe1.loc)
    }
  }

  /** Reduces the given associated into its definition, will crash if not able to. */
  private def reduceAssocType(assoc: Type.AssocType)(implicit root: TypedAst.Root, flix: Flix): Type = {
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
  private def simplify(tpe: Type, isGround: Boolean)(implicit root: TypedAst.Root, flix: Flix): Type = tpe match {
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
    * @param isGround  If true then `app` will be normalized.
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

}
