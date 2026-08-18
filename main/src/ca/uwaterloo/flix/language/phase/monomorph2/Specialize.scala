/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Instance}
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.{Kind, MonoAst, RigidityEnv, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
  * Solution-driven specialization uses [[ConstraintSolver]]'s solution to specialize every def/enum/
  * struct/restrictable-enum in a single parallel pass.
  *
  * `run` builds the [[SpecializationTables]].
  *
  * [[SpecializeAndLower.visitDef]] does the actual per-def specialize+lower walk, resolving each
  * call/tag/struct site via `lookupSym`/`lookupCaseSym`/`lookupRestrictableCaseSym`/
  * `lookupStructSym`/`resolveSigSym`.
  */
object Specialize {
  /**
    * Lookup tables mapping each parametric def/enum/struct/restrictable-enum's original sym,
    * at a given ground instantiation, to its fresh specialized sym.
    *
    * @param defTable              Fresh syms for parametric defs.
    * @param enumTable             Fresh syms for parametric enums only.
    * @param structTable           Fresh syms for parametric structs only.
    * @param restrictableEnumTable Fresh syms for (parametric) restrictable enums. (Restrictable enums always carry the case-set index as an implicit tparam.)
    * @param instances             Every instance in `root`, keyed by trait and the type constructor it is defined for.
    */
  private[monomorph2] case class SpecializationTables(
    defTable: Map[(Symbol.DefnSym, Type), Symbol.DefnSym],
    enumTable: Map[(Symbol.EnumSym, List[Type]), Symbol.EnumSym],
    structTable: Map[(Symbol.StructSym, List[Type]), Symbol.StructSym],
    restrictableEnumTable: Map[(Symbol.RestrictableEnumSym, List[Type]), Symbol.EnumSym],
    instances: Map[(Symbol.TraitSym, TypeConstructor), Instance]
  )

  /**
    * The mutable data used throughout specialization.
    *
    * This class is thread-safe.
    */
  private[monomorph2] class SharedContext {
    private val specializedDefsQueue: ConcurrentLinkedQueue[(Symbol.DefnSym, MonoAst.Def)] = new ConcurrentLinkedQueue()

    /** Records `defn` under its fresh specialized `sym`. */
    def addSpecializedDef(sym: Symbol.DefnSym, defn: MonoAst.Def): Unit =
      specializedDefsQueue.add((sym, defn))

    /** Returns all specialized defs recorded so far. */
    def specializedDefs: Map[Symbol.DefnSym, MonoAst.Def] =
      specializedDefsQueue.asScala.toMap
  }

  /**
    * Returns the sym to use for a call to `sym` at ground arrow type `groundArrowTpe`.
    */
  private[monomorph2] def lookupSym(sym: Symbol.DefnSym, groundArrowTpe: Type)
                       (implicit tables: SpecializationTables, root: TypedAst.Root): Symbol.DefnSym =
    tables.defTable.get((sym, groundArrowTpe)) match {
      case Some(specializedSym) => specializedSym
      case None =>
        if (root.defs(sym).spec.tparams.isEmpty) {
          sym
        } else {
          throw InternalCompilerException(s"No specialization for $sym at type $groundArrowTpe.", sym.loc)
        }
    }

  /**
    * Returns the case sym to use for a `Tag`/`Pattern.Tag` at ground enum type `groundEnumTpe`.
    */
  private[monomorph2] def lookupCaseSym(caseSym: Symbol.CaseSym, groundEnumTpe: Type)(implicit tables: SpecializationTables): Symbol.CaseSym = {
    val argTypes = groundEnumTpe.typeArguments
    tables.enumTable.get((caseSym.enumSym, argTypes)) match {
      case Some(freshEnumSym) => new Symbol.CaseSym(freshEnumSym, caseSym.name, caseSym.ordinal, caseSym.loc)
      case None  =>
        if (argTypes.isEmpty) {
          caseSym
        } else {
          throw InternalCompilerException(s"No enum specialization for ${caseSym.enumSym} at $argTypes. ", caseSym.loc)
        }
    }
  }

  /**
    * Returns the (regular) case sym for a restrictable tag/pattern at ground restrictable-enum
    * type `groundRestrictableEnumTpe`.
    */
  private[monomorph2] def lookupRestrictableCaseSym(caseSym: Symbol.RestrictableCaseSym, groundRestrictableEnumTpe: Type)(implicit tables: SpecializationTables): Symbol.CaseSym = {
    val argTypes = groundRestrictableEnumTpe.typeArguments
    tables.restrictableEnumTable.get((caseSym.enumSym, argTypes)) match {
      case Some(freshEnumSym) => new Symbol.CaseSym(freshEnumSym, caseSym.name, Symbol.CaseSym.NoOrdinal, caseSym.loc)
      case None =>
        throw InternalCompilerException(s"No restrictable enum specialization for ${caseSym.enumSym} at $argTypes. ", caseSym.loc)
    }
  }

  /**
    * Returns the struct sym for a `StructNew`/`StructGet`/`StructPut` at ground type
    * `groundStructTpe`.
    */
  private[monomorph2] def lookupStructSym(sym: Symbol.StructSym, groundStructTpe: Type)(implicit tables: SpecializationTables): Symbol.StructSym = {
    val argTypes = groundStructTpe.typeArguments
    tables.structTable.get((sym, argTypes)) match {
      case Some(freshStructSym) => freshStructSym
      case None =>
        if (argTypes.isEmpty) {
          sym
        } else {
          throw InternalCompilerException(s"No struct specialization for $sym at $argTypes.", groundStructTpe.loc)
        }
    }
  }

  private[monomorph2] object StrictSubstitution {
    /** The empty substitution. */
    val empty: StrictSubstitution = StrictSubstitution(Substitution.empty)

    /** Returns `s` as a [[StrictSubstitution]], with every type in its image simplified and grounded. */
    def mk(s: Substitution)(implicit root: TypedAst.Root, flix: Flix): StrictSubstitution = {
      val m = s.m.map {
        case (sym, tpe) => sym -> Canonicalization.simplify(tpe.map(Canonicalization.default), isGround = true)
      }
      StrictSubstitution(Substitution(m))
    }
  }

  private[monomorph2] case class StrictSubstitution(s: Substitution) {
    /** Applies this substitution to `tpe0`, defaulting any free type variable to its kind's default type. */
    def apply(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = applySubst(MonomorphHelpers.rewriteRegionToIO(tpe0))

    /** N.B. `tpe0` must already have every `Region` rewritten to `IO`. */
    private def applySubst(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = tpe0 match {
      case Type.Var(sym, _)                         => s.m.get(sym) match {
        case None    => Canonicalization.default(tpe0)
        case Some(t) => t
      }

      case Type.Cst(TypeConstructor.Region(_), loc) => throw InternalCompilerException("unexpected Region: should have been rewritten to IO already", loc)

      case Type.Cst(_, _)                           => tpe0

      case app@Type.Apply(_, _, _)                  => Canonicalization.normalizeApply(applySubst, app, isGround = true)

      case Type.Alias(_, _, t, _)                   => applySubst(t)

      case Type.AssocType(symUse, arg0, kind, loc)  =>
        val arg = applySubst(arg0)
        val assoc = Type.AssocType(symUse, arg, kind, loc)
        val reducedType = Canonicalization.reduceAssocType(assoc)
        Canonicalization.simplify(reducedType, isGround = true)

      case Type.JvmToType(_, loc)                   => throw InternalCompilerException("unexpected JVM type", loc)
      case Type.JvmToEff(_, loc)                    => throw InternalCompilerException("unexpected JVM eff", loc)
      case Type.UnresolvedJvmType(_, loc)           => throw InternalCompilerException("unexpected JVM type", loc)
    }
  }

  /** Simplifies the types embedded in `field`. */
  private def visitStructField(field: TypedAst.StructField)(implicit root: TypedAst.Root, flix: Flix): TypedAst.StructField =
    field match {
      case TypedAst.StructField(fieldSym, tpe, loc) =>
        TypedAst.StructField(fieldSym, Canonicalization.simplify(tpe, isGround = false), loc)
    }

  /** Simplifies the types embedded in `caze`. */
  private def visitEnumCase(caze: TypedAst.Case)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Case =
    caze match {
      case TypedAst.Case(sym, tpes, sc, loc) =>
        TypedAst.Case(sym, tpes.map(Canonicalization.simplify(_, isGround = false)), sc, loc)
    }

  /** Simplifies the types embedded in `op`. */
  private def visitEffectOp(op: TypedAst.Op)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Op =
    op match {
      case TypedAst.Op(sym, TypedAst.Spec(doc, ann, mod, tparams, fparams0, declaredScheme, retTpe, eff, tconstrs, econstrs), loc) =>
        val fparams = fparams0.map {
          case TypedAst.FormalParam(varSym, tpe, src, decreasing, fpLoc) =>
            TypedAst.FormalParam(varSym, MonomorphHelpers.groundType(tpe), src, decreasing, fpLoc)
        }
        // declaredScheme.base needs the same canonicalization as fparams/retTpe/eff because
        // enumTable/structTable lookups are keyed on canonicalized types.
        val canonScheme = declaredScheme.copy(base = MonomorphHelpers.groundType(declaredScheme.base))
        val spec = TypedAst.Spec(doc, ann, mod, tparams, fparams, canonScheme, MonomorphHelpers.groundType(retTpe), MonomorphHelpers.groundType(eff), tconstrs, econstrs)
        TypedAst.Op(sym, spec, loc)
    }

  /** Returns the sym to use for a call to signature `sym`'s instance implementation (or trait-level default) at ground arrow type `groundArrowTpe`. */
  private[monomorph2] def resolveSigSym(sym: Symbol.SigSym, groundArrowTpe: Type)
                            (implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): Symbol.DefnSym = {
    val sig = root.sigs(sym)
    val trt = root.traits(sym.trt)
    // groundArrowTpe comes from an already-solved, reachable call site, so it must unify with
    // the sig's own declared scheme.
    val subst = ConstraintSolver2.fullyUnify(sig.spec.declaredScheme.base, groundArrowTpe, RegionScope.Top, RigidityEnv.empty)(root.eqEnv, flix).get
    val traitType = subst.m(trt.tparam.sym)
    // traitType is ground (groundArrowTpe has no free vars), so it always has a type constructor.
    val tyCon = traitType.typeConstructor.get
    val instance = tables.instances((sym.trt, tyCon))
    val defns = instance.defs.filter(_.sym.text == sig.sym.name)
    val (resolvedSym, isParametric) = (sig.exp, defns) match {
      // An instance implementation exists. Use it.
      case (_, defn :: Nil) => (defn.sym, defn.spec.tparams.nonEmpty || instance.tparams.nonEmpty)
      // No instance implementation, but a default implementation exists. Use it.
      case (Some(_), Nil)   => (MonomorphHelpers.defaultSigImplSym(sig), true)
      // Multiple matching defs. Should have been caught previously.
      case (_, _ :: _ :: _) => throw InternalCompilerException(s"Expected at most one matching definition for '$sym', but found ${defns.size} signatures.", sym.loc)
      // No matching defs and no default. Should have been caught previously.
      case (None, Nil)      => throw InternalCompilerException(s"No default or matching definition found for '$sym'.", sym.loc)
    }
    tables.defTable.get((resolvedSym, groundArrowTpe)) match {
      case Some(specializedSym) => specializedSym
      case None if !isParametric => resolvedSym
      case None => throw InternalCompilerException(s"No specialization for $resolvedSym at type $groundArrowTpe.", sym.loc)
    }
  }

  /** Specializes `fparams0` under `subst0`, returning the fresh params and the old-to-fresh var-sym renaming. */
  private[monomorph2] def specializeFormalParams(fparams0: List[TypedAst.FormalParam], subst0: StrictSubstitution)
                                     (implicit root: TypedAst.Root, flix: Flix): (List[TypedAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    val (params, pairs) = fparams0.map(specializeFormalParam(_, subst0)).unzip
    (params, pairs.toMap)
  }

  /** Specializes `fparam0` under `subst0`, returning the fresh param and its old-to-fresh var-sym binding. */
  private[monomorph2] def specializeFormalParam(fparam0: TypedAst.FormalParam, subst0: StrictSubstitution)
                                    (implicit root: TypedAst.Root, flix: Flix): (TypedAst.FormalParam, (Symbol.VarSym, Symbol.VarSym)) = {
    val TypedAst.FormalParam(bnd, tpe, src, decreasing, loc) = fparam0
    val freshSym = Symbol.freshVarSym(bnd.sym)
    (TypedAst.FormalParam(Binder(freshSym, subst0(bnd.tpe)), subst0(tpe), src, decreasing, loc), bnd.sym -> freshSym)
  }

  /**
    * Rewrites any specialized `Enum`/`Struct`/`RestrictableEnum` reference in `tpe` to its fresh sym.
    * `tpe` must already be substituted and lowered (i.e. only ever called via `visitType`/`visitTypeSubstituted`).
    */
  private[monomorph2] def rewriteEnumStructType(tpe: Type)(implicit tables: SpecializationTables): Type = tpe match {
    case Type.Cst(_, _)                    => tpe

    case Type.Apply(_, _, loc)             =>
      val args = tpe.typeArguments
      tpe.baseType match {
        case Type.Cst(TypeConstructor.Enum(sym, _), _) => Type.mkEnum(tables.enumTable((sym, args)), Nil, loc)
        case Type.Cst(TypeConstructor.RestrictableEnum(sym, _), _) => Type.mkEnum(tables.restrictableEnumTable((sym, args)), Nil, loc)
        case Type.Cst(TypeConstructor.Struct(sym, _), _) => Type.mkStruct(tables.structTable((sym, args)), Nil, loc)
        case _ => Type.mkApply(rewriteEnumStructType(tpe.baseType), args.map(rewriteEnumStructType), loc)
      }

    case Type.Alias(sym, args, inner, loc) =>
      Type.Alias(sym, args.map(rewriteEnumStructType), rewriteEnumStructType(inner), loc)

    case Type.Var(_, loc)                  => throw InternalCompilerException("Unexpected type variable", loc)
    case Type.AssocType(_, _, _, loc)      => throw InternalCompilerException("Unexpected associated type", loc)
    case Type.JvmToType(_, loc)            => throw InternalCompilerException("Unexpected JVM type", loc)
    case Type.JvmToEff(_, loc)             => throw InternalCompilerException("Unexpected JVM eff", loc)
    case Type.UnresolvedJvmType(_, loc)    => throw InternalCompilerException("Unexpected JVM type", loc)
  }

  /** Applies [[rewriteEnumStructType]] to `fp`'s type. */
  private[monomorph2] def rewriteFormalParam(fp: MonoAst.FormalParam)(implicit tables: SpecializationTables): MonoAst.FormalParam =
    fp.copy(tpe = rewriteEnumStructType(fp.tpe))

  /** Every def reachable from `root`: top-level, instance, default-sig-impl,
    * and the tables [[mkDefEntries]] needs for each one's declared type parameters.
    */
  private case class AllDefs(
    allDefs: Map[Symbol.DefnSym, TypedAst.Def],
    defToInst: Map[Symbol.DefnSym, TypedAst.Instance],
    defaultSigDefs: Map[Symbol.DefnSym, TypedAst.Def],
    prefixTparams: Map[Symbol.DefnSym, List[TypedAst.TypeParam]]
  )

  /** Returns every def reachable from `root`. */
  private def mkAllDefs(root: TypedAst.Root): AllDefs = {
    val allInstanceDefs: Map[Symbol.DefnSym, TypedAst.Def] = (for {
      inst <- root.instances.values
      d    <- inst.defs
    } yield d.sym -> d).toMap

    val defToInst: Map[Symbol.DefnSym, TypedAst.Instance] = (for {
      inst <- root.instances.values
      d    <- inst.defs
    } yield d.sym -> inst).toMap

    val defaultSigDefs: Map[Symbol.DefnSym, TypedAst.Def] = (for {
      sig    <- root.sigs.values
      exp    <- sig.exp
      defnSym = MonomorphHelpers.defaultSigImplSym(sig)
    } yield defnSym -> TypedAst.Def(defnSym, sig.spec, exp, sig.sym.loc)).toMap

    val defaultSigTraitTparams: Map[Symbol.DefnSym, List[TypedAst.TypeParam]] = (for {
      sig    <- root.sigs.values
      _      <- sig.exp // Filters out sigs without a default impl.
      defnSym = MonomorphHelpers.defaultSigImplSym(sig)
    } yield defnSym -> List(root.traits(sig.sym.trt).tparam)).toMap

    val prefixTparams: Map[Symbol.DefnSym, List[TypedAst.TypeParam]] =
      MapOps.mapValues(defToInst)(_.tparams) ++ defaultSigTraitTparams

    AllDefs(root.defs ++ allInstanceDefs ++ defaultSigDefs, defToInst, defaultSigDefs, prefixTparams)
  }

  /**
    * Returns one `(freshSym, defn, subst, instantiatedType)` entry per solved `GroundInstantiation`
    * of a parametric def. Instance/default-sig args are `[inst.tparams..., sig-own tparams...]`.
    */
  private def mkDefEntries(
    solution: Solution,
    allDefs: Map[Symbol.DefnSym, TypedAst.Def],
    prefixTparamsMap: Map[Symbol.DefnSym, List[TypedAst.TypeParam]]
  )(implicit root: TypedAst.Root, flix: Flix): List[(Symbol.DefnSym, TypedAst.Def, StrictSubstitution, Type)] =
    for {
      (sym, instantiations)  <- solution.defs.toList
      defn           <- allDefs.get(sym).toList
      args           <- instantiations.map(_.args)
      prefixTparams   = prefixTparamsMap.getOrElse(sym, Nil)
      substMap        = (prefixTparams.zip(args) ++ defn.spec.tparams.zip(args.drop(prefixTparams.length)))
                          .map { case (tp, ty) => tp.sym -> ty }.toMap
      if defn.spec.tparams.nonEmpty || prefixTparams.nonEmpty
      freshSym        = Symbol.freshDefnSym(defn.sym)
      subst           = StrictSubstitution.mk(Substitution(substMap))
    } yield (freshSym, defn, subst, subst(defn.spec.declaredScheme.base))

  /**
    * Returns one `(sym, args, freshSym, newEnum)` entry per solved `GroundInstantiation` of a
    * parametric enum.
    */
  private def mkEnumEntries(solution: Solution)(implicit root: TypedAst.Root, flix: Flix): List[(Symbol.EnumSym, List[Type], Symbol.EnumSym, TypedAst.Enum)] =
    for {
      (sym, instantiations) <- solution.enums.toList
      enm           <- root.enums.get(sym).toList
      if enm.tparams.nonEmpty
      args         <- instantiations.map(_.args)
      substMap       = enm.tparams.zip(args).map { case (tp, ty) => tp.sym -> ty }.toMap
      freshSym       = Symbol.freshEnumSym(enm.sym)
      subst          = StrictSubstitution.mk(Substitution(substMap))
      newCases       = enm.cases.map { case (caseSym, TypedAst.Case(_, tpes, sc, cloc)) =>
                          val newCaseSym = new Symbol.CaseSym(freshSym, caseSym.name, caseSym.ordinal, caseSym.loc)
                          newCaseSym -> TypedAst.Case(newCaseSym, tpes.map(subst.apply), sc, cloc)
                        }
      newEnum        = TypedAst.Enum(enm.doc, enm.ann, enm.mod, freshSym, Nil, enm.derives, newCases, enm.loc)
    } yield (sym, args, freshSym, newEnum)

  /**
    * Returns one `(sym, args, freshSym, newEnum)` entry per solved `GroundInstantiation` of a
    * restrictable enum. Restrictable enums lower to regular enums, so the result is a plain
    * `TypedAst.Enum`.
    */
  private def mkRestrictableEnumEntries(solution: Solution)(implicit root: TypedAst.Root, flix: Flix): List[(Symbol.RestrictableEnumSym, List[Type], Symbol.EnumSym, TypedAst.Enum)] =
    for {
      (sym, instantiations) <- solution.restrictableEnums.toList
      enm           <- root.restrictableEnums.get(sym).toList
      args         <- instantiations.map(_.args)
      substMap       = (enm.index :: enm.tparams).zip(args).map { case (tp, ty) => tp.sym -> ty }.toMap
      freshSym       = Symbol.freshEnumSym(SpecializeAndLower.lowerRestrictableEnumSym(sym))
      subst          = StrictSubstitution.mk(Substitution(substMap))
      newCases       = enm.cases.map { case (caseSym, TypedAst.RestrictableCase(_, tpes, sc, cloc)) =>
                          val newCaseSym = new Symbol.CaseSym(freshSym, caseSym.name, Symbol.CaseSym.NoOrdinal, caseSym.loc)
                          newCaseSym -> TypedAst.Case(newCaseSym, tpes.map(subst.apply), sc, cloc)
                        }
      newEnum        = TypedAst.Enum(enm.doc, enm.ann, enm.mod, freshSym, Nil, enm.derives, newCases, enm.loc)
    } yield (sym, args, freshSym, newEnum)

  /**
    * Returns one `(sym, args, freshSym, newEnum)` entry per solved `GroundInstantiation` of a
    * parametric struct.
    */
  private def mkStructEntries(solution: Solution)(implicit root: TypedAst.Root, flix: Flix): List[(Symbol.StructSym, List[Type], Symbol.StructSym, TypedAst.Struct)] =
    for {
      (sym, instantiations) <- solution.structs.toList
      struct        <- root.structs.get(sym).toList
      if struct.tparams.nonEmpty
      args         <- instantiations.map(_.args)
      substMap       = struct.tparams.zip(args).map { case (tp, ty) => tp.sym -> ty }.toMap
      freshSym       = Symbol.freshStructSym(struct.sym)
      subst          = StrictSubstitution.mk(Substitution(substMap))
      newFields      = struct.fields.map { case (fieldSym, TypedAst.StructField(_, tpe, floc)) =>
                          val newFieldSym = new Symbol.StructFieldSym(freshSym, fieldSym.name, fieldSym.loc)
                          newFieldSym -> TypedAst.StructField(newFieldSym, subst(tpe), floc)
                        }
      newStruct      = TypedAst.Struct(struct.doc, struct.ann, struct.mod, freshSym, Nil, struct.sc, newFields, struct.loc)
    } yield (sym, args, freshSym, newStruct)

  // TODO: THIS WILL BE FILLED IN PROPERLY LATER.
  def run(root: TypedAst.Root, solution: Solution)(implicit flix: Flix): MonoAst.Root = ???
}
