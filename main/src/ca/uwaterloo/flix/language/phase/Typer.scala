/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.api.{Flix, FlixEvent}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.*
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Substitution, TraitEnv}
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.collection.{ListMap, MapOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (TypedAst.Root, List[TypeError]) = flix.phaseNew("Typer") {
    implicit val sctx: SharedContext = SharedContext.mk()

    val traitEnv = mkTraitEnv(root.traits, root.instances)
    val eqEnv = mkEqualityEnv(root.traits, root.instances)

    val traits = visitTraits(root, traitEnv, eqEnv, oldRoot, changeSet)
    val instances = visitInstances(root, traitEnv, eqEnv)
    val defs = visitDefs(root, oldRoot, changeSet, traitEnv, eqEnv)
    val enums = visitEnums(root)
    val structs = visitStructs(root)
    val restrictableEnums = visitRestrictableEnums(root)
    val effs = visitEffs(root)
    val typeAliases = visitTypeAliases(root)
    val precedenceGraph = LabelledPrecedenceGraph.empty
    val sigs = traits.values.flatMap(_.sigs).map(sig => sig.sym -> sig).toMap
    val modules = ListMap(collectModules(root))
    val defaultHandlers = DefaultHandlers.visitDefaultHandlers(root)(flix, sctx, traitEnv, eqEnv)
    val result = TypedAst.Root(modules, traits, instances, sigs, defs, enums, structs, restrictableEnums, effs, typeAliases, root.uses, root.mainEntryPoint, Set.empty, defaultHandlers, root.sources, traitEnv, eqEnv, root.availableClasses, precedenceGraph, DependencyGraph.empty, root.tokens)

    (result, sctx.errors.asScala.toList)

  }

  /**
    * Collects the symbols in the given root into a map.
    */
  private def collectModules(root: KindedAst.Root): Map[Symbol.ModuleSym, List[Symbol]] = root match {
    case KindedAst.Root(traits, _, defs, enums, structs, _, effects, typeAliases, _, _, _, _, _) =>
      val sigs = traits.values.flatMap { trt => trt.sigs.values.map(_.sym) }
      val ops = effects.values.flatMap { eff => eff.ops.map(_.sym) }

      val syms0 = traits.keys ++ defs.keys ++ enums.keys ++ structs.keys ++ effects.keys ++ typeAliases.keys ++ sigs ++ ops

      // collect namespaces from prefixes of other symbols
      // TODO this should be done in resolver once the duplicate namespace issue is managed
      val namespaces = syms0.collect {
        case sym: Symbol.DefnSym => sym.namespace
        case sym: Symbol.EnumSym => sym.namespace
        case sym: Symbol.StructSym => sym.namespace
        case sym: Symbol.StructFieldSym => sym.namespace
        case sym: Symbol.RestrictableEnumSym => sym.namespace
        case sym: Symbol.TraitSym => sym.namespace
        case sym: Symbol.TypeAliasSym => sym.namespace
        case sym: Symbol.EffSym => sym.namespace
      }.flatMap {
        fullNs =>
          fullNs.inits.collect {
            case ns@(_ :: _) => new Symbol.ModuleSym(ns, ModuleKind.Standalone)
          }
      }.toSet
      val syms = syms0 ++ namespaces

      val groups = syms.groupBy {
        case sym: Symbol.DefnSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.EnumSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.StructSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.StructFieldSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.RestrictableEnumSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.TraitSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.TypeAliasSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)
        case sym: Symbol.EffSym => new Symbol.ModuleSym(sym.namespace, ModuleKind.Standalone)

        case sym: Symbol.SigSym => new Symbol.ModuleSym(sym.trt.namespace :+ sym.trt.name, ModuleKind.Standalone)
        case sym: Symbol.OpSym => new Symbol.ModuleSym(sym.eff.namespace :+ sym.eff.name, ModuleKind.Standalone)
        case sym: Symbol.AssocTypeSym => new Symbol.ModuleSym(sym.trt.namespace :+ sym.trt.name, ModuleKind.Standalone)

        case sym: Symbol.ModuleSym => new Symbol.ModuleSym(sym.ns.init, ModuleKind.Standalone)

        case sym: Symbol.CaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.RestrictableCaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.RegionSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.KindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.UnkindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.LabelSym => throw InternalCompilerException(s"unexpected symbol: $sym", SourceLocation.Unknown)
        case sym: Symbol.HoleSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
      }

      groups.map {
        case (k, v) => (k, v.toList)
      }
  }

  /**
    * Creates a trait environment from the traits and instances in the root.
    */
  private def mkTraitEnv(traits0: Map[Symbol.TraitSym, KindedAst.Trait], instances0: ListMap[Symbol.TraitSym, KindedAst.Instance]): TraitEnv = {
    val m = traits0.map {
      case (traitSym, trt) =>
        val instances = instances0.get(traitSym)
        val envInsts = instances.flatMap {
          case KindedAst.Instance(_, _, _, _, tparams, tpe, tconstrs, econstrs, _, _, _, _) =>
            tpe.typeConstructor.map {
              tc => TypeHead.Cst(tc) -> Instance(tparams.map(_.sym), tpe, tconstrs, econstrs)
            }
        }.toMap[TypeHead, Instance]
        // ignore the super trait parameters since they should all be the same as the trait param
        val superTraits = trt.superTraits.map(_.symUse.sym)
        (traitSym, TraitContext(superTraits, envInsts))
    }
    TraitEnv(m)
  }

  /**
    * Creates an equality environment from the traits and instances in the root.
    */
  private def mkEqualityEnv(traits0: Map[Symbol.TraitSym, KindedAst.Trait], instances0: ListMap[Symbol.TraitSym, KindedAst.Instance]): EqualityEnv = {
    val assocs = for {
      (traitSym, trt) <- traits0.iterator
      inst <- instances0.get(traitSym)
      assocSig <- trt.assocs
      head <- TypeHead.fromType(inst.tpe)
      assocDefOpt = inst.assocs.find(_.symUse.sym == assocSig.sym)
      tparams = inst.tparams.map(_.sym)
      assocDef = assocDefOpt match {
        // If there's no definition, then we fall back to the default
        case None =>
          val subst = Substitution.singleton(trt.tparam.sym, inst.tpe)
          val tpe = subst(assocSig.tpe.get)
          AssocTypeDef(tparams, inst.tpe, tpe)
        case Some(KindedAst.AssocTypeDef(_, _, _, arg, tpe, _)) =>
          AssocTypeDef(tparams, arg, tpe)
      }
    } yield {
      ((assocSig.sym, head), assocDef)
    }

    EqualityEnv(assocs.toMap)
  }

  /**
    * Reconstructs types in the given defs.
    */
  private def visitDefs(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit sctx: SharedContext, flix: Flix): Map[Symbol.DefnSym, TypedAst.Def] = {
    changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_) {
      case defn =>
        // SUB-EFFECTING: Check if sub-effecting is enabled for module-level defs.
        // If no effect is specified, we assume the function is pure
        val eff1 = defn.spec.eff.getOrElse(Type.Pure)
        val enableSubeffects = shouldSubeffect(eff1, Subeffecting.ModDefs)
        visitDef(defn, tconstrs0 = Nil, econstrs0 = Nil, RigidityEnv.empty, root, traitEnv, eqEnv, enableSubeffects)
    })
  }

  /**
    * Reconstructs types in the given def.
    */
  private def visitDef(defn: KindedAst.Def, tconstrs0: List[TraitConstraint], econstrs0: List[EqualityConstraint], renv0: RigidityEnv, root: KindedAst.Root, traitEnv: TraitEnv, eqEnv: EqualityEnv, open: Boolean)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
    implicit val scope: Scope = Scope.Top
    implicit val r: KindedAst.Root = root
    implicit val context: TypeContext = new TypeContext
    val (tpe, eff0) = ConstraintGen.visitExp(defn.exp)
    val infRenv = context.getRigidityEnv
    val infTconstrs = context.getTypeConstraints

    flix.emitEvent(FlixEvent.NewConstraintsDef(defn.sym, infTconstrs))

    // SUB-EFFECTING: Check if the open flag is set (i.e. if we should enable subeffecting).
    val eff = if (open) Type.mkUnion(eff0, Type.freshEffSlackVar(eff0.loc), eff0.loc) else eff0

    val infResult = InfResult(infTconstrs, tpe, eff, infRenv)
    val (subst, constraintErrors) = ConstraintSolverInterface.visitDef(defn, infResult, renv0, tconstrs0, econstrs0, traitEnv, eqEnv, root)
    constraintErrors.foreach(sctx.errors.add)
    checkSpecAssocTypes(defn.spec, tconstrs0, traitEnv)
    TypeReconstruction.visitDef(defn, subst)
  }

  /**
    * Performs type inference and reassembly on all traits in the given AST root.
    */
  private def visitTraits(root: KindedAst.Root, traitEnv: TraitEnv, eqEnv: EqualityEnv, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit sctx: SharedContext, flix: Flix): Map[Symbol.TraitSym, TypedAst.Trait] = {
    changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait(_, root, traitEnv, eqEnv)))
  }

  /**
    * Reassembles a single trait.
    */
  private def visitTrait(trt: KindedAst.Trait, root: KindedAst.Root, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit sctx: SharedContext, flix: Flix): TypedAst.Trait = trt match {
    case KindedAst.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val renv = RigidityEnv.empty.markRigid(tparam0.sym)
      val superTraits = superTraits0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeSig(sigDoc, sigMod, sigSym, tp0, kind, tpe, sigLoc) =>
          val tp = visitTypeParam(tp0)
          TypedAst.AssocTypeSig(sigDoc, sigMod, sigSym, tp, kind, tpe, sigLoc)
      }
      val tconstr = TraitConstraint(TraitSymUse(sym, sym.loc), Type.Var(tparam.sym, tparam.loc), sym.loc)
      val sigs = sigs0.values.map(visitSig(_, renv, List(tconstr), root, traitEnv, eqEnv)).toList
      val laws = laws0.map(visitDef(_, List(tconstr), Nil, renv, root, traitEnv, eqEnv, open = false))
      TypedAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc)
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: KindedAst.Sig, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], root: KindedAst.Root, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit sctx: SharedContext, flix: Flix): TypedAst.Sig = {
    implicit val scope: Scope = Scope.Top
    implicit val r: KindedAst.Root = root
    implicit val context: TypeContext = new TypeContext
    sig.exp match {
      case None => TypeReconstruction.visitSig(sig, SubstitutionTree.empty)
      case Some(exp) =>
        val (tpe, eff0) = ConstraintGen.visitExp(exp)
        val renv = context.getRigidityEnv
        val constrs = context.getTypeConstraints

        // SUB-EFFECTING: Check if sub-effecting is enabled for module-level defs. Note: We consider signatures implemented in traits to be module-level.
        // A small optimization: If the signature is pure there is no room for subeffecting.
        // If no effect is specified, we assume the signature is pure
        val eff1 = sig.spec.eff.getOrElse(Type.Pure)
        val open = shouldSubeffect(eff1, Subeffecting.ModDefs)
        val eff = if (open) Type.mkUnion(eff0, Type.freshEffSlackVar(eff0.loc), eff0.loc) else eff0

        val infResult = InfResult(constrs, tpe, eff, renv)
        val (subst, constraintErrors) = ConstraintSolverInterface.visitSig(sig, infResult, renv0, tconstrs0, traitEnv, eqEnv, root)
        constraintErrors.foreach(sctx.errors.add)
        TypeReconstruction.visitSig(sig, subst)
    }
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    */
  private def visitInstances(root: KindedAst.Root, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit sctx: SharedContext, flix: Flix): ListMap[Symbol.TraitSym, TypedAst.Instance] = {
    val instances0 = root.instances.values

    val instances = ParOps.parMap(instances0)(visitInstance(_, root, traitEnv, eqEnv))
    val mapping = instances.map {
      case instance => instance.trt.sym -> instance
    }
    ListMap.from(mapping)
  }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit sctx: SharedContext, flix: Flix): TypedAst.Instance = inst match {
    case KindedAst.Instance(doc, ann, mod, symUse, tparams0, tpe, tconstrs0, econstrs0, assocs0, defs0, ns, loc) =>
      val renv = tparams0.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
      val tparams = tparams0.map(visitTypeParam)
      val tconstrs = tconstrs0 // no subst to be done
      val econstrs = econstrs0 // no subst to be done
      checkInstAssocTypes(inst, traitEnv)
      val assocs = assocs0.map {
        case KindedAst.AssocTypeDef(defDoc, defMod, defSymUse, args, defTpe, defLoc) =>
          TypedAst.AssocTypeDef(defDoc, defMod, defSymUse, args, defTpe, defLoc)
      }

      val defs = defs0.map {
        defn =>
          // SUB-EFFECTING: Check if sub-effecting is enabled for instance-level defs.
          // If no effect is specified, we assume the function is pure
          val eff1 = defn.spec.eff.getOrElse(Type.Pure)
          val open = shouldSubeffect(eff1, Subeffecting.InsDefs)
          visitDef(defn, tconstrs, econstrs, renv, root, traitEnv, eqEnv, open)
      }
      TypedAst.Instance(doc, ann, mod, symUse, tparams, tpe, tconstrs, econstrs, assocs, defs, ns, loc)
  }

  /**
    * Reconstructs types in the given enums.
    */
  private def visitEnums(root: KindedAst.Root): Map[Symbol.EnumSym, TypedAst.Enum] = {
    MapOps.mapValues(root.enums)(visitEnum)
  }

  /**
    * Reconstructs types in the given enum.
    */
  private def visitEnum(enum0: KindedAst.Enum): TypedAst.Enum = enum0 match {
    case KindedAst.Enum(doc, ann, mod, enumSym, tparams0, derives, cases0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val cases = MapOps.mapValues(cases0) {
        case KindedAst.Case(caseSym, tagTypes, sc, caseLoc) =>
          TypedAst.Case(caseSym, tagTypes, sc, caseLoc)
      }

      TypedAst.Enum(doc, ann, mod, enumSym, tparams, derives, cases, loc)
  }

  /**
    * Reconstructs types in the given structs.
    */
  private def visitStructs(root: KindedAst.Root): Map[Symbol.StructSym, TypedAst.Struct] = {
    MapOps.mapValues(root.structs)(visitStruct)
  }

  /**
    * Reconstructs types in the given struct.
    */
  private def visitStruct(struct0: KindedAst.Struct): TypedAst.Struct = struct0 match {
    case KindedAst.Struct(doc, ann, mod, sym, tparams0, sc, fields0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val fields = fields0.zipWithIndex.map {
        case (field, _) =>
          field.sym -> TypedAst.StructField(field.sym, field.tpe, field.loc)
      }

      TypedAst.Struct(doc, ann, mod, sym, tparams, sc, fields.toMap, loc)
  }

  /**
    * Reconstructs types in the given restrictable enums.
    */
  private def visitRestrictableEnums(root: KindedAst.Root): Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum] = {
    MapOps.mapValues(root.restrictableEnums)(visitRestrictableEnum)
  }

  /**
    * Reconstructs types in the given restrictable enum.
    */
  private def visitRestrictableEnum(enum0: KindedAst.RestrictableEnum): TypedAst.RestrictableEnum = enum0 match {
    case KindedAst.RestrictableEnum(doc, ann, mod, enumSym, index0, tparams0, derives, cases0, _, loc) =>
      val index = TypedAst.TypeParam(index0.name, index0.sym, index0.loc)
      val tparams = tparams0.map(visitTypeParam)
      val cases = MapOps.mapValues(cases0) {
        case KindedAst.RestrictableCase(caseSym, tagTypes, sc, caseLoc) =>
          TypedAst.RestrictableCase(caseSym, tagTypes, sc, caseLoc)
      }

      TypedAst.RestrictableEnum(doc, ann, mod, enumSym, index, tparams, derives, cases, loc)
  }

  /**
    * Reconstructs types in the given effects.
    */
  private def visitEffs(root: KindedAst.Root): Map[Symbol.EffSym, TypedAst.Effect] = {
    MapOps.mapValues(root.effects)(visitEff)
  }

  /**
    * Reconstructs types in the given effect.
    */
  private def visitEff(eff: KindedAst.Effect): TypedAst.Effect = eff match {
    case KindedAst.Effect(doc, ann, mod, sym, tparams0, ops0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val ops = ops0.map(TypeReconstruction.visitOp)
      TypedAst.Effect(doc, ann, mod, sym, tparams, ops, loc)
  }

  /**
    * Reconstructs types in the given type aliases.
    */
  private def visitTypeAliases(root: KindedAst.Root): Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] = {
    MapOps.mapValues(root.typeAliases)(visitTypeAlias)
  }

  /**
    * Reconstructs types in the given type alias.
    */
  private def visitTypeAlias(alias: KindedAst.TypeAlias): TypedAst.TypeAlias = alias match {
    case KindedAst.TypeAlias(doc, ann, mod, sym, tparams0, tpe, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      TypedAst.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
  }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }


  /**
    * Verifies that all the associated types in the spec are resolvable, according to the declared type constraints.
    */
  private def checkSpecAssocTypes(spec0: KindedAst.Spec, extraTconstrs: List[TraitConstraint], tenv: TraitEnv)(implicit sctx: SharedContext, flix: Flix): Unit = spec0 match {
    case KindedAst.Spec(_, _, _, tparams, fparams, _, tpe, eff, tconstrs, econstrs) =>
      // get all the associated types in the spec
      val tpes = fparams.map(_.tpe) ::: tpe :: eff.getOrElse(Type.Pure) :: econstrs.flatMap(getTypes)

      // check that they are all covered by the type constraints
      for {
        t <- tpes
        assoc <- getAssocTypes(t)
      } {
        checkAssocType(assoc, tparams, extraTconstrs ::: tconstrs, tenv)
      }
  }

  /**
    * Verifies that all the associated types in the instance are resolvable, according to the declared type constraints.
    */
  private def checkInstAssocTypes(inst: KindedAst.Instance, tenv: TraitEnv)(implicit sctx: SharedContext, flix: Flix): Unit = inst match {
    case KindedAst.Instance(_, _, _, _, tparams, tpe, tconstrs, econstrs, _, _, _, _) =>
      // get all the associated types in the instance
      val tpes = tpe :: econstrs.flatMap(getTypes)

      // check that they are all covered by the type constraints
      for {
        t <- tpes
        assoc <- getAssocTypes(t)
      } {
        checkAssocType(assoc, tparams, tconstrs, tenv)
      }
  }

  /**
    * Collects all associated types from the type.
    */
  private def getAssocTypes(t: Type): List[Type.AssocType] = t match {
    case Type.Var(_, _) => Nil
    case Type.Cst(_, _) => Nil
    case Type.Apply(tpe1, tpe2, _) => getAssocTypes(tpe1) ::: getAssocTypes(tpe2)
    case Type.Alias(_, args, _, _) => args.flatMap(getAssocTypes) // TODO ASSOC-TYPES what to do about alias
    case assoc: Type.AssocType => List(assoc)
    case Type.JvmToType(tpe, _) => getAssocTypes(tpe)
    case Type.JvmToEff(tpe, _) => getAssocTypes(tpe)
    case Type.UnresolvedJvmType(member, _) => member.getTypeArguments.flatMap(getAssocTypes)
  }

  /**
    * Returns a list containing both types in the constraint.
    */
  private def getTypes(econstr: EqualityConstraint): List[Type] = econstr match {
    case EqualityConstraint(cst, tpe1, tpe2, _) =>
      // Kind is irrelevant for our purposes
      List(Type.AssocType(cst, tpe1, Kind.Wild, tpe1.loc), tpe2) // TODO ASSOC-TYPES better location for left
  }

  /**
    * Verifies that the associated type is resolvable, according to the declared type constraints.
    */
  private def checkAssocType(assocType: Type.AssocType, tparams: List[KindedAst.TypeParam], tconstrs: List[TraitConstraint], tenv: TraitEnv)(implicit sctx: SharedContext, flix: Flix): Unit = assocType match {
    case Type.AssocType(AssocTypeSymUse(assocSym, _), arg@Type.Var(tvarSym1, _), _, loc) =>
      val trtSym = assocSym.trt
      val matches = tconstrs.flatMap(withSupers(_, tenv)).exists {
        case TraitConstraint(TraitSymUse(tconstrSym, _), Type.Var(tvarSym2, _), _) =>
          trtSym == tconstrSym && tvarSym1 == tvarSym2
        case _ => false
      }
      if (matches) {
        ()
      } else {
        val renv = tparams.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
        val error = TypeError.MissingTraitConstraint(trtSym, arg, renv, loc)
        sctx.errors.add(error)
        ()
      }

    // Resiliency: If the associated type is ill-formed, then we ignore it.
    case _ => ()
  }

  /**
    * Gets the list of type constraints implied by this type constraint due to a supertrait relationship,
    * including the type constraint itself.
    *
    * For example, `Order[a]` implies `Order[a]` and `Eq[a]`
    */
  private def withSupers(tconstr: TraitConstraint, tenv: TraitEnv): List[TraitConstraint] = {
    val superSyms = tenv.getSuperTraits(tconstr.symUse.sym)
    val directSupers = superSyms.map {
      case sym => TraitConstraint(TraitSymUse(sym, SourceLocation.Unknown), tconstr.arg, tconstr.loc)
    }
    val allSupers = directSupers.flatMap(withSupers(_, tenv))
    tconstr :: allSupers
  }

  /**
    * Returns `true` if `subeffecting` is enabled by [[Flix]] and it is not redundant for a
    * function with effect `eff`.
    */
  private def shouldSubeffect(eff: Type, subeffecting: Subeffecting)(implicit flix: Flix): Boolean = {
    val enabled = flix.options.xsubeffecting.contains(subeffecting)
    val useless = eff == Type.Pure
    enabled && !useless
  }


  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {
    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[TypeError]]s in the AST, if any.
    */
  case class SharedContext(errors: ConcurrentLinkedQueue[TypeError])

}
