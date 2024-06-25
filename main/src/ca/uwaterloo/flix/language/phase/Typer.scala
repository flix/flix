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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.LabelledPrecedenceGraph
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.{ConstraintGen, ConstraintSolver, TypeContext}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.Validation.{mapN, traverse}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListMap

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.phase("Typer") {
    val traitEnv = mkTraitEnv(root.traits, root.instances)
    val eqEnv = mkEqualityEnv(root.traits, root.instances)

    val traitsVal = visitTraits(root, traitEnv, eqEnv, oldRoot, changeSet)
    val instancesVal = visitInstances(root, traitEnv, eqEnv)
    val defsVal = visitDefs(root, oldRoot, changeSet, traitEnv, eqEnv)
    val enums = visitEnums(root)
    val structs = Map.empty[Symbol.StructSym, TypedAst.Struct]
    val restrictableEnums = visitRestrictableEnums(root)
    val effs = visitEffs(root)
    val typeAliases = visitTypeAliases(root)
    val precedenceGraph = LabelledPrecedenceGraph.empty

    mapN(traitsVal, instancesVal, defsVal) {
      case (traits, instances, defs) =>
        val sigs = traits.values.flatMap(_.sigs).map(sig => sig.sym -> sig).toMap
        val modules = collectModules(root)
        TypedAst.Root(modules, traits, instances.m, sigs, defs, enums, structs, restrictableEnums, effs, typeAliases, root.uses, root.entryPoint, Set.empty, root.sources, traitEnv, eqEnv, root.names, precedenceGraph)
    }

  }(DebugValidation())

  /**
    * Collects the symbols in the given root into a map.
    */
  private def collectModules(root: KindedAst.Root): Map[Symbol.ModuleSym, List[Symbol]] = root match {
    case KindedAst.Root(traits, _, defs, enums, structs, restrictableEnums, effects, typeAliases, _, _, _, loc) =>
      val sigs = traits.values.flatMap { trt => trt.sigs.values.map(_.sym) }
      val ops = effects.values.flatMap { eff => eff.ops.map(_.sym) }

      val syms0 = traits.keys ++ defs.keys ++ enums.keys ++ effects.keys ++ typeAliases.keys ++ sigs ++ ops

      // collect namespaces from prefixes of other symbols
      // TODO this should be done in resolver once the duplicate namespace issue is managed
      val namespaces = syms0.collect {
        case sym: Symbol.DefnSym => sym.namespace
        case sym: Symbol.EnumSym => sym.namespace
        case sym: Symbol.RestrictableEnumSym => sym.namespace
        case sym: Symbol.TraitSym => sym.namespace
        case sym: Symbol.TypeAliasSym => sym.namespace
        case sym: Symbol.EffectSym => sym.namespace
      }.flatMap {
        fullNs =>
          fullNs.inits.collect {
            case ns@(_ :: _) => new Symbol.ModuleSym(ns)
          }
      }.toSet
      val syms = syms0 ++ namespaces

      val groups = syms.groupBy {
        case sym: Symbol.DefnSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.StructSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.RestrictableEnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.TraitSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.TypeAliasSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EffectSym => new Symbol.ModuleSym(sym.namespace)

        case sym: Symbol.SigSym => new Symbol.ModuleSym(sym.trt.namespace :+ sym.trt.name)
        case sym: Symbol.OpSym => new Symbol.ModuleSym(sym.eff.namespace :+ sym.eff.name)
        case sym: Symbol.AssocTypeSym => new Symbol.ModuleSym(sym.trt.namespace :+ sym.trt.name)

        case sym: Symbol.ModuleSym => new Symbol.ModuleSym(sym.ns.init)

        case sym: Symbol.CaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.StructFieldSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.RestrictableCaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
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
  private def mkTraitEnv(traits0: Map[Symbol.TraitSym, KindedAst.Trait], instances0: Map[Symbol.TraitSym, List[KindedAst.Instance]])(implicit flix: Flix): Map[Symbol.TraitSym, Ast.TraitContext] = {
      traits0.map {
        case (traitSym, trt) =>
          val instances = instances0.getOrElse(traitSym, Nil)
          val envInsts = instances.map {
            case KindedAst.Instance(_, _, _, _, tpe, tconstrs, _, _, _, _) => Ast.Instance(tpe, tconstrs)
          }
          // ignore the super trait parameters since they should all be the same as the trait param
          val superTraits = trt.superTraits.map(_.head.sym)
          (traitSym, Ast.TraitContext(superTraits, envInsts))
      }
    }

  /**
    * Creates an equality environment from the traits and instances in the root.
    */
  private def mkEqualityEnv(traits0: Map[Symbol.TraitSym, KindedAst.Trait], instances0: Map[Symbol.TraitSym, List[KindedAst.Instance]])(implicit flix: Flix): ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = {
    val assocs = for {
      (traitSym, trt) <- traits0.iterator
      inst <- instances0.getOrElse(traitSym, Nil)
      assocSig <- trt.assocs
      assocDefOpt = inst.assocs.find(_.sym.sym == assocSig.sym)
      assocDef = assocDefOpt match {
        case None =>
          val subst = Substitution.singleton(trt.tparam.sym, inst.tpe)
          val tpe = subst(assocSig.tpe.get)
          Ast.AssocTypeDef(inst.tpe, tpe)
        case Some(KindedAst.AssocTypeDef(doc, mod, sym, arg, tpe, loc)) =>
          Ast.AssocTypeDef(arg, tpe)
      }
    } yield (assocSig.sym, assocDef)


    assocs.foldLeft(ListMap.empty[Symbol.AssocTypeSym, Ast.AssocTypeDef]) {
      case (acc, (sym, defn)) => acc + (sym -> defn)
    }
    }

  /**
    * Reconstructs types in the given defs.
    */
  private def visitDefs(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)
      mapN(ParOps.parTraverseValues(staleDefs) {
        case defn =>
          // Use sub-effecting for defs if the appropriate option is set
          val open = flix.options.xsubeffecting >= SubEffectLevel.LambdasAndDefs
          visitDef(defn, tconstrs0 = Nil, RigidityEnv.empty, root, traitEnv, eqEnv, open)
      })(_ ++ freshDefs)
    }

  /**
    * Reconstructs types in the given def.
    */
  private def visitDef(defn: KindedAst.Def, tconstrs0: List[Ast.TypeConstraint], renv0: RigidityEnv, root: KindedAst.Root, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], open: Boolean)(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = {
    implicit val r: KindedAst.Root = root
    implicit val context: TypeContext = new TypeContext
    val (tpe, eff0) = ConstraintGen.visitExp(defn.exp)
    val infRenv = context.getRigidityEnv
    val infTconstrs = context.getTypeConstraints
    // If open is set and the annotated effect is not pure, use a sub-effecting
    val eff = if (!open || defn.spec.eff == Type.Pure) eff0 else Type.mkUnion(eff0, Type.freshVar(Kind.Eff, eff0.loc), eff0.loc)
    val infResult = ConstraintSolver.InfResult(infTconstrs, tpe, eff, infRenv)
    val substVal = ConstraintSolver.visitDef(defn, infResult, renv0, tconstrs0, traitEnv, eqEnv, root)
    val assocVal = checkAssocTypes(defn.spec, tconstrs0, traitEnv)
    mapN(substVal, assocVal) {
      case (subst, _) => TypeReconstruction.visitDef(defn, subst)
    }
  }

  /**
    * Performs type inference and reassembly on all traits in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitTraits(root: KindedAst.Root, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.TraitSym, TypedAst.Trait], TypeError] = {
      val (staleTraits, freshTraits) = changeSet.partition(root.traits, oldRoot.traits)
      mapN(ParOps.parTraverseValues(staleTraits)(visitTrait(_, root, traitEnv, eqEnv)))(_ ++ freshTraits)
    }

  /**
    * Reassembles a single trait.
    */
  private def visitTrait(trt: KindedAst.Trait, root: KindedAst.Root, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Trait, TypeError] = trt match {
    case KindedAst.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0, root) // TODO ASSOC-TYPES redundant?
      val renv = RigidityEnv.empty.markRigid(tparam0.sym)
      val superTraits = superTraits0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeSig(doc, mod, sym, tp0, kind, tpe, loc) =>
          val tp = visitTypeParam(tp0, root) // TODO ASSOC-TYPES redundant?
          TypedAst.AssocTypeSig(doc, mod, sym, tp, kind, tpe, loc) // TODO ASSOC-TYPES trivial
      }
      val tconstr = Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), Type.Var(tparam.sym, tparam.loc), sym.loc)
      val sigsVal = traverse(sigs0.values)(visitSig(_, renv, List(tconstr), root, traitEnv, eqEnv))
      val lawsVal = traverse(laws0)(visitDef(_, List(tconstr), renv, root, traitEnv, eqEnv, open = false))
      mapN(sigsVal, lawsVal) {
        case (sigs, laws) => TypedAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc)
      }
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: KindedAst.Sig, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], root: KindedAst.Root, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Sig, TypeError] = {
    implicit val r: KindedAst.Root = root
    implicit val context: TypeContext = new TypeContext
    sig.exp match {
      case None => Validation.success(TypeReconstruction.visitSig(sig, Substitution.empty))
      case Some(exp) =>
        val (tpe, eff0) = ConstraintGen.visitExp(exp)
        val renv = context.getRigidityEnv
        val constrs = context.getTypeConstraints
        // If defs should have sub-effecting and the annotated effect is not pure, use a sub-effecting
        val open = flix.options.xsubeffecting >= SubEffectLevel.LambdasAndDefs
        val eff = if (!open || sig.spec.eff == Type.Pure) eff0 else Type.mkUnion(eff0, Type.freshVar(Kind.Eff, eff0.loc), eff0.loc)
        val infResult = ConstraintSolver.InfResult(constrs, tpe, eff, renv)
        val substVal = ConstraintSolver.visitSig(sig, infResult, renv0, tconstrs0, traitEnv, eqEnv, root)
        mapN(substVal) {
          case subst => TypeReconstruction.visitSig(sig, subst)
        }
    }
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: KindedAst.Root, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[ListMap[Symbol.TraitSym, TypedAst.Instance], TypeError] = {
      val instances0 = for {
        (_, insts) <- root.instances
        inst <- insts
      } yield inst

      val instancesVal = ParOps.parTraverse(instances0)(visitInstance(_, root, traitEnv, eqEnv))

      mapN(instancesVal) {
        case instances =>
          val map = instances.map {
            case instance => instance.trt.sym -> instance
          }
          ListMap.from(map)
      }
    }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Instance, TypeError] = inst match {
    case KindedAst.Instance(doc, ann, mod, sym, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tpe = tpe0 // TODO ASSOC-TYPES redundant?
      val renv = tpe0.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
      val tconstrs = tconstrs0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) =>
          TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) // TODO ASSOC-TYPES trivial
      }
      // Use sub-effecting for instance defs if the appropriate option is set
      val open = flix.options.xsubeffecting >= SubEffectLevel.LambdasAndInstances
      val defsVal = Validation.traverse(defs0)(visitDef(_, tconstrs, renv, root, traitEnv, eqEnv, open))
      mapN(defsVal) {
        case defs => TypedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs, defs, ns, loc)
      }
  }

  /**
    * Reconstructs types in the given enums.
    */
  private def visitEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.EnumSym, TypedAst.Enum] = {
      // Visit every enum in the ast.
      val result = root.enums.toList.map {
        case (_, enum) => visitEnum(enum, root)
      }

      // Sequence the results and convert them back to a map.
      result.toMap
    }

  /**
    * Reconstructs types in the given enum.
    */
  private def visitEnum(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.EnumSym, TypedAst.Enum) = enum0 match {
    case KindedAst.Enum(doc, ann, mod, enumSym, tparams0, derives, cases0, tpe, loc) =>
      val tparams = tparams0.map(visitTypeParam(_, root))
      val cases = cases0 map {
        case (name, KindedAst.Case(caseSym, tagType, sc, caseLoc)) =>
          name -> TypedAst.Case(caseSym, tagType, sc, caseLoc)
      }

      enumSym -> TypedAst.Enum(doc, ann, mod, enumSym, tparams, derives, cases, tpe, loc)
  }

  /**
    * Reconstructs types in the given restrictable enums.
    */
  private def visitRestrictableEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum] = {
      // Visit every restrictable enum in the ast.
      val result = root.restrictableEnums.toList.map {
        case (_, re) => visitRestrictableEnum(re, root)
      }

      // Sequence the results and convert them back to a map.
      result.toMap
    }

  /**
    * Reconstructs types in the given restrictable enum.
    */
  private def visitRestrictableEnum(enum0: KindedAst.RestrictableEnum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum) = enum0 match {
    case KindedAst.RestrictableEnum(doc, ann, mod, enumSym, index0, tparams0, derives, cases0, tpe, loc) =>
      val index = TypedAst.TypeParam(index0.name, index0.sym, index0.loc)
      val tparams = tparams0.map(visitTypeParam(_, root))
      val cases = cases0 map {
        case (name, KindedAst.RestrictableCase(caseSym, tagType, sc, caseLoc)) =>
          name -> TypedAst.RestrictableCase(caseSym, tagType, sc, caseLoc)
      }

      enumSym -> TypedAst.RestrictableEnum(doc, ann, mod, enumSym, index, tparams, derives, cases, tpe, loc)
  }

  /**
    * Reconstructs types in the given effects.
    */
  private def visitEffs(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.EffectSym, TypedAst.Effect] = {
      root.effects.map {
        case (sym, eff) => sym -> visitEff(eff, root)
      }
    }

  /**
    * Reconstructs types in the given effect.
    */
  private def visitEff(eff: KindedAst.Effect, root: KindedAst.Root)(implicit flix: Flix): TypedAst.Effect = eff match {
    case KindedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(TypeReconstruction.visitOp)
      TypedAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Reconstructs types in the given type aliases.
    */
  private def visitTypeAliases(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] = {
      def visitTypeAlias(alias: KindedAst.TypeAlias): (Symbol.TypeAliasSym, TypedAst.TypeAlias) = alias match {
        case KindedAst.TypeAlias(doc, ann, mod, sym, tparams0, tpe, loc) =>
          val tparams = tparams0.map(visitTypeParam(_, root))
          sym -> TypedAst.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
      }

      root.typeAliases.values.map(visitTypeAlias).toMap
    }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam, root: KindedAst.Root): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }


  /**
    * Verifies that all the associated types in the spec are resolvable, according to the declared type constraints.
    */
  private def checkAssocTypes(spec0: KindedAst.Spec, extraTconstrs: List[Ast.TypeConstraint], tenv: Map[Symbol.TraitSym, Ast.TraitContext])(implicit flix: Flix): Validation[Unit, TypeError] = {
    def getAssocTypes(t: Type): List[Type.AssocType] = t match {
      case Type.Var(_, _) => Nil
      case Type.Cst(_, _) => Nil
      case Type.Apply(tpe1, tpe2, _) => getAssocTypes(tpe1) ::: getAssocTypes(tpe2)
      case Type.Alias(_, args, _, _) => args.flatMap(getAssocTypes) // TODO ASSOC-TYPES what to do about alias
      case assoc: Type.AssocType => List(assoc)
    }

    spec0 match {
      case KindedAst.Spec(_, _, _, tparams, fparams, _, tpe, eff, tconstrs, econstrs, _) =>
        // get all the associated types in the spec
        val tpes = fparams.map(_.tpe) ::: tpe :: eff :: econstrs.flatMap {
          case Ast.EqualityConstraint(cst, tpe1, tpe2, _) =>
            // Kind is irrelevant for our purposes
            List(Type.AssocType(cst, tpe1, Kind.Wild, tpe1.loc), tpe2) // TODO ASSOC-TYPES better location for left
        }

        // check that they are all covered by the type constraints
        Validation.traverseX(tpes.flatMap(getAssocTypes)) {
          case Type.AssocType(Ast.AssocTypeConstructor(assocSym, _), arg@Type.Var(tvarSym1, _), _, loc) =>
            val trtSym = assocSym.trt
            val matches = (extraTconstrs ::: tconstrs).flatMap(ConstraintSolver.withSupers(_, tenv)).exists {
              case Ast.TypeConstraint(Ast.TypeConstraint.Head(tconstrSym, _), Type.Var(tvarSym2, _), _) =>
                trtSym == tconstrSym && tvarSym1 == tvarSym2
              case _ => false
            }
            if (matches) {
              Validation.success(())
            } else {
              val renv = tparams.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
              Validation.toSoftFailure((), TypeError.MissingTraitConstraint(trtSym, arg, renv, loc))
            }
          case t => throw InternalCompilerException(s"illegal type: $t", t.loc)
        }
    }
  }

}
