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
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Validation.{mapN, traverse}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListMap

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.phase("Typer") {
    val classEnv = mkClassEnv(root.classes, root.instances)
    val eqEnv = mkEqualityEnv(root.classes, root.instances)

    val classesVal = visitClasses(root, classEnv, eqEnv, oldRoot, changeSet)
    val instancesVal = visitInstances(root, classEnv, eqEnv)
    val defsVal = visitDefs(root, oldRoot, changeSet, classEnv, eqEnv)
    val enums = visitEnums(root)
    val restrictableEnums = visitRestrictableEnums(root)
    val effs = visitEffs(root)
    val typeAliases = visitTypeAliases(root)
    val precedenceGraph = LabelledPrecedenceGraph.empty

    mapN(classesVal, instancesVal, defsVal) {
      case (classes, instances, defs) =>
        val sigs = classes.values.flatMap(_.sigs).map(sig => sig.sym -> sig).toMap
        val modules = collectModules(root)
        TypedAst.Root(modules, classes, instances.m, sigs, defs, enums, restrictableEnums, effs, typeAliases, root.uses, root.entryPoint, root.sources, classEnv, eqEnv, root.names, precedenceGraph)
    }

  }

  /**
    * Collects the symbols in the given root into a map.
    */
  private def collectModules(root: KindedAst.Root): Map[Symbol.ModuleSym, List[Symbol]] = root match {
    case KindedAst.Root(classes, _, defs, enums, restrictableEnums, effects, typeAliases, _, _, _, loc) =>
      val sigs = classes.values.flatMap { clazz => clazz.sigs.values.map(_.sym) }
      val ops = effects.values.flatMap { eff => eff.ops.map(_.sym) }

      val syms0 = classes.keys ++ defs.keys ++ enums.keys ++ effects.keys ++ typeAliases.keys ++ sigs ++ ops

      // collect namespaces from prefixes of other symbols
      // TODO this should be done in resolver once the duplicate namespace issue is managed
      val namespaces = syms0.collect {
        case sym: Symbol.DefnSym => sym.namespace
        case sym: Symbol.EnumSym => sym.namespace
        case sym: Symbol.RestrictableEnumSym => sym.namespace
        case sym: Symbol.ClassSym => sym.namespace
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
        case sym: Symbol.RestrictableEnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.ClassSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.TypeAliasSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EffectSym => new Symbol.ModuleSym(sym.namespace)

        case sym: Symbol.SigSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)
        case sym: Symbol.OpSym => new Symbol.ModuleSym(sym.eff.namespace :+ sym.eff.name)
        case sym: Symbol.AssocTypeSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)

        case sym: Symbol.ModuleSym => new Symbol.ModuleSym(sym.ns.init)

        case sym: Symbol.CaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
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
    * Creates a class environment from the classes and instances in the root.
    */
  private def mkClassEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): Map[Symbol.ClassSym, Ast.ClassContext] =
    flix.subphase("ClassEnv") {
      classes0.map {
        case (classSym, clazz) =>
          val instances = instances0.getOrElse(classSym, Nil)
          val envInsts = instances.map {
            case KindedAst.Instance(_, _, _, _, tpe, tconstrs, _, _, _, _) => Ast.Instance(tpe, tconstrs)
          }
          // ignore the super class parameters since they should all be the same as the class param
          val superClasses = clazz.superClasses.map(_.head.sym)
          (classSym, Ast.ClassContext(superClasses, envInsts))
      }
    }

  /**
    * Creates an equality environment from the classes and instances in the root.
    */
  private def mkEqualityEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] =
    flix.subphase("EqualityEnv") {

      val assocs = for {
        (classSym, _) <- classes0.iterator
        inst <- instances0.getOrElse(classSym, Nil)
        assoc <- inst.assocs
      } yield (assoc.sym.sym, Ast.AssocTypeDef(assoc.arg, assoc.tpe))


      assocs.foldLeft(ListMap.empty[Symbol.AssocTypeSym, Ast.AssocTypeDef]) {
        case (acc, (sym, defn)) => acc + (sym -> defn)
      }
    }

  /**
    * Reconstructs types in the given defs.
    */
  private def visitDefs(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    flix.subphase("Defs") {
      val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)
      ParOps.parTraverseValues(staleDefs) {
        case defn => visitDef(defn, assumedTconstrs = Nil, root, classEnv, eqEnv)
      }.map(_ ++ freshDefs)
    }
  }

  /**
    * Reconstructs types in the given def.
    */
  private def visitDef(defn: KindedAst.Def, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = {
    val substVal = TypeInference.visitDefn(defn, assumedTconstrs, root, classEnv, eqEnv)
    mapN(substVal) {
      case subst => TypeReconstruction.visitDef(defn, root, subst)
    }
  }

  /**
    * Performs type inference and reassembly on all classes in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitClasses(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.ClassSym, TypedAst.Class], TypeError] =
    flix.subphase("Classes") {
      val (staleClasses, freshClasses) = changeSet.partition(root.classes, oldRoot.classes)
      ParOps.parTraverseValues(staleClasses)(visitClass(_, root, classEnv, eqEnv)).map(_ ++ freshClasses)
    }

  /**
    * Reassembles a single class.
    */
  private def visitClass(clazz: KindedAst.Class, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Class, TypeError] = clazz match {
    case KindedAst.Class(doc, ann, mod, sym, tparam0, superClasses0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0, root) // TODO ASSOC-TYPES redundant?
      val superClasses = superClasses0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeSig(doc, mod, sym, tp0, kind, loc) =>
          val tp = visitTypeParam(tp0, root) // TODO ASSOC-TYPES redundant?
          TypedAst.AssocTypeSig(doc, mod, sym, tp, kind, loc) // TODO ASSOC-TYPES trivial
      }
      val tconstr = Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), Type.Var(tparam.sym, tparam.loc), sym.loc)
      val sigsVal = traverse(sigs0.values)(visitSig(_, List(tconstr), root, classEnv, eqEnv))
      val lawsVal = traverse(laws0)(visitDef(_, List(tconstr), root, classEnv, eqEnv))
      mapN(sigsVal, lawsVal) {
        case (sigs, laws) => TypedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, laws, loc)
      }
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: KindedAst.Sig, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Sig, TypeError] = {
    val substVal = TypeInference.visitSig(sig, assumedTconstrs, root, classEnv, eqEnv)
    mapN(substVal) {
      case subst => TypeReconstruction.visitSig(sig, root, subst)
    }
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[ListMap[Symbol.ClassSym, TypedAst.Instance], TypeError] =
    flix.subphase("Instances") {

      val instances0 = for {
        (_, insts) <- root.instances
        inst <- insts
      } yield inst

      val instancesVal = ParOps.parTraverse(instances0)(visitInstance(_, root, classEnv, eqEnv))

      mapN(instancesVal) {
        case instances =>
          val map = instances.map {
            case instance => instance.clazz.sym -> instance
          }
          ListMap.from(map)
      }
    }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Instance, TypeError] = inst match {
    case KindedAst.Instance(doc, ann, mod, sym, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tpe = tpe0 // TODO ASSOC-TYPES redundant?
      val tconstrs = tconstrs0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) =>
          TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) // TODO ASSOC-TYPES trivial
      }
      val defsVal = Validation.traverse(defs0)(visitDef(_, tconstrs, root, classEnv, eqEnv))
      mapN(defsVal) {
        case defs => TypedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs, defs, ns, loc)
      }
  }

  /**
    * Reconstructs types in the given enums.
    */
  private def visitEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.EnumSym, TypedAst.Enum] =
    flix.subphase("Enums") {
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
  private def visitRestrictableEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum] =
    flix.subphase("RestrictableEnums") {
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
    flix.subphase("Effs") {
      root.effects.map {
        case (sym, eff) => sym -> visitEff(eff, root)
      }
    }
  }

  /**
    * Reconstructs types in the given effect.
    */
  private def visitEff(eff: KindedAst.Effect, root: KindedAst.Root)(implicit flix: Flix): TypedAst.Effect = eff match {
    case KindedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(TypeReconstruction.visitOp(_, root))
      TypedAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Reconstructs types in the given type aliases.
    */
  private def visitTypeAliases(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] =
    flix.subphase("TypeAliases") {
      def visitTypeAlias(alias: KindedAst.TypeAlias): (Symbol.TypeAliasSym, TypedAst.TypeAlias) = alias match {
        case KindedAst.TypeAlias(doc, mod, sym, tparams0, tpe, loc) =>
          val tparams = tparams0.map(visitTypeParam(_, root))
          sym -> TypedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }

      root.typeAliases.values.map(visitTypeAlias).toMap
    }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam, root: KindedAst.Root): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }
}
