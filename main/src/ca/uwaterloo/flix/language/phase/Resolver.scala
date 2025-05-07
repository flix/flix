/*
 *  Copyright 2017 Magnus Madsen
 *  Copyright 2024 Alexander Dybdahl Troelsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration
import ca.uwaterloo.flix.language.ast.ResolvedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.UnkindedType.*
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.{NamedAst, Symbol, *}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.errors.ResolutionError.*
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.Validation.*
import ca.uwaterloo.flix.util.collection.{Chain, ListMap, MapOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * The Resolver phase performs name resolution on the program.
  */
object Resolver {

  /**
    * The set of cases that are used by default in the namespace.
    */
  private val DefaultCases = Map(
    "Nil" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "List", SourceLocation.Unknown), "Nil", SourceLocation.Unknown),
    "Cons" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "List", SourceLocation.Unknown), "Cons", SourceLocation.Unknown),

    "None" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Option", SourceLocation.Unknown), "None", SourceLocation.Unknown),
    "Some" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Option", SourceLocation.Unknown), "Some", SourceLocation.Unknown),

    "Err" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Result", SourceLocation.Unknown), "Err", SourceLocation.Unknown),
    "Ok" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Result", SourceLocation.Unknown), "Ok", SourceLocation.Unknown)
  )

  /**
    * Built-in Kinds.
    */
  private val Kinds = Map(
    "Bool" -> Kind.Bool,
    "Eff" -> Kind.Eff,
    "Type" -> Kind.Star,
    "Region" -> Kind.Eff,
    "RecordRow" -> Kind.RecordRow,
    "SchemaRow" -> Kind.SchemaRow,
    "Predicate" -> Kind.Predicate
  )

  /**
    * Performs name resolution on the given program `root`.
    */
  def run(root: NamedAst.Root, oldRoot: ResolvedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (Validation[ResolvedAst.Root, ResolutionError], List[ResolutionError]) = flix.phaseNew("Resolver") {
    implicit val sctx: SharedContext = SharedContext.mk()

    // Get the default uses.
    // Skip over anything we can't find
    // in order to support LibMin/LibNix
    val defaultUses: LocalScope = LocalScope(ListMap(MapOps.mapValues(DefaultCases) {
      case sym => root.symbols.getOrElse(Name.mkUnlocatedNName(sym.namespace), Map.empty).getOrElse(sym.name, Nil).map(Resolution.Declaration.apply)
    }))

    val usesVal = root.uses.map {
      case (ns, uses0) =>
        mapN(traverse(uses0)(visitUseOrImport(_, ns, root))) {
          u => new Symbol.ModuleSym(ns.parts, ModuleKind.Standalone) -> u
        }
    }

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    val resolvedRoot = flatMapN(sequence(usesVal), resolveTypeAliases(defaultUses, root)) {
      case (uses, (taenv, taOrder)) =>

        val unitsVal = ParOps.parTraverse(root.units.values)(visitUnit(_, defaultUses)(taenv, sctx, root, flix))
        flatMapN(unitsVal) {
          case units =>
            val table = SymbolTable.traverse(units)(tableUnit)
            mapN(checkSuperTraitDag(table.traits)) {
              case () =>
                ResolvedAst.Root(
                  table.traits,
                  table.instances,
                  table.defs,
                  table.enums,
                  table.structs,
                  table.restrictableEnums,
                  table.effects,
                  table.typeAliases,
                  ListMap(uses.toMap),
                  taOrder,
                  root.mainEntryPoint,
                  root.sources,
                  root.availableClasses,
                  root.tokens
                )
            }
        }
    }

    (resolvedRoot, sctx.errors.asScala.toList)
  }(DebugValidation())

  /**
    * Builds a symbol table from the compilation unit.
    */
  private def tableUnit(unit: ResolvedAst.CompilationUnit): SymbolTable = unit match {
    case ResolvedAst.CompilationUnit(_, decls, _) => SymbolTable.traverse(decls)(tableDecl)
  }

  /**
    * Builds a symbol table from the declaration.
    */
  private def tableDecl(decl: ResolvedAst.Declaration): SymbolTable = decl match {
    case ResolvedAst.Declaration.Namespace(_, _, decls, _) => SymbolTable.traverse(decls)(tableDecl)
    case trt: ResolvedAst.Declaration.Trait => SymbolTable.empty.addTrait(trt)
    case inst: ResolvedAst.Declaration.Instance => SymbolTable.empty.addInstance(inst)
    case defn: ResolvedAst.Declaration.Def => SymbolTable.empty.addDef(defn)
    case enum0: ResolvedAst.Declaration.Enum => SymbolTable.empty.addEnum(enum0)
    case struct: ResolvedAst.Declaration.Struct => SymbolTable.empty.addStruct(struct)
    case enum0: ResolvedAst.Declaration.RestrictableEnum => SymbolTable.empty.addRestrictableEnum(enum0)
    case alias: ResolvedAst.Declaration.TypeAlias => SymbolTable.empty.addTypeAlias(alias)
    case effect: ResolvedAst.Declaration.Effect => SymbolTable.empty.addEffect(effect)
    case ResolvedAst.Declaration.Case(sym, _, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.RestrictableCase(sym, _, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.Op(sym, _, loc) => throw InternalCompilerException(s"Unexpected declaration: $sym", loc)
    case ResolvedAst.Declaration.Sig(sym, _, _, loc) => throw InternalCompilerException(s"Unexpected declaration: $sym", loc)
    case ResolvedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.AssocTypeDef(_, _, symUse, _, _, _) => throw InternalCompilerException(s"Unexpected declaration: $symUse", symUse.loc)
  }

  /**
    * Semi-resolves the type aliases in the root.
    */
  private def semiResolveTypeAliases(defaultUses: LocalScope, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ResolutionError] = {
    fold(root.units.values, Map.empty[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
      case (acc, unit) => mapN(semiResolveTypeAliasesInUnit(unit, defaultUses, root)) {
        case aliases => aliases.foldLeft(acc) {
          case (innerAcc, alias) => innerAcc + (alias.sym -> alias)
        }
      }
    }
  }

  /**
    * Semi-resolves the type aliases in the unit.
    */
  private def semiResolveTypeAliasesInUnit(unit: NamedAst.CompilationUnit, defaultUses: LocalScope, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[List[ResolvedAst.Declaration.TypeAlias], ResolutionError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports0, decls, _) =>
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, Name.RootNS, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val scp = appendAllUseScp(defaultUses, usesAndImports, root)
          val namespaces = decls.collect {
            case ns: NamedAst.Declaration.Namespace => ns
          }
          val aliases0 = decls.collect {
            case alias: NamedAst.Declaration.TypeAlias => alias
          }
          val aliasesVal = traverse(aliases0)(semiResolveTypeAlias(_, scp, Name.RootNS, root))
          val nsVal = traverse(namespaces)(semiResolveTypeAliasesInNamespace(_, defaultUses, root))
          mapN(aliasesVal, nsVal) {
            case (aliases, ns) => aliases ::: ns.flatten
          }
      }
  }

  /**
    * Semi-resolves the type aliases in the namespace.
    */
  private def semiResolveTypeAliasesInNamespace(ns0: NamedAst.Declaration.Namespace, defaultUses: LocalScope, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[List[ResolvedAst.Declaration.TypeAlias], ResolutionError] = ns0 match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports0, decls, _) =>
      val ns0 = Name.mkUnlocatedNName(sym.ns)
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, ns0, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val scp = appendAllUseScp(defaultUses, usesAndImports, root)
          val namespaces = decls.collect {
            case ns: NamedAst.Declaration.Namespace => ns
          }
          val aliases0 = decls.collect {
            case alias: NamedAst.Declaration.TypeAlias => alias
          }
          val aliasesVal = traverse(aliases0)(semiResolveTypeAlias(_, scp, ns0, root))
          val nsVal = traverse(namespaces)(semiResolveTypeAliasesInNamespace(_, defaultUses, root))
          mapN(aliasesVal, nsVal) {
            case (aliases, ns) => aliases ::: ns.flatten
          }
      }
  }

  /**
    * Partially resolves the type alias.
    *
    * Type aliases within the type are given temporary placeholders.
    */
  private def semiResolveTypeAlias(alias: NamedAst.Declaration.TypeAlias, scp0: LocalScope, ns: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.TypeAlias, ResolutionError] = alias match {
    case NamedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams0, tpe0, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, scp0, ns, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val scp = scp0 ++ mkTypeParamScp(tparams)
          mapN(semiResolveType(tpe0, None, Wildness.ForbidWild, scp, ns, root)(Scope.Top, sctx, flix)) {
            tpe => ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
          }
      }
  }

  /**
    * Resolves the type aliases in the given root.
    *
    * Returns a pair:
    *   - a map of type alias symbols to their AST nodes
    *   - a list of the aliases in a processing order,
    *     such that any alias only depends on those earlier in the list
    */
  private def resolveTypeAliases(defaultUses: LocalScope, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[(Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], List[Symbol.TypeAliasSym]), ResolutionError] = {
    flatMapN(semiResolveTypeAliases(defaultUses, root)) {
      case semiResolved =>
        flatMapN(findResolutionOrder(semiResolved.values)) {
          case orderedSyms =>
            val orderedSemiResolved = orderedSyms.map(semiResolved)
            mapN(finishResolveTypeAliases(orderedSemiResolved)) {
              case aliases => (aliases, orderedSyms)
            }
        }
    }
  }

  /**
    * Gets a list of all type aliases used in the partially resolved type tpe0.
    */
  private def getAliasUses(tpe0: UnkindedType): List[Symbol.TypeAliasSym] = tpe0 match {
    case _: UnkindedType.Var => Nil
    case UnkindedType.Ascribe(tpe, _, _) => getAliasUses(tpe)
    case UnkindedType.UnappliedAlias(sym, _) => sym :: Nil
    case _: UnkindedType.UnappliedAssocType => Nil
    case _: UnkindedType.Cst => Nil
    case UnkindedType.Apply(tpe1, tpe2, _) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case _: UnkindedType.Arrow => Nil
    case _: UnkindedType.CaseSet => Nil
    case UnkindedType.CaseComplement(tpe, _) => getAliasUses(tpe)
    case UnkindedType.CaseUnion(tpe1, tpe2, _) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case UnkindedType.CaseIntersection(tpe1, tpe2, _) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case _: UnkindedType.Enum => Nil
    case _: UnkindedType.Struct => Nil
    case _: UnkindedType.RestrictableEnum => Nil
    case _: UnkindedType.Error => Nil
    case alias: UnkindedType.Alias => throw InternalCompilerException("unexpected applied alias", alias.loc)
    case assoc: UnkindedType.AssocType => throw InternalCompilerException("unexpected applied associated type", assoc.loc)
  }

  /**
    * Create a list of CyclicTypeAliases errors, one for each type alias.
    */
  private def mkCycleErrors[T](cycle: List[Symbol.TypeAliasSym]): Validation.Failure[T, ResolutionError] = {
    val errors = cycle.map {
      sym => ResolutionError.CyclicTypeAliases(cycle, sym.loc)
    }
    Validation.Failure(Chain.from(errors))
  }

  /**
    * Gets the resolution order for the aliases.
    *
    * Any alias only depends on those earlier in the list
    */
  private def findResolutionOrder(aliases: Iterable[ResolvedAst.Declaration.TypeAlias]): Validation[List[Symbol.TypeAliasSym], ResolutionError] = {
    val aliasSyms = aliases.map(_.sym)
    val aliasLookup = aliases.map(alias => alias.sym -> alias).toMap
    val getUses = (sym: Symbol.TypeAliasSym) => getAliasUses(aliasLookup(sym).tpe)

    Graph.topologicalSort(aliasSyms, getUses) match {
      case Graph.TopologicalSort.Sorted(sorted) => Validation.Success(sorted)
      case Graph.TopologicalSort.Cycle(path) => mkCycleErrors(path)
    }
  }

  /**
    * Finishes the resolution of the given type aliases.
    *
    * Replaces placeholder type alias constructors with the real type aliases.
    *
    * The given aliases must be in resolution order.
    */
  private def finishResolveTypeAliases(aliases0: List[ResolvedAst.Declaration.TypeAlias])(implicit sctx: SharedContext): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ResolutionError] = {
    Validation.fold(aliases0, Map.empty[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
      case (taenv, ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe0, loc)) =>
        mapN(finishResolveType(tpe0, taenv)) {
          tpe =>
            val alias = ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
            taenv + (sym -> alias)
        }
    }
  }

  /**
    * Performs name resolution on the compilation unit.
    */
  private def visitUnit(unit: NamedAst.CompilationUnit, defaultUses: LocalScope)(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.CompilationUnit, ResolutionError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, Name.RootNS, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val scp = appendAllUseScp(defaultUses, usesAndImports, root)
          val declsVal = traverse(decls0)(visitDecl(_, scp, Name.RootNS.copy(loc = loc), defaultUses))
          mapN(declsVal) {
            case decls => ResolvedAst.CompilationUnit(usesAndImports, decls, loc)
          }
      }
  }

  /**
    * Performs name resolution on the declaration.
    */
  private def visitDecl(decl: NamedAst.Declaration, scp0: LocalScope, ns0: Name.NName, defaultUses: LocalScope)(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration, ResolutionError] = decl match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports0, decls0, loc) =>
      // TODO NS-REFACTOR move to helper for consistency
      // use the new namespace
      val ns = Name.mkUnlocatedNNameWithLoc(sym.ns, loc)
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, ns, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          // reset the scp
          val scp = appendAllUseScp(defaultUses, usesAndImports, root)
          val declsVal = traverse(decls0)(visitDecl(_, scp, ns, defaultUses))
          mapN(declsVal) {
            case decls => ResolvedAst.Declaration.Namespace(sym, usesAndImports, decls, loc)
          }
      }
    case trt@NamedAst.Declaration.Trait(_, _, _, _, _, _, _, _, _, _) =>
      resolveTrait(trt, scp0, ns0)
    case inst@NamedAst.Declaration.Instance(_, _, _, _, _, _, _, _, _, _, _) =>
      resolveInstance(inst, scp0, ns0)
    case defn@NamedAst.Declaration.Def(_, _, _, _) =>
      resolveDef(defn, None, scp0)(ns0, taenv, sctx, root, flix)
    case enum0@NamedAst.Declaration.Enum(_, _, _, _, _, _, _, _) =>
      resolveEnum(enum0, scp0, taenv, ns0, root)
    case struct@NamedAst.Declaration.Struct(_, _, _, _, _, _, _, _) =>
      resolveStruct(struct, scp0, taenv, ns0, root)
    case enum0@NamedAst.Declaration.RestrictableEnum(_, _, _, _, _, _, _, _, _) =>
      resolveRestrictableEnum(enum0, scp0, taenv, ns0, root)
    case NamedAst.Declaration.TypeAlias(_, _, _, sym, _, _, _) =>
      Validation.Success(taenv(sym))
    case eff@NamedAst.Declaration.Effect(_, _, _, _, _, _) =>
      resolveEffect(eff, scp0, taenv, ns0, root)
    case NamedAst.Declaration.Op(sym, _, _) => throw InternalCompilerException("unexpected op", sym.loc)
    case NamedAst.Declaration.Sig(sym, _, _, _) => throw InternalCompilerException("unexpected sig", sym.loc)
    case NamedAst.Declaration.Case(sym, _, _) => throw InternalCompilerException("unexpected case", sym.loc)
    case NamedAst.Declaration.StructField(_, sym, _, _) => throw InternalCompilerException("unexpected struct field", sym.loc)
    case NamedAst.Declaration.RestrictableCase(sym, _, _) => throw InternalCompilerException("unexpected case", sym.loc)
    case NamedAst.Declaration.AssocTypeDef(_, _, ident, _, _, _) => throw InternalCompilerException("unexpected associated type definition", ident.loc)
    case NamedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) => throw InternalCompilerException("unexpected associated type signature", sym.loc)
  }

  /**
    * Checks that the super traits form a DAG (no cycles).
    */
  private def checkSuperTraitDag(traits: Map[Symbol.TraitSym, ResolvedAst.Declaration.Trait]): Validation[Unit, ResolutionError] = {

    /**
      * Create a list of CyclicTraitHierarchy errors, one for each trait.
      */
    def mkCycleErrors[T](cycle: List[Symbol.TraitSym]): Validation.Failure[T, ResolutionError] = {
      val errors = cycle.map {
        sym => ResolutionError.CyclicTraitHierarchy(cycle, sym.loc)
      }
      Validation.Failure(Chain.from(errors))
    }

    val traitSyms = traits.values.map(_.sym)
    val getSuperTraits = (trt: Symbol.TraitSym) => traits(trt).superTraits.map(_.symUse.sym)
    Graph.topologicalSort(traitSyms, getSuperTraits) match {
      case Graph.TopologicalSort.Cycle(path) => mkCycleErrors(path)
      case Graph.TopologicalSort.Sorted(_) => Validation.Success(())
    }
  }

  /**
    * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
    */
  private def resolveConstraint(c0: NamedAst.Constraint, scp0: LocalScope)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Constraint, ResolutionError] = c0 match {
    case NamedAst.Constraint(cparams0, head0, body0, loc) =>
      val cparams = resolveConstraintParams(cparams0, scp0)
      val scp = scp0 ++ mkConstraintParamScp(cparams)
      val headVal = resolvePredicateHead(head0, scp)
      val bodyVal = traverse(body0)(resolvePredicateBody(_, scp))
      mapN(headVal, bodyVal) {
        case (head, body) => ResolvedAst.Constraint(cparams, head, body, loc)
      }
  }

  /**
    * Resolves all the traits in the given root.
    */
  private def resolveTrait(c0: NamedAst.Declaration.Trait, scp0: LocalScope, ns0: Name.NName)(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Trait, ResolutionError] = c0 match {
    case NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, signatures, laws0, loc) =>
      val tparamVal = resolveTypeParam(tparam0, scp0, ns0, root)
      flatMapN(tparamVal) {
        case tparam =>
          val scp = scp0 ++ mkTypeParamScp(List(tparam))
          // ignore the parameter of the super traits; we don't use it
          val superTraitsVal = traverse(superTraits0)(tconstr => resolveSuperTrait(tconstr, scp, taenv, ns0, root))
          val tconstr = ResolvedAst.TraitConstraint(TraitSymUse(sym, sym.loc), UnkindedType.Var(tparam.sym, tparam.sym.loc), sym.loc)
          val assocsVal = traverse(assocs0)(resolveAssocTypeSig(_, scp, taenv, ns0, root))
          val sigsListVal = traverse(signatures)(resolveSig(_, sym, tparam.sym, scp)(ns0, taenv, sctx, root, flix))
          val lawsVal = traverse(laws0)(resolveDef(_, Some(tconstr), scp)(ns0, taenv, sctx, root, flix))
          mapN(superTraitsVal, assocsVal, sigsListVal, lawsVal) {
            case (superTraits, assocs, sigsList, laws) =>
              val sigs = sigsList.map(sig => (sig.sym, sig)).toMap
              ResolvedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given instance `i0` in the given namespace `ns0`.
    */
  private def resolveInstance(i0: NamedAst.Declaration.Instance, scp0: LocalScope, ns0: Name.NName)(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Instance, ResolutionError] = i0 match {
    case NamedAst.Declaration.Instance(doc, ann, mod, trt0, tparams0, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, scp0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val scp = scp0 ++ mkTypeParamScp(tparams)
          val traitVal = lookupTraitForImplementation(trt0, TraitUsageKind.Implementation, scp, ns0, root)
          val tpeVal = resolveType(tpe0, None, Wildness.ForbidWild, scp, taenv, ns0, root)(Scope.Top, sctx, flix)
          val optTconstrsVal = traverse(tconstrs0)(resolveTraitConstraint(_, scp, taenv, ns0, root))
          flatMapN(traitVal, tpeVal, optTconstrsVal) {
            case (trt, tpe, optTconstrs) =>
              val assocsVal = resolveAssocTypeDefs(assocs0, trt, tpe, scp, taenv, ns0, root, loc)
              val tconstr = ResolvedAst.TraitConstraint(TraitSymUse(trt.sym, trt0.loc), tpe, trt0.loc)
              val defsVal = traverse(defs0)(resolveDef(_, Some(tconstr), scp)(ns0, taenv, sctx, root, flix))
              val tconstrs = optTconstrs.collect { case Some(t) => t }
              mapN(defsVal, assocsVal) {
                case (defs, assocs) =>
                  val symUse = TraitSymUse(trt.sym, trt0.loc)
                  ResolvedAst.Declaration.Instance(doc, ann, mod, symUse, tparams, tpe, tconstrs, assocs, defs, Name.mkUnlocatedNName(ns), loc)
              }
          }
      }
  }

  /**
    * Performs name resolution on the given signature `s0` in the given namespace `ns0`.
    */
  private def resolveSig(s0: NamedAst.Declaration.Sig, trt: Symbol.TraitSym, traitTvar: Symbol.UnkindedTypeVarSym, scp0: LocalScope)(implicit ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Sig, ResolutionError] = s0 match {
    case NamedAst.Declaration.Sig(sym, spec0, exp0, loc) =>
      val tconstr = ResolvedAst.TraitConstraint(TraitSymUse(trt, trt.loc), UnkindedType.Var(traitTvar, traitTvar.loc), trt.loc)
      val specVal = resolveSpec(spec0, Some(tconstr), scp0, taenv, ns0, root)
      flatMapN(specVal) {
        case spec =>
          val scp = scp0 ++ mkSpecScp(spec)
          checkSigSpec(sym, spec, traitTvar)
          val expVal = traverseOpt(exp0)(resolveExp(_, scp)(Scope.Top, ns0, taenv, sctx, root, flix))
          mapN(expVal) {
            case exp => ResolvedAst.Declaration.Sig(sym, spec, exp, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given definition `d0` in the given namespace `ns0`.
    */
  private def resolveDef(d0: NamedAst.Declaration.Def, tconstr: Option[ResolvedAst.TraitConstraint], scp0: LocalScope)(implicit ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Def, ResolutionError] = d0 match {
    case NamedAst.Declaration.Def(sym, spec0, exp0, loc) =>
      flix.subtask(sym.toString, sample = true)

      val specVal = resolveSpec(spec0, tconstr, scp0, taenv, ns0, root)
      flatMapN(specVal) {
        case spec =>
          val scp = scp0 ++ mkSpecScp(spec)
          val expVal = resolveExp(exp0, scp)(Scope.Top, ns0, taenv, sctx, root, flix)
          mapN(expVal) {
            case exp => ResolvedAst.Declaration.Def(sym, spec, exp, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given spec `s0` in the given namespace `ns0`.
    */
  private def resolveSpec(s0: NamedAst.Spec, tconstr: Option[ResolvedAst.TraitConstraint], scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Spec, ResolutionError] = s0 match {
    case NamedAst.Spec(doc, ann, mod, tparams0, fparams0, tpe0, eff0, tconstrs0, econstrs0) =>
      val tparamsVal = resolveTypeParams(tparams0, scp0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val scp1 = scp0 ++ mkTypeParamScp(tparams)
          val fparamsVal = traverse(fparams0)(resolveFormalParam(_, scp1, taenv, ns0, root)(Scope.Top, sctx, flix))
          flatMapN(fparamsVal) {
            case fparams =>
              val scp2 = scp1 ++ mkFormalParamScp(fparams)
              val tpeVal = resolveType(tpe0, Some(Kind.Star), Wildness.AllowWild, scp2, taenv, ns0, root)(Scope.Top, sctx, flix)
              val effVal = traverseOpt(eff0)(resolveType(_, Some(Kind.Eff), Wildness.AllowWild, scp2, taenv, ns0, root)(Scope.Top, sctx, flix))
              val optTconstrsVal = traverse(tconstrs0)(resolveTraitConstraint(_, scp2, taenv, ns0, root))
              val econstrsVal = traverse(econstrs0)(resolveEqualityConstraint(_, scp2, taenv, ns0, root))

              mapN(tpeVal, effVal, optTconstrsVal, econstrsVal) {
                case (tpe, eff, optTconstrs, econstrs) =>
                  // add the inherited type constraint to the list
                  val tconstrs = (tconstr :: optTconstrs).collect { case Some(t) => t }
                  ResolvedAst.Spec(doc, ann, mod, tparams, fparams, tpe, eff, tconstrs, econstrs)
              }
          }
      }
  }

  /**
    * Performs name resolution on the given enum `e0` in the given namespace `ns0`.
    */
  private def resolveEnum(e0: NamedAst.Declaration.Enum, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.Enum, ResolutionError] = e0 match {
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams0, derives0, cases0, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, scp0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val scp = scp0 ++ mkTypeParamScp(tparams)
          val derives = resolveDerivations(derives0, scp, ns0, root)
          val casesVal = traverse(cases0)(resolveCase(_, scp, taenv, ns0, root))
          mapN(casesVal) {
            case cases =>
              ResolvedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given struct `s0` in the given namespace `ns0`.
    */
  private def resolveStruct(s0: NamedAst.Declaration.Struct, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.Struct, ResolutionError] = s0 match {
    case NamedAst.Declaration.Struct(doc, ann, mod, sym, tparams0, fields0, _, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, scp0, ns0, root)
      flatMapN(tparamsVal) {
        tparams =>
          val scp = scp0 ++ mkTypeParamScp(tparams)
          val fieldsVal = traverse(fields0) { case field => resolveStructField(field, scp, taenv, ns0, root) }
          mapN(fieldsVal) {
            fields => ResolvedAst.Declaration.Struct(doc, ann, mod, sym, tparams, fields, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given restrictable enum `e0` in the given namespace `ns0`.
    */
  private def resolveRestrictableEnum(e0: NamedAst.Declaration.RestrictableEnum, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.RestrictableEnum, ResolutionError] = e0 match {
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index0, tparams0, derives0, cases0, loc) =>
      val indexVal = resolveTypeParam(index0, scp0, ns0, root)
      val tparamsVal = resolveTypeParams(tparams0, scp0, ns0, root)
      flatMapN(indexVal, tparamsVal) {
        case (index, tparams) =>
          val scp = scp0 ++ mkTypeParamScp(index :: tparams)
          val derives = resolveDerivations(derives0, scp, ns0, root)
          val casesVal = traverse(cases0)(resolveRestrictableCase(_, scp, taenv, ns0, root))
          mapN(casesVal) {
            cases => ResolvedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveCase(caze0: NamedAst.Declaration.Case, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.Case, ResolutionError] = caze0 match {
    case NamedAst.Declaration.Case(sym, tpes0, loc) =>
      val tpesVal = traverse(tpes0)(resolveType(_, Some(Kind.Star), Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix))
      mapN(tpesVal) {
        tpes => ResolvedAst.Declaration.Case(sym, tpes, loc)
      }
  }

  /**
    * Performs name resolution on the given struct field `field0` in the given namespace `ns0`.
    */
  private def resolveStructField(field0: NamedAst.Declaration.StructField, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.StructField, ResolutionError] = field0 match {
    case NamedAst.Declaration.StructField(mod, sym, tpe0, loc) =>
      val tpeVal = resolveType(tpe0, Some(Kind.Star), Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)
      mapN(tpeVal) {
        tpe => ResolvedAst.Declaration.StructField(mod, sym, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveRestrictableCase(caze0: NamedAst.Declaration.RestrictableCase, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.RestrictableCase, ResolutionError] = caze0 match {
    case NamedAst.Declaration.RestrictableCase(sym, tpes0, loc) =>
      val tpesVal = traverse(tpes0)(resolveType(_, Some(Kind.Star), Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix))
      mapN(tpesVal) {
        tpes => ResolvedAst.Declaration.RestrictableCase(sym, tpes, loc)
      }
  }

  /**
    * Performs name resolution on the given effect `eff0` in the given namespace `ns0`.
    */
  private def resolveEffect(eff0: NamedAst.Declaration.Effect, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.Effect, ResolutionError] = eff0 match {
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops0, loc) =>
      // TODO NS-REFACTOR maybe start a new scp
      val opsVal = traverse(ops0)(resolveOp(_, scp0, taenv, ns0, root))
      mapN(opsVal) {
        case ops => ResolvedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs name resolution on the given effect operation `op0` in the given namespace `ns0`.
    */
  private def resolveOp(op0: NamedAst.Declaration.Op, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.Op, ResolutionError] = op0 match {
    case NamedAst.Declaration.Op(sym, spec0, loc) =>
      val specVal = resolveSpec(spec0, None, scp0, taenv, ns0, root)
      mapN(specVal) {
        spec => ResolvedAst.Declaration.Op(sym, spec, loc)
      }
  }

  /**
    * Performs name resolution on the given associated type signature `s0` in the given namespace `ns0`.
    */
  private def resolveAssocTypeSig(s0: NamedAst.Declaration.AssocTypeSig, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.AssocTypeSig, ResolutionError] = s0 match {
    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam0, kind0, tpe0, loc) =>
      val tparamVal = resolveTypeParam(tparam0, scp0, ns0, root)
      val kind = resolveKind(kind0, scp0, ns0, root)
      val tpeVal = traverseOpt(tpe0)(resolveType(_, Some(kind), Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix))
      mapN(tparamVal, tpeVal) {
        case (tparam, tpe) => ResolvedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given associated type definitions `d0` in the given namespace `ns0`.
    * `loc` is the location of the instance symbol for reporting errors.
    */
  private def resolveAssocTypeDefs(d0: List[NamedAst.Declaration.AssocTypeDef], trt: NamedAst.Declaration.Trait, targ: UnkindedType, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation)(implicit sctx: SharedContext, flix: Flix): Validation[List[ResolvedAst.Declaration.AssocTypeDef], ResolutionError] = {
    flatMapN(Validation.traverse(d0)(resolveAssocTypeDef(_, trt, scp0, taenv, ns0, root))) {
      case xs =>
        // Computes a map from associated type symbols to their definitions.
        val m = mutable.Map.empty[Symbol.AssocTypeSym, ResolvedAst.Declaration.AssocTypeDef]

        // We collect [[DuplicateAssocTypeDef]] and [[DuplicateAssocTypeDef]] errors.
        val errors = mutable.ListBuffer.empty[ResolutionError]

        // Build the map `m` and check for [[DuplicateAssocTypeDef]].
        for (d@ResolvedAst.Declaration.AssocTypeDef(_, _, symUse, _, _, loc1) <- xs) {
          val sym = symUse.sym
          m.get(sym) match {
            case None =>
              m.put(sym, d)
            case Some(otherDecl) =>
              val loc2 = otherDecl.loc
              errors += ResolutionError.DuplicateAssocTypeDef(sym, loc1, loc2)
              errors += ResolutionError.DuplicateAssocTypeDef(sym, loc2, loc1)
          }
        }

        // Check for [[MissingAssocTypeDef]] and recover.
        for (NamedAst.Declaration.AssocTypeSig(_, _, ascSym, _, _, tpe, _) <- trt.assocs) {
          if (!m.contains(ascSym) && tpe.isEmpty) {
            // Missing associated type.
            errors += ResolutionError.MissingAssocTypeDef(ascSym.name, loc)

            // We recover by introducing a dummy associated type definition.
            val doc = Doc(Nil, loc)
            val mod = Modifiers.Empty
            val symUse = AssocTypeSymUse(ascSym, loc)
            val arg = targ
            val tpe = UnkindedType.Error(loc)
            val ascDef = ResolvedAst.Declaration.AssocTypeDef(doc, mod, symUse, arg, tpe, loc)
            m.put(ascSym, ascDef)
          }
        }

        // TODO ASSOC-TYPES this should be a soft failure once we know how to handle error types in unification
        // We use `m.values` here because we have eliminated duplicates and introduced missing associated type defs.
        if (errors.isEmpty) {
          Validation.Success(m.values.toList)
        } else {
          Validation.Failure(Chain.from(errors))
        }
    }
  }

  /**
    * Performs name resolution on the given associated type definition `d0` in the given namespace `ns0`.
    */
  private def resolveAssocTypeDef(d0: NamedAst.Declaration.AssocTypeDef, trt: NamedAst.Declaration.Trait, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.Declaration.AssocTypeDef, ResolutionError] = d0 match {
    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, arg0, tpe0, loc) =>

      // For now, we don't add any tvars from the args. We should have gotten those directly from the instance
      val argVal = resolveType(arg0, None, Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)
      val tpeVal = resolveType(tpe0, None, Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)
      val symVal = trt.assocs.collectFirst {
        case NamedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) if sym.name == ident.name => sym
      } match {
        case None => Validation.Failure(ResolutionError.UndefinedAssocType(Name.QName(Name.RootNS, ident, ident.loc), ident.loc))
        case Some(sym) => Validation.Success(sym)
      }
      mapN(symVal, argVal, tpeVal) {
        case (sym, arg, tpe) =>
          val symUse = AssocTypeSymUse(sym, ident.loc)
          ResolvedAst.Declaration.AssocTypeDef(doc, mod, symUse, arg, tpe, loc)
      }
  }

  /**
    * Checks that the signature spec is legal.
    *
    * A signature spec is legal if it contains the trait's type variable in its formal parameters or return type.
    */
  private def checkSigSpec(sym: Symbol.SigSym, spec0: ResolvedAst.Spec, tvar: Symbol.UnkindedTypeVarSym)(implicit sctx: SharedContext): Unit = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, tpe, _, _, _) =>
      val tpes = tpe :: fparams.flatMap(_.tpe)
      val tvars = tpes.flatMap(_.definiteTypeVars).to(SortedSet)
      if (!tvars.contains(tvar)) {
        val error = ResolutionError.IllegalSignature(sym, sym.loc)
        sctx.errors.add(error)
      }
  }

  private def resolveKind(kind0: NamedAst.Kind, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Kind = kind0 match {
    case NamedAst.Kind.Ambiguous(qname, loc) =>
      if (qname.isUnqualified) {
        val name = qname.ident.name
        Kinds.get(name) match {
          case None =>
            lookupRestrictableEnum(qname, scp0, ns0, root).toResult match {
              case Result.Ok(enum0) =>
                Kind.CaseSet(enum0.sym)
              case Result.Err(_) =>
                // We don't know the kind, but we can find the best match.
                val error = ResolutionError.UndefinedKind(qname, ns0, loc)
                sctx.errors.add(error)
                Similarity.closestMatch(name, Kinds)
            }
          case Some(kind) => kind
        }
      } else {
        lookupRestrictableEnum(qname, scp0, ns0, root).toResult match {
          case Result.Ok(enum0) => Kind.CaseSet(enum0.sym)
          case Result.Err(_) =>
            // We don't know the kind, so default to Star.
            val error = ResolutionError.UndefinedKind(qname, ns0, loc)
            sctx.errors.add(error)
            Kind.Star
        }
      }
    case NamedAst.Kind.Arrow(k10, k20, _) =>
      val k1 = resolveKind(k10, scp0, ns0, root)
      val k2 = resolveKind(k20, scp0, ns0, root)
      Kind.Arrow(k1, k2)
  }

  /**
    * Performs name resolution on the given expression `exp0` in the namespace `ns0`.
    */
  private def resolveExp(exp0: NamedAst.Expr, scp0: LocalScope)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = exp0 match {
    case NamedAst.Expr.Ambiguous(qname, loc) =>
      // Special Case: We must check if we have a static field access, e.g. Math.PI
      if (qname.namespace.idents.length == 1) {
        // We may have an imported class name.
        val className = qname.namespace.idents.head
        scp0.get(className.name) match {
          case List(Resolution.JavaClass(clazz)) =>
            // We have a static field access.
            val fieldName = qname.ident
            JvmUtils.getField(clazz, fieldName.name, static = true) match {
              case Some(field) =>
                // Returns out of resolveExp
                return Validation.Success(ResolvedAst.Expr.GetStaticField(field, loc))
              case None =>
                val error = ResolutionError.UndefinedJvmStaticField(clazz, fieldName, loc)
                sctx.errors.add(error)
                return Validation.Success(ResolvedAst.Expr.Error(error))
            }
          case _ =>
          // Fallthrough to below.
        }
      }

      val exp = lookupQName(qname, scp0, ns0, root) match {
        case ResolvedQName.Def(defn) => visitDef(defn, loc)
        case ResolvedQName.Sig(sig) => visitSig(sig, loc)
        case ResolvedQName.Op(op) => visitOp(op, loc)
        case ResolvedQName.LocalDef(sym, fparams) => visitLocalDef(sym, fparams.length, loc)
        case ResolvedQName.Var(sym) => ResolvedAst.Expr.Var(sym, loc)
        case ResolvedQName.Tag(caze) => visitTag(caze, loc)
        case ResolvedQName.RestrictableTag(caze) => visitRestrictableTag(caze, isOpen = false, loc)
        case ResolvedQName.Error(e) => ResolvedAst.Expr.Error(e)
      }
      Validation.Success(exp)

    case NamedAst.Expr.Open(name, loc) =>
      val exp = lookupQName(name, scp0, ns0, root) match {
        case ResolvedQName.Def(defn) => visitDef(defn, loc)
        case ResolvedQName.Sig(sig) => visitSig(sig, loc)
        case ResolvedQName.Op(op) => visitOp(op, loc)
        case ResolvedQName.LocalDef(sym, fparams) => visitLocalDef(sym, fparams.length, loc)
        case ResolvedQName.Var(sym) => ResolvedAst.Expr.Var(sym, loc)
        case ResolvedQName.Tag(caze) => visitTag(caze, loc)
        case ResolvedQName.RestrictableTag(caze) => visitRestrictableTag(caze, isOpen = true, loc)
        case ResolvedQName.Error(e) => ResolvedAst.Expr.Error(e)
      }
      Validation.Success(exp)

    case NamedAst.Expr.OpenAs(name, exp, loc) =>
      val enumVal = lookupRestrictableEnum(name, scp0, ns0, root)
      val eVal = resolveExp(exp, scp0)
      mapN(enumVal, eVal) {
        case (enum0, e) => ResolvedAst.Expr.OpenAs(RestrictableEnumSymUse(enum0.sym, name.loc), e, loc)
      }

    case NamedAst.Expr.Hole(nameOpt, loc) =>
      val sym = nameOpt match {
        case None => Symbol.freshHoleSym(loc)
        case Some(name) => Symbol.mkHoleSym(ns0, name)
      }
      Validation.Success(ResolvedAst.Expr.Hole(sym, scp0, loc))

    case NamedAst.Expr.HoleWithExp(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.HoleWithExp(e, scp0, loc)
      }

    case NamedAst.Expr.Use(use, exp, loc) =>
      // Lookup the used name and add it to the scp
      use match {
        case NamedAst.UseOrImport.Use(qname, alias, _) =>
          // TODO NS-REFACTOR allowing relative uses here...
          flatMapN(lookupQualifiedName(qname, scp0, ns0, root)) {
            case decls =>
              val scp = decls.foldLeft(scp0) {
                case (acc, decl) => acc + (alias.name -> Resolution.Declaration(decl))
              }
              mapN(resolveExp(exp, scp)) {
                // TODO NS-REFACTOR: multiple uses here
                case e => ResolvedAst.Expr.Use(getSym(decls.head), alias, e, loc)
              }
          }

        case NamedAst.UseOrImport.Import(_, _, importLoc) => throw InternalCompilerException("unexpected import", importLoc)
      }

    case NamedAst.Expr.Cst(cst, loc) =>
      Validation.Success(ResolvedAst.Expr.Cst(cst, loc))

    case app@NamedAst.Expr.Apply(NamedAst.Expr.Ambiguous(qname, innerLoc), exps, outerLoc) =>
      // Special Case: We must check if we have a static method call, i.e. Math.abs(123)
      if (qname.namespace.idents.length == 1) {
        // We may have an imported class name.
        val className = qname.namespace.idents.head
        scp0.get(className.name) match {
          case List(Resolution.JavaClass(clazz)) =>
            // We have a static method call.
            val methodName = qname.ident
            val expsVal = traverse(exps)(resolveExp(_, scp0))
            // Check for a single Unit argument
            // Returns out of resolveExp
            return flatMapN(expsVal) {
              case ResolvedAst.Expr.Cst(Constant.Unit, _) :: Nil =>
                Validation.Success(ResolvedAst.Expr.InvokeStaticMethod(clazz, methodName, Nil, outerLoc))
              case es =>
                Validation.Success(ResolvedAst.Expr.InvokeStaticMethod(clazz, methodName, es, outerLoc))
            }
          case _ =>
          // Fallthrough to below.
        }
      }

      lookupQName(qname, scp0, ns0, root) match {
        case ResolvedQName.Def(defn) => visitApplyDef(defn, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.Sig(sig) => visitApplySig(sig, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.Op(op) => visitApplyOp(op, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.LocalDef(sym, fparams) => visitApplyLocalDef(sym, fparams.length, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.Var(_) => visitApplyClo(app, scp0)
        case ResolvedQName.Tag(caze) => visitApplyTag(caze, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.RestrictableTag(caze) => visitApplyRestrictableTag(caze, exps, isOpen = false, scp0, innerLoc, outerLoc)
        case ResolvedQName.Error(m) => visitApplyError(m, exps, scp0, outerLoc)
      }

    case app@NamedAst.Expr.Apply(NamedAst.Expr.Open(qname, innerLoc), exps, outerLoc) =>
      lookupQName(qname, scp0, ns0, root) match {
        case ResolvedQName.Def(defn) => visitApplyDef(defn, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.Sig(sig) => visitApplySig(sig, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.Op(op) => visitApplyOp(op, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.LocalDef(sym, fparams) => visitApplyLocalDef(sym, fparams.length, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.Var(_) => visitApplyClo(app, scp0)
        case ResolvedQName.Tag(caze) => visitApplyTag(caze, exps, scp0, innerLoc, outerLoc)
        case ResolvedQName.RestrictableTag(caze) => visitApplyRestrictableTag(caze, exps, isOpen = true, scp0, innerLoc, outerLoc)
        case ResolvedQName.Error(error) => Validation.Success(ResolvedAst.Expr.Error(error))
      }

    case app@NamedAst.Expr.Apply(_, _, _) =>
      visitApplyClo(app, scp0)

    case NamedAst.Expr.Lambda(fparam, exp, loc) =>
      val pVal = resolveFormalParam(fparam, scp0, taenv, ns0, root)
      flatMapN(pVal) {
        case p =>
          val scp = scp0 ++ mkFormalParamScp(List(p))
          val eVal = resolveExp(exp, scp)
          mapN(eVal) {
            case e => ResolvedAst.Expr.Lambda(p, e, allowSubeffecting = true, loc)
          }
      }

    case NamedAst.Expr.Unary(sop, exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.Unary(sop, e, loc)
      }

    case NamedAst.Expr.Binary(sop, exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.Binary(sop, e1, e2, loc)
      }

    case NamedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      val e3Val = resolveExp(exp3, scp0)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) => ResolvedAst.Expr.IfThenElse(e1, e2, e3, loc)
      }

    case NamedAst.Expr.Stm(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.Stm(e1, e2, loc)
      }

    case NamedAst.Expr.Discard(exp, loc) =>
      mapN(resolveExp(exp, scp0)) {
        case e => ResolvedAst.Expr.Discard(e, loc)
      }

    case NamedAst.Expr.Let(sym, exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val scp = scp0 ++ mkVarScp(sym)
      val e2Val = resolveExp(exp2, scp)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.Let(sym, e1, e2, loc)
      }

    case NamedAst.Expr.LocalDef(sym, fparams0, exp1, exp2, loc) =>
      val fparamsVal = traverse(fparams0)(resolveFormalParam(_, scp0, taenv, ns0, root))
      flatMapN(fparamsVal) {
        fparams =>
          val scp1 = scp0 ++ mkLocalDefScp(sym, fparams) ++ mkFormalParamScp(fparams)
          val exp1Val = resolveExp(exp1, scp1)
          val scp2 = scp0 ++ mkLocalDefScp(sym, fparams)
          val exp2Val = resolveExp(exp2, scp2)
          mapN(exp1Val, exp2Val) {
            case (e1, e2) => ResolvedAst.Expr.LocalDef(sym, fparams, e1, e2, loc)
          }
      }

    case NamedAst.Expr.Region(tpe, loc) =>
      Validation.Success(ResolvedAst.Expr.Region(tpe, loc))

    case NamedAst.Expr.Scope(sym, regSym, exp, loc) =>
      val scp = scp0 ++ mkVarScp(sym) ++ mkTypeVarScp(regSym)
      // Visit the body in the new scope
      val eVal = resolveExp(exp, scp)(scope.enter(regSym), ns0, taenv, sctx, root, flix)
      mapN(eVal) {
        e => ResolvedAst.Expr.Scope(sym, regSym, e, loc)
      }

    case NamedAst.Expr.Match(exp, rules, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.MatchRule(pat, guard, body, ruleLoc) =>
          val p = resolvePattern(pat, scp0, ns0, root)
          val scp = scp0 ++ mkPatternScp(p)
          val gVal = traverseOpt(guard)(resolveExp(_, scp))
          val bVal = resolveExp(body, scp)
          mapN(gVal, bVal) {
            case (g, b) => ResolvedAst.MatchRule(p, g, b, ruleLoc)
          }
      }

      val eVal = resolveExp(exp, scp0)
      val rsVal = rulesVal
      mapN(eVal, rsVal) {
        case (e, rs) => ResolvedAst.Expr.Match(e, rs, loc)
      }

    case NamedAst.Expr.TypeMatch(exp, rules, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.TypeMatchRule(sym, tpe, body, ruleLoc) =>
          val tVal = resolveType(tpe, None, Wildness.AllowWild, scp0, taenv, ns0, root)
          val scp = scp0 ++ mkVarScp(sym)
          val bVal = resolveExp(body, scp)
          mapN(tVal, bVal) {
            case (t, b) => ResolvedAst.TypeMatchRule(sym, t, b, ruleLoc)
          }
      }

      val eVal = resolveExp(exp, scp0)
      mapN(eVal, rulesVal) {
        case (e, rs) => ResolvedAst.Expr.TypeMatch(e, rs, loc)
      }

    case NamedAst.Expr.RestrictableChoose(star, exp, rules, loc) =>
      val expVal = resolveExp(exp, scp0)
      val rulesVal = traverse(rules) {
        case NamedAst.RestrictableChooseRule(pat0, body) =>
          val pVal = pat0 match {
            case NamedAst.RestrictableChoosePattern.Tag(qname, pat, tagLoc) =>
              val tagVal = lookupRestrictableTag(qname, scp0, ns0, root)
              val pats = pat.map {
                case NamedAst.RestrictableChoosePattern.Wild(patLoc) => ResolvedAst.RestrictableChoosePattern.Wild(patLoc)
                case NamedAst.RestrictableChoosePattern.Var(sym, patLoc) => ResolvedAst.RestrictableChoosePattern.Var(sym, patLoc)
                case NamedAst.RestrictableChoosePattern.Error(patLoc) => ResolvedAst.RestrictableChoosePattern.Error(patLoc)
              }
              mapN(tagVal) {
                case tag => ResolvedAst.RestrictableChoosePattern.Tag(RestrictableCaseSymUse(tag.sym, qname.loc), pats, tagLoc)
              }
            case NamedAst.RestrictableChoosePattern.Error(tagLoc) => Validation.Success(ResolvedAst.RestrictableChoosePattern.Error(tagLoc))
          }
          val scp = pat0 match {
            case NamedAst.RestrictableChoosePattern.Tag(_, pat, _) =>
              pat.foldLeft(scp0) {
                case (acc, NamedAst.RestrictableChoosePattern.Var(sym, _)) => acc + (sym.text -> Resolution.Var(sym))
                case (acc, NamedAst.RestrictableChoosePattern.Wild(_)) => acc
                case (acc, NamedAst.RestrictableChoosePattern.Error(_)) => acc
              }
            case NamedAst.RestrictableChoosePattern.Error(_) => scp0
          }

          val eVal = resolveExp(body, scp)
          flatMapN(pVal, eVal) {
            case (p, e) =>
              Validation.Success(ResolvedAst.RestrictableChooseRule(p, e))
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => ResolvedAst.Expr.RestrictableChoose(star, e, rs, loc)
      }

    case NamedAst.Expr.ExtensibleTag(label, exps, loc) =>
      val esVal = traverse(exps)(e => resolveExp(e, scp0))
      mapN(esVal) {
        es => ResolvedAst.Expr.ExtensibleTag(label, es, loc)
      }

    case NamedAst.Expr.Tuple(elms, loc) =>
      val esVal = traverse(elms)(e => resolveExp(e, scp0))
      mapN(esVal) {
        es => ResolvedAst.Expr.Tuple(es, loc)
      }

    case NamedAst.Expr.RecordSelect(base, label, loc) =>
      val bVal = resolveExp(base, scp0)
      mapN(bVal) {
        b => ResolvedAst.Expr.RecordSelect(b, label, loc)
      }

    case NamedAst.Expr.RecordExtend(label, value, rest, loc) =>
      val vVal = resolveExp(value, scp0)
      val rVal = resolveExp(rest, scp0)
      mapN(vVal, rVal) {
        case (v, r) => ResolvedAst.Expr.RecordExtend(label, v, r, loc)
      }

    case NamedAst.Expr.RecordRestrict(label, rest, loc) =>
      val rVal = resolveExp(rest, scp0)
      mapN(rVal) {
        r => ResolvedAst.Expr.RecordRestrict(label, r, loc)
      }

    case NamedAst.Expr.ArrayLit(exps, exp, loc) =>
      val expsVal = traverse(exps)(resolveExp(_, scp0))
      val expVal = resolveExp(exp, scp0)
      mapN(expsVal, expVal) {
        case (es, e) =>
          ResolvedAst.Expr.ArrayLit(es, e, loc)
      }

    case NamedAst.Expr.ArrayNew(exp1, exp2, exp3, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      val e3Val = resolveExp(exp3, scp0)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) =>
          ResolvedAst.Expr.ArrayNew(e1, e2, e3, loc)
      }

    case NamedAst.Expr.ArrayLoad(base, index, loc) =>
      val bVal = resolveExp(base, scp0)
      val iVal = resolveExp(index, scp0)
      mapN(bVal, iVal) {
        case (b, i) => ResolvedAst.Expr.ArrayLoad(b, i, loc)
      }

    case NamedAst.Expr.ArrayStore(base, index, elm, loc) =>
      val bVal = resolveExp(base, scp0)
      val iVal = resolveExp(index, scp0)
      val eVal = resolveExp(elm, scp0)
      mapN(bVal, iVal, eVal) {
        case (b, i, e) => ResolvedAst.Expr.ArrayStore(b, i, e, loc)
      }

    case NamedAst.Expr.ArrayLength(base, loc) =>
      val bVal = resolveExp(base, scp0)
      mapN(bVal) {
        b => ResolvedAst.Expr.ArrayLength(b, loc)
      }

    case NamedAst.Expr.StructNew(name, fields0, region0, loc) =>
      lookupStruct(name, scp0, ns0, root) match {
        case Result.Ok(st0) =>
          checkStructIsAccessible(st0, ns0, loc)
          val fieldsVal = traverse(fields0) {
            case (f, exp) =>
              val eVal = resolveExp(exp, scp0)
              val (idx, defLoc) = st0.indicesAndLocs.getOrElse(f, (0, SourceLocation.Unknown))
              val fieldSym = Symbol.mkStructFieldSym(st0.sym, idx, Name.Label(f.name, defLoc))
              val fieldSymUse = StructFieldSymUse(fieldSym, f.loc)
              mapN(eVal) {
                case e => (fieldSymUse, e)
              }
          }
          val regionVal = resolveExp(region0, scp0)
          val structNew = mapN(fieldsVal, regionVal) {
            case (fields, region) =>
              ResolvedAst.Expr.StructNew(st0.sym, fields, region, loc)
          }
          // Potential errors
          val providedFieldNames = fields0.map { case (k, _) => Name.Label(k.name, k.loc) }
          val expectedFieldNames = st0.fields.map(field => Name.Label(field.sym.name, field.sym.loc))
          val extraFields = providedFieldNames.diff(expectedFieldNames)
          val missingFields = expectedFieldNames.diff(providedFieldNames)

          val extraFieldErrors = extraFields.map(ResolutionError.ExtraStructFieldInNew(st0.sym, _, loc))
          val missingFieldErrors = missingFields.map(ResolutionError.MissingStructFieldInNew(st0.sym, _, loc))
          val errors0 = extraFieldErrors ++ missingFieldErrors
          val errors = if (errors0.nonEmpty) {
            errors0
          } else if (providedFieldNames != expectedFieldNames) {
            List(ResolutionError.IllegalFieldOrderInNew(st0.sym, providedFieldNames, expectedFieldNames, loc))
          } else {
            Nil
          }
          errors.foreach(sctx.errors.add)
          structNew
        case Result.Err(error) =>
          sctx.errors.add(error)
          Validation.Success(ResolvedAst.Expr.Error(error))
      }

    case NamedAst.Expr.StructGet(exp, field0, loc) =>
      lookupStructField(field0, scp0, ns0, root) match {
        case Result.Ok(field) =>
          val eVal = resolveExp(exp, scp0)
          val symUse = StructFieldSymUse(field.sym, field0.loc)
          mapN(eVal) {
            case e => ResolvedAst.Expr.StructGet(e, symUse, loc)
          }
        case Result.Err(error) =>
          sctx.errors.add(error)
          Validation.Success(ResolvedAst.Expr.Error(error))
      }

    case NamedAst.Expr.StructPut(exp1, field0, exp2, loc) =>
      lookupStructField(field0, scp0, ns0, root) match {
        case Result.Ok(field) =>
          val e1Val = resolveExp(exp1, scp0)
          val e2Val = resolveExp(exp2, scp0)
          val symUse = StructFieldSymUse(field.sym, field0.loc)
          if (!field.mod.isMutable) {
            val error = ResolutionError.ImmutableField(field.sym, field0.loc)
            sctx.errors.add(error)
          }
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expr.StructPut(e1, symUse, e2, loc)
          }

        case Result.Err(error) =>
          sctx.errors.add(error)
          Validation.Success(ResolvedAst.Expr.Error(error))
      }

    case NamedAst.Expr.VectorLit(exps, loc) =>
      val expsVal = traverse(exps)(resolveExp(_, scp0))
      mapN(expsVal) {
        case es => ResolvedAst.Expr.VectorLit(es, loc)
      }

    case NamedAst.Expr.VectorLoad(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.VectorLoad(e1, e2, loc)
      }

    case NamedAst.Expr.VectorLength(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.VectorLength(e, loc)
      }

    case NamedAst.Expr.Ascribe(exp, expectedType, expectedEff, loc) =>
      val expectedTypVal = traverseOpt(expectedType)(resolveType(_, Some(Kind.Star), Wildness.AllowWild, scp0, taenv, ns0, root))
      val expectedEffVal = traverseOpt(expectedEff)(resolveType(_, Some(Kind.Eff), Wildness.AllowWild, scp0, taenv, ns0, root))

      val eVal = resolveExp(exp, scp0)
      mapN(eVal, expectedTypVal, expectedEffVal) {
        case (e, t, f) => ResolvedAst.Expr.Ascribe(e, t, f, loc)
      }

    case NamedAst.Expr.InstanceOf(exp, className, loc) =>
      flatMapN(resolveExp(exp, scp0)) {
        case e => scp0.get(className.name) match {
          case List(Resolution.JavaClass(clazz)) =>
            Validation.Success(ResolvedAst.Expr.InstanceOf(e, clazz, loc))
          case _ =>
            val error = ResolutionError.UndefinedJvmClass(className, AnchorPosition.mkImportOrUseAnchor(ns0), "", loc)
            sctx.errors.add(error)
            Validation.Success(ResolvedAst.Expr.Error(error))
        }
      }

    case NamedAst.Expr.CheckedCast(c, exp, loc) =>
      mapN(resolveExp(exp, scp0)) {
        case e => ResolvedAst.Expr.CheckedCast(c, e, loc)
      }

    case NamedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, loc) =>
      val declaredTypVal = traverseOpt(declaredType)(resolveType(_, Some(Kind.Star), Wildness.ForbidWild, scp0, taenv, ns0, root))
      val declaredEffVal = traverseOpt(declaredEff)(resolveType(_, Some(Kind.Eff), Wildness.ForbidWild, scp0, taenv, ns0, root))

      val eVal = resolveExp(exp, scp0)
      mapN(eVal, declaredTypVal, declaredEffVal) {
        case (e, t, f) => ResolvedAst.Expr.UncheckedCast(e, t, f, loc)
      }

    case NamedAst.Expr.Unsafe(exp, eff0, loc) =>
      val eVal = resolveExp(exp, scp0)
      val effVal = resolveType(eff0, Some(Kind.Eff), Wildness.ForbidWild, scp0, taenv, ns0, root)
      mapN(eVal, effVal) {
        case (e, eff) => ResolvedAst.Expr.Unsafe(e, eff, loc)
      }

    case NamedAst.Expr.TryCatch(exp, rules, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.CatchRule(sym, className, body, ruleLoc) =>
          val scp = scp0 ++ mkVarScp(sym)
          val clazzVal = lookupJvmClass2(className, ns0, scp0).toValidation
          val bVal = resolveExp(body, scp)
          mapN(clazzVal, bVal) {
            case (clazz, b) => ResolvedAst.CatchRule(sym, clazz, b, ruleLoc)
          }
      }

      val eVal = resolveExp(exp, scp0)
      mapN(eVal, rulesVal) {
        case (e, rs) => ResolvedAst.Expr.TryCatch(e, rs, loc)
      }

    case NamedAst.Expr.Throw(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.Throw(e, loc)
      }

    case NamedAst.Expr.Without(exp, qname, loc) =>
      lookupEffect(qname, scp0, ns0, root) match {
        case Result.Ok(decl) =>
          checkEffectIsAccessible(decl, ns0, qname.loc)
          val symUse = EffectSymUse(decl.sym, qname)
          val expVal = resolveExp(exp, scp0)
          mapN(expVal) {
            case e => ResolvedAst.Expr.Without(e, symUse, loc)
          }
        case Result.Err(error) =>
          sctx.errors.add(error)
          Validation.Success(ResolvedAst.Expr.Error(error))
      }

    case NamedAst.Expr.Handler(qname, rules, loc) =>
      val handlerVal = visitHandler(qname, rules, scp0)
      mapN(handlerVal) {
        case Result.Ok((symUse, rs)) =>
          ResolvedAst.Expr.Handler(symUse, rs, loc)
        case Result.Err(error) =>
          sctx.errors.add(error)
          ResolvedAst.Expr.Error(error)
      }

    case NamedAst.Expr.RunWith(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.RunWith(e1, e2, loc)
      }

    case NamedAst.Expr.InvokeConstructor(className, exps, loc) =>
      val esVal = traverse(exps)(resolveExp(_, scp0))
      flatMapN(esVal) {
        es =>
          scp0.get(className.name) match {
            case List(Resolution.JavaClass(clazz)) =>
              Validation.Success(ResolvedAst.Expr.InvokeConstructor(clazz, es, loc))
            case _ =>
              val error = ResolutionError.UndefinedNewJvmClassOrStruct(className, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, "", loc)
              sctx.errors.add(error)
              Validation.Success(ResolvedAst.Expr.Error(error))
          }
      }

    case NamedAst.Expr.InvokeMethod(exp, name, exps, loc) =>
      val eVal = resolveExp(exp, scp0)
      val esVal = traverse(exps)(resolveExp(_, scp0))
      mapN(eVal, esVal) {
        case (e, es) =>
          ResolvedAst.Expr.InvokeMethod(e, name, es, loc)
      }

    case NamedAst.Expr.GetField(exp, name, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        case e =>
          ResolvedAst.Expr.GetField(e, name, loc)
      }

    case NamedAst.Expr.NewObject(name, tpe, methods, loc) =>
      flatMapN(resolveType(tpe, Some(Kind.Star), Wildness.ForbidWild, scp0, taenv, ns0, root), traverse(methods)(visitJvmMethod(_, scp0))) {
        case (t, ms) =>
          //
          // Check that the type is a JVM type (after type alias erasure).
          //
          UnkindedType.eraseAliases(t) match {
            case UnkindedType.Cst(TypeConstructor.Native(clazz), _) =>
              Validation.Success(ResolvedAst.Expr.NewObject(name, clazz, ms, loc))
            case _ =>
              val error = ResolutionError.IllegalNonJavaType(t, t.loc)
              sctx.errors.add(error)
              Validation.Success(ResolvedAst.Expr.Error(error))
          }
      }

    case NamedAst.Expr.NewChannel(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.NewChannel(e, loc)
      }

    case NamedAst.Expr.GetChannel(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.GetChannel(e, loc)
      }

    case NamedAst.Expr.PutChannel(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.PutChannel(e1, e2, loc)
      }

    case NamedAst.Expr.SelectChannel(rules, default, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.SelectChannelRule(sym, chan, body, ruleLoc) =>
          val cVal = resolveExp(chan, scp0)
          val scp = scp0 ++ mkVarScp(sym)
          val bVal = resolveExp(body, scp)
          mapN(cVal, bVal) {
            case (c, b) => ResolvedAst.SelectChannelRule(sym, c, b, ruleLoc)
          }
      }

      val defaultVal = default match {
        case Some(exp) =>
          val eVal = resolveExp(exp, scp0)
          mapN(eVal) {
            e => Some(e)
          }
        case None =>
          Validation.Success(None)
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => ResolvedAst.Expr.SelectChannel(rs, d, loc)
      }

    case NamedAst.Expr.Spawn(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) =>
          ResolvedAst.Expr.Spawn(e1, e2, loc)
      }

    case NamedAst.Expr.ParYield(frags, exp, loc) =>
      // mutable LocalScope to be updated during traversal
      var finalUscp = scp0

      val fragsVal = traverse(frags) {
        case NamedAst.ParYieldFragment(pat, e0, l0) =>
          val p = resolvePattern(pat, scp0, ns0, root)
          val patScp = mkPatternScp(p)
          val scp = scp0 ++ patScp
          finalUscp = finalUscp ++ patScp
          val e0Val = resolveExp(e0, scp)
          mapN(e0Val) {
            case e1 => ResolvedAst.ParYieldFragment(p, e1, l0)
          }
      }

      mapN(fragsVal, resolveExp(exp, finalUscp)) {
        case (fs, e) => ResolvedAst.Expr.ParYield(fs, e, loc)
      }

    case NamedAst.Expr.Lazy(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.Lazy(e, loc)
      }

    case NamedAst.Expr.Force(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.Force(e, loc)
      }

    case NamedAst.Expr.FixpointConstraintSet(cs0, loc) =>
      val csVal = traverse(cs0)(resolveConstraint(_, scp0))
      mapN(csVal) {
        cs => ResolvedAst.Expr.FixpointConstraintSet(cs, loc)
      }

    case NamedAst.Expr.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(resolvePredicateParam(_, scp0))
      val eVal = resolveExp(exp, scp0)
      mapN(psVal, eVal) {
        case (ps, e) => ResolvedAst.Expr.FixpointLambda(ps, e, loc)
      }

    case NamedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.FixpointMerge(e1, e2, loc)
      }

    case NamedAst.Expr.FixpointSolve(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.FixpointSolve(e, loc)
      }

    case NamedAst.Expr.FixpointFilter(pred, exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.FixpointFilter(pred, e, loc)
      }

    case NamedAst.Expr.FixpointInject(exp, pred, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Expr.FixpointInject(e, pred, loc)
      }

    case NamedAst.Expr.FixpointProject(pred, exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, scp0)
      val e2Val = resolveExp(exp2, scp0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.FixpointProject(pred, e1, e2, loc)
      }

    case NamedAst.Expr.Error(m) =>
      Validation.Success(ResolvedAst.Expr.Error(m))

  }

  /**
    * Creates `arity` fresh fparams for use in a curried def or sig application.
    */
  private def mkFreshFparams(arity: Int, loc: SourceLocation)(implicit scope: Scope, flix: Flix): List[ResolvedAst.FormalParam] = {
    // Introduce a fresh variable symbol for each argument of the function definition.
    val varSyms = (0 until arity).map(i => freshVarSym("arg" + i, BoundBy.FormalParam, loc)).toList

    // Introduce a formal parameter for each variable symbol.
    varSyms.map(sym => ResolvedAst.FormalParam(sym, Modifiers.Empty, None, loc))
  }

  /**
    * Returns a curried lambda and application of `caze`.
    *
    *   - `Cons ===> x -> y -> Cons(x, y)`
    */
  private def visitTag(caze: NamedAst.Declaration.Case, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val base = es => ResolvedAst.Expr.Tag(CaseSymUse(caze.sym, loc), es, loc.asSynthetic)
    visitApplyFull(base, caze.tpes.length, Nil, loc.asSynthetic)
  }

  /**
    * Returns a curried lambda and application of `caze`.
    *
    *   - `Add ===> x -> y -> Add(x, y)`
    */
  private def visitRestrictableTag(caze: NamedAst.Declaration.RestrictableCase, isOpen: Boolean, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val base = es => ResolvedAst.Expr.RestrictableTag(RestrictableCaseSymUse(caze.sym, loc), es, isOpen, loc.asSynthetic)
    visitApplyFull(base, caze.tpes.length, Nil, loc.asSynthetic)
  }

  /**
    * Resolve the application expression, performing currying over the subexpressions.
    */
  private def visitApplyClo(exp: NamedAst.Expr.Apply, scp0: LocalScope)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = exp match {
    case NamedAst.Expr.Apply(exp0, exps0, loc) =>
      val expVal = resolveExp(exp0, scp0)
      val expsVal = traverse(exps0)(resolveExp(_, scp0))
      mapN(expVal, expsVal) {
        case (e, es) => mkApplyClo(e, es, loc.asSynthetic)
      }
  }

  /**
    * Resolves the exps and creates an ApplyClo node applying an error node to the exps.
    */
  private def visitApplyError(err: CompilationMessage, exps: List[NamedAst.Expr], scp0: LocalScope, loc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    val expsVal = traverse(exps)(resolveExp(_, scp0))
    val exp: ResolvedAst.Expr = ResolvedAst.Expr.Error(err)
    mapN(expsVal) {
      case es => mkApplyClo(exp, es, loc.asSynthetic)
    }
  }

  /** A trait used by [[visitApplyFull]] to let-bind some partially-applied arguments. */
  private sealed trait Arg {
    def arg: ResolvedAst.Expr
  }

  private object Arg {

    /** Expression is captured as is. */
    case class Capture(arg: ResolvedAst.Expr) extends Arg

    /** Expression `bound` is let-bound under the name `ref`. */
    case class Bind(arg: ResolvedAst.Expr.Var, toBind: ResolvedAst.Expr) extends Arg

  }

  /**
    * Resolve the application of a top-level function or signature `base` with some `arity`.
    *
    * Example with `def f(a: Char, b: Char): Char -> Char`
    *   - under applied: `   f     ===> x -> y -> f(x, y)`
    *   - under applied: `  f(a)   ===> let x = a; y -> f(x, y)`
    *   - fully applied: ` f(a,b)  ===> f(a, b)`
    *   - over applied:  `f(a,b,c) ===> f(a, b)(c)`
    */
  private def visitApplyFull(base: List[ResolvedAst.Expr] => ResolvedAst.Expr, arity: Int, exps: List[ResolvedAst.Expr], loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val argsGiven = exps.length
    if (argsGiven < arity) {
      // Case: under applied.

      val argInfo = captureArgs(exps, loc.asSynthetic)
      // The arguments to `base`.
      val fparamsPadding = mkFreshFparams(arity - exps.length, loc.asSynthetic)
      val argsPadding = fparamsPadding.map(fp => ResolvedAst.Expr.Var(fp.sym, loc.asSynthetic))

      val fullDefApplication = base(argInfo.map(_.arg) ++ argsPadding)

      val fullDefLambda = mkPrefixPureLambda(fparamsPadding, fullDefApplication, loc.asSynthetic)

      // Let-bind the arguments that require it.
      argInfo.foldRight(fullDefLambda) {
        case (Arg.Capture(_), acc) => acc
        case (Arg.Bind(arg, toBind), acc) => ResolvedAst.Expr.Let(arg.sym, toBind, acc, loc.asSynthetic)
      }
    } else if (argsGiven == arity) {
      // Case: fully applied.
      base(exps)
    } else {
      // Case: over applied.
      val (directArgs, cloArgs) = exps.splitAt(arity)
      val defApplication = base(directArgs)
      mkApplyClo(defApplication, cloArgs, loc.asSynthetic)
    }
  }

  /**
    * Returns a lambda `(fp_0, fp_1, .., fp_n) -> body \ ef` where all but the last lambda is pure.
    */
  private def mkPrefixPureLambda(fparams: List[ResolvedAst.FormalParam], body: ResolvedAst.Expr, loc: SourceLocation): ResolvedAst.Expr = {
    val (lambda, _) = fparams.foldRight((body, true)) {
      case (fp, (acc, first)) =>
        if (first) (ResolvedAst.Expr.Lambda(fp, acc, allowSubeffecting = false, loc.asSynthetic), false)
        else (mkPureLambda(fp, acc, loc.asSynthetic), false)
    }
    lambda
  }

  /**
    * Returns `exps` where [[Arg.Capture]] marks expressions that can be captured and [[Arg.Bind]]
    * must be let-bound.
    *
    * For `List("x", "Ref.get(y)")`, `List(Capture("x"), Bind("tmp$123", "Ref.get(y)"))` is returned.
    */
  private def captureArgs(exps: List[ResolvedAst.Expr], loc: SourceLocation)(implicit scope: Scope, flix: Flix): List[Arg] = {
    exps.map {
      case cst@ResolvedAst.Expr.Cst(_, _) => Arg.Capture(cst)
      case v@ResolvedAst.Expr.Var(_, _) => Arg.Capture(v)
      case other =>
        val varSym = freshVarSym("arg", BoundBy.Let, loc.asSynthetic)
        Arg.Bind(ResolvedAst.Expr.Var(varSym, loc.asSynthetic), other)
    }
  }

  /** Returns `base(arg_0)(arg_1)..(arg_n)`. */
  private def mkApplyClo(base: ResolvedAst.Expr, args: List[ResolvedAst.Expr], loc: SourceLocation): ResolvedAst.Expr = {
    args.foldLeft(base) {
      case (acc, cloArg) => ResolvedAst.Expr.ApplyClo(acc, cloArg, loc.asSynthetic)
    }
  }

  /**
    * Returns a curried lambda and application of `defn`.
    *
    *   - `Int32.add ===> x -> y -> Int32.add(x, y)`
    */
  private def visitDef(defn: NamedAst.Declaration.Def, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val base = es => ResolvedAst.Expr.ApplyDef(DefSymUse(defn.sym, loc), es, loc.asSynthetic)
    visitApplyFull(base, defn.spec.fparams.length, Nil, loc.asSynthetic)
  }

  /**
    * Resolve the application expression, applying `defn` to `exps`.
    *
    * Example with `def f(a: Char, b: Char): Char -> Char`
    *   - `   f     ===> x -> y -> f(x, y)`
    *   - `  f(a)   ===> x -> f(a, x)`
    *   - ` f(a,b)  ===> f(a, b)`
    *   - `f(a,b,c) ===> f(a, b)(c)`
    */
  private def visitApplyDef(defn: NamedAst.Declaration.Def, exps: List[NamedAst.Expr], scp0: LocalScope, innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    mapN(traverse(exps)(resolveExp(_, scp0))) {
      es =>
        val base = args => ResolvedAst.Expr.ApplyDef(DefSymUse(defn.sym, innerLoc), args, outerLoc)
        visitApplyFull(base, defn.spec.fparams.length, es, outerLoc)
    }
  }

  /**
    * Returns a curried lambda and application of `sig`.
    *
    *   - `Add.add ===> x -> y -> Add.add(x, y)`
    */
  private def visitSig(sig: NamedAst.Declaration.Sig, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val base = es => ResolvedAst.Expr.ApplySig(SigSymUse(sig.sym, loc), es, loc.asSynthetic)
    visitApplyFull(base, sig.spec.fparams.length, Nil, loc.asSynthetic)
  }

  /**
    * Resolve the application expression, applying `sig` to `exps`.
    *
    * Example with `def f(a: Char, b: Char): Char -> Char`
    *   - `   f     ===> x -> y -> f(x, y)`
    *   - `  f(a)   ===> x -> f(a, x)`
    *   - ` f(a,b)  ===> f(a, b)`
    *   - `f(a,b,c) ===> f(a, b)(c)`
    */
  private def visitApplySig(sig: NamedAst.Declaration.Sig, exps: List[NamedAst.Expr], scp0: LocalScope, innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    mapN(traverse(exps)(resolveExp(_, scp0))) {
      es =>
        val base = args => ResolvedAst.Expr.ApplySig(SigSymUse(sig.sym, innerLoc), args, outerLoc)
        visitApplyFull(base, sig.spec.fparams.length, es, outerLoc)
    }
  }

  /**
    * Returns a curried lambda and application of `sym`.
    *
    *   - `loop ===> x -> y -> loop(x, y)`
    */
  private def visitLocalDef(sym: Symbol.VarSym, arity: Int, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val base = es => ResolvedAst.Expr.ApplyLocalDef(LocalDefSymUse(sym, loc), es, loc.asSynthetic)
    visitApplyFull(base, arity, Nil, loc.asSynthetic)
  }

  /**
    * Resolve the application expression, applying `sym` to `exps`.
    *
    * Example with `def f(a: Char, b: Char): Char -> Char`
    *   - `   f     ===> x -> y -> f(x, y)`
    *   - `  f(a)   ===> x -> f(a, x)`
    *   - ` f(a,b)  ===> f(a, b)`
    *   - `f(a,b,c) ===> f(a, b)(c)`
    */
  private def visitApplyLocalDef(sym: Symbol.VarSym, arity: Int, exps: List[NamedAst.Expr], scp0: LocalScope, innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    mapN(traverse(exps)(resolveExp(_, scp0))) {
      es =>
        val base = args => ResolvedAst.Expr.ApplyLocalDef(LocalDefSymUse(sym, innerLoc), args, outerLoc)
        visitApplyFull(base, arity, es, outerLoc)
    }
  }

  /**
    * Returns a curried lambda and application of `defn`.
    *
    *   - `Int32.add ===> x -> y -> Int32.add(x, y)`
    */
  private def visitOp(op: NamedAst.Declaration.Op, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    val base = es => ResolvedAst.Expr.Do(OpSymUse(op.sym, loc), es, loc.asSynthetic)
    visitApplyFull(base, op.spec.fparams.length, Nil, loc.asSynthetic)
  }

  /**
    * Resolve the application expression, applying `op` to `exps`.
    *
    * Example with `def f(a: Char, b: Char): Char -> Char`
    *   - `   f     ===> x -> y -> f(x, y)`
    *   - `  f(a)   ===> x -> f(a, x)`
    *   - ` f(a,b)  ===> f(a, b)`
    *   - `f(a,b,c) ===> f(a, b)(c)`
    */
  private def visitApplyOp(op: NamedAst.Declaration.Op, exps: List[NamedAst.Expr], scp0: LocalScope, innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    mapN(traverse(exps)(resolveExp(_, scp0))) {
      es =>
        val base = args => ResolvedAst.Expr.Do(OpSymUse(op.sym, innerLoc), args, outerLoc)
        visitApplyFull(base, op.spec.fparams.length, es, outerLoc)
    }
  }

  /**
    * Resolve the tag application.
    *
    * Example with `Cons(a, List[a])`
    *   - `   Cons     ===> x -> y -> Cons(x, y)`
    *   - `  Cons(a)   ===> x -> Cons(a, x)`
    *   - ` Cons(a,b)  ===> Cons(a, b)`
    *   - `Cons(a,b,c) ===> Cons(a, b)(c)`
    */
  private def visitApplyTag(caze: NamedAst.Declaration.Case, exps: List[NamedAst.Expr], scp0: LocalScope, innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    mapN(traverse(exps)(resolveExp(_, scp0))) {
      es =>
        val base = args => ResolvedAst.Expr.Tag(CaseSymUse(caze.sym, innerLoc), args, outerLoc)
        visitApplyFull(base, caze.tpes.length, es, outerLoc)
    }
  }

  /**
    * Resolve the tag application.
    *
    * Example with `Add(Exp, Exp)`
    *   - `   Add     ===> x -> y -> Add(x, y)`
    *   - `  Add(a)   ===> x -> Add(a, x)`
    *   - ` Add(a,b)  ===> Add(a, b)`
    *   - `Add(a,b,c) ===> Add(a, b)(c)`
    */
  private def visitApplyRestrictableTag(caze: NamedAst.Declaration.RestrictableCase, exps: List[NamedAst.Expr], isOpen: Boolean, scp0: LocalScope, innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    mapN(traverse(exps)(resolveExp(_, scp0))) {
      es =>
        val base = args => ResolvedAst.Expr.RestrictableTag(RestrictableCaseSymUse(caze.sym, innerLoc), args, isOpen, outerLoc)
        visitApplyFull(base, caze.tpes.length, es, outerLoc)
    }
  }

  /**
    * Performs name resolution on the given JvmMethod `method` in the namespace `ns0`.
    */
  private def visitJvmMethod(method: NamedAst.JvmMethod, scp0: LocalScope)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.JvmMethod, ResolutionError] = method match {
    case NamedAst.JvmMethod(ident, fparams0, exp, tpe, eff, loc) =>
      val fparamsVal = traverse(fparams0)(resolveFormalParam(_, scp0, taenv, ns0, root))
      flatMapN(fparamsVal) {
        case fparams =>
          val scp = scp0 ++ mkFormalParamScp(fparams)
          val expVal = resolveExp(exp, scp)
          val tpeVal = resolveType(tpe, Some(Kind.Star), Wildness.ForbidWild, scp, taenv, ns0, root)
          val effVal = traverseOpt(eff)(resolveType(_, Some(Kind.Eff), Wildness.ForbidWild, scp, taenv, ns0, root))
          mapN(expVal, tpeVal, effVal) {
            case (e, t, p) => ResolvedAst.JvmMethod(ident, fparams, e, t, p, loc)
          }
      }
  }

  /**
    * Performs name resolution on the handler that handles `eff` with rules `rules0`.
    */
  // TODO: the nested Result/Validation is ugly here, but should be fixed by the Validation removal refactoring
  private def visitHandler(qname: Name.QName, rules0: List[NamedAst.HandlerRule], scp0: LocalScope)(implicit scope: Scope, ns: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[Result[(EffectSymUse, List[ResolvedAst.HandlerRule]), ResolutionError.UndefinedEffect], ResolutionError] = {
    lookupEffect(qname, scp0, ns, root) match {
      case Result.Ok(decl) =>
        checkEffectIsAccessible(decl, ns, qname.loc)
        val symUse = EffectSymUse(decl.sym, qname)
        val rulesVal = traverse(rules0) {
          case NamedAst.HandlerRule(ident, fparams, body, loc) =>
            val opVal = findOpInEffect(ident, decl, ns, scp0)
            val fparamsVal = traverse(fparams)(resolveFormalParam(_, scp0, taenv, ns, root))
            flatMapN(opVal, fparamsVal) {
              case (o, fp) =>
                val scp = scp0 ++ mkFormalParamScp(fp)
                checkOpArity(o, fp.length - 1, ident.loc)
                val bodyVal = resolveExp(body, scp)
                mapN(bodyVal) {
                  case b =>
                    ResolvedAst.HandlerRule(OpSymUse(o.sym, ident.loc), fp, b, loc)
                }
            }
        }
        mapN(rulesVal) {
          case rules =>
            // Check that all the operations have respective definitions
            val allOps = decl.ops.map(_.sym)
            val missingDefs = allOps.toSet -- rules.map(_.symUse.sym)
            missingDefs.foreach {
              case sym => sctx.errors.add(ResolutionError.MissingHandlerDef(sym, qname.loc))
            }
            Result.Ok((symUse, rules))
        }
      case Result.Err(error) =>
        Validation.Success(Result.Err(error))
    }
  }


  /**
    * Performs name resolution on the given constraint pattern `pat0` in the namespace `ns0`.
    * Constraint patterns do not introduce new variables.
    */
  private def resolvePatternInConstraint(pat0: NamedAst.Pattern, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): ResolvedAst.Pattern = {

    def visit(p0: NamedAst.Pattern): ResolvedAst.Pattern = p0 match {
      case NamedAst.Pattern.Wild(loc) =>
        ResolvedAst.Pattern.Wild(loc)

      case NamedAst.Pattern.Var(sym0, loc) =>
        // TODO NS-REFACTOR wild patterns should not be counted as vars
        // if the sym is wild then just call the pattern wild
        if (sym0.isWild) {
          ResolvedAst.Pattern.Wild(loc)
        } else {
          scp0(sym0.text).collectFirst {
            case Resolution.Var(sym) => sym
          } match {
            case Some(sym) =>
              ResolvedAst.Pattern.Var(sym, loc)
            case None => throw InternalCompilerException("unexpected unrecognized sym in constraint pattern", loc)
          }
        }

      case NamedAst.Pattern.Cst(cst, loc) =>
        ResolvedAst.Pattern.Cst(cst, loc)

      case NamedAst.Pattern.Tag(qname, pats, loc) =>
        lookupTag(qname, scp0, ns0, root) match {
          case Result.Ok(c) =>
            val ps = pats.map(visit)
            ResolvedAst.Pattern.Tag(CaseSymUse(c.sym, qname.loc), ps, loc)
          case Result.Err(error) =>
            sctx.errors.add(error)
            ResolvedAst.Pattern.Error(loc)
        }

      case NamedAst.Pattern.Tuple(elms, loc) =>
        val es = elms.map(visit)
        ResolvedAst.Pattern.Tuple(es, loc)

      case NamedAst.Pattern.Record(pats, pat, loc) =>
        val ps = pats.map {
          case NamedAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
            val p = visit(pat1)
            ResolvedAst.Pattern.Record.RecordLabelPattern(label, p, loc1)
        }
        val p = visit(pat)
        ResolvedAst.Pattern.Record(ps, p, loc)

      case NamedAst.Pattern.Error(loc) =>
        ResolvedAst.Pattern.Error(loc)
    }

    visit(pat0)
  }

  /**
    * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
    */
  private def resolvePattern(pat0: NamedAst.Pattern, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): ResolvedAst.Pattern = {

    def visit(p0: NamedAst.Pattern): ResolvedAst.Pattern = p0 match {
      case NamedAst.Pattern.Wild(loc) =>
        ResolvedAst.Pattern.Wild(loc)

      case NamedAst.Pattern.Var(sym, loc) =>
        ResolvedAst.Pattern.Var(sym, loc)

      case NamedAst.Pattern.Cst(cst, loc) =>
        ResolvedAst.Pattern.Cst(cst, loc)

      case NamedAst.Pattern.Tag(qname, pats, loc) =>
        lookupTag(qname, scp0, ns0, root) match {
          case Result.Ok(c) =>
            val ps0 = pats.map(visit)
            // Check if the number of arguments match.
            val ps = if (ps0.lengthCompare(c.tpes) != 0) {
              val expectedTerms = c.tpes.length
              val error = ResolutionError.MismatchedTagPatternArity(c.sym, expected = expectedTerms, actual = ps0.length, loc)
              sctx.errors.add(error)
              // Maintain as many sensible arguments as possible to allow further type checking, etc.
              ps0.take(expectedTerms).padTo(expectedTerms, ResolvedAst.Pattern.Error(loc.asSynthetic))
            } else {
              ps0
            }
            ResolvedAst.Pattern.Tag(CaseSymUse(c.sym, qname.loc), ps, loc)
          case Result.Err(error) =>
            sctx.errors.add(error)
            ResolvedAst.Pattern.Error(loc)
        }

      case NamedAst.Pattern.Tuple(elms, loc) =>
        val es = elms.map(visit)
        ResolvedAst.Pattern.Tuple(es, loc)

      case NamedAst.Pattern.Record(pats, pat, loc) =>
        val ps = pats.map {
          case NamedAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
            val p = visit(pat1)
            ResolvedAst.Pattern.Record.RecordLabelPattern(label, p, loc1)
        }
        val p = visit(pat)
        ResolvedAst.Pattern.Record(ps, p, loc)

      case NamedAst.Pattern.Error(loc) =>
        ResolvedAst.Pattern.Error(loc)
    }

    visit(pat0)
  }

  /**
    * Performs name resolution on the given head predicate `h0` in the given namespace `ns0`.
    */
  private def resolvePredicateHead(h0: NamedAst.Predicate.Head, scp0: LocalScope)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
    case NamedAst.Predicate.Head.Atom(pred, den, terms, loc) =>
      val tsVal = traverse(terms)(t => resolveExp(t, scp0))
      mapN(tsVal) {
        ts => ResolvedAst.Predicate.Head.Atom(pred, den, ts, loc)
      }
  }

  /**
    * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
    */
  private def resolvePredicateBody(b0: NamedAst.Predicate.Body, scp0: LocalScope)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
    case NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
      val ts = terms.map(resolvePatternInConstraint(_, scp0, ns0, root))
      Validation.Success(ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc))

    case NamedAst.Predicate.Body.Functional(idents, exp, loc) =>
      val syms = idents.map {
        case ident => scp0(ident.name).collectFirst {
          case Resolution.Var(sym) => sym
        }.getOrElse(throw InternalCompilerException(s"Unbound variable in functional predicate: '$ident'.", ident.loc))
      }
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        case e => ResolvedAst.Predicate.Body.Functional(syms, e, loc)
      }

    case NamedAst.Predicate.Body.Guard(exp, loc) =>
      val eVal = resolveExp(exp, scp0)
      mapN(eVal) {
        e => ResolvedAst.Predicate.Body.Guard(e, loc)
      }
  }

  /**
    * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
    */
  private def resolveFormalParam(fparam0: NamedAst.FormalParam, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[ResolvedAst.FormalParam, ResolutionError] = {
    val tVal = traverseOpt(fparam0.tpe)(resolveType(_, Some(Kind.Star), Wildness.AllowWild, scp0, taenv, ns0, root))
    mapN(tVal) {
      t => ResolvedAst.FormalParam(fparam0.sym, fparam0.mod, t, fparam0.loc)
    }
  }

  /**
    * Performs name resolution on the given predicate parameter `pparam0` in the given namespace `ns0`.
    */
  private def resolvePredicateParam(pparam0: NamedAst.PredicateParam, scp0: LocalScope)(implicit ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], scope: Scope, sctx: SharedContext, root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.PredicateParam, ResolutionError] = pparam0 match {
    case NamedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      Validation.Success(ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc))

    case NamedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(resolveType(_, Some(Kind.Star), Wildness.ForbidWild, scp0, taenv, ns0, root)(scope, sctx, flix))) {
        case ts => ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
      }

  }

  /**
    * Performs name resolution on the given type parameter `tparam0` in the given namespace `ns0`.
    */
  private def resolveTypeParam(tparam0: NamedAst.TypeParam, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Validation[ResolvedAst.TypeParam, ResolutionError] = tparam0 match {
    case tparam: NamedAst.TypeParam.Kinded => resolveKindedTypeParam(tparam, scp0, ns0, root)
    case tparam: NamedAst.TypeParam.Unkinded => Validation.Success(resolveUnkindedTypeParam(tparam))
    case tparam: NamedAst.TypeParam.Implicit => throw InternalCompilerException("unexpected implicit tparam", tparam.loc)
  }

  /**
    * Performs name resolution on the given kinded type parameter `tparam0` in the given namespace `ns0`.
    */
  private def resolveKindedTypeParam(tparam0: NamedAst.TypeParam.Kinded, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Validation[ResolvedAst.TypeParam.Kinded, ResolutionError] = tparam0 match {
    case NamedAst.TypeParam.Kinded(name, sym, kind0, loc) =>
      val kind = resolveKind(kind0, scp0, ns0, root)
      Validation.Success(ResolvedAst.TypeParam.Kinded(name, sym, kind, loc))
  }

  /**
    * Performs name resolution on the given unkinded type parameter `tparam0` in the given namespace `ns0`.
    */
  private def resolveUnkindedTypeParam(tparam0: NamedAst.TypeParam.Unkinded): ResolvedAst.TypeParam.Unkinded = tparam0 match {
    case NamedAst.TypeParam.Unkinded(name, sym, loc) => ResolvedAst.TypeParam.Unkinded(name, sym, loc)
  }

  /**
    * Performs name resolution on the given implicit type parameter `tparam0` in the given namespace `ns0`.
    */
  private def resolveImplicitTypeParam(tparam0: NamedAst.TypeParam, scp0: LocalScope): Option[ResolvedAst.TypeParam] = tparam0 match {
    case NamedAst.TypeParam.Implicit(name, sym, loc) =>
      // Check if the tparam is in the LocalScope
      scp0(name.name) collectFirst {
        case Resolution.TypeVar(scpSym) => scpSym
      } match {
        // Case 1: Already in the LocalScope, this is not a type parameter.
        case Some(_) => None
        // Case 2: Not in the LocalScope. This is a real type parameter.
        case None => Some(ResolvedAst.TypeParam.Implicit(name, sym, loc))
      }
    case NamedAst.TypeParam.Kinded(_, _, _, loc) => throw InternalCompilerException("unexpected explicit type parameter", loc)
    case NamedAst.TypeParam.Unkinded(_, _, loc) => throw InternalCompilerException("unexpected explicit type parameter", loc)
  }

  /**
    * Performs name resolution on the given constraint parameter.
    */
  private def resolveConstraintParam(cparam0: NamedAst.ConstraintParam, scp0: LocalScope): Option[ResolvedAst.ConstraintParam] = cparam0 match {
    case NamedAst.ConstraintParam(sym, loc) =>
      // Check if the cparam is in the LocalScope
      scp0(sym.text) collectFirst {
        case Resolution.Var(varSym) => varSym
      } match {
        // Case 1: Already in the LocalScope, this is not a constraint parameter.
        case Some(_) => None
        // Case 2: Not in the LocalScope. This is a real constraint parameter.
        case None => Some(ResolvedAst.ConstraintParam(sym, loc))
      }
  }

  /**
    * Performs name resolution on the given type parameters `tparams0`.
    */
  private def resolveTypeParams(tparams0: List[NamedAst.TypeParam], scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Validation[List[ResolvedAst.TypeParam], ResolutionError] = {
    // Isolate the implicit type params: these are allowed to have redundancies.
    val (impTparams0, expTparams0) = tparams0.partition {
      case _: NamedAst.TypeParam.Implicit => true
      case _: NamedAst.TypeParam.Kinded => false
      case _: NamedAst.TypeParam.Unkinded => false
    }

    val impTparams = impTparams0.flatMap(resolveImplicitTypeParam(_, scp0))
    val expTparamsVal = traverse(expTparams0)(resolveTypeParam(_, scp0, ns0, root))

    mapN(expTparamsVal) {
      case expTparams =>
        impTparams ++ expTparams
    }
  }

  /**
    * Performs name resolution on the given constraint parameters `cparams0`.
    */
  private def resolveConstraintParams(cparams0: List[NamedAst.ConstraintParam], scp0: LocalScope): List[ResolvedAst.ConstraintParam] = {
    cparams0.flatMap(resolveConstraintParam(_, scp0))
  }

  /**
    * Performs name resolution on the given type constraint `tconstr0`.
    */
  private def resolveTraitConstraint(tconstr0: NamedAst.TraitConstraint, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Option[ResolvedAst.TraitConstraint], ResolutionError] = tconstr0 match {
    case NamedAst.TraitConstraint(trt0, tpe0, loc) =>
      val optTrait = lookupTrait(trt0, TraitUsageKind.Constraint, scp0, ns0, root)
      val tpeVal = resolveType(tpe0, None, Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)

      mapN(tpeVal) {
        tpe =>
          optTrait.map {
            trt =>
              val head = TraitSymUse(trt.sym, trt0.loc)
              ResolvedAst.TraitConstraint(head, tpe, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given equality constraint `econstr0`.
    */
  private def resolveEqualityConstraint(tconstr0: NamedAst.EqualityConstraint, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.EqualityConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.EqualityConstraint(qname, tpe1, tpe2, loc) =>
      val assocVal = lookupAssocType(qname, scp0, ns0, root)

      val t1Val = resolveType(tpe1, None, Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)
      val t2Val = resolveType(tpe2, None, Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)

      mapN(assocVal, t1Val, t2Val) {
        case (assoc, t1, t2) =>
          val head = AssocTypeSymUse(assoc.sym, qname.loc)
          ResolvedAst.EqualityConstraint(head, t1, t2, loc)
      }
  }

  /**
    * Performs name resolution on the given supertrait constraint `tconstr0`.
    */
  private def resolveSuperTrait(tconstr0: NamedAst.TraitConstraint, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[ResolvedAst.TraitConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TraitConstraint(trt0, tpe0, loc) =>
      val traitVal = lookupTraitForImplementation(trt0, TraitUsageKind.Constraint, scp0, ns0, root)
      val tpeVal = resolveType(tpe0, None, Wildness.ForbidWild, scp0, taenv, ns0, root)(Scope.Top, sctx, flix)

      mapN(traitVal, tpeVal) {
        case (trt, tpe) =>
          val symUse = TraitSymUse(trt.sym, trt0.loc)
          ResolvedAst.TraitConstraint(symUse, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given derivations `derives0`.
    */
  private def resolveDerivations(derives0: NamedAst.Derivations, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Derivations = {
    val qnames = derives0.traits
    val derives = qnames.flatMap(resolveDerivation(_, scp0, ns0, root))
    // Check for [[DuplicateDerivation]].
    val seen = mutable.Map.empty[Symbol.TraitSym, SourceLocation]
    val errors = mutable.ArrayBuffer.empty[DuplicateDerivation]
    for (Derivation(sym, loc1) <- derives) {
      seen.get(sym) match {
        case None =>
          seen.put(sym, loc1)
        case Some(loc2) =>
          errors += DuplicateDerivation(sym, loc1, loc2)
          errors += DuplicateDerivation(sym, loc2, loc1)
      }
    }
    errors.foreach(sctx.errors.add)
    Derivations(derives, derives0.loc)
  }

  /**
    * Performs name resolution on the given of derivation `derive0`.
    */
  private def resolveDerivation(derive0: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Option[Derivation] = {
    lookupTrait(derive0, TraitUsageKind.Derivation, scp0, ns0, root).map {
      trt => Derivation(trt.sym, derive0.loc)
    }
  }

  /**
    * Finds the trait with the qualified name `qname` in the namespace `ns0`, for the purposes of implementation.
    */
  private def lookupTraitForImplementation(qname: Name.QName, traitUseKind: TraitUsageKind, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Validation[NamedAst.Declaration.Trait, ResolutionError] = {
    val traitOpt = tryLookupName(qname, scp0, ns0, root)
    traitOpt.collectFirst {
      case Resolution.Declaration(trt: NamedAst.Declaration.Trait) => trt
    } match {
      case Some(trt) =>
        getTraitAccessibility(trt, ns0) match {
          case TraitAccessibility.Accessible =>
            Validation.Success(trt)
          case TraitAccessibility.Sealed =>
            val error = ResolutionError.SealedTrait(trt.sym, ns0, qname.loc)
            sctx.errors.add(error)
            Validation.Success(trt)
          case TraitAccessibility.Inaccessible =>
            val error = ResolutionError.InaccessibleTrait(trt.sym, ns0, qname.loc)
            sctx.errors.add(error)
            Validation.Success(trt)
        }
      case None =>
        Validation.Failure(ResolutionError.UndefinedTrait(qname, traitUseKind, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, ns0, qname.loc))
    }
  }

  /**
    * Finds the trait with the qualified name `qname` in the namespace `ns0`.
    */
  private def lookupTrait(qname: Name.QName, traitUseKind: TraitUsageKind, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Option[NamedAst.Declaration.Trait] = {
    val traitOpt = tryLookupName(qname, scp0, ns0, root)
    traitOpt.collectFirst {
      case Resolution.Declaration(trt: NamedAst.Declaration.Trait) => trt
    } match {
      case Some(trt) =>
        getTraitAccessibility(trt, ns0) match {
          case TraitAccessibility.Accessible | TraitAccessibility.Sealed =>
            Some(trt)

          case TraitAccessibility.Inaccessible =>
            val error = ResolutionError.InaccessibleTrait(trt.sym, ns0, qname.loc)
            sctx.errors.add(error)
            Some(trt)
        }
      case None =>
        val error = ResolutionError.UndefinedTrait(qname, traitUseKind, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, ns0, qname.loc)
        sctx.errors.add(error)
        None
    }
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupQName(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): ResolvedQName = {
    // first look in the LocalScope
    val resolutions = tryLookupName(qname, scp0, ns0, root)

    resolutions.collect {
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Def) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Sig) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Case) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.RestrictableCase) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Op) => decl
      case decl@Resolution.Var(_) => decl
      case decl@Resolution.LocalDef(_, _) => decl
    } match {
      case Resolution.Declaration(defn: NamedAst.Declaration.Def) :: _ =>
        if (isDefAccessible(defn, ns0)) {
          ResolvedQName.Def(defn)
        } else {
          val error = ResolutionError.InaccessibleDef(defn.sym, ns0, qname.loc)
          sctx.errors.add(error)
          ResolvedQName.Def(defn)
        }
      case Resolution.Declaration(sig: NamedAst.Declaration.Sig) :: _ =>
        if (isSigAccessible(sig, ns0)) {
          ResolvedQName.Sig(sig)
        } else {
          val error = ResolutionError.InaccessibleSig(sig.sym, ns0, qname.loc)
          sctx.errors.add(error)
          ResolvedQName.Sig(sig)
        }

      case Resolution.Declaration(op: NamedAst.Declaration.Op) :: _ =>
        ResolvedQName.Op(op)
      case Resolution.Declaration(caze: NamedAst.Declaration.Case) :: _ =>
        ResolvedQName.Tag(caze)
      // TODO NS-REFACTOR check accessibility
      case Resolution.Declaration(caze: NamedAst.Declaration.RestrictableCase) :: Nil =>
        ResolvedQName.RestrictableTag(caze)
      // TODO NS-REFACTOR check accessibility
      case Resolution.LocalDef(sym, fparams) :: _ => ResolvedQName.LocalDef(sym, fparams)
      case Resolution.Var(sym) :: _ => ResolvedQName.Var(sym)
      case _ =>
        val error = ResolutionError.UndefinedName(qname, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, qname.loc)
        sctx.errors.add(error)
        ResolvedQName.Error(error)
    }
  }

  /**
    * Looks up the effect operation as a member of the given effect.
    */
  private def findOpInEffect(ident: Name.Ident, eff: NamedAst.Declaration.Effect, ns: Name.NName, scp0: LocalScope): Validation[NamedAst.Declaration.Op, ResolutionError] = {
    val opOpt = eff.ops.find(o => o.sym.name == ident.name)
    opOpt match {
      case None =>
        val nname = eff.sym.namespace :+ eff.sym.name
        val qname = Name.mkQName(nname, ident.name, ident.loc)
        Validation.Failure(ResolutionError.UndefinedOp(qname, AnchorPosition.mkImportOrUseAnchor(ns), scp0, ident.loc))
      case Some(op) =>
        Validation.Success(op)
    }
  }

  /**
    * Finds the enum case that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  private def lookupTag(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.Case, ResolutionError.UndefinedTag] = {
    // look up the name
    val matches = tryLookupName(qname, scp0, ns0, root).collect {
      case Resolution.Declaration(c: NamedAst.Declaration.Case) => c
    }

    matches match {
      // Case 0: No matches. Error.
      case Nil => Result.Err(ResolutionError.UndefinedTag(qname, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, ns0, qname.loc))
      // Case 1: A match was found. Success. Note that multiple matches can be found, but they are prioritized by tryLookupName so this is fine.
      case caze :: _ => Result.Ok(caze)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Finds the struct that matches the given name `qname` in the namespace `ns0`.
    */
  private def lookupStruct(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.Struct, ResolutionError.UndefinedStruct] = {
    val matches = tryLookupName(qname, scp0, ns0, root).collect {
      case Resolution.Declaration(s: NamedAst.Declaration.Struct) => s
    }
    matches match {
      // Case 0: No matches. Error.
      case Nil => Result.Err(ResolutionError.UndefinedStruct(qname, AnchorPosition.mkImportOrUseAnchor(ns0), qname.loc))
      // Case 1: A match was found. Success. Note that multiple matches can be found, but they are prioritized by tryLookupName so this is fine.
      case st :: _ => Result.Ok(st)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Finds the struct field that matches the given name `name` in the namespace `ns0`.
    */
  private def lookupStructField(name: Name.Label, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.StructField, ResolutionError.UndefinedStructField] = {
    val matches = tryLookupName(Name.mkQName(ns0.parts, "€" + name.name, name.loc), scp0, ns0, root) collect {
      case Resolution.Declaration(s: NamedAst.Declaration.StructField) => s
    }
    matches match {
      // Case 0: No matches. Error.
      case Nil =>
        if (ns0.idents.nonEmpty) {
          // The struct name is the same as the mod name
          val struct_namespace = Name.NName(ns0.idents.init, ns0.loc)
          val struct_name = ns0.idents.last
          Result.Err(ResolutionError.UndefinedStructField(Some(Symbol.mkStructSym(struct_namespace, struct_name)), name, name.loc))
        } else {
          // If we are in the root namespace, we can't give figure out the struct name
          Result.Err(ResolutionError.UndefinedStructField(None, name, name.loc))
        }
      // Case 1: A match was found. Success. Note that multiple matches can be found, but they are prioritized by tryLookupName so this is fine.
      case field :: _ => Result.Ok(field)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Finds the restrictable enum case that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  private def lookupRestrictableTag(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.RestrictableCase, ResolutionError] = {
    // look up the name
    val matches = tryLookupName(qname, scp0, ns0, root) collect {
      case Resolution.Declaration(c: NamedAst.Declaration.RestrictableCase) => c
    }

    matches match {
      // Case 0: No matches. Error.
      case Nil => Validation.Failure(ResolutionError.UndefinedRestrictableTag(qname.ident.name, ns0, qname.loc))
      // Case 1: Exactly one match. Success.
      case caze :: Nil =>
        Validation.Success(caze)
      // Case 2: Multiple matches. Error
      case _ => throw InternalCompilerException(s"unexpected duplicate tag: $qname", qname.loc)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Looks up the restrictable enum name.
    */
  private def lookupRestrictableEnum(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.RestrictableEnum, ResolutionError] = {
    val matches = tryLookupName(qname, scp0, ns0, root) collect {
      case Resolution.Declaration(c: NamedAst.Declaration.RestrictableEnum) => c
    }

    matches match {
      case Nil => Validation.Failure(ResolutionError.UndefinedRestrictableType(qname, ns0, qname.loc))
      case enum0 :: _ => Validation.Success(enum0)
    }
  }

  /**
    * Partially resolves the given type `tpe0` in the given namespace `ns0`.
    *
    * Type aliases are given temporary placeholders.
    */
  private def semiResolveType(tpe0: NamedAst.Type, kindOpt: Option[Kind], wildness: Wildness, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[UnkindedType, ResolutionError] = {
    def visit(tpe0: NamedAst.Type): Validation[UnkindedType, ResolutionError] = tpe0 match {
      case NamedAst.Type.Var(ident, loc) =>
        lookupLowerType(ident, wildness, scp0) match {
          case Result.Ok(LowerType.Var(sym)) => Validation.Success(UnkindedType.Var(sym, loc))
          case Result.Ok(LowerType.Region(sym)) => Validation.Success(UnkindedType.Cst(TypeConstructor.Region(sym), loc))
          case Result.Err(error) =>
            // Note: We assume the default type variable has kind Star.
            sctx.errors.add(error)
            Validation.Success(UnkindedType.Error(loc))
        }

      case NamedAst.Type.Unit(loc) => Validation.Success(UnkindedType.Cst(TypeConstructor.Unit, loc))

      case NamedAst.Type.Ambiguous(qname, loc) if qname.isUnqualified => qname.ident.name match {
        // Basic Types
        case "Void" => Validation.Success(UnkindedType.Cst(TypeConstructor.Void, loc))
        case "Unit" => Validation.Success(UnkindedType.Cst(TypeConstructor.Unit, loc))
        case "Null" => Validation.Success(UnkindedType.Cst(TypeConstructor.Null, loc))
        case "Bool" => Validation.Success(UnkindedType.Cst(TypeConstructor.Bool, loc))
        case "Char" => Validation.Success(UnkindedType.Cst(TypeConstructor.Char, loc))
        case "Float32" => Validation.Success(UnkindedType.Cst(TypeConstructor.Float32, loc))
        case "Float64" => Validation.Success(UnkindedType.Cst(TypeConstructor.Float64, loc))
        case "BigDecimal" => Validation.Success(UnkindedType.Cst(TypeConstructor.BigDecimal, loc))
        case "Int8" => Validation.Success(UnkindedType.Cst(TypeConstructor.Int8, loc))
        case "Int16" => Validation.Success(UnkindedType.Cst(TypeConstructor.Int16, loc))
        case "Int32" => Validation.Success(UnkindedType.Cst(TypeConstructor.Int32, loc))
        case "Int64" => Validation.Success(UnkindedType.Cst(TypeConstructor.Int64, loc))
        case "BigInt" => Validation.Success(UnkindedType.Cst(TypeConstructor.BigInt, loc))
        case "String" => Validation.Success(UnkindedType.Cst(TypeConstructor.Str, loc))
        case "Regex" => Validation.Success(UnkindedType.Cst(TypeConstructor.Regex, loc))
        case "Sender" => Validation.Success(UnkindedType.Cst(TypeConstructor.Sender, loc))
        case "Receiver" => Validation.Success(UnkindedType.Cst(TypeConstructor.Receiver, loc))
        case "Lazy" => Validation.Success(UnkindedType.Cst(TypeConstructor.Lazy, loc))
        case "Array" => Validation.Success(UnkindedType.Cst(TypeConstructor.Array, loc))
        case "Vector" => Validation.Success(UnkindedType.Cst(TypeConstructor.Vector, loc))
        case "Region" => Validation.Success(UnkindedType.Cst(TypeConstructor.RegionToStar, loc))
        case "XVar" => Validation.Success(UnkindedType.Cst(TypeConstructor.Extensible, loc))

        // Disambiguate type.
        case _ => // typeName
          lookupType(qname, scp0, ns0, root) match {
            case TypeLookupResult.Enum(enum0) => Validation.Success(getEnumTypeIfAccessible(enum0, ns0, loc))
            case TypeLookupResult.Struct(struct) => Validation.Success(getStructTypeIfAccessible(struct, ns0, loc))
            case TypeLookupResult.RestrictableEnum(enum0) => Validation.Success(getRestrictableEnumTypeIfAccessible(enum0, ns0, loc))
            case TypeLookupResult.TypeAlias(typeAlias) => Validation.Success(getTypeAliasTypeIfAccessible(typeAlias, ns0, loc))
            case TypeLookupResult.Effect(eff) => Validation.Success(getEffectTypeIfAccessible(eff, ns0, loc))
            case TypeLookupResult.JavaClass(clazz) => Validation.Success(flixifyType(clazz, loc))
            case TypeLookupResult.AssocType(assoc) => Validation.Success(getAssocTypeTypeIfAccessible(assoc, loc))
            case TypeLookupResult.NotFound =>
              val error = ResolutionError.UndefinedType(qname, kindOpt, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, loc)
              sctx.errors.add(error)
              Validation.Success(UnkindedType.Error(loc))
          }
      }

      case NamedAst.Type.Ambiguous(qname, loc) =>
        // Disambiguate type.
        lookupType(qname, scp0, ns0, root) match {
          case TypeLookupResult.Enum(enum0) => Validation.Success(getEnumTypeIfAccessible(enum0, ns0, loc))
          case TypeLookupResult.Struct(struct) => Validation.Success(getStructTypeIfAccessible(struct, ns0, loc))
          case TypeLookupResult.RestrictableEnum(enum0) => Validation.Success(getRestrictableEnumTypeIfAccessible(enum0, ns0, loc))
          case TypeLookupResult.TypeAlias(typeAlias) => Validation.Success(getTypeAliasTypeIfAccessible(typeAlias, ns0, loc))
          case TypeLookupResult.Effect(eff) => Validation.Success(getEffectTypeIfAccessible(eff, ns0, loc))
          case TypeLookupResult.JavaClass(clazz) => Validation.Success(flixifyType(clazz, loc))
          case TypeLookupResult.AssocType(assoc) => Validation.Success(getAssocTypeTypeIfAccessible(assoc, loc))
          case TypeLookupResult.NotFound =>
            val error = ResolutionError.UndefinedType(qname, kindOpt, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, loc)
            sctx.errors.add(error)
            Validation.Success(UnkindedType.Error(loc))
        }

      case NamedAst.Type.Tuple(elms0, loc) =>
        val elmsVal = traverseNel(elms0)(tpe => visit(tpe))
        mapN(elmsVal) {
          elms => UnkindedType.mkTuple(elms, loc)
        }

      case NamedAst.Type.RecordRowEmpty(loc) =>
        Validation.Success(UnkindedType.Cst(TypeConstructor.RecordRowEmpty, loc))

      case NamedAst.Type.RecordRowExtend(label, value, rest, loc) =>
        val vVal = visit(value)
        val rVal = visit(rest)
        mapN(vVal, rVal) {
          case (v, r) => UnkindedType.mkRecordRowExtend(label, v, r, loc)
        }

      case NamedAst.Type.Record(row, loc) =>
        val rVal = visit(row)
        mapN(rVal) {
          r => UnkindedType.mkRecord(r, loc)
        }

      case NamedAst.Type.SchemaRowEmpty(loc) =>
        Validation.Success(UnkindedType.Cst(TypeConstructor.SchemaRowEmpty, loc))

      case NamedAst.Type.SchemaRowExtendWithAlias(qname, targs, rest, loc) =>
        // Lookup the type alias.
        flatMapN(lookupTypeAlias(qname, scp0, ns0, root)) {
          typeAlias =>
            val t = getTypeAliasTypeIfAccessible(typeAlias, ns0, loc)
            val tsVal = traverse(targs)(visit)
            val rVal = visit(rest)
            mapN(tsVal, rVal) {
              case (ts, r) =>
                val app = UnkindedType.mkApply(t, ts, loc)
                UnkindedType.mkSchemaRowExtend(Name.mkPred(qname.ident), app, r, loc)
            }
        }

      case NamedAst.Type.SchemaRowExtendWithTypes(ident, den, tpes, rest, loc) =>
        val tsVal = traverse(tpes)(visit)
        val rVal = visit(rest)
        mapN(tsVal, rVal) {
          case (ts, r) =>
            val pred = mkPredicate(den, ts, loc)
            UnkindedType.mkSchemaRowExtend(Name.mkPred(ident), pred, r, loc)
        }

      case NamedAst.Type.Schema(row, loc) =>
        val rVal = visit(row)
        mapN(rVal) {
          r => UnkindedType.mkSchema(r, loc)
        }

      case NamedAst.Type.Arrow(tparams0, eff0, tresult0, loc) =>
        val tparamsVal = traverse(tparams0)(visit)
        val tresultVal = visit(tresult0)
        val effVal = traverseOpt(eff0)(visit)
        mapN(tparamsVal, tresultVal, effVal) {
          case (tparams, tresult, eff) => mkUncurriedArrowWithEffect(tparams, eff, tresult, loc)
        }

      case NamedAst.Type.Apply(base0, targ0, loc) =>
        val tpe1Val = visit(base0)
        val tpe2Val = visit(targ0)
        mapN(tpe1Val, tpe2Val) {
          case (tpe1, tpe2) => UnkindedType.Apply(tpe1, tpe2, loc)
        }

      case NamedAst.Type.True(loc) =>
        Validation.Success(UnkindedType.Cst(TypeConstructor.True, loc))

      case NamedAst.Type.False(loc) =>
        Validation.Success(UnkindedType.Cst(TypeConstructor.False, loc))

      case NamedAst.Type.Not(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => mkNot(t, loc)
        }

      case NamedAst.Type.And(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkAnd(t1, t2, loc)
        }

      case NamedAst.Type.Or(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkOr(t1, t2, loc)
        }

      case NamedAst.Type.Complement(tpe, loc) =>
        mapN(visit(tpe)) {
          t => mkComplement(t, loc)
        }

      case NamedAst.Type.Union(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkUnion(t1, t2, loc)
        }

      case NamedAst.Type.Intersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkIntersection(t1, t2, loc)
        }

      case NamedAst.Type.Difference(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkDifference(t1, t2, loc)
        }

      case NamedAst.Type.Pure(loc) =>
        Validation.Success(UnkindedType.Cst(TypeConstructor.Pure, loc))

      case NamedAst.Type.CaseSet(cases0, loc) =>
        val casesVal = traverse(cases0)(lookupRestrictableTag(_, scp0, ns0, root))
        mapN(casesVal) {
          case cases => UnkindedType.CaseSet(cases.map(_.sym), loc)
        }

      case NamedAst.Type.CaseComplement(tpe, loc) =>
        mapN(visit(tpe)) {
          t => UnkindedType.CaseComplement(t, loc)
        }

      case NamedAst.Type.CaseUnion(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => UnkindedType.CaseUnion(t1, t2, loc)
        }

      case NamedAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => UnkindedType.CaseIntersection(t1, t2, loc)
        }

      case NamedAst.Type.Ascribe(t0, kind0, loc) =>
        val tVal = visit(t0)
        val kind = resolveKind(kind0, scp0, ns0, root)
        mapN(tVal) {
          t => UnkindedType.Ascribe(t, kind, loc)
        }

      case NamedAst.Type.Error(loc) =>
        Validation.Success(UnkindedType.Error(loc))

    }

    visit(tpe0)
  }

  /**
    * Finishes resolving the partially resolved type `tpe0`.
    *
    * Replaces type alias placeholders with the real type aliases.
    */
  private def finishResolveType(tpe0: UnkindedType, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias])(implicit sctx: SharedContext): Validation[UnkindedType, ResolutionError] = {

    /**
      * Performs beta-reduction on the given type alias.
      * The list of arguments must be the same length as the alias's parameters.
      */
    def applyAlias(alias: ResolvedAst.Declaration.TypeAlias, args: List[UnkindedType], cstLoc: SourceLocation): UnkindedType = {
      val map = alias.tparams.map(_.sym).zip(args).toMap[Symbol.UnkindedTypeVarSym, UnkindedType]
      val tpe = alias.tpe.map(map)
      val symUse = TypeAliasSymUse(alias.sym, cstLoc)
      UnkindedType.Alias(symUse, args, tpe, tpe0.loc)
    }

    val baseType = tpe0.baseType
    val targs = tpe0.typeArguments

    baseType match {
      case UnkindedType.UnappliedAlias(sym, loc) =>
        val alias = taenv(sym)
        val tparams = alias.tparams
        val numParams = tparams.length
        if (targs.length < numParams) {
          // Case 1: The type alias is under-applied.
          val error = ResolutionError.UnderAppliedTypeAlias(sym, loc)
          sctx.errors.add(error)
          Validation.Success(UnkindedType.Error(loc))
        } else {
          // Case 2: The type alias is fully applied.
          // Apply the types within the alias, then apply any leftover types.
          mapN(traverse(targs)(finishResolveType(_, taenv))) {
            resolvedArgs =>
              val (usedArgs, extraArgs) = resolvedArgs.splitAt(numParams)
              UnkindedType.mkApply(applyAlias(alias, usedArgs, loc), extraArgs, tpe0.loc)
          }
        }

      case UnkindedType.UnappliedAssocType(sym, loc) =>
        targs match {
          // Case 1: The associated type is under-applied.
          case Nil =>
            val error = ResolutionError.UnderAppliedAssocType(sym, loc)
            sctx.errors.add(error)
            Validation.Success(UnkindedType.Error(loc))

          // Case 2: The associated type is fully applied.
          // Apply the types first type inside the assoc type, then apply any leftover types.
          case targHead :: targTail =>
            val targHeadVal = finishResolveType(targHead, taenv)
            val targTailVal = traverse(targTail)(finishResolveType(_, taenv))
            flatMapN(targHeadVal, targTailVal) {
              case (targHd: UnkindedType.Var, targTl) =>
                val cst = AssocTypeSymUse(sym, loc)
                val assoc = UnkindedType.AssocType(cst, targHd, tpe0.loc)
                Validation.Success(UnkindedType.mkApply(assoc, targTl, tpe0.loc))
              case _ =>
                val error = ResolutionError.IllegalAssocTypeApplication(tpe0.loc)
                sctx.errors.add(error)
                Validation.Success(UnkindedType.Error(loc))
            }
        }

      case _: UnkindedType.Var =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Cst =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Enum =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Struct =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.RestrictableEnum =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.CaseSet =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case UnkindedType.Arrow(eff, arity, loc) =>
        val effVal = traverseOpt(eff)(finishResolveType(_, taenv))
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(effVal, targsVal) {
          case (p, ts) => UnkindedType.mkApply(UnkindedType.Arrow(p, arity, loc), ts, tpe0.loc)
        }

      case UnkindedType.CaseComplement(tpe, loc) =>
        val tpeVal = finishResolveType(tpe, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpeVal, targsVal) {
          case (t, ts) => UnkindedType.mkApply(UnkindedType.CaseComplement(t, loc), ts, tpe0.loc)
        }

      case UnkindedType.CaseUnion(tpe1, tpe2, loc) =>
        val tpe1Val = finishResolveType(tpe1, taenv)
        val tpe2Val = finishResolveType(tpe2, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpe1Val, tpe2Val, targsVal) {
          case (t1, t2, ts) => UnkindedType.mkApply(UnkindedType.CaseUnion(t1, t2, loc), ts, tpe0.loc)
        }

      case UnkindedType.CaseIntersection(tpe1, tpe2, loc) =>
        val tpe1Val = finishResolveType(tpe1, taenv)
        val tpe2Val = finishResolveType(tpe2, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpe1Val, tpe2Val, targsVal) {
          case (t1, t2, ts) => UnkindedType.mkApply(UnkindedType.CaseIntersection(t1, t2, loc), ts, tpe0.loc)
        }

      case UnkindedType.Ascribe(tpe, kind, loc) =>
        val tpeVal = finishResolveType(tpe, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpeVal, targsVal) {
          case (t, ts) => UnkindedType.mkApply(UnkindedType.Ascribe(t, kind, loc), ts, tpe0.loc)
        }

      case _: UnkindedType.Error =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", baseType.loc)
      case _: UnkindedType.Alias => throw InternalCompilerException("unexpected resolved alias", baseType.loc)
      case _: UnkindedType.AssocType => throw InternalCompilerException("unexpected resolved associated type", baseType.loc)
    }
  }

  /**
    * Performs name resolution on the given type `tpe0` in the given namespace `ns0`.
    *
    * Note: the kindOpt argument is _NOT_ used for resolution. It is only used for any potential error message.
    */
  private def resolveType(tpe0: NamedAst.Type, kindOpt: Option[Kind], wildness: Wildness, scp0: LocalScope, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[UnkindedType, ResolutionError] = {
    val tVal = semiResolveType(tpe0, kindOpt, wildness, scp0, ns0, root)
    flatMapN(tVal) {
      t => finishResolveType(t, taenv)
    }
  }

  /**
    * The result of looking up an ambiguous type.
    */
  private sealed trait TypeLookupResult

  private object TypeLookupResult {
    /**
      * The result is an enum.
      */
    case class Enum(enum0: NamedAst.Declaration.Enum) extends TypeLookupResult

    /**
      * The result is a struct.
      */
    case class Struct(struct0: NamedAst.Declaration.Struct) extends TypeLookupResult

    /**
      * The result is a restrictable enum.
      */
    case class RestrictableEnum(enum0: NamedAst.Declaration.RestrictableEnum) extends TypeLookupResult

    /**
      * The result is a type alias.
      */
    case class TypeAlias(typeAlias: NamedAst.Declaration.TypeAlias) extends TypeLookupResult

    /**
      * The result is an effect.
      */
    case class Effect(eff: NamedAst.Declaration.Effect) extends TypeLookupResult

    /**
      * The result is a Java class.
      */
    case class JavaClass(clazz: Class[?]) extends TypeLookupResult

    /**
      * The result is an associated type constructor.
      */
    case class AssocType(assoc: NamedAst.Declaration.AssocTypeSig) extends TypeLookupResult

    /**
      * The type cannot be found.
      */
    case object NotFound extends TypeLookupResult
  }


  /**
    * Looks up the ambiguous type.
    */
  private def lookupType(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): TypeLookupResult = {
    tryLookupName(qname, scp0, ns0, root).collectFirst {
      case Resolution.Declaration(alias: NamedAst.Declaration.TypeAlias) =>
        // Case 1: found a type alias
        TypeLookupResult.TypeAlias(alias)
      case Resolution.Declaration(enum0: NamedAst.Declaration.Enum) =>
        // Case 2: found an enum
        TypeLookupResult.Enum(enum0)
      case Resolution.Declaration(struct: NamedAst.Declaration.Struct) =>
        // Case 3: found a struct
        TypeLookupResult.Struct(struct)
      case Resolution.Declaration(enum0: NamedAst.Declaration.RestrictableEnum) =>
        // Case 4: found a restrictable enum
        TypeLookupResult.RestrictableEnum(enum0)
      case Resolution.Declaration(effect: NamedAst.Declaration.Effect) =>
        // Case 5: found an effect
        TypeLookupResult.Effect(effect)
      case Resolution.Declaration(assoc: NamedAst.Declaration.AssocTypeSig) =>
        // Case 6: found an associated type
        TypeLookupResult.AssocType(assoc)
      case Resolution.JavaClass(clazz) =>
        // Case 7: found a Java class
        TypeLookupResult.JavaClass(clazz)
    }.getOrElse(TypeLookupResult.NotFound)
  }

  /**
    * Optionally returns the type alias with the given `name` in the given namespace `ns0`.
    */
  private def lookupTypeAlias(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root)(implicit sctx: SharedContext): Validation[NamedAst.Declaration.TypeAlias, ResolutionError] = {
    val symOpt = tryLookupName(qname, scp0, ns0, root)

    symOpt.collectFirst {
      case Resolution.Declaration(alias: NamedAst.Declaration.TypeAlias) =>
        checkTypeAliasIsAccessible(alias, ns0, qname.loc)
        Validation.Success(alias)
    }.getOrElse(Validation.Failure(ResolutionError.UndefinedNameUnrecoverable(qname, ns0, scp0, qname.loc)))
  }

  /**
    * Optionally returns the associated type signature with the given `name` in the given namespace `ns0`.
    */
  private def lookupAssocType(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.AssocTypeSig, ResolutionError] = {
    val symOpt = tryLookupName(qname, scp0, ns0, root)

    symOpt.collectFirst {
      case Resolution.Declaration(assoc: NamedAst.Declaration.AssocTypeSig) =>
        getAssocTypeIfAccessible(assoc)
        Validation.Success(assoc)
    }.getOrElse(Validation.Failure(ResolutionError.UndefinedNameUnrecoverable(qname, ns0, scp0, qname.loc)))
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupEffect(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.Effect, UndefinedEffect] = {
    val effOpt = tryLookupName(qname, scp0, ns0, root).collectFirst {
      case Resolution.Declaration(eff: NamedAst.Declaration.Effect) => eff
    }

    effOpt match {
      case None => Result.Err(ResolutionError.UndefinedEffect(qname, AnchorPosition.mkImportOrUseAnchor(ns0), scp0, ns0, qname.loc))
      case Some(decl) => Result.Ok(decl)
    }
  }

  /**
    * Looks up the type with the given lowercase name.
    */
  private def lookupLowerType(ident: Name.Ident, wildness: Wildness, scp0: LocalScope)(implicit scope: Scope, flix: Flix): Result[LowerType, ResolutionError] = {
    if (ident.isWild) {
      wildness match {
        case Wildness.AllowWild =>
          // We use Top scope because these lookups only occur at top level
          Result.Ok(LowerType.Var(Symbol.freshUnkindedTypeVarSym(VarText.SourceText(ident.name), ident.loc)))
        case Wildness.ForbidWild =>
          Result.Err(ResolutionError.IllegalWildType(ident, ident.loc))
      }
    } else {
      val typeVarOpt = scp0(ident.name).collectFirst {
        case Resolution.TypeVar(sym) => LowerType.Var(sym)
        case Resolution.Region(sym) => LowerType.Region(sym)
      }
      typeVarOpt match {
        case Some(sym) => Result.Ok(sym)
        case None => Result.Err(ResolutionError.UndefinedTypeVar(ident.name, ident.loc))
      }
    }
  }

  /**
    * Returns the list of symbols this name points to, ordered from most closely declared to furthest.
    */
  private def tryLookupName(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): List[Resolution] = {
    if (qname.isUnqualified) {
      // Case 1: Unqualified name.

      // Gather names according to priority:
      // 1st priority: imported names
      val scpNames = scp0(qname.ident.name)

      // 2nd priority: names in the current namespace
      val localNames = if (ns0.idents.nonEmpty) {
        root.symbols.getOrElse(ns0, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration.apply)
      } else {
        Nil
      }

      // 3rd priority: the name of the current namespace
      val currentNamespace = {
        // Make sure we don't duplicate results in `rootNames`
        if (ns0.idents.size > 1 && ns0.idents.lastOption.contains(qname.ident)) {
          // Case 1.1.1.1: We are referring to the current namespace. Use that.
          root.symbols.getOrElse(Name.mkUnlocatedNName(ns0.parts.init), Map.empty).getOrElse(ns0.parts.last, Nil).map(Resolution.Declaration.apply)
        } else {
          Nil
        }
      }

      // 4th priority: names in the root namespace
      val rootNames = root.symbols.getOrElse(Name.RootNS, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration.apply)

      scpNames ::: localNames ::: currentNamespace ::: rootNames

    } else {
      // Case 2. Qualified name. Look it up directly.
      tryLookupQualifiedName(qname, scp0, ns0, root).getOrElse(Nil).map(Resolution.Declaration.apply)
    }
  }

  /**
    * Looks up the qualified name in the given root.
    */
  private def tryLookupQualifiedName(qname0: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Option[List[NamedAst.Declaration]] = {
    // First resolve the root of the qualified name
    val head = qname0.namespace.parts.head
    tryLookupModule(head, scp0, ns0, root) match {
      case None => None
      case Some(prefix) =>
        val ns = prefix ::: qname0.namespace.parts.tail
        val qname = Name.mkQName(ns, qname0.ident.name, SourceLocation.Unknown)
        root.symbols.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Looks up the given module in the root.
    */
  private def tryLookupModule(name: String, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Option[List[String]] = {
    // First see if there's a module with this name imported into our LocalScope
    scp0(name).collectFirst {
      case Resolution.Declaration(ns: NamedAst.Declaration.Namespace) => ns.sym.ns
      case Resolution.Declaration(trt: NamedAst.Declaration.Trait) => trt.sym.namespace :+ trt.sym.name
      case Resolution.Declaration(enum0: NamedAst.Declaration.Enum) => enum0.sym.namespace :+ enum0.sym.name
      case Resolution.Declaration(struct: NamedAst.Declaration.Struct) => struct.sym.namespace :+ struct.sym.name
      case Resolution.Declaration(enum0: NamedAst.Declaration.RestrictableEnum) => enum0.sym.namespace :+ enum0.sym.name
      case Resolution.Declaration(eff: NamedAst.Declaration.Effect) => eff.sym.namespace :+ eff.sym.name
    }.orElse {
      // Then see if there's a module with this name declared in our namespace
      root.symbols.getOrElse(ns0, Map.empty).getOrElse(name, Nil).collectFirst {
        case Declaration.Namespace(sym, _, _, _) => sym.ns
        case Declaration.Trait(_, _, _, sym, _, _, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.Enum(_, _, _, sym, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.Struct(_, _, _, sym, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.RestrictableEnum(_, _, _, sym, _, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.Effect(_, _, _, sym, _, _) => sym.namespace :+ sym.name
      }
    }.orElse {
      // Then see if there's a module with this name declared in the root namespace
      root.symbols.getOrElse(Name.RootNS, Map.empty).getOrElse(name, Nil).collectFirst {
        case Declaration.Namespace(sym, _, _, _) => sym.ns
        case Declaration.Trait(_, _, _, sym, _, _, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.Enum(_, _, _, sym, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.Struct(_, _, _, sym, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.RestrictableEnum(_, _, _, sym, _, _, _, _, _) => sym.namespace :+ sym.name
        case Declaration.Effect(_, _, _, sym, _, _) => sym.namespace :+ sym.name
      }
    }
  }

  /**
    * Looks up the qualified name in the given root.
    */
  private def lookupQualifiedName(qname: Name.QName, scp0: LocalScope, ns0: Name.NName, root: NamedAst.Root): Validation[List[NamedAst.Declaration], ResolutionError] = {
    tryLookupQualifiedName(qname, scp0, ns0, root) match {
      case None => Validation.Failure(ResolutionError.UndefinedNameUnrecoverable(qname, ns0, scp0, qname.loc))
      case Some(decl) => Validation.Success(decl)
    }
  }

  /**
    * Determines if the trait is accessible from the namespace.
    *
    * Accessibility depends on the modifiers on the trait
    * and the accessing namespace's relation to the trait namespace:
    *
    * |            | same | child | other |
    * |------------|------|-------|-------|
    * | (none)     | A    | A     | I     |
    * | sealed     | A    | S     | I     |
    * | pub        | A    | A     | A     |
    * | pub sealed | A    | S     | S     |
    *
    * (A: Accessible, S: Sealed, I: Inaccessible)
    */
  private def getTraitAccessibility(trait0: NamedAst.Declaration.Trait, ns0: Name.NName): TraitAccessibility = {

    val traitNs = trait0.sym.namespace
    val accessingNs = ns0.idents.map(_.name)

    if (traitNs == accessingNs) {
      // Case 1: We're in the same namespace: Accessible
      TraitAccessibility.Accessible
    } else if (!trait0.mod.isPublic && !accessingNs.startsWith(traitNs)) {
      // Case 2: The trait is private and we're in unrelated namespaces: Inaccessible
      TraitAccessibility.Inaccessible
    } else if (trait0.mod.isSealed) {
      // Case 3: The trait is accessible but sealed
      TraitAccessibility.Sealed
    } else {
      // Case 4: The trait is otherwise accessible
      TraitAccessibility.Accessible
    }
  }

  /**
    * Determines if the definition is accessible from the namespace.
    *
    * A definition `defn0` is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def isDefAccessible(defn0: NamedAst.Declaration.Def, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (defn0.spec.mod.isPublic)
      return true

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = defn0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }

  /**
    * Determines if the signature is accessible from the namespace.
    *
    * A signature `sig0` is accessible from a namespace `ns0` if:
    *
    * (a) the signature is marked public, or
    * (b) the signature is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def isSigAccessible(sig0: NamedAst.Declaration.Sig, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (sig0.spec.mod.isPublic)
      return true

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = sig0.sym.trt.namespace :+ sig0.sym.trt.name
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }


  /**
    * Checks whether `enum0` is accessible from the given namespace `ns0`.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def checkEnumIsAccessible(enum0: NamedAst.Declaration.Enum, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    //
    // Check if the definition is marked public.
    //
    val isPublic = enum0.mod.isPublic

    //
    // Check if the enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    val isInScopeOfNS = targetNs.startsWith(prefixNs)

    val isAccessible = isPublic || isInScopeOfNS

    if (!isAccessible) {
      //
      // The enum is not accessible.
      //
      val error = ResolutionError.InaccessibleEnum(enum0.sym, ns0, loc)
      sctx.errors.add(error)
    }
  }

  /**
    * Checks whether `struct0` is accessible from the given namespace `ns0`.
    *
    * A struct is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def checkStructIsAccessible(struct0: NamedAst.Declaration.Struct, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    //
    // Check if the definition is marked public.
    //
    val isPublic = struct0.mod.isPublic

    //
    // Check if the struct is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = struct0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    val isInScopeOfNS = targetNs.startsWith(prefixNs)

    val isAccessible = isPublic || isInScopeOfNS

    if (!isAccessible) {
      //
      // The struct is not accessible.
      //
      val error = ResolutionError.InaccessibleStruct(struct0.sym, ns0, loc)
      sctx.errors.add(error)
    }
  }


  /**
    * Checks whether `enum0` is accessible from the given namespace `ns0`.
    *
    * A restrictable enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def checkRestrictableEnumIsAccessible(enum0: NamedAst.Declaration.RestrictableEnum, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    //
    // Check if the definition is marked public.
    //
    val isPublic = enum0.mod.isPublic

    //
    // Check if the restrictable enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    val isInScopeOfNS = targetNs.startsWith(prefixNs)

    val isAccessible = isPublic || isInScopeOfNS

    if (!isAccessible) {
      //
      // The restrictable enum is not accessible.
      //
      val error = ResolutionError.InaccessibleRestrictableEnum(enum0.sym, ns0, loc)
      sctx.errors.add(error)
    }
  }


  /**
    * Returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    */
  private def getEnumTypeIfAccessible(enum0: NamedAst.Declaration.Enum, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): UnkindedType = {
    checkEnumIsAccessible(enum0, ns0, loc)
    mkEnum(enum0.sym, loc)
  }

  /**
    * Returns the type of the given `struct0` if it is accessible from the given namespace `ns0`.
    */
  private def getStructTypeIfAccessible(struct0: NamedAst.Declaration.Struct, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): UnkindedType = {
    checkStructIsAccessible(struct0, ns0, loc)
    mkStruct(struct0.sym, loc)
  }

  /**
    * Returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    */
  private def getRestrictableEnumTypeIfAccessible(enum0: NamedAst.Declaration.RestrictableEnum, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): UnkindedType = {
    checkRestrictableEnumIsAccessible(enum0, ns0, loc)
    mkRestrictableEnum(enum0.sym, loc)
  }

  /**
    * Checks whether the given type alias `alias0` is accessible from the given namespace `ns0`.
    *
    * A type alias is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def checkTypeAliasIsAccessible(alias0: NamedAst.Declaration.TypeAlias, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    //
    // Check if the definition is marked public.
    //
    val isPublic = alias0.mod.isPublic

    //
    // Check if the type alias is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = alias0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    val isInScopeOfNS = targetNs.startsWith(prefixNs)

    val isAccessible = isPublic || isInScopeOfNS

    if (!isAccessible) {
      //
      // The type alias is not accessible.
      //
      val error = ResolutionError.InaccessibleTypeAlias(alias0.sym, ns0, loc)
      sctx.errors.add(error)
    }
  }

  /**
    * Returns the type of the given type alias `alias0` if it is accessible from the given namespace `ns0`.
    */
  private def getTypeAliasTypeIfAccessible(alias0: NamedAst.Declaration.TypeAlias, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): UnkindedType = {
    checkTypeAliasIsAccessible(alias0, ns0, loc)
    mkUnappliedTypeAlias(alias0.sym, loc)
  }

  /**
    * Checks whether the given associated type `assoc0` it is accessible from the given namespace `ns0`.
    *
    * An associated type is accessible from a namespace `ns0` if:
    *
    * (a) its trait is marked public, or
    * (b) the trait is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getAssocTypeIfAccessible(assoc0: NamedAst.Declaration.AssocTypeSig): NamedAst.Declaration.AssocTypeSig = {
    assoc0 // TODO ASSOC-TYPES check class accessibility
  }

  /**
    * Returns the type of the given associated type `assoc0` if it is accessible from the given namespace `ns0`.
    */
  private def getAssocTypeTypeIfAccessible(assoc0: NamedAst.Declaration.AssocTypeSig, loc: SourceLocation): UnkindedType = {
    getAssocTypeIfAccessible(assoc0)
    mkUnappliedAssocType(assoc0.sym, loc)
  }

  /**
    * Checks whether the given `eff0` is accessible from the given namespace `ns0`.
    *
    * An effect is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def checkEffectIsAccessible(eff0: NamedAst.Declaration.Effect, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    //
    // Check if the definition is marked public.
    //
    val isPublic = eff0.mod.isPublic

    //
    // Check if the effect is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = eff0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    val isInScopeOfNS = targetNs.startsWith(prefixNs)

    val isAccessible = isPublic || isInScopeOfNS

    if (!isAccessible) {
      //
      // The effect is not accessible.
      //
      val error = ResolutionError.InaccessibleEffect(eff0.sym, ns0, loc)
      sctx.errors.add(error)
    }
  }

  /**
    * Returns the type of the given effect `eff0` if it is accessible from the given namespace `ns0`.
    */
  private def getEffectTypeIfAccessible(eff0: NamedAst.Declaration.Effect, ns0: Name.NName, loc: SourceLocation)(implicit sctx: SharedContext): UnkindedType = {
    checkEffectIsAccessible(eff0, ns0, loc)
    mkEffect(eff0.sym, loc)
  }

  /**
    * Returns the class reflection object for the given `className`.
    */
  private def lookupJvmClass(className: String, ns0: Name.NName, loc: SourceLocation)(implicit flix: Flix): Result[Class[?], ResolutionError] = try {
    // Don't initialize the class; we don't want to execute static initializers.
    val initialize = false
    Result.Ok(Class.forName(className, initialize, flix.jarLoader))
  } catch {
    case ex: ClassNotFoundException => Result.Err(ResolutionError.UndefinedJvmImport(className, AnchorPosition.mkImportOrUseAnchor(ns0), ex.getMessage, loc))
    case ex: NoClassDefFoundError => Result.Err(ResolutionError.UndefinedJvmImport(className, AnchorPosition.mkImportOrUseAnchor(ns0), ex.getMessage, loc))
  }

  /**
    * Returns the class reflection object for the given `className`.
    */
  private def lookupJvmClass2(className: Name.Ident, ns0: Name.NName, scp0: LocalScope)(implicit flix: Flix): Result[Class[?], ResolutionError] = {
    lookupJvmClass(className.name, ns0, className.loc) match {
      case Result.Ok(clazz) => Result.Ok(clazz)
      case Result.Err(e) => scp0.get(className.name) match {
        case List(Resolution.JavaClass(clazz)) => Result.Ok(clazz)
        case _ => Result.Err(e)
      }
    }
  }

  /**
    * Construct the type alias type constructor for the given symbol `sym` with the given kind `k`.
    */
  private def mkUnappliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation): UnkindedType = UnkindedType.UnappliedAlias(sym, loc)

  /**
    * Construct the associated type constructor for the given symbol `sym` with the given kind `k`.
    */
  private def mkUnappliedAssocType(sym: Symbol.AssocTypeSym, loc: SourceLocation): UnkindedType = UnkindedType.UnappliedAssocType(sym, loc)

  /**
    * Gets the proper symbol from the given named symbol.
    */
  private def getSym(symbol: NamedAst.Declaration): Symbol = symbol match {
    case NamedAst.Declaration.Namespace(sym, _, _, _) => sym
    case NamedAst.Declaration.Trait(_, _, _, sym, _, _, _, _, _, _) => sym
    case NamedAst.Declaration.Sig(sym, _, _, _) => sym
    case NamedAst.Declaration.Def(sym, _, _, _) => sym
    case NamedAst.Declaration.Enum(_, _, _, sym, _, _, _, _) => sym
    case NamedAst.Declaration.Struct(_, _, _, sym, _, _, _, _) => sym
    case NamedAst.Declaration.StructField(_, sym, _, _) => sym
    case NamedAst.Declaration.RestrictableEnum(_, _, _, sym, _, _, _, _, _) => sym
    case NamedAst.Declaration.TypeAlias(_, _, _, sym, _, _, _) => sym
    case NamedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) => sym
    case NamedAst.Declaration.Effect(_, _, _, sym, _, _) => sym
    case NamedAst.Declaration.Op(sym, _, _) => sym
    case NamedAst.Declaration.Case(sym, _, _) => sym
    case NamedAst.Declaration.RestrictableCase(sym, _, _) => sym
    case NamedAst.Declaration.AssocTypeDef(_, _, _, _, _, loc) => throw InternalCompilerException("unexpected associated type definition", loc)
    case NamedAst.Declaration.Instance(_, _, _, _, _, _, _, _, _, _, loc) => throw InternalCompilerException("unexpected instance", loc)
  }

  /**
    * Resolves the symbol where the symbol is known to point to a valid declaration.
    */
  private def infallableLookupSym(sym: Symbol, root: NamedAst.Root): List[NamedAst.Declaration] = sym match {
    case sym: Symbol.DefnSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.EnumSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.StructSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.RestrictableEnumSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.CaseSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.StructFieldSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.RestrictableCaseSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.TraitSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.SigSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.TypeAliasSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.AssocTypeSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.EffectSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.OpSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.ModuleSym => root.symbols(Name.mkUnlocatedNName(sym.ns.init))(sym.ns.last)
    case sym: Symbol.RegionSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.KindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.UnkindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.LabelSym => throw InternalCompilerException(s"unexpected symbol $sym", SourceLocation.Unknown)
    case sym: Symbol.HoleSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
  }

  /**
    * Resolves the given Use.
    */
  private def visitUseOrImport(useOrImport: NamedAst.UseOrImport, ns: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UseOrImport, ResolutionError] = useOrImport match {
    case NamedAst.UseOrImport.Use(qname, alias, loc) => tryLookupName(qname, LocalScope.empty, ns, root) match {
      // Case 1: No matches. Error.
      case Nil => Validation.Failure(ResolutionError.UndefinedUse(qname, ns, Map.empty, loc))
      // Case 2: A match. Map it to a use.
      // TODO NS-REFACTOR: should map to multiple uses or ignore namespaces or something
      case Resolution.Declaration(d) :: _ =>
        Validation.Success(UseOrImport.Use(getSym(d), alias, loc))
      // Case 3: Impossible. Crash.
      case _ => throw InternalCompilerException("unexpected conflicted imports", loc)
    }

    case NamedAst.UseOrImport.Import(name, alias, loc) =>
      val clazzVal = lookupJvmClass(name.toString, ns, name.loc).toValidation
      mapN(clazzVal) {
        case clazz => UseOrImport.Import(clazz, alias, loc)
      }
  }

  /**
    * Adds the given use or import to the use LocalScope.
    */
  private def appendUseScp(scp0: LocalScope, useOrImport: UseOrImport, root: NamedAst.Root): LocalScope = useOrImport match {
    case UseOrImport.Use(sym, alias, _) =>
      val decls = infallableLookupSym(sym, root)
      decls.foldLeft(scp0) {
        case (acc, decl) => acc + (alias.name -> Resolution.Declaration(decl))
      }
    case UseOrImport.Import(clazz, alias, _) => scp0 + (alias.name -> Resolution.JavaClass(clazz))
  }

  /**
    * Adds the given uses and imports to the use LocalScope.
    */
  private def appendAllUseScp(scp0: LocalScope, usesAndImports: List[UseOrImport], root: NamedAst.Root): LocalScope = {
    usesAndImports.foldLeft(scp0)(appendUseScp(_, _, root))
  }

  /**
    * Creates a use LocalScope from the given type parameters.
    */
  private def mkTypeParamScp(tparams: List[ResolvedAst.TypeParam]): LocalScope = {
    tparams.foldLeft(LocalScope.empty) {
      case (acc, tparam) => acc + (tparam.name.name -> Resolution.TypeVar(tparam.sym))
    }
  }

  /**
    * Creates a use LocalScope from the given formal parameters.
    */
  private def mkFormalParamScp(fparams: List[ResolvedAst.FormalParam]): LocalScope = {
    fparams.foldLeft(LocalScope.empty) {
      case (acc, fparam) => acc + (fparam.sym.text -> Resolution.Var(fparam.sym))
    }
  }

  /**
    * Creates an LocalScope from the given constraint parameters.
    */
  private def mkConstraintParamScp(cparams: List[ResolvedAst.ConstraintParam]): LocalScope = {
    cparams.foldLeft(LocalScope.empty) {
      case (acc, cparam) => acc + (cparam.sym.text -> Resolution.Var(cparam.sym))
    }
  }

  /**
    * Creates an LocalScope from the given spec.
    */
  private def mkSpecScp(spec: ResolvedAst.Spec): LocalScope = spec match {
    case ResolvedAst.Spec(_, _, _, tparams, fparams, _, _, _, _) =>
      mkTypeParamScp(tparams) ++ mkFormalParamScp(fparams)
  }

  /**
    * Creates an LocalScope from the given pattern.
    */
  private def mkPatternScp(pat0: ResolvedAst.Pattern): LocalScope = pat0 match {
    case ResolvedAst.Pattern.Wild(_) => LocalScope.empty
    case ResolvedAst.Pattern.Var(sym, _) => mkVarScp(sym)
    case ResolvedAst.Pattern.Cst(_, _) => LocalScope.empty
    case ResolvedAst.Pattern.Tag(_, pats, _) => mkPatternsScp(pats)
    case ResolvedAst.Pattern.Tuple(elms, _) => mkPatternsScp(elms)
    case ResolvedAst.Pattern.Record(pats, pat, _) => mkRecordPatternScp(pats, pat)
    case ResolvedAst.Pattern.Error(_) => LocalScope.empty
  }

  /**
    * Creates an LocalScope from the given record pattern.
    */
  private def mkRecordPatternScp(pats: List[Record.RecordLabelPattern], pat: ResolvedAst.Pattern): LocalScope = {
    mkPatternsScp(pats.map(_.pat)) ++ mkPatternScp(pat)
  }

  /**
    * Creates an LocalScope from the given patterns.
    */
  private def mkPatternsScp(pats: Iterable[ResolvedAst.Pattern]): LocalScope = {
    pats.foldLeft(LocalScope.empty) {
      case (acc, pat) => acc ++ mkPatternScp(pat)
    }
  }

  /**
    * Creates an LocalScope from the given local def symbol and formal parameters.
    */
  private def mkLocalDefScp(sym: Symbol.VarSym, fparams: List[ResolvedAst.FormalParam]): LocalScope = {
    LocalScope.singleton(sym.text, Resolution.LocalDef(sym, fparams))
  }

  /**
    * Creates an LocalScope from the given variable symbol.
    */
  private def mkVarScp(sym: Symbol.VarSym): LocalScope = LocalScope.singleton(sym.text, Resolution.Var(sym))

  /**
    * Creates an LocalScope from the given type variable symbol.
    */
  private def mkTypeVarScp(sym: Symbol.RegionSym): LocalScope = LocalScope.singleton(sym.text, Resolution.Region(sym))

  /**
    * Converts the class into a Flix type.
    */
  private def flixifyType(clazz: Class[?], loc: SourceLocation): UnkindedType = clazz.getName match {
    case "java.math.BigDecimal" => UnkindedType.Cst(TypeConstructor.BigDecimal, loc)
    case "java.math.BigInteger" => UnkindedType.Cst(TypeConstructor.BigInt, loc)
    case "java.lang.String" => UnkindedType.Cst(TypeConstructor.Str, loc)
    case "java.util.regex.Pattern" => UnkindedType.Cst(TypeConstructor.Regex, loc)
    case "java.util.function.Function" => UnkindedType.mkIoArrow(UnkindedType.mkObject(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.Consumer" => UnkindedType.mkIoArrow(UnkindedType.mkObject(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.Predicate" => UnkindedType.mkIoArrow(UnkindedType.mkObject(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.IntFunction" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.IntConsumer" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.IntPredicate" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.IntUnaryOperator" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkInt32(loc), loc)
    case "java.util.function.LongFunction" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.LongConsumer" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.LongPredicate" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.LongUnaryOperator" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkInt64(loc), loc)
    case "java.util.function.DoubleFunction" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.DoubleConsumer" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.DoublePredicate" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.DoubleUnaryOperator" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkFloat64(loc), loc)
    case _ => UnkindedType.Cst(TypeConstructor.Native(clazz), loc)
  }

  /**
    * Checks that the operator's arity matches the number of arguments given.
    */
  private def checkOpArity(op: Declaration.Op, numArgs: Int, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    if (op.spec.fparams.length != numArgs) {
      val error = ResolutionError.MismatchedOpArity(op.sym, op.spec.fparams.length, numArgs, loc)
      sctx.errors.add(error)
    }
  }

  /**
    * Enum describing the extent to which a class is accessible.
    */
  private sealed trait TraitAccessibility

  private object TraitAccessibility {
    case object Accessible extends TraitAccessibility

    case object Sealed extends TraitAccessibility

    case object Inaccessible extends TraitAccessibility
  }

  /**
    * Union of variables, definitions, and signatures.
    */
  private sealed trait ResolvedQName

  private object ResolvedQName {
    case class Var(sym: Symbol.VarSym) extends ResolvedQName

    case class Def(defn: NamedAst.Declaration.Def) extends ResolvedQName

    case class LocalDef(sym: Symbol.VarSym, fparams: List[ResolvedAst.FormalParam]) extends ResolvedQName

    case class Sig(sig: NamedAst.Declaration.Sig) extends ResolvedQName

    case class Tag(caze: NamedAst.Declaration.Case) extends ResolvedQName

    case class Op(op: NamedAst.Declaration.Op) extends ResolvedQName

    case class RestrictableTag(caze: NamedAst.Declaration.RestrictableCase) extends ResolvedQName

    case class Error(e: ResolutionError.UndefinedName) extends ResolvedQName
  }


  /**
    * Enum indicating whether a variable may be a wildcard.
    */
  private sealed trait Wildness

  private object Wildness {
    case object AllowWild extends Wildness

    case object ForbidWild extends Wildness
  }

  /**
    * A table of all the symbols in the program.
    */
  private case class SymbolTable(traits: Map[Symbol.TraitSym, ResolvedAst.Declaration.Trait],
                                 instances: ListMap[Symbol.TraitSym, ResolvedAst.Declaration.Instance],
                                 defs: Map[Symbol.DefnSym, ResolvedAst.Declaration.Def],
                                 enums: Map[Symbol.EnumSym, ResolvedAst.Declaration.Enum],
                                 structs: Map[Symbol.StructSym, ResolvedAst.Declaration.Struct],
                                 structFields: Map[Symbol.StructFieldSym, ResolvedAst.Declaration.StructField],
                                 restrictableEnums: Map[Symbol.RestrictableEnumSym, ResolvedAst.Declaration.RestrictableEnum],
                                 effects: Map[Symbol.EffectSym, ResolvedAst.Declaration.Effect],
                                 typeAliases: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
    def addTrait(trt: ResolvedAst.Declaration.Trait): SymbolTable = copy(traits = traits + (trt.sym -> trt))

    def addDef(defn: ResolvedAst.Declaration.Def): SymbolTable = copy(defs = defs + (defn.sym -> defn))

    def addEnum(enum0: ResolvedAst.Declaration.Enum): SymbolTable = copy(enums = enums + (enum0.sym -> enum0))

    def addStruct(struct: ResolvedAst.Declaration.Struct): SymbolTable = copy(structs = structs + (struct.sym -> struct))

    def addRestrictableEnum(enum0: ResolvedAst.Declaration.RestrictableEnum): SymbolTable = copy(restrictableEnums = restrictableEnums + (enum0.sym -> enum0))

    def addEffect(effect: ResolvedAst.Declaration.Effect): SymbolTable = copy(effects = effects + (effect.sym -> effect))

    def addTypeAlias(alias: ResolvedAst.Declaration.TypeAlias): SymbolTable = copy(typeAliases = typeAliases + (alias.sym -> alias))

    def addInstance(inst: ResolvedAst.Declaration.Instance): SymbolTable = copy(instances = instances + (inst.symUse.sym -> inst))

    private def ++(that: SymbolTable): SymbolTable = {
      SymbolTable(
        traits = this.traits ++ that.traits,
        instances = this.instances ++ that.instances,
        defs = this.defs ++ that.defs,
        enums = this.enums ++ that.enums,
        structs = this.structs ++ that.structs,
        structFields = this.structFields ++ that.structFields,
        restrictableEnums = this.restrictableEnums ++ that.restrictableEnums,
        effects = this.effects ++ that.effects,
        typeAliases = this.typeAliases ++ that.typeAliases
      )
    }
  }

  private object SymbolTable {
    val empty: SymbolTable = SymbolTable(Map.empty, ListMap.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    /**
      * Traverses `xs`, gathering the symbols from each element by applying the function `f`.
      */
    def traverse[T](xs: Iterable[T])(f: T => SymbolTable): SymbolTable = {
      xs.foldLeft(SymbolTable.empty) {
        case (acc, x) => acc ++ f(x)
      }
    }
  }

  /**
    * Returns a fresh [[Symbol.VarSym]] with fresh text to avoid shadowing errors
    * in [[ca.uwaterloo.flix.language.phase.Redundancy]].
    */
  def freshVarSym(name: String, boundBy: BoundBy, loc: SourceLocation)(implicit scope: Scope, flix: Flix): Symbol.VarSym =
    Symbol.freshVarSym(name + Flix.Delimiter + flix.genSym.freshId(), boundBy, loc)

  /** Returns a [[ResolvedAst.Expr.Lambda]] where the body is ascribed to have no effect. */
  private def mkPureLambda(param: ResolvedAst.FormalParam, exp: ResolvedAst.Expr, loc: SourceLocation): ResolvedAst.Expr = {
    ResolvedAst.Expr.Lambda(param, ResolvedAst.Expr.Ascribe(exp, None, Some(UnkindedType.Cst(TypeConstructor.Pure, loc)), loc), allowSubeffecting = false, loc)
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
    * @param errors the [[ResolutionError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[ResolutionError])

  /**
    * A type represented by a lowercase name.
    */
  private sealed trait LowerType

  private object LowerType {
    case class Var(sym: Symbol.UnkindedTypeVarSym) extends LowerType

    case class Region(sym: Symbol.RegionSym) extends LowerType
  }

}
