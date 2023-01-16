/*
 *  Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, VarText}
import ca.uwaterloo.flix.language.ast.NamedAst.{Declaration, RestrictableChoicePattern}
import ca.uwaterloo.flix.language.ast.UnkindedType._
import ca.uwaterloo.flix.language.ast.{NamedAst, Symbol, _}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{Graph, InternalCompilerException, Validation}

import java.lang.reflect.{Constructor, Field, Method, Modifier}
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/**
  * The Resolver phase performs name resolution on the program.
  */
object Resolver {

  /**
    * Symbols of classes that are derivable.
    */
  private val BoxableSym = new Symbol.ClassSym(Nil, "Boxable", SourceLocation.Unknown)
  private val EqSym = new Symbol.ClassSym(Nil, "Eq", SourceLocation.Unknown)
  private val OrderSym = new Symbol.ClassSym(Nil, "Order", SourceLocation.Unknown)
  private val ToStringSym = new Symbol.ClassSym(Nil, "ToString", SourceLocation.Unknown)
  private val HashSym = new Symbol.ClassSym(Nil, "Hash", SourceLocation.Unknown)
  private val SendableSym = new Symbol.ClassSym(Nil, "Sendable", SourceLocation.Unknown)

  val DerivableSyms: List[Symbol.ClassSym] = List(BoxableSym, EqSym, OrderSym, ToStringSym, HashSym, SendableSym)

  /**
    * Java classes for primitives and Object
    */
  private val Int = classOf[Int]
  private val Long = classOf[Long]
  private val Double = classOf[Double]
  private val Boolean = classOf[Boolean]
  private val Object = classOf[AnyRef]

  /**
    * Performs name resolution on the given program `root`.
    */
  def run(root: NamedAst.Root, oldRoot: ResolvedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[ResolvedAst.Root, ResolutionError] = flix.phase("Resolver") {

    val usesVal = root.uses.map {
      case (ns, uses0) =>
        mapN(traverse(uses0)(visitUseOrImport(_, ns, root))) {
          u => (new Symbol.ModuleSym(ns.parts) -> u)
        }
    }

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    flatMapN(sequence(usesVal), resolveTypeAliases(root)) {
      case (uses, (taenv, taOrder)) =>

        val unitsVal = traverse(root.units.values)(visitUnit(_, taenv, root))
        flatMapN(unitsVal) {
          case units =>
            val table = SymbolTable.traverse(units)(tableUnit)
            mapN(checkSuperClassDag(table.classes)) {
              case () =>
                ResolvedAst.Root(
                  table.classes,
                  table.instances.m, // TODO NS-REFACTOR use ListMap elsewhere for this too
                  table.defs,
                  table.enums,
                  table.restrictableEnums,
                  table.effects,
                  table.typeAliases,
                  uses.toMap,
                  taOrder,
                  root.entryPoint,
                  root.sources,
                  root.names
                )
            }
        }
    }
  }

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
    case clazz: ResolvedAst.Declaration.Class => SymbolTable.empty.addClass(clazz)
    case inst: ResolvedAst.Declaration.Instance => SymbolTable.empty.addInstance(inst)
    case defn: ResolvedAst.Declaration.Def => SymbolTable.empty.addDef(defn)
    case enum: ResolvedAst.Declaration.Enum => SymbolTable.empty.addEnum(enum)
    case enum: ResolvedAst.Declaration.RestrictableEnum => SymbolTable.empty.addRestrictableEnum(enum)
    case alias: ResolvedAst.Declaration.TypeAlias => SymbolTable.empty.addTypeAlias(alias)
    case effect: ResolvedAst.Declaration.Effect => SymbolTable.empty.addEffect(effect)
    // TODO NS-REFACTOR this will be added once Cases become proper symbols
    case ResolvedAst.Declaration.Case(sym, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.RestrictableCase(sym, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.Op(sym, spec) => throw InternalCompilerException(s"Unexpected declaration: $sym", spec.loc)
    case ResolvedAst.Declaration.Sig(sym, spec, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", spec.loc)
  }

  /**
    * Semi-resolves the type aliases in the root.
    */
  private def semiResolveTypeAliases(root: NamedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ResolutionError] = {
    fold(root.units.values, Map.empty[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
      case (acc, unit) => mapN(semiResolveTypeAliasesInUnit(unit, root)) {
        case aliases => aliases.foldLeft(acc) {
          case (innerAcc, alias) => innerAcc + (alias.sym -> alias)
        }
      }
    }
  }

  /**
    * Semi-resolves the type aliases in the unit.
    */
  private def semiResolveTypeAliasesInUnit(unit: NamedAst.CompilationUnit, root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Declaration.TypeAlias], ResolutionError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports0, decls, loc) =>
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, Name.RootNS, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val env = mkUseEnv(usesAndImports, root)
          val namespaces = decls.collect {
            case ns: NamedAst.Declaration.Namespace => ns
          }
          val aliases0 = decls.collect {
            case alias: NamedAst.Declaration.TypeAlias => alias
          }
          val aliasesVal = traverse(aliases0)(semiResolveTypeAlias(_, env, Name.RootNS, root))
          val nsVal = traverse(namespaces)(semiResolveTypeAliasesInNamespace(_, root))
          mapN(aliasesVal, nsVal) {
            case (aliases, ns) => aliases ::: ns.flatten
          }
      }
  }

  /**
    * Semi-resolves the type aliases in the namespace.
    */
  private def semiResolveTypeAliasesInNamespace(ns0: NamedAst.Declaration.Namespace, root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Declaration.TypeAlias], ResolutionError] = ns0 match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports0, decls, loc) =>
      val ns = Name.mkUnlocatedNName(sym.ns)
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, ns, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val env = mkUseEnv(usesAndImports, root)
          val namespaces = decls.collect {
            case ns: NamedAst.Declaration.Namespace => ns
          }
          val aliases0 = decls.collect {
            case alias: NamedAst.Declaration.TypeAlias => alias
          }
          val aliasesVal = traverse(aliases0)(semiResolveTypeAlias(_, env, ns, root))
          val nsVal = traverse(namespaces)(semiResolveTypeAliasesInNamespace(_, root))
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
  def semiResolveTypeAlias(alias: NamedAst.Declaration.TypeAlias, env0: ListMap[String, Resolution], ns: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.TypeAlias, ResolutionError] = alias match {
    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val tparams = resolveTypeParams(tparams0, env0, ns, root)
      val env = env0 ++ mkTypeParamEnv(tparams.tparams)
      semiResolveType(tpe0, Wildness.ForbidWild, env, ns, root) map {
        tpe => ResolvedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc)
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
  def resolveTypeAliases(root: NamedAst.Root)(implicit flix: Flix): Validation[(Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], List[Symbol.TypeAliasSym]), ResolutionError] = {
    for {
      semiResolved <- semiResolveTypeAliases(root)
      orderedSyms <- findResolutionOrder(semiResolved.values)
      orderedSemiResolved = orderedSyms.map(semiResolved)
      aliases <- finishResolveTypeAliases(orderedSemiResolved)
    } yield (aliases, orderedSyms)

  }

  /**
    * Gets a list of all type aliases used in the partially resolved type tpe0.
    */
  def getAliasUses(tpe0: UnkindedType): List[Symbol.TypeAliasSym] = tpe0 match {
    case _: UnkindedType.Var => Nil
    case UnkindedType.Ascribe(tpe, _, _) => getAliasUses(tpe)
    case UnkindedType.UnappliedAlias(sym, _) => sym :: Nil
    case _: UnkindedType.Cst => Nil
    case UnkindedType.Apply(tpe1, tpe2, _) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case _: UnkindedType.Arrow => Nil
    case UnkindedType.ReadWrite(tpe, loc) => getAliasUses(tpe)
    case _: UnkindedType.Enum => Nil
    case _: UnkindedType.RestrictableEnum => Nil
    case alias: UnkindedType.Alias => throw InternalCompilerException("unexpected applied alias", alias.loc)
  }

  /**
    * Create a list of CyclicTypeAliases errors, one for each type alias.
    */
  def mkCycleErrors[T](cycle: List[Symbol.TypeAliasSym]): Validation.Failure[T, ResolutionError] = {
    val errors = cycle.map {
      sym => ResolutionError.CyclicTypeAliases(cycle, sym.loc)
    }
    Validation.Failure(LazyList.from(errors))
  }

  /**
    * Gets the resolution order for the aliases.
    *
    * Any alias only depends on those earlier in the list
    */
  def findResolutionOrder(aliases: Iterable[ResolvedAst.Declaration.TypeAlias]): Validation[List[Symbol.TypeAliasSym], ResolutionError] = {
    val aliasSyms = aliases.map(_.sym)
    val aliasLookup = aliases.map(alias => alias.sym -> alias).toMap
    val getUses = (sym: Symbol.TypeAliasSym) => getAliasUses(aliasLookup(sym).tpe)

    Graph.topologicalSort(aliasSyms, getUses) match {
      case Graph.TopologicalSort.Sorted(sorted) => sorted.toSuccess
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
  def finishResolveTypeAliases(aliases0: List[ResolvedAst.Declaration.TypeAlias]): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ResolutionError] = {
    Validation.fold(aliases0, Map.empty[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
      case (taenv, ResolvedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe0, loc)) =>
        finishResolveType(tpe0, taenv) map {
          tpe =>
            val alias = ResolvedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc)
            taenv + (sym -> alias)
        }
    }
  }

  /**
    * Performs name resolution on the compilation unit.
    */
  private def visitUnit(unit: NamedAst.CompilationUnit, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.CompilationUnit, ResolutionError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, Name.RootNS, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val env = mkUseEnv(usesAndImports, root)
          val declsVal = traverse(decls0)(visitDecl(_, env, taenv, Name.RootNS, root))
          mapN(declsVal) {
            case decls => ResolvedAst.CompilationUnit(usesAndImports, decls, loc)
          }
      }
  }

  /**
    * Performs name resolution on the declaration.
    */
  private def visitDecl(decl: NamedAst.Declaration, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration, ResolutionError] = decl match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports0, decls0, loc) =>
      // TODO NS-REFACTOR move to helper for consistency
      // use the new namespace
      val ns = Name.mkUnlocatedNName(sym.ns)
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, ns, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          // reset the env
          val env = mkUseEnv(usesAndImports, root)
          val declsVal = traverse(decls0)(visitDecl(_, env, taenv, ns, root))
          mapN(declsVal) {
            case decls => ResolvedAst.Declaration.Namespace(sym, usesAndImports, decls, loc)
          }
      }
    case clazz@NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc) =>
      resolveClass(clazz, env0, taenv, ns0, root)
    case inst@NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, defs, ns, loc) =>
      resolveInstance(inst, env0, taenv, ns0, root)
    case defn@NamedAst.Declaration.Def(sym, spec, exp) =>
      resolveDef(defn, None, env0, taenv, ns0, root)
    case enum@NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) =>
      resolveEnum(enum, env0, taenv, ns0, root)
    case enum@NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc) =>
      resolveRestrictableEnum(enum, env0, taenv, ns0, root)
    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc) =>
      taenv(sym).toSuccess
    case eff@NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) =>
      resolveEffect(eff, env0, taenv, ns0, root)
    case op@NamedAst.Declaration.Op(sym, spec) => throw InternalCompilerException("unexpected op", sym.loc)
    case NamedAst.Declaration.Sig(sym, spec, exp) => throw InternalCompilerException("unexpected sig", sym.loc)
    case NamedAst.Declaration.Case(sym, tpe) => throw InternalCompilerException("unexpected case", sym.loc)
    case NamedAst.Declaration.RestrictableCase(sym, tpe) => throw InternalCompilerException("unexpected case", sym.loc)
  }

  /**
    * Creates a map from a list of key-(value list) pairs, appending in the case of duplicates.
    */
  private def combine[K, V](list: List[(K, List[V])]): Map[K, List[V]] = {
    list.foldLeft(Map.empty[K, List[V]]) {
      case (acc, (key, value)) => acc + (key -> (value ++ acc.getOrElse(key, Nil)))
    }
  }

  /**
    * Checks that the super classes form a DAG (no cycles).
    */
  private def checkSuperClassDag(classes: Map[Symbol.ClassSym, ResolvedAst.Declaration.Class]): Validation[Unit, ResolutionError] = {

    /**
      * Create a list of CyclicClassHierarchy errors, one for each class.
      */
    def mkCycleErrors[T](cycle: List[Symbol.ClassSym]): Validation.Failure[T, ResolutionError] = {
      val errors = cycle.map {
        sym => ResolutionError.CyclicClassHierarchy(cycle, sym.loc)
      }
      Validation.Failure(LazyList.from(errors))
    }

    val classSyms = classes.values.map(_.sym)
    val getSuperClasses = (clazz: Symbol.ClassSym) => classes(clazz).superClasses.map(_.head.sym)
    Graph.topologicalSort(classSyms, getSuperClasses) match {
      case Graph.TopologicalSort.Cycle(path) => mkCycleErrors(path)
      case Graph.TopologicalSort.Sorted(_) => ().toSuccess
    }
  }

  object Constraints {

    /**
      * Performs name resolution on the given `constraints` in the given namespace `ns0`.
      */
    def resolve(constraints: List[NamedAst.Constraint], env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Constraint], ResolutionError] = {
      traverse(constraints)(c => resolve(c, env, taenv, ns0, root))
    }

    /**
      * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
      */
    def resolve(c0: NamedAst.Constraint, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Constraint, ResolutionError] = c0 match {
      case NamedAst.Constraint(cparams0, head0, body0, loc) =>
        val cparams = resolveConstraintParams(cparams0, env0)
        val env = env0 ++ mkConstraintParamEnv(cparams)
        val headVal = Predicates.Head.resolve(head0, env, taenv, ns0, root)
        val bodyVal = traverse(body0)(Predicates.Body.resolve(_, env, taenv, ns0, root))
        mapN(headVal, bodyVal) {
          case (head, body) => ResolvedAst.Constraint(cparams, head, body, loc)
        }
    }

  }

  /**
    * Resolves all the classes in the given root.
    */
  def resolveClass(c0: NamedAst.Declaration.Class, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Class, ResolutionError] = c0 match {
    case NamedAst.Declaration.Class(doc, ann, mod, sym, tparam0, superClasses0, signatures, laws0, loc) =>
      val tparam = Params.resolveTparam(tparam0)
      val env = env0 ++ mkTypeParamEnv(List(tparam))
      val sigsListVal = traverse(signatures)(resolveSig(_, sym, tparam.sym, env, taenv, ns0, root))
      // ignore the parameter of the super class; we don't use it
      val superClassesVal = traverse(superClasses0)(tconstr => resolveSuperClass(tconstr, env, taenv, ns0, root))
      val tconstr = ResolvedAst.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), UnkindedType.Var(tparam.sym, tparam.sym.loc), sym.loc)
      val lawsVal = traverse(laws0)(resolveDef(_, Some(tconstr), env, taenv, ns0, root))
      mapN(sigsListVal, superClassesVal, lawsVal) {
        case (sigsList, superClasses, laws) =>
          val sigs = sigsList.map(sig => (sig.sym, sig)).toMap
          ResolvedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc)
      }
  }

  /**
    * Performs name resolution on the given instance `i0` in the given namespace `ns0`.
    */
  def resolveInstance(i0: NamedAst.Declaration.Instance, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Instance, ResolutionError] = i0 match {
    case NamedAst.Declaration.Instance(doc, ann, mod, clazz0, tparams0, tpe0, tconstrs0, defs0, ns, loc) =>
      // TODO NS-REFACTOR pull tparams all the way through phases
      val tparams = resolveTypeParams(tparams0, env0, ns0, root)
      val env = env0 ++ mkTypeParamEnv(tparams.tparams)
      val clazzVal = lookupClassForImplementation(clazz0, env, ns0, root)
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      val tconstrsVal = traverse(tconstrs0)(resolveTypeConstraint(_, env, taenv, ns0, root))
      flatMapN(clazzVal, tpeVal, tconstrsVal) {
        case (clazz, tpe, tconstrs) =>
          val tconstr = ResolvedAst.TypeConstraint(Ast.TypeConstraint.Head(clazz.sym, clazz0.loc), tpe, clazz0.loc)
          val defsVal = traverse(defs0)(resolveDef(_, Some(tconstr), env, taenv, ns0, root))
          mapN(defsVal) {
            case defs =>
              val classUse = Ast.ClassSymUse(clazz.sym, clazz0.loc)
              ResolvedAst.Declaration.Instance(doc, ann, mod, classUse, tpe, tconstrs, defs, Name.mkUnlocatedNName(ns), loc)
          }
      }
  }

  /**
    * Performs name resolution on the given signature `s0` in the given namespace `ns0`.
    */
  def resolveSig(s0: NamedAst.Declaration.Sig, clazz: Symbol.ClassSym, classTvar: Symbol.UnkindedTypeVarSym, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Sig, ResolutionError] = s0 match {
    case NamedAst.Declaration.Sig(sym, spec0, exp0) =>
      val tconstr = ResolvedAst.TypeConstraint(Ast.TypeConstraint.Head(clazz, clazz.loc), UnkindedType.Var(classTvar, classTvar.loc), clazz.loc)
      val specVal = resolveSpec(spec0, Some(tconstr), env0, taenv, ns0, root)
      flatMapN(specVal) {
        case spec =>
          val env = env0 ++ mkSpecEnv(spec)
          val specCheckVal = checkSigSpec(sym, spec, classTvar)
          val expVal = traverseOpt(exp0)(Expressions.resolve(_, env, taenv, ns0, root))
          mapN(specCheckVal, expVal) {
            case (_, exp) => ResolvedAst.Declaration.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs name resolution on the given definition `d0` in the given namespace `ns0`.
    */
  def resolveDef(d0: NamedAst.Declaration.Def, tconstr: Option[ResolvedAst.TypeConstraint], env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Def, ResolutionError] = d0 match {
    case NamedAst.Declaration.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      val specVal = resolveSpec(spec0, tconstr, env0, taenv, ns0, root)
      flatMapN(specVal) {
        case spec =>
          val env = env0 ++ mkSpecEnv(spec)
          val expVal = Expressions.resolve(exp0, env, taenv, ns0, root)
          mapN(expVal) {
            case exp => ResolvedAst.Declaration.Def(sym, spec, exp)
          }
      }
  }

  /**
    * Performs name resolution on the given spec `s0` in the given namespace `ns0`.
    */
  def resolveSpec(s0: NamedAst.Spec, tconstr: Option[ResolvedAst.TypeConstraint], env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Spec, ResolutionError] = s0 match {
    case NamedAst.Spec(doc, ann, mod, tparams0, fparams0, tpe0, purAndEff0, tconstrs0, loc) =>

      val tparams = resolveTypeParams(tparams0, env0, ns0, root)
      val env1 = env0 ++ mkTypeParamEnv(tparams.tparams)
      val fparamsVal = resolveFormalParams(fparams0, env1, taenv, ns0, root)
      flatMapN(fparamsVal) {
        case fparams =>
          val env = env1 ++ mkFormalParamEnv(fparams)
          val tpeVal = resolveType(tpe0, Wildness.AllowWild, env, taenv, ns0, root)
          val purAndEffVal = resolvePurityAndEffect(purAndEff0, Wildness.AllowWild, env, taenv, ns0, root)
          val tconstrsVal = traverse(tconstrs0)(resolveTypeConstraint(_, env, taenv, ns0, root))

          mapN(tpeVal, purAndEffVal, tconstrsVal) {
            case (tpe, purAndEff, tconstrs) =>
              // add the inherited type constraint to the the list
              ResolvedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstr.toList ::: tconstrs, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given enum `e0` in the given namespace `ns0`.
    */
  def resolveEnum(e0: NamedAst.Declaration.Enum, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Enum, ResolutionError] = e0 match {
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams0, derives0, cases0, loc) =>
      val tparams = resolveTypeParams(tparams0, env0, ns0, root)
      val env = env0 ++ mkTypeParamEnv(tparams.tparams)
      val derivesVal = resolveDerivations(derives0, env, ns0, root)
      val casesVal = traverse(cases0)(resolveCase(_, env, taenv, ns0, root))
      mapN(derivesVal, casesVal) {
        case (derives, cases) =>
          ResolvedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
      }
  }

  /**
    * Performs name resolution on the given restrictable enum `e0` in the given namespace `ns0`.
    */
  def resolveRestrictableEnum(e0: NamedAst.Declaration.RestrictableEnum, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.RestrictableEnum, ResolutionError] = e0 match {
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index0, tparams0, derives0, cases0, loc) =>
      val index = Params.resolveTparam(index0)
      val tparams = resolveTypeParams(tparams0, env0, ns0, root)
      val env = env0 ++ mkTypeParamEnv(index :: tparams.tparams)
      val derivesVal = resolveDerivations(derives0, env, ns0, root)
      val casesVal = traverse(cases0)(resolveRestrictableCase(_, env, taenv, ns0, root))
      mapN(derivesVal, casesVal) {
        case (derives, cases) =>
          ResolvedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc)
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveCase(caze0: NamedAst.Declaration.Case, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Case, ResolutionError] = caze0 match {
    case NamedAst.Declaration.Case(sym, tpe0) =>
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      mapN(tpeVal) {
        tpe => ResolvedAst.Declaration.Case(sym, tpe)
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveRestrictableCase(caze0: NamedAst.Declaration.RestrictableCase, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.RestrictableCase, ResolutionError] = caze0 match {
    case NamedAst.Declaration.RestrictableCase(sym, tpe0) =>
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      mapN(tpeVal) {
        tpe => ResolvedAst.Declaration.RestrictableCase(sym, tpe)
      }
  }

  /**
    * Performs name resolution on the given effect `eff0` in the given namespace `ns0`.
    */
  private def resolveEffect(eff0: NamedAst.Declaration.Effect, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Effect, ResolutionError] = eff0 match {
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops0, loc) =>
      // TODO NS-REFACTOR maybe start a new env
      val opsVal = traverse(ops0)(resolveOp(_, env, taenv, ns0, root))
      mapN(opsVal) {
        case ops => ResolvedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs name resolution on the given effect operation `op0` in the given namespace `ns0`.
    */
  private def resolveOp(op0: NamedAst.Declaration.Op, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Op, ResolutionError] = op0 match {
    case NamedAst.Declaration.Op(sym, spec0) =>
      val specVal = resolveSpec(spec0, None, env, taenv, ns0, root)
      mapN(specVal) {
        spec => ResolvedAst.Declaration.Op(sym, spec)
      }
  }

  /**
    * Checks that the signature spec is legal.
    *
    * A signature spec is legal if it contains the class's type variable in its formal parameters or return type.
    */
  private def checkSigSpec(sym: Symbol.SigSym, spec0: ResolvedAst.Spec, tvar: Symbol.UnkindedTypeVarSym): Validation[Unit, ResolutionError] = spec0 match {
    case ResolvedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstrs, loc) =>
      val tpes = tpe :: fparams.flatMap(_.tpe)
      val tvars = tpes.flatMap(_.typeVars).to(SortedSet)
      if (tvars.contains(tvar)) {
        ().toSuccess
      } else {
        ResolutionError.IllegalSignature(sym, sym.loc).toFailure
      }
  }

  object Expressions {

    /**
      * Performs name resolution on the given expression `exp0` in the namespace `ns0`.
      */
    def resolve(exp0: NamedAst.Expression, env00: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Expression, ResolutionError] = {

      /**
        * Creates `arity` fresh fparams for use in a curried def or sig application.
        */
      def mkFreshFparams(arity: Int, loc: SourceLocation): List[ResolvedAst.FormalParam] = {
        // Introduce a fresh variable symbol for each argument of the function definition.
        val varSyms = (0 until arity).map(i => Symbol.freshVarSym(Flix.Delimiter + i, BoundBy.FormalParam, loc)).toList

        // Introduce a formal parameter for each variable symbol.
        varSyms.map(sym => ResolvedAst.FormalParam(sym, Ast.Modifiers.Empty, None, loc))
      }

      /**
        * Creates a lambda for use in a curried dif or sig application.
        */
      def mkCurriedLambda(fparams: List[ResolvedAst.FormalParam], baseExp: ResolvedAst.Expression, loc: SourceLocation): ResolvedAst.Expression = {
        val l = loc.asSynthetic

        // The arguments passed to the definition (i.e. the fresh variable symbols).
        val argExps = fparams.map(fparam => ResolvedAst.Expression.Var(fparam.sym, l))

        // The apply expression inside the lambda.
        val applyExp = ResolvedAst.Expression.Apply(baseExp, argExps, l)

        // The curried lambda expressions.
        fparams.foldRight(applyExp: ResolvedAst.Expression) {
          case (fparam, acc) => ResolvedAst.Expression.Lambda(fparam, acc, l)
        }
      }

      /**
        * Curry the def, wrapping it in lambda expressions.
        */
      def visitDef(defn: NamedAst.Declaration.Def, loc: SourceLocation): ResolvedAst.Expression = {
        // Find the arity of the function definition.
        val arity = defn.spec.fparams.length

        // Create the fresh fparams
        val fparams = mkFreshFparams(arity, loc.asSynthetic)

        // The definition expression.
        val defExp = ResolvedAst.Expression.Def(defn.sym, loc)

        // Create and apply the lambda expressions
        mkCurriedLambda(fparams, defExp, loc.asSynthetic)
      }

      /**
        * Curry the sig, wrapping it in lambda expressions.
        */
      def visitSig(sig: NamedAst.Declaration.Sig, loc: SourceLocation): ResolvedAst.Expression = {
        // Find the arity of the function definition.
        val arity = sig.spec.fparams.length

        // Create the fresh fparams
        val fparams = mkFreshFparams(arity, loc.asSynthetic)

        // The signature expression.
        val sigExp = ResolvedAst.Expression.Sig(sig.sym, loc)

        // Create and apply the lambda expressions
        mkCurriedLambda(fparams, sigExp, loc.asSynthetic)
      }

      /**
        * Curry the tag, wrapping it in a lambda expression if it is not nullary.
        */
      def visitTag(caze: NamedAst.Declaration.Case, loc: SourceLocation): ResolvedAst.Expression = {
        // Check if the tag value has Unit type.
        if (isUnitType(caze.tpe)) {
          // Case 1: The tag value has Unit type. Construct the Unit expression.
          val e = ResolvedAst.Expression.Cst(Ast.Constant.Unit, loc)
          ResolvedAst.Expression.Tag(Ast.CaseSymUse(caze.sym, loc), e, loc)
        } else {
          // Case 2: The tag has a non-Unit type. Hence the tag is used as a function.
          // If the tag is `Some` we construct the lambda: x -> Some(x).

          // Construct a fresh symbol for the formal parameter.
          val freshVar = Symbol.freshVarSym("x" + Flix.Delimiter, BoundBy.FormalParam, loc)

          // Construct the formal parameter for the fresh symbol.
          val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, None, loc)

          // Construct a variable expression for the fresh symbol.
          val varExp = ResolvedAst.Expression.Var(freshVar, loc)

          // Construct the tag expression on the fresh symbol expression.
          val tagExp = ResolvedAst.Expression.Tag(Ast.CaseSymUse(caze.sym, loc), varExp, loc)

          // Assemble the lambda expressions.
          ResolvedAst.Expression.Lambda(freshParam, tagExp, loc)
        }
      }

      /**
        * Curry the tag, wrapping it in a lambda expression if it is not nullary.
        */
      def visitRestrictableTag(caze: NamedAst.Declaration.RestrictableCase, loc: SourceLocation): ResolvedAst.Expression = {
        // Check if the tag value has Unit type.
        if (isUnitType(caze.tpe)) {
          // Case 1: The tag value has Unit type. Construct the Unit expression.
          val e = ResolvedAst.Expression.Cst(Ast.Constant.Unit, loc)
          ResolvedAst.Expression.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, loc), e, loc)
        } else {
          // Case 2: The tag has a non-Unit type. Hence the tag is used as a function.
          // If the tag is `Some` we construct the lambda: x -> Some(x).

          // Construct a fresh symbol for the formal parameter.
          val freshVar = Symbol.freshVarSym("x" + Flix.Delimiter, BoundBy.FormalParam, loc)

          // Construct the formal parameter for the fresh symbol.
          val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, None, loc)

          // Construct a variable expression for the fresh symbol.
          val varExp = ResolvedAst.Expression.Var(freshVar, loc)

          // Construct the tag expression on the fresh symbol expression.
          val tagExp = ResolvedAst.Expression.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, loc), varExp, loc)

          // Assemble the lambda expressions.
          ResolvedAst.Expression.Lambda(freshParam, tagExp, loc)
        }
      }

      /**
        * Resolve the application expression, performing currying over the subexpressions.
        */
      def visitApply(exp: NamedAst.Expression.Apply, env0: ListMap[String, Resolution], region: Option[Symbol.VarSym]): Validation[ResolvedAst.Expression, ResolutionError] = exp match {
        case NamedAst.Expression.Apply(exp0, exps0, loc) =>
          val expVal = visitExp(exp0, env0, region)
          val expsVal = traverse(exps0)(visitExp(_, env0, region))
          mapN(expVal, expsVal) {
            case (e, es) =>
              es.foldLeft(e) {
                case (acc, a) => ResolvedAst.Expression.Apply(acc, List(a), loc.asSynthetic)
              }
          }
      }

      /**
        * Resolve the application expression, applying `defn` to `exps`.
        */
      def visitApplyDef(app: NamedAst.Expression.Apply, defn: NamedAst.Declaration.Def, exps: List[NamedAst.Expression], env0: ListMap[String, Resolution], region: Option[Symbol.VarSym], innerLoc: SourceLocation, outerLoc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        if (defn.spec.fparams.length == exps.length) {
          // Case 1: Hooray! We can call the function directly.
          val esVal = traverse(exps)(visitExp(_, env0, region))
          mapN(esVal) {
            es =>
              val base = ResolvedAst.Expression.Def(defn.sym, innerLoc)
              ResolvedAst.Expression.Apply(base, es, outerLoc)
          }
        } else {
          // Case 2: We have to curry. (See below).
          visitApply(app, env0, region)
        }
      }

      /**
        * Resolve the application expression, applying `sig` to `exps`.
        */
      def visitApplySig(app: NamedAst.Expression.Apply, sig: NamedAst.Declaration.Sig, exps: List[NamedAst.Expression], env0: ListMap[String, Resolution], region: Option[Symbol.VarSym], innerLoc: SourceLocation, outerLoc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        if (sig.spec.fparams.length == exps.length) {
          // Case 1: Hooray! We can call the function directly.
          val esVal = traverse(exps)(visitExp(_, env0, region))
          mapN(esVal) {
            case es =>
              val base = ResolvedAst.Expression.Sig(sig.sym, innerLoc)
              ResolvedAst.Expression.Apply(base, es, outerLoc)
          }
        } else {
          // Case 2: We have to curry. (See below).
          visitApply(app, env0, region)
        }
      }

      /**
        * Resolves the tag application.
        */
      def visitApplyTag(caze: NamedAst.Declaration.Case, exps: List[NamedAst.Expression], env0: ListMap[String, Resolution], region: Option[Symbol.VarSym], innerLoc: SourceLocation, outerLoc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        val esVal = traverse(exps)(visitExp(_, env0, region))
        mapN(esVal) {
          // Case 1: one expression. No tuple.
          case e :: Nil =>
            ResolvedAst.Expression.Tag(Ast.CaseSymUse(caze.sym, innerLoc), e, outerLoc)
          // Case 2: multiple expressions. Make them a tuple
          case es =>
            val exp = ResolvedAst.Expression.Tuple(es, outerLoc)
            ResolvedAst.Expression.Tag(Ast.CaseSymUse(caze.sym, innerLoc), exp, outerLoc)
        }
      }

      /**
        * Resolves the tag application.
        */
      def visitApplyRestrictableTag(caze: NamedAst.Declaration.RestrictableCase, exps: List[NamedAst.Expression], env0: ListMap[String, Resolution], region: Option[Symbol.VarSym], innerLoc: SourceLocation, outerLoc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        val esVal = traverse(exps)(visitExp(_, env0, region))
        mapN(esVal) {
          // Case 1: one expression. No tuple.
          case e :: Nil =>
            ResolvedAst.Expression.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, innerLoc), e, outerLoc)
          // Case 2: multiple expressions. Make them a tuple
          case es =>
            val exp = ResolvedAst.Expression.Tuple(es, outerLoc)
            ResolvedAst.Expression.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, innerLoc), exp, outerLoc)
        }
      }


      /**
        * Local visitor.
        */
      def visitExp(e0: NamedAst.Expression, env0: ListMap[String, Resolution], region: Option[Symbol.VarSym]): Validation[ResolvedAst.Expression, ResolutionError] = e0 match {

        case NamedAst.Expression.Wild(loc) =>
          ResolvedAst.Expression.Wild(loc).toSuccess

        case NamedAst.Expression.Ambiguous(name, loc) =>
          mapN(lookupTerm(name, env0, ns0, root)) {
            case ResolvedTerm.Def(defn) => visitDef(defn, loc)
            case ResolvedTerm.Sig(sig) => visitSig(sig, loc)
            case ResolvedTerm.Var(sym) => ResolvedAst.Expression.Var(sym, loc)
            case ResolvedTerm.Tag(caze) => visitTag(caze, loc)
            case ResolvedTerm.RestrictableTag(caze) => visitRestrictableTag(caze, loc)
          }

        case NamedAst.Expression.Hole(nameOpt, loc) =>
          val sym = nameOpt match {
            case None => Symbol.freshHoleSym(loc)
            case Some(name) => Symbol.mkHoleSym(ns0, name)
          }
          ResolvedAst.Expression.Hole(sym, loc).toSuccess

        case NamedAst.Expression.HoleWithExp(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            case e => ResolvedAst.Expression.HoleWithExp(e, loc)
          }

        case NamedAst.Expression.Use(use, exp, loc) =>
          // Lookup the used name and add it to the env
          use match {
            case NamedAst.UseOrImport.Use(qname, alias, _) =>
              // TODO NS-REFACTOR allowing relative uses here...
              flatMapN(lookupQualifiedName(qname, env0, ns0, root)) {
                case decls =>
                  val env = decls.foldLeft(env0) {
                    case (acc, decl) => acc + (alias.name -> Resolution.Declaration(decl))
                  }
                  mapN(visitExp(exp, env, region)) {
                    // TODO NS-REFACTOR: multiple uses here
                    case e => ResolvedAst.Expression.Use(getSym(decls.head), e, loc)
                  }
              }

            case NamedAst.UseOrImport.Import(_, _, loc) => throw InternalCompilerException("unexpected import", loc)
          }

        case NamedAst.Expression.Cst(cst, loc) => ResolvedAst.Expression.Cst(cst, loc).toSuccess

        case app@NamedAst.Expression.Apply(NamedAst.Expression.Ambiguous(qname, innerLoc), exps, outerLoc) =>
          flatMapN(lookupTerm(qname, env0, ns0, root)) {
            case ResolvedTerm.Def(defn) => visitApplyDef(app, defn, exps, env0, region, innerLoc, outerLoc)
            case ResolvedTerm.Sig(sig) => visitApplySig(app, sig, exps, env0, region, innerLoc, outerLoc)
            case ResolvedTerm.Var(_) => visitApply(app, env0, region)
            case ResolvedTerm.Tag(caze) => visitApplyTag(caze, exps, env0, region, innerLoc, outerLoc)
            case ResolvedTerm.RestrictableTag(caze) => visitApplyRestrictableTag(caze, exps, env0, region, innerLoc, outerLoc)
          }

        case app@NamedAst.Expression.Apply(_, _, _) =>
          visitApply(app, env0, region)

        case NamedAst.Expression.Lambda(fparam, exp, loc) =>
          val pVal = Params.resolve(fparam, env0, taenv, ns0, root)
          flatMapN(pVal) {
            case p =>
              val env = env0 ++ mkFormalParamEnv(List(p))
              val eVal = visitExp(exp, env, region)
              mapN(eVal) {
                case e => ResolvedAst.Expression.Lambda(p, e, loc)
              }
          }

        case NamedAst.Expression.Unary(sop, exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Unary(sop, e, loc)
          }

        case NamedAst.Expression.Binary(sop, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Binary(sop, e1, e2, loc)
          }

        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          val e3Val = visitExp(exp3, env0, region)
          mapN(e1Val, e2Val, e3Val) {
            case (e1, e2, e3) => ResolvedAst.Expression.IfThenElse(e1, e2, e3, loc)
          }

        case NamedAst.Expression.Stm(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Stm(e1, e2, loc)
          }

        case NamedAst.Expression.Discard(exp, loc) =>
          visitExp(exp, env0, region) map {
            case e => ResolvedAst.Expression.Discard(e, loc)
          }

        case NamedAst.Expression.Let(sym, mod, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val env = env0 ++ mkVarEnv(sym)
          val e2Val = visitExp(exp2, env, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Let(sym, mod, e1, e2, loc)
          }

        case NamedAst.Expression.LetRec(sym, mod, exp1, exp2, loc) =>
          val env = env0 ++ mkVarEnv(sym)
          val e1Val = visitExp(exp1, env, region)
          val e2Val = visitExp(exp2, env, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.LetRec(sym, mod, e1, e2, loc)
          }

        case NamedAst.Expression.Region(tpe, loc) =>
          ResolvedAst.Expression.Region(tpe, loc).toSuccess

        case NamedAst.Expression.Scope(sym, regionVar, exp, loc) =>
          val env = env0 ++ mkVarEnv(sym) ++ mkTypeVarEnv(regionVar)
          val eVal = visitExp(exp, env, Some(sym))
          mapN(eVal) {
            e => ResolvedAst.Expression.Scope(sym, regionVar, e, loc)
          }

        case NamedAst.Expression.Match(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.MatchRule(pat, guard, body) =>
              val pVal = Patterns.resolve(pat, env0, ns0, root)
              flatMapN(pVal) {
                case p =>
                  val env = env0 ++ mkPatternEnv(p)
                  val gVal = traverseOpt(guard)(visitExp(_, env, region))
                  val bVal = visitExp(body, env, region)
                  mapN(gVal, bVal) {
                    case (g, b) => ResolvedAst.MatchRule(p, g, b)
                  }
              }
          }

          val eVal = visitExp(exp, env0, region)
          val rsVal = rulesVal
          mapN(eVal, rsVal) {
            case (e, rs) => ResolvedAst.Expression.Match(e, rs, loc)
          }

        case NamedAst.Expression.TypeMatch(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.MatchTypeRule(sym, tpe, body) =>
              val tVal = resolveType(tpe, Wildness.AllowWild, env0, taenv, ns0, root)
              val env = env0 ++ mkVarEnv(sym)
              val bVal = visitExp(body, env, region)
              mapN(tVal, bVal) {
                case (t, b) => ResolvedAst.MatchTypeRule(sym, t, b)
              }
          }

          val eVal = visitExp(exp, env0, region)
          val rsVal = rulesVal
          mapN(eVal, rsVal) {
            case (e, rs) => ResolvedAst.Expression.TypeMatch(e, rs, loc)
          }

        case NamedAst.Expression.RelationalChoose(star, exps, rules, loc) =>
          val expsVal = traverse(exps)(visitExp(_, env0, region))
          val rulesVal = traverse(rules) {
            case NamedAst.RelationalChoiceRule(pat0, exp0) =>
              val p = pat0.map {
                case NamedAst.RelationalChoicePattern.Wild(loc) => ResolvedAst.RelationalChoicePattern.Wild(loc)
                case NamedAst.RelationalChoicePattern.Absent(loc) => ResolvedAst.RelationalChoicePattern.Absent(loc)
                case NamedAst.RelationalChoicePattern.Present(sym, loc) => ResolvedAst.RelationalChoicePattern.Present(sym, loc)
              }
              val env = pat0.foldLeft(env0) {
                case (acc, NamedAst.RelationalChoicePattern.Wild(_)) => acc
                case (acc, NamedAst.RelationalChoicePattern.Absent(_)) => acc
                case (acc, NamedAst.RelationalChoicePattern.Present(sym, _)) => acc + (sym.text -> Resolution.Var(sym))
              }
              mapN(visitExp(exp0, env, region)) {
                case e => ResolvedAst.RelationalChoiceRule(p, e)
              }
          }
          mapN(expsVal, rulesVal) {
            case (es, rs) => ResolvedAst.Expression.RelationalChoose(star, es, rs, loc)
          }

        case NamedAst.Expression.RestrictableChoose(star, exp, rules, loc) =>
          val expVal = visitExp(exp, env0, region)
          val rulesVal = traverse(rules) {
            case NamedAst.RestrictableChoiceRule(pat0, exp0) =>
              val pVal = pat0 match {
                case NamedAst.RestrictableChoicePattern.Tag(qname, pat, loc) =>
                  val tagVal = lookupRestrictableTag(qname, env0, ns0, root)
                  val pats = pat.map {
                    case NamedAst.RestrictableChoicePattern.Wild(loc) => ResolvedAst.RestrictableChoicePattern.Wild(loc)
                    case NamedAst.RestrictableChoicePattern.Var(sym, loc) => ResolvedAst.RestrictableChoicePattern.Var(sym, loc)
                  }
                  mapN(tagVal) {
                    case tag => ResolvedAst.RestrictableChoicePattern.Tag(Ast.RestrictableCaseSymUse(tag.sym, qname.loc), pats, loc)
                  }
              }
              val env = pat0 match {
                case RestrictableChoicePattern.Tag(qname, pat, loc) =>
                  pat.foldLeft(env0) {
                    case (acc, NamedAst.RestrictableChoicePattern.Var(sym, loc)) => acc + (sym.text -> Resolution.Var(sym))
                    case (acc, NamedAst.RestrictableChoicePattern.Wild(loc)) => acc
                  }
              }

              val eVal = visitExp(exp0, env, region)
              flatMapN(pVal, eVal) {
                case (p, e) =>
                  val symVal = if (star) {
                    e match {
                      case ResolvedAst.Expression.RestrictableTag(sym, exp, loc) => Some(sym.sym).toSuccess
                      case ResolvedAst.Expression.Of(sym, exp, loc) => Some(sym.sym).toSuccess
                      case otherExp => ResolutionError.MissingRestrictableTag(otherExp.loc).toFailure
                    }
                  } else {
                    None.toSuccess
                  }

                  mapN(symVal) {
                    case sym => ResolvedAst.RestrictableChoiceRule(p, sym, e)
                  }
              }
          }
          mapN(expVal, rulesVal) {
            case (e, rs) => ResolvedAst.Expression.RestrictableChoose(star, e, rs, loc)
          }

        case NamedAst.Expression.Tuple(elms, loc) =>
          val esVal = traverse(elms)(e => visitExp(e, env0, region))
          mapN(esVal) {
            es => ResolvedAst.Expression.Tuple(es, loc)
          }

        case NamedAst.Expression.RecordEmpty(loc) =>
          ResolvedAst.Expression.RecordEmpty(loc).toSuccess

        case NamedAst.Expression.RecordSelect(base, field, loc) =>
          val bVal = visitExp(base, env0, region)
          mapN(bVal) {
            b => ResolvedAst.Expression.RecordSelect(b, field, loc)
          }

        case NamedAst.Expression.RecordExtend(field, value, rest, loc) =>
          val vVal = visitExp(value, env0, region)
          val rVal = visitExp(rest, env0, region)
          mapN(vVal, rVal) {
            case (v, r) => ResolvedAst.Expression.RecordExtend(field, v, r, loc)
          }

        case NamedAst.Expression.RecordRestrict(field, rest, loc) =>
          val rVal = visitExp(rest, env0, region)
          mapN(rVal) {
            r => ResolvedAst.Expression.RecordRestrict(field, r, loc)
          }

        case NamedAst.Expression.New(qname, exp, loc) =>
          val erVal = traverseOpt(exp)(visitExp(_, env0, region))
          mapN(erVal) {
            er =>
              ///
              /// Translate [[new Foo(r)]] => Newable.new(r)
              /// Translate [[new Foo()]]  => Newable.new(currentRegion)
              ///
              val sp1 = qname.sp1
              val sp2 = qname.sp2
              val classSym = Symbol.mkClassSym(Name.RootNS, Name.Ident(sp1, "Newable", sp2))
              val sigSym = Symbol.mkSigSym(classSym, Name.Ident(sp1, "new", sp2))
              val newExp = ResolvedAst.Expression.Sig(sigSym, loc)
              val reg = getExplicitOrImplicitRegion(er, region, loc)
              ResolvedAst.Expression.Apply(newExp, List(reg), loc)
          }

        case NamedAst.Expression.ArrayLit(exps, exp, loc) =>
          val esVal = traverse(exps)(visitExp(_, env0, region))
          val erVal = traverseOpt(exp)(visitExp(_, env0, region))
          mapN(esVal, erVal) {
            case (es, er) =>
              val reg = getExplicitOrImplicitRegion(er, region, loc)
              ResolvedAst.Expression.ArrayLit(es, reg, loc)
          }

        case NamedAst.Expression.ArrayNew(exp1, exp2, exp3, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          val e3Val = visitExp(exp3, env0, region)
          mapN(e1Val, e2Val, e3Val) {
            case (e1, e2, e3) =>
              ResolvedAst.Expression.ArrayNew(e1, e2, e3, loc)
          }

        case NamedAst.Expression.ArrayLoad(base, index, loc) =>
          val bVal = visitExp(base, env0, region)
          val iVal = visitExp(index, env0, region)
          mapN(bVal, iVal) {
            case (b, i) => ResolvedAst.Expression.ArrayLoad(b, i, loc)
          }

        case NamedAst.Expression.ArrayStore(base, index, elm, loc) =>
          val bVal = visitExp(base, env0, region)
          val iVal = visitExp(index, env0, region)
          val eVal = visitExp(elm, env0, region)
          mapN(bVal, iVal, eVal) {
            case (b, i, e) => ResolvedAst.Expression.ArrayStore(b, i, e, loc)
          }

        case NamedAst.Expression.ArrayLength(base, loc) =>
          val bVal = visitExp(base, env0, region)
          mapN(bVal) {
            b => ResolvedAst.Expression.ArrayLength(b, loc)
          }

        case NamedAst.Expression.ArraySlice(reg, base, startIndex, endIndex, loc) =>
          val regVal = visitExp(reg, env0, region)
          val bVal = visitExp(base, env0, region)
          val i1Val = visitExp(startIndex, env0, region)
          val i2Val = visitExp(endIndex, env0, region)
          mapN(regVal, bVal, i1Val, i2Val) {
            case (r, b, i1, i2) => ResolvedAst.Expression.ArraySlice(r, b, i1, i2, loc)
          }

        case NamedAst.Expression.Ref(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = traverseOpt(exp2)(visitExp(_, env0, region))
          mapN(e1Val, e2Val) {
            case (e1, e2) =>
              val reg = getExplicitOrImplicitRegion(e2, region, loc)
              ResolvedAst.Expression.Ref(e1, reg, loc)
          }

        case NamedAst.Expression.Deref(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Deref(e, loc)
          }

        case NamedAst.Expression.Assign(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Assign(e1, e2, loc)
          }

        case NamedAst.Expression.Ascribe(exp, expectedType, expectedEff, loc) =>
          val expectedTypVal = expectedType match {
            case None => (None: Option[UnkindedType]).toSuccess
            case Some(t) => mapN(resolveType(t, Wildness.AllowWild, env0, taenv, ns0, root))(x => Some(x))
          }
          val expectedEffVal = resolvePurityAndEffect(expectedEff, Wildness.AllowWild, env0, taenv, ns0, root)

          val eVal = visitExp(exp, env0, region)
          mapN(eVal, expectedTypVal, expectedEffVal) {
            case (e, t, f) => ResolvedAst.Expression.Ascribe(e, t, f, loc)
          }

        case NamedAst.Expression.Of(qname, exp, loc) =>
          val tagVal = lookupRestrictableTag(qname, env0, ns0, root)
          val eVal = visitExp(exp, env0, region)
          mapN(tagVal, eVal) {
            case (tag, e) => ResolvedAst.Expression.Of(Ast.RestrictableCaseSymUse(tag.sym, qname.loc), e, loc)
          }

        case NamedAst.Expression.Cast(exp, declaredType, declaredEff, loc) =>
          val declaredTypVal = declaredType match {
            case None => (None: Option[UnkindedType]).toSuccess
            case Some(t) => mapN(resolveType(t, Wildness.ForbidWild, env0, taenv, ns0, root))(x => Some(x))
          }
          val declaredEffVal = resolvePurityAndEffect(declaredEff, Wildness.ForbidWild, env0, taenv, ns0, root)

          val eVal = visitExp(exp, env0, region)
          mapN(eVal, declaredTypVal, declaredEffVal) {
            case (e, t, f) => ResolvedAst.Expression.Cast(e, t, f, loc)
          }

        case NamedAst.Expression.Mask(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            case e => ResolvedAst.Expression.Mask(e, loc)
          }

        case NamedAst.Expression.Upcast(exp, loc) =>
          mapN(visitExp(exp, env0, region)) {
            case e => ResolvedAst.Expression.Upcast(e, loc)
          }

        case NamedAst.Expression.Supercast(exp, loc) =>
          mapN(visitExp(exp, env0, region)) {
            case e => ResolvedAst.Expression.Supercast(e, loc)
          }

        case NamedAst.Expression.TryCatch(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.CatchRule(sym, className, body) =>
              val env = env0 ++ mkVarEnv(sym)
              val clazzVal = lookupJvmClass(className, sym.loc)
              val bVal = visitExp(body, env, region)
              mapN(clazzVal, bVal) {
                case (clazz, b) => ResolvedAst.CatchRule(sym, clazz, b)
              }
          }

          val eVal = visitExp(exp, env0, region)
          mapN(eVal, rulesVal) {
            case (e, rs) => ResolvedAst.Expression.TryCatch(e, rs, loc)
          }

        case NamedAst.Expression.Without(exp, eff, loc) =>
          val eVal = visitExp(exp, env0, region)
          val fVal = lookupEffect(eff, env0, ns0, root)
          mapN(eVal, fVal) {
            case (e, f) =>
              val effUse = Ast.EffectSymUse(f.sym, eff.loc)
              ResolvedAst.Expression.Without(e, effUse, loc)
          }

        case NamedAst.Expression.TryWith(exp, eff, rules, loc) =>
          val eVal = visitExp(exp, env0, region)
          val fVal = lookupEffect(eff, env0, ns0, root)
          flatMapN(eVal, fVal) {
            case (e, f) =>
              val effUse = Ast.EffectSymUse(f.sym, eff.loc)
              val rulesVal = traverse(rules) {
                case NamedAst.HandlerRule(ident, fparams, body) =>
                  val opVal = findOpInEffect(ident, f)
                  val fparamsVal = resolveFormalParams(fparams, env0, taenv, ns0, root)
                  flatMapN(opVal, fparamsVal) {
                    case (o, fp) =>
                      val env = env0 ++ mkFormalParamEnv(fp)
                      val bodyVal = visitExp(body, env, region)
                      mapN(bodyVal) {
                        case b =>
                          val opUse = Ast.OpSymUse(o.sym, ident.loc)
                          ResolvedAst.HandlerRule(opUse, fp, b)
                      }
                  }
              }
              mapN(rulesVal) {
                rs => ResolvedAst.Expression.TryWith(e, effUse, rs, loc)
              }
          }

        case NamedAst.Expression.Do(op, exps, loc) =>
          val opVal = lookupOp(op, env0, ns0, root)
          val expsVal = traverse(exps)(visitExp(_, env0, region))
          mapN(opVal, expsVal) {
            case (o, es) =>
              val opUse = Ast.OpSymUse(o.sym, op.loc)
              ResolvedAst.Expression.Do(opUse, es, loc)
          }

        case NamedAst.Expression.Resume(exp, loc) =>
          val expVal = visitExp(exp, env0, region)
          mapN(expVal) {
            e => ResolvedAst.Expression.Resume(e, loc)
          }

        case NamedAst.Expression.InvokeConstructor(className, args, sig, loc) =>
          val argsVal = traverse(args)(visitExp(_, env0, region))
          val sigVal = traverse(sig)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmConstructor(className, ts, loc)) {
                case constructor => ResolvedAst.Expression.InvokeConstructor(constructor, as, loc)
              }
          }

        case NamedAst.Expression.InvokeMethod(className, methodName, exp, args, sig, retTpe, loc) =>
          val expVal = visitExp(exp, env0, region)
          val argsVal = traverse(args)(visitExp(_, env0, region))
          val sigVal = traverse(sig)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
          val retVal = resolveType(retTpe, Wildness.ForbidWild, env0, taenv, ns0, root)
          val clazzVal = lookupJvmClass(className, loc)
          flatMapN(sigVal, expVal, argsVal, retVal, clazzVal) {
            case (ts, e, as, ret, clazz) =>
              mapN(lookupJvmMethod(clazz, methodName, ts, ret, static = false, loc)) {
                case method => ResolvedAst.Expression.InvokeMethod(method, clazz, e, as, loc)
              }
          }

        case NamedAst.Expression.InvokeStaticMethod(className, methodName, args, sig, retTpe, loc) =>
          val argsVal = traverse(args)(visitExp(_, env0, region))
          val sigVal = traverse(sig)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
          val retVal = resolveType(retTpe, Wildness.ForbidWild, env0, taenv, ns0, root)
          val clazzVal = lookupJvmClass(className, loc)
          flatMapN(sigVal, argsVal, retVal, clazzVal) {
            case (ts, as, ret, clazz) =>
              mapN(lookupJvmMethod(clazz, methodName, ts, ret, static = true, loc)) {
                case method => ResolvedAst.Expression.InvokeStaticMethod(method, as, loc)
              }
          }

        case NamedAst.Expression.GetField(className, fieldName, exp, loc) =>
          flatMapN(lookupJvmClass(className, loc)) {
            case clazz =>
              mapN(lookupJvmField(clazz, fieldName, static = false, loc), visitExp(exp, env0, region)) {
                case (field, e) => ResolvedAst.Expression.GetField(field, clazz, e, loc)
              }
          }

        case NamedAst.Expression.PutField(className, fieldName, exp1, exp2, loc) =>
          flatMapN(lookupJvmClass(className, loc)) {
            case clazz =>
              mapN(lookupJvmField(clazz, fieldName, static = false, loc), visitExp(exp1, env0, region), visitExp(exp2, env0, region)) {
                case (field, e1, e2) => ResolvedAst.Expression.PutField(field, clazz, e1, e2, loc)
              }
          }

        case NamedAst.Expression.GetStaticField(className, fieldName, loc) =>
          flatMapN(lookupJvmClass(className, loc)) {
            case clazz =>
              mapN(lookupJvmField(clazz, fieldName, static = true, loc)) {
                case field => ResolvedAst.Expression.GetStaticField(field, loc)
              }
          }

        case NamedAst.Expression.PutStaticField(className, fieldName, exp, loc) =>
          flatMapN(lookupJvmClass(className, loc)) {
            case clazz =>
              mapN(lookupJvmField(clazz, fieldName, static = true, loc), visitExp(exp, env0, region)) {
                case (field, e) => ResolvedAst.Expression.PutStaticField(field, e, loc)
              }
          }

        case NamedAst.Expression.NewObject(name, tpe, methods, loc) =>
          flatMapN(resolveType(tpe, Wildness.ForbidWild, env0, taenv, ns0, root), traverse(methods)(visitJvmMethod(_, env0, taenv, ns0, root))) {
            case (t, ms) =>
              //
              // Check that the type is a JVM type (after type alias erasure).
              //
              UnkindedType.eraseAliases(t) match {
                case UnkindedType.Cst(TypeConstructor.Native(clazz), _) =>
                  ResolvedAst.Expression.NewObject(name, clazz, ms, loc).toSuccess
                case _ => ResolutionError.IllegalNonJavaType(t, t.loc).toFailure
              }
          }

        case NamedAst.Expression.NewChannel(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.NewChannel(e1, e2, loc)
          }

        case NamedAst.Expression.GetChannel(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.GetChannel(e, loc)
          }

        case NamedAst.Expression.PutChannel(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.PutChannel(e1, e2, loc)
          }

        case NamedAst.Expression.SelectChannel(rules, default, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.SelectChannelRule(sym, chan, body) =>
              val cVal = visitExp(chan, env0, region)
              val env = env0 ++ mkVarEnv(sym)
              val bVal = visitExp(body, env, region)
              mapN(cVal, bVal) {
                case (c, b) => ResolvedAst.SelectChannelRule(sym, c, b)
              }
          }

          val defaultVal = default match {
            case Some(exp) =>
              val eVal = visitExp(exp, env0, region)
              mapN(eVal) {
                e => Some(e)
              }
            case None => None.toSuccess
          }

          mapN(rulesVal, defaultVal) {
            case (rs, d) => ResolvedAst.Expression.SelectChannel(rs, d, loc)
          }

        case NamedAst.Expression.Spawn(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) =>
              val reg = getExplicitOrImplicitRegion(Some(e2), region, loc)
              ResolvedAst.Expression.Spawn(e1, reg, loc)
          }

        case NamedAst.Expression.Par(exp, loc) =>
          mapN(visitExp(exp, env0, region)) {
            e => ResolvedAst.Expression.Par(e, loc)
          }

        case NamedAst.Expression.ParYield(frags, exp, loc) =>

          // mutable env to be updated during traversal
          var finalUenv = env0

          val fragsVal = traverse(frags) {
            case NamedAst.ParYieldFragment(pat, e0, l0) =>
              val pVal = Patterns.resolve(pat, env0, ns0, root)
              flatMapN(pVal) {
                case p =>
                  val patEnv = mkPatternEnv(p)
                  val env = env0 ++ patEnv
                  finalUenv = finalUenv ++ patEnv
                  val e0Val = visitExp(e0, env, region)
                  mapN(e0Val) {
                    case e1 => ResolvedAst.ParYieldFragment(p, e1, l0)
                  }
              }
          }

          mapN(fragsVal, visitExp(exp, finalUenv, region)) {
            case (fs, e) => ResolvedAst.Expression.ParYield(fs, e, loc)
          }

        case NamedAst.Expression.Lazy(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Lazy(e, loc)
          }

        case NamedAst.Expression.Force(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Force(e, loc)
          }

        case NamedAst.Expression.FixpointConstraintSet(cs0, loc) =>
          val csVal = traverse(cs0)(Constraints.resolve(_, env0, taenv, ns0, root))
          mapN(csVal) {
            cs => ResolvedAst.Expression.FixpointConstraintSet(cs, loc)
          }

        case NamedAst.Expression.FixpointLambda(pparams, exp, loc) =>
          val psVal = traverse(pparams)(Params.resolve(_, env0, taenv, ns0, root))
          val eVal = visitExp(exp, env0, region)
          mapN(psVal, eVal) {
            case (ps, e) => ResolvedAst.Expression.FixpointLambda(ps, e, loc)
          }

        case NamedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.FixpointMerge(e1, e2, loc)
          }

        case NamedAst.Expression.FixpointSolve(exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.FixpointSolve(e, loc)
          }

        case NamedAst.Expression.FixpointFilter(pred, exp, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.FixpointFilter(pred, e, loc)
          }

        case NamedAst.Expression.FixpointInject(exp, pred, loc) =>
          val eVal = visitExp(exp, env0, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.FixpointInject(e, pred, loc)
          }

        case NamedAst.Expression.FixpointProject(pred, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, env0, region)
          val e2Val = visitExp(exp2, env0, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.FixpointProject(pred, e1, e2, loc)
          }

        case NamedAst.Expression.Error(m) =>
          ResolvedAst.Expression.Error(m).toSoftFailure
      }

      /**
        * Performs name resolution on the given JvmMethod `method` in the namespace `ns0`.
        */
      def visitJvmMethod(method: NamedAst.JvmMethod, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.JvmMethod, ResolutionError] = method match {
        case NamedAst.JvmMethod(ident, fparams, exp, tpe, purAndEff, loc) =>
          val fparamsVal = resolveFormalParams(fparams, env0, taenv, ns0, root)
          flatMapN(fparamsVal) {
            case fparams =>
              val env = env0 ++ mkFormalParamEnv(fparams)
              val expVal = visitExp(exp, env, None)
              val tpeVal = resolveType(tpe, Wildness.ForbidWild, env, taenv, ns0, root)
              val purAndEffVal = resolvePurityAndEffect(purAndEff, Wildness.ForbidWild, env, taenv, ns0, root)
              mapN(expVal, tpeVal, purAndEffVal) {
                case (e, t, p) => ResolvedAst.JvmMethod(ident, fparams, e, t, p, loc)
              }
          }
      }

      visitExp(exp0, env00, None)
    }

  }

  object Patterns {

    /**
      * Performs name resolution on the given constraint pattern `pat0` in the namespace `ns0`.
      * Constraint patterns do not introduce new variables.
      */
    def resolveInConstraint(pat0: NamedAst.Pattern, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

      def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
        case NamedAst.Pattern.Wild(loc) => ResolvedAst.Pattern.Wild(loc).toSuccess

        case NamedAst.Pattern.Var(sym0, loc) =>
          // TODO NS-REFACTOR wild patterns should not be counted as vars
          // if the sym is wild then just call the pattern wild
          if (sym0.isWild) {
            ResolvedAst.Pattern.Wild(loc).toSuccess
          } else {
            env(sym0.text).collectFirst {
              case Resolution.Var(sym) => sym
            } match {
              case Some(sym) => ResolvedAst.Pattern.Var(sym, loc).toSuccess
              case None => throw InternalCompilerException("unexpected unrecognized sym in constraint pattern", loc)
            }
          }

        case NamedAst.Pattern.Cst(cst, loc) => ResolvedAst.Pattern.Cst(cst, loc).toSuccess

        case NamedAst.Pattern.Tag(qname, pat, loc) =>
          val cVal = lookupTag(qname, env, ns0, root)
          val pVal = visit(pat)
          mapN(cVal, pVal) {
            case (c, p) =>
              ResolvedAst.Pattern.Tag(Ast.CaseSymUse(c.sym, qname.loc), p, loc)
          }

        case NamedAst.Pattern.Tuple(elms, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.Tuple(es, loc)
          }

        case NamedAst.Pattern.Array(elms, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.Array(es, loc)
          }

        case NamedAst.Pattern.ArrayTailSpread(elms, sym, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.ArrayTailSpread(es, sym, loc)
          }

        case NamedAst.Pattern.ArrayHeadSpread(sym, elms, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.ArrayHeadSpread(sym, es, loc)
          }
      }

      visit(pat0)
    }

    /**
      * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
      */
    def resolve(pat0: NamedAst.Pattern, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

      def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
        case NamedAst.Pattern.Wild(loc) => ResolvedAst.Pattern.Wild(loc).toSuccess

        case NamedAst.Pattern.Var(sym, loc) => ResolvedAst.Pattern.Var(sym, loc).toSuccess

        case NamedAst.Pattern.Cst(cst, loc) => ResolvedAst.Pattern.Cst(cst, loc).toSuccess

        case NamedAst.Pattern.Tag(qname, pat, loc) =>
          val cVal = lookupTag(qname, env, ns0, root)
          val pVal = visit(pat)
          mapN(cVal, pVal) {
            case (c, p) =>
              ResolvedAst.Pattern.Tag(Ast.CaseSymUse(c.sym, qname.loc), p, loc)
          }

        case NamedAst.Pattern.Tuple(elms, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.Tuple(es, loc)
          }

        case NamedAst.Pattern.Array(elms, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.Array(es, loc)
          }

        case NamedAst.Pattern.ArrayTailSpread(elms, sym, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.ArrayTailSpread(es, sym, loc)
          }

        case NamedAst.Pattern.ArrayHeadSpread(sym, elms, loc) =>
          val esVal = traverse(elms)(visit)
          mapN(esVal) {
            es => ResolvedAst.Pattern.ArrayHeadSpread(sym, es, loc)
          }
      }

      visit(pat0)
    }

  }

  object Predicates {

    object Head {
      /**
        * Performs name resolution on the given head predicate `h0` in the given namespace `ns0`.
        */
      def resolve(h0: NamedAst.Predicate.Head, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
        case NamedAst.Predicate.Head.Atom(pred, den, terms, loc) =>
          val tsVal = traverse(terms)(t => Expressions.resolve(t, env, taenv, ns0, root))
          mapN(tsVal) {
            ts => ResolvedAst.Predicate.Head.Atom(pred, den, ts, loc)
          }
      }
    }

    object Body {
      /**
        * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
        */
      def resolve(b0: NamedAst.Predicate.Body, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
        case NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
          val tsVal = traverse(terms)(t => Patterns.resolveInConstraint(t, env, ns0, root))
          mapN(tsVal) {
            ts => ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc)
          }

        case NamedAst.Predicate.Body.Guard(exp, loc) =>
          val eVal = Expressions.resolve(exp, env, taenv, ns0, root)
          mapN(eVal) {
            e => ResolvedAst.Predicate.Body.Guard(e, loc)
          }

        case NamedAst.Predicate.Body.Loop(idents, exp, loc) =>
          val varsVal = traverse(idents)(lookupVar(_, env))
          val eVal = Expressions.resolve(exp, env, taenv, ns0, root)
          mapN(varsVal, eVal) {
            case (vars, e) => ResolvedAst.Predicate.Body.Loop(vars, e, loc)
          }
      }
    }

  }

  object Params {

    /**
      * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
      */
    def resolve(fparam0: NamedAst.FormalParam, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.FormalParam, ResolutionError] = {
      val tVal = traverseOpt(fparam0.tpe)(resolveType(_, Wildness.AllowWild, env, taenv, ns0, root))
      mapN(tVal) {
        t => ResolvedAst.FormalParam(fparam0.sym, fparam0.mod, t, fparam0.loc)
      }
    }

    /**
      * Performs name resolution on the given predicate parameter `pparam0` in the given namespace `ns0`.
      */
    def resolve(pparam0: NamedAst.PredicateParam, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.PredicateParam, ResolutionError] = pparam0 match {
      case NamedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
        ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc).toSuccess

      case NamedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
        mapN(traverse(tpes)(resolveType(_, Wildness.ForbidWild, env, taenv, ns0, root))) {
          case ts => ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
        }

    }

    /**
      * Performs name resolution on the given type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolveTparam(tparam0: NamedAst.TypeParam): ResolvedAst.TypeParam = tparam0 match {
      case tparam: NamedAst.TypeParam.Kinded => resolveKindedTparam(tparam)
      case tparam: NamedAst.TypeParam.Unkinded => resolveUnkindedTparam(tparam)
      case tparam: NamedAst.TypeParam.Implicit => throw InternalCompilerException("unexpected implicit tparam", tparam.loc)
    }

    /**
      * Performs name resolution on the given kinded type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolveKindedTparam(tparam0: NamedAst.TypeParam.Kinded): ResolvedAst.TypeParam.Kinded = tparam0 match {
      case NamedAst.TypeParam.Kinded(name, tpe, kind, loc) => ResolvedAst.TypeParam.Kinded(name, tpe, kind, loc)
    }

    /**
      * Performs name resolution on the given unkinded type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolveUnkindedTparam(tparam0: NamedAst.TypeParam.Unkinded): ResolvedAst.TypeParam.Unkinded = tparam0 match {
      case NamedAst.TypeParam.Unkinded(name, tpe, loc) => ResolvedAst.TypeParam.Unkinded(name, tpe, loc)
    }

    /**
      * Performs name resolution on the given implicit type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolveImplicitTparam(tparam0: NamedAst.TypeParam.Implicit, env0: ListMap[String, Resolution]): Option[ResolvedAst.TypeParam.Unkinded] = tparam0 match {
      case NamedAst.TypeParam.Implicit(name, tpe, loc) =>
        // Check if the tparam is in the environment
        env0(name.name) collectFirst {
          case Resolution.TypeVar(sym) => sym
        } match {
          // Case 1: Already in the environment, this is not a type parameter.
          case Some(_) => None
          // Case 2: Not in the environment. This is a real type parameter.
          case None => Some(ResolvedAst.TypeParam.Unkinded(name, tpe, loc))
        }
    }

    /**
      * Performs name resolution on the given constraint parameter.
      */
    def resolveConstraintParam(cparam0: NamedAst.ConstraintParam, env0: ListMap[String, Resolution]): Option[ResolvedAst.ConstraintParam] = cparam0 match {
      case NamedAst.ConstraintParam(sym, loc) =>
        // Check if the cparam is in the environment
        env0(sym.text) collectFirst {
          case Resolution.Var(sym) => sym
        } match {
          // Case 1: Already in the environment, this is not a constraint parameter.
          case Some(_) => None
          // Case 2: Not in the environment. This is a real constraint parameter.
          case None => Some(ResolvedAst.ConstraintParam(sym, loc))
        }
    }
  }

  /**
    * Performs name resolution on the given formal parameters `fparams0`.
    */
  def resolveFormalParams(fparams0: List[NamedAst.FormalParam], env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.FormalParam], ResolutionError] = {
    traverse(fparams0)(fparam => Params.resolve(fparam, env, taenv, ns0, root))
  }

  /**
    * Performs name resolution on the given type parameters `tparams0`.
    */
  def resolveTypeParams(tparams0: NamedAst.TypeParams, env0: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): ResolvedAst.TypeParams = tparams0 match {
    case NamedAst.TypeParams.Kinded(tparams1) =>
      val tparams2 = tparams1.map(Params.resolveKindedTparam)
      ResolvedAst.TypeParams.Kinded(tparams2)
    case NamedAst.TypeParams.Unkinded(tparams1) =>
      val tparams2 = tparams1.map(Params.resolveUnkindedTparam)
      ResolvedAst.TypeParams.Unkinded(tparams2)
    case NamedAst.TypeParams.Implicit(tparams1) =>
      val tparams2 = tparams1.flatMap(Params.resolveImplicitTparam(_, env0))
      ResolvedAst.TypeParams.Unkinded(tparams2)
  }

  /**
    * Performs name resolution on the given constraint parameters `cparams0`.
    */
  def resolveConstraintParams(cparams0: List[NamedAst.ConstraintParam], env0: ListMap[String, Resolution]): List[ResolvedAst.ConstraintParam] = {
    cparams0.flatMap(Params.resolveConstraintParam(_, env0))
  }

  /**
    * Performs name resolution on the given type constraint `tconstr0`.
    */
  def resolveTypeConstraint(tconstr0: NamedAst.TypeConstraint, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.TypeConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TypeConstraint(clazz0, tpe0, loc) =>
      val classVal = lookupClass(clazz0, env, ns0, root)
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)

      mapN(classVal, tpeVal) {
        case (clazz, tpe) =>
          val head = Ast.TypeConstraint.Head(clazz.sym, clazz0.loc)
          ResolvedAst.TypeConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given superclass constraint `tconstr0`.
    */
  def resolveSuperClass(tconstr0: NamedAst.TypeConstraint, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.TypeConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TypeConstraint(clazz0, tpe0, loc) =>
      val classVal = lookupClassForImplementation(clazz0, env, ns0, root)
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)

      mapN(classVal, tpeVal) {
        case (clazz, tpe) =>
          val head = Ast.TypeConstraint.Head(clazz.sym, clazz0.loc)
          ResolvedAst.TypeConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given list of derivations `derives0`.
    */
  def resolveDerivations(qnames: List[Name.QName], env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[List[Ast.Derivation], ResolutionError] = {
    val derivesVal = Validation.traverse(qnames)(resolveDerivation(_, env, ns0, root))
    flatMapN(derivesVal) {
      derives =>
        val derivesWithIndex = derives.zipWithIndex
        val failures = for {
          (Ast.Derivation(sym1, loc1), i1) <- derivesWithIndex
          (Ast.Derivation(sym2, loc2), i2) <- derivesWithIndex

          // don't compare a sym against itself
          if i1 != i2
          if sym1 == sym2
        } yield ResolutionError.DuplicateDerivation(sym1, loc1, loc2).toFailure

        Validation.sequenceX(failures) map {
          _ =>
            // if the enum derives Eq, Order, and ToString
            // AND it does not already derive Boxable
            // then add Boxable to its derivations
            // otherwise just use the given list of derivations
            val classesImplyingBoxable = List(EqSym, OrderSym, ToStringSym)
            val deriveSyms = derives.map(_.clazz)
            if (classesImplyingBoxable.forall(deriveSyms.contains) && !deriveSyms.contains(BoxableSym)) {
              val loc = derives.map(_.loc).min
              Ast.Derivation(BoxableSym, loc) :: derives
            } else {
              derives
            }
        }
    }
  }

  /**
    * Performs name resolution on the given of derivation `derive0`.
    */
  def resolveDerivation(derive0: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[Ast.Derivation, ResolutionError] = {
    val clazzVal = lookupClass(derive0, env, ns0, root)
    flatMapN(clazzVal) {
      clazz =>
        mapN(checkDerivable(clazz.sym, derive0.loc)) {
          _ => Ast.Derivation(clazz.sym, derive0.loc)
        }
    }
  }

  /**
    * Checks that the given class `sym` is derivable.
    */
  def checkDerivable(sym: Symbol.ClassSym, loc: SourceLocation): Validation[Unit, ResolutionError] = {
    if (DerivableSyms.contains(sym)) {
      ().toSuccess
    } else {
      ResolutionError.IllegalDerivation(sym, DerivableSyms, loc).toFailure
    }
  }

  /**
    * Finds the class with the qualified name `qname` in the namespace `ns0`, for the purposes of implementation.
    */
  def lookupClassForImplementation(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Class, ResolutionError] = {
    val classOpt = tryLookupName(qname, allowCase = false, env, ns0, root)
    classOpt.collectFirst {
      case Resolution.Declaration(clazz: NamedAst.Declaration.Class) => clazz
    } match {
      case Some(clazz) =>
        getClassAccessibility(clazz, ns0) match {
          case ClassAccessibility.Accessible => clazz.toSuccess
          case ClassAccessibility.Sealed => ResolutionError.SealedClass(clazz.sym, ns0, qname.loc).toFailure
          case ClassAccessibility.Inaccessible => ResolutionError.InaccessibleClass(clazz.sym, ns0, qname.loc).toFailure
        }
      case None => ResolutionError.UndefinedClass(qname, ns0, qname.loc).toFailure
    }
  }

  /**
    * Finds the class with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupClass(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Class, ResolutionError] = {
    val classOpt = tryLookupName(qname, allowCase = false, env, ns0, root)
    classOpt.collectFirst {
      case Resolution.Declaration(clazz: NamedAst.Declaration.Class) => clazz
    } match {
      case Some(clazz) =>
        getClassAccessibility(clazz, ns0) match {
          case ClassAccessibility.Accessible | ClassAccessibility.Sealed => clazz.toSuccess
          case ClassAccessibility.Inaccessible => ResolutionError.InaccessibleClass(clazz.sym, ns0, qname.loc).toFailure
        }
      case None => ResolutionError.UndefinedClass(qname, ns0, qname.loc).toFailure
    }
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupTerm(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedTerm, ResolutionError] = {
    // first look in the local env
    val resolutions = tryLookupName(qname, allowCase = true, env, ns0, root)

    resolutions.collect {
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Def) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Sig) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Case) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.RestrictableCase) => decl
      case decl@Resolution.Var(_) => decl
    } match {
      case Resolution.Declaration(defn: NamedAst.Declaration.Def) :: _ =>
        if (isDefAccessible(defn, ns0)) {
          ResolvedTerm.Def(defn).toSuccess
        } else {
          ResolutionError.InaccessibleDef(defn.sym, ns0, qname.loc).toFailure
        }
      case Resolution.Declaration(sig: NamedAst.Declaration.Sig) :: _ =>
        if (isSigAccessible(sig, ns0)) {
          ResolvedTerm.Sig(sig).toSuccess
        } else {
          ResolutionError.InaccessibleSig(sig.sym, ns0, qname.loc).toFailure
        }
      case Resolution.Declaration(caze: NamedAst.Declaration.Case) :: Nil =>
        ResolvedTerm.Tag(caze).toSuccess
      // TODO NS-REFACTOR check accessibility
      case Resolution.Declaration(caze: NamedAst.Declaration.RestrictableCase) :: Nil =>
        ResolvedTerm.RestrictableTag(caze).toSuccess
      // TODO NS-REFACTOR check accessibility
      case Resolution.Declaration(caze1: NamedAst.Declaration.Case) :: Resolution.Declaration(caze2: NamedAst.Declaration.Case) :: _ =>
        // Multiple case matches. Error.
        ResolutionError.AmbiguousTag(qname.ident.name, ns0, List(caze1.sym.loc, caze2.sym.loc), qname.ident.loc).toFailure
      case Resolution.Var(sym) :: _ => ResolvedTerm.Var(sym).toSuccess
      case _ => ResolutionError.UndefinedName(qname, ns0, filterToVarEnv(env), qname.loc).toFailure
    }
  }

  /**
    * Looks up the effect operation with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupOp(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Op, ResolutionError] = {
    val opOpt = tryLookupName(qname, allowCase = false, env, ns0, root)

    opOpt match {
      case Resolution.Declaration(op: NamedAst.Declaration.Op) :: Nil =>
        if (isOpAccessible(op, ns0)) {
          op.toSuccess
        } else {
          ResolutionError.InaccessibleOp(op.sym, ns0, qname.loc).toFailure
        }
      case _ => ResolutionError.UndefinedOp(qname, qname.loc).toFailure
    }
  }

  /**
    * Looks up the effect operation as a member of the given effect.
    */
  private def findOpInEffect(ident: Name.Ident, eff: NamedAst.Declaration.Effect): Validation[NamedAst.Declaration.Op, ResolutionError] = {
    val opOpt = eff.ops.find(o => o.sym.name == ident.name)
    opOpt match {
      case None =>
        val nname = eff.sym.namespace :+ eff.sym.name
        val qname = Name.mkQName(nname, ident.name, SourcePosition.Unknown, SourcePosition.Unknown)
        ResolutionError.UndefinedOp(qname, ident.loc).toFailure
      case Some(op) => op.toSuccess
    }
  }

  /**
    * Finds the enum case that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  private def lookupTag(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Case, ResolutionError] = {
    // look up the name
    val matches = tryLookupName(qname, allowCase = true, env, ns0, root) collect {
      case Resolution.Declaration(c: NamedAst.Declaration.Case) => c
    }

    matches match {
      // Case 0: No matches. Error.
      case Nil => ResolutionError.UndefinedTag(qname.ident.name, ns0, qname.loc).toFailure
      // Case 1: Exactly one match. Success.
      case caze :: Nil => caze.toSuccess
      // Case 2: Multiple matches. Error
      case cazes =>
        val locs = cazes.map(_.sym.loc).sorted
        ResolutionError.AmbiguousTag(qname.ident.name, ns0, locs, qname.loc).toFailure
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Finds the restrictable enum case that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  private def lookupRestrictableTag(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.RestrictableCase, ResolutionError] = {
    // look up the name
    val matches = tryLookupName(qname, allowCase = false, env, ns0, root) collect { // TODO RESTR-VARS disallowing case for now
      case Resolution.Declaration(c: NamedAst.Declaration.RestrictableCase) => c
    }

    matches match {
      // Case 0: No matches. Error.
      case Nil => ResolutionError.UndefinedTag(qname.ident.name, ns0, qname.loc).toFailure
      // Case 1: Exactly one match. Success.
      case caze :: Nil => caze.toSuccess
      // Case 2: Multiple matches. Error
      case cazes =>
        val locs = cazes.map(_.sym.loc).sorted
        ResolutionError.AmbiguousTag(qname.ident.name, ns0, locs, qname.loc).toFailure
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Returns `true` iff the given type `tpe0` is the Unit type.
    */
  def isUnitType(tpe: NamedAst.Type): Boolean = tpe match {
    case NamedAst.Type.Unit(loc) => true
    case _ => false
  }

  /**
    * Partially resolves the given type `tpe0` in the given namespace `ns0`.
    *
    * Type aliases are given temporary placeholders.
    */
  private def semiResolveType(tpe0: NamedAst.Type, wildness: Wildness, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UnkindedType, ResolutionError] = {
    def visit(tpe0: NamedAst.Type): Validation[UnkindedType, ResolutionError] = tpe0 match {
      case NamedAst.Type.Var(ident, loc) =>
        val symVal = lookupTypeVar(ident, wildness, env)
        mapN(symVal) {
          case sym => UnkindedType.Var(sym, loc)
        }

      case NamedAst.Type.Unit(loc) => UnkindedType.Cst(TypeConstructor.Unit, loc).toSuccess

      case NamedAst.Type.Ambiguous(qname, loc) if qname.isUnqualified => qname.ident.name match {
        // Basic Types
        case "Unit" => UnkindedType.Cst(TypeConstructor.Unit, loc).toSuccess
        case "Null" => UnkindedType.Cst(TypeConstructor.Null, loc).toSuccess
        case "Bool" => UnkindedType.Cst(TypeConstructor.Bool, loc).toSuccess
        case "Char" => UnkindedType.Cst(TypeConstructor.Char, loc).toSuccess
        case "Float32" => UnkindedType.Cst(TypeConstructor.Float32, loc).toSuccess
        case "Float64" => UnkindedType.Cst(TypeConstructor.Float64, loc).toSuccess
        case "BigDecimal" => UnkindedType.Cst(TypeConstructor.BigDecimal, loc).toSuccess
        case "Int8" => UnkindedType.Cst(TypeConstructor.Int8, loc).toSuccess
        case "Int16" => UnkindedType.Cst(TypeConstructor.Int16, loc).toSuccess
        case "Int32" => UnkindedType.Cst(TypeConstructor.Int32, loc).toSuccess
        case "Int64" => UnkindedType.Cst(TypeConstructor.Int64, loc).toSuccess
        case "BigInt" => UnkindedType.Cst(TypeConstructor.BigInt, loc).toSuccess
        case "String" => UnkindedType.Cst(TypeConstructor.Str, loc).toSuccess
        case "Sender" => UnkindedType.Cst(TypeConstructor.Sender, loc).toSuccess
        case "Receiver" => UnkindedType.Cst(TypeConstructor.Receiver, loc).toSuccess
        case "Lazy" => UnkindedType.Cst(TypeConstructor.Lazy, loc).toSuccess
        case "Array" => UnkindedType.Cst(TypeConstructor.Array, loc).toSuccess
        case "Ref" => UnkindedType.Cst(TypeConstructor.Ref, loc).toSuccess
        case "Region" => UnkindedType.Cst(TypeConstructor.RegionToStar, loc).toSuccess

        // Disambiguate type.
        case typeName =>
          lookupType(qname, env, ns0, root) match {
            case TypeLookupResult.Enum(enum) => getEnumTypeIfAccessible(enum, ns0, loc)
            case TypeLookupResult.RestrictableEnum(enum) => getRestrictableEnumTypeIfAccessible(enum, ns0, loc)
            case TypeLookupResult.TypeAlias(typeAlias) => getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
            case TypeLookupResult.Effect(eff) => getEffectTypeIfAccessible(eff, ns0, root, loc)
            case TypeLookupResult.JavaClass(clazz) => flixifyType(clazz, loc).toSuccess
            case TypeLookupResult.NotFound => ResolutionError.UndefinedType(qname, ns0, loc).toFailure
          }
      }

      case NamedAst.Type.Ambiguous(qname, loc) =>
        // Disambiguate type.
        lookupType(qname, env, ns0, root) match {
          case TypeLookupResult.Enum(enum) => getEnumTypeIfAccessible(enum, ns0, loc)
          case TypeLookupResult.RestrictableEnum(enum) => getRestrictableEnumTypeIfAccessible(enum, ns0, loc)
          case TypeLookupResult.TypeAlias(typeAlias) => getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
          case TypeLookupResult.Effect(eff) => getEffectTypeIfAccessible(eff, ns0, root, loc)
          case TypeLookupResult.JavaClass(clazz) => flixifyType(clazz, loc).toSuccess
          case TypeLookupResult.NotFound => ResolutionError.UndefinedType(qname, ns0, loc).toFailure
        }

      case NamedAst.Type.Tuple(elms0, loc) =>
        val elmsVal = traverse(elms0)(tpe => visit(tpe))
        mapN(elmsVal) {
          elms => UnkindedType.mkTuple(elms, loc)
        }

      case NamedAst.Type.RecordRowEmpty(loc) => UnkindedType.Cst(TypeConstructor.RecordRowEmpty, loc).toSuccess

      case NamedAst.Type.RecordRowExtend(field, value, rest, loc) =>
        val vVal = visit(value)
        val rVal = visit(rest)
        mapN(vVal, rVal) {
          case (v, r) => UnkindedType.mkRecordRowExtend(field, v, r, loc)
        }

      case NamedAst.Type.Record(row, loc) =>
        val rVal = visit(row)
        mapN(rVal) {
          r => UnkindedType.mkRecord(r, loc)
        }

      case NamedAst.Type.SchemaRowEmpty(loc) => UnkindedType.Cst(TypeConstructor.SchemaRowEmpty, loc).toSuccess

      case NamedAst.Type.SchemaRowExtendWithAlias(qname, targs, rest, loc) =>
        // Lookup the type alias.
        flatMapN(lookupTypeAlias(qname, env, ns0, root)) {
          typeAlias =>
            val tVal = getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
            val tsVal = traverse(targs)(visit(_))
            val rVal = visit(rest)
            mapN(tVal, tsVal, rVal) {
              case (t, ts, r) =>
                val app = UnkindedType.mkApply(t, ts, loc)
                UnkindedType.mkSchemaRowExtend(Name.mkPred(qname.ident), app, r, loc)
            }
        }

      case NamedAst.Type.SchemaRowExtendWithTypes(ident, den, tpes, rest, loc) =>
        val tsVal = traverse(tpes)(visit(_))
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

      case NamedAst.Type.Relation(tpes, loc) =>
        val tsVal = traverse(tpes)(visit(_))
        mapN(tsVal) {
          ts => UnkindedType.mkRelation(ts, loc)
        }

      case NamedAst.Type.Lattice(tpes, loc) =>
        val tsVal = traverse(tpes)(visit(_))
        mapN(tsVal) {
          ts => UnkindedType.mkLattice(ts, loc)
        }

      case NamedAst.Type.Native(fqn, loc) =>
        mapN(lookupJvmClass(fqn, loc)) {
          case clazz => flixifyType(clazz, loc)
        }

      case NamedAst.Type.Arrow(tparams0, purAndEff0, tresult0, loc) =>
        val tparamsVal = traverse(tparams0)(visit(_))
        val tresultVal = visit(tresult0)
        val purAndEffVal = semiResolvePurityAndEffect(purAndEff0, wildness, env, ns0, root)
        mapN(tparamsVal, tresultVal, purAndEffVal) {
          case (tparams, tresult, purAndEff) => mkUncurriedArrowWithEffect(tparams, purAndEff, tresult, loc)
        }

      case NamedAst.Type.Apply(base0, targ0, loc) =>
        val tpe1Val = visit(base0)
        val tpe2Val = visit(targ0)
        mapN(tpe1Val, tpe2Val) {
          case (tpe1, tpe2) => UnkindedType.Apply(tpe1, tpe2, loc)
        }

      case NamedAst.Type.True(loc) => UnkindedType.Cst(TypeConstructor.True, loc).toSuccess

      case NamedAst.Type.False(loc) => UnkindedType.Cst(TypeConstructor.False, loc).toSuccess

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

      case NamedAst.Type.Read(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => UnkindedType.ReadWrite(t, loc)
        }

      case NamedAst.Type.Write(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => UnkindedType.ReadWrite(t, loc)
        }

      case NamedAst.Type.Empty(loc) => UnkindedType.Cst(TypeConstructor.Empty, loc).toSuccess

      case NamedAst.Type.Ascribe(tpe, kind, loc) =>
        mapN(visit(tpe)) {
          t => UnkindedType.Ascribe(t, kind, loc)
        }

    }

    visit(tpe0)
  }

  /**
    * Finishes resolving the partially resolved type `tpe0`.
    *
    * Replaces type alias placeholders with the real type aliases.
    */
  private def finishResolveType(tpe0: UnkindedType, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]): Validation[UnkindedType, ResolutionError] = {

    /**
      * Performs beta-reduction on the given type alias.
      * The list of arguments must be the same length as the alias's parameters.
      */
    def applyAlias(alias: ResolvedAst.Declaration.TypeAlias, args: List[UnkindedType], cstLoc: SourceLocation): UnkindedType = {
      val map = alias.tparams.tparams.map(_.sym).zip(args).toMap[Symbol.UnkindedTypeVarSym, UnkindedType]
      val tpe = alias.tpe.map(map)
      val cst = Ast.AliasConstructor(alias.sym, cstLoc)
      UnkindedType.Alias(cst, args, tpe, tpe0.loc)
    }

    val baseType = tpe0.baseType
    val targs = tpe0.typeArguments

    baseType match {
      case UnkindedType.UnappliedAlias(sym, loc) =>
        val alias = taenv(sym)
        val tparams = alias.tparams.tparams
        val numParams = tparams.length
        if (targs.length < numParams) {
          // Case 1: The type alias is under-applied.
          ResolutionError.UnderAppliedTypeAlias(sym, loc).toFailure
        } else {
          // Case 2: The type alias is fully applied.
          // Apply the types within the alias, then apply any leftover types.
          traverse(targs)(finishResolveType(_, taenv)) map {
            resolvedArgs =>
              val (usedArgs, extraArgs) = resolvedArgs.splitAt(numParams)
              UnkindedType.mkApply(applyAlias(alias, usedArgs, loc), extraArgs, tpe0.loc)
          }
        }

      case _: UnkindedType.Var =>
        traverse(targs)(finishResolveType(_, taenv)) map {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Cst =>
        traverse(targs)(finishResolveType(_, taenv)) map {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Enum =>
        traverse(targs)(finishResolveType(_, taenv)) map {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.RestrictableEnum =>
        traverse(targs)(finishResolveType(_, taenv)) map {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case UnkindedType.Arrow(purAndEff, arity, loc) =>
        val purAndEffVal = finishResolvePurityAndEffect(purAndEff, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(purAndEffVal, targsVal) {
          case (p, ts) => UnkindedType.mkApply(UnkindedType.Arrow(p, arity, loc), ts, tpe0.loc)
        }

      case UnkindedType.ReadWrite(tpe, loc) =>
        val tpeVal = finishResolveType(tpe, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpeVal, targsVal) {
          case (t, ts) => UnkindedType.mkApply(UnkindedType.ReadWrite(t, loc), ts, tpe0.loc)
        }

      case UnkindedType.Ascribe(tpe, kind, loc) =>
        val tpeVal = finishResolveType(tpe, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpeVal, targsVal) {
          case (t, ts) => UnkindedType.mkApply(UnkindedType.Ascribe(t, kind, loc), ts, tpe0.loc)
        }

      case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", baseType.loc)
      case _: UnkindedType.Alias => throw InternalCompilerException("unexpected resolved alias", baseType.loc)
    }
  }

  /**
    * Performs name resolution on the given type `tpe0` in the given namespace `ns0`.
    */
  def resolveType(tpe0: NamedAst.Type, wildness: Wildness, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UnkindedType, ResolutionError] = {
    val tVal = semiResolveType(tpe0, wildness, env, ns0, root)
    flatMapN(tVal) {
      t => finishResolveType(t, taenv)
    }
  }

  /**
    * Partially resolves the given purity and effect.
    */
  private def semiResolvePurityAndEffect(purAndEff0: NamedAst.PurityAndEffect, wildness: Wildness, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UnkindedType.PurityAndEffect, ResolutionError] = purAndEff0 match {
    case NamedAst.PurityAndEffect(pur0, eff0) =>
      val purVal = traverseOpt(pur0)(semiResolveType(_, wildness, env, ns0, root))
      val effVal = traverseOpt(eff0)(effs => traverse(effs)(semiResolveType(_, wildness, env, ns0, root)))
      mapN(purVal, effVal) {
        case (pur, eff) => UnkindedType.PurityAndEffect(pur, eff)
      }
  }

  /**
    * Finishes resolution of the given purity and effect.
    */
  private def finishResolvePurityAndEffect(purAndEff0: UnkindedType.PurityAndEffect, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]): Validation[UnkindedType.PurityAndEffect, ResolutionError] = purAndEff0 match {
    case UnkindedType.PurityAndEffect(pur0, eff0) =>
      val purVal = traverseOpt(pur0)(finishResolveType(_, taenv))
      val effVal = traverseOpt(eff0)(effs => traverse(effs)(finishResolveType(_, taenv)))
      mapN(purVal, effVal) {
        case (pur, eff) => UnkindedType.PurityAndEffect(pur, eff)
      }
  }

  /**
    * Performs name resolution on the given purity and effect `purAndEff0` in the given namespace `ns0`.
    */
  private def resolvePurityAndEffect(purAndEff0: NamedAst.PurityAndEffect, wildness: Wildness, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UnkindedType.PurityAndEffect, ResolutionError] = {
    flatMapN(semiResolvePurityAndEffect(purAndEff0, wildness, env, ns0, root)) {
      case purAndEff => finishResolvePurityAndEffect(purAndEff, taenv)
    }
  }

  /**
    * The result of looking up an ambiguous type.
    */
  private sealed trait TypeLookupResult {
    /**
      * Returns `other` if this result is [[TypeLookupResult.NotFound]].
      *
      * Otherwise, returns this result.
      */
    def orElse(other: => TypeLookupResult): TypeLookupResult = this match {
      case res: TypeLookupResult.Enum => res
      case res: TypeLookupResult.RestrictableEnum => res
      case res: TypeLookupResult.TypeAlias => res
      case res: TypeLookupResult.Effect => res
      case res: TypeLookupResult.JavaClass => res
      case TypeLookupResult.NotFound => other
    }
  }

  private object TypeLookupResult {
    /**
      * The result is an enum.
      */
    case class Enum(enum0: NamedAst.Declaration.Enum) extends TypeLookupResult

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
    case class JavaClass(clazz: Class[_]) extends TypeLookupResult

    /**
      * The type cannot be found.
      */
    case object NotFound extends TypeLookupResult
  }

  /**
    * Looks up the ambiguous type.
    */
  private def lookupType(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): TypeLookupResult = {

    /**
      * Looks up the type in the given namespace.
      */
    def lookupInNamespace(ns: Name.NName): TypeLookupResult = {
      val symbolsInNamespace = root.symbols.getOrElse(ns, Map.empty)
      symbolsInNamespace.getOrElse(qname.ident.name, Nil).collectFirst {
        case alias: NamedAst.Declaration.TypeAlias =>
          // Case 2: found a type alias
          TypeLookupResult.TypeAlias(alias)
        case enum: NamedAst.Declaration.Enum =>
          // Case 3: found an enum
          TypeLookupResult.Enum(enum)
        case enum: NamedAst.Declaration.RestrictableEnum =>
          // Case 4: found a restrictable enum
          TypeLookupResult.RestrictableEnum(enum)
        case effect: NamedAst.Declaration.Effect =>
          // Case 5: found an effect
          TypeLookupResult.Effect(effect)
      }.getOrElse(TypeLookupResult.NotFound)
    }

    def lookupInUseEnv(name: String): TypeLookupResult = env(name).collectFirst {
      case Resolution.Declaration(alias: NamedAst.Declaration.TypeAlias) =>
        // Case 1: found a type alias
        TypeLookupResult.TypeAlias(alias)
      case Resolution.Declaration(enum: NamedAst.Declaration.Enum) =>
        // Case 2: found an enum
        TypeLookupResult.Enum(enum)
      case Resolution.Declaration(enum: NamedAst.Declaration.RestrictableEnum) =>
        // Case 3: found a restrictable enum
        TypeLookupResult.RestrictableEnum(enum)
      case Resolution.Declaration(effect: NamedAst.Declaration.Effect) =>
        // Case 4: found an effect
        TypeLookupResult.Effect(effect)
      case Resolution.JavaClass(clazz) =>
        // Case 5: found a Java class
        TypeLookupResult.JavaClass(clazz)
    }.getOrElse(TypeLookupResult.NotFound)

    if (qname.isUnqualified) {
      // Case 1. The name is unqualified. Look it up in the use environment.
      lookupInUseEnv(qname.ident.name).orElse {
        // Case 1: The name is unqualified. Lookup in the current namespace.
        lookupInNamespace(ns0).orElse {
          // Case 1.1: The name was not found in the current namespace. Try the root namespace.
          lookupInNamespace(Name.RootNS)
        }
      }
    } else {
      // Case 2: The name is qualified. Look it up in its namespace.
      lookupInNamespace(qname.namespace)
    }
  }

  /**
    * Optionally returns the type alias with the given `name` in the given namespace `ns0`.
    */
  private def lookupTypeAlias(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.TypeAlias, ResolutionError] = {
    val symOpt = tryLookupName(qname, allowCase = false, env, ns0, root)

    symOpt match {
      case Resolution.Declaration(alias: NamedAst.Declaration.TypeAlias) :: Nil => getTypeAliasIfAccessible(alias, ns0, qname.loc)
      case _ => ResolutionError.UndefinedName(qname, ns0, Map.empty, qname.loc).toFailure
    }
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupEffect(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Effect, ResolutionError] = {
    val symOpt = tryLookupName(qname, allowCase = false, env, ns0, root)

    symOpt match {
      case Resolution.Declaration(eff: NamedAst.Declaration.Effect) :: Nil => getEffectIfAccessible(eff, ns0, qname.loc)
      case _ => ResolutionError.UndefinedEffect(qname, ns0, qname.loc).toFailure
    }
  }

  /**
    * Looks up the type variable with the given name.
    */
  private def lookupTypeVar(ident: Name.Ident, wildness: Wildness, env: ListMap[String, Resolution])(implicit flix: Flix): Validation[Symbol.UnkindedTypeVarSym, ResolutionError] = {
    if (ident.isWild) {
      wildness match {
        case Wildness.AllowWild =>
          Symbol.freshUnkindedTypeVarSym(VarText.SourceText(ident.name), isRegion = false, ident.loc).toSuccess
        case Wildness.ForbidWild =>
          ResolutionError.IllegalWildType(ident, ident.loc).toFailure
      }
    } else {
      env(ident.name).collectFirst {
        case Resolution.TypeVar(sym) => sym.toSuccess
      }.getOrElse(ResolutionError.UndefinedTypeVar(ident.name, ident.loc).toFailure)
    }
  }

  /**
    * Looks up the variable with the given name.
    */
  private def lookupVar(ident: Name.Ident, env: ListMap[String, Resolution]): Validation[Symbol.VarSym, ResolutionError] = {
    env(ident.name).collectFirst {
      case Resolution.Var(sym) => sym.toSuccess
      // TODO NS-REFACTOR add tests
    }.getOrElse(ResolutionError.UndefinedVar(ident.name, ident.loc).toFailure)
  }


  /**
    * Returns the list of symbols this name points to, ordered from most closely declared to furthest.
    */
  private def tryLookupName(qname: Name.QName, allowCase: Boolean, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): List[Resolution] = {
    if (qname.isUnqualified) {
      // Case 1: Unqualified name. Try in the use environment.
      env(qname.ident.name) match {
        // Case 1.1: It's not in the env.
        case Nil =>
          // First check the local namespace
          // Collect both normal symbols in this env, and nested cases
          val localDecls = root.symbols.getOrElse(ns0, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration)
          val localCases = if (allowCase) { // TODO NS-REFACTOR don't need this after changing case env into auto-uses
            root.cases.getOrElse(ns0, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration)
          } else {
            Nil
          }

          localDecls ::: localCases match {
            // Case 1.1.1: Nothing local. Check the root env
            case Nil =>
              val rootDecls = root.symbols.getOrElse(Name.RootNS, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration)
              val rootCases = if (allowCase) { // TODO NS-REFACTOR don't need this after changing case env into auto-uses
                root.cases.getOrElse(Name.RootNS, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration)
              } else {
                Nil
              }
              rootDecls ::: rootCases
            // Case 1.1.2: There are locals. Use them.
            case locals => locals
          }

        // Case 1.2 It is in the env, return the results
        case l => l
      }
    } else {
      // Case 2. Qualified name. Look it up directly.
      tryLookupQualifiedName(qname, env, ns0, root).getOrElse(Nil).map(Resolution.Declaration)
    }
  }

  /**
    * Looks up the qualified name in the given root.
    */
  private def tryLookupQualifiedName(qname0: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Option[List[NamedAst.Declaration]] = {
    // First resolve the root of the qualified name
    val head = qname0.namespace.parts.head
    tryLookupModule(head, env, ns0, root) match {
      case None => None
      case Some(prefix) =>
        val ns = prefix ::: qname0.namespace.parts.tail
        val qname = Name.mkQName(ns, qname0.ident.name, SourcePosition.Unknown, SourcePosition.Unknown)
        root.symbols.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Looks up the given module in the root.
    */
  private def tryLookupModule(name: String, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Option[List[String]] = {
    // First see if there's a module with this name imported into our environment
    env(name).collectFirst {
      case Resolution.Declaration(ns: NamedAst.Declaration.Namespace) => ns.sym.ns
      case Resolution.Declaration(clazz: NamedAst.Declaration.Class) => clazz.sym.namespace :+ clazz.sym.name
      case Resolution.Declaration(enum: NamedAst.Declaration.Enum) => enum.sym.namespace :+ enum.sym.name
      case Resolution.Declaration(enum: NamedAst.Declaration.RestrictableEnum) => enum.sym.namespace :+ enum.sym.name
      case Resolution.Declaration(eff: NamedAst.Declaration.Effect) => eff.sym.namespace :+ eff.sym.name
    }.orElse {
      // Then see if there's a module with this name declared in our namespace
      root.symbols.getOrElse(ns0, Map.empty).getOrElse(name, Nil).collectFirst {
        case Declaration.Namespace(sym, usesAndImports, decls, loc) => sym.ns
        case Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc) => sym.namespace :+ sym.name
        case Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.namespace :+ sym.name
      }
    }.orElse {
      // Then see if there's a module with this name declared in the root namespace
      root.symbols.getOrElse(Name.RootNS, Map.empty).getOrElse(name, Nil).collectFirst {
        case Declaration.Namespace(sym, usesAndImports, decls, loc) => sym.ns
        case Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc) => sym.namespace :+ sym.name
        case Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.namespace :+ sym.name
      }
    }
  }

  /**
    * Looks up the qualified name in the given root.
    */
  private def lookupQualifiedName(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[List[NamedAst.Declaration], ResolutionError] = {
    tryLookupQualifiedName(qname, env, ns0, root) match {
      case None => ResolutionError.UndefinedName(qname, ns0, Map.empty, qname.loc).toFailure
      case Some(decl) => decl.toSuccess
    }
  }

  /**
    * Determines if the class is accessible from the namespace.
    *
    * Accessibility depends on the modifiers on the class
    * and the accessing namespace's relation to the class namespace:
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
  private def getClassAccessibility(class0: NamedAst.Declaration.Class, ns0: Name.NName): ClassAccessibility = {

    val classNs = class0.sym.namespace
    val accessingNs = ns0.idents.map(_.name)

    if (classNs == accessingNs) {
      // Case 1: We're in the same namespace: Accessible
      ClassAccessibility.Accessible
    } else if (!class0.mod.isPublic && !accessingNs.startsWith(classNs)) {
      // Case 2: The class is private and we're in unrelated namespaces: Inaccessible
      ClassAccessibility.Inaccessible
    } else if (class0.mod.isSealed) {
      // Case 3: The class is accessible but sealed
      ClassAccessibility.Sealed
    } else {
      // Case 4: The class is otherwise accessible
      ClassAccessibility.Accessible
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
    val prefixNs = sig0.sym.clazz.namespace :+ sig0.sym.clazz.name
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }


  /**
    * Determines if the operation is accessible from the namespace.
    *
    * An operation `op0` is accessible from a namespace `ns0` if:
    *
    * (a) the operation is marked public, or
    * (b) the operation is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def isOpAccessible(op0: NamedAst.Declaration.Op, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (op0.spec.mod.isPublic)
      return true

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = op0.sym.eff.namespace :+ op0.sym.eff.name
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }

  /**
    * Determines if the enum is accessible from the namespace.
    *
    * Accessibility depends on the modifiers on the enum
    * and the accessing namespace's relation to the enum namespace:
    *
    * |            | same | child | other |
    * |------------|------|-------|-------|
    * | (none)     | A    | A     | I     |
    * | opaque     | A    | A     | I     |
    * | pub        | A    | A     | A     |
    * | pub opaque | A    | A     | O     |
    *
    * (A: Accessible, O: Opaque, I: Inaccessible)
    */
  private def getEnumAccessibility(enum0: NamedAst.Declaration.Enum, ns0: Name.NName): EnumAccessibility = {

    val enumNs = enum0.sym.namespace
    val accessingNs = ns0.idents.map(_.name)

    val fromChild = accessingNs.startsWith(enumNs)
    (enum0.mod.isPublic, enum0.mod.isOpaque, fromChild) match {
      // Case 1: Access from child namespace. Accessible.
      case (_, _, true) => EnumAccessibility.Accessible

      // Case 2: Private. Inaccessible.
      case (false, _, false) => EnumAccessibility.Inaccessible

      // Case 3: Public but opaque. Opaque.
      case (true, true, false) => EnumAccessibility.Opaque

      // Case 4: Public and non-opaque. Accessible.
      case (true, false, false) => EnumAccessibility.Accessible
    }
  }

  /**
    * Successfully returns the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getEnumIfAccessible(enum0: NamedAst.Declaration.Enum, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.Enum, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (enum0.mod.isPublic)
      return enum0.toSuccess

    //
    // Check if the enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return enum0.toSuccess

    //
    // The enum is not accessible.
    //
    ResolutionError.InaccessibleEnum(enum0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getRestrictableEnumIfAccessible(enum0: NamedAst.Declaration.RestrictableEnum, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.RestrictableEnum, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (enum0.mod.isPublic)
      return enum0.toSuccess


    //
    // Check if the enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return enum0.toSuccess

    //
    // The enum is not accessible.
    //
    ResolutionError.InaccessibleRestrictableEnum(enum0.sym, ns0, loc).toFailure
  }


  /**
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getEnumTypeIfAccessible(enum0: NamedAst.Declaration.Enum, ns0: Name.NName, loc: SourceLocation): Validation[UnkindedType, ResolutionError] =
    getEnumIfAccessible(enum0, ns0, loc) map {
      case enum => mkEnum(enum.sym, loc)
    }

  /**
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getRestrictableEnumTypeIfAccessible(enum0: NamedAst.Declaration.RestrictableEnum, ns0: Name.NName, loc: SourceLocation): Validation[UnkindedType, ResolutionError] =
    getRestrictableEnumIfAccessible(enum0, ns0, loc) map {
      case enum => mkRestrictableEnum(enum.sym, loc)
    }

  /**
    * Successfully returns the given type alias `alia0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getTypeAliasIfAccessible(alia0: NamedAst.Declaration.TypeAlias, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.TypeAlias, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (alia0.mod.isPublic)
      return alia0.toSuccess

    //
    // Check if the type alias is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = alia0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return alia0.toSuccess

    //
    // The type alias is not accessible.
    //
    ResolutionError.InaccessibleTypeAlias(alia0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the type of the given type alias `alia0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getTypeAliasTypeIfAccessible(alia0: NamedAst.Declaration.TypeAlias, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[UnkindedType, ResolutionError] = {
    getTypeAliasIfAccessible(alia0, ns0, loc) map {
      alias => mkUnappliedTypeAlias(alias.sym, loc)
    }
  }

  /**
    * Successfully returns the given `eff0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getEffectIfAccessible(eff0: NamedAst.Declaration.Effect, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.Effect, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (eff0.mod.isPublic)
      return eff0.toSuccess

    //
    // Check if the type alias is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = eff0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return eff0.toSuccess

    //
    // The type alias is not accessible.
    //
    ResolutionError.InaccessibleEffect(eff0.sym, ns0, loc).toFailure
  }

  /**
    * Successfully returns the type of the given effect `eff0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getEffectTypeIfAccessible(eff0: NamedAst.Declaration.Effect, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[UnkindedType, ResolutionError] = {
    getEffectIfAccessible(eff0, ns0, loc) map {
      alias => mkEffect(alias.sym, loc)
    }
  }

  /**
    * Returns the class reflection object for the given `className`.
    */
  private def lookupJvmClass(className: String, loc: SourceLocation)(implicit flix: Flix): Validation[Class[_], ResolutionError] = try {
    // Don't initialize the class; we don't want to execute static initializers.
    val initialize = false
    Class.forName(className, initialize, flix.jarLoader).toSuccess
  } catch {
    case ex: ClassNotFoundException => ResolutionError.UndefinedJvmClass(className, loc).toFailure
    case ex: NoClassDefFoundError => ResolutionError.MissingJvmDependency(className, ex.getMessage, loc).toFailure
  }

  /**
    * Returns the constructor reflection object for the given `className` and `signature`.
    */
  private def lookupJvmConstructor(className: String, signature: List[UnkindedType], loc: SourceLocation)(implicit flix: Flix): Validation[Constructor[_], ResolutionError] = {
    // Lookup the class and signature.
    flatMapN(lookupJvmClass(className, loc), lookupSignature(signature, loc)) {
      case (clazz, sig) => try {
        // Lookup the constructor with the appropriate signature.
        clazz.getConstructor(sig: _*).toSuccess
      } catch {
        case ex: ClassNotFoundException => ResolutionError.UndefinedJvmClass(className, loc).toFailure
        case ex: NoSuchMethodException => ResolutionError.UndefinedJvmConstructor(className, sig, clazz.getConstructors.toList, loc).toFailure
        case ex: NoClassDefFoundError => ResolutionError.MissingJvmDependency(className, ex.getMessage, loc).toFailure
      }
    }
  }

  /**
    * Returns the method reflection object for the given `clazz`, `methodName`, and `signature`.
    */
  private def lookupJvmMethod(clazz: Class[_], methodName: String, signature: List[UnkindedType], retTpe: UnkindedType, static: Boolean, loc: SourceLocation)(implicit flix: Flix): Validation[Method, ResolutionError] = {
    // Lookup the signature.
    flatMapN(lookupSignature(signature, loc)) {
      sig =>
        try {
          // Lookup the method with the appropriate signature.
          val method = clazz.getMethod(methodName, sig: _*)

          // Check if the method should be and is static.
          if (static != Modifier.isStatic(method.getModifiers)) {
            throw new NoSuchMethodException()
          } else {
            // Check that the return type of the method matches the declared type.
            // We currently don't know how to handle all possible return types,
            // so only check the straightforward cases for now and succeed all others.
            // TODO move to typer
            val erasedRetTpe = UnkindedType.eraseAliases(retTpe)
            erasedRetTpe.baseType match {
              case UnkindedType.Cst(TypeConstructor.Unit, _) | UnkindedType.Cst(TypeConstructor.Bool, _) |
                   UnkindedType.Cst(TypeConstructor.Char, _) | UnkindedType.Cst(TypeConstructor.Float32, _) |
                   UnkindedType.Cst(TypeConstructor.Float64, _) | UnkindedType.Cst(TypeConstructor.BigDecimal, _) |
                   UnkindedType.Cst(TypeConstructor.Int8, _) | UnkindedType.Cst(TypeConstructor.Int16, _) |
                   UnkindedType.Cst(TypeConstructor.Int32, _) | UnkindedType.Cst(TypeConstructor.Int64, _) |
                   UnkindedType.Cst(TypeConstructor.BigInt, _) | UnkindedType.Cst(TypeConstructor.Str, _) |
                   UnkindedType.Cst(TypeConstructor.Native(_), _) =>

                val expectedTpe = UnkindedType.getFlixType(method.getReturnType)
                if (expectedTpe != erasedRetTpe)
                  ResolutionError.MismatchingReturnType(clazz.getName, methodName, retTpe, expectedTpe, loc).toFailure
                else
                  method.toSuccess

              case _ => method.toSuccess
            }
          }
        } catch {
          case ex: NoSuchMethodException =>
            val candidateMethods = clazz.getMethods.filter(m => m.getName == methodName).toList
            ResolutionError.UndefinedJvmMethod(clazz.getName, methodName, static, sig, candidateMethods, loc).toFailure
          case ex: NoClassDefFoundError => ResolutionError.MissingJvmDependency(clazz.getName, ex.getMessage, loc).toFailure
        }
    }
  }

  /**
    * Returns the field reflection object for the given `clazz` and `fieldName`.
    */
  private def lookupJvmField(clazz: Class[_], fieldName: String, static: Boolean, loc: SourceLocation)(implicit flix: Flix): Validation[Field, ResolutionError] = {
    try {
      // Lookup the field.
      val field = clazz.getField(fieldName)

      // Check if the field should be and is static.
      if (static == Modifier.isStatic(field.getModifiers))
        field.toSuccess
      else
        throw new NoSuchFieldException()
    } catch {
      case ex: NoSuchFieldException =>
        val candidateFields = clazz.getFields.toList
        ResolutionError.UndefinedJvmField(clazz.getName, fieldName, static, candidateFields, loc).toFailure
      case ex: NoClassDefFoundError => ResolutionError.MissingJvmDependency(clazz.getName, ex.getMessage, loc).toFailure
    }
  }

  /**
    * Performs name resolution on the given `signature`.
    */
  private def lookupSignature(signature: List[UnkindedType], loc: SourceLocation)(implicit flix: Flix): Validation[List[Class[_]], ResolutionError] = {
    traverse(signature)(getJVMType(_, loc))
  }

  /**
    * Returns the JVM type corresponding to the given Flix type `tpe`.
    *
    * A non-primitive Flix type is mapped to java.lang.Object.
    *
    * An array type is mapped to the corresponding array type.
    */
  private def getJVMType(tpe: UnkindedType, loc: SourceLocation)(implicit flix: Flix): Validation[Class[_], ResolutionError] = {
    val erased = UnkindedType.eraseAliases(tpe)
    val baseType = erased.baseType
    baseType match {
      // Case 1: Constant: Match on the type.
      case UnkindedType.Cst(tc, _) => tc match {
        case TypeConstructor.Unit => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.Bool => classOf[Boolean].toSuccess

        case TypeConstructor.Char => classOf[Char].toSuccess

        case TypeConstructor.Float32 => classOf[Float].toSuccess

        case TypeConstructor.Float64 => classOf[Double].toSuccess

        case TypeConstructor.BigDecimal => Class.forName("java.math.BigDecimal").toSuccess

        case TypeConstructor.Int8 => classOf[Byte].toSuccess

        case TypeConstructor.Int16 => classOf[Short].toSuccess

        case TypeConstructor.Int32 => classOf[Int].toSuccess

        case TypeConstructor.Int64 => classOf[Long].toSuccess

        case TypeConstructor.BigInt => Class.forName("java.math.BigInteger").toSuccess

        case TypeConstructor.Str => Class.forName("java.lang.String").toSuccess

        case TypeConstructor.Sender => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.Receiver => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.Ref => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.Tuple(_) => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.Array =>
          erased.typeArguments match {
            case elmTyp :: region :: Nil =>
              mapN(getJVMType(elmTyp, loc)) {
                case elmClass =>
                  // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
                  java.lang.reflect.Array.newInstance(elmClass, 0).getClass
              }
            case _ =>
              ResolutionError.IllegalType(tpe, loc).toFailure
          }

        case TypeConstructor.Native(clazz) => clazz.toSuccess

        case TypeConstructor.Record => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.Schema => Class.forName("java.lang.Object").toSuccess

        case TypeConstructor.All => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.And => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Complement => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Effect(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Empty => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.False => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Intersection => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Lattice => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Lazy => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Not => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Null => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Or => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.RecordRowEmpty => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.RecordRowExtend(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.RegionToStar => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Relation => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.SchemaRowEmpty => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.SchemaRowExtend(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.True => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.Union => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.CaseComplement(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.CaseConstant(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.CaseEmpty(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.CaseAll(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.CaseIntersection(_) => ResolutionError.IllegalType(tpe, loc).toFailure
        case TypeConstructor.CaseUnion(_) => ResolutionError.IllegalType(tpe, loc).toFailure

        case t: TypeConstructor.Arrow => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)
        case t: TypeConstructor.Enum => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)
        case t: TypeConstructor.RestrictableEnum => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)

      }

      // Case 2: Arrow. Convert to Java function interface
      case UnkindedType.Arrow(_, _, _) =>
        val targsVal = traverse(erased.typeArguments)(targ => getJVMType(targ, targ.loc))
        val returnsUnit = erased.typeArguments.lastOption match {
          case Some(ty) => isBaseTypeUnit(ty)
          case None => false
        }
        flatMapN(targsVal) {
          case Object :: Object :: Nil =>
            if (returnsUnit) Class.forName("java.util.function.Consumer").toSuccess else Class.forName("java.util.function.Function").toSuccess
          case Object :: Boolean :: Nil => Class.forName("java.util.function.Predicate").toSuccess
          case Int :: Object :: Nil =>
            if (returnsUnit) Class.forName("java.util.function.IntConsumer").toSuccess else Class.forName("java.util.function.IntFunction").toSuccess
          case Int :: Boolean :: Nil => Class.forName("java.util.function.IntPredicate").toSuccess
          case Int :: Int :: Nil => Class.forName("java.util.function.IntUnaryOperator").toSuccess
          case Long :: Object :: Nil =>
            if (returnsUnit) Class.forName("java.util.function.LongConsumer").toSuccess else Class.forName("java.util.function.LongFunction").toSuccess
          case Long :: Boolean :: Nil => Class.forName("java.util.function.LongPredicate").toSuccess
          case Long :: Long :: Nil => Class.forName("java.util.function.LongUnaryOperator").toSuccess
          case Double :: Object :: Nil =>
            if (returnsUnit) Class.forName("java.util.function.DoubleConsumer").toSuccess else Class.forName("java.util.function.DoubleFunction").toSuccess
          case Double :: Boolean :: Nil => Class.forName("java.util.function.DoublePredicate").toSuccess
          case Double :: Double :: Nil => Class.forName("java.util.function.DoubleUnaryOperator").toSuccess
          case _ => ResolutionError.IllegalType(tpe, loc).toFailure
        }

      // Case 3: Enum. Return an object type.
      case _: UnkindedType.Enum => Class.forName("java.lang.Object").toSuccess
      case _: UnkindedType.RestrictableEnum => Class.forName("java.lang.Object").toSuccess

      // Case 4: Ascription. Ignore it and recurse.
      case UnkindedType.Ascribe(t, _, _) => getJVMType(UnkindedType.mkApply(t, erased.typeArguments, loc), loc)

      // Case 5: Illegal type. Error.
      case _: UnkindedType.Var => ResolutionError.IllegalType(tpe, loc).toFailure
      case _: UnkindedType.ReadWrite => ResolutionError.IllegalType(tpe, loc).toFailure

      // Case 6: Unexpected type. Crash.
      case t: UnkindedType.Apply => throw InternalCompilerException(s"unexpected type: $t", loc)
      case t: UnkindedType.UnappliedAlias => throw InternalCompilerException(s"unexpected type: $t", loc)
      case t: UnkindedType.Alias => throw InternalCompilerException(s"unexpected type: $t", loc)
    }
  }

  private def isBaseTypeUnit(tpe: UnkindedType): Boolean = {
    val erased = UnkindedType.eraseAliases(tpe)
    val baseType = erased.baseType
    baseType match {
      // Case 1: Constant: Match on the type.
      case UnkindedType.Cst(tc, _) => tc match {
        case TypeConstructor.Unit => true
        case _ => false
      }
      case _ => false
    }
  }

  /**
    * Construct the type alias type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkUnappliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation): UnkindedType = UnkindedType.UnappliedAlias(sym, loc)

  /**
    * Returns either the explicit region (if present), the current region (if present), or the global region.
    */
  private def getExplicitOrImplicitRegion(explicitRegion: Option[ResolvedAst.Expression], currentRegion: Option[Symbol.VarSym], loc: SourceLocation): ResolvedAst.Expression = explicitRegion match {
    case Some(result) =>
      // Case 1: The region is explicitly given.
      result
    case None =>
      // Case 2: The region is absent. Either use the current region or the global region.
      currentRegion match {
        case Some(sym) =>
          // Case 2.1: Use the current region.
          ResolvedAst.Expression.Var(sym, sym.loc)
        case None =>
          // Case 2.2: Use the global region.
          val tpe = Type.mkRegion(Type.False, loc)
          ResolvedAst.Expression.Region(tpe, loc)
      }
  }

  /**
    * Gets the proper symbol from the given named symbol.
    */
  private def getSym(symbol: NamedAst.Declaration): Symbol = symbol match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports, decls, loc) => sym
    case NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc) => sym
    case NamedAst.Declaration.Sig(sym, spec, exp) => sym
    case NamedAst.Declaration.Def(sym, spec, exp) => sym
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym
    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc) => sym
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym
    case NamedAst.Declaration.Op(sym, spec) => sym
    case NamedAst.Declaration.Case(sym, tpe) => sym
    case NamedAst.Declaration.RestrictableCase(sym, tpe) => sym
    case NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, defs, ns, loc) => throw InternalCompilerException("unexpected instance", loc)
  }

  /**
    * Resolves the symbol where the symbol is known to point to a valid declaration.
    */
  private def infallableLookupSym(sym: Symbol, root: NamedAst.Root)(implicit flix: Flix): List[NamedAst.Declaration] = sym match {
    case sym: Symbol.DefnSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.EnumSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.RestrictableEnumSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.CaseSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.RestrictableCaseSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.ClassSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.SigSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.TypeAliasSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.EffectSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.OpSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.ModuleSym => root.symbols(Name.mkUnlocatedNName(sym.ns.init))(sym.ns.last)
    case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.KindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.UnkindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.LabelSym => throw InternalCompilerException(s"unexpected symbol $sym", SourceLocation.Unknown)
    case sym: Symbol.HoleSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
  }

  /**
    * Resolves the given Use.
    */
  private def visitUseOrImport(useOrImport: NamedAst.UseOrImport, ns: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[Ast.UseOrImport, ResolutionError] = useOrImport match {
    case NamedAst.UseOrImport.Use(qname, alias, loc) => tryLookupName(qname, allowCase = false, ListMap.empty, ns, root) match {
      // Case 1: No matches. Error.
      case Nil => ResolutionError.UndefinedName(qname, ns, Map.empty, loc).toFailure
      // Case 2: A match. Map it to a use.
      // TODO NS-REFACTOR: should map to multiple uses or ignore namespaces or something
      case Resolution.Declaration(d) :: _ => Ast.UseOrImport.Use(getSym(d), alias, loc).toSuccess
      // Case 3: Impossible. Hard error.
      case _ => throw InternalCompilerException("unexpected conflicted imports", loc)
    }

    case NamedAst.UseOrImport.Import(name, alias, loc) =>
      val clazzVal = lookupJvmClass(name.toString, loc)
      mapN(clazzVal) {
        case clazz => Ast.UseOrImport.Import(clazz, alias, loc)
      }
  }

  /**
    * Creates a use environment from the given uses and imports.
    */
  private def mkUseEnv(usesAndImports: List[Ast.UseOrImport], root: NamedAst.Root)(implicit flix: Flix): ListMap[String, Resolution] = {
    appendAllUseEnv(ListMap.empty, usesAndImports, root)
  }

  /**
    * Adds the given use or import to the use environment.
    */
  private def appendUseEnv(env: ListMap[String, Resolution], useOrImport: Ast.UseOrImport, root: NamedAst.Root)(implicit flix: Flix): ListMap[String, Resolution] = useOrImport match {
    case Ast.UseOrImport.Use(sym, alias, loc) =>
      val decls = infallableLookupSym(sym, root)
      decls.foldLeft(env) {
        case (acc, decl) => acc + (alias.name -> Resolution.Declaration(decl))
      }
    case Ast.UseOrImport.Import(clazz, alias, loc) => env + (alias.name -> Resolution.JavaClass(clazz))
  }

  /**
    * Adds the given uses and imports to the use environment.
    */
  private def appendAllUseEnv(env: ListMap[String, Resolution], usesAndImports: List[Ast.UseOrImport], root: NamedAst.Root)(implicit flix: Flix): ListMap[String, Resolution] = {
    usesAndImports.foldLeft(env)(appendUseEnv(_, _, root))
  }

  /**
    * Creates a use environment from the given type parameters.
    */
  private def mkTypeParamEnv(tparams: List[ResolvedAst.TypeParam]): ListMap[String, Resolution] = {
    tparams.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, tparam) => acc + (tparam.name.name -> Resolution.TypeVar(tparam.sym))
    }
  }

  /**
    * Creates a use environment from the given formal parameters.
    */
  private def mkFormalParamEnv(fparams: List[ResolvedAst.FormalParam]): ListMap[String, Resolution] = {
    fparams.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, fparam) => acc + (fparam.sym.text -> Resolution.Var(fparam.sym))
    }
  }

  /**
    * Creates an environment from the given constraint parameters.
    */
  private def mkConstraintParamEnv(cparams: List[ResolvedAst.ConstraintParam]): ListMap[String, Resolution] = {
    cparams.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, cparam) => acc + (cparam.sym.text -> Resolution.Var(cparam.sym))
    }
  }

  /**
    * Creates an environment from the given spec.
    */
  private def mkSpecEnv(spec: ResolvedAst.Spec): ListMap[String, Resolution] = spec match {
    case ResolvedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstrs, loc) =>
      mkTypeParamEnv(tparams.tparams) ++ mkFormalParamEnv(fparams)
  }

  /**
    * Creates an environment from the given pattern.
    */
  @tailrec
  private def mkPatternEnv(pat0: ResolvedAst.Pattern): ListMap[String, Resolution] = pat0 match {
    case ResolvedAst.Pattern.Wild(loc) => ListMap.empty
    case ResolvedAst.Pattern.Var(sym, loc) => mkVarEnv(sym)
    case ResolvedAst.Pattern.Cst(cst, loc) => ListMap.empty
    case ResolvedAst.Pattern.Tag(sym, pat, loc) => mkPatternEnv(pat)
    case ResolvedAst.Pattern.Tuple(elms, loc) => mkPatternsEnv(elms)
    case ResolvedAst.Pattern.Array(elms, loc) => mkPatternsEnv(elms)
    case ResolvedAst.Pattern.ArrayTailSpread(elms, sym, loc) => mkPatternsEnv(elms) ++ mkVarEnv(sym)
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms, loc) => mkVarEnv(sym) ++ mkPatternsEnv(elms)
  }

  /**
    * Creates an environment from the given patterns.
    */
  private def mkPatternsEnv(pats: List[ResolvedAst.Pattern]): ListMap[String, Resolution] = {
    pats.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, pat) => acc ++ mkPatternEnv(pat)
    }
  }

  /**
    * Creates an environment from the given variable symbol.
    */
  private def mkVarEnv(sym: Symbol.VarSym): ListMap[String, Resolution] = ListMap.singleton(sym.text, Resolution.Var(sym))

  /**
    * Creates an environment from the given type variable symbol.
    */
  private def mkTypeVarEnv(sym: Symbol.UnkindedTypeVarSym): ListMap[String, Resolution] = {
    sym.text match {
      case VarText.Absent => throw InternalCompilerException("unexpected unnamed type var sym", sym.loc)
      case VarText.SourceText(s) => ListMap.singleton(s, Resolution.TypeVar(sym))
    }
  }

  /**
    * Builds a variable environment from the given resolution environment.
    */
  private def filterToVarEnv(env: ListMap[String, Resolution]): Map[String, Symbol.VarSym] = {
    env.m.flatMap {
      case (name, res) => res.collectFirst {
        case Resolution.Var(sym) => (name, sym)
      }
    }
  }

  /**
    * Converts the class into a Flix type.
    */
  private def flixifyType(clazz: Class[_], loc: SourceLocation): UnkindedType = clazz.getName match {
    case "java.math.BigDecimal" => UnkindedType.Cst(TypeConstructor.BigDecimal, loc)
    case "java.math.BigInteger" => UnkindedType.Cst(TypeConstructor.BigInt, loc)
    case "java.lang.String" => UnkindedType.Cst(TypeConstructor.Str, loc)
    case "java.util.function.Function" => UnkindedType.mkImpureArrow(UnkindedType.mkObject(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.Consumer" => UnkindedType.mkImpureArrow(UnkindedType.mkObject(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.Predicate" => UnkindedType.mkImpureArrow(UnkindedType.mkObject(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.IntFunction" => UnkindedType.mkImpureArrow(UnkindedType.mkInt32(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.IntConsumer" => UnkindedType.mkImpureArrow(UnkindedType.mkInt32(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.IntPredicate" => UnkindedType.mkImpureArrow(UnkindedType.mkInt32(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.IntUnaryOperator" => UnkindedType.mkImpureArrow(UnkindedType.mkInt32(loc), UnkindedType.mkInt32(loc), loc)
    case "java.util.function.LongFunction" => UnkindedType.mkImpureArrow(UnkindedType.mkInt64(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.LongConsumer" => UnkindedType.mkImpureArrow(UnkindedType.mkInt64(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.LongPredicate" => UnkindedType.mkImpureArrow(UnkindedType.mkInt64(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.LongUnaryOperator" => UnkindedType.mkImpureArrow(UnkindedType.mkInt64(loc), UnkindedType.mkInt64(loc), loc)
    case "java.util.function.DoubleFunction" => UnkindedType.mkImpureArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.DoubleConsumer" => UnkindedType.mkImpureArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.DoublePredicate" => UnkindedType.mkImpureArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.DoubleUnaryOperator" => UnkindedType.mkImpureArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkFloat64(loc), loc)
    case _ => UnkindedType.Cst(TypeConstructor.Native(clazz), loc)
  }

  /**
    * Enum describing the extent to which a class is accessible.
    */
  private sealed trait ClassAccessibility

  private object ClassAccessibility {
    case object Accessible extends ClassAccessibility

    case object Sealed extends ClassAccessibility

    case object Inaccessible extends ClassAccessibility
  }


  /**
    * Enum describing the extent to which an enum is accessible.
    */
  private sealed trait EnumAccessibility

  private object EnumAccessibility {
    case object Accessible extends EnumAccessibility

    case object Opaque extends EnumAccessibility

    case object Inaccessible extends EnumAccessibility
  }

  /**
    * Union of variables, definitions, and signatures.
    */
  private sealed trait ResolvedTerm

  private object ResolvedTerm {
    case class Var(sym: Symbol.VarSym) extends ResolvedTerm

    case class Def(defn: NamedAst.Declaration.Def) extends ResolvedTerm

    case class Sig(sig: NamedAst.Declaration.Sig) extends ResolvedTerm

    case class Tag(caze: NamedAst.Declaration.Case) extends ResolvedTerm

    case class RestrictableTag(caze: NamedAst.Declaration.RestrictableCase) extends ResolvedTerm
  }

  /**
    * Result of a name resolution.
    */
  private sealed trait Resolution

  private object Resolution {
    case class Declaration(decl: NamedAst.Declaration) extends Resolution

    case class JavaClass(clazz: Class[_]) extends Resolution

    case class Var(sym: Symbol.VarSym) extends Resolution

    case class TypeVar(sym: Symbol.UnkindedTypeVarSym) extends Resolution
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
  private case class SymbolTable(classes: Map[Symbol.ClassSym, ResolvedAst.Declaration.Class],
                                 instances: ListMap[Symbol.ClassSym, ResolvedAst.Declaration.Instance],
                                 defs: Map[Symbol.DefnSym, ResolvedAst.Declaration.Def],
                                 enums: Map[Symbol.EnumSym, ResolvedAst.Declaration.Enum],
                                 restrictableEnums: Map[Symbol.RestrictableEnumSym, ResolvedAst.Declaration.RestrictableEnum],
                                 effects: Map[Symbol.EffectSym, ResolvedAst.Declaration.Effect],
                                 typeAliases: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
    def addClass(clazz: ResolvedAst.Declaration.Class): SymbolTable = copy(classes = classes + (clazz.sym -> clazz))

    def addDef(defn: ResolvedAst.Declaration.Def): SymbolTable = copy(defs = defs + (defn.sym -> defn))

    def addEnum(enum: ResolvedAst.Declaration.Enum): SymbolTable = copy(enums = enums + (enum.sym -> enum))

    def addRestrictableEnum(enum: ResolvedAst.Declaration.RestrictableEnum): SymbolTable = copy(restrictableEnums = restrictableEnums + (enum.sym -> enum))

    def addEffect(effect: ResolvedAst.Declaration.Effect): SymbolTable = copy(effects = effects + (effect.sym -> effect))

    def addTypeAlias(alias: ResolvedAst.Declaration.TypeAlias): SymbolTable = copy(typeAliases = typeAliases + (alias.sym -> alias))

    def addInstance(inst: ResolvedAst.Declaration.Instance): SymbolTable = copy(instances = instances + (inst.clazz.sym -> inst))

    def ++(that: SymbolTable): SymbolTable = {
      SymbolTable(
        classes = this.classes ++ that.classes,
        instances = this.instances ++ that.instances,
        defs = this.defs ++ that.defs,
        enums = this.enums ++ that.enums,
        restrictableEnums = this.restrictableEnums ++ that.restrictableEnums,
        effects = this.effects ++ that.effects,
        typeAliases = this.typeAliases ++ that.typeAliases
      )
    }
  }

  private object SymbolTable {
    val empty: SymbolTable = SymbolTable(Map.empty, ListMap.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    /**
      * Traverses `xs`, gathering the symbols from each element by applying the function `f`.
      */
    def traverse[T](xs: Iterable[T])(f: T => SymbolTable): SymbolTable = {
      xs.foldLeft(SymbolTable.empty) {
        case (acc, x) => acc ++ f(x)
      }
    }
  }
}
