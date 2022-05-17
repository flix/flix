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
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, Denotation}
import ca.uwaterloo.flix.language.ast.{Symbol, _}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Graph, InternalCompilerException, ParOps, Validation}

import java.lang.reflect.{Constructor, Field, Method, Modifier}
import scala.collection.mutable

/**
  * The Resolver phase performs name resolution on the program.
  */
object Resolver {

  /**
    * The maximum depth to which type aliases are unfolded.
    */
  val RecursionLimit: Int = 25

  /**
    * Symbols of classes that are derivable.
    */
  private val BoxableSym = new Symbol.ClassSym(Nil, "Boxable", SourceLocation.Unknown)
  private val EqSym = new Symbol.ClassSym(Nil, "Eq", SourceLocation.Unknown)
  private val OrderSym = new Symbol.ClassSym(Nil, "Order", SourceLocation.Unknown)
  private val ToStringSym = new Symbol.ClassSym(Nil, "ToString", SourceLocation.Unknown)
  private val HashSym = new Symbol.ClassSym(Nil, "Hash", SourceLocation.Unknown)

  private val DerivableSyms = List(BoxableSym, EqSym, OrderSym, ToStringSym, HashSym)

  /**
    * Performs name resolution on the given program `root`.
    */
  def run(root: NamedAst.Root, oldRoot: ResolvedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[ResolvedAst.Root, ResolutionError] = flix.phase("Resolver") {

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    flatMapN(resolveTypeAliases(root.typeAliases, root)) {
      case (taenv, taOrder) =>

        val classesVal = resolveClasses(root, taenv, oldRoot, changeSet)

        val instancesVal = root.instances.flatMap {
          case (ns0, instances0) => instances0.map {
            case (_, instances) => traverse(instances)(resolveInstance(_, taenv, ns0, root)) map {
              case is => is.head.sym.clazz -> is
            }
          }
        }

        val defsVal = resolveDefs(root, taenv, oldRoot, changeSet)

        val enumsVal = root.enums.flatMap {
          case (ns0, enums) => enums.map {
            case (_, enum) => resolveEnum(enum, taenv, ns0, root) map {
              case d => d.sym -> d
            }
          }
        }

        flatMapN(classesVal, sequence(instancesVal), defsVal, sequence(enumsVal)) {
          case (classes, instances, defs, enums) =>
            mapN(checkSuperClassDag(classes)) {
              _ => ResolvedAst.Root(classes, combine(instances), defs, enums.toMap, taenv, taOrder, root.entryPoint, root.reachable, root.sources)
            }
        }
    }
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
  private def checkSuperClassDag(classes: Map[Symbol.ClassSym, ResolvedAst.Class]): Validation[Unit, ResolutionError] = {

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

  /**
    * Resolves the type aliases in the given root.
    *
    * Returns a pair:
    *   - a map of type alias symbols to their AST nodes
    *   - a list of the aliases in a processing order,
    *     such that any alias only depends on those earlier in the list
    */
  private def resolveTypeAliases(aliases0: Map[Name.NName, Map[String, NamedAst.TypeAlias]], root: NamedAst.Root)(implicit flix: Flix): Validation[(Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], List[Symbol.TypeAliasSym]), ResolutionError] = {

    /**
      * Partially resolves the type alias.
      *
      * Type aliases within the type are given temporary placeholders.
      */
    def semiResolveTypeAlias(alias: NamedAst.TypeAlias, ns: Name.NName): Validation[ResolvedAst.TypeAlias, ResolutionError] = alias match {
      case NamedAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
        val tparams = resolveTypeParams(tparams0, ns, root)
        semiResolveType(tpe0, ns, root) map {
          tpe => ResolvedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
        }
    }

    /**
      * Gets a list of all type aliases used in the partially resolved type tpe0.
      */
    def getAliasUses(tpe0: Type): List[Symbol.TypeAliasSym] = tpe0 match {
      case tvar: Type.Var => tvar.asUnkinded; Nil
      case Type.Ascribe(tpe, _, _) => getAliasUses(tpe)
      case Type.Cst(TypeConstructor.UnappliedAlias(sym), _) => sym :: Nil
      case Type.Cst(_, _) => Nil
      case Type.Apply(tpe1, tpe2, _) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
      case _: Type.Alias => throw InternalCompilerException("unexpected applied alias")
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
    def findResolutionOrder(aliases: Iterable[ResolvedAst.TypeAlias]): Validation[List[Symbol.TypeAliasSym], ResolutionError] = {
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
    def finishResolveTypeAliases(aliases0: List[ResolvedAst.TypeAlias]): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ResolutionError] = {
      Validation.fold(aliases0, Map.empty[Symbol.TypeAliasSym, ResolvedAst.TypeAlias]) {
        case (taenv, ResolvedAst.TypeAlias(doc, mod, sym, tparams, tpe0, loc)) =>
          finishResolveType(tpe0, taenv) map {
            tpe =>
              val alias = ResolvedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
              taenv + (sym -> alias)
          }
      }
    }

    // Extract all the aliases and namespaces from the map.
    val aliasesMap0 = for {
      (ns, aliasesInNs) <- aliases0
      (_, alias) <- aliasesInNs
    } yield (alias, ns)

    // Partially resolve the aliases
    val semiAliasesVal = traverse(aliasesMap0) {
      case (alias, ns) => semiResolveTypeAlias(alias, ns)
    }

    flatMapN(semiAliasesVal) {
      // Get the resolution order
      semiAliases =>
        flatMapN(findResolutionOrder(semiAliases)) {
          sortedSyms =>
            // Create mapping for the partially resolved aliases
            val semiAliasEnv = semiAliases.map(alias => alias.sym -> alias).toMap

            // Get the sorted aliases from the mapping
            val sortedAliases = sortedSyms.map(semiAliasEnv)

            // Resolve the sorted aliases
            val aliasesVal = finishResolveTypeAliases(sortedAliases)

            mapN(aliasesVal) {
              aliases => (aliases, sortedSyms)
            }

        }
    }
  }


  object Constraints {

    /**
      * Performs name resolution on the given `constraints` in the given namespace `ns0`.
      */
    def resolve(constraints: List[NamedAst.Constraint], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Constraint], ResolutionError] = {
      traverse(constraints)(c => resolve(c, taenv, ns0, root))
    }

    /**
      * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
      */
    def resolve(c0: NamedAst.Constraint, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Constraint, ResolutionError] = c0 match {
      case NamedAst.Constraint(cparams0, head0, body0, loc) =>
        val cparamsVal = traverse(cparams0)(p => Params.resolve(p, ns0, root))
        val headVal = Predicates.Head.resolve(head0, taenv, ns0, root)
        val bodyVal = traverse(body0)(Predicates.Body.resolve(_, taenv, ns0, root))
        mapN(cparamsVal, headVal, bodyVal) {
          case (cparams, head, body) => ResolvedAst.Constraint(cparams, head, body, loc)
        }
    }

  }

  /**
    * Resolves all the classes in the given root.
    */
  private def resolveClasses(root: NamedAst.Root, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], oldRoot: ResolvedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.ClassSym, ResolvedAst.Class], ResolutionError] = {

    val rootClasses = for {
      (ns, classes) <- root.classes
      (_, clazz) <- classes
    } yield clazz.sym -> (clazz, ns)

    val (staleClasses, freshClasses) = changeSet.partition(rootClasses, oldRoot.classes)

    val results = ParOps.parMap(staleClasses.values) {
      case (clazz, ns) => resolveClass(clazz, taenv, ns, root)
    }

    Validation.sequence(results) map {
      res =>
        res.foldLeft(freshClasses) {
          case (acc, clazz) => acc + (clazz.sym -> clazz)
        }
    }
  }

  /**
    * Resolves all the classes in the given root.
    */
  def resolveClass(c0: NamedAst.Class, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Class, ResolutionError] = c0 match {
    case NamedAst.Class(doc, ann0, mod, sym, tparam0, superClasses0, signatures, laws0, loc) =>
      val tparam = Params.resolveTparam(tparam0)
      val annVal = traverse(ann0)(visitAnnotation(_, taenv, ns0, root))
      val sigsListVal = traverse(signatures)(resolveSig(_, taenv, ns0, root))
      // ignore the parameter of the super class; we don't use it
      val superClassesVal = traverse(superClasses0)(tconstr => resolveSuperClass(tconstr, taenv, ns0, root))
      val lawsVal = traverse(laws0)(resolveDef(_, taenv, ns0, root))
      mapN(annVal, sigsListVal, superClassesVal, lawsVal) {
        case (ann, sigsList, superClasses, laws) =>
          val sigs = sigsList.map(sig => (sig.sym, sig)).toMap
          ResolvedAst.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc)
      }
  }

  /**
    * Performs name resolution on the given instance `i0` in the given namespace `ns0`.
    */
  def resolveInstance(i0: NamedAst.Instance, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Instance, ResolutionError] = i0 match {
    case NamedAst.Instance(doc, ann0, mod, clazz0, tpe0, tconstrs0, defs0, loc) =>
      val annVal = traverse(ann0)(visitAnnotation(_, taenv, ns0, root))
      val clazzVal = lookupClassForImplementation(clazz0, ns0, root)
      val tpeVal = resolveType(tpe0, taenv, ns0, root)
      val tconstrsVal = traverse(tconstrs0)(resolveTypeConstraint(_, taenv, ns0, root))
      val defsVal = traverse(defs0)(resolveDef(_, taenv, ns0, root))
      mapN(annVal, clazzVal, tpeVal, tconstrsVal, defsVal) {
        case (ann, clazz, tpe, tconstrs, defs) =>
          val sym = Symbol.freshInstanceSym(clazz.sym, clazz0.loc)
          ResolvedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, defs, ns0, loc)
      }
  }

  /**
    * Performs name resolution on the given signature `s0` in the given namespace `ns0`.
    */
  def resolveSig(s0: NamedAst.Sig, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Sig, ResolutionError] = s0 match {
    case NamedAst.Sig(sym, spec0, exp0) =>
      val specVal = resolveSpec(spec0, taenv, ns0, root)
      val expVal = traverse(exp0)(Expressions.resolve(_, taenv, ns0, root))
      mapN(specVal, expVal) {
        case (spec, exp) => ResolvedAst.Sig(sym, spec, exp.headOption)
      }
  }

  /**
    * Resolves all the definitions in the given root.
    */
  private def resolveDefs(root: NamedAst.Root, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], oldRoot: ResolvedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.DefnSym, ResolvedAst.Def], ResolutionError] = {
    def getDef(defOrSig: NamedAst.DefOrSig): Option[NamedAst.Def] = defOrSig match {
      case NamedAst.DefOrSig.Def(d) => Some(d)
      case NamedAst.DefOrSig.Sig(_) => None
    }

    val rootDefs = for {
      (ns, defsAndSigs) <- root.defsAndSigs
      (_, defOrSig) <- defsAndSigs
      defn <- getDef(defOrSig)
    } yield defn.sym -> (defn, ns)

    val (staleDefs, freshDefs) = changeSet.partition(rootDefs, oldRoot.defs)

    val results = ParOps.parMap(staleDefs.values) {
      case (defn, ns) => resolveDef(defn, taenv, ns, root)
    }

    Validation.sequence(results) map {
      res =>
        res.foldLeft(freshDefs) {
          case (acc, defn) => acc + (defn.sym -> defn)
        }
    }
  }

  /**
    * Performs name resolution on the given definition `d0` in the given namespace `ns0`.
    */
  def resolveDef(d0: NamedAst.Def, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Def, ResolutionError] = d0 match {
    case NamedAst.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      val specVal = resolveSpec(spec0, taenv, ns0, root)
      val expVal = Expressions.resolve(exp0, taenv, ns0, root)
      mapN(specVal, expVal) {
        case (spec, exp) => ResolvedAst.Def(sym, spec, exp)
      }
  }

  /**
    * Performs name resolution on the given spec `s0` in the given namespace `ns0`.
    */
  def resolveSpec(s0: NamedAst.Spec, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Spec, ResolutionError] = s0 match {
    case NamedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, retTpe0, pur0, eff0, loc) => // TODO handle eff

      val tparams = resolveTypeParams(tparams0, ns0, root)
      val fparamsVal = resolveFormalParams(fparams0, taenv, ns0, root)
      val annVal = traverse(ann0)(visitAnnotation(_, taenv, ns0, root))
      val schemeVal = resolveScheme(sc0, taenv, ns0, root)
      val retTpeVal = resolveType(retTpe0, taenv, ns0, root)
      val purVal = resolveType(pur0, taenv, ns0, root)

      mapN(fparamsVal, annVal, schemeVal, retTpeVal, purVal) {
        case (fparams, ann, scheme, retTpe, pur) =>
          ResolvedAst.Spec(doc, ann, mod, tparams, fparams, scheme, retTpe, pur, loc)
      }
  }

  /**
    * Performs name resolution on the given enum `e0` in the given namespace `ns0`.
    */
  def resolveEnum(e0: NamedAst.Enum, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Enum, ResolutionError] = e0 match {
    case NamedAst.Enum(doc, ann0, mod, sym, tparams0, derives0, cases0, tpe0, loc) =>
      val annVal = traverse(ann0)(visitAnnotation(_, taenv, ns0, root))
      val tparams = resolveTypeParams(tparams0, ns0, root)
      val derivesVal = resolveDerivations(derives0, ns0, root)
      val casesVal = traverse(cases0)(resolveCase(_, e0, taenv, ns0, root))
      val tpeVal = resolveType(tpe0, taenv, ns0, root)
      mapN(annVal, derivesVal, casesVal, tpeVal) {
        case (ann, derives, cases, tpe) =>
          val sc = ResolvedAst.Scheme(tparams.tparams.map(_.sym), Nil, tpe)
          ResolvedAst.Enum(doc, ann, mod, sym, tparams, derives, cases.toMap, tpe, sc, loc)
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveCase(caze0: (Name.Tag, NamedAst.Case), enum0: NamedAst.Enum, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix) = (enum0, caze0) match {
    case (NamedAst.Enum(doc, ann, mod, sym, tparams, _, _, _, loc), (name, NamedAst.Case(enumIdent, tag, tpe0))) =>
      val tpeVal = resolveType(tpe0, taenv, ns0, root)
      mapN(tpeVal) {
        tpe =>
          val freeVars = tparams.tparams.map(_.sym)
          val caseType = tpe
          val enumType = mkUnkindedEnum(sym, freeVars, sym.loc)
          val base = Type.mkTag(sym, tag, caseType, enumType, tpe.loc)
          val sc = ResolvedAst.Scheme(freeVars, Nil, base)
          name -> ResolvedAst.Case(enumIdent, tag, tpe, sc)
      }
  }

  /**
    * Performs name resolution on the given attribute `a0` in the given namespace `ns0`.
    */
  private def visitAttribute(a0: NamedAst.Attribute, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Attribute, ResolutionError] = {
    for {
      tpe <- resolveType(a0.tpe, taenv, ns0, root)
    } yield ResolvedAst.Attribute(a0.ident, tpe, a0.loc)
  }

  /**
    * Performs name resolution on the given annotation `a0` in the given namespace `ns0`.
    */
  private def visitAnnotation(a0: NamedAst.Annotation, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Annotation, ResolutionError] = {
    val argsVal = traverse(a0.args)(Expressions.resolve(_, taenv, ns0, root))
    mapN(argsVal) {
      args => ResolvedAst.Annotation(a0.name, args, a0.loc)
    }
  }

  object Expressions {

    /**
      * Performs name resolution on the given expression `exp0` in the namespace `ns0`.
      */
    def resolve(exp0: NamedAst.Expression, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Expression, ResolutionError] = {

      /**
        * Creates `arity` fresh fparams for use in a curried def or sig application.
        */
      def mkFreshFparams(arity: Int, loc: SourceLocation): List[ResolvedAst.FormalParam] = {
        // Introduce a fresh variable symbol for each argument of the function definition.
        val varSyms = (0 until arity).map(i => Symbol.freshVarSym(Flix.Delimiter + i, BoundBy.FormalParam, loc)).toList

        // Introduce a formal parameter for each variable symbol.
        varSyms.map(sym => ResolvedAst.FormalParam(sym, Ast.Modifiers.Empty, sym.tvar, loc))
      }

      /**
        * Creates a lambda for use in a curried dif or sig application.
        */
      def mkCurriedLambda(fparams: List[ResolvedAst.FormalParam], baseExp: ResolvedAst.Expression, loc: SourceLocation): ResolvedAst.Expression = {
        val l = loc.asSynthetic

        // The arguments passed to the definition (i.e. the fresh variable symbols).
        val argExps = fparams.map(fparam => ResolvedAst.Expression.Var(fparam.sym, fparam.sym.tvar, l))

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
      def visitDef(defn: NamedAst.Def, loc: SourceLocation): ResolvedAst.Expression = {
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
      def visitSig(sig: NamedAst.Sig, loc: SourceLocation): ResolvedAst.Expression = {
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
        * Resolve the application expression, performing currying over the subexpressions.
        */
      def visitApply(exp: NamedAst.Expression.Apply, region: Option[Symbol.VarSym]): Validation[ResolvedAst.Expression, ResolutionError] = exp match {
        case NamedAst.Expression.Apply(exp0, exps0, loc) =>
          val expVal = visitExp(exp0, region)
          val expsVal = traverse(exps0)(visitExp(_, region))
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
      def visitApplyDef(app: NamedAst.Expression.Apply, defn: NamedAst.Def, exps: List[NamedAst.Expression], region: Option[Symbol.VarSym], innerLoc: SourceLocation, outerLoc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        if (defn.spec.fparams.length == exps.length) {
          // Case 1: Hooray! We can call the function directly.
          val esVal = traverse(exps)(visitExp(_, region))
          mapN(esVal) {
            es =>
              val base = ResolvedAst.Expression.Def(defn.sym, innerLoc)
              ResolvedAst.Expression.Apply(base, es, outerLoc)
          }
        } else {
          // Case 2: We have to curry. (See below).
          visitApply(app, region)
        }
      }

      /**
        * Resolve the application expression, applying `sig` to `exps`.
        */
      def visitApplySig(app: NamedAst.Expression.Apply, sig: NamedAst.Sig, exps: List[NamedAst.Expression], region: Option[Symbol.VarSym], innerLoc: SourceLocation, outerLoc: SourceLocation): Validation[ResolvedAst.Expression, ResolutionError] = {
        if (sig.spec.fparams.length == exps.length) {
          // Case 1: Hooray! We can call the function directly.
          val esVal = traverse(exps)(visitExp(_, region))
          mapN(esVal) {
            case (es) =>
              val base = ResolvedAst.Expression.Sig(sig.sym, innerLoc)
              ResolvedAst.Expression.Apply(base, es, outerLoc)
          }
        } else {
          // Case 2: We have to curry. (See below).
          visitApply(app, region)
        }
      }

      /**
        * Local visitor.
        */
      def visitExp(e0: NamedAst.Expression, region: Option[Symbol.VarSym]): Validation[ResolvedAst.Expression, ResolutionError] = e0 match {

        case NamedAst.Expression.Wild(loc) =>
          ResolvedAst.Expression.Wild(loc).toSuccess

        case NamedAst.Expression.Var(sym, loc) =>
          ResolvedAst.Expression.Var(sym, sym.tvar, loc).toSuccess

        case NamedAst.Expression.DefOrSig(qname, loc) =>
          mapN(lookupDefOrSig(qname, ns0, root)) {
            case NamedAst.DefOrSig.Def(defn) => visitDef(defn, loc)
            case NamedAst.DefOrSig.Sig(sig) => visitSig(sig, loc)
          }

        case NamedAst.Expression.Hole(nameOpt, loc) =>
          val sym = nameOpt match {
            case None => Symbol.freshHoleSym(loc)
            case Some(name) => Symbol.mkHoleSym(ns0, name)
          }
          ResolvedAst.Expression.Hole(sym, loc).toSuccess

        case NamedAst.Expression.Use(use, exp, loc) =>
          // Lookup the used name to ensure that it exists.
          use match {
            case NamedAst.Use.UseDefOrSig(qname, _, _) =>
              flatMapN(lookupDefOrSig(qname, ns0, root))(_ => visitExp(exp, region))

            case NamedAst.Use.UseTypeOrClass(qname, _, _) =>
              flatMapN(resolveType(NamedAst.Type.Ambiguous(qname, loc), taenv, ns0, root))(_ => visitExp(exp, region))

            case NamedAst.Use.UseTag(qname, tag, _, _) =>
              flatMapN(lookupEnumByTag(Some(qname), tag, ns0, root))(_ => visitExp(exp, region))
          }

        case NamedAst.Expression.Unit(loc) => ResolvedAst.Expression.Unit(loc).toSuccess

        case NamedAst.Expression.Null(loc) => ResolvedAst.Expression.Null(loc).toSuccess

        case NamedAst.Expression.True(loc) => ResolvedAst.Expression.True(loc).toSuccess

        case NamedAst.Expression.False(loc) => ResolvedAst.Expression.False(loc).toSuccess

        case NamedAst.Expression.Char(lit, loc) => ResolvedAst.Expression.Char(lit, loc).toSuccess

        case NamedAst.Expression.Float32(lit, loc) => ResolvedAst.Expression.Float32(lit, loc).toSuccess

        case NamedAst.Expression.Float64(lit, loc) => ResolvedAst.Expression.Float64(lit, loc).toSuccess

        case NamedAst.Expression.Int8(lit, loc) => ResolvedAst.Expression.Int8(lit, loc).toSuccess

        case NamedAst.Expression.Int16(lit, loc) => ResolvedAst.Expression.Int16(lit, loc).toSuccess

        case NamedAst.Expression.Int32(lit, loc) => ResolvedAst.Expression.Int32(lit, loc).toSuccess

        case NamedAst.Expression.Int64(lit, loc) => ResolvedAst.Expression.Int64(lit, loc).toSuccess

        case NamedAst.Expression.BigInt(lit, loc) => ResolvedAst.Expression.BigInt(lit, loc).toSuccess

        case NamedAst.Expression.Str(lit, loc) => ResolvedAst.Expression.Str(lit, loc).toSuccess

        case NamedAst.Expression.Default(loc) => ResolvedAst.Expression.Default(loc).toSuccess

        case app@NamedAst.Expression.Apply(NamedAst.Expression.DefOrSig(qname, innerLoc), exps, outerLoc) =>
          flatMapN(lookupDefOrSig(qname, ns0, root)) {
            case NamedAst.DefOrSig.Def(defn) => visitApplyDef(app, defn, exps, region, innerLoc, outerLoc)
            case NamedAst.DefOrSig.Sig(sig) => visitApplySig(app, sig, exps, region, innerLoc, outerLoc)
          }

        case app@NamedAst.Expression.Apply(_, _, _) => visitApply(app, region)

        case NamedAst.Expression.Lambda(fparam, exp, loc) =>
          val pVal = Params.resolve(fparam, taenv, ns0, root)
          val eVal = visitExp(exp, region)
          mapN(pVal, eVal) {
            case (p, e) => ResolvedAst.Expression.Lambda(p, e, loc)
          }

        case NamedAst.Expression.Unary(sop, exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Unary(sop, e, loc)
          }

        case NamedAst.Expression.Binary(sop, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Binary(sop, e1, e2, loc)
          }

        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          val e3Val = visitExp(exp3, region)
          mapN(e1Val, e2Val, e3Val) {
            case (e1, e2, e3) => ResolvedAst.Expression.IfThenElse(e1, e2, e3, loc)
          }

        case NamedAst.Expression.Stm(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Stm(e1, e2, loc)
          }

        case NamedAst.Expression.Discard(exp, loc) =>
          visitExp(exp, region) map {
            case e => ResolvedAst.Expression.Discard(e, loc)
          }

        case NamedAst.Expression.Let(sym, mod, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Let(sym, mod, e1, e2, loc)
          }

        case NamedAst.Expression.LetRec(sym, mod, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.LetRec(sym, mod, e1, e2, loc)
          }

        case NamedAst.Expression.Region(tpe, loc) =>
          ResolvedAst.Expression.Region(tpe, loc).toSuccess

        case NamedAst.Expression.Scope(sym, regionVar, exp, loc) =>
          val eVal = visitExp(exp, Some(sym))
          mapN(eVal) {
            e => ResolvedAst.Expression.Scope(sym, regionVar, e, loc)
          }

        case NamedAst.Expression.Match(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.MatchRule(pat, guard, body) =>
              val pVal = Patterns.resolve(pat, ns0, root)
              val gVal = visitExp(guard, region)
              val bVal = visitExp(body, region)
              mapN(pVal, gVal, bVal) {
                case (p, g, b) => ResolvedAst.MatchRule(p, g, b)
              }
          }

          val eVal = visitExp(exp, region)
          val rsVal = rulesVal
          mapN(eVal, rsVal) {
            case (e, rs) => ResolvedAst.Expression.Match(e, rs, loc)
          }

        case NamedAst.Expression.Choose(star, exps, rules, loc) =>
          val expsVal = traverse(exps)(visitExp(_, region))
          val rulesVal = traverse(rules) {
            case NamedAst.ChoiceRule(pat0, exp0) =>
              val p = pat0.map {
                case NamedAst.ChoicePattern.Wild(loc) => ResolvedAst.ChoicePattern.Wild(loc)
                case NamedAst.ChoicePattern.Absent(loc) => ResolvedAst.ChoicePattern.Absent(loc)
                case NamedAst.ChoicePattern.Present(sym, loc) => ResolvedAst.ChoicePattern.Present(sym, loc)
              }
              mapN(visitExp(exp0, region)) {
                case e => ResolvedAst.ChoiceRule(p, e)
              }
          }
          mapN(expsVal, rulesVal) {
            case (es, rs) => ResolvedAst.Expression.Choose(star, es, rs, loc)
          }

        case NamedAst.Expression.Tag(enum, tag, expOpt, loc) => expOpt match {
          case None =>
            // Case 1: The tag has does not have an expression.
            // Either it is implicitly Unit or the tag is used as a function.

            // Lookup the enum to determine the type of the tag.
            lookupEnumByTag(enum, tag, ns0, root) map {
              case decl =>
                // Retrieve the relevant case.
                val caze = decl.cases(tag)

                // Check if the tag value has Unit type.
                if (isUnitType(caze.tpe)) {
                  // Case 1.1: The tag value has Unit type. Construct the Unit expression.
                  val e = ResolvedAst.Expression.Unit(loc)
                  ResolvedAst.Expression.Tag(decl.sym, tag, e, loc)
                } else {
                  // Case 1.2: The tag has a non-Unit type. Hence the tag is used as a function.
                  // If the tag is `Some` we construct the lambda: x -> Some(x).

                  // Construct a fresh symbol for the formal parameter.
                  val freshVar = Symbol.freshVarSym("x" + Flix.Delimiter, BoundBy.FormalParam, loc)

                  // Construct the formal parameter for the fresh symbol.
                  val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, Type.freshUnkindedVar(loc), loc)

                  // Construct a variable expression for the fresh symbol.
                  val varExp = ResolvedAst.Expression.Var(freshVar, freshVar.tvar, loc)

                  // Construct the tag expression on the fresh symbol expression.
                  val tagExp = ResolvedAst.Expression.Tag(decl.sym, caze.tag, varExp, loc)

                  // Assemble the lambda expressions.
                  ResolvedAst.Expression.Lambda(freshParam, tagExp, loc)
                }
            }
          case Some(exp) =>
            // Case 2: The tag has an expression. Perform resolution on it.
            val dVal = lookupEnumByTag(enum, tag, ns0, root)
            val eVal = visitExp(exp, region)
            mapN(dVal, eVal) {
              case (d, e) => ResolvedAst.Expression.Tag(d.sym, tag, e, loc)
            }
        }

        case NamedAst.Expression.Tuple(elms, loc) =>
          val esVal = traverse(elms)(e => visitExp(e, region))
          mapN(esVal) {
            es => ResolvedAst.Expression.Tuple(es, loc)
          }

        case NamedAst.Expression.RecordEmpty(loc) =>
          ResolvedAst.Expression.RecordEmpty(loc).toSuccess

        case NamedAst.Expression.RecordSelect(base, field, loc) =>
          val bVal = visitExp(base, region)
          mapN(bVal) {
            b => ResolvedAst.Expression.RecordSelect(b, field, loc)
          }

        case NamedAst.Expression.RecordExtend(field, value, rest, loc) =>
          val vVal = visitExp(value, region)
          val rVal = visitExp(rest, region)
          mapN(vVal, rVal) {
            case (v, r) => ResolvedAst.Expression.RecordExtend(field, v, r, loc)
          }

        case NamedAst.Expression.RecordRestrict(field, rest, loc) =>
          val rVal = visitExp(rest, region)
          mapN(rVal) {
            r => ResolvedAst.Expression.RecordRestrict(field, r, loc)
          }

        case NamedAst.Expression.New(qname, exp, loc) =>
          val erVal = traverse(exp)(visitExp(_, region)).map(_.headOption)
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
          val esVal = traverse(exps)(visitExp(_, region))
          val erVal = traverse(exp)(visitExp(_, region)).map(_.headOption)
          mapN(esVal, erVal) {
            case (es, er) =>
              val reg = getExplicitOrImplicitRegion(er, region, loc)
              ResolvedAst.Expression.ArrayLit(es, reg, loc)
          }

        case NamedAst.Expression.ArrayNew(exp1, exp2, exp3, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          val erVal = traverse(exp3)(visitExp(_, region)).map(_.headOption)
          mapN(e1Val, e2Val, erVal) {
            case (e1, e2, er) =>
              val reg = getExplicitOrImplicitRegion(er, region, loc)
              ResolvedAst.Expression.ArrayNew(e1, e2, reg, loc)
          }

        case NamedAst.Expression.ArrayLoad(base, index, loc) =>
          val bVal = visitExp(base, region)
          val iVal = visitExp(index, region)
          mapN(bVal, iVal) {
            case (b, i) => ResolvedAst.Expression.ArrayLoad(b, i, loc)
          }

        case NamedAst.Expression.ArrayStore(base, index, elm, loc) =>
          val bVal = visitExp(base, region)
          val iVal = visitExp(index, region)
          val eVal = visitExp(elm, region)
          mapN(bVal, iVal, eVal) {
            case (b, i, e) => ResolvedAst.Expression.ArrayStore(b, i, e, loc)
          }

        case NamedAst.Expression.ArrayLength(base, loc) =>
          val bVal = visitExp(base, region)
          mapN(bVal) {
            b => ResolvedAst.Expression.ArrayLength(b, loc)
          }

        case NamedAst.Expression.ArraySlice(base, startIndex, endIndex, loc) =>
          val bVal = visitExp(base, region)
          val i1Val = visitExp(startIndex, region)
          val i2Val = visitExp(endIndex, region)
          mapN(bVal, i1Val, i2Val) {
            case (b, i1, i2) => ResolvedAst.Expression.ArraySlice(b, i1, i2, loc)
          }

        case NamedAst.Expression.Ref(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = traverse(exp2)(visitExp(_, region)).map(_.headOption)
          mapN(e1Val, e2Val) {
            case (e1, e2) =>
              val reg = getExplicitOrImplicitRegion(e2, region, loc)
              ResolvedAst.Expression.Ref(e1, reg, loc)
          }

        case NamedAst.Expression.Deref(exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Deref(e, loc)
          }

        case NamedAst.Expression.Assign(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.Assign(e1, e2, loc)
          }

        case NamedAst.Expression.Ascribe(exp, expectedType, expectedEff, loc) =>
          val expectedTypVal = expectedType match {
            case None => (None: Option[Type]).toSuccess
            case Some(t) => mapN(resolveType(t, taenv, ns0, root))(x => Some(x))
          }
          val expectedEffVal = expectedEff match {
            case None => (None: Option[Type]).toSuccess
            case Some(f) => mapN(resolveType(f, taenv, ns0, root))(x => Some(x))
          }

          val eVal = visitExp(exp, region)
          mapN(eVal, expectedTypVal, expectedEffVal) {
            case (e, t, f) => ResolvedAst.Expression.Ascribe(e, t, f, loc)
          }

        case NamedAst.Expression.Cast(exp, declaredType, declaredEff, loc) =>

          val declaredTypVal = declaredType match {
            case None => (None: Option[Type]).toSuccess
            case Some(t) => mapN(resolveType(t, taenv, ns0, root))(x => Some(x))
          }
          val declaredEffVal = declaredEff match {
            case None => (None: Option[Type]).toSuccess
            case Some(f) => mapN(resolveType(f, taenv, ns0, root))(x => Some(x))
          }

          val eVal = visitExp(exp, region)
          mapN(eVal, declaredTypVal, declaredEffVal) {
            case (e, t, f) => ResolvedAst.Expression.Cast(e, t, f, loc)
          }

        case NamedAst.Expression.TryCatch(exp, rules, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.CatchRule(sym, className, body) =>
              val clazzVal = lookupJvmClass(className, sym.loc)
              val bVal = visitExp(body, region)
              mapN(clazzVal, bVal) {
                case (clazz, b) => ResolvedAst.CatchRule(sym, clazz, b)
              }
          }

          val eVal = visitExp(exp, region)
          mapN(eVal, rulesVal) {
            case (e, rs) => ResolvedAst.Expression.TryCatch(e, rs, loc)
          }

        case NamedAst.Expression.InvokeConstructor(className, args, sig, loc) =>
          val argsVal = traverse(args)(visitExp(_, region))
          val sigVal = traverse(sig)(resolveType(_, taenv, ns0, root))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmConstructor(className, ts, loc)) {
                case constructor => ResolvedAst.Expression.InvokeConstructor(constructor, as, loc)
              }
          }

        case NamedAst.Expression.InvokeMethod(className, methodName, exp, args, sig, loc) =>
          val expVal = visitExp(exp, region)
          val argsVal = traverse(args)(visitExp(_, region))
          val sigVal = traverse(sig)(resolveType(_, taenv, ns0, root))
          flatMapN(sigVal, expVal, argsVal) {
            case (ts, e, as) =>
              mapN(lookupJvmMethod(className, methodName, ts, static = false, loc)) {
                case method => ResolvedAst.Expression.InvokeMethod(method, e, as, loc)
              }
          }

        case NamedAst.Expression.InvokeStaticMethod(className, methodName, args, sig, loc) =>
          val argsVal = traverse(args)(visitExp(_, region))
          val sigVal = traverse(sig)(resolveType(_, taenv, ns0, root))
          flatMapN(sigVal, argsVal) {
            case (ts, as) =>
              mapN(lookupJvmMethod(className, methodName, ts, static = true, loc)) {
                case method => ResolvedAst.Expression.InvokeStaticMethod(method, as, loc)
              }
          }

        case NamedAst.Expression.GetField(className, fieldName, exp, loc) =>
          mapN(lookupJvmField(className, fieldName, static = false, loc), visitExp(exp, region)) {
            case (field, e) => ResolvedAst.Expression.GetField(field, e, loc)
          }

        case NamedAst.Expression.PutField(className, fieldName, exp1, exp2, loc) =>
          mapN(lookupJvmField(className, fieldName, static = false, loc), visitExp(exp1, region), visitExp(exp2, region)) {
            case (field, e1, e2) => ResolvedAst.Expression.PutField(field, e1, e2, loc)
          }

        case NamedAst.Expression.GetStaticField(className, fieldName, loc) =>
          mapN(lookupJvmField(className, fieldName, static = true, loc)) {
            case field => ResolvedAst.Expression.GetStaticField(field, loc)
          }

        case NamedAst.Expression.PutStaticField(className, fieldName, exp, loc) =>
          mapN(lookupJvmField(className, fieldName, static = true, loc), visitExp(exp, region)) {
            case (field, e) => ResolvedAst.Expression.PutStaticField(field, e, loc)
          }

        case NamedAst.Expression.NewChannel(exp, tpe, loc) =>
          val tVal = resolveType(tpe, taenv, ns0, root)
          val eVal = visitExp(exp, region)
          mapN(tVal, eVal) {
            case (t, e) => ResolvedAst.Expression.NewChannel(e, t, loc)
          }

        case NamedAst.Expression.GetChannel(exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.GetChannel(e, loc)
          }

        case NamedAst.Expression.PutChannel(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.PutChannel(e1, e2, loc)
          }

        case NamedAst.Expression.SelectChannel(rules, default, loc) =>
          val rulesVal = traverse(rules) {
            case NamedAst.SelectChannelRule(sym, chan, body) =>
              val cVal = visitExp(chan, region)
              val bVal = visitExp(body, region)
              mapN(cVal, bVal) {
                case (c, b) => ResolvedAst.SelectChannelRule(sym, c, b)
              }
          }

          val defaultVal = default match {
            case Some(exp) =>
              val eVal = visitExp(exp, region)
              mapN(eVal) {
                e => Some(e)
              }
            case None => None.toSuccess
          }

          mapN(rulesVal, defaultVal) {
            case (rs, d) => ResolvedAst.Expression.SelectChannel(rs, d, loc)
          }

        case NamedAst.Expression.Spawn(exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Spawn(e, loc)
          }

        case NamedAst.Expression.Lazy(exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Lazy(e, loc)
          }

        case NamedAst.Expression.Force(exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.Force(e, loc)
          }

        case NamedAst.Expression.FixpointConstraintSet(cs0, loc) =>
          val csVal = traverse(cs0)(Constraints.resolve(_, taenv, ns0, root))
          mapN(csVal) {
            cs => ResolvedAst.Expression.FixpointConstraintSet(cs, loc)
          }

        case NamedAst.Expression.FixpointLambda(pparams, exp, loc) =>
          val psVal = traverse(pparams)(Params.resolve(_, taenv, ns0, root))
          val eVal = visitExp(exp, region)
          mapN(psVal, eVal) {
            case (ps, e) => ResolvedAst.Expression.FixpointLambda(ps, e, loc)
          }

        case NamedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.FixpointMerge(e1, e2, loc)
          }

        case NamedAst.Expression.FixpointSolve(exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.FixpointSolve(e, loc)
          }

        case NamedAst.Expression.FixpointFilter(pred, exp, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.FixpointFilter(pred, e, loc)
          }

        case NamedAst.Expression.FixpointProjectIn(exp, pred, loc) =>
          val eVal = visitExp(exp, region)
          mapN(eVal) {
            e => ResolvedAst.Expression.FixpointProjectIn(e, pred, loc)
          }

        case NamedAst.Expression.FixpointProjectOut(pred, exp1, exp2, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          mapN(e1Val, e2Val) {
            case (e1, e2) => ResolvedAst.Expression.FixpointProjectOut(pred, e1, e2, loc)
          }

        case NamedAst.Expression.Reify(t0, loc) =>
          val tVal = resolveType(t0, taenv, ns0, root)
          mapN(tVal) {
            t => ResolvedAst.Expression.Reify(t, loc)
          }

        case NamedAst.Expression.ReifyType(t0, k, loc) =>
          val tVal = resolveType(t0, taenv, ns0, root)
          mapN(tVal) {
            t => ResolvedAst.Expression.ReifyType(t, k, loc)
          }

        case NamedAst.Expression.ReifyEff(sym, exp1, exp2, exp3, loc) =>
          val e1Val = visitExp(exp1, region)
          val e2Val = visitExp(exp2, region)
          val e3Val = visitExp(exp3, region)
          mapN(e1Val, e2Val, e3Val) {
            case (e1, e2, e3) => ResolvedAst.Expression.ReifyEff(sym, e1, e2, e3, loc)
          }

      }

      visitExp(exp0, None)
    }

  }

  object Patterns {

    /**
      * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
      */
    def resolve(pat0: NamedAst.Pattern, ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

      def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
        case NamedAst.Pattern.Wild(loc) => ResolvedAst.Pattern.Wild(loc).toSuccess

        case NamedAst.Pattern.Var(sym, loc) => ResolvedAst.Pattern.Var(sym, loc).toSuccess

        case NamedAst.Pattern.Unit(loc) => ResolvedAst.Pattern.Unit(loc).toSuccess

        case NamedAst.Pattern.True(loc) => ResolvedAst.Pattern.True(loc).toSuccess

        case NamedAst.Pattern.False(loc) => ResolvedAst.Pattern.False(loc).toSuccess

        case NamedAst.Pattern.Char(lit, loc) => ResolvedAst.Pattern.Char(lit, loc).toSuccess

        case NamedAst.Pattern.Float32(lit, loc) => ResolvedAst.Pattern.Float32(lit, loc).toSuccess

        case NamedAst.Pattern.Float64(lit, loc) => ResolvedAst.Pattern.Float64(lit, loc).toSuccess

        case NamedAst.Pattern.Int8(lit, loc) => ResolvedAst.Pattern.Int8(lit, loc).toSuccess

        case NamedAst.Pattern.Int16(lit, loc) => ResolvedAst.Pattern.Int16(lit, loc).toSuccess

        case NamedAst.Pattern.Int32(lit, loc) => ResolvedAst.Pattern.Int32(lit, loc).toSuccess

        case NamedAst.Pattern.Int64(lit, loc) => ResolvedAst.Pattern.Int64(lit, loc).toSuccess

        case NamedAst.Pattern.BigInt(lit, loc) => ResolvedAst.Pattern.BigInt(lit, loc).toSuccess

        case NamedAst.Pattern.Str(lit, loc) => ResolvedAst.Pattern.Str(lit, loc).toSuccess

        case NamedAst.Pattern.Tag(enum, tag, pat, loc) =>
          val dVal = lookupEnumByTag(enum, tag, ns0, root)
          val pVal = visit(pat)
          mapN(dVal, pVal) {
            case (d, p) => ResolvedAst.Pattern.Tag(d.sym, tag, p, loc)
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
      def resolve(h0: NamedAst.Predicate.Head, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
        case NamedAst.Predicate.Head.Atom(pred, den, terms, loc) =>
          val tsVal = traverse(terms)(t => Expressions.resolve(t, taenv, ns0, root))
          mapN(tsVal) {
            ts => ResolvedAst.Predicate.Head.Atom(pred, den, ts, loc)
          }
      }
    }

    object Body {
      /**
        * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
        */
      def resolve(b0: NamedAst.Predicate.Body, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
        case NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
          val tsVal = traverse(terms)(t => Patterns.resolve(t, ns0, root))
          mapN(tsVal) {
            ts => ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc)
          }

        case NamedAst.Predicate.Body.Guard(exp, loc) =>
          val eVal = Expressions.resolve(exp, taenv, ns0, root)
          mapN(eVal) {
            e => ResolvedAst.Predicate.Body.Guard(e, loc)
          }

        case NamedAst.Predicate.Body.Loop(varSyms, exp, loc) =>
          val eVal = Expressions.resolve(exp, taenv, ns0, root)
          mapN(eVal) {
            e => ResolvedAst.Predicate.Body.Loop(varSyms, e, loc)
          }
      }
    }

  }

  object Params {

    /**
      * Performs name resolution on the given constraint parameter `cparam0` in the given namespace `ns0`.
      */
    def resolve(cparam0: NamedAst.ConstraintParam, ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.ConstraintParam, ResolutionError] = cparam0 match {
      case NamedAst.ConstraintParam.HeadParam(sym, tpe, loc) => ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc).toSuccess
      case NamedAst.ConstraintParam.RuleParam(sym, tpe, loc) => ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc).toSuccess
    }

    /**
      * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
      */
    def resolve(fparam0: NamedAst.FormalParam, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.FormalParam, ResolutionError] = {
      val tVal = resolveType(fparam0.tpe, taenv, ns0, root)
      mapN(tVal) {
        t => ResolvedAst.FormalParam(fparam0.sym, fparam0.mod, t, fparam0.loc)
      }
    }

    /**
      * Performs name resolution on the given predicate parameter `pparam0` in the given namespace `ns0`.
      */
    def resolve(pparam0: NamedAst.PredicateParam, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.PredicateParam, ResolutionError] = pparam0 match {
      case NamedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
        ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc).toSuccess

      case NamedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
        mapN(traverse(tpes)(resolveType(_, taenv, ns0, root))) {
          case ts => ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
        }

    }

    /**
      * Performs name resolution on the given type parameter `tparam0` in the given namespace `ns0`.
      */
    def resolveTparam(tparam0: NamedAst.TypeParam): ResolvedAst.TypeParam = tparam0 match {
      case tparam: NamedAst.TypeParam.Kinded => resolveKindedTparam(tparam)
      case tparam: NamedAst.TypeParam.Unkinded => resolveUnkindedTparam(tparam)
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
  }

  /**
    * Performs name resolution on the given formal parameters `fparams0`.
    */
  def resolveFormalParams(fparams0: List[NamedAst.FormalParam], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.FormalParam], ResolutionError] = {
    traverse(fparams0)(fparam => Params.resolve(fparam, taenv, ns0, root))
  }

  /**
    * Performs name resolution on the given type parameters `tparams0`.
    */
  def resolveTypeParams(tparams0: NamedAst.TypeParams, ns0: Name.NName, root: NamedAst.Root): ResolvedAst.TypeParams = tparams0 match {
    case NamedAst.TypeParams.Kinded(tparams1) =>
      val tparams2 = tparams1.map(Params.resolveKindedTparam)
      ResolvedAst.TypeParams.Kinded(tparams2)
    case NamedAst.TypeParams.Unkinded(tparams1) =>
      val tparams2 = tparams1.map(Params.resolveUnkindedTparam)
      ResolvedAst.TypeParams.Unkinded(tparams2)
  }

  /**
    * Performs name resolution on the given scheme `sc0`.
    */
  def resolveScheme(sc0: NamedAst.Scheme, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Scheme, ResolutionError] = {
    val baseVal = resolveType(sc0.base, taenv, ns0, root)
    val tconstrsVal = sequence(sc0.tconstrs.map(resolveTypeConstraint(_, taenv, ns0, root)))
    mapN(baseVal, tconstrsVal) {
      case (base, tconstrs) => ResolvedAst.Scheme(sc0.quantifiers, tconstrs, base)
    }
  }

  /**
    * Performs name resolution on the given type constraint `tconstr0`.
    */
  def resolveTypeConstraint(tconstr0: NamedAst.TypeConstraint, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.TypeConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TypeConstraint(clazz0, tpe0, loc) =>
      val classVal = lookupClass(clazz0, ns0, root)
      val tpeVal = resolveType(tpe0, taenv, ns0, root)

      mapN(classVal, tpeVal) {
        case (clazz, tpe) =>
          val head = Ast.TypeConstraint.Head(clazz.sym, clazz0.loc)
          ResolvedAst.TypeConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given superclass constraint `tconstr0`.
    */
  def resolveSuperClass(tconstr0: NamedAst.TypeConstraint, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.TypeConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TypeConstraint(clazz0, tpe0, loc) =>
      val classVal = lookupClassForImplementation(clazz0, ns0, root)
      val tpeVal = resolveType(tpe0, taenv, ns0, root)

      mapN(classVal, tpeVal) {
        case (clazz, tpe) =>
          val head = Ast.TypeConstraint.Head(clazz.sym, clazz0.loc)
          ResolvedAst.TypeConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given list of derivations `derives0`.
    */
  def resolveDerivations(qnames: List[Name.QName], ns0: Name.NName, root: NamedAst.Root): Validation[List[Ast.Derivation], ResolutionError] = {
    val derivesVal = Validation.traverse(qnames)(resolveDerivation(_, ns0, root))
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
  def resolveDerivation(derive0: Name.QName, ns0: Name.NName, root: NamedAst.Root): Validation[Ast.Derivation, ResolutionError] = {
    val clazzVal = lookupClass(derive0, ns0, root)
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
  def lookupClassForImplementation(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Class, ResolutionError] = {
    val classOpt = tryLookupClass(qname, ns0, root)
    classOpt match {
      case None => ResolutionError.UndefinedClass(qname, ns0, qname.loc).toFailure
      case Some(clazz) =>
        getClassAccessibility(clazz, ns0) match {
          case Accessibility.Accessible => clazz.toSuccess
          case Accessibility.Sealed => ResolutionError.SealedClass(clazz.sym, ns0, qname.loc).toFailure
          case Accessibility.Inaccessible => ResolutionError.InaccessibleClass(clazz.sym, ns0, qname.loc).toFailure
        }
    }
  }

  /**
    * Finds the class with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupClass(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Class, ResolutionError] = {
    val classOpt = tryLookupClass(qname, ns0, root)
    classOpt match {
      case None => ResolutionError.UndefinedClass(qname, ns0, qname.loc).toFailure
      case Some(clazz) =>
        getClassAccessibility(clazz, ns0) match {
          case Accessibility.Accessible | Accessibility.Sealed => clazz.toSuccess
          case Accessibility.Inaccessible => ResolutionError.InaccessibleClass(clazz.sym, ns0, qname.loc).toFailure
        }
    }
  }

  /**
    * Tries to find a class with the qualified name `qname` in the namespace `ns0`.
    */
  def tryLookupClass(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Class] = {
    // Check whether the name is fully-qualified.
    if (qname.isUnqualified) {
      // Case 1: Unqualified name. Lookup in the current namespace.
      val classOpt = root.classes.getOrElse(ns0, Map.empty).get(qname.ident.name)

      classOpt match {
        case Some(clazz) =>
          // Case 1.2: Found in the current namespace.
          Some(clazz)
        case None =>
          // Case 1.1: Try the global namespace.
          root.classes.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Case 2: Qualified. Lookup in the given namespace.
      root.classes.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  def lookupDefOrSig(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.DefOrSig, ResolutionError] = {
    val defOrSigOpt = tryLookupDefOrSig(qname, ns0, root)

    defOrSigOpt match {
      case None => ResolutionError.UndefinedName(qname, ns0, qname.loc).toFailure
      case Some(d@NamedAst.DefOrSig.Def(defn)) =>
        if (isDefAccessible(defn, ns0)) {
          d.toSuccess
        } else {
          ResolutionError.InaccessibleDef(defn.sym, ns0, qname.loc).toFailure
        }
      case Some(s@NamedAst.DefOrSig.Sig(sig)) =>
        if (isSigAccessible(sig, ns0)) {
          s.toSuccess
        } else {
          ResolutionError.InaccessibleSig(sig.sym, ns0, qname.loc).toFailure
        }
    }
  }

  /**
    * Attempts to lookup the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  def tryLookupDefOrSig(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.DefOrSig] = {
    if (qname.isUnqualified) {
      // Case 1: Unqualified name. Lookup in the current namespace.
      val defnOpt = root.defsAndSigs.getOrElse(ns0, Map.empty).get(qname.ident.name)

      defnOpt match {
        case Some(defn) =>
          // Case 1.2: Found in the current namespace.
          Some(defn)
        case None =>
          // Case 1.1: Try the global namespace.
          root.defsAndSigs.getOrElse(Name.RootNS, Map.empty).get(qname.ident.name)
      }
    } else {
      // Case 2: Qualified. Lookup in the given namespace.
      root.defsAndSigs.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Finds the enum that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  def lookupEnumByTag(qnameOpt: Option[Name.QName], tag: Name.Tag, ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Enum, ResolutionError] = {
    // Determine whether the name is qualified.
    qnameOpt match {
      case None =>
        // Case 1: The name is unqualified.

        // Find all matching enums in the current namespace.
        val namespaceMatches = mutable.Set.empty[NamedAst.Enum]
        for ((enumName, decl) <- root.enums.getOrElse(ns0, Map.empty[Name.Tag, NamedAst.Enum])) {
          for ((enumTag, caze) <- decl.cases) {
            if (tag == enumTag) {
              namespaceMatches += decl
            }
          }
        }

        // Case 1.1.1: Exact match found in the namespace.
        if (namespaceMatches.size == 1) {
          return getEnumIfAccessible(namespaceMatches.head, ns0, tag.loc)
        }

        // Case 1.1.2: Multiple matches found in the namespace.
        if (namespaceMatches.size > 1) {
          val locs = namespaceMatches.map(_.sym.loc).toList.sorted
          return ResolutionError.AmbiguousTag(tag.name, ns0, locs, tag.loc).toFailure
        }

        // Find all matching enums in the root namespace.
        val globalMatches = mutable.Set.empty[NamedAst.Enum]
        for (decls <- root.enums.get(Name.RootNS)) {
          for ((enumName, decl) <- decls) {
            for ((enumTag, caze) <- decl.cases) {
              if (tag == enumTag) {
                globalMatches += decl
              }
            }
          }
        }

        // Case 1.2.1: Exact match found in the root namespace.
        if (globalMatches.size == 1) {
          return getEnumIfAccessible(globalMatches.head, ns0, tag.loc)
        }

        // Case 1.2.2: Multiple matches found in the root namespace.
        if (globalMatches.size > 1) {
          val locs = globalMatches.map(_.sym.loc).toList.sorted
          return ResolutionError.AmbiguousTag(tag.name, ns0, locs, tag.loc).toFailure
        }

        // Case 1.2.3: No match found.
        ResolutionError.UndefinedTag(tag.name, ns0, tag.loc).toFailure

      case Some(qname) =>
        // Case 2: The name is qualified.

        // Determine where to search for the enum.
        val enumsInNS = if (qname.isUnqualified) {
          // The name is unqualified (e.g. Option.None) so search in the current namespace.
          root.enums.getOrElse(ns0, Map.empty[String, NamedAst.Enum])
        } else {
          // The name is qualified (e.g. Foo/Bar/Baz.Qux) so search in the Foo/Bar/Baz namespace.
          root.enums.getOrElse(qname.namespace, Map.empty[String, NamedAst.Enum])
        }

        // Lookup the enum declaration.
        enumsInNS.get(qname.ident.name) match {
          case None =>
            // Case 2.1: The enum does not exist.
            ResolutionError.UndefinedType(qname, ns0, qname.loc).toFailure
          case Some(enumDecl) =>
            // Case 2.2: Enum declaration found. Look for the tag.
            for ((enumTag, caze) <- enumDecl.cases) {
              if (tag == enumTag) {
                // Case 2.2.1: Tag found.
                return getEnumIfAccessible(enumDecl, ns0, tag.loc)
              }
            }

            // Case 2.2.2: No match found.
            ResolutionError.UndefinedTag(tag.name, ns0, tag.loc).toFailure
        }
    }

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
  private def semiResolveType(tpe0: NamedAst.Type, ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[Type, ResolutionError] = tpe0 match {
    case NamedAst.Type.Var(sym, loc) => Type.UnkindedVar(sym, loc).toSuccess

    case NamedAst.Type.Unit(loc) => Type.mkUnit(loc).toSuccess

    case NamedAst.Type.Ambiguous(qname, loc) if qname.isUnqualified => qname.ident.name match {
      // Basic Types
      case "Unit" => Type.mkUnit(loc).toSuccess
      case "Null" => Type.mkNull(loc).toSuccess
      case "Bool" => Type.mkBool(loc).toSuccess
      case "Char" => Type.mkChar(loc).toSuccess
      case "Float32" => Type.mkFloat32(loc).toSuccess
      case "Float64" => Type.mkFloat64(loc).toSuccess
      case "Int8" => Type.mkInt8(loc).toSuccess
      case "Int16" => Type.mkInt16(loc).toSuccess
      case "Int32" => Type.mkInt32(loc).toSuccess
      case "Int64" => Type.mkInt64(loc).toSuccess
      case "BigInt" => Type.mkBigInt(loc).toSuccess
      case "String" => Type.mkString(loc).toSuccess
      case "Channel" => Type.mkChannel(loc).toSuccess
      case "Lazy" => Type.mkLazy(loc).toSuccess
      case "ScopedArray" => Type.Cst(TypeConstructor.ScopedArray, loc).toSuccess
      case "ScopedRef" => Type.Cst(TypeConstructor.ScopedRef, loc).toSuccess
      case "Region" => Type.Cst(TypeConstructor.Region, loc).toSuccess

      // Disambiguate type.
      case typeName =>
        lookupEnumOrTypeAlias(qname, ns0, root) match {
          case EnumOrTypeAliasLookupResult.Enum(enum) => getEnumTypeIfAccessible(enum, ns0, loc)
          case EnumOrTypeAliasLookupResult.TypeAlias(typeAlias) => getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
          case EnumOrTypeAliasLookupResult.NotFound => ResolutionError.UndefinedType(qname, ns0, loc).toFailure
        }
    }

    case NamedAst.Type.Ambiguous(qname, loc) =>
      // Disambiguate type.
      lookupEnumOrTypeAlias(qname, ns0, root) match {
        case EnumOrTypeAliasLookupResult.Enum(enum) => getEnumTypeIfAccessible(enum, ns0, loc)
        case EnumOrTypeAliasLookupResult.TypeAlias(typeAlias) => getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
        case EnumOrTypeAliasLookupResult.NotFound => ResolutionError.UndefinedType(qname, ns0, loc).toFailure
      }

    case NamedAst.Type.Enum(sym, loc) =>
      mkUnkindedEnum(sym, loc).toSuccess

    case NamedAst.Type.Tuple(elms0, loc) =>
      val elmsVal = traverse(elms0)(tpe => semiResolveType(tpe, ns0, root))
      mapN(elmsVal) {
        elms => Type.mkTuple(elms, loc)
      }

    case NamedAst.Type.RecordRowEmpty(loc) =>
      Type.mkRecordRowEmpty(loc).toSuccess

    case NamedAst.Type.RecordRowExtend(field, value, rest, loc) =>
      val vVal = semiResolveType(value, ns0, root)
      val rVal = semiResolveType(rest, ns0, root)
      mapN(vVal, rVal) {
        case (v, r) => Type.mkRecordRowExtend(field, v, r, loc)
      }

    case NamedAst.Type.Record(row, loc) =>
      val rVal = semiResolveType(row, ns0, root)
      mapN(rVal) {
        r => Type.mkRecord(r, loc)
      }

    case NamedAst.Type.SchemaRowEmpty(loc) =>
      Type.mkSchemaRowEmpty(loc).toSuccess

    case NamedAst.Type.SchemaRowExtendWithAlias(qname, targs, rest, loc) =>
      // Lookup the type alias.
      lookupTypeAlias(qname, ns0, root) match {
        case None =>
          // Case 1: The type alias was not found. Report an error.
          ResolutionError.UndefinedName(qname, ns0, loc).toFailure
        case Some(typeAlias) =>
          // Case 2: The type alias was found. Use it.
          val tVal = getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
          val tsVal = traverse(targs)(semiResolveType(_, ns0, root))
          val rVal = semiResolveType(rest, ns0, root)
          mapN(tVal, tsVal, rVal) {
            case (t, ts, r) =>
              val app = Type.mkApply(t, ts, loc)
              Type.mkSchemaRowExtend(Name.mkPred(qname.ident), app, r, loc)
          }
      }

    case NamedAst.Type.SchemaRowExtendWithTypes(ident, den, tpes, rest, loc) =>
      val tsVal = traverse(tpes)(semiResolveType(_, ns0, root))
      val rVal = semiResolveType(rest, ns0, root)
      mapN(tsVal, rVal) {
        case (ts, r) =>
          val pred = mkPredicate(den, ts, loc)
          Type.mkSchemaRowExtend(Name.mkPred(ident), pred, r, loc)
      }

    case NamedAst.Type.Schema(row, loc) =>
      val rVal = semiResolveType(row, ns0, root)
      mapN(rVal) {
        r => Type.mkSchema(r, loc)
      }

    case NamedAst.Type.Relation(tpes, loc) =>
      val tsVal = traverse(tpes)(semiResolveType(_, ns0, root))
      mapN(tsVal) {
        ts => Type.mkRelation(ts, loc)
      }

    case NamedAst.Type.Lattice(tpes, loc) =>
      val tsVal = traverse(tpes)(semiResolveType(_, ns0, root))
      mapN(tsVal) {
        ts => Type.mkLattice(ts, loc)
      }

    case NamedAst.Type.Native(fqn, loc) =>
      fqn match {
        case "java.math.BigInteger" => Type.mkBigInt(loc).toSuccess
        case "java.lang.String" => Type.mkString(loc).toSuccess
        case _ => lookupJvmClass(fqn, loc) map {
          case clazz => Type.mkNative(clazz, loc)
        }
      }

    case NamedAst.Type.Arrow(tparams0, pur0, eff0, tresult0, loc) => // TODO handle eff0
      val tparamsVal = traverse(tparams0)(semiResolveType(_, ns0, root))
      val tresultVal = semiResolveType(tresult0, ns0, root)
      val purVal = semiResolveType(pur0, ns0, root)
      mapN(tparamsVal, tresultVal, purVal) {
        case (tparams, tresult, pur) => Type.mkUncurriedArrowWithEffect(tparams, pur, tresult, loc)
      }

    case NamedAst.Type.Apply(base0, targ0, loc) =>
      val tpe1Val = semiResolveType(base0, ns0, root)
      val tpe2Val = semiResolveType(targ0, ns0, root)
      mapN(tpe1Val, tpe2Val) {
        case (tpe1, tpe2) => Type.Apply(tpe1, tpe2, loc)
      }

    case NamedAst.Type.True(loc) =>
      Type.mkTrue(loc).toSuccess

    case NamedAst.Type.False(loc) =>
      Type.mkFalse(loc).toSuccess

    case NamedAst.Type.Not(tpe, loc) =>
      mapN(semiResolveType(tpe, ns0, root)) {
        case t => Type.mkNot(t, loc)
      }

    case NamedAst.Type.And(tpe1, tpe2, loc) =>
      mapN(semiResolveType(tpe1, ns0, root), semiResolveType(tpe2, ns0, root)) {
        case (t1, t2) => mkAnd(t1, t2, loc)
      }

    case NamedAst.Type.Or(tpe1, tpe2, loc) =>
      mapN(semiResolveType(tpe1, ns0, root), semiResolveType(tpe2, ns0, root)) {
        case (t1, t2) => mkOr(t1, t2, loc)
      }

    case NamedAst.Type.Complement(tpe, loc) =>
      mapN(semiResolveType(tpe, ns0, root)) {
        t => Type.mkNot(t, loc) // TODO change to Complement
      }

    case NamedAst.Type.Union(tpe1, tpe2, loc) =>
      mapN(semiResolveType(tpe1, ns0, root), semiResolveType(tpe2, ns0, root)) {
        case (t1, t2) => mkAnd(t1, t2, loc) // TODO change to Union
      }

    case NamedAst.Type.Intersection(tpe1, tpe2, loc) =>
      mapN(semiResolveType(tpe1, ns0, root), semiResolveType(tpe2, ns0, root)) {
        case (t1, t2) => mkOr(t1, t2, loc) // TODO change to Intersection
      }

    case NamedAst.Type.Difference(tpe1, tpe2, loc) =>
      mapN(semiResolveType(tpe1, ns0, root), semiResolveType(tpe2, ns0, root)) {
        case (t1, t2) => mkOr(t1, t2, loc) // TODO change to Difference
      }

    case _: NamedAst.Type.Read | _: NamedAst.Type.Write =>
      // TODO not handling region effect types yet
      Type.mkTrue(SourceLocation.Unknown).toSuccess

    case NamedAst.Type.Ascribe(tpe, kind, loc) =>
      mapN(semiResolveType(tpe, ns0, root)) {
        t => Type.Ascribe(t, kind, loc)
      }

  }

  /**
    * Finishes resolving the partially resolved type `tpe0`.
    *
    * Replaces type alias placeholders with the real type aliases.
    */
  private def finishResolveType(tpe0: Type, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias]): Validation[Type, ResolutionError] = {

    /**
      * Performs beta-reduction on the given type alias.
      * The list of arguments must be the same length as the alias's parameters.
      */
    def applyAlias(alias: ResolvedAst.TypeAlias, args: List[Type], cstLoc: SourceLocation): Type = {
      val map = alias.tparams.tparams.map(_.sym).zip(args).toMap[Symbol.TypeVarSym, Type]
      val subst = Substitution(map)
      val tpe = subst(alias.tpe)
      val cst = Type.AliasConstructor(alias.sym, cstLoc)
      Type.Alias(cst, args, tpe, tpe0.loc)
    }

    val baseType = tpe0.baseType
    val targs = tpe0.typeArguments

    baseType match {
      case Type.Cst(TypeConstructor.UnappliedAlias(sym), loc) =>
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
              Type.mkApply(applyAlias(alias, usedArgs, loc), extraArgs, tpe0.loc)
          }
        }
      case _ =>
        traverse(targs)(finishResolveType(_, taenv)) map {
          resolvedArgs => Type.mkApply(baseType, resolvedArgs, tpe0.loc)
        }
    }
  }

  /**
    * Performs name resolution on the given type `tpe0` in the given namespace `ns0`.
    */
  def resolveType(tpe0: NamedAst.Type, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[Type, ResolutionError] = {
    val tVal = semiResolveType(tpe0, ns0, root)
    flatMapN(tVal) {
      t => finishResolveType(t, taenv)
    }
  }

  /**
    * The result of looking up an ambiguous type.
    */
  private sealed trait EnumOrTypeAliasLookupResult {
    /**
      * Returns `other` if this result is [[EnumOrTypeAliasLookupResult.NotFound]].
      *
      * Otherwise, returns this result.
      */
    def orElse(other: => EnumOrTypeAliasLookupResult): EnumOrTypeAliasLookupResult = this match {
      case res: EnumOrTypeAliasLookupResult.Enum => res
      case res: EnumOrTypeAliasLookupResult.TypeAlias => res
      case EnumOrTypeAliasLookupResult.NotFound => other
    }
  }

  private object EnumOrTypeAliasLookupResult {
    /**
      * The result is an enum.
      */
    case class Enum(enum0: NamedAst.Enum) extends EnumOrTypeAliasLookupResult

    /**
      * The result is a type alias.
      */
    case class TypeAlias(typeAlias: NamedAst.TypeAlias) extends EnumOrTypeAliasLookupResult

    /**
      * The type cannot be found.
      */
    case object NotFound extends EnumOrTypeAliasLookupResult
  }

  /**
    * Looks up the ambiguous type.
    */
  private def lookupEnumOrTypeAlias(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): EnumOrTypeAliasLookupResult = {

    /**
      * Looks up the type in the given namespace.
      */
    def lookupIn(ns: Name.NName): EnumOrTypeAliasLookupResult = {
      val enumsInNamespace = root.enums.getOrElse(ns, Map.empty)
      val aliasesInNamespace = root.typeAliases.getOrElse(ns, Map.empty)
      (enumsInNamespace.get(qname.ident.name), aliasesInNamespace.get(qname.ident.name)) match {
        case (None, None) =>
          // Case 1: name not found
          EnumOrTypeAliasLookupResult.NotFound
        case (Some(enum), None) =>
          // Case 2: found an enum
          EnumOrTypeAliasLookupResult.Enum(enum)
        case (None, Some(alias)) =>
          // Case 3: found a type alias
          EnumOrTypeAliasLookupResult.TypeAlias(alias)
        case (Some(_), Some(_)) =>
          // Case 4: found both -- error
          throw InternalCompilerException("Unexpected ambiguity: Duplicate types / classes should have been resolved.")
      }
    }

    if (qname.isUnqualified) {
      // Case 1: The name is unqualified. Lookup in the current namespace.
      lookupIn(ns0).orElse {
        // Case 1.1: The name was not found in the current namespace. Try the root namespace.
        lookupIn(Name.RootNS)
      }
    } else {
      // Case 2: The name is qualified. Look it up in its namespace.
      lookupIn(qname.namespace)
    }
  }

  /**
    * Optionally returns the enum with the given `name` in the given namespace `ns0`.
    */
  private def lookupEnum(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.Enum] = {
    if (qname.isUnqualified) {
      // Case 1: The name is unqualified. Lookup in the current namespace.
      val enumsInNamespace = root.enums.getOrElse(ns0, Map.empty)
      enumsInNamespace.get(qname.ident.name) orElse {
        // Case 1.1: The name was not found in the current namespace. Try the root namespace.
        val enumsInRootNS = root.enums.getOrElse(Name.RootNS, Map.empty)
        enumsInRootNS.get(qname.ident.name)
      }
    } else {
      // Case 2: The name is qualified. Look it up in its namespace.
      root.enums.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Optionally returns the type alias with the given `name` in the given namespace `ns0`.
    */
  private def lookupTypeAlias(qname: Name.QName, ns0: Name.NName, root: NamedAst.Root): Option[NamedAst.TypeAlias] = {
    if (qname.isUnqualified) {
      // Case 1: The name is unqualified. Lookup in the current namespace.
      val typeAliasesInNamespace = root.typeAliases.getOrElse(ns0, Map.empty)
      typeAliasesInNamespace.get(qname.ident.name) orElse {
        // Case 1.1: The name was not found in the current namespace. Try the root namespace.
        val typeAliasesInRootNS = root.typeAliases.getOrElse(Name.RootNS, Map.empty)
        typeAliasesInRootNS.get(qname.ident.name)
      }
    } else {
      // Case 2: The name is qualified. Look it up in its namespace.
      root.typeAliases.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
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
  private def getClassAccessibility(class0: NamedAst.Class, ns0: Name.NName): Accessibility = {

    val classNs = class0.sym.namespace
    val accessingNs = ns0.idents.map(_.name)

    if (classNs == accessingNs) {
      // Case 1: We're in the same namespace: Accessible
      Accessibility.Accessible
    } else if (!class0.mod.isPublic && !accessingNs.startsWith(classNs)) {
      // Case 2: The class is private and we're in unrelated namespaces: Inaccessible
      Accessibility.Inaccessible
    } else if (class0.mod.isSealed) {
      // Case 3: The class is accessible but sealed
      Accessibility.Sealed
    } else {
      // Case 4: The class is otherwise accessible
      Accessibility.Accessible
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
  def isDefAccessible(defn0: NamedAst.Def, ns0: Name.NName): Boolean = {
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
  def isSigAccessible(sig0: NamedAst.Sig, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (sig0.spec.mod.isPublic) // MATT check instance availability instead?
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
    * Successfully returns the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  def getEnumIfAccessible(enum0: NamedAst.Enum, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Enum, ResolutionError] = {
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
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getEnumTypeIfAccessible(enum0: NamedAst.Enum, ns0: Name.NName, loc: SourceLocation): Validation[Type, ResolutionError] =
    getEnumIfAccessible(enum0, ns0, loc) map {
      case enum => mkUnkindedEnum(enum.sym, loc)
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
  private def getTypeAliasIfAccessible(alia0: NamedAst.TypeAlias, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.TypeAlias, ResolutionError] = {
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
  private def getTypeAliasTypeIfAccessible(alia0: NamedAst.TypeAlias, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[Type, ResolutionError] = {
    getTypeAliasIfAccessible(alia0, ns0, loc) map {
      alias => mkUnappliedTypeAlias(alias.sym, loc)
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
  }

  /**
    * Returns the constructor reflection object for the given `className` and `signature`.
    */
  private def lookupJvmConstructor(className: String, signature: List[Type], loc: SourceLocation)(implicit flix: Flix): Validation[Constructor[_], ResolutionError] = {
    // Lookup the class and signature.
    flatMapN(lookupJvmClass(className, loc), lookupSignature(signature, loc)) {
      case (clazz, sig) => try {
        // Lookup the constructor with the appropriate signature.
        clazz.getConstructor(sig: _*).toSuccess
      } catch {
        case ex: ClassNotFoundException => ResolutionError.UndefinedJvmClass(className, loc).toFailure
        case ex: NoSuchMethodException => ResolutionError.UndefinedJvmConstructor(className, sig, clazz.getConstructors.toList, loc).toFailure
      }
    }
  }

  /**
    * Returns the method reflection object for the given `className`, `methodName`, and `signature`.
    */
  private def lookupJvmMethod(className: String, methodName: String, signature: List[Type], static: Boolean, loc: SourceLocation)(implicit flix: Flix): Validation[Method, ResolutionError] = {
    // Lookup the class and signature.
    flatMapN(lookupJvmClass(className, loc), lookupSignature(signature, loc)) {
      case (clazz, sig) => try {
        // Lookup the method with the appropriate signature.
        val method = clazz.getDeclaredMethod(methodName, sig: _*)

        // Check if the method should be and is static.
        if (static == Modifier.isStatic(method.getModifiers))
          method.toSuccess
        else
          throw new NoSuchMethodException()
      } catch {
        case ex: NoSuchMethodException =>
          val candidateMethods = clazz.getMethods.filter(m => m.getName == methodName).toList
          ResolutionError.UndefinedJvmMethod(className, methodName, static, sig, candidateMethods, loc).toFailure
      }
    }
  }

  /**
    * Returns the field reflection object for the given `className` and `fieldName`.
    */
  private def lookupJvmField(className: String, fieldName: String, static: Boolean, loc: SourceLocation)(implicit flix: Flix): Validation[Field, ResolutionError] = {
    flatMapN(lookupJvmClass(className, loc)) {
      case clazz => try {
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
          ResolutionError.UndefinedJvmField(className, fieldName, static, candidateFields, loc).toFailure
      }
    }
  }

  /**
    * Performs name resolution on the given `signature`.
    */
  private def lookupSignature(signature: List[Type], loc: SourceLocation)(implicit flix: Flix): Validation[List[Class[_]], ResolutionError] = {
    traverse(signature)(getJVMType(_, loc))
  }

  /**
    * Returns the JVM type corresponding to the given Flix type `tpe`.
    *
    * A non-primitive Flix type is mapped to java.lang.Object.
    *
    * An array type is mapped to the corresponding array type.
    */
  private def getJVMType(tpe: Type, loc: SourceLocation)(implicit flix: Flix): Validation[Class[_], ResolutionError] = Type.eraseAliases(tpe).typeConstructor match {
    case None =>
      ResolutionError.IllegalType(tpe, loc).toFailure

    case Some(tc) => tc match {
      case TypeConstructor.Unit => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Bool => classOf[Boolean].toSuccess

      case TypeConstructor.Char => classOf[Char].toSuccess

      case TypeConstructor.Float32 => classOf[Float].toSuccess

      case TypeConstructor.Float64 => classOf[Double].toSuccess

      case TypeConstructor.Int8 => classOf[Byte].toSuccess

      case TypeConstructor.Int16 => classOf[Short].toSuccess

      case TypeConstructor.Int32 => classOf[Int].toSuccess

      case TypeConstructor.Int64 => classOf[Long].toSuccess

      case TypeConstructor.BigInt => Class.forName("java.math.BigInteger").toSuccess

      case TypeConstructor.Str => Class.forName("java.lang.String").toSuccess

      case TypeConstructor.Channel => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.KindedEnum(_, _) => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.UnkindedEnum(_) => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.ScopedRef => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.Tuple(_) => Class.forName("java.lang.Object").toSuccess

      case TypeConstructor.ScopedArray =>
        Type.eraseAliases(tpe).typeArguments match {
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

      case _ => ResolutionError.IllegalType(tpe, loc).toFailure
    }
  }

  /**
    * Returns a synthetic namespace obtained from the given sequence of namespace `parts`.
    */
  private def getNS(parts: List[String]): Name.NName = {
    val sp1 = SourcePosition.Unknown
    val sp2 = SourcePosition.Unknown
    val idents = parts.map(s => Name.Ident(sp1, s, sp2))
    Name.NName(sp1, idents, sp2)
  }

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkUnkindedEnum(sym: Symbol.EnumSym, loc: SourceLocation): Type = Type.Cst(TypeConstructor.UnkindedEnum(sym), loc)

  /**
    * Construct the enum type `Sym[ts]`.
    */
  def mkUnkindedEnum(sym: Symbol.EnumSym, ts: List[Symbol.UnkindedTypeVarSym], loc: SourceLocation): Type = {
    val args = ts.map(sym => Type.UnkindedVar(sym, sym.loc))
    Type.mkApply(Type.Cst(TypeConstructor.UnkindedEnum(sym), loc), args, loc)
  }

  /**
    * Construct the type alias type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkUnappliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation): Type = Type.Cst(TypeConstructor.UnappliedAlias(sym), loc)

  /**
    * Constructs a predicate type.
    */
  private def mkPredicate(den: Ast.Denotation, ts0: List[Type], loc: SourceLocation): Type = {
    val tycon = den match {
      case Denotation.Relational => Type.Cst(TypeConstructor.Relation, loc)
      case Denotation.Latticenal => Type.Cst(TypeConstructor.Lattice, loc)
    }
    val ts = ts0 match {
      case Nil => Type.mkUnit(loc)
      case x :: Nil => x
      case xs => Type.mkTuple(xs, loc)
    }

    Type.Apply(tycon, ts, loc)
  }

  /**
    * Returns the type `And(tpe1, tpe2)`.
    */
  private def mkAnd(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, loc), tpe1, loc), tpe2, loc)

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  private def mkOr(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, loc), tpe1, loc), tpe2, loc)

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
          ResolvedAst.Expression.Var(sym, sym.tvar, sym.loc)
        case None =>
          // Case 2.2: Use the global region.
          val tpe = Type.mkRegion(Type.False, loc)
          ResolvedAst.Expression.Region(tpe, loc)
      }
  }

  /**
    * Enum describing the extent to which a class is accessible.
    */
  private sealed trait Accessibility

  private object Accessibility {
    case object Accessible extends Accessibility

    case object Sealed extends Accessibility

    case object Inaccessible extends Accessibility
  }
}
