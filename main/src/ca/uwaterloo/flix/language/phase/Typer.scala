/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Denotation, Stratification}
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.InferMonad.{seqM, traverseM}
import ca.uwaterloo.flix.language.phase.unification.TypeMinimization.minimizeScheme
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, mapN, traverse}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListOps.unzip4

import java.io.PrintWriter
import scala.annotation.tailrec

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, CompilationMessage] = flix.phase("Typer") {
    val classEnv = mkClassEnv(root.classes, root.instances)
    val classesVal = visitClasses(root, classEnv, oldRoot, changeSet)
    val instancesVal = visitInstances(root, classEnv)
    val defsVal = visitDefs(root, classEnv, oldRoot, changeSet)
    val enums = visitEnums(root)
    val effsVal = visitEffs(root)
    val typeAliases = visitTypeAliases(root)

    Validation.mapN(classesVal, instancesVal, defsVal, effsVal) {
      case (classes, instances, defs, effs) =>
        val sigs = classes.values.flatMap(_.signatures).map(sig => sig.sym -> sig).toMap
        val modules = collectModules(root)
        TypedAst.Root(modules, classes, instances, sigs, defs, enums, effs, typeAliases, root.uses, root.entryPoint, root.sources, classEnv, root.names)
    }
  }

  /**
    * Collects the symbols in the given root into a map.
    */
  private def collectModules(root: KindedAst.Root): Map[Symbol.ModuleSym, List[Symbol]] = root match {
    case KindedAst.Root(classes, _, defs, enums, effects, typeAliases, _, _, _, loc) =>
      val sigs = classes.values.flatMap { clazz => clazz.sigs.values.map(_.sym) }
      val ops = effects.values.flatMap { eff => eff.ops.map(_.sym) }

      val syms = classes.keys ++ defs.keys ++ enums.keys ++ effects.keys ++ typeAliases.keys ++ sigs ++ ops

      val groups = syms.groupBy {
        case sym: Symbol.DefnSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.RestrictableEnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.ClassSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.TypeAliasSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EffectSym => new Symbol.ModuleSym(sym.namespace)

        case sym: Symbol.SigSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)
        case sym: Symbol.OpSym => new Symbol.ModuleSym(sym.eff.namespace :+ sym.eff.name)

        case sym: Symbol.CaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.RestrictableCaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.ModuleSym => throw InternalCompilerException(s"unexpected symbol: $sym", SourceLocation.Unknown)
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
    * Creates a class environment from a the classes and instances in the root.
    */
  private def mkClassEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): Map[Symbol.ClassSym, Ast.ClassContext] =
    flix.subphase("ClassEnv") {
      classes0.map {
        case (classSym, clazz) =>
          val instances = instances0.getOrElse(classSym, Nil)
          val envInsts = instances.map {
            case KindedAst.Instance(_, _, _, _, tpe, tconstrs, _, _, _) => Ast.Instance(tpe, tconstrs)
          }
          // ignore the super class parameters since they should all be the same as the class param
          val superClasses = clazz.superClasses.map(_.head.sym)
          (classSym, Ast.ClassContext(superClasses, envInsts))
      }
    }

  /**
    * Performs type inference and reassembly on all classes in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitClasses(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.ClassSym, TypedAst.Class], TypeError] =
    flix.subphase("Classes") {
      // Compute the stale and fresh classes.
      val (staleClasses, freshClasses) = changeSet.partition(root.classes, oldRoot.classes)

      // Process the stale classes in parallel.
      val results = ParOps.parMap(staleClasses.values)(visitClass(_, root, classEnv))

      // Sequence the results using the freshClasses as the initial value.
      Validation.sequence(results) map {
        case xs => xs.foldLeft(freshClasses) {
          case (acc, clazzPair) => acc + clazzPair
        }
      }
    }

  /**
    * Reassembles a single class.
    */
  private def visitClass(clazz: KindedAst.Class, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[(Symbol.ClassSym, TypedAst.Class), TypeError] = clazz match {
    case KindedAst.Class(doc, ann, mod, sym, tparam, superClasses, sigs0, laws0, loc) =>
      val tparams = getTypeParams(List(tparam))
      val tconstr = Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), Type.Var(tparam.sym, tparam.loc), sym.loc)
      val sigsVal = traverse(sigs0.values)(visitSig(_, List(tconstr), root, classEnv))
      val lawsVal = traverse(laws0)(visitDefn(_, List(tconstr), root, classEnv))
      mapN(sigsVal, lawsVal) {
        case (sigs, laws) => (sym, TypedAst.Class(doc, ann, mod, sym, tparams.head, superClasses, sigs, laws, loc))
      }
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Map[Symbol.ClassSym, List[TypedAst.Instance]], TypeError] =
    flix.subphase("Instances") {
      val results = ParOps.parMap(root.instances.values.flatten)(visitInstance(_, root, classEnv))
      Validation.sequence(results) map {
        insts => insts.groupBy(inst => inst.clazz.sym)
      }
    }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Instance, TypeError] = inst match {
    case KindedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, defs0, ns, loc) =>
      val defsVal = traverse(defs0)(visitDefn(_, tconstrs, root, classEnv))
      mapN(defsVal) {
        case defs => TypedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, defs, ns, loc)
      }
  }

  /**
    * Performs type inference and reassembly on all effects in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitEffs(root: KindedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.EffectSym, TypedAst.Effect], TypeError] = {
    val results = ParOps.parMap(root.effects.values)(visitEff(_, root))

    // Sequence the results using the freshDefs as the initial value.
    Validation.sequence(results) map {
      case xs => xs.foldLeft(Map.empty[Symbol.EffectSym, TypedAst.Effect]) {
        case (acc, eff) => acc + (eff.sym -> eff)
      }
    }
  }

  /**
    * Performs type inference and reassembly on the given effect `eff`.
    */
  private def visitEff(eff: KindedAst.Effect, root: KindedAst.Root)(implicit flix: Flix): Validation[TypedAst.Effect, TypeError] = eff match {
    case KindedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val opsVal = traverse(ops0)(visitOp(_, root))
      mapN(opsVal) {
        case (ops) => TypedAst.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs type inference and reassembly on the given definition `defn`.
    */
  private def visitDefn(defn: KindedAst.Def, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = defn match {
    case KindedAst.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      for {
        // check the main signature before typechecking the def
        res <- typeCheckDecl(spec0, exp0, assumedTconstrs, root, classEnv, sym.loc)
        (spec, exp) = res
      } yield TypedAst.Def(sym, spec, exp)
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: KindedAst.Sig, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Sig, TypeError] = sig match {
    case KindedAst.Sig(sym, spec0, Some(exp0)) =>
      typeCheckDecl(spec0, exp0, assumedTconstrs, root, classEnv, sym.loc) map {
        case (spec, exp) => TypedAst.Sig(sym, spec, Some(exp))
      }
    case KindedAst.Sig(sym, spec0, None) =>
      val spec = visitSpec(spec0, root, Substitution.empty)
      TypedAst.Sig(sym, spec, None).toSuccess
  }

  /**
    * Performs type inference and reassembly on the given effect operation `op`
    */
  private def visitOp(op: KindedAst.Op, root: KindedAst.Root)(implicit flix: Flix): Validation[TypedAst.Op, TypeError] = op match {
    case KindedAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0, root, Substitution.empty)
      TypedAst.Op(sym, spec).toSuccess
  }

  /**
    * Performs type inference and reassembly on the given Spec `spec`.
    */
  private def visitSpec(spec: KindedAst.Spec, root: KindedAst.Root, subst: Substitution)(implicit flix: Flix): TypedAst.Spec = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc, tpe, pur, eff, tconstrs, loc) =>
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst)
      TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, pur, eff, tconstrs, loc)
  }

  /**
    * Performs type inference and reassembly on all definitions in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitDefs(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] =
    flix.subphase("Defs") {
      // Compute the stale and fresh definitions.
      val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)

      // println(s"Stale = ${staleDefs.keySet}")
      // println(s"Fresh = ${freshDefs.keySet.size}")

      // Process the stale defs in parallel.
      val results = ParOps.parMap(staleDefs.values)(visitDefn(_, Nil, root, classEnv))

      // Sequence the results using the freshDefs as the initial value.
      Validation.sequence(results) map {
        case xs => xs.foldLeft(freshDefs) {
          case (acc, defn) => acc + (defn.sym -> defn)
        }
      }
    }

  /**
    * Infers the type of the given definition `defn0`.
    */
  private def typeCheckDecl(spec0: KindedAst.Spec, exp0: KindedAst.Expression, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], loc: SourceLocation)(implicit flix: Flix): Validation[(TypedAst.Spec, TypedAst.Impl), TypeError] = spec0 match {
    case KindedAst.Spec(_, _, _, _, fparams0, sc, _, _, _, _, _) =>

      ///
      /// Infer the type of the expression `exp0`.
      ///
      val result = for {
        (inferredConstrs, inferredTyp, inferredPur, inferredEff) <- inferExp(exp0, root)
      } yield (inferredConstrs, Type.mkUncurriedArrowWithEffect(fparams0.map(_.tpe), inferredPur, inferredEff, inferredTyp, loc)) // TODO use eff


      // Add the assumed constraints to the declared scheme
      val declaredScheme = sc.copy(constraints = sc.constraints ++ assumedTconstrs)

      ///
      /// Pattern match on the result to determine if type inference was successful.
      ///
      result match {
        case InferMonad(run) =>

          ///
          /// NB: We *DO NOT* run the type inference under the empty environment (as you would expect).
          /// Instead, we pre-populate the environment with the types from the formal parameters.
          /// This is required because we have expressions such as `x + y` where we must know the type of `x`
          /// (or y) to determine the type of floating-point or integer operations.
          ///
          val initialSubst = getSubstFromParams(fparams0)
          val initialRenv = getRigidityFromParams(fparams0)

          run(initialSubst, initialRenv) match {
            case Ok((subst, renv0, (partialTconstrs, partialType))) =>

              ///
              /// The partial type returned by the inference monad does not have the substitution applied.
              ///
              val (inferredConstrs, inferredType) = (partialTconstrs.map(subst.apply), subst(partialType))

              ///
              /// Check that the inferred type is at least as general as the declared type.
              ///
              /// NB: Because the inferredType is always a function type, the purect is always implicitly accounted for.
              ///
              val inferredSc = Scheme.generalize(inferredConstrs, inferredType)
              Scheme.checkLessThanEqual(inferredSc, declaredScheme, classEnv) match {
                // Case 1: no errors, continue
                case Validation.Success(_) => // noop
                case failure =>
                  val instanceErrs = failure.errors.collect {
                    case UnificationError.NoMatchingInstance(tconstr) =>
                      tconstr.arg.typeConstructor match {
                        case Some(tc: TypeConstructor.Arrow) =>
                          TypeError.MissingArrowInstance(tconstr.head.sym, tconstr.arg, renv0, tconstr.loc)
                        case _ =>
                          if (tconstr.head.sym.name == "Eq")
                            TypeError.MissingEq(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "Order")
                            TypeError.MissingOrder(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "ToString")
                            TypeError.MissingToString(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "Sendable")
                            TypeError.MissingSendable(tconstr.arg, renv0, tconstr.loc)
                          else
                            TypeError.MissingInstance(tconstr.head.sym, tconstr.arg, renv0, tconstr.loc)
                      }
                  }
                  // Case 2: non instance error
                  if (instanceErrs.isEmpty) {
                    //
                    // Determine the most precise type error to emit.
                    //
                    val inferredPur = inferredSc.base.arrowPurityType
                    val declaredPur = declaredScheme.base.arrowPurityType

                    if (declaredPur == Type.Pure && inferredPur == Type.Impure) {
                      // Case 1: Declared as pure, but impure.
                      return TypeError.ImpureDeclaredAsPure(loc).toFailure
                    } else if (declaredPur == Type.Pure && inferredPur != Type.Pure) {
                      // Case 2: Declared as pure, but purity polymorphic.
                      return TypeError.EffectPolymorphicDeclaredAsPure(inferredPur, loc).toFailure
                    } else {
                      // Case 3: Check if it is the purity that cannot be generalized.
                      val inferredPurScheme = Scheme(inferredSc.quantifiers, Nil, inferredPur)
                      val declaredPurScheme = Scheme(declaredScheme.quantifiers, Nil, declaredPur)
                      Scheme.checkLessThanEqual(inferredPurScheme, declaredPurScheme, classEnv) match {
                        case Validation.Success(_) =>
                        // Case 3.1: The purity is not the problem. Regular generalization error.
                        // Fall through to below.

                        case _failure =>
                          // Case 3.2: The purity cannot be generalized.
                          return TypeError.EffectGeneralizationError(declaredPur, inferredPur, loc).toFailure
                      }

                      return TypeError.GeneralizationError(declaredScheme, minimizeScheme(inferredSc), loc).toFailure
                    }
                  } else {
                    // Case 3: instance error
                    return Validation.Failure(instanceErrs)
                  }
              }

              ///
              /// Compute the expression, type parameters, and formal parameters with the substitution applied everywhere.
              ///
              val exp = reassembleExp(exp0, root, subst)
              val spec = visitSpec(spec0, root, subst)

              ///
              /// Compute a type scheme that matches the type variables that appear in the expression body.
              ///
              /// NB: It is very important to understand that: The type scheme a function is declared with must match the inferred type scheme.
              /// However, we require an even stronger property for the implementation to work. The inferred type scheme used in the rest of the
              /// compiler must *use the same type variables* in the scheme as in the body expression. Otherwise monomorphization et al. will break.
              ///
              val finalInferredType = subst(partialType)
              val finalInferredTconstrs = partialTconstrs.map(subst.apply)
              val inferredScheme = Scheme(finalInferredType.typeVars.toList.map(_.sym), finalInferredTconstrs, finalInferredType)

              (spec, TypedAst.Impl(exp, inferredScheme)).toSuccess

            case Err(e) => Validation.Failure(LazyList(e))
          }
      }
  }

  /**
    * Performs type inference and reassembly on all enums in the given AST root.
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
    * Performs type resolution on the given enum and its cases.
    */
  private def visitEnum(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.EnumSym, TypedAst.Enum) = enum0 match {
    case KindedAst.Enum(doc, ann, mod, enumSym, tparams0, derives, cases0, tpe, loc) =>
      val tparams = getTypeParams(tparams0)
      val cases = cases0 map {
        case (name, KindedAst.Case(caseSym, tagType, sc)) =>
          name -> TypedAst.Case(caseSym, tagType, sc, caseSym.loc) // TODO this should be full case location
      }

      enumSym -> TypedAst.Enum(doc, ann, mod, enumSym, tparams, derives, cases, tpe, loc)
  }

  /**
    * Performs typing on the type aliases in the given `root`.
    */
  private def visitTypeAliases(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] =
    flix.subphase("TypeAliases") {
      def visitTypeAlias(alias: KindedAst.TypeAlias): (Symbol.TypeAliasSym, TypedAst.TypeAlias) = alias match {
        case KindedAst.TypeAlias(doc, mod, sym, tparams0, tpe, loc) =>
          val tparams = getTypeParams(tparams0)
          sym -> TypedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }

      root.typeAliases.values.map(visitTypeAlias).toMap
    }

  /**
    * Infers the type of the given expression `exp0`.
    */
  private def inferExp(exp0: KindedAst.Expression, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: KindedAst.Expression): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = e0 match {

      case KindedAst.Expression.Wild(tvar, _) =>
        liftM(List.empty, tvar, Type.Pure, Type.Empty)

      case KindedAst.Expression.Var(sym, loc) =>
        liftM(List.empty, sym.tvar, Type.Pure, Type.Empty)

      case KindedAst.Expression.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs0, defType) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
        for {
          resultTyp <- unifyTypeM(tvar, defType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure, Type.Empty)

      case KindedAst.Expression.Sig(sym, tvar, loc) =>
        // find the declared signature corresponding to this symbol
        val sig = root.classes(sym.clazz).sigs(sym)
        val (tconstrs0, sigType) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
        for {
          resultTyp <- unifyTypeM(tvar, sigType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure, Type.Empty)

      case KindedAst.Expression.Hole(_, tvar, _) =>
        liftM(List.empty, tvar, Type.Pure, Type.Empty)

      case KindedAst.Expression.HoleWithExp(exp, tvar, pvar, evar, loc) =>
        for {
          (tconstrs, tpe, pur, eff) <- visitExp(exp)
          // result type is whatever is needed for the hole
          resultTpe = tvar
          // purity/effect type is AT LEAST the inner expression's purity/effect
          atLeastPur = Type.mkAnd(pur, Type.freshVar(Kind.Bool, loc.asSynthetic), loc.asSynthetic)
          resultPur <- unifyTypeM(atLeastPur, pvar, loc)
          atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Effect, loc.asSynthetic), loc.asSynthetic)
          resultEff <- unifyTypeM(atLeastEff, evar, loc)
        } yield (tconstrs, resultTpe, resultPur, resultEff)

      case KindedAst.Expression.Use(_, exp, _) => visitExp(exp)

      case KindedAst.Expression.Cst(Ast.Constant.Unit, loc) =>
        liftM(List.empty, Type.mkUnit(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Null, loc) =>
        liftM(List.empty, Type.mkNull(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Bool(_), loc) =>
        liftM(List.empty, Type.mkBool(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Char(_), loc) =>
        liftM(List.empty, Type.mkChar(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Float32(_), loc) =>
        liftM(List.empty, Type.mkFloat32(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Float64(_), loc) =>
        liftM(List.empty, Type.mkFloat64(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.BigDecimal(_), loc) =>
        liftM(List.empty, Type.mkBigDecimal(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Int8(_), loc) =>
        liftM(List.empty, Type.mkInt8(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Int16(_), loc) =>
        liftM(List.empty, Type.mkInt16(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Int32(_), loc) =>
        liftM(List.empty, Type.mkInt32(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Int64(_), loc) =>
        liftM(List.empty, Type.mkInt64(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.BigInt(_), loc) =>
        liftM(List.empty, Type.mkBigInt(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Cst(Ast.Constant.Str(_), loc) =>
        liftM(List.empty, Type.mkString(loc.asSynthetic), Type.Pure, Type.Empty)

      case KindedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val argType = fparam.tpe
        val argTypeVar = fparam.sym.tvar
        for {
          (constrs, bodyType, bodyPur, bodyEff) <- visitExp(exp)
          _ <- unifyTypeM(argType, argTypeVar, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArrowWithEffect(argType, bodyPur, bodyEff, bodyType, loc), loc)
        } yield (constrs, resultTyp, Type.Pure, Type.Empty)

      case KindedAst.Expression.Apply(exp, exps, tvar, pvar, evar, loc) =>
        //
        // Determine if there is a direct call to a Def or Sig.
        //
        val knownTarget = exp match {
          case KindedAst.Expression.Def(sym, tvar2, loc2) =>
            // Case 1: Lookup the sym and instantiate its scheme.
            val defn = root.defs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(defn.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case KindedAst.Expression.Sig(sym, tvar2, loc2) =>
            // Case 2: Lookup the sym and instantiate its scheme.
            val sig = root.classes(sym.clazz).sigs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(sig.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case _ =>
            // Case 3: Unknown target.
            None
        }

        knownTarget match {
          case Some((sym, tvar2, constrs1, declaredType)) =>
            //
            // Special Case: We are applying a Def or Sig and we break apart its declared type.
            //
            val declaredPur = declaredType.typeArguments.head
            val declaredEff = declaredType.typeArguments.drop(1).head
            val declaredArgumentTypes = declaredType.typeArguments.drop(2).dropRight(1)
            val declaredResultType = declaredType.typeArguments.last

            for {
              (constrs2, tpes, purs, effs) <- traverseM(exps)(visitExp).map(unzip4)
              _ <- expectTypeArguments(sym, declaredArgumentTypes, tpes, exps.map(_.loc), loc)
              _ <- unifyTypeM(tvar2, declaredType, loc)
              // The below line should not be needed, but it seems it is.
              _ <- expectTypeM(tvar2, Type.mkUncurriedArrowWithEffect(tpes, declaredPur, declaredEff, declaredResultType, loc), loc)
              resultTyp <- unifyTypeM(tvar, declaredResultType, loc)
              resultPur <- unifyBoolM(pvar, Type.mkAnd(declaredPur :: purs, loc), loc)
              resultEff <- unifyTypeM(evar, Type.mkUnion(declaredEff :: effs, loc), loc)
            } yield (constrs1 ++ constrs2.flatten, resultTyp, resultPur, resultEff)

          case None =>
            //
            // Default Case: Apply.
            //
            val lambdaBodyType = Type.freshVar(Kind.Star, loc)
            val lambdaBodyPur = Type.freshVar(Kind.Bool, loc)
            val lambdaBodyEff = Type.freshVar(Kind.Effect, loc)
            for {
              (constrs1, tpe, pur, eff) <- visitExp(exp)
              (constrs2, tpes, purs, effs) <- traverseM(exps)(visitExp).map(unzip4)
              _ <- expectTypeM(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyPur, lambdaBodyEff, lambdaBodyType, loc), loc)
              resultTyp <- unifyTypeM(tvar, lambdaBodyType, loc)
              resultPur <- unifyBoolM(pvar, Type.mkAnd(lambdaBodyPur :: pur :: purs, loc), loc)
              resultEff <- unifyTypeM(evar, Type.mkUnion(lambdaBodyEff :: eff :: effs, loc), loc)
              _ <- unbindVar(lambdaBodyType) // NB: Safe to unbind since the variable is not used elsewhere.
              _ <- unbindVar(lambdaBodyPur) // NB: Safe to unbind since the variable is not used elsewhere.
            } yield (constrs1 ++ constrs2.flatten, resultTyp, resultPur, resultEff)
        }

      case KindedAst.Expression.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOperator.BoolOp.Not =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Bool, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.Float32Op.Neg =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float32, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.Float64Op.Neg =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float64, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.BigDecimalOp.Neg =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.BigDecimal, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int8Op.Neg | SemanticOperator.Int8Op.Not =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int8, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int16Op.Neg | SemanticOperator.Int16Op.Not =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int16, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int32Op.Neg | SemanticOperator.Int32Op.Not =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int32, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int64Op.Neg | SemanticOperator.Int64Op.Not =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int64, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case SemanticOperator.BigIntOp.Neg | SemanticOperator.BigIntOp.Not =>
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.BigInt, actual = tpe, bind = tvar, exp.loc)
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", loc)
      }

      case KindedAst.Expression.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOperator.BoolOp.And | SemanticOperator.BoolOp.Or =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Bool, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Float32Op.Add | SemanticOperator.Float32Op.Sub | SemanticOperator.Float32Op.Mul | SemanticOperator.Float32Op.Div
             | SemanticOperator.Float32Op.Exp =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float32, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Float64Op.Add | SemanticOperator.Float64Op.Sub | SemanticOperator.Float64Op.Mul | SemanticOperator.Float64Op.Div
             | SemanticOperator.Float64Op.Exp =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float64, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.BigDecimalOp.Add | SemanticOperator.BigDecimalOp.Sub | SemanticOperator.BigDecimalOp.Mul | SemanticOperator.BigDecimalOp.Div
             | SemanticOperator.BigDecimalOp.Exp =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.BigDecimal, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.BigDecimal, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.BigDecimal, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int8Op.Add | SemanticOperator.Int8Op.Sub | SemanticOperator.Int8Op.Mul | SemanticOperator.Int8Op.Div
             | SemanticOperator.Int8Op.Rem | SemanticOperator.Int8Op.Exp
             | SemanticOperator.Int8Op.And | SemanticOperator.Int8Op.Or | SemanticOperator.Int8Op.Xor =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int8, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int8, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int8, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int16Op.Add | SemanticOperator.Int16Op.Sub | SemanticOperator.Int16Op.Mul | SemanticOperator.Int16Op.Div
             | SemanticOperator.Int16Op.Rem | SemanticOperator.Int16Op.Exp
             | SemanticOperator.Int16Op.And | SemanticOperator.Int16Op.Or | SemanticOperator.Int16Op.Xor =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int16, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int16, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int16, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int32Op.Add | SemanticOperator.Int32Op.Sub | SemanticOperator.Int32Op.Mul | SemanticOperator.Int32Op.Div
             | SemanticOperator.Int32Op.Rem | SemanticOperator.Int32Op.Exp
             | SemanticOperator.Int32Op.And | SemanticOperator.Int32Op.Or | SemanticOperator.Int32Op.Xor =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int32, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int64Op.Add | SemanticOperator.Int64Op.Sub | SemanticOperator.Int64Op.Mul | SemanticOperator.Int64Op.Div
             | SemanticOperator.Int64Op.Rem | SemanticOperator.Int64Op.Exp
             | SemanticOperator.Int64Op.And | SemanticOperator.Int64Op.Or | SemanticOperator.Int64Op.Xor =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int64, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.BigIntOp.Add | SemanticOperator.BigIntOp.Sub | SemanticOperator.BigIntOp.Mul | SemanticOperator.BigIntOp.Div
             | SemanticOperator.BigIntOp.Rem | SemanticOperator.BigIntOp.Exp
             | SemanticOperator.BigIntOp.And | SemanticOperator.BigIntOp.Or | SemanticOperator.BigIntOp.Xor =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.BigInt, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.BigInt, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.BigInt, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.Int8Op.Shl | SemanticOperator.Int8Op.Shr
             | SemanticOperator.Int16Op.Shl | SemanticOperator.Int16Op.Shr
             | SemanticOperator.Int32Op.Shl | SemanticOperator.Int32Op.Shr
             | SemanticOperator.Int64Op.Shl | SemanticOperator.Int64Op.Shr
             | SemanticOperator.BigIntOp.Shl | SemanticOperator.BigIntOp.Shr =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- unifyTypeM(tvar, tpe1, loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, lhs, resultPur, resultEff)

        case SemanticOperator.BoolOp.Eq | SemanticOperator.BoolOp.Neq
             | SemanticOperator.CharOp.Eq | SemanticOperator.CharOp.Neq
             | SemanticOperator.Float32Op.Eq | SemanticOperator.Float32Op.Neq
             | SemanticOperator.Float64Op.Eq | SemanticOperator.Float64Op.Neq
             | SemanticOperator.BigDecimalOp.Eq | SemanticOperator.BigDecimalOp.Neq
             | SemanticOperator.Int8Op.Eq | SemanticOperator.Int8Op.Neq
             | SemanticOperator.Int16Op.Eq | SemanticOperator.Int16Op.Neq
             | SemanticOperator.Int32Op.Eq | SemanticOperator.Int32Op.Neq
             | SemanticOperator.Int64Op.Eq | SemanticOperator.Int64Op.Neq
             | SemanticOperator.BigIntOp.Eq | SemanticOperator.BigIntOp.Neq
             | SemanticOperator.StringOp.Eq | SemanticOperator.StringOp.Neq =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.CharOp.Lt | SemanticOperator.CharOp.Le | SemanticOperator.CharOp.Gt | SemanticOperator.CharOp.Ge
             | SemanticOperator.Float32Op.Lt | SemanticOperator.Float32Op.Le | SemanticOperator.Float32Op.Gt | SemanticOperator.Float32Op.Ge
             | SemanticOperator.Float64Op.Lt | SemanticOperator.Float64Op.Le | SemanticOperator.Float64Op.Gt | SemanticOperator.Float64Op.Ge
             | SemanticOperator.BigDecimalOp.Lt | SemanticOperator.BigDecimalOp.Le | SemanticOperator.BigDecimalOp.Gt | SemanticOperator.BigDecimalOp.Ge
             | SemanticOperator.Int8Op.Lt | SemanticOperator.Int8Op.Le | SemanticOperator.Int8Op.Gt | SemanticOperator.Int8Op.Ge
             | SemanticOperator.Int16Op.Lt | SemanticOperator.Int16Op.Le | SemanticOperator.Int16Op.Gt | SemanticOperator.Int16Op.Ge
             | SemanticOperator.Int32Op.Lt | SemanticOperator.Int32Op.Le | SemanticOperator.Int32Op.Gt | SemanticOperator.Int32Op.Ge
             | SemanticOperator.Int64Op.Lt | SemanticOperator.Int64Op.Le | SemanticOperator.Int64Op.Gt | SemanticOperator.Int64Op.Ge
             | SemanticOperator.BigIntOp.Lt | SemanticOperator.BigIntOp.Le | SemanticOperator.BigIntOp.Gt | SemanticOperator.BigIntOp.Ge =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case SemanticOperator.StringOp.Concat =>
          for {
            (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Str, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Str, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Str, loc)
            resultPur = Type.mkAnd(pur1, pur2, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop'.", loc)
      }

      case KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, pur3, eff3) <- visitExp(exp3)
          condType <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
          resultTyp <- unifyTypeM(tpe2, tpe3, loc)
          resultPur = Type.mkAnd(pur1, pur2, pur3, loc)
          resultEff = Type.mkUnion(List(eff1, eff2, eff3), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Stm(exp1, exp2, loc) =>
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultPur = Type.mkAnd(pur1, pur2, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Discard(exp, loc) =>
        for {
          (constrs, _, pur, eff) <- visitExp(exp)
          resultTyp = Type.Unit
        } yield (constrs, resultTyp, pur, eff)

      case KindedAst.Expression.Let(sym, mod, exp1, exp2, loc) =>
        // Note: The call to unify on sym.tvar occurs immediately after we have inferred the type of exp1.
        // This ensures that uses of sym inside exp2 are type checked according to this type.
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          boundVar <- unifyTypeM(sym.tvar, tpe1, loc)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultPur = Type.mkAnd(pur1, pur2, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc) =>
        // Ensure that `exp1` is a lambda.
        val a = Type.freshVar(Kind.Star, loc)
        val b = Type.freshVar(Kind.Star, loc)
        val p = Type.freshVar(Kind.Bool, loc)
        val ef = Type.freshVar(Kind.Effect, loc)
        val expectedType = Type.mkArrowWithEffect(a, p, ef, b, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          arrowTyp <- unifyTypeM(expectedType, tpe1, exp1.loc)
          boundVar <- unifyTypeM(sym.tvar, tpe1, exp1.loc)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultPur = Type.mkAnd(pur1, pur2, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Region(tpe, _) =>
        liftM(Nil, tpe, Type.Pure, Type.Empty)

      case KindedAst.Expression.Scope(sym, regionVar, exp, pvar, loc) =>
        for {
          // don't make the region var rigid if the --Xflexible-regions flag is set
          _ <- if (flix.options.xflexibleregions) InferMonad.point(()) else rigidifyM(regionVar)
          _ <- unifyTypeM(sym.tvar, Type.mkRegion(regionVar, loc), loc)
          (constrs, tpe, pur, eff) <- visitExp(exp)
          purifiedPur <- purifyEffM(regionVar, pur)
          resultPur <- unifyTypeM(pvar, purifiedPur, loc)
          _ <- noEscapeM(regionVar, tpe)
          resultTyp = tpe
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Match(exp, rules, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.flatMap(_.guard)
        val bodies = rules.map(_.exp)
        val guardLocs = guards.map(_.loc)

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          patternType <- unifyTypeM(tpe :: patternTypes, loc)
          (guardConstrs, guardTypes, guardPurs, guardEffs) <- traverseM(guards)(visitExp).map(unzip4)
          guardType <- traverseM(guardTypes.zip(guardLocs)) { case (gTpe, gLoc) => expectTypeM(expected = Type.Bool, actual = gTpe, loc = gLoc) }
          (bodyConstrs, bodyTypes, bodyPurs, bodyEffs) <- traverseM(bodies)(visitExp).map(unzip4)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultPur = Type.mkAnd(pur :: guardPurs ::: bodyPurs, loc)
          resultEff = Type.mkUnion(eff :: guardEffs ::: bodyEffs, loc)
        } yield (constrs ++ guardConstrs.flatten ++ bodyConstrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.TypeMatch(exp, rules, loc) =>
        val bodies = rules.map(_.exp)

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          // rigidify all the type vars in the rules
          _ <- traverseM(rules.flatMap(rule => rule.tpe.typeVars.toList))(rigidifyM)
          // unify each rule's variable with its type
          _ <- traverseM(rules)(rule => unifyTypeM(rule.sym.tvar, rule.tpe, rule.sym.loc))
          (bodyConstrs, bodyTypes, bodyPurs, bodyEffs) <- traverseM(bodies)(visitExp).map(unzip4)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultPur = Type.mkAnd(pur :: bodyPurs, loc)
          resultEff = Type.mkUnion(eff :: bodyEffs, loc)
        } yield (constrs ++ bodyConstrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.RelationalChoose(star, exps0, rules0, tvar, loc) =>

        /**
          * Performs type inference on the given match expressions `exps` and nullity `vars`.
          *
          * Returns a pair of lists of the types and purects of the match expressions.
          */
        def visitMatchExps(exps: List[KindedAst.Expression], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type], List[Type])] = {
          def visitMatchExp(exp: KindedAst.Expression, isAbsentVar: Type.Var, isPresentVar: Type.Var): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = {
            val freshElmVar = Type.freshVar(Kind.Star, loc)
            for {
              (constrs, tpe, pur, eff) <- visitExp(exp)
              _ <- unifyTypeM(tpe, Type.mkChoice(freshElmVar, isAbsentVar, isPresentVar, loc), loc)
            } yield (constrs, freshElmVar, pur, eff)
          }

          traverseM(exps.zip(isAbsentVars.zip(isPresentVars))) {
            case (matchExp, (isAbsentVar, isPresentVar)) => visitMatchExp(matchExp, isAbsentVar, isPresentVar)
          }.map(unzip4)
        }

        /**
          * Performs type inference of the given null rules `rs`.
          *
          * Returns a pair of list of the types and purects of the rule expressions.
          */
        def visitRuleBodies(rs: List[KindedAst.RelationalChoiceRule]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type], List[Type])] = {
          def visitRuleBody(r: KindedAst.RelationalChoiceRule): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = r match {
            case KindedAst.RelationalChoiceRule(_, exp0) => visitExp(exp0)
          }

          traverseM(rs)(visitRuleBody).map(unzip4)
        }

        /**
          * Returns a transformed result type that encodes the Boolean constraint of each row pattern in the result type.
          *
          * NB: Requires that the `ts` types are Choice-types.
          */
        def transformResultTypes(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], rs: List[KindedAst.RelationalChoiceRule], ts: List[Type], loc: SourceLocation): InferMonad[Type] = {
          def visitRuleBody(r: KindedAst.RelationalChoiceRule, resultType: Type): InferMonad[(Type, Type, Type)] = r match {
            case KindedAst.RelationalChoiceRule(r, exp0) =>
              val cond = mkOverApprox(isAbsentVars, isPresentVars, r)
              val innerType = Type.freshVar(Kind.Star, exp0.loc)
              val isAbsentVar = Type.freshVar(Kind.Bool, exp0.loc)
              val isPresentVar = Type.freshVar(Kind.Bool, exp0.loc)
              for {
                choiceType <- unifyTypeM(resultType, Type.mkChoice(innerType, isAbsentVar, isPresentVar, loc), loc)
              } yield (Type.mkAnd(cond, isAbsentVar, loc), Type.mkAnd(cond, isPresentVar, loc), innerType)
          }

          ///
          /// Simply compute the mgu of the `ts` types if this is not a star relational_choose.
          ///
          if (!star) {
            return unifyTypeM(ts, loc)
          }

          ///
          /// Otherwise construct a new Choice type with isAbsent and isPresent conditions that depend on each pattern row.
          ///
          for {
            (isAbsentConds, isPresentConds, innerTypes) <- traverseM(rs.zip(ts))(p => visitRuleBody(p._1, p._2)).map(_.unzip3)
            isAbsentCond = Type.mkOr(isAbsentConds, loc)
            isPresentCond = Type.mkOr(isPresentConds, loc)
            innerType <- unifyTypeM(innerTypes, loc)
            resultType = Type.mkChoice(innerType, isAbsentCond, isPresentCond, loc)
          } yield resultType
        }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an under-approximation.
          *
          * If a pattern is a wildcard it *must* always match.
          * If a pattern is `Absent`  its corresponding `isPresentVar` must be `false` (i.e. to prevent the value from being `Present`).
          * If a pattern is `Present` its corresponding `isAbsentVar`  must be `false` (i.e. to prevent the value from being `Absent`).
          */
        def mkUnderApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[KindedAst.RelationalChoicePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, KindedAst.RelationalChoicePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), KindedAst.RelationalChoicePattern.Present(_, _, _))) =>
              // Case 2: A `Present` pattern forces the `isAbsentVar` to be equal to `false`.
              Type.mkAnd(acc, Type.mkEquiv(isAbsentVar, Type.False, loc), loc)
            case (acc, ((_, isPresentVar), KindedAst.RelationalChoicePattern.Absent(_))) =>
              // Case 3: An `Absent` pattern forces the `isPresentVar` to be equal to `false`.
              Type.mkAnd(acc, Type.mkEquiv(isPresentVar, Type.False, loc), loc)
          }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an over-approximation.
          *
          * If a pattern is a wildcard it *may* always match.
          * If a pattern is `Absent` it *may* match if its corresponding `isAbsent` is `true`.
          * If a pattern is `Present` it *may* match if its corresponding `isPresentVar`is `true`.
          */
        def mkOverApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[KindedAst.RelationalChoicePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, KindedAst.RelationalChoicePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), KindedAst.RelationalChoicePattern.Absent(_))) =>
              // Case 2: An `Absent` pattern may match if the `isAbsentVar` is `true`.
              Type.mkAnd(acc, isAbsentVar, loc)
            case (acc, ((_, isPresentVar), KindedAst.RelationalChoicePattern.Present(_, _, _))) =>
              // Case 3: A `Present` pattern may match if the `isPresentVar` is `true`.
              Type.mkAnd(acc, isPresentVar, loc)
          }

        /**
          * Constructs a disjunction of the constraints of each choice rule.
          */
        def mkOuterDisj(m: List[List[KindedAst.RelationalChoicePattern]], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): Type =
          m.foldLeft(Type.False) {
            case (acc, rule) => Type.mkOr(acc, mkUnderApprox(isAbsentVars, isPresentVars, rule), loc)
          }

        /**
          * Performs type inference and unification with the `matchTypes` against the given choice rules `rs`.
          */
        def unifyMatchTypesAndRules(matchTypes: List[Type], rs: List[KindedAst.RelationalChoiceRule]): InferMonad[List[List[Type]]] = {
          def unifyWithRule(r: KindedAst.RelationalChoiceRule): InferMonad[List[Type]] = {
            traverseM(matchTypes.zip(r.pat)) {
              case (matchType, KindedAst.RelationalChoicePattern.Wild(_)) =>
                // Case 1: The pattern is wildcard. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, KindedAst.RelationalChoicePattern.Absent(_)) =>
                // Case 2: The pattern is a `Absent`. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, KindedAst.RelationalChoicePattern.Present(sym, tvar, loc)) =>
                // Case 3: The pattern is `Present`. Must constraint the type of the local variable with the type of the match expression.
                unifyTypeM(matchType, sym.tvar, tvar, loc)
            }
          }

          traverseM(rs)(unifyWithRule)
        }

        //
        // Introduce an isAbsent variable for each match expression in `exps`.
        //
        val isAbsentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc))

        //
        // Introduce an isPresent variable for each math expression in `exps`.
        //
        val isPresentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc))

        //
        // Extract the choice pattern match matrix.
        //
        val matrix = rules0.map(_.pat)

        //
        // Compute the saturated pattern match matrix..
        //
        val saturated = ChoiceMatch.saturate(matrix)

        //
        // Build the Boolean formula.
        //
        val formula = mkOuterDisj(saturated, isAbsentVars, isPresentVars)

        //
        // Put everything together.
        //
        for {
          _ <- unifyBoolM(formula, Type.True, loc)
          (matchConstrs, matchTyp, matchPur, matchEff) <- visitMatchExps(exps0, isAbsentVars, isPresentVars)
          _ <- unifyMatchTypesAndRules(matchTyp, rules0)
          (ruleBodyConstrs, ruleBodyTyp, ruleBodyPur, ruleBodyEff) <- visitRuleBodies(rules0)
          resultTypes <- transformResultTypes(isAbsentVars, isPresentVars, rules0, ruleBodyTyp, loc)
          resultTyp <- unifyTypeM(tvar, resultTypes, loc)
          resultPur = Type.mkAnd(matchPur ::: ruleBodyPur, loc)
          resultEff = Type.mkUnion(matchEff ::: ruleBodyEff, loc)
        } yield (matchConstrs.flatten ++ ruleBodyConstrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Tag(symUse, exp, tvar, loc) =>
        if (symUse.sym.enumSym == Symbol.mkEnumSym("Choice")) {
          //
          // Special Case 1: Absent or Present Tag
          //
          if (symUse.sym.name == "Absent") {
            // Case 1.1: Absent Tag.
            val elmVar = Type.freshVar(Kind.Star, loc)
            val isAbsent = Type.True
            val isPresent = Type.freshVar(Kind.Bool, loc)
            for {
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(elmVar, isAbsent, isPresent, loc), loc)
              resultPur = Type.Pure
              resultEff = Type.Empty
            } yield (List.empty, resultTyp, resultPur, resultEff)
          }
          else if (symUse.sym.name == "Present") {
            // Case 1.2: Present Tag.
            val isAbsent = Type.freshVar(Kind.Bool, loc)
            val isPresent = Type.True
            for {
              (constrs, tpe, pur, eff) <- visitExp(exp)
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(tpe, isAbsent, isPresent, loc), loc)
              resultPur = pur
              resultEff = eff
            } yield (constrs, resultTyp, resultPur, resultEff)
          } else {
            // Case 1.3: Unknown tag.
            throw InternalCompilerException(s"Unexpected choice tag: '${symUse.sym}'.", loc)
          }
        } else {
          //
          // General Case:
          //

          // Lookup the enum declaration.
          val decl = root.enums(symUse.sym.enumSym)

          // Lookup the case declaration.
          val caze = decl.cases(symUse.sym)

          // Instantiate the type scheme of the case.
          val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

          //
          // The tag type is a function from the type of variant to the type of the enum.
          //
          for {
            (constrs, tpe, pur, eff) <- visitExp(exp)
            _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
            resultTyp = tvar
            resultPur = pur
            resultEff = eff
          } yield (constrs, resultTyp, resultPur, resultEff)
        }

      case KindedAst.Expression.RestrictableTag(_, _, _, _) => ??? // TODO RESTR-VARS

      case KindedAst.Expression.Tuple(elms, loc) =>
        for {
          (elementConstrs, elementTypes, elementPurs, elementEffs) <- traverseM(elms)(visitExp).map(unzip4)
          resultPur = Type.mkAnd(elementPurs, loc)
          resultEff = Type.mkUnion(elementEffs, loc)
        } yield (elementConstrs.flatten, Type.mkTuple(elementTypes, loc), resultPur, resultEff)

      case KindedAst.Expression.RecordEmpty(loc) =>
        liftM(List.empty, Type.mkRecord(Type.RecordRowEmpty, loc), Type.Pure, Type.Empty)

      case KindedAst.Expression.RecordSelect(exp, field, tvar, loc) =>
        //
        // r : { field = tpe | row }
        // -------------------------
        //       r.field : tpe
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val expectedRowType = Type.mkRecordRowExtend(field, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, expectedRecordType, loc)
          resultPur = pur
          resultEff = eff
        } yield (constrs, tvar, resultPur, resultEff)

      case KindedAst.Expression.RecordExtend(field, exp1, exp2, tvar, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { field = exp1 | exp2 } : { field  :: tpe | r }
        //
        val restRow = Type.freshVar(Kind.RecordRow, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe2, Type.mkRecord(restRow, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(Type.mkRecordRowExtend(field, tpe1, restRow, loc), loc), loc)
          resultPur = Type.mkAnd(pur1, pur2, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.RecordRestrict(field, exp, tvar, loc) =>
        //
        //  exp : { field  :: t | r }
        // -------------------------
        // { -field | exp } : {| r }
        //
        val freshFieldType = Type.freshVar(Kind.Star, loc)
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, Type.mkRecord(Type.mkRecordRowExtend(field, freshFieldType, freshRowVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(freshRowVar, loc), loc)
          resultPur = pur
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.ArrayLit(exps, exp, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, elmTypes, pur1, eff1) <- traverseM(exps)(visitExp).map(unzip4)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp)
          _ <- expectTypeM(expected = regionType, actual = tpe2, loc)
          elmTyp <- unifyTypeAllowEmptyM(elmTypes, Kind.Star, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(elmTyp, regionVar, loc), loc)
          resultPur <- unifyTypeM(pvar, Type.mkAnd(Type.mkAnd(pur1, loc), pur2, regionVar, loc), loc)
          resultEff = Type.mkUnion(eff2 :: eff1, loc)
        } yield (constrs1.flatten ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, pur3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = regionType, actual = tpe1, loc)
          _lenType <- expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(tpe2, regionVar, loc), loc)
          resultPur <- unifyTypeM(pvar, Type.mkAnd(pur1, pur2, pur3, regionVar, loc), loc)
          resultEff = Type.mkUnion(List(eff1, eff2, eff3), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.ArrayLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Bool, loc)
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          _ <- expectTypeM(Type.mkArray(elmVar, regionVar, loc), tpe, exp.loc)
          resultTyp = Type.Int32
          resultPur = pur
          resultEff = eff
          _ <- unbindVar(elmVar)
          _ <- unbindVar(regionVar)
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          arrayType <- expectTypeM(expected = Type.mkArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
          indexType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultPur <- unifyTypeM(pvar, Type.mkAnd(regionVar, pur1, pur2, loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, tvar, resultPur, resultEff)

      case KindedAst.Expression.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val arrayType = Type.mkArray(elmVar, regionVar, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, pur3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe3, exp3.loc)
          resultTyp = Type.Unit
          resultPur <- unifyTypeM(pvar, Type.mkAnd(List(regionVar, pur1, pur2, pur3), loc), loc)
          resultEff = Type.mkUnion(List(eff1, eff2, eff3), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.ArraySlice(exp1, exp2, exp3, exp4, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar0 = Type.freshVar(Kind.Bool, loc)
        val regionType = Type.mkRegion(regionVar0, loc)
        val regionVar1 = Type.freshVar(Kind.Bool, loc)
        val arrayType = Type.mkArray(elmVar, regionVar1, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, pur3, eff3) <- visitExp(exp3)
          (constrs4, tpe4, pur4, eff4) <- visitExp(exp4)
          _regionType <- expectTypeM(expected = regionType, actual = tpe1, exp1.loc)
          _arrayType <- expectTypeM(expected = arrayType, actual = tpe2, exp2.loc)
          _beginIndexType <- expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
          _endIndexType <- expectTypeM(expected = Type.Int32, actual = tpe4, exp4.loc)
          resultTyp = Type.mkArray(elmVar, regionVar0, loc)
          resultPur <- unifyTypeM(pvar, Type.mkAnd(List(regionVar0, regionVar1, pur1, pur2, pur3, pur4), loc), loc)
          resultEff = Type.mkUnion(List(eff1, eff2, eff3, eff4), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3 ++ constrs4, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Ref(exp1, exp2, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe2, regionType, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRef(tpe1, regionVar, loc), loc)
          resultPur <- unifyTypeM(pvar, Type.mkAnd(pur1, pur2, regionVar, loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Deref(exp, tvar, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = refType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultPur <- unifyTypeM(pvar, Type.mkAnd(pur, regionVar, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Assign(exp1, exp2, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = refType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultPur <- unifyTypeM(pvar, Type.mkAnd(pur1, pur2, regionVar, loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Ascribe(exp, expectedTyp, expectedPur, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (constrs, actualTyp, actualPur, actualEff) <- visitExp(exp)
          resultTyp <- expectTypeM(expected = expectedTyp.getOrElse(Type.freshVar(Kind.Star, loc)), actual = actualTyp, bind = tvar, loc)
          resultPur <- expectTypeM(expected = expectedPur.getOrElse(Type.freshVar(Kind.Bool, loc)), actual = actualPur, loc)
          resultEff <- expectTypeM(expected = expectedEff.getOrElse(Type.freshVar(Kind.Effect, loc)), actual = actualEff, loc)
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Cast(exp, declaredTyp, declaredPur, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        for {
          (constrs, actualTyp, actualPur, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, declaredTyp.getOrElse(actualTyp), loc)
          resultPur = declaredPur.getOrElse(actualPur)
          resultEff = declaredEff.getOrElse(actualEff)
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Mask(exp, _) =>
        // A mask expression is unsound; the type system assumes the expression is pure.
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
        } yield (constrs, tpe, Type.Pure, Type.Empty)

      case KindedAst.Expression.Upcast(exp, tvar, _) =>
        for {
          (constrs, _, pur, eff) <- visitExp(exp)
        } yield (constrs, tvar, pur, eff)

      case KindedAst.Expression.Supercast(exp, tvar, _) =>
        // Ignore the type of exp since we later check substitute
        // tvar for the type required by exp and check
        // during Safety that this is actually legal.
        for {
          (constrs, _, pur, eff) <- visitExp(exp)
        } yield (constrs, tvar, pur, eff)

      case KindedAst.Expression.Without(exp, effUse, loc) =>
        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        val expected = Type.mkDifference(Type.freshVar(Kind.Effect, loc), effType, loc)
        for {
          (tconstrs, tpe, pur, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = expected, actual = eff, exp.loc)
        } yield (tconstrs, tpe, pur, expected)

      case KindedAst.Expression.TryCatch(exp, rules, loc) =>
        val rulesType = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          (ruleConstrs, ruleTypes, rulePurs, ruleEffs) <- seqM(rulesType).map(unzip4)
          ruleType <- unifyTypeM(ruleTypes, loc)
          resultTyp <- unifyTypeM(tpe, ruleType, loc)
          resultPur = Type.mkAnd(pur :: rulePurs, loc)
          resultEff = Type.mkUnion(eff :: ruleEffs, loc)
        } yield (constrs ++ ruleConstrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.TryWith(exp, effUse, rules, tvar, loc) =>
        val effect = root.effects(effUse.sym)
        val ops = effect.ops.map(op => op.sym -> op).toMap

        def unifyFormalParams(op: Symbol.OpSym, expected: List[KindedAst.FormalParam], actual: List[KindedAst.FormalParam]): InferMonad[Unit] = {
          if (expected.length != actual.length) {
            InferMonad.errPoint(TypeError.InvalidOpParamCount(op, expected = expected.length, actual = actual.length, loc))
          } else {
            traverseM(expected zip actual) {
              case (ex, ac) =>
                for {
                  _ <- expectTypeM(expected = ex.tpe, actual = ac.tpe, ac.loc)
                } yield ()
            }.map(_ => ())
          }
        }

        def visitHandlerRule(rule: KindedAst.HandlerRule): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = rule match {
          case KindedAst.HandlerRule(op, actualFparams, body, opTvar) =>
            // Don't need to generalize since ops are monomorphic
            // Don't need to handle unknown op because resolver would have caught this
            ops(op.sym) match {
              case KindedAst.Op(_, KindedAst.Spec(_, _, _, _, expectedFparams, _, opTpe, expectedPur, expectedEff, _, _)) =>
                for {
                  _ <- unifyFormalParams(op.sym, expected = expectedFparams, actual = actualFparams)
                  (actualTconstrs, actualTpe, actualPur, actualEff) <- visitExp(body)

                  // unify the operation return type with its tvar
                  _ <- unifyTypeM(opTpe, opTvar, body.loc)

                  // unify the handler result type with the whole block's tvar
                  resultTpe <- expectTypeM(expected = tvar, actual = actualTpe, body.loc)
                  resultPur <- expectTypeM(expected = expectedPur, actual = actualPur, body.loc) // MATT improve error message for this
                  resultEff <- expectTypeM(expected = expectedEff, actual = actualEff, body.loc)
                } yield (actualTconstrs, resultTpe, resultPur, resultEff)
            }
        }

        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        for {
          (tconstrs, tpe, pur, eff) <- visitExp(exp)
          (tconstrss, _, purs, effs) <- traverseM(rules)(visitHandlerRule).map(unzip4)
          resultTconstrs = (tconstrs :: tconstrss).flatten
          resultTpe <- unifyTypeM(tvar, tpe, loc)
          resultPur = Type.mkAnd(pur :: purs, loc)
          resultEff = Type.mkUnion(Type.mkDifference(eff, effType, loc) :: effs, loc)
        } yield (resultTconstrs, resultTpe, resultPur, resultEff)

      case KindedAst.Expression.Do(op, args, loc) =>
        val effect = root.effects(op.sym.eff)
        val operation = effect.ops.find(_.sym == op.sym)
          .getOrElse(throw InternalCompilerException(s"Unexpected missing operation $op in effect ${op.sym.eff}", loc))
        val effTpe = Type.Cst(TypeConstructor.Effect(op.sym.eff), loc)

        def visitArg(arg: KindedAst.Expression, fparam: KindedAst.FormalParam): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = {
          for {
            (tconstrs, tpe, pur, eff) <- visitExp(arg)
            _ <- expectTypeM(expected = fparam.tpe, tpe, arg.loc)
          } yield (tconstrs, tpe, pur, eff)
        }

        if (operation.spec.fparams.length != args.length) {
          InferMonad.errPoint(TypeError.InvalidOpParamCount(op.sym, expected = operation.spec.fparams.length, actual = args.length, loc))
        } else {
          val argM = (args zip operation.spec.fparams) map {
            case (arg, fparam) => visitArg(arg, fparam)
          }
          for {
            (tconstrss, _, purs, effs) <- seqM(argM).map(unzip4)
            resultTconstrs = tconstrss.flatten
            resultTpe = operation.spec.tpe
            resultPur = Type.mkAnd(operation.spec.pur :: purs, loc)
            resultEff = Type.mkUnion(effTpe :: operation.spec.eff :: effs, loc)
          } yield (resultTconstrs, resultTpe, resultPur, resultEff)
        }

      case KindedAst.Expression.Resume(exp, argTvar, retTvar, loc) =>
        for {
          (tconstrs, tpe, pur, eff) <- visitExp(exp)
          resultTconstrs = tconstrs
          _ <- expectTypeM(expected = argTvar, actual = tpe, exp.loc)
          resultTpe = retTvar
          resultPur = pur
          resultEff = eff
        } yield (resultTconstrs, resultTpe, resultPur, resultEff)

      case KindedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val classType = getFlixType(constructor.getDeclaringClass)
        for {
          (constrs, _, _, effs) <- traverseM(args)(visitExp).map(unzip4)
          resultTyp = classType
          resultPur = Type.Impure
          resultEff = Type.mkUnion(effs, loc)
        } yield (constrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.InvokeMethod(method, clazz, exp, args, loc) =>
        val classType = getFlixType(clazz)
        val returnType = getFlixType(method.getReturnType)
        for {
          (baseConstrs, baseTyp, _, baseEff) <- visitExp(exp)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          (constrs, tpes, purs, effs) <- traverseM(args)(visitExp).map(unzip4)
          resultTyp = getFlixType(method.getReturnType)
          resultPur = Type.Impure
          resultEff = Type.mkUnion(effs, loc)
        } yield (baseConstrs ++ constrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val returnType = getFlixType(method.getReturnType)
        for {
          (constrs, tpes, purs, effs) <- traverseM(args)(visitExp).map(unzip4)
          resultTyp = returnType
          resultPur = Type.Impure
          resultEff = Type.mkUnion(effs, loc)
        } yield (constrs.flatten, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.GetField(field, clazz, exp, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        for {
          (constrs, tpe, _, eff) <- visitExp(exp)
          objectTyp <- expectTypeM(expected = classType, actual = tpe, exp.loc)
          resultTyp = fieldType
          resultPur = Type.Impure
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.PutField(field, clazz, exp1, exp2, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        for {
          (constrs1, tpe1, _, eff1) <- visitExp(exp1)
          (constrs2, tpe2, _, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = classType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = fieldType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultPur = Type.Impure
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.GetStaticField(field, loc) =>
        val fieldType = getFlixType(field.getType)
        val resultTyp = fieldType
        val resultPur = Type.Impure
        val resultEff = Type.Empty
        liftM(List.empty, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.PutStaticField(field, exp, loc) =>
        for {
          (valueConstrs, valueTyp, _, valueEff) <- visitExp(exp)
          fieldTyp <- expectTypeM(expected = getFlixType(field.getType), actual = valueTyp, exp.loc)
          resultTyp = Type.Unit
          resultPur = Type.Impure
          resultEff = valueEff
        } yield (valueConstrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.NewObject(_, clazz, methods, loc) =>

        /**
          * Performs type inference on the given JVM `method`.
          */
        def inferJvmMethod(method: KindedAst.JvmMethod): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = method match {
          case KindedAst.JvmMethod(ident, fparams, exp, returnTpe, pur, eff, loc) =>

            /**
              * Constrains the given formal parameter to its declared type.
              */
            def inferParam(fparam: KindedAst.FormalParam): InferMonad[Unit] = fparam match {
              case KindedAst.FormalParam(sym, _, tpe, _, loc) =>
                unifyTypeM(sym.tvar, tpe, loc).map(_ => ())
            }

            for {
              _ <- traverseM(fparams)(inferParam)
              (constrs, bodyTpe, bodyPur, bodyEff) <- visitExp(exp)
              _ <- expectTypeM(expected = returnTpe, actual = bodyTpe, exp.loc)
            } yield (constrs, returnTpe, bodyPur, bodyEff)
        }

        for {
          (constrs, _, _, _) <- traverseM(methods)(inferJvmMethod).map(unzip4)
          resultTyp = getFlixType(clazz)
          resultPur = Type.Impure
          resultEff = Type.Empty
        } yield (constrs.flatten, resultTyp, resultPur, resultEff)


      case KindedAst.Expression.NewChannel(exp1, exp2, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = regionType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultTyp <- liftM(tvar)
          resultPur = Type.mkAnd(pur1, pur2, regionVar, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.GetChannel(exp, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkReceiver(elmVar, regionVar, loc)

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = channelType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultPur = Type.mkAnd(pur, regionVar, loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.PutChannel(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkSender(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = channelType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.mkUnit(loc)
          resultPur = Type.mkAnd(pur1, pur2, regionVar, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.SelectChannel(rules, default, tvar, loc) =>

        val regionVar = Type.freshVar(Kind.Bool, loc)

        /**
          * Performs type inference on the given select rule `sr0`.
          */
        def inferSelectRule(sr0: KindedAst.SelectChannelRule): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] =
          sr0 match {
            case KindedAst.SelectChannelRule(sym, chan, body) => for {
              (chanConstrs, chanType, pur1, chanEff) <- visitExp(chan)
              (bodyConstrs, bodyType, pur2, bodyEff) <- visitExp(body)
              _ <- unifyTypeM(chanType, Type.mkReceiver(sym.tvar, regionVar, sym.loc), sym.loc)
              resultCon = chanConstrs ++ bodyConstrs
              resultTyp = bodyType
              resultPur = Type.mkAnd(pur1, pur2, regionVar, loc)
              resultEff = Type.mkUnion(chanEff, bodyEff, loc)
            } yield (resultCon, resultTyp, resultPur, resultEff)
          }

        /**
          * Performs type inference on the given optional default expression `exp0`.
          */
        def inferDefaultRule(exp0: Option[KindedAst.Expression]): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] =
          exp0 match {
            case None => liftM(Nil, Type.freshVar(Kind.Star, loc), Type.Pure, Type.Empty)
            case Some(exp) => visitExp(exp)
          }

        for {
          (ruleConstrs, ruleTypes, rulePurs, ruleEffs) <- traverseM(rules)(inferSelectRule).map(unzip4)
          (defaultConstrs, defaultType, pur2, defaultEff) <- inferDefaultRule(default)
          resultCon = ruleConstrs.flatten ++ defaultConstrs
          resultTyp <- unifyTypeM(tvar :: defaultType :: ruleTypes, loc)
          resultPur = Type.mkAnd(regionVar :: pur2 :: rulePurs, loc)
          resultEff = Type.mkUnion(defaultEff :: ruleEffs, loc)
        } yield (resultCon, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Spawn(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, _, eff1) <- visitExp(exp1)
          (constrs2, tpe2, _, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultPur = Type.mkAnd(Type.Impure, regionVar, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Par(exp, _) =>
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          resultPur <- expectTypeM(expected = Type.Pure, actual = pur, exp.loc)
          resultEff <- expectTypeM(expected = Type.Empty, actual = eff, exp.loc)
        } yield (constrs, tpe, resultPur, resultEff)

      case KindedAst.Expression.ParYield(frags, exp, loc) =>
        val patterns = frags.map(_.pat)
        val parExps = frags.map(_.exp)
        val patLocs = frags.map(_.loc)
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          (fragConstrs, fragTypes, fragPurs, fragEffs) <- seqM(parExps map visitExp).map(unzip4)
          _ <- seqM(patternTypes.zip(fragTypes).zip(patLocs).map { case ((patTpe, expTpe), l) => unifyTypeM(List(patTpe, expTpe), l) })
          _ <- seqM(fragPurs.zip(patLocs) map { case (p, l) => expectTypeM(expected = Type.Pure, actual = p, l) })
          _ <- seqM(fragEffs.zip(patLocs) map { case (e, l) => expectTypeM(expected = Type.Empty, actual = e, l) })
        } yield (constrs ++ fragConstrs.flatten, tpe, pur, eff)

      case KindedAst.Expression.Lazy(exp, loc) =>
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          resultTyp = Type.mkLazy(tpe, loc)
          resultPur <- expectTypeM(expected = Type.Pure, actual = pur, exp.loc)
          resultEff <- expectTypeM(expected = Type.Empty, actual = eff, exp.loc)
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.Force(exp, tvar, loc) =>
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          lazyTyp <- expectTypeM(expected = Type.mkLazy(tvar, loc), actual = tpe, exp.loc)
          resultTyp = tvar
          resultPur = pur
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.FixpointConstraintSet(cs, tvar, loc) =>
        for {
          (constrs, constraintTypes) <- traverseM(cs)(visitConstraint).map(_.unzip)
          schemaRow <- unifyTypeAllowEmptyM(constraintTypes, Kind.SchemaRow, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(schemaRow, loc), loc)
        } yield (constrs.flatten, resultTyp, Type.Pure, Type.Empty)

      case KindedAst.Expression.FixpointLambda(pparams, exp, tvar, loc) =>

        def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
          case KindedAst.PredicateParam(pred, tpe, loc) => Type.mkSchemaRowExtend(pred, tpe, restRow, tpe.loc)
        }

        def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

        val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))
        val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          _ <- unifyTypeM(tpe, Type.mkSchema(expectedRowType, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(resultRowType, loc), loc)
          resultPur = pur
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tpe1, tpe2, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultPur = Type.mkAnd(pur1, pur2, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.FixpointSolve(exp, loc) =>
        //
        //  exp : #{...}
        //  ---------------
        //  solve exp : tpe
        //
        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tpe, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultPur = pur
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.FixpointFilter(pred, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : #{ P : a  | b }
        //  -------------------------------------------
        //  project P exp2 : #{ P : a | c }
        //
        val freshPredicateTypeVar = Type.freshVar(Kind.Predicate, loc)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)
        val freshResultSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshRestSchemaTypeVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshResultSchemaTypeVar, loc), loc), loc)
          resultPur = pur
          resultEff = eff
        } yield (constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.FixpointInject(exp, pred, tvar, loc) =>
        //
        //  exp : F[freshElmType] where F is Foldable
        //  -------------------------------------------
        //  project exp into A: #{A(freshElmType) | freshRestSchemaType}
        //
        val freshTypeConstructorVar = Type.freshVar(Kind.Star ->: Kind.Star, loc)
        val freshElmTypeVar = Type.freshVar(Kind.Star, loc)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

        // Require Boxable and Foldable instances.
        val boxableSym = PredefinedClasses.lookupClassSym("Boxable", root)
        val foldableSym = PredefinedClasses.lookupClassSym("Foldable", root)
        val boxable = Ast.TypeConstraint(Ast.TypeConstraint.Head(boxableSym, loc), freshElmTypeVar, loc)
        val foldable = Ast.TypeConstraint(Ast.TypeConstraint.Head(foldableSym, loc), freshTypeConstructorVar, loc)

        for {
          (constrs, tpe, pur, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkApply(freshTypeConstructorVar, List(freshElmTypeVar), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(List(freshElmTypeVar), loc), freshRestSchemaTypeVar, loc), loc), loc)
          resultPur = pur
          resultEff = eff
        } yield (boxable :: foldable :: constrs, resultTyp, resultPur, resultEff)

      case KindedAst.Expression.FixpointProject(pred, exp1, exp2, tvar, loc) =>
        //
        //  exp1: {$Result(freshRelOrLat, freshTupleVar) | freshRestSchemaVar }
        //  exp2: freshRestSchemaVar
        //  --------------------------------------------------------------------
        //  FixpointQuery pred, exp1, exp2 : Array[freshTupleVar]
        //
        val freshRelOrLat = Type.freshVar(Kind.Star ->: Kind.Predicate, loc)
        val freshTupleVar = Type.freshVar(Kind.Star, loc)
        val freshRestSchemaVar = Type.freshVar(Kind.SchemaRow, loc)
        val expectedSchemaType = Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.Apply(freshRelOrLat, freshTupleVar, loc), freshRestSchemaVar, loc), loc)
        for {
          (constrs1, tpe1, pur1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, pur2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe1, expectedSchemaType, loc)
          _ <- unifyTypeM(tpe2, Type.mkSchema(freshRestSchemaVar, loc), loc)
          resultTyp <- unifyTypeM(tvar, mkList(freshTupleVar, loc), loc)
          resultPur = Type.mkAnd(pur1, pur2, loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultPur, resultEff)

    }

    /**
      * Infers the type of the given constraint `con0` inside the inference monad.
      */
    def visitConstraint(con0: KindedAst.Constraint): InferMonad[(List[Ast.TypeConstraint], Type)] = {
      val KindedAst.Constraint(cparams, head0, body0, loc) = con0
      //
      //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
      //  -----------------------------------
      //  A_0 :- A_1, ..., A_n : tpe
      //
      for {
        (constrs1, headPredicateType) <- inferHeadPredicate(head0, root)
        (constrs2, bodyPredicateTypes) <- traverseM(body0)(b => inferBodyPredicate(b, root)).map(_.unzip)
        bodyPredicateType <- unifyTypeAllowEmptyM(bodyPredicateTypes, Kind.SchemaRow, loc)
        resultType <- unifyTypeM(headPredicateType, bodyPredicateType, loc)
      } yield (constrs1 ++ constrs2.flatten, resultType)
    }

    visitExp(exp0)
  }

  private def mkList(t: Type, loc: SourceLocation): Type =
    Type.mkEnum(Symbol.mkEnumSym("List"), List(t), loc)

  /**
    * Applies the given substitution `subst0` to the given expression `exp0`.
    */
  private def reassembleExp(exp0: KindedAst.Expression, root: KindedAst.Root, subst0: Substitution): TypedAst.Expression = {
    /**
      * Applies the given substitution `subst0` to the given expression `exp0`.
      */
    def visitExp(exp0: KindedAst.Expression, subst0: Substitution): TypedAst.Expression = exp0 match {

      case KindedAst.Expression.Wild(tvar, loc) =>
        TypedAst.Expression.Wild(subst0(tvar), loc)

      case KindedAst.Expression.Var(sym, loc) =>
        TypedAst.Expression.Var(sym, subst0(sym.tvar), loc)

      case KindedAst.Expression.Def(sym, tvar, loc) =>
        TypedAst.Expression.Def(sym, subst0(tvar), loc)

      case KindedAst.Expression.Sig(sym, tvar, loc) =>
        TypedAst.Expression.Sig(sym, subst0(tvar), loc)

      case KindedAst.Expression.Hole(sym, tpe, loc) =>
        TypedAst.Expression.Hole(sym, subst0(tpe), loc)

      case KindedAst.Expression.HoleWithExp(exp, tvar, pvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.HoleWithExp(e, subst0(tvar), subst0(pvar), subst0(evar), loc)

      case KindedAst.Expression.Use(sym, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Use(sym, e, loc)

      // change null to Unit type
      case KindedAst.Expression.Cst(Ast.Constant.Null, loc) => TypedAst.Expression.Cst(Ast.Constant.Null, Type.Unit, loc)

      case KindedAst.Expression.Cst(cst, loc) => TypedAst.Expression.Cst(cst, constantType(cst), loc)

      case KindedAst.Expression.Apply(exp, exps, tvar, pvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        val es = exps.map(visitExp(_, subst0))
        TypedAst.Expression.Apply(e, es, subst0(tvar), subst0(pvar), subst0(evar), loc)

      case KindedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp, subst0)
        val t = subst0(tvar)
        TypedAst.Expression.Lambda(p, e, t, loc)

      case KindedAst.Expression.Unary(sop, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.Unary(sop, e, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.Binary(sop, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val pur = Type.mkAnd(e1.pur, e2.pur, loc)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.Binary(sop, e1, e2, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e2.tpe
        val pur = Type.mkAnd(e1.pur, e2.pur, e3.pur, loc)
        val eff = Type.mkUnion(List(e1.eff, e2.eff, e3.eff), loc)
        TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, pur, eff, loc)

      case KindedAst.Expression.Stm(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val pur = Type.mkAnd(e1.pur, e2.pur, loc)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.Stm(e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.Discard(exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Discard(e, e.pur, e.eff, loc)

      case KindedAst.Expression.Let(sym, mod, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val pur = Type.mkAnd(e1.pur, e2.pur, loc)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.Let(sym, mod, e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val pur = Type.mkAnd(e1.pur, e2.pur, loc)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.LetRec(sym, mod, e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.Region(tpe, loc) =>
        TypedAst.Expression.Region(tpe, loc)

      case KindedAst.Expression.Scope(sym, regionVar, exp, pvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val pur = subst0(pvar)
        val eff = e.eff
        TypedAst.Expression.Scope(sym, regionVar, e, tpe, pur, eff, loc)

      case KindedAst.Expression.Match(matchExp, rules, loc) =>
        val e1 = visitExp(matchExp, subst0)
        val rs = rules map {
          case KindedAst.MatchRule(pat, guard, exp) =>
            val p = reassemblePattern(pat, root, subst0)
            val g = guard.map(visitExp(_, subst0))
            val b = visitExp(exp, subst0)
            TypedAst.MatchRule(p, g, b)
        }
        val tpe = rs.head.exp.tpe
        val pur = rs.foldLeft(e1.pur) {
          case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkAnd(g.map(_.pur).toList ::: List(b.pur, acc), loc)
        }
        val eff = rs.foldLeft(e1.eff) {
          case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkUnion(g.map(_.eff).toList ::: List(b.eff, acc), loc)
        }
        TypedAst.Expression.Match(e1, rs, tpe, pur, eff, loc)

      case KindedAst.Expression.TypeMatch(matchExp, rules, loc) =>
        val e1 = visitExp(matchExp, subst0)
        val rs = rules map {
          case KindedAst.MatchTypeRule(sym, tpe0, exp) =>
            val t = subst0(tpe0)
            val b = visitExp(exp, subst0)
            TypedAst.MatchTypeRule(sym, t, b)
        }
        val tpe = rs.head.exp.tpe
        val pur = rs.foldLeft(e1.pur) {
          case (acc, TypedAst.MatchTypeRule(_, _, b)) => Type.mkAnd(b.pur, acc, loc)
        }
        val eff = rs.foldLeft(e1.eff) {
          case (acc, TypedAst.MatchTypeRule(_, _, b)) => Type.mkUnion(List(b.eff, acc), loc)
        }
        TypedAst.Expression.TypeMatch(e1, rs, tpe, pur, eff, loc)

      case KindedAst.Expression.RelationalChoose(_, exps, rules, tvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val rs = rules.map {
          case KindedAst.RelationalChoiceRule(pat0, exp) =>
            val pat = pat0.map {
              case KindedAst.RelationalChoicePattern.Wild(loc) => TypedAst.RelationalChoicePattern.Wild(loc)
              case KindedAst.RelationalChoicePattern.Absent(loc) => TypedAst.RelationalChoicePattern.Absent(loc)
              case KindedAst.RelationalChoicePattern.Present(sym, tvar, loc) => TypedAst.RelationalChoicePattern.Present(sym, subst0(tvar), loc)
            }
            TypedAst.RelationalChoiceRule(pat, visitExp(exp, subst0))
        }
        val tpe = subst0(tvar)
        val pur = Type.mkAnd(rs.map(_.exp.pur), loc)
        val eff = Type.mkUnion(rs.map(_.exp.eff), loc)
        TypedAst.Expression.RelationalChoose(es, rs, tpe, pur, eff, loc)

      case KindedAst.Expression.Tag(sym, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.Tag(sym, e, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.RestrictableTag(sym, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.RestrictableTag(sym, e, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.Tuple(elms, loc) =>
        val es = elms.map(visitExp(_, subst0))
        val tpe = Type.mkTuple(es.map(_.tpe), loc)
        val pur = Type.mkAnd(es.map(_.pur), loc)
        val eff = Type.mkUnion(es.map(_.eff), loc)
        TypedAst.Expression.Tuple(es, tpe, pur, eff, loc)

      case KindedAst.Expression.RecordEmpty(loc) =>
        TypedAst.Expression.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)

      case KindedAst.Expression.RecordSelect(exp, field, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.RecordSelect(e, field, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.RecordExtend(field, value, rest, tvar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        val pur = Type.mkAnd(v.pur, r.pur, loc)
        val eff = Type.mkUnion(v.eff, r.eff, loc)
        TypedAst.Expression.RecordExtend(field, v, r, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.RecordRestrict(field, rest, tvar, loc) =>
        val r = visitExp(rest, subst0)
        val pur = r.pur
        val eff = r.eff
        TypedAst.Expression.RecordRestrict(field, r, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.ArrayLit(exps, exp, tvar, pvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val pur = subst0(pvar)
        val eff = Type.mkUnion(e.eff :: es.map(_.eff), loc)
        TypedAst.Expression.ArrayLit(es, e, tpe, pur, eff, loc)

      case KindedAst.Expression.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = subst0(tvar)
        val pur = subst0(pvar)
        val eff = Type.mkUnion(List(e1.eff, e2.eff, e3.eff), loc)
        TypedAst.Expression.ArrayNew(e1, e2, e3, tpe, pur, eff, loc)

      case KindedAst.Expression.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = subst0(tvar)
        val pur = subst0(pvar)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.ArrayLoad(e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val pur = subst0(pvar)
        val eff = Type.mkUnion(List(e1.eff, e2.eff, e3.eff), loc)
        TypedAst.Expression.ArrayStore(e1, e2, e3, pur, eff, loc)

      case KindedAst.Expression.ArrayLength(exp, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.ArrayLength(e, pur, eff, loc)

      case KindedAst.Expression.ArraySlice(exp1, exp2, exp3, exp4, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val e4 = visitExp(exp4, subst0)
        val tpe = e2.tpe
        val pur = subst0(pvar)
        val eff = Type.mkUnion(List(e1.eff, e2.eff, e3.eff, e4.eff), loc)
        TypedAst.Expression.ArraySlice(e1, e2, e3, e4, tpe, pur, eff, loc)

      case KindedAst.Expression.Ref(exp1, exp2, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = subst0(tvar)
        val pur = subst0(pvar)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.Ref(e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.Deref(exp, tvar, pvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val pur = subst0(pvar)
        val eff = e.eff
        TypedAst.Expression.Deref(e, tpe, pur, eff, loc)

      case KindedAst.Expression.Assign(exp1, exp2, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val pur = subst0(pvar)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.Assign(e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.Ascribe(exp, _, _, _, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.Ascribe(e, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.Cast(KindedAst.Expression.Cst(Ast.Constant.Null, _), _, _, _, tvar, loc) =>
        val t = subst0(tvar)
        TypedAst.Expression.Cst(Ast.Constant.Null, t, loc)

      case KindedAst.Expression.Cast(exp, declaredType, declaredPur, declaredEff, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val dt = declaredType.map(tpe => subst0(tpe))
        val dp = declaredPur.map(pur => subst0(pur))
        val de = declaredEff.map(eff => subst0(eff))
        val tpe = subst0(tvar)
        val pur = declaredPur.getOrElse(e.pur)
        val eff = declaredEff.getOrElse(e.eff)
        TypedAst.Expression.Cast(e, dt, dp, de, tpe, pur, eff, loc)

      case KindedAst.Expression.Mask(exp, loc) =>
        // We explicitly mark a `Mask` expression as Impure.
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val pur = Type.Impure
        val eff = e.eff
        TypedAst.Expression.Mask(e, tpe, pur, eff, loc)

      case KindedAst.Expression.Upcast(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Upcast(e, subst0(tvar), loc)

      case KindedAst.Expression.Supercast(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Supercast(e, subst0(tvar), loc)

      case KindedAst.Expression.Without(exp, effUse, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.Without(e, effUse, tpe, pur, eff, loc)

      case KindedAst.Expression.TryCatch(exp, rules, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        val tpe = rs.head.exp.tpe
        val pur = Type.mkAnd(e.pur :: rs.map(_.exp.pur), loc)
        val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
        TypedAst.Expression.TryCatch(e, rs, tpe, pur, eff, loc)

      case KindedAst.Expression.TryWith(exp, effUse, rules, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case KindedAst.HandlerRule(op, fparams, hexp, htvar) =>
            val fps = fparams.map(visitFormalParam)
            val he = visitExp(hexp, subst0)
            TypedAst.HandlerRule(op, fps, he)
        }
        val tpe = subst0(tvar)
        val pur = Type.mkAnd(e.pur :: rs.map(_.exp.pur), loc)
        val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
        TypedAst.Expression.TryWith(e, effUse, rs, tpe, pur, eff, loc)

      case KindedAst.Expression.Do(op, exps, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val pur = Type.mkAnd(es.map(_.pur), loc)
        val eff = Type.mkUnion(Type.Cst(TypeConstructor.Effect(op.sym.eff), loc) :: es.map(_.eff), loc)
        TypedAst.Expression.Do(op, es, pur, eff, loc)

      case KindedAst.Expression.Resume(exp, _, retTvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(retTvar)
        TypedAst.Expression.Resume(e, tpe, loc)

      case KindedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(constructor.getDeclaringClass)
        val pur = Type.Impure
        val eff = Type.mkUnion(as.map(_.eff), loc)
        TypedAst.Expression.InvokeConstructor(constructor, as, tpe, pur, eff, loc)

      case KindedAst.Expression.InvokeMethod(method, _, exp, args, loc) =>
        val e = visitExp(exp, subst0)
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val pur = Type.Impure
        val eff = Type.mkUnion(e.eff :: as.map(_.eff), loc)
        TypedAst.Expression.InvokeMethod(method, e, as, tpe, pur, eff, loc)

      case KindedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val pur = Type.Impure
        val eff = Type.mkUnion(as.map(_.eff), loc)
        TypedAst.Expression.InvokeStaticMethod(method, as, tpe, pur, eff, loc)

      case KindedAst.Expression.GetField(field, _, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = getFlixType(field.getType)
        val pur = Type.Impure
        val eff = e.eff
        TypedAst.Expression.GetField(field, e, tpe, pur, eff, loc)

      case KindedAst.Expression.PutField(field, _, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val pur = Type.Impure
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.PutField(field, e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.GetStaticField(field, loc) =>
        val tpe = getFlixType(field.getType)
        val pur = Type.Impure
        val eff = Type.Empty
        TypedAst.Expression.GetStaticField(field, tpe, pur, eff, loc)

      case KindedAst.Expression.PutStaticField(field, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.Unit
        val pur = Type.Impure
        val eff = e.eff
        TypedAst.Expression.PutStaticField(field, e, tpe, pur, eff, loc)

      case KindedAst.Expression.NewObject(name, clazz, methods, loc) =>
        val tpe = getFlixType(clazz)
        val pur = Type.Impure
        val eff = Type.Empty
        val ms = methods map visitJvmMethod
        TypedAst.Expression.NewObject(name, clazz, tpe, pur, eff, ms, loc)

      case KindedAst.Expression.NewChannel(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val pur = Type.Impure
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.NewChannel(e1, e2, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.GetChannel(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = Type.Impure
        val eff = e.eff
        TypedAst.Expression.GetChannel(e, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.PutChannel(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.mkUnit(loc)
        val pur = Type.Impure
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.PutChannel(e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.SelectChannel(rules, default, tvar, loc) =>
        val rs = rules map {
          case KindedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule(sym, c, b)
        }
        val d = default.map(visitExp(_, subst0))
        val pur = Type.Impure
        val effs = (d.toList ::: rs.map(_.exp)).map(_.eff)
        val eff = Type.mkUnion(effs, loc)
        TypedAst.Expression.SelectChannel(rs, d, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.Spawn(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val pur = Type.Impure
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.Spawn(e1, e2, tpe, pur, eff, loc)

      case KindedAst.Expression.Par(exp, loc) =>
        TypedAst.Expression.Par(visitExp(exp, subst0), loc)

      case KindedAst.Expression.ParYield(frags, exp, loc) =>
        val e = visitExp(exp, subst0)
        val fs = frags map {
          case KindedAst.ParYieldFragment(pat, e0, l0) =>
            val p = reassemblePattern(pat, root, subst0)
            val e1 = visitExp(e0, subst0)
            TypedAst.ParYieldFragment(p, e1, l0)
        }
        val tpe = e.tpe
        val pur = fs.foldLeft(e.pur) {
          case (acc, TypedAst.ParYieldFragment(_, e1, _)) => Type.mkAnd(acc, e1.pur, loc)
        }
        val eff = fs.foldLeft(e.eff) {
          case (acc, TypedAst.ParYieldFragment(_, e1, _)) => Type.mkUnion(acc, e1.eff, loc)
        }
        TypedAst.Expression.ParYield(fs, e, tpe, pur, eff, loc)

      case KindedAst.Expression.Lazy(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.mkLazy(e.tpe, loc)
        TypedAst.Expression.Lazy(e, tpe, loc)

      case KindedAst.Expression.Force(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.Force(e, tpe, pur, eff, loc)

      case KindedAst.Expression.FixpointConstraintSet(cs0, tvar, loc) =>
        val cs = cs0.map(visitConstraint)
        TypedAst.Expression.FixpointConstraintSet(cs, Stratification.empty, subst0(tvar), loc)

      case KindedAst.Expression.FixpointLambda(pparams, exp, tvar, loc) =>
        val ps = pparams.map(visitPredicateParam)
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.FixpointLambda(ps, e, Stratification.empty, tpe, pur, eff, loc)

      case KindedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e1.tpe
        val pur = Type.mkAnd(e1.pur, e2.pur, loc)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expression.FixpointMerge(e1, e2, Stratification.empty, tpe, pur, eff, loc)

      case KindedAst.Expression.FixpointSolve(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.FixpointSolve(e, Stratification.empty, tpe, pur, eff, loc)

      case KindedAst.Expression.FixpointFilter(pred, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.FixpointFilter(pred, e, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.FixpointInject(exp, pred, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val pur = e.pur
        val eff = e.eff
        TypedAst.Expression.FixpointInject(e, pred, subst0(tvar), pur, eff, loc)

      case KindedAst.Expression.FixpointProject(pred, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val stf = Stratification.empty
        val tpe = subst0(tvar)
        val pur = Type.mkAnd(e1.pur, e2.pur, loc)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)

        // Note: This transformation should happen in the Weeder but it is here because
        // `#{#Result(..)` | _} cannot be unified with `#{A(..)}` (a closed row).
        // See Weeder for more details.
        val mergeExp = TypedAst.Expression.FixpointMerge(e1, e2, stf, e1.tpe, pur, eff, loc)
        val solveExp = TypedAst.Expression.FixpointSolve(mergeExp, stf, e1.tpe, pur, eff, loc)
        TypedAst.Expression.FixpointProject(pred, solveExp, tpe, pur, eff, loc)
    }

    /**
      * Applies the substitution to the given constraint.
      */
    def visitConstraint(c0: KindedAst.Constraint): TypedAst.Constraint = {
      // Pattern match on the constraint.
      val KindedAst.Constraint(cparams0, head0, body0, loc) = c0

      // Unification was successful. Reassemble the head and body predicates.
      val head = reassembleHeadPredicate(head0, root, subst0)
      val body = body0.map(b => reassembleBodyPredicate(b, root, subst0))

      // Reassemble the constraint parameters.
      val cparams = cparams0.map {
        case KindedAst.ConstraintParam(sym, l) =>
          TypedAst.ConstraintParam(sym, subst0(sym.tvar), l)
      }

      // Reassemble the constraint.
      TypedAst.Constraint(cparams, head, body, loc)
    }

    /**
      * Applies the substitution to the given list of formal parameters.
      */
    def visitFormalParam(fparam: KindedAst.FormalParam): TypedAst.FormalParam =
      TypedAst.FormalParam(fparam.sym, fparam.mod, subst0(fparam.tpe), fparam.src, fparam.loc)

    /**
      * Applies the substitution to the given list of predicate parameters.
      */
    def visitPredicateParam(pparam: KindedAst.PredicateParam): TypedAst.PredicateParam =
      TypedAst.PredicateParam(pparam.pred, subst0(pparam.tpe), pparam.loc)

    /**
      * Applies the substitution to the given jvm method.
      */
    def visitJvmMethod(method: KindedAst.JvmMethod): TypedAst.JvmMethod = {
      method match {
        case KindedAst.JvmMethod(ident, fparams0, exp0, tpe, pur, eff, loc) =>
          val fparams = getFormalParams(fparams0, subst0)
          val exp = visitExp(exp0, subst0)
          TypedAst.JvmMethod(ident, fparams, exp, tpe, pur, eff, loc)
      }
    }

    visitExp(exp0, subst0)
  }

  /**
    * Infers the type of the given pattern `pat0`.
    */
  private def inferPattern(pat0: KindedAst.Pattern, root: KindedAst.Root)(implicit flix: Flix): InferMonad[Type] = {
    /**
      * Local pattern visitor.
      */
    def visit(p: KindedAst.Pattern): InferMonad[Type] = p match {
      case KindedAst.Pattern.Wild(tvar, loc) => liftM(tvar)

      case KindedAst.Pattern.Var(sym, tvar, loc) => unifyTypeM(sym.tvar, tvar, loc)

      case KindedAst.Pattern.Cst(Ast.Constant.Unit, loc) => liftM(Type.Unit)

      case KindedAst.Pattern.Cst(Ast.Constant.Bool(b), loc) => liftM(Type.Bool)

      case KindedAst.Pattern.Cst(Ast.Constant.Char(c), loc) => liftM(Type.Char)

      case KindedAst.Pattern.Cst(Ast.Constant.Float32(i), loc) => liftM(Type.Float32)

      case KindedAst.Pattern.Cst(Ast.Constant.Float64(i), loc) => liftM(Type.Float64)

      case KindedAst.Pattern.Cst(Ast.Constant.BigDecimal(i), loc) => liftM(Type.BigDecimal)

      case KindedAst.Pattern.Cst(Ast.Constant.Int8(i), loc) => liftM(Type.Int8)

      case KindedAst.Pattern.Cst(Ast.Constant.Int16(i), loc) => liftM(Type.Int16)

      case KindedAst.Pattern.Cst(Ast.Constant.Int32(i), loc) => liftM(Type.Int32)

      case KindedAst.Pattern.Cst(Ast.Constant.Int64(i), loc) => liftM(Type.Int64)

      case KindedAst.Pattern.Cst(Ast.Constant.BigInt(i), loc) => liftM(Type.BigInt)

      case KindedAst.Pattern.Cst(Ast.Constant.Str(s), loc) => liftM(Type.Str)

      case KindedAst.Pattern.Cst(Ast.Constant.Null, loc) => throw InternalCompilerException("unexpected null pattern", loc)

      case KindedAst.Pattern.Tag(symUse, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.enums(symUse.sym.enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // Instantiate the type scheme of the case.
        val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of variant to the type of the enum.
        //
        for {
          tpe <- visit(pat)
          _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
          resultTyp = tvar
        } yield resultTyp

      case KindedAst.Pattern.Tuple(elms, loc) =>
        for {
          elementTypes <- traverseM(elms)(visit)
        } yield Type.mkTuple(elementTypes, loc)

      case KindedAst.Pattern.Array(elms, tvar, loc) =>
        for {
          elementTypes <- traverseM(elms)(visit)
          elementType <- unifyTypeAllowEmptyM(elementTypes, Kind.Star, loc)
          resultType <- unifyTypeM(tvar, Type.mkArray(elementType, Type.False, loc), loc)
        } yield resultType

      case KindedAst.Pattern.ArrayTailSpread(elms, varSym, tvar, loc) =>
        for {
          elementTypes <- traverseM(elms)(visit)
          elementType <- unifyTypeAllowEmptyM(elementTypes, Kind.Star, loc)
          arrayType <- unifyTypeM(tvar, Type.mkArray(elementType, Type.False, loc), loc)
          resultType <- unifyTypeM(varSym.tvar, arrayType, loc)
        } yield resultType

      case KindedAst.Pattern.ArrayHeadSpread(varSym, elms, tvar, loc) =>
        for {
          elementTypes <- traverseM(elms)(visit)
          elementType <- unifyTypeAllowEmptyM(elementTypes, Kind.Star, loc)
          arrayType <- unifyTypeM(tvar, Type.mkArray(elementType, Type.False, loc), loc)
          resultType <- unifyTypeM(varSym.tvar, arrayType, loc)
        } yield resultType

    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[KindedAst.Pattern], root: KindedAst.Root)(implicit flix: Flix): InferMonad[List[Type]] = {
    traverseM(pats0)(inferPattern(_, root))
  }

  /**
    * Applies the substitution `subst0` to the given pattern `pat0`.
    */
  private def reassemblePattern(pat0: KindedAst.Pattern, root: KindedAst.Root, subst0: Substitution): TypedAst.Pattern = {
    /**
      * Local pattern visitor.
      */
    def visit(p: KindedAst.Pattern): TypedAst.Pattern = p match {
      case KindedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst0(tvar), loc)
      case KindedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym, subst0(tvar), loc)
      case KindedAst.Pattern.Cst(cst, loc) => TypedAst.Pattern.Cst(cst, constantType(cst), loc)

      case KindedAst.Pattern.Tag(sym, pat, tvar, loc) => TypedAst.Pattern.Tag(sym, visit(pat), subst0(tvar), loc)

      case KindedAst.Pattern.Tuple(elms, loc) =>
        val es = elms.map(visit)
        val tpe = Type.mkTuple(es.map(_.tpe), loc)
        TypedAst.Pattern.Tuple(es, tpe, loc)

      case KindedAst.Pattern.Array(elms, tvar, loc) => TypedAst.Pattern.Array(elms map visit, subst0(tvar), loc)
      case KindedAst.Pattern.ArrayTailSpread(elms, sym, tvar, loc) => TypedAst.Pattern.ArrayTailSpread(elms map visit, sym, subst0(tvar), loc)
      case KindedAst.Pattern.ArrayHeadSpread(sym, elms, tvar, loc) => TypedAst.Pattern.ArrayHeadSpread(sym, elms map visit, subst0(tvar), loc)
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given head predicate.
    */
  private def inferHeadPredicate(head: KindedAst.Predicate.Head, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = head match {
    case KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc) =>
      // Adds additional type constraints if the denotation is a lattice.
      val restRow = Type.freshVar(Kind.SchemaRow, loc)
      for {
        (termConstrs, termTypes, termPurs, termEffs) <- traverseM(terms)(inferExp(_, root)).map(unzip4)
        pureTermPurs <- unifyBoolM(Type.Pure, Type.mkAnd(termPurs, loc), loc)
        pureTermEffs <- unifyTypeM(Type.Empty, Type.mkUnion(termEffs, loc), loc)
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
      } yield (termConstrs.flatten ++ tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))
  }

  /**
    * Applies the given substitution `subst0` to the given head predicate `head0`.
    */
  private def reassembleHeadPredicate(head0: KindedAst.Predicate.Head, root: KindedAst.Root, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case KindedAst.Predicate.Head.Atom(pred, den0, terms, tvar, loc) =>
      val ts = terms.map(t => reassembleExp(t, root, subst0))
      TypedAst.Predicate.Head.Atom(pred, den0, ts, subst0(tvar), loc)
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: KindedAst.Predicate.Body, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = body0 match {
    case KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, tvar, loc) =>
      val restRow = Type.freshVar(Kind.SchemaRow, loc)
      for {
        termTypes <- traverseM(terms)(inferPattern(_, root))
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
      } yield (tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      for {
        (constrs, tpe, pur, eff) <- inferExp(exp, root)
        expPur <- unifyBoolM(Type.Pure, pur, loc)
        expTyp <- unifyTypeM(Type.Bool, tpe, loc)
        expEff <- unifyTypeM(Type.Empty, eff, loc)
      } yield (constrs, mkAnySchemaRowType(loc))

    case KindedAst.Predicate.Body.Loop(varSyms, exp, loc) =>
      // TODO: Use type classes instead of array?
      val tupleType = Type.mkTuple(varSyms.map(_.tvar), loc)
      val expectedType = Type.mkArray(tupleType, Type.False, loc)
      for {
        (constrs, tpe, pur, eff) <- inferExp(exp, root)
        expPur <- unifyBoolM(Type.Pure, pur, loc)
        expTyp <- unifyTypeM(expectedType, tpe, loc)
        expEff <- unifyTypeM(Type.Empty, eff, loc)
      } yield (constrs, mkAnySchemaRowType(loc))
  }

  /**
    * Applies the given substitution `subst0` to the given body predicate `body0`.
    */
  private def reassembleBodyPredicate(body0: KindedAst.Predicate.Body, root: KindedAst.Root, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case KindedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, terms, tvar, loc) =>
      val ts = terms.map(t => reassemblePattern(t, root, subst0))
      TypedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, ts, subst0(tvar), loc)

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Body.Guard(e, loc)

    case KindedAst.Predicate.Body.Loop(varSyms, exp, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Body.Loop(varSyms, e, loc)

  }

  /**
    * Returns the relation or lattice type of `name` with the term types `ts`.
    */
  private def mkRelationOrLatticeType(name: String, den: Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation)(implicit flix: Flix): Type = den match {
    case Denotation.Relational => Type.mkRelation(ts, loc)
    case Denotation.Latticenal => Type.mkLattice(ts, loc)
  }

  /**
    * Returns the type class constraints for the given term types `ts` with the given denotation `den`.
    */
  private def getTermTypeClassConstraints(den: Ast.Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = den match {
    case Denotation.Relational =>
      ts.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root, loc))
    case Denotation.Latticenal =>
      ts.init.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root, loc)) ::: mkTypeClassConstraintsForLatticeTerm(ts.last, root, loc)
  }

  /**
    * Constructs the type class constraints for the given relational term type `tpe`.
    */
  private def mkTypeClassConstraintsForRelationalTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Boxable", root),
      PredefinedClasses.lookupClassSym("Eq", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Constructs the type class constraints for the given lattice term type `tpe`.
    */
  private def mkTypeClassConstraintsForLatticeTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Boxable", root),
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("PartialOrder", root),
      PredefinedClasses.lookupClassSym("LowerBound", root),
      PredefinedClasses.lookupClassSym("JoinLattice", root),
      PredefinedClasses.lookupClassSym("MeetLattice", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    */
  private def getSubstFromParams(params: List[KindedAst.FormalParam])(implicit flix: Flix): Substitution = {
    // Compute the substitution by mapping the symbol of each parameter to its declared type.
    val declaredTypes = params.map(_.tpe)
    (params zip declaredTypes).foldLeft(Substitution.empty) {
      case (macc, (KindedAst.FormalParam(sym, _, _, _, _), declaredType)) =>
        macc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(declaredType))
    }
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit flix: Flix): Type = {
    @tailrec
    def transformRow(tpe: Type, acc: Type => Type): Type = tpe match {
      case Type.Cst(TypeConstructor.SchemaRowEmpty, loc) =>
        acc(Type.freshVar(TypeConstructor.SchemaRowEmpty.kind, loc))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), rest, loc3) =>
        transformRow(rest, inner =>
          // copy into acc, just replacing `rest` with `inner`
          acc(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), inner, loc3))
        )
      case other => acc(other)
    }

    tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), row, loc2) =>
        Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), transformRow(row, x => x), loc2)
      case other => other
    }
  }

  /**
    * Collects all the type variables from the formal params and sets them as rigid.
    */
  private def getRigidityFromParams(params: List[KindedAst.FormalParam])(implicit flix: Flix): RigidityEnv = {
    params.flatMap(_.tpe.typeVars).foldLeft(RigidityEnv.empty) {
      case (renv, tvar) => renv.markRigid(tvar.sym)
    }
  }

  /**
    * Returns the typed version of the given type parameters `tparams0`.
    */
  private def getTypeParams(tparams0: List[KindedAst.TypeParam]): List[TypedAst.TypeParam] = tparams0.map {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /**
    * Returns the typed version of the given formal parameters `fparams0`.
    */
  private def getFormalParams(fparams0: List[KindedAst.FormalParam], subst0: Substitution): List[TypedAst.FormalParam] = fparams0.map {
    case KindedAst.FormalParam(sym, mod, tpe, src, loc) => TypedAst.FormalParam(sym, mod, subst0(tpe), src, sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaRowType(loc: SourceLocation)(implicit flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc)

  /**
    * Returns the type of the given constant.
    */
  private def constantType(cst: Ast.Constant): Type = cst match {
    case Constant.Unit => Type.Unit
    case Constant.Null => Type.Null
    case Constant.Bool(_) => Type.Bool
    case Constant.Char(_) => Type.Char
    case Constant.Float32(_) => Type.Float32
    case Constant.Float64(_) => Type.Float64
    case Constant.BigDecimal(_) => Type.BigDecimal
    case Constant.Int8(_) => Type.Int8
    case Constant.Int16(_) => Type.Int16
    case Constant.Int32(_) => Type.Int32
    case Constant.Int64(_) => Type.Int64
    case Constant.BigInt(_) => Type.BigInt
    case Constant.Str(_) => Type.Str
  }


  /**
    * Computes and prints statistics about the given substitution.
    */
  private def printStatistics(m: Map[Symbol.DefnSym, Substitution]): Unit = {
    val t = new AsciiTable().
      withTitle("Substitution Statistics").
      withCols("Def", "Type Vars", "Mean Type Size", "Median Type Size", "Total Type Size")

    for ((sym, subst) <- m) {
      val size = subst.m.size
      val sizes = subst.m.values.map(_.size.toLong).toList
      val mean = StatUtils.avg(sizes)
      val median = StatUtils.median(sizes)
      val total = sizes.sum
      t.mkRow(List(sym.toString, size, f"$mean%2.1f", median, total))
    }
    t.write(new PrintWriter(System.out))
  }
}
