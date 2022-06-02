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
import ca.uwaterloo.flix.language.ast.Ast.VarText.FallbackText
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Stratification}
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.InferMonad.seqM
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, mapN, traverse}
import ca.uwaterloo.flix.util._

import java.io.PrintWriter

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, CompilationMessage] = flix.phase("Typer") {
    val classEnv = mkClassEnv(root.classes, root.instances)
    val classesVal = visitClasses(root, classEnv, oldRoot, changeSet)
    val instancesVal = visitInstances(root, classEnv)
    val defsVal = visitDefs(root, classEnv, oldRoot, changeSet)
    val enumsVal = visitEnums(root)
    val typeAliases = visitTypeAliases(root)

    Validation.mapN(classesVal, instancesVal, defsVal, enumsVal) {
      case (classes, instances, defs, enums) =>
        val sigs = classes.values.flatMap(_.signatures).map(sig => sig.sym -> sig).toMap
        TypedAst.Root(classes, instances, sigs, defs, enums, typeAliases, root.entryPoint, root.reachable, root.sources, classEnv)
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
    case KindedAst.Class(doc, ann0, mod, sym, tparam, superClasses, sigs0, laws0, loc) =>
      val tparams = getTypeParams(List(tparam))
      val tconstr = Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), Type.KindedVar(tparam.sym, tparam.loc), sym.loc)
      val annVal = visitAnnotations(ann0, root)
      val sigsVal = traverse(sigs0.values)(visitSig(_, List(tconstr), root, classEnv))
      val lawsVal = traverse(laws0)(visitDefn(_, List(tconstr), root, classEnv))
      mapN(annVal, sigsVal, lawsVal) {
        case (ann, sigs, laws) => (sym, TypedAst.Class(doc, ann, mod, sym, tparams.head, superClasses, sigs, laws, loc))
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
        insts => insts.groupBy(inst => inst.sym.clazz)
      }
    }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Instance, TypeError] = inst match {
    case KindedAst.Instance(doc, ann0, mod, sym, tpe, tconstrs, defs0, ns, loc) =>
      val annVal = visitAnnotations(ann0, root)
      val defsVal = traverse(defs0)(visitDefn(_, tconstrs, root, classEnv))
      mapN(annVal, defsVal) {
        case (ann, defs) => TypedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, defs, ns, loc)
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
      visitSpec(spec0, root, Substitution.empty) map {
        spec => TypedAst.Sig(sym, spec, None)
      }
  }

  /**
    * Performs type inference and reassembly on the given Spec `spec`.
    */
  private def visitSpec(spec: KindedAst.Spec, root: KindedAst.Root, subst: Substitution)(implicit flix: Flix): Validation[TypedAst.Spec, TypeError] = spec match {
    case KindedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc, tpe, eff, loc) =>
      val annVal = visitAnnotations(ann0, root)
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst)
      Validation.mapN(annVal) {
        ann => TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, loc)
      }
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
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc, tpe, eff, _) =>

      ///
      /// Infer the type of the expression `exp0`.
      ///
      val result = for {
        (inferredConstrs, inferredTyp, inferredEff) <- inferExp(exp0, root)
      } yield (inferredConstrs, Type.mkUncurriedArrowWithEffect(fparams0.map(_.tpe), inferredEff, inferredTyp, loc))


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

          run(initialSubst, Map.empty) match { // TODO renv
            case Ok((subst0, renv0, (partialTconstrs, partialType))) =>

              // propogate the type variable names
              val subst = subst0.propagate

              ///
              /// The partial type returned by the inference monad does not have the substitution applied.
              ///
              val (inferredConstrs, inferredType) = (partialTconstrs.map(subst.apply), subst(partialType))

              ///
              /// Check that the inferred type is at least as general as the declared type.
              ///
              /// NB: Because the inferredType is always a function type, the effect is always implicitly accounted for.
              ///
              val inferredSc = Scheme.generalize(inferredConstrs, inferredType)
              Scheme.checkLessThanEqual(inferredSc, declaredScheme, classEnv) match {
                // Case 1: no errors, continue
                case Validation.Success(_) => // noop
                case Validation.Failure(errs) =>
                  val instanceErrs = errs.collect {
                    case UnificationError.NoMatchingInstance(tconstr) =>
                      tconstr.arg.typeConstructor match {
                        case Some(tc: TypeConstructor.Arrow) =>
                          TypeError.MissingArrowInstance(tconstr.head.sym, tconstr.arg, tconstr.loc)
                        case _ =>
                          if (tconstr.head.sym.name == "Eq")
                            TypeError.MissingEq(tconstr.arg, tconstr.loc)
                          else if (tconstr.head.sym.name == "Order")
                            TypeError.MissingOrder(tconstr.arg, tconstr.loc)
                          else if (tconstr.head.sym.name == "ToString")
                            TypeError.MissingToString(tconstr.arg, tconstr.loc)
                          else
                            TypeError.MissingInstance(tconstr.head.sym, tconstr.arg, tconstr.loc)
                      }
                  }
                  // Case 2: non instance error
                  if (instanceErrs.isEmpty) {
                    //
                    // Determine the most precise type error to emit.
                    //
                    val inferredEff = inferredSc.base.arrowEffectType
                    val declaredEff = declaredScheme.base.arrowEffectType

                    if (declaredEff == Type.Pure && inferredEff == Type.Impure) {
                      // Case 1: Declared as pure, but impure.
                      return TypeError.ImpureDeclaredAsPure(loc).toFailure
                    } else if (declaredEff == Type.Pure && inferredEff != Type.Pure) {
                      // Case 2: Declared as pure, but effect polymorphic.
                      return TypeError.EffectPolymorphicDeclaredAsPure(inferredEff, loc).toFailure
                    } else {
                      // Case 3: Check if it is the effect that cannot be generalized.
                      val inferredEffScheme = Scheme(inferredSc.quantifiers, Nil, inferredEff)
                      val declaredEffScheme = Scheme(declaredScheme.quantifiers, Nil, declaredEff)
                      Scheme.checkLessThanEqual(inferredEffScheme, declaredEffScheme, classEnv) match {
                        case Validation.Success(_) =>
                        // Case 3.1: The effect is not the problem. Regular generalization error.
                        // Fall through to below.
                        case Validation.Failure(_) =>
                          // Case 3.2: The effect cannot be generalized.
                          return TypeError.EffectGeneralizationError(declaredEff, inferredEff, loc).toFailure
                      }

                      return TypeError.GeneralizationError(declaredScheme, inferredSc, loc).toFailure
                    }
                  } else {
                    // Case 3: instance error
                    return Validation.Failure(instanceErrs)
                  }
              }

              // Pivot the substititution to keep the type parameters from being replaced
              val finalSubst = pivot(subst, tparams0)

              ///
              /// Compute the expression, type parameters, and formal parameters with the substitution applied everywhere.
              ///
              val exp = reassembleExp(exp0, root, finalSubst)
              val specVal = visitSpec(spec0, root, finalSubst)

              ///
              /// Compute a type scheme that matches the type variables that appear in the expression body.
              ///
              /// NB: It is very important to understand that: The type scheme a function is declared with must match the inferred type scheme.
              /// However, we require an even stronger property for the implementation to work. The inferred type scheme used in the rest of the
              /// compiler must *use the same type variables* in the scheme as in the body expression. Otherwise monomorphization et al. will break.
              ///
              val finalInferredType = finalSubst(partialType)
              val finalInferredTconstrs = partialTconstrs.map(finalSubst.apply)
              val inferredScheme = Scheme(finalInferredType.typeVars.toList.map(_.sym), finalInferredTconstrs, finalInferredType)

              specVal map {
                spec => (spec, TypedAst.Impl(exp, inferredScheme))
              }

            case Err(e) => Validation.Failure(LazyList(e))
          }
      }
  }

  /**
    * Computes an equ-most general substitution with the given `tparams` as rigid variables.
    */
  private def pivot(subst: Substitution, tparams: List[KindedAst.TypeParam])(implicit flix: Flix): Substitution = {
    // We only pivot Boolean type variables
    val boolTparams = tparams.filter(tparam => tparam.sym.kind == Kind.Bool)
    boolTparams match {
      // Case 1: exactly one boolean type parameter.
      case tparam :: Nil => subst.pivot(tparam.sym)
      // Case 2: multiple or zero boolean type parameters. No action
      case _ => subst
    }
  }

  /**
    * Performs type inference and reassembly on all enums in the given AST root.
    */
  private def visitEnums(root: KindedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.EnumSym, TypedAst.Enum], TypeError] =
    flix.subphase("Enums") {
      // Visit every enum in the ast.
      val result = root.enums.toList.map {
        case (_, enum) => visitEnum(enum, root)
      }

      // Sequence the results and convert them back to a map.
      Validation.sequence(result).map(_.toMap)
    }

  /**
    * Performs type resolution on the given enum and its cases.
    */
  private def visitEnum(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): Validation[(Symbol.EnumSym, TypedAst.Enum), TypeError] = enum0 match {
    case KindedAst.Enum(doc, ann, mod, enumSym, tparams0, derives, cases0, tpeDeprecated, sc, loc) =>
      val annVal = visitAnnotations(ann, root)
      val tparams = getTypeParams(tparams0)
      val cases = cases0 map {
        case (name, KindedAst.Case(_, tagName, tagType, tagScheme)) =>
          name -> TypedAst.Case(enumSym, tagName, tagType, tagScheme, tagName.loc)
      }

      Validation.mapN(annVal) {
        ann => enumSym -> TypedAst.Enum(doc, ann, mod, enumSym, tparams, derives, cases, tpeDeprecated, sc, loc)
      }
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
    * Visits all annotations.
    */
  private def visitAnnotations(ann: List[KindedAst.Annotation], root: KindedAst.Root)(implicit flix: Flix): Validation[List[TypedAst.Annotation], TypeError] = {
    traverse(ann)(inferAnnotation(_, root))
  }

  /**
    * Performs type inference on the given annotation `ann0`.
    */
  private def inferAnnotation(ann0: KindedAst.Annotation, root: KindedAst.Root)(implicit flix: Flix): Validation[TypedAst.Annotation, TypeError] = ann0 match {
    case KindedAst.Annotation(name, exps, loc) =>
      //
      // Perform type inference on the arguments.
      //
      val result = for {
        (constrs, tpes, effs) <- seqM(exps.map(inferExp(_, root))).map(_.unzip3)
        _ <- unifyTypeM(Type.Pure :: effs, loc)
      } yield Type.Int32

      //
      // Run the type inference monad with an empty substitution.
      //
      val initialSubst = Substitution.empty
      result.run(initialSubst, Map.empty).toValidation.map { // TODO renv
        case (subst, _, _) =>
          val es = exps.map(reassembleExp(_, root, subst))
          TypedAst.Annotation(name, es, loc)
      }
  }

  /**
    * Infers the type of the given expression `exp0`.
    */
  private def inferExp(exp0: KindedAst.Expression, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: KindedAst.Expression): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = e0 match {

      case KindedAst.Expression.Wild(tvar, _) =>
        liftM(List.empty, tvar, Type.Pure)

      case KindedAst.Expression.Var(sym, tpe, loc) =>
        for {
          resultTyp <- unifyTypeM(sym.tvar.ascribedWith(Kind.Star), tpe, loc)
        } yield (List.empty, resultTyp, Type.Pure)

      case KindedAst.Expression.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs0, defType) = Scheme.instantiate(defn.spec.sc, InstantiateMode.Flexible)
        for {
          resultTyp <- unifyTypeM(tvar, defType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure)

      case KindedAst.Expression.Sig(sym, tvar, loc) =>
        // find the declared signature corresponding to this symbol
        val sig = root.classes(sym.clazz).sigs(sym)
        val (tconstrs0, sigType) = Scheme.instantiate(sig.spec.sc, InstantiateMode.Flexible)
        for {
          resultTyp <- unifyTypeM(tvar, sigType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure)

      case KindedAst.Expression.Hole(sym, tvar, _) =>
        liftM(List.empty, tvar, Type.Pure)

      case KindedAst.Expression.Unit(_) =>
        liftM(List.empty, Type.Unit, Type.Pure)

      case KindedAst.Expression.Null(_) =>
        liftM(List.empty, Type.Null, Type.Pure)

      case KindedAst.Expression.True(_) =>
        liftM(List.empty, Type.Bool, Type.Pure)

      case KindedAst.Expression.False(_) =>
        liftM(List.empty, Type.Bool, Type.Pure)

      case KindedAst.Expression.Char(lit, _) =>
        liftM(List.empty, Type.Char, Type.Pure)

      case KindedAst.Expression.Float32(lit, _) =>
        liftM(List.empty, Type.Float32, Type.Pure)

      case KindedAst.Expression.Float64(lit, _) =>
        liftM(List.empty, Type.Float64, Type.Pure)

      case KindedAst.Expression.Int8(lit, _) =>
        liftM(List.empty, Type.Int8, Type.Pure)

      case KindedAst.Expression.Int16(lit, _) =>
        liftM(List.empty, Type.Int16, Type.Pure)

      case KindedAst.Expression.Int32(lit, _) =>
        liftM(List.empty, Type.Int32, Type.Pure)

      case KindedAst.Expression.Int64(lit, _) =>
        liftM(List.empty, Type.Int64, Type.Pure)

      case KindedAst.Expression.BigInt(lit, _) =>
        liftM(List.empty, Type.BigInt, Type.Pure)

      case KindedAst.Expression.Str(lit, _) =>
        liftM(List.empty, Type.Str, Type.Pure)

      case KindedAst.Expression.Default(tvar, _) =>
        liftM(List.empty, tvar, Type.Pure)

      case KindedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val argType = fparam.tpe
        val argTypeVar = fparam.sym.tvar.ascribedWith(Kind.Star)
        for {
          (constrs, bodyType, bodyEff) <- visitExp(exp)
          _ <- unifyTypeM(argType, argTypeVar, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArrowWithEffect(argType, bodyEff, bodyType, loc), loc)
        } yield (constrs, resultTyp, Type.Pure)

      case KindedAst.Expression.Apply(exp, exps, tvar, evar, loc) =>
        val lambdaBodyType = Type.freshVar(Kind.Star, loc, text = FallbackText("result"))
        val lambdaBodyEff = Type.freshVar(Kind.Bool, loc, text = FallbackText("eff"))
        for {
          (constrs1, tpe, eff) <- visitExp(exp)
          (constrs2, tpes, effs) <- seqM(exps.map(visitExp)).map(_.unzip3)
          lambdaType <- unifyTypeM(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyEff, lambdaBodyType, loc), loc)
          resultTyp <- unifyTypeM(tvar, lambdaBodyType, loc)
          resultEff <- unifyBoolM(evar, Type.mkAnd(lambdaBodyEff :: eff :: effs, loc), loc)
          _ <- unbindVar(lambdaBodyType) // NB: Safe to unbind since the variable is not used elsewhere.
          _ <- unbindVar(lambdaBodyEff) // NB: Safe to unbind since the variable is not used elsewhere.
        } yield (constrs1 ++ constrs2.flatten, resultTyp, resultEff)

      case KindedAst.Expression.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOperator.BoolOp.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Bool, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Float32Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float32, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Float64Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float64, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int8Op.Neg | SemanticOperator.Int8Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int8, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int16Op.Neg | SemanticOperator.Int16Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int16, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int32Op.Neg | SemanticOperator.Int32Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int32, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int64Op.Neg | SemanticOperator.Int64Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int64, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.BigIntOp.Neg | SemanticOperator.BigIntOp.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.BigInt, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop' near ${loc.format}.")
      }

      case KindedAst.Expression.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOperator.BoolOp.And | SemanticOperator.BoolOp.Or =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Bool, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Float32Op.Add | SemanticOperator.Float32Op.Sub | SemanticOperator.Float32Op.Mul | SemanticOperator.Float32Op.Div
             | SemanticOperator.Float32Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float32, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Float64Op.Add | SemanticOperator.Float64Op.Sub | SemanticOperator.Float64Op.Mul | SemanticOperator.Float64Op.Div
             | SemanticOperator.Float64Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float64, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int8Op.Add | SemanticOperator.Int8Op.Sub | SemanticOperator.Int8Op.Mul | SemanticOperator.Int8Op.Div
             | SemanticOperator.Int8Op.Rem | SemanticOperator.Int8Op.Exp
             | SemanticOperator.Int8Op.And | SemanticOperator.Int8Op.Or | SemanticOperator.Int8Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int8, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int8, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int8, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int16Op.Add | SemanticOperator.Int16Op.Sub | SemanticOperator.Int16Op.Mul | SemanticOperator.Int16Op.Div
             | SemanticOperator.Int16Op.Rem | SemanticOperator.Int16Op.Exp
             | SemanticOperator.Int16Op.And | SemanticOperator.Int16Op.Or | SemanticOperator.Int16Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int16, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int16, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int16, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int32Op.Add | SemanticOperator.Int32Op.Sub | SemanticOperator.Int32Op.Mul | SemanticOperator.Int32Op.Div
             | SemanticOperator.Int32Op.Rem | SemanticOperator.Int32Op.Exp
             | SemanticOperator.Int32Op.And | SemanticOperator.Int32Op.Or | SemanticOperator.Int32Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int32, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int64Op.Add | SemanticOperator.Int64Op.Sub | SemanticOperator.Int64Op.Mul | SemanticOperator.Int64Op.Div
             | SemanticOperator.Int64Op.Rem | SemanticOperator.Int64Op.Exp
             | SemanticOperator.Int64Op.And | SemanticOperator.Int64Op.Or | SemanticOperator.Int64Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int64, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.BigIntOp.Add | SemanticOperator.BigIntOp.Sub | SemanticOperator.BigIntOp.Mul | SemanticOperator.BigIntOp.Div
             | SemanticOperator.BigIntOp.Rem | SemanticOperator.BigIntOp.Exp
             | SemanticOperator.BigIntOp.And | SemanticOperator.BigIntOp.Or | SemanticOperator.BigIntOp.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.BigInt, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.BigInt, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.BigInt, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int8Op.Shl | SemanticOperator.Int8Op.Shr
             | SemanticOperator.Int16Op.Shl | SemanticOperator.Int16Op.Shr
             | SemanticOperator.Int32Op.Shl | SemanticOperator.Int32Op.Shr
             | SemanticOperator.Int64Op.Shl | SemanticOperator.Int64Op.Shr
             | SemanticOperator.BigIntOp.Shl | SemanticOperator.BigIntOp.Shr =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- unifyTypeM(tvar, tpe1, loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, lhs, resultEff)

        case SemanticOperator.BoolOp.Eq | SemanticOperator.BoolOp.Neq
             | SemanticOperator.CharOp.Eq | SemanticOperator.CharOp.Neq
             | SemanticOperator.Float32Op.Eq | SemanticOperator.Float32Op.Neq
             | SemanticOperator.Float64Op.Eq | SemanticOperator.Float64Op.Neq
             | SemanticOperator.Int8Op.Eq | SemanticOperator.Int8Op.Neq
             | SemanticOperator.Int16Op.Eq | SemanticOperator.Int16Op.Neq
             | SemanticOperator.Int32Op.Eq | SemanticOperator.Int32Op.Neq
             | SemanticOperator.Int64Op.Eq | SemanticOperator.Int64Op.Neq
             | SemanticOperator.BigIntOp.Eq | SemanticOperator.BigIntOp.Neq
             | SemanticOperator.StringOp.Eq | SemanticOperator.StringOp.Neq =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.CharOp.Lt | SemanticOperator.CharOp.Le | SemanticOperator.CharOp.Gt | SemanticOperator.CharOp.Ge
             | SemanticOperator.Float32Op.Lt | SemanticOperator.Float32Op.Le | SemanticOperator.Float32Op.Gt | SemanticOperator.Float32Op.Ge
             | SemanticOperator.Float64Op.Lt | SemanticOperator.Float64Op.Le | SemanticOperator.Float64Op.Gt | SemanticOperator.Float64Op.Ge
             | SemanticOperator.Int8Op.Lt | SemanticOperator.Int8Op.Le | SemanticOperator.Int8Op.Gt | SemanticOperator.Int8Op.Ge
             | SemanticOperator.Int16Op.Lt | SemanticOperator.Int16Op.Le | SemanticOperator.Int16Op.Gt | SemanticOperator.Int16Op.Ge
             | SemanticOperator.Int32Op.Lt | SemanticOperator.Int32Op.Le | SemanticOperator.Int32Op.Gt | SemanticOperator.Int32Op.Ge
             | SemanticOperator.Int64Op.Lt | SemanticOperator.Int64Op.Le | SemanticOperator.Int64Op.Gt | SemanticOperator.Int64Op.Ge
             | SemanticOperator.BigIntOp.Lt | SemanticOperator.BigIntOp.Le | SemanticOperator.BigIntOp.Gt | SemanticOperator.BigIntOp.Ge =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.StringOp.Concat =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Str, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Str, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Str, loc)
            resultEff = Type.mkAnd(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop' near ${loc.format}.")
      }

      case KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          condType <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
          resultTyp <- unifyTypeM(tpe2, tpe3, loc)
          resultEff = Type.mkAnd(eff1, eff2, eff3, loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expression.Stm(exp1, exp2, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkAnd(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.Discard(exp, loc) =>
        for {
          (constrs, _, eff) <- visitExp(exp)
          resultTyp = Type.Unit
        } yield (constrs, resultTyp, eff)

      case KindedAst.Expression.Let(sym, mod, exp1, exp2, loc) =>
        // Note: The call to unify on sym.tvar occurs immediately after we have inferred the type of exp1.
        // This ensures that uses of sym inside exp2 are type checked according to this type.
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          boundVar <- unifyTypeM(sym.tvar.ascribedWith(Kind.Star), tpe1, loc)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkAnd(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc) =>
        // Ensure that `exp1` is a lambda.
        val a = Type.freshVar(Kind.Star, loc, text = FallbackText("arg"))
        val b = Type.freshVar(Kind.Star, loc, text = FallbackText("result"))
        val ef = Type.freshVar(Kind.Bool, loc, text = FallbackText("eff"))
        val expectedType = Type.mkArrowWithEffect(a, ef, b, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          arrowTyp <- unifyTypeM(expectedType, tpe1, loc)
          boundVar <- unifyTypeM(sym.tvar.ascribedWith(Kind.Star), tpe1, loc)
          resultTyp = tpe2
          resultEff = Type.mkAnd(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.Region(tpe, _) =>
        liftM(Nil, tpe, Type.Pure)

      case KindedAst.Expression.Scope(sym, regionVar, exp, evar, loc) =>
        for {
          _ <- unifyTypeM(sym.tvar.ascribedWith(Kind.Star), Type.mkRegion(regionVar, loc), loc)
          (constrs, tpe, eff) <- visitExp(exp)
          purifiedEff <- purifyEffM(regionVar, eff)
          resultEff <- unifyTypeM(evar, purifiedEff, loc)
          _ <- noEscapeM(regionVar, tpe)
          resultTyp = tpe
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.Match(exp, rules, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.map(_.guard)
        val bodies = rules.map(_.exp)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          patternType <- unifyTypeM(tpe :: patternTypes, loc)
          (guardConstrs, guardTypes, guardEffects) <- seqM(guards map visitExp).map(_.unzip3)
          guardType <- unifyTypeM(Type.Bool :: guardTypes, loc)
          (bodyConstrs, bodyTypes, bodyEffects) <- seqM(bodies map visitExp).map(_.unzip3)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultEff = Type.mkAnd(eff :: guardEffects ::: bodyEffects, loc)
        } yield (constrs ++ guardConstrs.flatten ++ bodyConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expression.Choose(star, exps0, rules0, tvar, loc) =>

        /**
          * Performs type inference on the given match expressions `exps` and nullity `vars`.
          *
          * Returns a pair of lists of the types and effects of the match expressions.
          */
        def visitMatchExps(exps: List[KindedAst.Expression], isAbsentVars: List[Type.KindedVar], isPresentVars: List[Type.KindedVar]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type])] = {
          def visitMatchExp(exp: KindedAst.Expression, isAbsentVar: Type.KindedVar, isPresentVar: Type.KindedVar): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
            val freshElmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
            for {
              (constrs, tpe, eff) <- visitExp(exp)
              _ <- unifyTypeM(tpe, Type.mkChoice(freshElmVar, isAbsentVar, isPresentVar, loc), loc)
            } yield (constrs, freshElmVar, eff)
          }

          seqM(exps.zip(isAbsentVars.zip(isPresentVars)).map {
            case (matchExp, (isAbsentVar, isPresentVar)) => visitMatchExp(matchExp, isAbsentVar, isPresentVar)
          }).map(_.unzip3)
        }

        /**
          * Performs type inference of the given null rules `rs`.
          *
          * Returns a pair of list of the types and effects of the rule expressions.
          */
        def visitRuleBodies(rs: List[KindedAst.ChoiceRule]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type])] = {
          def visitRuleBody(r: KindedAst.ChoiceRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = r match {
            case KindedAst.ChoiceRule(_, exp0) => visitExp(exp0)
          }

          seqM(rs.map(visitRuleBody)).map(_.unzip3)
        }

        /**
          * Returns a transformed result type that encodes the Boolean constraint of each row pattern in the result type.
          *
          * NB: Requires that the `ts` types are Choice-types.
          */
        def transformResultTypes(isAbsentVars: List[Type.KindedVar], isPresentVars: List[Type.KindedVar], rs: List[KindedAst.ChoiceRule], ts: List[Type], loc: SourceLocation): InferMonad[Type] = {
          def visitRuleBody(r: KindedAst.ChoiceRule, resultType: Type): InferMonad[(Type, Type, Type)] = r match {
            case KindedAst.ChoiceRule(r, exp0) =>
              val cond = mkOverApprox(isAbsentVars, isPresentVars, r)
              val innerType = Type.freshVar(Kind.Star, exp0.loc, text = FallbackText("inner"))
              val isAbsentVar = Type.freshVar(Kind.Bool, exp0.loc, text = FallbackText("isAbs"))
              val isPresentVar = Type.freshVar(Kind.Bool, exp0.loc, text = FallbackText("isPres"))
              for {
                choiceType <- unifyTypeM(resultType, Type.mkChoice(innerType, isAbsentVar, isPresentVar, loc), loc)
              } yield (Type.mkAnd(cond, isAbsentVar, loc), Type.mkAnd(cond, isPresentVar, loc), innerType)
          }

          ///
          /// Simply compute the mgu of the `ts` types if this is not a star choose.
          ///
          if (!star) {
            return unifyTypeM(ts, loc)
          }

          ///
          /// Otherwise construct a new Choice type with isAbsent and isPresent conditions that depend on each pattern row.
          ///
          for {
            (isAbsentConds, isPresentConds, innerTypes) <- seqM(rs.zip(ts).map(p => visitRuleBody(p._1, p._2))).map(_.unzip3)
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
        def mkUnderApprox(isAbsentVars: List[Type.KindedVar], isPresentVars: List[Type.KindedVar], r: List[KindedAst.ChoicePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, KindedAst.ChoicePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), KindedAst.ChoicePattern.Present(_, _, _))) =>
              // Case 2: A `Present` pattern forces the `isAbsentVar` to be equal to `false`.
              BoolUnification.mkAnd(acc, Type.mkEquiv(isAbsentVar, Type.False, loc))
            case (acc, ((_, isPresentVar), KindedAst.ChoicePattern.Absent(_))) =>
              // Case 3: An `Absent` pattern forces the `isPresentVar` to be equal to `false`.
              BoolUnification.mkAnd(acc, Type.mkEquiv(isPresentVar, Type.False, loc))
          }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an over-approximation.
          *
          * If a pattern is a wildcard it *may* always match.
          * If a pattern is `Absent` it *may* match if its corresponding `isAbsent` is `true`.
          * If a pattern is `Present` it *may* match if its corresponding `isPresentVar`is `true`.
          */
        def mkOverApprox(isAbsentVars: List[Type.KindedVar], isPresentVars: List[Type.KindedVar], r: List[KindedAst.ChoicePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, KindedAst.ChoicePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), KindedAst.ChoicePattern.Absent(_))) =>
              // Case 2: An `Absent` pattern may match if the `isAbsentVar` is `true`.
              BoolUnification.mkAnd(acc, isAbsentVar)
            case (acc, ((_, isPresentVar), KindedAst.ChoicePattern.Present(_, _, _))) =>
              // Case 3: A `Present` pattern may match if the `isPresentVar` is `true`.
              BoolUnification.mkAnd(acc, isPresentVar)
          }

        /**
          * Constructs a disjunction of the constraints of each choice rule.
          */
        def mkOuterDisj(m: List[List[KindedAst.ChoicePattern]], isAbsentVars: List[Type.KindedVar], isPresentVars: List[Type.KindedVar]): Type =
          m.foldLeft(Type.False) {
            case (acc, rule) => BoolUnification.mkOr(acc, mkUnderApprox(isAbsentVars, isPresentVars, rule))
          }

        /**
          * Performs type inference and unification with the `matchTypes` against the given choice rules `rs`.
          */
        def unifyMatchTypesAndRules(matchTypes: List[Type], rs: List[KindedAst.ChoiceRule]): InferMonad[List[List[Type]]] = {
          def unifyWithRule(r: KindedAst.ChoiceRule): InferMonad[List[Type]] = {
            seqM(matchTypes.zip(r.pat).map {
              case (matchType, KindedAst.ChoicePattern.Wild(_)) =>
                // Case 1: The pattern is wildcard. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, KindedAst.ChoicePattern.Absent(_)) =>
                // Case 2: The pattern is a `Absent`. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, KindedAst.ChoicePattern.Present(sym, tvar, loc)) =>
                // Case 3: The pattern is `Present`. Must constraint the type of the local variable with the type of the match expression.
                unifyTypeM(matchType, sym.tvar.ascribedWith(Kind.Star), tvar, loc)
            })
          }

          seqM(rs.map(unifyWithRule))
        }

        //
        // Introduce an isAbsent variable for each match expression in `exps`.
        //
        val isAbsentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc, text = FallbackText("isAbs")))

        //
        // Introduce an isPresent variable for each math expression in `exps`.
        //
        val isPresentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc, text = FallbackText("isPres")))

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
          (matchConstrs, matchTyp, matchEff) <- visitMatchExps(exps0, isAbsentVars, isPresentVars)
          _ <- unifyMatchTypesAndRules(matchTyp, rules0)
          (ruleBodyConstrs, ruleBodyTyp, ruleBodyEff) <- visitRuleBodies(rules0)
          resultTypes <- transformResultTypes(isAbsentVars, isPresentVars, rules0, ruleBodyTyp, loc)
          resultTyp <- unifyTypeM(tvar, resultTypes, loc)
          resultEff = Type.mkAnd(matchEff ::: ruleBodyEff, loc)
        } yield (matchConstrs.flatten ++ ruleBodyConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        if (sym == Symbol.mkEnumSym("Choice")) {
          //
          // Special Case 1: Absent or Present Tag
          //
          if (tag.name == "Absent") {
            // Case 1.1: Absent Tag.
            val elmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
            val isAbsent = Type.True
            val isPresent = Type.freshVar(Kind.Bool, loc, text = FallbackText("isPres"))
            for {
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(elmVar, isAbsent, isPresent, loc), loc)
              resultEff = Type.Pure
            } yield (List.empty, resultTyp, resultEff)
          }
          else if (tag.name == "Present") {
            // Case 1.2: Present Tag.
            val isAbsent = Type.freshVar(Kind.Bool, loc, text = FallbackText("isAbs"))
            val isPresent = Type.True
            for {
              (constrs, tpe, eff) <- visitExp(exp)
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(tpe, isAbsent, isPresent, loc), loc)
              resultEff = eff
            } yield (constrs, resultTyp, resultEff)
          } else {
            // Case 1.3: Unknown tag.
            throw InternalCompilerException(s"Unexpected choice tag: '$tag' near ${loc.format}.")
          }
        } else {
          //
          // General Case:
          //

          // Lookup the enum declaration.
          val decl = root.enums(sym)

          // Lookup the case declaration.
          val caze = decl.cases(tag)

          // Instantiate the type scheme of the case.
          val (_, tagType) = Scheme.instantiate(caze.sc, InstantiateMode.Flexible)

          //
          // The tag type can be thought of as a function from the type of variant to the type of the enum.
          // See Type.mkTag for details.
          //
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            _ <- unifyTypeM(tagType, Type.mkTag(sym, tag, tpe, tvar, loc), loc)
            resultTyp = tvar
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)
        }

      case KindedAst.Expression.Tuple(elms, loc) =>
        for {
          (elementConstrs, elementTypes, elementEffects) <- seqM(elms.map(visitExp)).map(_.unzip3)
          resultEff = Type.mkAnd(elementEffects, loc)
        } yield (elementConstrs.flatten, Type.mkTuple(elementTypes, loc), resultEff)

      case KindedAst.Expression.RecordEmpty(loc) =>
        liftM(List.empty, Type.mkRecord(Type.RecordRowEmpty, loc), Type.Pure)

      case KindedAst.Expression.RecordSelect(exp, field, tvar, loc) =>
        //
        // r : { field = tpe | row }
        // -------------------------
        //       r.field : tpe
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc, text = FallbackText("row"))
        val expectedRowType = Type.mkRecordRowExtend(field, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, expectedRecordType, loc)
          resultEff = eff
        } yield (constrs, tvar, resultEff)

      case KindedAst.Expression.RecordExtend(field, exp1, exp2, tvar, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { field = exp1 | exp2 } : { field  :: tpe | r }
        //
        val restRow = Type.freshVar(Kind.RecordRow, loc, text = FallbackText("row"))
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe2, Type.mkRecord(restRow, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(Type.mkRecordRowExtend(field, tpe1, restRow, loc), loc), loc)
          resultEff = Type.mkAnd(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.RecordRestrict(field, exp, tvar, loc) =>
        //
        //  exp : { field  :: t | r }
        // -------------------------
        // { -field | exp } : {| r }
        //
        val freshFieldType = Type.freshVar(Kind.Star, loc, text = FallbackText("field"))
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc, text = FallbackText("row"))
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, Type.mkRecord(Type.mkRecordRowExtend(field, freshFieldType, freshRowVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(freshRowVar, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.ArrayLit(exps, exp, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, elmTypes, eff1) <- seqM(exps.map(visitExp)).map(_.unzip3)
          (constrs2, tpe2, eff2) <- visitExp(exp)
          _ <- expectTypeM(expected = regionType, actual = tpe2, loc)
          elmTyp <- unifyTypeAllowEmptyM(elmTypes, Kind.Star, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkScopedArray(elmTyp, regionVar, loc), loc)
          resultEff <- unifyTypeM(evar, Type.mkAnd(Type.mkAnd(eff1, loc), eff2, regionVar, loc), loc)
        } yield (constrs1.flatten ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = regionType, actual = tpe3, loc)
          lenType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultTyp <- unifyTypeM(tvar, Type.mkScopedArray(tpe1, regionVar, loc), loc)
          resultEff <- unifyTypeM(evar, Type.mkAnd(eff1, eff2, eff3, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expression.ArrayLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(Type.mkScopedArray(elmVar, regionVar, loc), tpe, exp.loc)
          resultTyp = Type.Int32
          resultEff = Type.Pure
          _ <- unbindVar(elmVar)
          _ <- unbindVar(regionVar)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          arrayType <- expectTypeM(expected = Type.mkScopedArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
          indexType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultEff = Type.mkAnd(regionVar, eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, tvar, resultEff)

      case KindedAst.Expression.ArrayStore(exp1, exp2, exp3, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        val arrayType = Type.mkScopedArray(elmVar, regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe3, exp3.loc)
          resultTyp = Type.Unit
          resultEff = Type.mkAnd(List(regionVar, eff1, eff2, eff3), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expression.ArraySlice(exp1, exp2, exp3, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        val arrayType = Type.mkScopedArray(elmVar, regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
          resultTyp <- expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
          resultEff = Type.mkAnd(List(regionVar, eff1, eff2, eff3), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expression.Ref(exp1, exp2, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe2, regionType, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkScopedRef(tpe1, regionVar, loc), loc)
          resultEff <- unifyTypeM(evar, Type.mkAnd(eff1, eff2, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.Deref(exp, tvar, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, Rigidity.Flexible, text = FallbackText("elm"))
        val regionVar = Type.freshVar(Kind.Bool, loc, Rigidity.Flexible, text = FallbackText("region"))
        val refType = Type.mkScopedRef(elmVar, regionVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = refType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultEff <- unifyTypeM(evar, Type.mkAnd(eff, regionVar, loc), loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.Assign(exp1, exp2, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, Rigidity.Flexible, text = FallbackText("elm"))
        val regionVar = Type.freshVar(Kind.Bool, loc, text = FallbackText("region"))
        val refType = Type.mkScopedRef(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = refType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff <- unifyTypeM(evar, Type.mkAnd(eff1, eff2, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.Ascribe(exp, expectedTyp, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (constrs, actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- expectTypeM(expected = expectedTyp.getOrElse(Type.freshVar(Kind.Star, loc, text = FallbackText("tpe"))), actual = actualTyp, bind = tvar, loc)
          resultEff <- expectTypeM(expected = expectedEff.getOrElse(Type.freshVar(Kind.Bool, loc, text = FallbackText("eff"))), actual = actualEff, loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.Cast(exp, declaredTyp, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        for {
          (constrs, actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, declaredTyp.getOrElse(actualTyp), loc)
          resultEff = declaredEff.getOrElse(actualEff)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.Without(exp, eff, loc) => visitExp(exp) // TODO actually infer

      case KindedAst.Expression.TryCatch(exp, rules, loc) =>
        val rulesType = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          (ruleConstrs, ruleTypes, ruleEffects) <- seqM(rulesType).map(_.unzip3)
          ruleType <- unifyTypeM(ruleTypes, loc)
          resultTyp <- unifyTypeM(tpe, ruleType, loc)
          resultEff = Type.mkAnd(eff :: ruleEffects, loc)
        } yield (constrs ++ ruleConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expression.TryWith(exp, eff, rules, loc) => visitExp(exp) // TODO actually infer

      case KindedAst.Expression.Do(op, args, loc) => seqM(args.map(visitExp)).map(_.unzip3) // TODO actually infer

      case KindedAst.Expression.Resume(args, loc) => seqM(args.map(visitExp)).map(_.unzip3) // TODO actually infer

      case KindedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val classType = getFlixType(constructor.getDeclaringClass)
        for {
          (constrs, _, _) <- seqM(args.map(visitExp)).map(_.unzip3)
          resultTyp = classType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expression.InvokeMethod(method, exp, args, loc) =>
        val classType = getFlixType(method.getDeclaringClass)
        val returnType = getFlixType(method.getReturnType)
        for {
          (baseConstrs, baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          (constrs, tpes, effs) <- seqM(args.map(visitExp)).map(_.unzip3)
          resultTyp = getFlixType(method.getReturnType)
          resultEff = Type.Impure
        } yield (baseConstrs ++ constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val returnType = getFlixType(method.getReturnType)
        for {
          (constrs, tpes, effs) <- seqM(args.map(visitExp)).map(_.unzip3)
          resultTyp = returnType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expression.GetField(field, exp, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(field.getDeclaringClass)
        for {
          (constrs, tpe, _) <- visitExp(exp)
          objectTyp <- expectTypeM(expected = classType, actual = tpe, exp.loc)
          resultTyp = fieldType
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.PutField(field, exp1, exp2, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(field.getDeclaringClass)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = classType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = fieldType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.GetStaticField(field, loc) =>
        val fieldType = getFlixType(field.getType)
        val resultTyp = fieldType
        val resultEff = Type.Impure
        liftM(List.empty, resultTyp, resultEff)

      case KindedAst.Expression.PutStaticField(field, exp, loc) =>
        for {
          (valueConstrs, valueTyp, _) <- visitExp(exp)
          fieldTyp <- expectTypeM(expected = getFlixType(field.getType), actual = valueTyp, exp.loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (valueConstrs, resultTyp, resultEff)

      case KindedAst.Expression.NewChannel(exp, declaredType, loc) =>
        for {
          (constrs, tpe, _) <- visitExp(exp)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe, exp.loc)
          resultTyp <- liftM(Type.mkChannel(declaredType, loc))
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.GetChannel(exp, tvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
        val channelType = Type.mkChannel(elmVar, loc)

        for {
          (constrs, tpe, _) <- visitExp(exp)
          _ <- expectTypeM(expected = channelType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.PutChannel(exp1, exp2, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
        val channelType = Type.mkChannel(elmVar, loc)

        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = channelType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.mkUnit(loc)
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.SelectChannel(rules, default, tvar, loc) =>

        /**
          * Performs type inference on the given select rule `sr0`.
          */
        def inferSelectRule(sr0: KindedAst.SelectChannelRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          sr0 match {
            case KindedAst.SelectChannelRule(sym, chan, body) => for {
              (chanConstrs, chanType, _) <- visitExp(chan)
              (bodyConstrs, bodyType, _) <- visitExp(body)
              _ <- unifyTypeM(chanType, Type.mkChannel(sym.tvar.ascribedWith(Kind.Star), sym.loc), sym.loc)
              resultCon = chanConstrs ++ bodyConstrs
              resultTyp = bodyType
              resultEff = Type.Impure
            } yield (resultCon, resultTyp, resultEff)
          }

        /**
          * Performs type inference on the given optional default expression `exp0`.
          */
        def inferDefaultRule(exp0: Option[KindedAst.Expression]): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          exp0 match {
            case None => liftM(Nil, Type.freshVar(Kind.Star, loc, text = FallbackText("default")), Type.Pure)
            case Some(exp) => visitExp(exp)
          }

        for {
          (ruleConstrs, ruleTypes, _) <- seqM(rules.map(inferSelectRule)).map(_.unzip3)
          (defaultConstrs, defaultType, _) <- inferDefaultRule(default)
          resultCon = ruleConstrs.flatten ++ defaultConstrs
          resultTyp <- unifyTypeM(tvar :: defaultType :: ruleTypes, loc)
          resultEff = Type.Impure
        } yield (resultCon, resultTyp, resultEff)

      case KindedAst.Expression.Spawn(exp, loc) =>
        for {
          (constrs, tpe, _) <- visitExp(exp)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.Lazy(exp, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp = Type.mkLazy(tpe, loc)
          resultEff <- expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.Force(exp, tvar, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          lazyTyp <- expectTypeM(expected = Type.mkLazy(tvar, loc), actual = tpe, exp.loc)
          resultTyp = tvar
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.FixpointConstraintSet(cs, tvar, loc) =>
        for {
          (constrs, constraintTypes) <- seqM(cs.map(visitConstraint)).map(_.unzip)
          schemaRow <- unifyTypeAllowEmptyM(constraintTypes, Kind.SchemaRow, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(schemaRow, loc), loc)
        } yield (constrs.flatten, resultTyp, Type.Pure)

      case KindedAst.Expression.FixpointLambda(pparams, exp, tvar, loc) =>

        def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
          case KindedAst.PredicateParam(pred, tpe, loc) => Type.mkSchemaRowExtend(pred, tpe, restRow, tpe.loc)
        }

        def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

        val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row")))
        val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row")))

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- unifyTypeM(tpe, Type.mkSchema(expectedRowType, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(resultRowType, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tpe1, tpe2, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultEff = Type.mkAnd(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.FixpointSolve(exp, loc) =>
        //
        //  exp : #{...}
        //  ---------------
        //  solve exp : tpe
        //
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tpe, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.FixpointFilter(pred, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : #{ P : a  | b }
        //  -------------------------------------------
        //  project P exp2 : #{ P : a | c }
        //
        val freshPredicateTypeVar = Type.freshVar(Kind.Predicate, loc, text = FallbackText("pred"))
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row"))
        val freshResultSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("result"))

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshRestSchemaTypeVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshResultSchemaTypeVar, loc), loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expression.FixpointProjectIn(exp, pred, tvar, loc) =>
        //
        //  exp : F[freshElmType] where F is Foldable
        //  -------------------------------------------
        //  project exp into A: #{A(freshElmType) | freshRestSchemaType}
        //
        val freshTypeConstructorVar = Type.freshVar(Kind.Star ->: Kind.Star, loc, text = FallbackText("tycon"))
        val freshElmTypeVar = Type.freshVar(Kind.Star, loc, text = FallbackText("elm"))
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row"))

        // Require Boxable and Foldable instances.
        val boxableSym = PredefinedClasses.lookupClassSym("Boxable", root)
        val foldableSym = PredefinedClasses.lookupClassSym("Foldable", root)
        val boxable = Ast.TypeConstraint(Ast.TypeConstraint.Head(boxableSym, loc), freshElmTypeVar, loc)
        val foldable = Ast.TypeConstraint(Ast.TypeConstraint.Head(foldableSym, loc), freshTypeConstructorVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkApply(freshTypeConstructorVar, List(freshElmTypeVar), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(List(freshElmTypeVar), loc), freshRestSchemaTypeVar, loc), loc), loc)
          resultEff = eff
        } yield (boxable :: foldable :: constrs, resultTyp, resultEff)

      case KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tvar, loc) =>
        //
        //  exp1: {$Result(freshRelOrLat, freshTupleVar) | freshRestSchemaVar }
        //  exp2: freshRestSchemaVar
        //  --------------------------------------------------------------------
        //  FixpointQuery pred, exp1, exp2 : Array[freshTupleVar]
        //
        val freshRelOrLat = Type.freshVar(Kind.Star ->: Kind.Predicate, loc, text = FallbackText("pred"))
        val freshTupleVar = Type.freshVar(Kind.Star, loc, text = FallbackText("tuple"))
        val freshRestSchemaVar = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row"))
        val expectedSchemaType = Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.Apply(freshRelOrLat, freshTupleVar, loc), freshRestSchemaVar, loc), loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe1, expectedSchemaType, loc)
          _ <- unifyTypeM(tpe2, Type.mkSchema(freshRestSchemaVar, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(freshTupleVar, loc), loc)
          resultEff = Type.mkAnd(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expression.Reify(t, loc) =>
        liftM(Nil, Type.Bool, Type.Pure)

      case KindedAst.Expression.ReifyType(t, k, loc) =>
        k match {
          case Kind.Bool =>
            val sym = Symbol.mkEnumSym("ReifiedBool")
            val tpe = Type.mkEnum(sym, Kind.Star, loc)
            liftM(Nil, tpe, Type.Pure)
          case Kind.Star =>
            val sym = Symbol.mkEnumSym("ReifiedType")
            val tpe = Type.mkEnum(sym, Kind.Star, loc)
            liftM(Nil, tpe, Type.Pure)
          case _ =>
            throw InternalCompilerException(s"Unexpected kind: '$k'.")
        }

      case KindedAst.Expression.ReifyEff(sym, exp1, exp2, exp3, loc) =>
        val a = Type.freshVar(Kind.Star, loc, text = FallbackText("arg"))
        val b = Type.freshVar(Kind.Star, loc, text = FallbackText("result"))
        val ef = Type.freshVar(Kind.Bool, loc, text = FallbackText("eff"))
        val polyLambdaType = Type.mkArrowWithEffect(a, ef, b, loc)
        val pureLambdaType = Type.mkPureArrow(a, b, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          actualLambdaType <- unifyTypeM(polyLambdaType, tpe1, loc)
          boundVar <- unifyTypeM(sym.tvar.ascribedWith(Kind.Star), pureLambdaType, loc)
          resultTyp <- unifyTypeM(tpe2, tpe3, loc)
          resultEff = Type.mkAnd(eff1, eff2, eff3, loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

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
        (constrs2, bodyPredicateTypes) <- seqM(body0.map(b => inferBodyPredicate(b, root))).map(_.unzip)
        bodyPredicateType <- unifyTypeAllowEmptyM(bodyPredicateTypes, Kind.SchemaRow, loc)
        resultType <- unifyTypeM(headPredicateType, bodyPredicateType, loc)
      } yield (constrs1 ++ constrs2.flatten, resultType)
    }

    visitExp(exp0)
  }

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

      case KindedAst.Expression.Var(sym, tvar, loc) =>
        TypedAst.Expression.Var(sym, subst0(sym.tvar.ascribedWith(Kind.Star)), loc)

      case KindedAst.Expression.Def(sym, tvar, loc) =>
        TypedAst.Expression.Def(sym, subst0(tvar), loc)

      case KindedAst.Expression.Sig(sym, tvar, loc) =>
        TypedAst.Expression.Sig(sym, subst0(tvar), loc)

      case KindedAst.Expression.Hole(sym, tpe, loc) =>
        TypedAst.Expression.Hole(sym, subst0(tpe), loc)

      case KindedAst.Expression.Unit(loc) => TypedAst.Expression.Unit(loc)

      case KindedAst.Expression.Null(loc) => TypedAst.Expression.Null(Type.Unit, loc)

      case KindedAst.Expression.True(loc) => TypedAst.Expression.True(loc)

      case KindedAst.Expression.False(loc) => TypedAst.Expression.False(loc)

      case KindedAst.Expression.Char(lit, loc) => TypedAst.Expression.Char(lit, loc)

      case KindedAst.Expression.Float32(lit, loc) => TypedAst.Expression.Float32(lit, loc)

      case KindedAst.Expression.Float64(lit, loc) => TypedAst.Expression.Float64(lit, loc)

      case KindedAst.Expression.Int8(lit, loc) => TypedAst.Expression.Int8(lit, loc)

      case KindedAst.Expression.Int16(lit, loc) => TypedAst.Expression.Int16(lit, loc)

      case KindedAst.Expression.Int32(lit, loc) => TypedAst.Expression.Int32(lit, loc)

      case KindedAst.Expression.Int64(lit, loc) => TypedAst.Expression.Int64(lit, loc)

      case KindedAst.Expression.BigInt(lit, loc) => TypedAst.Expression.BigInt(lit, loc)

      case KindedAst.Expression.Str(lit, loc) => TypedAst.Expression.Str(lit, loc)

      case KindedAst.Expression.Default(tvar, loc) => TypedAst.Expression.Default(subst0(tvar), loc)

      case KindedAst.Expression.Apply(exp, exps, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        val es = exps.map(visitExp(_, subst0))
        TypedAst.Expression.Apply(e, es, subst0(tvar), subst0(evar), loc)

      case KindedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp, subst0)
        val t = subst0(tvar)
        TypedAst.Expression.Lambda(p, e, t, loc)

      case KindedAst.Expression.Unary(sop, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Unary(sop, e, subst0(tvar), eff, loc)

      case KindedAst.Expression.Binary(sop, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.mkAnd(e1.eff, e2.eff, loc)
        TypedAst.Expression.Binary(sop, e1, e2, subst0(tvar), eff, loc)

      case KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, e3.eff, loc)
        TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case KindedAst.Expression.Stm(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, loc)
        TypedAst.Expression.Stm(e1, e2, tpe, eff, loc)

      case KindedAst.Expression.Discard(exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Discard(e, e.eff, loc)

      case KindedAst.Expression.Let(sym, mod, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, loc)
        TypedAst.Expression.Let(sym, mod, e1, e2, tpe, eff, loc)

      case KindedAst.Expression.LetRec(sym, mod, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, loc)
        TypedAst.Expression.LetRec(sym, mod, e1, e2, tpe, eff, loc)

      case KindedAst.Expression.Region(tpe, loc) =>
        TypedAst.Expression.Region(tpe, loc)

      case KindedAst.Expression.Scope(sym, regionVar, exp, evar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = subst0(evar)
        TypedAst.Expression.Scope(sym, regionVar, e, tpe, eff, loc)

      case KindedAst.Expression.Match(matchExp, rules, loc) =>
        val e1 = visitExp(matchExp, subst0)
        val rs = rules map {
          case KindedAst.MatchRule(pat, guard, exp) =>
            val p = reassemblePattern(pat, root, subst0)
            val g = visitExp(guard, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.MatchRule(p, g, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = rs.foldLeft(e1.eff) {
          case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkAnd(g.eff, b.eff, acc, loc)
        }
        TypedAst.Expression.Match(e1, rs, tpe, eff, loc)

      case KindedAst.Expression.Choose(_, exps, rules, tvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val rs = rules.map {
          case KindedAst.ChoiceRule(pat0, exp) =>
            val pat = pat0.map {
              case KindedAst.ChoicePattern.Wild(loc) => TypedAst.ChoicePattern.Wild(loc)
              case KindedAst.ChoicePattern.Absent(loc) => TypedAst.ChoicePattern.Absent(loc)
              case KindedAst.ChoicePattern.Present(sym, tvar, loc) => TypedAst.ChoicePattern.Present(sym, subst0(tvar), loc)
            }
            TypedAst.ChoiceRule(pat, visitExp(exp, subst0))
        }
        val tpe = subst0(tvar)
        val eff = Type.mkAnd(rs.map(_.exp.eff), loc)
        TypedAst.Expression.Choose(es, rs, tpe, eff, loc)

      case KindedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Tag(sym, tag, e, subst0(tvar), eff, loc)

      case KindedAst.Expression.Tuple(elms, loc) =>
        val es = elms.map(visitExp(_, subst0))
        val tpe = Type.mkTuple(es.map(_.tpe), loc)
        val eff = Type.mkAnd(es.map(_.eff), loc)
        TypedAst.Expression.Tuple(es, tpe, eff, loc)

      case KindedAst.Expression.RecordEmpty(loc) =>
        TypedAst.Expression.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)

      case KindedAst.Expression.RecordSelect(exp, field, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.RecordSelect(e, field, subst0(tvar), eff, loc)

      case KindedAst.Expression.RecordExtend(field, value, rest, tvar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        val eff = Type.mkAnd(v.eff, r.eff, loc)
        TypedAst.Expression.RecordExtend(field, v, r, subst0(tvar), eff, loc)

      case KindedAst.Expression.RecordRestrict(field, rest, tvar, loc) =>
        val r = visitExp(rest, subst0)
        val eff = r.eff
        TypedAst.Expression.RecordRestrict(field, r, subst0(tvar), eff, loc)

      case KindedAst.Expression.ArrayLit(exps, exp, tvar, evar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(evar)
        TypedAst.Expression.ArrayLit(es, e, tpe, eff, loc)

      case KindedAst.Expression.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(evar)
        TypedAst.Expression.ArrayNew(e1, e2, e3, tpe, eff, loc)

      case KindedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.Impure
        TypedAst.Expression.ArrayLoad(e1, e2, subst0(tvar), eff, loc)

      case KindedAst.Expression.ArrayStore(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.ArrayStore(e1, e2, e3, loc)

      case KindedAst.Expression.ArrayLength(exp, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.ArrayLength(e, eff, loc)

      case KindedAst.Expression.ArraySlice(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e1.tpe
        TypedAst.Expression.ArraySlice(e1, e2, e3, tpe, loc)

      case KindedAst.Expression.Ref(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(evar)
        TypedAst.Expression.Ref(e1, e2, tpe, eff, loc)

      case KindedAst.Expression.Deref(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(evar)
        TypedAst.Expression.Deref(e, tpe, eff, loc)

      case KindedAst.Expression.Assign(exp1, exp2, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = subst0(evar)
        TypedAst.Expression.Assign(e1, e2, tpe, eff, loc)

      case KindedAst.Expression.Ascribe(exp, _, _, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Ascribe(e, subst0(tvar), eff, loc)

      case KindedAst.Expression.Cast(KindedAst.Expression.Null(_), _, _, tvar, loc) =>
        val t = subst0(tvar)
        TypedAst.Expression.Null(t, loc)

      case KindedAst.Expression.Cast(exp, declaredType, declaredEff, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val dt = declaredType.map(tpe => subst0(tpe))
        val de = declaredEff.map(eff => subst0(eff))
        val tpe = subst0(tvar)
        val eff = declaredEff.getOrElse(e.eff)
        TypedAst.Expression.Cast(e, dt, de, tpe, eff, loc)

      case KindedAst.Expression.Without(exp, eff, loc) => ??? // MATT hmmmm

      case KindedAst.Expression.TryCatch(exp, rules, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = Type.mkAnd(e.eff :: rs.map(_.exp.eff), loc)
        TypedAst.Expression.TryCatch(e, rs, tpe, eff, loc)

      case KindedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(constructor.getDeclaringClass)
        val eff = Type.Impure
        TypedAst.Expression.InvokeConstructor(constructor, as, tpe, eff, loc)

      case KindedAst.Expression.InvokeMethod(method, exp, args, loc) =>
        val e = visitExp(exp, subst0)
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val eff = Type.Impure
        TypedAst.Expression.InvokeMethod(method, e, as, tpe, eff, loc)

      case KindedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val eff = Type.Impure
        TypedAst.Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

      case KindedAst.Expression.GetField(field, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = getFlixType(field.getType)
        val eff = Type.Impure
        TypedAst.Expression.GetField(field, e, tpe, eff, loc)

      case KindedAst.Expression.PutField(field, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expression.PutField(field, e1, e2, tpe, eff, loc)

      case KindedAst.Expression.GetStaticField(field, loc) =>
        val tpe = getFlixType(field.getType)
        val eff = Type.Impure
        TypedAst.Expression.GetStaticField(field, tpe, eff, loc)

      case KindedAst.Expression.PutStaticField(field, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expression.PutStaticField(field, e, tpe, eff, loc)

      case KindedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.NewChannel(e, Type.mkChannel(tpe, loc), eff, loc)

      case KindedAst.Expression.GetChannel(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.GetChannel(e, subst0(tvar), eff, loc)

      case KindedAst.Expression.PutChannel(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.mkUnit(loc)
        val eff = Type.Impure
        TypedAst.Expression.PutChannel(e1, e2, tpe, eff, loc)

      case KindedAst.Expression.SelectChannel(rules, default, tvar, loc) =>
        val rs = rules map {
          case KindedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule(sym, c, b)
        }
        val d = default.map(visitExp(_, subst0))
        val eff = Type.Impure
        TypedAst.Expression.SelectChannel(rs, d, subst0(tvar), eff, loc)

      case KindedAst.Expression.Spawn(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.Unit
        val eff = e.eff
        TypedAst.Expression.Spawn(e, tpe, eff, loc)

      case KindedAst.Expression.Lazy(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.mkLazy(e.tpe, loc)
        TypedAst.Expression.Lazy(e, tpe, loc)

      case KindedAst.Expression.Force(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = e.eff
        TypedAst.Expression.Force(e, tpe, eff, loc)

      case KindedAst.Expression.FixpointConstraintSet(cs0, tvar, loc) =>
        val cs = cs0.map(visitConstraint)
        TypedAst.Expression.FixpointConstraintSet(cs, Stratification.empty, subst0(tvar), loc)

      case KindedAst.Expression.FixpointLambda(pparams, exp, tvar, loc) =>
        val ps = pparams.map(visitPredicateParam)
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = e.eff
        TypedAst.Expression.FixpointLambda(ps, e, Stratification.empty, tpe, eff, loc)

      case KindedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e1.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, loc)
        TypedAst.Expression.FixpointMerge(e1, e2, Stratification.empty, tpe, eff, loc)

      case KindedAst.Expression.FixpointSolve(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = e.eff
        TypedAst.Expression.FixpointSolve(e, Stratification.empty, tpe, eff, loc)

      case KindedAst.Expression.FixpointFilter(pred, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.FixpointFilter(pred, e, subst0(tvar), eff, loc)

      case KindedAst.Expression.FixpointProjectIn(exp, pred, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.FixpointProjectIn(e, pred, subst0(tvar), eff, loc)

      case KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val stf = Stratification.empty
        val tpe = subst0(tvar)
        val eff = Type.mkAnd(e1.eff, e2.eff, loc)

        // Note: This transformation should happen in the Weeder but it is here because
        // `#{#Result(..)` | _} cannot be unified with `#{A(..)}` (a closed row).
        // See Weeder for more details.
        val mergeExp = TypedAst.Expression.FixpointMerge(e1, e2, stf, e1.tpe, eff, loc)
        val solveExp = TypedAst.Expression.FixpointSolve(mergeExp, stf, e1.tpe, eff, loc)
        TypedAst.Expression.FixpointProjectOut(pred, solveExp, tpe, eff, loc)

      case KindedAst.Expression.Reify(t0, loc) =>
        val t = subst0(t0)
        val tpe = Type.Bool
        val eff = Type.Pure
        TypedAst.Expression.Reify(t, tpe, eff, loc)

      case KindedAst.Expression.ReifyType(t0, k0, loc) =>
        val t = subst0(t0)
        val sym = Symbol.mkEnumSym("ReifiedType")
        val tpe = Type.mkEnum(sym, Kind.Star, loc)
        val eff = Type.Pure
        TypedAst.Expression.ReifyType(t, k0, tpe, eff, loc)

      case KindedAst.Expression.ReifyEff(sym, exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)

        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, e3.eff, loc)
        TypedAst.Expression.ReifyEff(sym, e1, e2, e3, tpe, eff, loc)
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
        case KindedAst.ConstraintParam.HeadParam(sym, tpe, l) =>
          TypedAst.ConstraintParam.HeadParam(sym, subst0(tpe), l)
        case KindedAst.ConstraintParam.RuleParam(sym, tpe, l) =>
          TypedAst.ConstraintParam.RuleParam(sym, subst0(tpe), l)
      }

      // Reassemble the constraint.
      TypedAst.Constraint(cparams, head, body, loc)
    }

    /**
      * Applies the substitution to the given list of formal parameters.
      */
    def visitFormalParam(fparam: KindedAst.FormalParam): TypedAst.FormalParam =
      TypedAst.FormalParam(fparam.sym, fparam.mod, subst0(fparam.tpe), fparam.loc)

    /**
      * Applies the substitution to the given list of predicate parameters.
      */
    def visitPredicateParam(pparam: KindedAst.PredicateParam): TypedAst.PredicateParam =
      TypedAst.PredicateParam(pparam.pred, subst0(pparam.tpe), pparam.loc)

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

      case KindedAst.Pattern.Var(sym, tvar, loc) => unifyTypeM(sym.tvar.ascribedWith(Kind.Star), tvar, loc)

      case KindedAst.Pattern.Unit(loc) => liftM(Type.Unit)

      case KindedAst.Pattern.True(loc) => liftM(Type.Bool)

      case KindedAst.Pattern.False(loc) => liftM(Type.Bool)

      case KindedAst.Pattern.Char(c, loc) => liftM(Type.Char)

      case KindedAst.Pattern.Float32(i, loc) => liftM(Type.Float32)

      case KindedAst.Pattern.Float64(i, loc) => liftM(Type.Float64)

      case KindedAst.Pattern.Int8(i, loc) => liftM(Type.Int8)

      case KindedAst.Pattern.Int16(i, loc) => liftM(Type.Int16)

      case KindedAst.Pattern.Int32(i, loc) => liftM(Type.Int32)

      case KindedAst.Pattern.Int64(i, loc) => liftM(Type.Int64)

      case KindedAst.Pattern.BigInt(i, loc) => liftM(Type.BigInt)

      case KindedAst.Pattern.Str(s, loc) => liftM(Type.Str)

      case KindedAst.Pattern.Tag(sym, tag, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.enums(sym)

        // Lookup the case declaration.
        val caze = decl.cases(tag)

        // Instantiate the type scheme of the case.
        val (_, tagType) = Scheme.instantiate(caze.sc, InstantiateMode.Flexible)

        //
        // The tag type can be thought of as a function from the type of variant to the type of the enum.
        // See Type.mkTag for details.
        //
        for {
          tpe <- visit(pat)
          _ <- unifyTypeM(tagType, Type.mkTag(sym, tag, tpe, tvar, loc), loc)
          resultTyp = tvar
        } yield resultTyp

      case KindedAst.Pattern.Tuple(elms, loc) =>
        for {
          elementTypes <- seqM(elms map visit)
        } yield Type.mkTuple(elementTypes, loc)

      case KindedAst.Pattern.Array(elms, tvar, loc) =>
        for {
          elementTypes <- seqM(elms map visit)
          elementType <- unifyTypeAllowEmptyM(elementTypes, Kind.Star, loc)
          resultType <- unifyTypeM(tvar, Type.mkArray(elementType, loc), loc)
        } yield resultType

      case KindedAst.Pattern.ArrayTailSpread(elms, varSym, tvar, loc) =>
        for {
          elementTypes <- seqM(elms map visit)
          elementType <- unifyTypeAllowEmptyM(elementTypes, Kind.Star, loc)
          arrayType <- unifyTypeM(tvar, Type.mkArray(elementType, loc), loc)
          resultType <- unifyTypeM(varSym.tvar.ascribedWith(Kind.Star), arrayType, loc)
        } yield resultType

      case KindedAst.Pattern.ArrayHeadSpread(varSym, elms, tvar, loc) =>
        for {
          elementTypes <- seqM(elms map visit)
          elementType <- unifyTypeAllowEmptyM(elementTypes, Kind.Star, loc)
          arrayType <- unifyTypeM(tvar, Type.mkArray(elementType, loc), loc)
          resultType <- unifyTypeM(varSym.tvar.ascribedWith(Kind.Star), arrayType, loc)
        } yield resultType

    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[KindedAst.Pattern], root: KindedAst.Root)(implicit flix: Flix): InferMonad[List[Type]] = {
    seqM(pats0.map(p => inferPattern(p, root)))
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
      case KindedAst.Pattern.Unit(loc) => TypedAst.Pattern.Unit(loc)
      case KindedAst.Pattern.True(loc) => TypedAst.Pattern.True(loc)
      case KindedAst.Pattern.False(loc) => TypedAst.Pattern.False(loc)
      case KindedAst.Pattern.Char(lit, loc) => TypedAst.Pattern.Char(lit, loc)
      case KindedAst.Pattern.Float32(lit, loc) => TypedAst.Pattern.Float32(lit, loc)
      case KindedAst.Pattern.Float64(lit, loc) => TypedAst.Pattern.Float64(lit, loc)
      case KindedAst.Pattern.Int8(lit, loc) => TypedAst.Pattern.Int8(lit, loc)
      case KindedAst.Pattern.Int16(lit, loc) => TypedAst.Pattern.Int16(lit, loc)
      case KindedAst.Pattern.Int32(lit, loc) => TypedAst.Pattern.Int32(lit, loc)
      case KindedAst.Pattern.Int64(lit, loc) => TypedAst.Pattern.Int64(lit, loc)
      case KindedAst.Pattern.BigInt(lit, loc) => TypedAst.Pattern.BigInt(lit, loc)
      case KindedAst.Pattern.Str(lit, loc) => TypedAst.Pattern.Str(lit, loc)

      case KindedAst.Pattern.Tag(sym, tag, pat, tvar, loc) => TypedAst.Pattern.Tag(sym, tag, visit(pat), subst0(tvar), loc)

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
      val restRow = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row"))
      for {
        (termConstrs, termTypes, termEffects) <- seqM(terms.map(inferExp(_, root))).map(_.unzip3)
        pureTermEffects <- unifyBoolM(Type.Pure, Type.mkAnd(termEffects, loc), loc)
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
      val restRow = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row"))
      for {
        termTypes <- seqM(terms.map(inferPattern(_, root)))
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
      } yield (tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      for {
        (constrs, tpe, eff) <- inferExp(exp, root)
        expEff <- unifyBoolM(Type.Pure, eff, loc)
        expTyp <- unifyTypeM(Type.Bool, tpe, loc)
      } yield (constrs, mkAnySchemaRowType(loc))

    case KindedAst.Predicate.Body.Loop(varSyms, exp, loc) =>
      // TODO: Use type classes instead of array?
      val tupleType = Type.mkTuple(varSyms.map(_.tvar.ascribedWith(Kind.Star)), loc)
      val expectedType = Type.mkArray(tupleType, loc)
      for {
        (constrs, tpe, eff) <- inferExp(exp, root)
        expEff <- unifyBoolM(Type.Pure, eff, loc)
        expTyp <- unifyTypeM(expectedType, tpe, loc)
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
      PredefinedClasses.lookupClassSym("ToString", root),
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
      PredefinedClasses.lookupClassSym("ToString", root),
      PredefinedClasses.lookupClassSym("PartialOrder", root),
      PredefinedClasses.lookupClassSym("LowerBound", root),
      PredefinedClasses.lookupClassSym("JoinLattice", root),
      PredefinedClasses.lookupClassSym("MeetLattice", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Performs type resolution on the given attribute `attr`.
    */
  private def typeCheckAttribute(attr: KindedAst.Attribute): Result[TypedAst.Attribute, TypeError] = attr match {
    case KindedAst.Attribute(ident, tpe, loc) => Ok(TypedAst.Attribute(ident.name, tpe, loc))
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
      case (macc, (KindedAst.FormalParam(sym, _, _, _), declaredType)) =>
        macc ++ Substitution.singleton(sym.tvar.sym.ascribedWith(Kind.Star), declaredType)
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
    case KindedAst.FormalParam(sym, mod, tpe, loc) => TypedAst.FormalParam(sym, mod, subst0(tpe), sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaRowType(loc: SourceLocation)(implicit flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc, text = FallbackText("row"))

  /**
    * Returns the Flix Type of a Java Class
    */
  private def getFlixType(c: Class[_]): Type = {
    if (c == java.lang.Boolean.TYPE) {
      Type.Bool
    }
    else if (c == java.lang.Byte.TYPE) {
      Type.Int8
    }
    else if (c == java.lang.Short.TYPE) {
      Type.Int16
    }
    else if (c == java.lang.Integer.TYPE) {
      Type.Int32
    }
    else if (c == java.lang.Long.TYPE) {
      Type.Int64
    }
    else if (c == java.lang.Character.TYPE) {
      Type.Char
    }
    else if (c == java.lang.Float.TYPE) {
      Type.Float32
    }
    else if (c == java.lang.Double.TYPE) {
      Type.Float64
    }
    else if (c == classOf[java.math.BigInteger]) {
      Type.BigInt
    }
    else if (c == classOf[java.lang.String]) {
      Type.Str
    }
    else if (c == java.lang.Void.TYPE) {
      Type.Unit
    }
    // handle arrays of types
    else if (c.isArray) {
      val comp = c.getComponentType
      val elmType = getFlixType(comp)
      Type.mkArray(elmType, SourceLocation.Unknown)
    }
    // otherwise native type
    else {
      Type.mkNative(c, SourceLocation.Unknown)
    }
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
