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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Stratification}
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.InferMonad.seqM
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.ToFailure
import ca.uwaterloo.flix.util._

import java.io.PrintWriter

object Typer extends Phase[ResolvedAst.Root, TypedAst.Root] {

  /**
    * The following classes are assumed to always exist.
    *
    * Anything added here must be mentioned in `CoreLibrary` in the Flix class.
    */
  object PredefinedClasses {

    /**
      * Returns the class symbol with the given `name`.
      */
    def lookupClassSym(name: String, root: ResolvedAst.Root): Symbol.ClassSym = {
      val key = new Symbol.ClassSym(Nil, name, SourceLocation.Unknown)
      root.classes.get(key) match {
        case None => throw InternalCompilerException(s"The type class: '$key' is not defined.")
        case Some(clazz) => clazz.sym
      }
    }

  }

  /**
    * Type checks the given AST root.
    */
  def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Typer") {
    val classEnv = mkClassEnv(root.classes, root.instances)

    val classesVal = visitClasses(root, classEnv)
    val instancesVal = visitInstances(root, classEnv)
    val defsVal = visitDefs(root, classEnv)
    val enumsVal = visitEnums(root)
    val propertiesVal = visitProperties(root)

    Validation.mapN(classesVal, instancesVal, defsVal, enumsVal, propertiesVal) {
      case (classes, instances, defs, enums, properties) =>
        val sigs = classes.values.flatMap(_.signatures).map(sig => sig.sym -> sig).toMap
        val latticeOps = Map.empty[Type, TypedAst.LatticeOps]
        val specialOps = Map.empty[SpecialOperator, Map[Type, Symbol.DefnSym]]
        TypedAst.Root(classes, instances, sigs, defs, enums, latticeOps, properties, specialOps, root.reachable, root.sources, classEnv)
    }
  }

  /**
    * Creates a class environment from a ClassSym-Instance multimap.
    */
  private def mkClassEnv(classes: Map[Symbol.ClassSym, ResolvedAst.Class], instances: Map[Symbol.ClassSym, List[ResolvedAst.Instance]]): Map[Symbol.ClassSym, Ast.ClassContext] = {
    instances.map {
      case (classSym, instances) =>
        val envInsts = instances.map {
          case ResolvedAst.Instance(_, _, _, tpe, tconstrs, _, _, _) => Ast.Instance(tpe, tconstrs)
        }
        val superClasses = classes.get(classSym) match {
          case Some(ResolvedAst.Class(_, _, _, _, superClasses, _, _, _)) => superClasses
          case None => throw InternalCompilerException(s"Unexpected unrecognized class $classSym")
        }
        (classSym, Ast.ClassContext(superClasses, envInsts))
    }
  }

  /**
    * Performs type inference and reassembly on all classes in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitClasses(root: ResolvedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Map[Symbol.ClassSym, TypedAst.Class], TypeError] = {

    def visitClass(clazz: ResolvedAst.Class): Validation[(Symbol.ClassSym, TypedAst.Class), TypeError] = clazz match {
      case ResolvedAst.Class(doc, mod, sym, tparam, superClasses, sigs, laws0, loc) =>
        val tparams = getTypeParams(List(tparam))
        val tconstr = Ast.TypeConstraint(sym, tparam.tpe)
        for {
          sigs <- Validation.traverse(sigs.values)(visitSig(_, List(tconstr), root, classEnv))
          laws <- Validation.traverse(laws0)(visitDefn(_, List(tconstr), root, classEnv))
        } yield (sym, TypedAst.Class(doc, mod, sym, tparams.head, superClasses, sigs, laws, loc))
    }

    // visit each class
    val result = root.classes.values.map(visitClass)

    Validation.sequence(result).map(_.toMap)
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: ResolvedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Map[Symbol.ClassSym, List[TypedAst.Instance]], TypeError] = {

    /**
      * Reassembles a single instance.
      */
    def visitInstance(inst: ResolvedAst.Instance): Validation[TypedAst.Instance, TypeError] = inst match {
      case ResolvedAst.Instance(doc, mod, sym, tpe, tconstrs, defs0, ns, loc) =>
        for {
          defs <- Validation.traverse(defs0)(visitDefn(_, tconstrs, root, classEnv))
        } yield TypedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc)
    }

    /**
      * Reassembles a set of instances of the same class.
      */
    def mapOverInstances(insts0: List[ResolvedAst.Instance]): Validation[(Symbol.ClassSym, List[TypedAst.Instance]), TypeError] = {
      val instsVal = Validation.traverse(insts0)(visitInstance)

      instsVal.map {
        insts => insts.head.sym -> insts
      }
    }

    // visit each instance
    val result = root.instances.values.map(mapOverInstances)
    Validation.sequence(result).map(insts => insts.toMap)

  }

  /**
    * Performs type inference and reassembly on the given definition `defn`.
    */
  private def visitDefn(defn: ResolvedAst.Def, assumedTconstrs: List[Ast.TypeConstraint], root: ResolvedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = defn match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      typeCheckDecl(spec0, exp0, assumedTconstrs, isMain = sym.isMain, root, classEnv) map {
        case (spec, exp) => TypedAst.Def(sym, spec, exp)
      }
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: ResolvedAst.Sig, assumedTconstrs: List[Ast.TypeConstraint], root: ResolvedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Sig, TypeError] = sig match {
    case ResolvedAst.Sig(sym, spec0, Some(exp0)) =>
      typeCheckDecl(spec0, exp0, assumedTconstrs, isMain = false, root, classEnv) map {
        case (spec, exp) => TypedAst.Sig(sym, spec, Some(exp))
      }
    case ResolvedAst.Sig(sym, spec0, None) =>
      visitSpec(spec0, root, Substitution.empty) map {
        spec => TypedAst.Sig(sym, spec, None)
      }
  }

  /**
    * Performs type inference and reassembly on the given Spec `spec`.
    */
  private def visitSpec(spec: ResolvedAst.Spec, root: ResolvedAst.Root, subst: Substitution)(implicit flix: Flix): Validation[TypedAst.Spec, TypeError] = spec match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc, eff, loc) =>
      val annVal = visitAnnotations(ann0, root)
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst)
      Validation.mapN(annVal) {
        ann => TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, eff, loc)
      }

  }

  /**
    * Performs type inference and reassembly on all definitions in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitDefs(root: ResolvedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    // Compute the results in parallel.
    val results = ParOps.parMap(root.defs.values, visitDefn(_, Nil, root, classEnv))

    // Sequence the results.
    Validation.sequence(results) map {
      case xs => xs.foldLeft(Map.empty[Symbol.DefnSym, TypedAst.Def]) {
        case (acc, defn) => acc + (defn.sym -> defn)
      }
    }
  }

  /**
    * Infers the type of the given definition `defn0`.
    */
  private def typeCheckDecl(spec0: ResolvedAst.Spec, exp0: ResolvedAst.Expression, assumedTconstrs: List[Ast.TypeConstraint], isMain: Boolean, root: ResolvedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[(TypedAst.Spec, TypedAst.Impl), TypeError] = spec0 match {
    case ResolvedAst.Spec(doc, ann, mod, tparams0, fparams0, sc, eff, loc) =>

      ///
      /// Infer the type of the expression `exp0`.
      ///
      val result = for {
        (inferredConstrs, inferredTyp, inferredEff) <- inferExp(exp0, root)
      } yield (inferredConstrs, Type.mkUncurriedArrowWithEffect(fparams0.map(_.tpe), inferredEff, inferredTyp))


      val declaredScheme = if (isMain) {
        // Case 1: This is the main function. Its type signature is fixed.
        Scheme(Nil, Nil, Type.mkImpureArrow(Type.mkArray(Type.Str), Type.Int32))
      } else {
        // Case 2: Use the declared type.
        sc.copy(constraints = sc.constraints ++ assumedTconstrs)
      }

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

          run(initialSubst) match {
            case Ok((subst, (partialTconstrs, partialType))) =>
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
                    case UnificationError.NoMatchingInstance(clazz, tpe) => TypeError.NoMatchingInstance(clazz, tpe, loc)
                  }
                  // Case 2: non instance error
                  if (instanceErrs.isEmpty) {
                    return TypeError.GeneralizationError(declaredScheme, inferredSc, loc).toFailure
                    // Case 3: instance error
                  } else {
                    return Validation.Failure(instanceErrs)
                  }
              }

              ///
              /// Compute the expression, type parameters, and formal parameters with the substitution applied everywhere.
              ///
              val exp = reassembleExp(exp0, root, subst)
              val specVal = visitSpec(spec0, root, subst)

              ///
              /// Compute a type scheme that matches the type variables that appear in the expression body.
              ///
              /// NB: It is very important to understand that: The type scheme a function is declared with must match the inferred type scheme.
              /// However, we require an even stronger property for the implementation to work. The inferred type scheme used in the rest of the
              /// compiler must *use the same type variables* in the scheme as in the body expression. Otherwise monomorphization et al. will break.
              ///
              val inferredScheme = Scheme(inferredType.typeVars.toList, inferredConstrs, inferredType)

              specVal map {
                spec => (spec, TypedAst.Impl(exp, inferredScheme))
              }

            case Err(e) => Validation.Failure(LazyList(e))
          }
      }
  }

  /**
    * Performs type inference and reassembly on all enums in the given AST root.
    */
  private def visitEnums(root: ResolvedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.EnumSym, TypedAst.Enum], TypeError] = {
    /**
      * Performs type resolution on the given enum and its cases.
      */
    def visitEnum(enum: ResolvedAst.Enum): Validation[(Symbol.EnumSym, TypedAst.Enum), TypeError] = enum match {
      case ResolvedAst.Enum(doc, mod, enumSym, tparams, cases0, tpe, sc, loc) =>
        val tparams = getTypeParams(enum.tparams)
        val cases = cases0 map {
          case (name, ResolvedAst.Case(_, tagName, tagType, tagScheme)) =>
            name -> TypedAst.Case(enumSym, tagName, tagType, tagScheme, tagName.loc)
        }

        Validation.Success(enumSym -> TypedAst.Enum(doc, mod, enumSym, tparams, cases, enum.tpeDeprecated, enum.sc, loc))
    }

    // Visit every enum in the ast.
    val result = root.enums.toList.map {
      case (_, enum) => visitEnum(enum)
    }

    // Sequence the results and convert them back to a map.
    Validation.sequence(result).map(_.toMap)
  }

  /**
    * Infers the types of all the properties in the given AST `root`.
    */
  private def visitProperties(root: ResolvedAst.Root)(implicit flix: Flix): Validation[List[TypedAst.Property], TypeError] = {

    /**
      * Infers the type of the given property `p0`.
      */
    def visitProperty(p0: ResolvedAst.Property): Result[TypedAst.Property, TypeError] = p0 match {
      case ResolvedAst.Property(law, defn, exp0, loc) =>
        val result = inferExp(exp0, root)
        result.run(Substitution.empty) map {
          case (subst, tpe) =>
            val exp = reassembleExp(exp0, root, subst)
            TypedAst.Property(law, defn, exp, loc)
        }
    }

    // Visit every property in the ast.
    val results = root.properties.map(visitProperty).map(_.toValidation)

    // Sequence the results and sort the properties by their source location.
    Validation.sequence(results)
  }

  /**
    * Visits all annotations.
    */
  private def visitAnnotations(ann: List[ResolvedAst.Annotation], root: ResolvedAst.Root)(implicit flix: Flix): Validation[List[TypedAst.Annotation], TypeError] = {
    Validation.traverse(ann)(inferAnnotation(_, root))
  }

  /**
    * Performs type inference on the given annotation `ann0`.
    */
  private def inferAnnotation(ann0: ResolvedAst.Annotation, root: ResolvedAst.Root)(implicit flix: Flix): Validation[TypedAst.Annotation, TypeError] = ann0 match {
    case ResolvedAst.Annotation(name, exps, loc) =>
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
      result.run(initialSubst).toValidation.map {
        case (subst, _) =>
          val es = exps.map(reassembleExp(_, root, subst))
          TypedAst.Annotation(name, es, loc)
      }
  }

  /**
    * Infers the type of the given expression `exp0`.
    */
  private def inferExp(exp0: ResolvedAst.Expression, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: ResolvedAst.Expression): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = e0 match {

      case ResolvedAst.Expression.Wild(tvar, loc) =>
        liftM(List.empty, tvar, Type.Pure)

      case ResolvedAst.Expression.Var(sym, tpe, loc) =>
        for {
          resultTyp <- unifyTypeM(sym.tvar, tpe, loc)
        } yield (List.empty, resultTyp, Type.Pure)

      case ResolvedAst.Expression.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs, defType) = Scheme.instantiate(defn.spec.sc, InstantiateMode.Flexible)
        for {
          resultTyp <- unifyTypeM(tvar, defType, loc)
        } yield (tconstrs, resultTyp, Type.Pure)

      case ResolvedAst.Expression.Sig(sym, tvar, loc) =>
        // find the declared signature corresponding to this symbol
        val sig = root.classes(sym.clazz).sigs(sym)
        val (tconstrs, sigType) = Scheme.instantiate(sig.spec.sc, InstantiateMode.Flexible)
        for {
          resultTyp <- unifyTypeM(tvar, sigType, loc)
        } yield (tconstrs, resultTyp, Type.Pure)

      case ResolvedAst.Expression.Hole(sym, tvar, evar, loc) =>
        liftM(List.empty, tvar, evar)

      case ResolvedAst.Expression.Unit(loc) =>
        liftM(List.empty, Type.Unit, Type.Pure)

      case ResolvedAst.Expression.Null(loc) =>
        liftM(List.empty, Type.Null, Type.Pure)

      case ResolvedAst.Expression.True(loc) =>
        liftM(List.empty, Type.Bool, Type.Pure)

      case ResolvedAst.Expression.False(loc) =>
        liftM(List.empty, Type.Bool, Type.Pure)

      case ResolvedAst.Expression.Char(lit, loc) =>
        liftM(List.empty, Type.Char, Type.Pure)

      case ResolvedAst.Expression.Float32(lit, loc) =>
        liftM(List.empty, Type.Float32, Type.Pure)

      case ResolvedAst.Expression.Float64(lit, loc) =>
        liftM(List.empty, Type.Float64, Type.Pure)

      case ResolvedAst.Expression.Int8(lit, loc) =>
        liftM(List.empty, Type.Int8, Type.Pure)

      case ResolvedAst.Expression.Int16(lit, loc) =>
        liftM(List.empty, Type.Int16, Type.Pure)

      case ResolvedAst.Expression.Int32(lit, loc) =>
        liftM(List.empty, Type.Int32, Type.Pure)

      case ResolvedAst.Expression.Int64(lit, loc) =>
        liftM(List.empty, Type.Int64, Type.Pure)

      case ResolvedAst.Expression.BigInt(lit, loc) =>
        liftM(List.empty, Type.BigInt, Type.Pure)

      case ResolvedAst.Expression.Str(lit, loc) =>
        liftM(List.empty, Type.Str, Type.Pure)

      case ResolvedAst.Expression.Default(tvar, loc) =>
        liftM(List.empty, tvar, Type.Pure)

      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val argType = fparam.tpe
        for {
          (constrs, bodyType, bodyEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, Type.mkArrowWithEffect(argType, bodyEff, bodyType), loc)
        } yield (constrs, resultTyp, Type.Pure)

      case ResolvedAst.Expression.Apply(exp, exps, tvar, evar, loc) =>
        val lambdaBodyType = Type.freshVar(Kind.Star)
        val lambdaBodyEff = Type.freshVar(Kind.Bool)
        for {
          (constrs1, tpe, eff) <- visitExp(exp)
          (constrs2, tpes, effs) <- seqM(exps.map(visitExp)).map(_.unzip3)
          lambdaType <- unifyTypeM(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyEff, lambdaBodyType), loc)
          resultTyp <- unifyTypeM(tvar, lambdaBodyType, loc)
          resultEff <- unifyBoolM(evar, Type.mkAnd(lambdaBodyEff :: eff :: effs), loc)
        } yield (constrs1 ++ constrs2.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOperator.BoolOp.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Bool, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Float32Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Float32, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Float64Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Float64, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int8Op.Neg | SemanticOperator.Int8Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Int8, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int16Op.Neg | SemanticOperator.Int16Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Int16, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int32Op.Neg | SemanticOperator.Int32Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Int32, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.Int64Op.Neg | SemanticOperator.Int64Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.Int64, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOperator.BigIntOp.Neg | SemanticOperator.BigIntOp.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypeM(tvar, tpe, Type.BigInt, loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop' near ${loc.format}.")
      }

      case ResolvedAst.Expression.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOperator.BoolOp.And | SemanticOperator.BoolOp.Or =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultType <- unifyTypeM(tvar, tpe1, tpe2, Type.Bool, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultType, resultEff)

        case SemanticOperator.Float32Op.Add | SemanticOperator.Float32Op.Sub | SemanticOperator.Float32Op.Mul | SemanticOperator.Float32Op.Div
             | SemanticOperator.Float32Op.Rem | SemanticOperator.Float32Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Float32, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Float64Op.Add | SemanticOperator.Float64Op.Sub | SemanticOperator.Float64Op.Mul | SemanticOperator.Float64Op.Div
             | SemanticOperator.Float64Op.Rem | SemanticOperator.Float64Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Float64, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int8Op.Add | SemanticOperator.Int8Op.Sub | SemanticOperator.Int8Op.Mul | SemanticOperator.Int8Op.Div
             | SemanticOperator.Int8Op.Rem | SemanticOperator.Int8Op.Exp
             | SemanticOperator.Int8Op.And | SemanticOperator.Int8Op.Or | SemanticOperator.Int8Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Int8, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int16Op.Add | SemanticOperator.Int16Op.Sub | SemanticOperator.Int16Op.Mul | SemanticOperator.Int16Op.Div
             | SemanticOperator.Int16Op.Rem | SemanticOperator.Int16Op.Exp
             | SemanticOperator.Int16Op.And | SemanticOperator.Int16Op.Or | SemanticOperator.Int16Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Int16, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int32Op.Add | SemanticOperator.Int32Op.Sub | SemanticOperator.Int32Op.Mul | SemanticOperator.Int32Op.Div
             | SemanticOperator.Int32Op.Rem | SemanticOperator.Int32Op.Exp
             | SemanticOperator.Int32Op.And | SemanticOperator.Int32Op.Or | SemanticOperator.Int32Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Int32, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int64Op.Add | SemanticOperator.Int64Op.Sub | SemanticOperator.Int64Op.Mul | SemanticOperator.Int64Op.Div
             | SemanticOperator.Int64Op.Rem | SemanticOperator.Int64Op.Exp
             | SemanticOperator.Int64Op.And | SemanticOperator.Int64Op.Or | SemanticOperator.Int64Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Int64, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.BigIntOp.Add | SemanticOperator.BigIntOp.Sub | SemanticOperator.BigIntOp.Mul | SemanticOperator.BigIntOp.Div
             | SemanticOperator.BigIntOp.Rem | SemanticOperator.BigIntOp.Exp
             | SemanticOperator.BigIntOp.And | SemanticOperator.BigIntOp.Or | SemanticOperator.BigIntOp.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.BigInt, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.Int8Op.Shl | SemanticOperator.Int8Op.Shr
             | SemanticOperator.Int16Op.Shl | SemanticOperator.Int16Op.Shr
             | SemanticOperator.Int32Op.Shl | SemanticOperator.Int32Op.Shr
             | SemanticOperator.Int64Op.Shl | SemanticOperator.Int64Op.Shr
             | SemanticOperator.BigIntOp.Shl | SemanticOperator.BigIntOp.Shr =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhsType <- unifyTypeM(tvar, tpe1, loc)
            rhsType <- unifyTypeM(tpe2, Type.Int32, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, lhsType, resultEff)

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
            resultEff = Type.mkAnd(eff1, eff2)
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
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOperator.StringOp.Concat =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypeM(tvar, Type.Str, tpe1, tpe2, loc)
            resultEff = Type.mkAnd(eff1, eff2)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop' near ${loc.format}.")
      }

      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          condType <- unifyTypeM(Type.Bool, tpe1, loc)
          resultTyp <- unifyTypeM(tpe2, tpe3, loc)
          resultEff = Type.mkAnd(eff1, eff2, eff3)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case ResolvedAst.Expression.Stm(exp1, exp2, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkAnd(eff1, eff2)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.Let(sym, exp1, exp2, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          boundVar <- unifyTypeM(sym.tvar, tpe1, loc)
          resultTyp = tpe2
          resultEff = Type.mkAnd(eff1, eff2)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.Match(exp, rules, loc) =>
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
          resultEff = Type.mkAnd(eff :: guardEffects ::: bodyEffects)
        } yield (constrs ++ guardConstrs.flatten ++ bodyConstrs.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.Choose(star, exps0, rules0, tvar, loc) =>

        /**
          * Performs type inference on the given match expressions `exps` and nullity `vars`.
          *
          * Returns a pair of lists of the types and effects of the match expressions.
          */
        def visitMatchExps(exps: List[ResolvedAst.Expression], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type])] = {
          def visitMatchExp(exp: ResolvedAst.Expression, isAbsentVar: Type.Var, isPresentVar: Type.Var): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
            val freshElmVar = Type.freshVar(Kind.Star)
            for {
              (constrs, tpe, eff) <- visitExp(exp)
              _ <- unifyTypeM(tpe, Type.mkChoice(freshElmVar, isAbsentVar, isPresentVar), loc)
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
        def visitRuleBodies(rs: List[ResolvedAst.ChoiceRule]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type])] = {
          def visitRuleBody(r: ResolvedAst.ChoiceRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = r match {
            case ResolvedAst.ChoiceRule(_, exp0) => visitExp(exp0)
          }

          seqM(rs.map(visitRuleBody)).map(_.unzip3)
        }

        /**
          * Returns a transformed result type that encodes the Boolean constraint of each row pattern in the result type.
          *
          * NB: Requires that the `ts` types are Choice-types.
          */
        def transformResultTypes(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], rs: List[ResolvedAst.ChoiceRule], ts: List[Type], loc: SourceLocation): InferMonad[Type] = {
          def visitRuleBody(r: ResolvedAst.ChoiceRule, resultType: Type): InferMonad[(Type, Type, Type)] = r match {
            case ResolvedAst.ChoiceRule(r, exp0) =>
              val cond = mkOverApprox(isAbsentVars, isPresentVars, r)
              val innerType = Type.freshVar(Kind.Star)
              val isAbsentVar = Type.freshVar(Kind.Bool)
              val isPresentVar = Type.freshVar(Kind.Bool)
              for {
                choiceType <- unifyTypeM(resultType, Type.mkChoice(innerType, isAbsentVar, isPresentVar), loc)
              } yield (Type.mkAnd(cond, isAbsentVar), Type.mkAnd(cond, isPresentVar), innerType)
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
            isAbsentCond = Type.mkOr(isAbsentConds)
            isPresentCond = Type.mkOr(isPresentConds)
            innerType <- unifyTypeM(innerTypes, loc)
            resultType = Type.mkChoice(innerType, isAbsentCond, isPresentCond)
          } yield resultType
        }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an under-approximation.
          *
          * If a pattern is a wildcard it *must* always match.
          * If a pattern is `Absent`  its corresponding `isPresentVar` must be `false` (i.e. to prevent the value from being `Present`).
          * If a pattern is `Present` its corresponding `isAbsentVar`  must be `false` (i.e. to prevent the value from being `Absent`).
          */
        def mkUnderApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[ResolvedAst.ChoicePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, ResolvedAst.ChoicePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), ResolvedAst.ChoicePattern.Present(_, _, _))) =>
              // Case 2: A `Present` pattern forces the `isAbsentVar` to be equal to `false`.
              BoolUnification.mkAnd(acc, Type.mkEquiv(isAbsentVar, Type.False))
            case (acc, ((_, isPresentVar), ResolvedAst.ChoicePattern.Absent(_))) =>
              // Case 3: An `Absent` pattern forces the `isPresentVar` to be equal to `false`.
              BoolUnification.mkAnd(acc, Type.mkEquiv(isPresentVar, Type.False))
          }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an over-approximation.
          *
          * If a pattern is a wildcard it *may* always match.
          * If a pattern is `Absent` it *may* match if its corresponding `isAbsent` is `true`.
          * If a pattern is `Present` it *may* match if its corresponding `isPresentVar`is `true`.
          */
        def mkOverApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[ResolvedAst.ChoicePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, ResolvedAst.ChoicePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), ResolvedAst.ChoicePattern.Absent(_))) =>
              // Case 2: An `Absent` pattern may match if the `isAbsentVar` is `true`.
              BoolUnification.mkAnd(acc, isAbsentVar)
            case (acc, ((_, isPresentVar), ResolvedAst.ChoicePattern.Present(_, _, _))) =>
              // Case 3: A `Present` pattern may match if the `isPresentVar` is `true`.
              BoolUnification.mkAnd(acc, isPresentVar)
          }

        /**
          * Constructs a disjunction of the constraints of each choice rule.
          */
        def mkOuterDisj(m: List[List[ResolvedAst.ChoicePattern]], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): Type =
          m.foldLeft(Type.False) {
            case (acc, rule) => BoolUnification.mkOr(acc, mkUnderApprox(isAbsentVars, isPresentVars, rule))
          }

        /**
          * Performs type inference and unification with the `matchTypes` against the given choice rules `rs`.
          */
        def unifyMatchTypesAndRules(matchTypes: List[Type], rs: List[ResolvedAst.ChoiceRule]): InferMonad[List[List[Type]]] = {
          def unifyWithRule(r: ResolvedAst.ChoiceRule): InferMonad[List[Type]] = {
            seqM(matchTypes.zip(r.pat).map {
              case (matchType, ResolvedAst.ChoicePattern.Wild(_)) =>
                // Case 1: The pattern is wildcard. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, ResolvedAst.ChoicePattern.Absent(_)) =>
                // Case 2: The pattern is a `Absent`. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, ResolvedAst.ChoicePattern.Present(sym, tvar, loc)) =>
                // Case 3: The pattern is `Present`. Must constraint the type of the local variable with the type of the match expression.
                unifyTypeM(matchType, sym.tvar, tvar, loc)
            })
          }

          seqM(rs.map(unifyWithRule))
        }

        //
        // Introduce an isAbsent variable for each match expression in `exps`.
        //
        val isAbsentVars = exps0.map(_ => Type.freshVar(Kind.Bool))

        //
        // Introduce an isPresent variable for each math expression in `exps`.
        //
        val isPresentVars = exps0.map(_ => Type.freshVar(Kind.Bool))

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
          resultEff = Type.mkAnd(matchEff ::: ruleBodyEff)
        } yield (matchConstrs.flatten ++ ruleBodyConstrs.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        if (sym == Symbol.mkEnumSym("Choice")) {
          //
          // Special Case 1: Absent or Present Tag
          //
          if (tag.name == "Absent") {
            // Case 1.1: Absent Tag.
            val elmVar = Type.freshVar(Kind.Star)
            val isAbsent = Type.True
            val isPresent = Type.freshVar(Kind.Bool)
            for {
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(elmVar, isAbsent, isPresent), loc)
              resultEff = Type.Pure
            } yield (List.empty, resultTyp, resultEff)
          }
          else if (tag.name == "Present") {
            // Case 1.2: Present Tag.
            val isAbsent = Type.freshVar(Kind.Bool)
            val isPresent = Type.True
            for {
              (constrs, tpe, eff) <- visitExp(exp)
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(tpe, isAbsent, isPresent), loc)
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
            _ <- unifyTypeM(tagType, Type.mkTag(sym, tag, tpe, tvar), loc)
            resultTyp = tvar
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)
        }

      case ResolvedAst.Expression.Tuple(elms, loc) =>
        for {
          (elementConstrs, elementTypes, elementEffects) <- seqM(elms.map(visitExp)).map(_.unzip3)
          resultEff = Type.mkAnd(elementEffects)
        } yield (elementConstrs.flatten, Type.mkTuple(elementTypes), resultEff)

      case ResolvedAst.Expression.RecordEmpty(tvar, loc) =>
        //
        //  ---------
        //  { } : { }
        //
        for {
          resultType <- unifyTypeM(tvar, Type.RecordEmpty, loc)
        } yield (List.empty, resultType, Type.Pure)

      case ResolvedAst.Expression.RecordSelect(exp, field, tvar, loc) =>
        //
        // r : { field = tpe | row }
        // -------------------------
        // r.field : tpe
        //
        val freshRowVar = Type.freshVar(Kind.Record)
        val expectedType = Type.mkRecordExtend(field, tvar, freshRowVar)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, expectedType, loc)
          resultEff = eff
        } yield (constrs, tvar, resultEff)

      case ResolvedAst.Expression.RecordExtend(field, exp1, exp2, tvar, loc) =>
        //
        // exp1 : tpe
        // ---------------------------------------------
        // { field = exp1 | exp2 } : { field : tpe | r }
        //
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tvar, Type.mkRecordExtend(field, tpe1, tpe2), loc)
          resultEff = Type.mkAnd(eff1, eff2)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.RecordRestrict(field, exp, tvar, loc) =>
        //
        // ----------------------
        // { -field | r } : { r }
        //
        val freshFieldType = Type.freshVar(Kind.Star)
        val freshRowVar = Type.freshVar(Kind.Record)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, Type.mkRecordExtend(field, freshFieldType, freshRowVar), loc)
          resultTyp <- unifyTypeM(tvar, freshRowVar, loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
        //
        //  e1 : t ... en: t
        //  --------------------------------
        //  [e1,..., en] : Array[t] @ Impure
        //
        if (elms.isEmpty) {
          for {
            resultTyp <- unifyTypeM(tvar, Type.mkArray(Type.freshVar(Kind.Star)), loc)
            resultEff = Type.Impure
          } yield (List.empty, resultTyp, resultEff)
        } else {
          for {
            (constrs, elementTypes, _) <- seqM(elms.map(visitExp)).map(_.unzip3)
            elementType <- unifyTypeM(elementTypes, loc)
            resultTyp <- unifyTypeM(tvar, Type.mkArray(elementType), loc)
            resultEff = Type.Impure
          } yield (constrs.flatten, resultTyp, resultEff)
        }

      case ResolvedAst.Expression.ArrayNew(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : t @ _    exp2: Int @ _
        //  ---------------------------------
        //  [exp1 ; exp2] : Array[t] @ Impure
        //
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          lengthType <- unifyTypeM(tpe2, Type.Int32, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(tpe1), loc)
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : Array[t] @ _   exp2: Int @ _
        //  -----------------------------------
        //  exp1[exp2] : t @ Impure
        //
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          arrayType <- unifyTypeM(tpe1, Type.mkArray(tvar), loc)
          indexType <- unifyTypeM(tpe2, Type.Int32, loc)
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, tvar, resultEff)

      case ResolvedAst.Expression.ArrayLength(exp, loc) =>
        //
        //  exp : Array[t] @ e
        //  --------------------
        //  exp.length : Int @ e
        //
        val elementType = Type.freshVar(Kind.Star)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          arrayType <- unifyTypeM(tpe, Type.mkArray(elementType), loc)
          resultEff = eff
        } yield (constrs, Type.Int32, resultEff)

      case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, loc) =>
        //
        //  exp1 : Array[t] @ _   exp2 : Int @ _   exp3 : t @ _
        //  ---------------------------------------------------
        //  exp1[exp2] = exp3 : Unit @ Impure
        //
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          (constrs3, tpe3, _) <- visitExp(exp3)
          arrayType <- unifyTypeM(tpe1, Type.mkArray(tpe3), loc)
          indexType <- unifyTypeM(tpe2, Type.Int32, loc)
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2 ++ constrs3, Type.Unit, resultEff)

      case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, loc) =>
        //
        //  exp1 : Array[t] @ _   exp2 : Int @ _   exp3 : Int @ _
        //  -----------------------------------------------------
        //  exp1[exp2..exp3] : Array[t] @ Impure
        //
        val elementType = Type.freshVar(Kind.Star)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          (constrs3, tpe3, _) <- visitExp(exp3)
          fstIndexType <- unifyTypeM(tpe2, Type.Int32, loc)
          lstIndexType <- unifyTypeM(tpe3, Type.Int32, loc)
          resultTyp <- unifyTypeM(tpe1, Type.mkArray(elementType), loc)
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case ResolvedAst.Expression.Ref(exp, loc) =>
        //
        //  exp : t @ eff
        //  -------------------------
        //  ref exp : Ref[t] @ Impure
        //
        for {
          (constrs, tpe, _) <- visitExp(exp)
          resultTyp = Type.mkRef(tpe)
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
        //
        //  exp : Ref[t] @ eff
        //  -------------------
        //  deref exp : t @ Impure
        //
        val elementType = Type.freshVar(Kind.Star)
        for {
          (constrs, typ, _) <- visitExp(exp)
          refType <- unifyTypeM(typ, Type.mkRef(elementType), loc)
          resultTyp <- unifyTypeM(tvar, elementType, loc)
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.Assign(exp1, exp2, loc) =>
        //
        //  exp1 : Ref[t] @ eff1   exp2: t @ eff2
        //  -------------------------------------
        //  exp1 := exp2 : Unit @ Impure
        //
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          refType <- unifyTypeM(tpe1, Type.mkRef(tpe2), loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
        for {
          paramTyp <- unifyTypeM(fparam.sym.tvar, fparam.tpe, loc)
          (constrs, typ, eff) <- visitExp(exp)
          resultTyp <- unifyTypeM(typ, Type.Bool, loc)
        } yield (constrs, resultTyp, Type.Pure)

      case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
        for {
          paramTyp <- unifyTypeM(fparam.sym.tvar, fparam.tpe, loc)
          (constrs, typ, eff) <- visitExp(exp)
          resultTyp <- unifyTypeM(typ, Type.Bool, loc)
        } yield (constrs, resultTyp, Type.Pure)

      case ResolvedAst.Expression.Ascribe(exp, expectedTyp, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (constrs, actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, actualTyp, expectedTyp.getOrElse(tvar), loc)
          resultEff <- unifyBoolM(actualEff, expectedEff.getOrElse(Type.freshVar(Kind.Bool)), loc)
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.Cast(exp, declaredTyp, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        for {
          (constrs, actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, declaredTyp.getOrElse(actualTyp), loc)
          resultEff = declaredEff.getOrElse(actualEff)
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.TryCatch(exp, rules, loc) =>
        val rulesType = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          (ruleConstrs, ruleTypes, ruleEffects) <- seqM(rulesType).map(_.unzip3)
          ruleType <- unifyTypeM(ruleTypes, loc)
          resultTyp <- unifyTypeM(tpe, ruleType, loc)
          resultEff = Type.mkAnd(eff :: ruleEffects)
        } yield (constrs ++ ruleConstrs.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val classType = getFlixType(constructor.getDeclaringClass)
        for {
          (constrs, _, _) <- seqM(args.map(visitExp)).map(_.unzip3)
          resultTyp = classType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.InvokeMethod(method, exp, args, loc) =>
        val classType = getFlixType(method.getDeclaringClass)
        val returnType = getFlixType(method.getReturnType)
        for {
          (baseConstrs, baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          (constrs, tpes, effs) <- seqM(args.map(visitExp)).map(_.unzip3)
          resultTyp = getFlixType(method.getReturnType)
          resultEff = Type.Impure
        } yield (baseConstrs ++ constrs.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val returnType = getFlixType(method.getReturnType)
        for {
          (constrs, tpes, effs) <- seqM(args.map(visitExp)).map(_.unzip3)
          resultTyp = returnType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case ResolvedAst.Expression.GetField(field, exp, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(field.getDeclaringClass)
        for {
          (baseConstrs, baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          resultTyp = fieldType
          resultEff = Type.Impure
        } yield (baseConstrs, resultTyp, resultEff)

      case ResolvedAst.Expression.PutField(field, exp1, exp2, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(field.getDeclaringClass)
        for {
          (baseConstrs, baseTyp, _) <- visitExp(exp1)
          (valueConstrs, valueType, _) <- visitExp(exp2)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          valueTyp <- unifyTypeM(valueType, fieldType, loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (baseConstrs ++ valueConstrs, resultTyp, resultEff)

      case ResolvedAst.Expression.GetStaticField(field, loc) =>
        val fieldType = getFlixType(field.getType)
        val resultTyp = fieldType
        val resultEff = Type.Impure
        liftM(List.empty, resultTyp, resultEff)

      case ResolvedAst.Expression.PutStaticField(field, exp, loc) =>
        for {
          (valueConstrs, valueTyp, _) <- visitExp(exp)
          fieldTyp <- unifyTypeM(getFlixType(field.getType), valueTyp, loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (valueConstrs, resultTyp, resultEff)

      case ResolvedAst.Expression.NewChannel(exp, declaredType, loc) =>
        //
        //  exp: Int @ _
        //  ---------------------------------
        //  channel exp : Channel[t] @ Impure
        //
        for {
          (constrs, tpe, _) <- visitExp(exp)
          lengthType <- unifyTypeM(tpe, Type.Int32, loc)
          resultTyp <- liftM(Type.mkChannel(declaredType))
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.GetChannel(exp, tvar, loc) =>
        val elementType = Type.freshVar(Kind.Star)
        for {
          (constrs, tpe, _) <- visitExp(exp)
          channelType <- unifyTypeM(tpe, Type.mkChannel(elementType), loc)
          resultTyp <- unifyTypeM(tvar, elementType, loc)
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, loc) =>
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tvar, tpe1, Type.mkChannel(tpe2), loc)
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.SelectChannel(rules, default, tvar, loc) =>

        /**
          * Performs type inference on the given select rule `sr0`.
          */
        def inferSelectRule(sr0: ResolvedAst.SelectChannelRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          sr0 match {
            case ResolvedAst.SelectChannelRule.SelectGet(sym, chan, body) => for {
              (chanConstrs, chanType, _) <- visitExp(chan)
              (bodyConstrs, bodyType, _) <- visitExp(body)
              _ <- unifyTypeM(chanType, Type.mkChannel(sym.tvar, sym.loc), sym.loc)
              resultCon = chanConstrs ++ bodyConstrs
              resultTyp = bodyType
              resultEff = Type.Impure
            } yield (resultCon, resultTyp, resultEff)

            case ResolvedAst.SelectChannelRule.SelectPut(chan, value, body) => for {
              (chanConstrs, chanType, _) <- visitExp(chan)
              (valueConstrs, valueType, _) <- visitExp(value)
              (bodyConstrs, bodyType, _) <- visitExp(body)
              // TODO: Should I unify something here?
              resultCon = chanConstrs ++ valueConstrs ++ bodyConstrs
              resultTyp = bodyType
              resultEff = Type.Impure
            } yield (resultCon, resultTyp, resultEff)
          }

        /**
          * Performs type inference on the given optional default expression `exp0`.
          */
        def inferDefaultRule(exp0: Option[ResolvedAst.Expression]): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          exp0 match {
            case None => liftM(Nil, Type.freshVar(Kind.Star), Type.Pure)
            case Some(exp) => visitExp(exp)
          }

        for {
          (ruleConstrs, ruleTypes, _) <- seqM(rules.map(inferSelectRule)).map(_.unzip3)
          (defaultConstrs, defaultType, _) <- inferDefaultRule(default)
          resultCon = ruleConstrs.flatten ++ defaultConstrs
          resultTyp <- unifyTypeM(tvar :: defaultType :: ruleTypes, loc)
          resultEff = Type.Impure
        } yield (resultCon, resultTyp, resultEff)

      case ResolvedAst.Expression.Spawn(exp, loc) =>
        for {
          (constrs, tpe, _) <- visitExp(exp)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.Lazy(exp, loc) =>
        //
        //  exp: t & Pure
        //  -------------------------
        //  lazy exp : Lazy[t] @ Pure
        //
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp = Type.mkLazy(tpe)
          resultEff <- unifyTypeM(Type.Pure, eff, loc)
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.Force(exp, tvar, loc) =>
        //
        //  exp: Lazy[t] @ e
        //  -------------------------
        //  force exp : t @ e
        //
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          lazyTyp <- unifyTypeM(tpe, Type.mkLazy(tvar), loc)
          resultTyp = tvar
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointConstraintSet(cs, tvar, loc) =>
        for {
          (constrs, constraintTypes) <- seqM(cs.map(visitConstraint)).map(_.unzip)
          resultTyp <- unifyTypeAllowEmptyM(tvar :: constraintTypes, loc)
        } yield (constrs.flatten, resultTyp, Type.Pure)

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tpe1, tpe2, mkAnySchemaType(), loc)
          resultEff = Type.mkAnd(eff1, eff2)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointSolve(exp, loc) =>
        //
        //  exp : #{...}
        //  ---------------
        //  solve exp : tpe
        //
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tpe, mkAnySchemaType(), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointProject(pred, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : #{ P : a  | b }
        //  -------------------------------------------
        //  project P exp2 : #{ P : a | c }
        //
        val freshPredicateTypeVar = Type.freshVar(Kind.Star)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.Schema)
        val freshResultSchemaTypeVar = Type.freshVar(Kind.Schema)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkSchemaExtend(pred, freshPredicateTypeVar, freshRestSchemaTypeVar), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchemaExtend(pred, freshPredicateTypeVar, freshResultSchemaTypeVar), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 |= exp2 : Bool
        //
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          schemaType <- unifyTypeM(tpe1, tpe2, mkAnySchemaType(), loc)
          resultTyp = Type.Bool
          resultEff = Type.mkAnd(eff1, eff2)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointFold(pred, exp1, exp2, exp3, tvar, loc) =>
        //
        // exp3 : #{P : a | c}    init : b   exp2 : a' -> b -> b
        // where a' is the tuple reification of relation a
        // ---------------------------------------------------
        // fold P exp1 exp2 exp3 : b
        //
        val freshPredicateNameTypeVar = Type.freshVar(Kind.Star ->: Kind.Star)
        val tupleType = Type.freshVar(Kind.Star)
        val restRow = Type.freshVar(Kind.Schema)
        for {
          (constrs1, initType, eff1) <- visitExp(exp1)
          (constrs2, fType, eff2) <- visitExp(exp2)
          (constrs3, constraintsType, eff3) <- visitExp(exp3)
          // constraints should have the form {pred.sym : R(tupleType) | freshRestTypeVar}
          constraintsType2 <- unifyTypeM(constraintsType, Type.mkSchemaExtend(pred, Type.Apply(freshPredicateNameTypeVar, tupleType), restRow), loc)
          // f is of type tupleType -> initType -> initType. It cannot have any effect.
          fType2 <- unifyTypeM(fType, Type.mkPureArrow(tupleType, Type.mkPureArrow(initType, initType)), loc)
          resultTyp <- unifyTypeM(tvar, initType, loc) // the result of the fold is the same type as init
          resultEff = Type.mkAnd(eff1, eff2, eff3)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)
    }

    /**
      * Infers the type of the given constraint `con0` inside the inference monad.
      */
    def visitConstraint(con0: ResolvedAst.Constraint): InferMonad[(List[Ast.TypeConstraint], Type)] = {
      val ResolvedAst.Constraint(cparams, head0, body0, loc) = con0
      //
      //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
      //  -----------------------------------
      //  A_0 :- A_1, ..., A_n : tpe
      //
      for {
        (constrs1, headPredicateType) <- inferHeadPredicate(head0, root)
        (constrs2, bodyPredicateTypes) <- seqM(body0.map(b => inferBodyPredicate(b, root))).map(_.unzip)
        bodyPredicateType <- unifyTypeAllowEmptyM(bodyPredicateTypes, loc)
        resultType <- unifyTypeM(headPredicateType, bodyPredicateType, loc)
      } yield (constrs1 ++ constrs2.flatten, resultType)
    }

    visitExp(exp0)
  }

  /**
    * Applies the given substitution `subst0` to the given expression `exp0`.
    */
  private def reassembleExp(exp0: ResolvedAst.Expression, root: ResolvedAst.Root, subst0: Substitution): TypedAst.Expression = {
    /**
      * Applies the given substitution `subst0` to the given expression `exp0`.
      */
    def visitExp(exp0: ResolvedAst.Expression, subst0: Substitution): TypedAst.Expression = exp0 match {

      case ResolvedAst.Expression.Wild(tvar, loc) =>
        TypedAst.Expression.Wild(subst0(tvar), loc)

      case ResolvedAst.Expression.Var(sym, tvar, loc) =>
        TypedAst.Expression.Var(sym, subst0(sym.tvar), loc)

      case ResolvedAst.Expression.Def(sym, tvar, loc) =>
        TypedAst.Expression.Def(sym, subst0(tvar), loc)

      case ResolvedAst.Expression.Sig(sym, tvar, loc) =>
        TypedAst.Expression.Sig(sym, subst0(tvar), loc)

      case ResolvedAst.Expression.Hole(sym, tpe, evar, loc) =>
        TypedAst.Expression.Hole(sym, subst0(tpe), subst0(evar), loc)

      case ResolvedAst.Expression.Unit(loc) => TypedAst.Expression.Unit(loc)

      case ResolvedAst.Expression.Null(loc) => TypedAst.Expression.Null(Type.Unit, loc)

      case ResolvedAst.Expression.True(loc) => TypedAst.Expression.True(loc)

      case ResolvedAst.Expression.False(loc) => TypedAst.Expression.False(loc)

      case ResolvedAst.Expression.Char(lit, loc) => TypedAst.Expression.Char(lit, loc)

      case ResolvedAst.Expression.Float32(lit, loc) => TypedAst.Expression.Float32(lit, loc)

      case ResolvedAst.Expression.Float64(lit, loc) => TypedAst.Expression.Float64(lit, loc)

      case ResolvedAst.Expression.Int8(lit, loc) => TypedAst.Expression.Int8(lit, loc)

      case ResolvedAst.Expression.Int16(lit, loc) => TypedAst.Expression.Int16(lit, loc)

      case ResolvedAst.Expression.Int32(lit, loc) => TypedAst.Expression.Int32(lit, loc)

      case ResolvedAst.Expression.Int64(lit, loc) => TypedAst.Expression.Int64(lit, loc)

      case ResolvedAst.Expression.BigInt(lit, loc) => TypedAst.Expression.BigInt(lit, loc)

      case ResolvedAst.Expression.Str(lit, loc) => TypedAst.Expression.Str(lit, loc)

      case ResolvedAst.Expression.Default(tvar, loc) => TypedAst.Expression.Default(subst0(tvar), loc)

      case ResolvedAst.Expression.Apply(exp, exps, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        val es = exps.map(visitExp(_, subst0))
        TypedAst.Expression.Apply(e, es, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val p = visitParam(fparam)
        val e = visitExp(exp, subst0)
        val t = subst0(tvar)
        TypedAst.Expression.Lambda(p, e, t, loc)

      case ResolvedAst.Expression.Unary(sop, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Unary(sop, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Binary(sop, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.Binary(sop, e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff, e3.eff)
        TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case ResolvedAst.Expression.Stm(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.Stm(e1, e2, tpe, eff, loc)

      case ResolvedAst.Expression.Let(sym, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.Let(sym, e1, e2, tpe, eff, loc)

      case ResolvedAst.Expression.Match(matchExp, rules, loc) =>
        val e1 = visitExp(matchExp, subst0)
        val rs = rules map {
          case ResolvedAst.MatchRule(pat, guard, exp) =>
            val p = reassemblePattern(pat, root, subst0)
            val g = visitExp(guard, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.MatchRule(p, g, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = rs.foldLeft(e1.eff) {
          case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkAnd(g.eff, b.eff, acc)
        }
        TypedAst.Expression.Match(e1, rs, tpe, eff, loc)

      case ResolvedAst.Expression.Choose(_, exps, rules, tvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val rs = rules.map {
          case ResolvedAst.ChoiceRule(pat0, exp) =>
            val pat = pat0.map {
              case ResolvedAst.ChoicePattern.Wild(loc) => TypedAst.ChoicePattern.Wild(loc)
              case ResolvedAst.ChoicePattern.Absent(loc) => TypedAst.ChoicePattern.Absent(loc)
              case ResolvedAst.ChoicePattern.Present(sym, tvar, loc) => TypedAst.ChoicePattern.Present(sym, subst0(tvar), loc)
            }
            TypedAst.ChoiceRule(pat, visitExp(exp, subst0))
        }
        val tpe = subst0(tvar)
        val eff = Type.mkAnd(rs.map(_.exp.eff))
        TypedAst.Expression.Choose(es, rs, tpe, eff, loc)

      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Tag(sym, tag, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Tuple(elms, loc) =>
        val es = elms.map(visitExp(_, subst0))
        val tpe = Type.mkTuple(es.map(_.tpe))
        val eff = Type.mkAnd(es.map(_.eff))
        TypedAst.Expression.Tuple(es, tpe, eff, loc)

      case ResolvedAst.Expression.RecordEmpty(tvar, loc) =>
        TypedAst.Expression.RecordEmpty(subst0(tvar), loc)

      case ResolvedAst.Expression.RecordSelect(exp, field, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.RecordSelect(e, field, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.RecordExtend(field, value, rest, tvar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        val eff = Type.mkAnd(v.eff, r.eff)
        TypedAst.Expression.RecordExtend(field, v, r, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.RecordRestrict(field, rest, tvar, loc) =>
        val r = visitExp(rest, subst0)
        val eff = r.eff
        TypedAst.Expression.RecordRestrict(field, r, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        val eff = Type.Impure
        TypedAst.Expression.ArrayLit(es, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.ArrayNew(elm, len, tvar, loc) =>
        val e = visitExp(elm, subst0)
        val ln = visitExp(len, subst0)
        val eff = Type.Impure
        TypedAst.Expression.ArrayNew(e, ln, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.Impure
        TypedAst.Expression.ArrayLoad(e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.ArrayStore(e1, e2, e3, loc)

      case ResolvedAst.Expression.ArrayLength(exp, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.ArrayLength(e, eff, loc)

      case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e1.tpe
        TypedAst.Expression.ArraySlice(e1, e2, e3, tpe, loc)

      case ResolvedAst.Expression.Ref(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.mkRef(e.tpe)
        val eff = Type.Impure
        TypedAst.Expression.Ref(e, tpe, eff, loc)

      case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.Deref(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Assign(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expression.Assign(e1, e2, tpe, eff, loc)

      case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Existential(visitParam(fparam), e, loc)

      case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Universal(visitParam(fparam), e, loc)

      case ResolvedAst.Expression.Ascribe(exp, _, _, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Ascribe(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Cast(ResolvedAst.Expression.Null(_), _, _, tvar, loc) =>
        val t = subst0(tvar)
        TypedAst.Expression.Null(t, loc)

      case ResolvedAst.Expression.Cast(exp, _, declaredEff, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = declaredEff.getOrElse(e.eff)
        TypedAst.Expression.Cast(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.TryCatch(exp, rules, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = Type.mkAnd(rs.map(_.exp.eff))
        TypedAst.Expression.TryCatch(e, rs, tpe, eff, loc)

      case ResolvedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(constructor.getDeclaringClass)
        val eff = Type.Impure
        TypedAst.Expression.InvokeConstructor(constructor, as, tpe, eff, loc)

      case ResolvedAst.Expression.InvokeMethod(method, exp, args, loc) =>
        val e = visitExp(exp, subst0)
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val eff = Type.Impure
        TypedAst.Expression.InvokeMethod(method, e, as, tpe, eff, loc)

      case ResolvedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val eff = Type.Impure
        TypedAst.Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

      case ResolvedAst.Expression.GetField(field, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = getFlixType(field.getType)
        val eff = Type.Impure
        TypedAst.Expression.GetField(field, e, tpe, eff, loc)

      case ResolvedAst.Expression.PutField(field, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expression.PutField(field, e1, e2, tpe, eff, loc)

      case ResolvedAst.Expression.GetStaticField(field, loc) =>
        val tpe = getFlixType(field.getType)
        val eff = Type.Impure
        TypedAst.Expression.GetStaticField(field, tpe, eff, loc)

      case ResolvedAst.Expression.PutStaticField(field, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expression.PutStaticField(field, e, tpe, eff, loc)

      case ResolvedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.NewChannel(e, Type.mkChannel(tpe), eff, loc)

      case ResolvedAst.Expression.GetChannel(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.GetChannel(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.Impure
        TypedAst.Expression.PutChannel(e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.SelectChannel(rules, default, tvar, loc) =>
        val rs = rules map {
          case ResolvedAst.SelectChannelRule.SelectGet(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule.SelectGet(sym, c, b)
          case ResolvedAst.SelectChannelRule.SelectPut(chan, value, exp) =>
            val c = visitExp(chan, subst0)
            val v = visitExp(value, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule.SelectPut(c, v, b)
        }
        val d = default.map(visitExp(_, subst0))
        val eff = Type.Impure
        TypedAst.Expression.SelectChannel(rs, d, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Spawn(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.Unit
        val eff = e.eff
        TypedAst.Expression.Spawn(e, tpe, eff, loc)

      case ResolvedAst.Expression.Lazy(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.mkLazy(e.tpe)
        TypedAst.Expression.Lazy(e, tpe, loc)

      case ResolvedAst.Expression.Force(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = e.eff
        TypedAst.Expression.Force(e, tpe, eff, loc)

      case ResolvedAst.Expression.FixpointConstraintSet(cs0, tvar, loc) =>
        val cs = cs0.map(visitConstraint)
        TypedAst.Expression.FixpointConstraintSet(cs, Stratification.Empty, subst0(tvar), loc)

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e1.tpe
        val eff = Type.mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.FixpointCompose(e1, e2, Stratification.Empty, tpe, eff, loc)

      case ResolvedAst.Expression.FixpointSolve(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = e.eff
        TypedAst.Expression.FixpointSolve(e, Stratification.Empty, tpe, eff, loc)

      case ResolvedAst.Expression.FixpointProject(pred, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.FixpointProject(pred, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Bool
        val eff = Type.mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.FixpointEntails(e1, e2, tpe, eff, loc)

      case ResolvedAst.Expression.FixpointFold(pred, init, f, constraints, tvar, loc) =>
        val e1 = visitExp(init, subst0)
        val e2 = visitExp(f, subst0)
        val e3 = visitExp(constraints, subst0)
        val eff = Type.mkAnd(e1.eff, e2.eff, e3.eff)
        TypedAst.Expression.FixpointFold(pred, e1, e2, e3, subst0(tvar), eff, loc)
    }

    /**
      * Applies the substitution to the given constraint.
      */
    def visitConstraint(c0: ResolvedAst.Constraint): TypedAst.Constraint = {
      // Pattern match on the constraint.
      val ResolvedAst.Constraint(cparams0, head0, body0, loc) = c0

      // Unification was successful. Reassemble the head and body predicates.
      val head = reassembleHeadPredicate(head0, root, subst0)
      val body = body0.map(b => reassembleBodyPredicate(b, root, subst0))

      // Reassemble the constraint parameters.
      val cparams = cparams0.map {
        case ResolvedAst.ConstraintParam.HeadParam(sym, tpe, l) =>
          TypedAst.ConstraintParam.HeadParam(sym, subst0(tpe), l)
        case ResolvedAst.ConstraintParam.RuleParam(sym, tpe, l) =>
          TypedAst.ConstraintParam.RuleParam(sym, subst0(tpe), l)
      }

      // Reassemble the constraint.
      TypedAst.Constraint(cparams, head, body, loc)
    }

    /**
      * Applies the substitution to the given list of formal parameters.
      */
    def visitParam(param: ResolvedAst.FormalParam): TypedAst.FormalParam =
      TypedAst.FormalParam(param.sym, param.mod, subst0(param.tpe), param.loc)

    visitExp(exp0, subst0)
  }

  /**
    * Infers the type of the given pattern `pat0`.
    */
  private def inferPattern(pat0: ResolvedAst.Pattern, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[Type] = {
    /**
      * Local pattern visitor.
      */
    def visit(p: ResolvedAst.Pattern): InferMonad[Type] = p match {
      case ResolvedAst.Pattern.Wild(tvar, loc) => liftM(tvar)

      case ResolvedAst.Pattern.Var(sym, tvar, loc) => unifyTypeM(sym.tvar, tvar, loc)

      case ResolvedAst.Pattern.Unit(loc) => liftM(Type.Unit)

      case ResolvedAst.Pattern.True(loc) => liftM(Type.Bool)

      case ResolvedAst.Pattern.False(loc) => liftM(Type.Bool)

      case ResolvedAst.Pattern.Char(c, loc) => liftM(Type.Char)

      case ResolvedAst.Pattern.Float32(i, loc) => liftM(Type.Float32)

      case ResolvedAst.Pattern.Float64(i, loc) => liftM(Type.Float64)

      case ResolvedAst.Pattern.Int8(i, loc) => liftM(Type.Int8)

      case ResolvedAst.Pattern.Int16(i, loc) => liftM(Type.Int16)

      case ResolvedAst.Pattern.Int32(i, loc) => liftM(Type.Int32)

      case ResolvedAst.Pattern.Int64(i, loc) => liftM(Type.Int64)

      case ResolvedAst.Pattern.BigInt(i, loc) => liftM(Type.BigInt)

      case ResolvedAst.Pattern.Str(s, loc) => liftM(Type.Str)

      case ResolvedAst.Pattern.Tag(sym, tag, pat, tvar, loc) =>
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
          _ <- unifyTypeM(tagType, Type.mkTag(sym, tag, tpe, tvar), loc)
          resultTyp = tvar
        } yield resultTyp

      case ResolvedAst.Pattern.Tuple(elms, loc) =>
        for {
          elementTypes <- seqM(elms map visit)
        } yield Type.mkTuple(elementTypes)

      case ResolvedAst.Pattern.Array(elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          elementType <- unifyTypeAllowEmptyM(elementTypes, loc);
          resultType <- unifyTypeM(tvar, Type.mkArray(elementType), loc)
        ) yield resultType

      case ResolvedAst.Pattern.ArrayTailSpread(elms, varSym, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          elementType <- unifyTypeAllowEmptyM(elementTypes, loc);
          arrayType <- unifyTypeM(tvar, Type.mkArray(elementType), loc);
          resultType <- unifyTypeM(varSym.tvar, arrayType, loc)
        ) yield resultType

      case ResolvedAst.Pattern.ArrayHeadSpread(varSym, elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          elementType <- unifyTypeAllowEmptyM(elementTypes, loc);
          arrayType <- unifyTypeM(tvar, Type.mkArray(elementType), loc);
          resultType <- unifyTypeM(varSym.tvar, arrayType, loc)
        ) yield resultType

    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[ResolvedAst.Pattern], root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[List[Type]] = {
    seqM(pats0.map(p => inferPattern(p, root)))
  }

  /**
    * Applies the substitution `subst0` to the given pattern `pat0`.
    */
  private def reassemblePattern(pat0: ResolvedAst.Pattern, root: ResolvedAst.Root, subst0: Substitution): TypedAst.Pattern = {
    /**
      * Local pattern visitor.
      */
    def visit(p: ResolvedAst.Pattern): TypedAst.Pattern = p match {
      case ResolvedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst0(tvar), loc)
      case ResolvedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym, subst0(tvar), loc)
      case ResolvedAst.Pattern.Unit(loc) => TypedAst.Pattern.Unit(loc)
      case ResolvedAst.Pattern.True(loc) => TypedAst.Pattern.True(loc)
      case ResolvedAst.Pattern.False(loc) => TypedAst.Pattern.False(loc)
      case ResolvedAst.Pattern.Char(lit, loc) => TypedAst.Pattern.Char(lit, loc)
      case ResolvedAst.Pattern.Float32(lit, loc) => TypedAst.Pattern.Float32(lit, loc)
      case ResolvedAst.Pattern.Float64(lit, loc) => TypedAst.Pattern.Float64(lit, loc)
      case ResolvedAst.Pattern.Int8(lit, loc) => TypedAst.Pattern.Int8(lit, loc)
      case ResolvedAst.Pattern.Int16(lit, loc) => TypedAst.Pattern.Int16(lit, loc)
      case ResolvedAst.Pattern.Int32(lit, loc) => TypedAst.Pattern.Int32(lit, loc)
      case ResolvedAst.Pattern.Int64(lit, loc) => TypedAst.Pattern.Int64(lit, loc)
      case ResolvedAst.Pattern.BigInt(lit, loc) => TypedAst.Pattern.BigInt(lit, loc)
      case ResolvedAst.Pattern.Str(lit, loc) => TypedAst.Pattern.Str(lit, loc)

      case ResolvedAst.Pattern.Tag(sym, tag, pat, tvar, loc) => TypedAst.Pattern.Tag(sym, tag, visit(pat), subst0(tvar), loc)

      case ResolvedAst.Pattern.Tuple(elms, loc) =>
        val es = elms.map(visit)
        val tpe = Type.mkTuple(es.map(_.tpe))
        TypedAst.Pattern.Tuple(es, tpe, loc)

      case ResolvedAst.Pattern.Array(elms, tvar, loc) => TypedAst.Pattern.Array(elms map visit, subst0(tvar), loc)
      case ResolvedAst.Pattern.ArrayTailSpread(elms, sym, tvar, loc) => TypedAst.Pattern.ArrayTailSpread(elms map visit, sym, subst0(tvar), loc)
      case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms, tvar, loc) => TypedAst.Pattern.ArrayHeadSpread(sym, elms map visit, subst0(tvar), loc)
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given head predicate.
    */
  private def inferHeadPredicate(head: ResolvedAst.Predicate.Head, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = head match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc) =>
      // Adds additional type constraints if the denotation is a lattice.
      val restRow = Type.freshVar(Kind.Schema)
      for {
        (termConstrs, termTypes, termEffects) <- seqM(terms.map(inferExp(_, root))).map(_.unzip3)
        pureTermEffects <- unifyBoolM(Type.Pure, Type.mkAnd(termEffects), loc)
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root)
      } yield (termConstrs.flatten ++ tconstrs, Type.mkSchemaExtend(pred, predicateType, restRow))

    case ResolvedAst.Predicate.Head.Union(exp, tvar, loc) =>
      //
      //  exp : typ
      //  ------------------------------------------------------------
      //  union exp : #{ ... }
      //
      for {
        (tconstrs, typ, eff) <- inferExp(exp, root)
        pureEff <- unifyBoolM(Type.Pure, eff, loc)
        resultType <- unifyTypeM(tvar, typ, loc)
      } yield (tconstrs, resultType)
  }

  /**
    * Applies the given substitution `subst0` to the given head predicate `head0`.
    */
  private def reassembleHeadPredicate(head0: ResolvedAst.Predicate.Head, root: ResolvedAst.Root, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case ResolvedAst.Predicate.Head.Atom(pred, den0, terms, tvar, loc) =>
      val ts = terms.map(t => reassembleExp(t, root, subst0))
      TypedAst.Predicate.Head.Atom(pred, den0, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Head.Union(exp, tvar, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Head.Union(e, subst0(tvar), loc)
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: ResolvedAst.Predicate.Body, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = body0 match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms, tvar, loc) =>
      val restRow = Type.freshVar(Kind.Schema)
      for {
        termTypes <- seqM(terms.map(inferPattern(_, root)))
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root)
      } yield (tconstrs, Type.mkSchemaExtend(pred, predicateType, restRow))

    //
    //  exp : Bool
    //  ----------
    //  if exp : a
    //
    case ResolvedAst.Predicate.Body.Guard(exp, loc) =>
      // Infer the types of the terms.
      for {
        (constrs, tpe, eff) <- inferExp(exp, root)
        expEff <- unifyBoolM(Type.Pure, eff, loc)
        expTyp <- unifyTypeM(Type.Bool, tpe, loc)
      } yield (constrs, mkAnySchemaType())
  }

  /**
    * Applies the given substitution `subst0` to the given body predicate `body0`.
    */
  private def reassembleBodyPredicate(body0: ResolvedAst.Predicate.Body, root: ResolvedAst.Root, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case ResolvedAst.Predicate.Body.Atom(pred, den0, polarity, terms, tvar, loc) =>
      val ts = terms.map(t => reassemblePattern(t, root, subst0))
      TypedAst.Predicate.Body.Atom(pred, den0, polarity, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Body.Guard(exp, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Body.Guard(e, loc)
  }

  /**
    * Returns the relation or lattice type of `name` with the term types `ts`.
    */
  private def mkRelationOrLatticeType(name: String, den: Denotation, ts: List[Type], root: ResolvedAst.Root)(implicit flix: Flix): Type = den match {
    case Denotation.Relational => Type.mkRelation(ts)
    case Denotation.Latticenal => Type.mkLattice(ts)
  }

  /**
    * Returns the type class constraints for the given term types `ts` with the given denotation `den`.
    */
  private def getTermTypeClassConstraints(den: Ast.Denotation, ts: List[Type], root: ResolvedAst.Root): List[Ast.TypeConstraint] = den match {
    case Denotation.Relational =>
      ts.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root))
    case Denotation.Latticenal =>
      ts.init.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root)) ::: mkTypeClassConstraintsForLatticeTerm(ts.last, root)
  }

  /**
    * Constructs the type class constraints for the given relational term type `tpe`.
    */
  private def mkTypeClassConstraintsForRelationalTerm(tpe: Type, root: ResolvedAst.Root): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Hash", root),
      PredefinedClasses.lookupClassSym("ToString", root),
    )
    classes.map(Ast.TypeConstraint(_, tpe))
  }

  /**
    * Constructs the type class constraints for the given lattice term type `tpe`.
    */
  private def mkTypeClassConstraintsForLatticeTerm(tpe: Type, root: ResolvedAst.Root): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Hash", root),
      PredefinedClasses.lookupClassSym("ToString", root),
      PredefinedClasses.lookupClassSym("PartialOrder", root),
      PredefinedClasses.lookupClassSym("LowerBound", root),
      PredefinedClasses.lookupClassSym("JoinLattice", root),
      PredefinedClasses.lookupClassSym("MeetLattice", root),
    )
    classes.map(Ast.TypeConstraint(_, tpe))
  }

  /**
    * Performs type resolution on the given attribute `attr`.
    */
  private def typeCheckAttribute(attr: ResolvedAst.Attribute): Result[TypedAst.Attribute, TypeError] = attr match {
    case ResolvedAst.Attribute(ident, tpe, loc) => Ok(TypedAst.Attribute(ident.name, tpe, loc))
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    */
  private def getSubstFromParams(params: List[ResolvedAst.FormalParam])(implicit flix: Flix): Substitution = {
    // Compute the substitution by mapping the symbol of each parameter to its declared type.
    val declaredTypes = params.map(_.tpe)
    (params zip declaredTypes).foldLeft(Substitution.empty) {
      case (macc, (ResolvedAst.FormalParam(sym, _, _, _), declaredType)) =>
        macc ++ Substitution.singleton(sym.tvar, declaredType)
    }
  }

  /**
    * Returns the typed version of the given type parameters `tparams0`.
    */
  private def getTypeParams(tparams0: List[ResolvedAst.TypeParam]): List[TypedAst.TypeParam] = tparams0.map {
    case ResolvedAst.TypeParam(name, tpe, classes, loc) => TypedAst.TypeParam(name, tpe, classes, loc)
  }

  /**
    * Returns the typed version of the given formal parameters `fparams0`.
    */
  private def getFormalParams(fparams0: List[ResolvedAst.FormalParam], subst0: Substitution): List[TypedAst.FormalParam] = fparams0.map {
    case ResolvedAst.FormalParam(sym, mod, tpe, loc) => TypedAst.FormalParam(sym, mod, subst0(tpe), sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaType()(implicit flix: Flix): Type = Type.freshVar(Kind.Schema)

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
      Type.mkArray(elmType)
    }
    // otherwise native type
    else {
      Type.mkNative(c)
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
