/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Ast.{CheckedCastType, Denotation}
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.inference.RestrictableChooseInference
import ca.uwaterloo.flix.language.phase.unification.InferMonad.{seqM, traverseM}
import ca.uwaterloo.flix.language.phase.unification.TypeMinimization.minimizeScheme
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{mapN, traverse, traverseValues}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.{Chain, ListMap}

import java.io.PrintWriter
import scala.annotation.tailrec

object TypeInference {

  /**
    * Performs type inference and reassembly on the given definition `defn`.
    */
  def visitDefn(defn: KindedAst.Def, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, TypeError] = defn match {
    case KindedAst.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      typeCheckDecl(spec0, exp0, assumedTconstrs, root, classEnv, eqEnv, sym.loc)

    //      recoverOne {
    //        case err: TypeError =>
    //          //
    //          // We recover from a type error by replacing the expression body with [[Expression.Error]].
    //          //
    //          // We use the declared type, purity, and effect as stand-ins.
    //          //
    //          val tpe = spec0.tpe
    //          val eff = spec0.eff
    //          val exp = TypedAst.Expr.Error(err, tpe, eff)
    //          val spec = visitSpec(spec0, root, Substitution.empty)
    //          TypedAst.Def(sym, spec, exp)
    //      }
    // TODO ASSOC-TYPES how to recover without node?
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  def visitSig(sig: KindedAst.Sig, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, TypeError] = sig match {
    case KindedAst.Sig(sym, spec0, Some(exp0)) =>
      typeCheckDecl(spec0, exp0, assumedTconstrs, root, classEnv, eqEnv, sym.loc)
    case KindedAst.Sig(sym, spec0, None) =>
      Validation.success(Substitution.empty) // TODO ASSOC-TYPES hack, should return Option or something
  }

  /**
    * Infers the type of the given definition `defn0`.
    */
  private def typeCheckDecl(spec0: KindedAst.Spec, exp0: KindedAst.Expr, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], loc: SourceLocation)(implicit flix: Flix): Validation[Substitution, TypeError] = spec0 match {
    case KindedAst.Spec(_, _, _, _, fparams0, sc, tpe, eff, _, _, _) =>

      ///
      /// Infer the type of the expression `exp0`.
      ///
      val result = for {
        (inferredConstrs, inferredTyp, inferredEff) <- inferExpectedExp(exp0, tpe, eff, root)
      } yield (inferredConstrs, Type.mkUncurriedArrowWithEffect(fparams0.map(_.tpe), inferredEff, inferredTyp, loc))


      // Add the assumed constraints to the declared scheme
      val declaredScheme = sc.copy(tconstrs = sc.tconstrs ++ assumedTconstrs)

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
          val initialRenv = getRigidityFromSpec(spec0)

          run(initialSubst, Nil, initialRenv) match { // TODO ASSOC-TYPES initial econstrs?
            case Ok((subst0, partialEconstrs, renv0, (partialTconstrs, partialType))) => // TODO ASSOC-TYPES check econstrs

              ///
              /// The partial type returned by the inference monad does not have the substitution applied.
              ///
              val inferredTconstrs = partialTconstrs.map(subst0.apply)
              val inferredEconstrs = partialEconstrs.map(subst0.apply)
              val inferredType = subst0(partialType)

              ///
              /// Check that the inferred type is at least as general as the declared type.
              ///
              /// NB: Because the inferredType is always a function type, the purect is always implicitly accounted for.
              ///
              val inferredSc = Scheme.generalize(inferredTconstrs, inferredEconstrs, inferredType, renv0)

              // get a substitution from the scheme comparison
              val eqSubst = Scheme.checkLessThanEqual(inferredSc, declaredScheme, classEnv, eqEnv) match {
                // Case 1: no errors, continue
                case Validation.Success(s) => s
                case failure =>
                  val instanceErrs = failure.errors.collect {
                    case UnificationError.NoMatchingInstance(tconstr) =>
                      tconstr.arg.typeConstructor match {
                        case Some(tc: TypeConstructor.Arrow) =>
                          TypeError.MissingInstanceArrow(tconstr.head.sym, tconstr.arg, renv0, tconstr.loc)
                        case _ =>
                          if (tconstr.head.sym.name == "Eq")
                            TypeError.MissingInstanceEq(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "Order")
                            TypeError.MissingInstanceOrder(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "ToString")
                            TypeError.MissingInstanceToString(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "Sendable")
                            TypeError.MissingInstanceSendable(tconstr.arg, renv0, tconstr.loc)
                          else
                            TypeError.MissingInstance(tconstr.head.sym, tconstr.arg, renv0, tconstr.loc)
                      }
                    case UnificationError.UnsupportedEquality(tpe1, tpe2) =>
                      TypeError.UnsupportedEquality(Ast.BroadEqualityConstraint(tpe1, tpe2), loc)
                    case UnificationError.IrreducibleAssocType(sym, t) =>
                      TypeError.IrreducibleAssocType(sym, t, loc)
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
                      return Validation.toHardFailure(TypeError.ImpureDeclaredAsPure(loc))
                    } else if (declaredEff == Type.Pure && inferredEff != Type.Pure) {
                      // Case 2: Declared as pure, but effectful.
                      return Validation.toHardFailure(TypeError.EffectfulDeclaredAsPure(inferredEff, loc))
                    } else {
                      // Case 3: Check if it is the effect that cannot be generalized.
                      val inferredEffScheme = Scheme(inferredSc.quantifiers, Nil, Nil, inferredEff)
                      val declaredEffScheme = Scheme(declaredScheme.quantifiers, Nil, Nil, declaredEff)
                      Scheme.checkLessThanEqual(inferredEffScheme, declaredEffScheme, classEnv, eqEnv) match {
                        case Validation.Success(_) =>
                        // Case 3.1: The effect is not the problem. Regular generalization error.
                        // Fall through to below.

                        case _failure =>
                          // Case 3.2: The effect cannot be generalized.
                          return Validation.toHardFailure(TypeError.EffectGeneralizationError(declaredEff, inferredEff, loc))
                      }

                      return Validation.toHardFailure(TypeError.GeneralizationError(declaredScheme, minimizeScheme(inferredSc), loc))
                    }
                  } else {
                    // Case 3: instance error
                    return Validation.HardFailure(Chain.from(instanceErrs))
                  }
              }

              // create a new substitution combining the econstr substitution and the base type substitution
              Validation.success((eqSubst @@ subst0))

            case Err(e) => Validation.HardFailure(Chain(e))
          }
      }
  }

  /**
    * Infers the type of the given expression `exp0`.
    */
  def inferExp(exp0: KindedAst.Expr, root: KindedAst.Root, level0: Level)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: KindedAst.Expr)(implicit level: Level): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = e0 match {

      case KindedAst.Expr.Var(sym, loc) =>
        liftM(List.empty, sym.tvar, Type.Pure)

      case KindedAst.Expr.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs0, defType) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
        for {
          resultTyp <- unifyTypeM(tvar, defType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure)

      case KindedAst.Expr.Sig(sym, tvar, loc) =>
        // find the declared signature corresponding to this symbol
        val sig = root.classes(sym.clazz).sigs(sym)
        val (tconstrs0, sigType) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
        for {
          resultTyp <- unifyTypeM(tvar, sigType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure)

      case KindedAst.Expr.Hole(_, tvar, _) =>
        liftM(List.empty, tvar, Type.Pure)

      case KindedAst.Expr.HoleWithExp(exp, tvar, pvar, loc) =>
        for {
          (tconstrs, tpe, eff) <- visitExp(exp)
          // result type is whatever is needed for the hole
          resultTpe = tvar
          // effect type is AT LEAST the inner expression's effect
          atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Eff, loc.asSynthetic), loc.asSynthetic)
          resultEff <- unifyTypeM(atLeastEff, pvar, loc)
        } yield (tconstrs, resultTpe, resultEff)

      case e: KindedAst.Expr.OpenAs => RestrictableChooseInference.inferOpenAs(e, root)

      case KindedAst.Expr.Use(_, alias, exp, _) => visitExp(exp)

      case KindedAst.Expr.Cst(Ast.Constant.Unit, loc) =>
        liftM(List.empty, Type.mkUnit(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Null, loc) =>
        liftM(List.empty, Type.mkNull(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Bool(_), loc) =>
        liftM(List.empty, Type.mkBool(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Char(_), loc) =>
        liftM(List.empty, Type.mkChar(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Float32(_), loc) =>
        liftM(List.empty, Type.mkFloat32(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Float64(_), loc) =>
        liftM(List.empty, Type.mkFloat64(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.BigDecimal(_), loc) =>
        liftM(List.empty, Type.mkBigDecimal(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int8(_), loc) =>
        liftM(List.empty, Type.mkInt8(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int16(_), loc) =>
        liftM(List.empty, Type.mkInt16(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int32(_), loc) =>
        liftM(List.empty, Type.mkInt32(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int64(_), loc) =>
        liftM(List.empty, Type.mkInt64(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.BigInt(_), loc) =>
        liftM(List.empty, Type.mkBigInt(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Str(_), loc) =>
        liftM(List.empty, Type.mkString(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Regex(_), loc) =>
        liftM(List.empty, Type.mkRegex(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Lambda(fparam, exp, loc) =>
        val argType = fparam.tpe
        val argTypeVar = fparam.sym.tvar
        for {
          (constrs, bodyType, bodyEff) <- visitExp(exp)
          _ <- unifyTypeM(argType, argTypeVar, loc)
          resultTyp = Type.mkArrowWithEffect(argType, bodyEff, bodyType, loc)
        } yield (constrs, resultTyp, Type.Pure)

      case KindedAst.Expr.Apply(exp, exps, tvar, pvar, loc) =>
        //
        // Determine if there is a direct call to a Def or Sig.
        //
        val knownTarget = exp match {
          case KindedAst.Expr.Def(sym, tvar2, loc2) =>
            // Case 1: Lookup the sym and instantiate its scheme.
            val defn = root.defs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(defn.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case KindedAst.Expr.Sig(sym, tvar2, loc2) =>
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
            val declaredEff = declaredType.arrowEffectType
            val declaredArgumentTypes = declaredType.arrowArgTypes
            val declaredResultType = declaredType.arrowResultType

            for {
              (constrs2, tpes, effs) <- traverseM(exps)(visitExp).map(_.unzip3)
              _ <- expectTypeArguments(sym, declaredArgumentTypes, tpes, exps.map(_.loc), loc)
              _ <- unifyTypeM(tvar2, declaredType, loc)
              resultTyp <- unifyTypeM(tvar, declaredResultType, loc)
              resultEff <- unifyEffM(pvar, Type.mkUnion(declaredEff :: effs, loc), loc)
            } yield (constrs1 ++ constrs2.flatten, resultTyp, resultEff)

          case None =>
            //
            // Default Case: Apply.
            //
            val lambdaBodyType = Type.freshVar(Kind.Star, loc)
            val lambdaBodyEff = Type.freshVar(Kind.Eff, loc)
            for {
              (constrs1, tpe, eff) <- visitExp(exp)
              (constrs2, tpes, effs) <- traverseM(exps)(visitExp).map(_.unzip3)
              _ <- expectTypeM(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyEff, lambdaBodyType, loc), loc)
              resultTyp <- unifyTypeM(tvar, lambdaBodyType, loc)
              resultEff <- unifyEffM(pvar, Type.mkUnion(lambdaBodyEff :: eff :: effs, loc), loc)
              _ <- unbindVar(lambdaBodyType) // NB: Safe to unbind since the variable is not used elsewhere.
              _ <- unbindVar(lambdaBodyEff) // NB: Safe to unbind since the variable is not used elsewhere.
            } yield (constrs1 ++ constrs2.flatten, resultTyp, resultEff)
        }

      case KindedAst.Expr.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOp.BoolOp.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Bool, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Float32Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float32, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Float64Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float64, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int8Op.Neg | SemanticOp.Int8Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int8, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int16Op.Neg | SemanticOp.Int16Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int16, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int32Op.Neg | SemanticOp.Int32Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int32, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int64Op.Neg | SemanticOp.Int64Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int64, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", loc)
      }

      case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOp.BoolOp.And | SemanticOp.BoolOp.Or =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Bool, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Float32Op.Add | SemanticOp.Float32Op.Sub | SemanticOp.Float32Op.Mul | SemanticOp.Float32Op.Div
             | SemanticOp.Float32Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float32, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Float64Op.Add | SemanticOp.Float64Op.Sub | SemanticOp.Float64Op.Mul | SemanticOp.Float64Op.Div
             | SemanticOp.Float64Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float64, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int8Op.Add | SemanticOp.Int8Op.Sub | SemanticOp.Int8Op.Mul | SemanticOp.Int8Op.Div
             | SemanticOp.Int8Op.Rem | SemanticOp.Int8Op.Exp
             | SemanticOp.Int8Op.And | SemanticOp.Int8Op.Or | SemanticOp.Int8Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int8, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int8, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int8, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int16Op.Add | SemanticOp.Int16Op.Sub | SemanticOp.Int16Op.Mul | SemanticOp.Int16Op.Div
             | SemanticOp.Int16Op.Rem | SemanticOp.Int16Op.Exp
             | SemanticOp.Int16Op.And | SemanticOp.Int16Op.Or | SemanticOp.Int16Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int16, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int16, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int16, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int32Op.Add | SemanticOp.Int32Op.Sub | SemanticOp.Int32Op.Mul | SemanticOp.Int32Op.Div
             | SemanticOp.Int32Op.Rem | SemanticOp.Int32Op.Exp
             | SemanticOp.Int32Op.And | SemanticOp.Int32Op.Or | SemanticOp.Int32Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int32, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int64Op.Add | SemanticOp.Int64Op.Sub | SemanticOp.Int64Op.Mul | SemanticOp.Int64Op.Div
             | SemanticOp.Int64Op.Rem | SemanticOp.Int64Op.Exp
             | SemanticOp.Int64Op.And | SemanticOp.Int64Op.Or | SemanticOp.Int64Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int64, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int8Op.Shl | SemanticOp.Int8Op.Shr
             | SemanticOp.Int16Op.Shl | SemanticOp.Int16Op.Shr
             | SemanticOp.Int32Op.Shl | SemanticOp.Int32Op.Shr
             | SemanticOp.Int64Op.Shl | SemanticOp.Int64Op.Shr =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- unifyTypeM(tvar, tpe1, loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, lhs, resultEff)

        case SemanticOp.BoolOp.Eq | SemanticOp.BoolOp.Neq
             | SemanticOp.CharOp.Eq | SemanticOp.CharOp.Neq
             | SemanticOp.Float32Op.Eq | SemanticOp.Float32Op.Neq
             | SemanticOp.Float64Op.Eq | SemanticOp.Float64Op.Neq
             | SemanticOp.Int8Op.Eq | SemanticOp.Int8Op.Neq
             | SemanticOp.Int16Op.Eq | SemanticOp.Int16Op.Neq
             | SemanticOp.Int32Op.Eq | SemanticOp.Int32Op.Neq
             | SemanticOp.Int64Op.Eq | SemanticOp.Int64Op.Neq =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.CharOp.Lt | SemanticOp.CharOp.Le | SemanticOp.CharOp.Gt | SemanticOp.CharOp.Ge
             | SemanticOp.Float32Op.Lt | SemanticOp.Float32Op.Le | SemanticOp.Float32Op.Gt | SemanticOp.Float32Op.Ge
             | SemanticOp.Float64Op.Lt | SemanticOp.Float64Op.Le | SemanticOp.Float64Op.Gt | SemanticOp.Float64Op.Ge
             | SemanticOp.Int8Op.Lt | SemanticOp.Int8Op.Le | SemanticOp.Int8Op.Gt | SemanticOp.Int8Op.Ge
             | SemanticOp.Int16Op.Lt | SemanticOp.Int16Op.Le | SemanticOp.Int16Op.Gt | SemanticOp.Int16Op.Ge
             | SemanticOp.Int32Op.Lt | SemanticOp.Int32Op.Le | SemanticOp.Int32Op.Gt | SemanticOp.Int32Op.Ge
             | SemanticOp.Int64Op.Lt | SemanticOp.Int64Op.Le | SemanticOp.Int64Op.Gt | SemanticOp.Int64Op.Ge =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.StringOp.Concat =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Str, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Str, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Str, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop'.", loc)
      }

      case KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          condType <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
          resultTyp <- unifyTypeM(tpe2, tpe3, loc)
          resultEff = Type.mkUnion(eff1, eff2, eff3, loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expr.Stm(exp1, exp2, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Discard(exp, loc) =>
        for {
          (constrs, _, eff) <- visitExp(exp)
          resultTyp = Type.Unit
        } yield (constrs, resultTyp, eff)

      case KindedAst.Expr.Let(sym, mod, exp1, exp2, loc) =>
        // Note: The call to unify on sym.tvar occurs immediately after we have inferred the type of exp1.
        // This ensures that uses of sym inside exp2 are type checked according to this type.
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          boundVar <- unifyTypeM(sym.tvar, tpe1, loc)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.LetRec(sym, _, _, exp1, exp2, loc) =>
        // Note 1: We do not have to ensure that `exp1` is a lambda because it is syntactically ensured.
        // Note 2: We purify the letrec bound function to simplify its inferred effect.
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)(level.incr)
          boundVar <- unifyTypeM(sym.tvar, tpe1, exp1.loc)
          _ <- purifyLetRec(boundVar)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Region(tpe, _) =>
        liftM(Nil, tpe, Type.Pure)

      case KindedAst.Expr.Scope(sym, regionVar, exp, pvar, loc) =>
        for {
          _ <- rigidifyM(regionVar)
          _ <- unifyTypeM(sym.tvar, Type.mkRegion(regionVar, loc), loc)
          // Increase the level environment as we enter the region
          (constrs, tpe, eff) <- visitExp(exp)(level.incr)
          // Purify the region's effect and unbind free local effect variables from the substitution.
          // This ensures that the substitution cannot re-introduce the region
          // in place of the free local effect variables.
          purifiedEff <- purifyEffAndUnbindM(regionVar, eff)
          resultEff <- unifyTypeM(pvar, purifiedEff, loc)
          _ <- noEscapeM(regionVar, tpe)
          resultTyp = tpe
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.ScopeExit(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val p = Type.freshVar(Kind.Eff, loc)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = Type.mkUncurriedArrowWithEffect(Type.Unit :: Nil, p, Type.Unit, loc.asSynthetic), actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.mkUnion(Type.Impure, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Match(exp, rules, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.flatMap(_.guard)
        val bodies = rules.map(_.exp)
        val guardLocs = guards.map(_.loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          patternType <- unifyTypeM(tpe :: patternTypes, loc)
          (guardConstrs, guardTypes, guardEffs) <- traverseM(guards)(visitExp).map(_.unzip3)
          guardType <- traverseM(guardTypes.zip(guardLocs)) { case (gTpe, gLoc) => expectTypeM(expected = Type.Bool, actual = gTpe, loc = gLoc) }
          (bodyConstrs, bodyTypes, bodyEffs) <- traverseM(bodies)(visitExp).map(_.unzip3)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultEff = Type.mkUnion(eff :: guardEffs ::: bodyEffs, loc)
        } yield (constrs ++ guardConstrs.flatten ++ bodyConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.TypeMatch(exp, rules, loc) =>
        val bodies = rules.map(_.exp)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          // rigidify all the type vars in the rules
          _ <- traverseM(rules.flatMap(rule => rule.tpe.typeVars.toList))(rigidifyM)
          // unify each rule's variable with its type
          _ <- traverseM(rules)(rule => unifyTypeM(rule.sym.tvar, rule.tpe, rule.sym.loc))
          (bodyConstrs, bodyTypes, bodyEffs) <- traverseM(bodies)(visitExp).map(_.unzip3)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultEff = Type.mkUnion(eff :: bodyEffs, loc)
        } yield (constrs ++ bodyConstrs.flatten, resultTyp, resultEff)

      case exp@KindedAst.Expr.RestrictableChoose(_, _, _, _, _) => RestrictableChooseInference.infer(exp, root)

      case KindedAst.Expr.Tag(symUse, exp, tvar, loc) =>
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
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
          resultTyp = tvar
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case exp@KindedAst.Expr.RestrictableTag(_, _, _, _, _) =>
        RestrictableChooseInference.inferRestrictableTag(exp, root)

      case KindedAst.Expr.Tuple(elms, loc) =>
        for {
          (elementConstrs, elementTypes, elementEffs) <- traverseM(elms)(visitExp).map(_.unzip3)
          resultEff = Type.mkUnion(elementEffs, loc)
        } yield (elementConstrs.flatten, Type.mkTuple(elementTypes, loc), resultEff)

      case KindedAst.Expr.RecordEmpty(loc) =>
        liftM(List.empty, Type.mkRecord(Type.RecordRowEmpty, loc), Type.Pure)

      case KindedAst.Expr.RecordSelect(exp, label, tvar, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        //       r.label : tpe
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val expectedRowType = Type.mkRecordRowExtend(label, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, expectedRecordType, loc)
          resultEff = eff
        } yield (constrs, tvar, resultEff)

      case KindedAst.Expr.RecordExtend(label, exp1, exp2, tvar, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { label = exp1 | exp2 } : { label  :: tpe | r }
        //
        val restRow = Type.freshVar(Kind.RecordRow, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe2, Type.mkRecord(restRow, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(Type.mkRecordRowExtend(label, tpe1, restRow, loc), loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.RecordRestrict(label, exp, tvar, loc) =>
        //
        //  exp : { label  :: t | r }
        // -------------------------
        // { -label | exp } : {| r }
        //
        val freshLabelType = Type.freshVar(Kind.Star, loc)
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, Type.mkRecord(Type.mkRecordRowExtend(label, freshLabelType, freshRowVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(freshRowVar, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.ArrayLit(exps, exp, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, elmTypes, eff1) <- traverseM(exps)(visitExp).map(_.unzip3)
          (constrs2, tpe2, eff2) <- visitExp(exp)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp.loc)
          elmTyp <- unifyTypeAllowEmptyM(elmTypes, Kind.Star, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(elmTyp, regionVar, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(Type.mkUnion(eff1, loc), eff2, regionVar, loc), loc)
        } yield (constrs1.flatten ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = regionType, actual = tpe1, loc)
          _lenType <- expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(tpe2, regionVar, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, eff3, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expr.ArrayLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(Type.mkArray(elmVar, regionVar, loc), tpe, exp.loc)
          resultTyp = Type.Int32
          resultEff = eff
          _ <- unbindVar(elmVar)
          _ <- unbindVar(regionVar)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          arrayType <- expectTypeM(expected = Type.mkArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
          indexType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(regionVar, eff1, eff2, loc), loc)
        } yield (constrs1 ++ constrs2, tvar, resultEff)

      case KindedAst.Expr.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val arrayType = Type.mkArray(elmVar, regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe3, exp3.loc)
          resultTyp = Type.Unit
          resultEff <- unifyTypeM(pvar, Type.mkUnion(List(regionVar, eff1, eff2, eff3), loc), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expr.VectorLit(exps, tvar, pvar, loc) =>
        for {
          (constrs, elmTypes, eff) <- traverseM(exps)(visitExp).map(_.unzip3)
          elmTyp <- unifyTypeAllowEmptyM(elmTypes, Kind.Star, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkVector(elmTyp, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff, loc), loc)
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.VectorLoad(exp1, exp2, tvar, pvar, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          arrayType <- expectTypeM(expected = Type.mkVector(tvar, loc), actual = tpe1, exp1.loc)
          indexType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, loc), loc)
        } yield (constrs1 ++ constrs2, tvar, resultEff)

      case KindedAst.Expr.VectorLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(Type.mkVector(elmVar, loc), tpe, exp.loc)
          resultTyp = Type.Int32
          resultEff = eff
          _ <- unbindVar(elmVar)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.Ref(exp1, exp2, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(tpe2, regionType, exp2.loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRef(tpe1, regionVar, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Deref(exp, tvar, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = refType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff, regionVar, loc), loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.Assign(exp1, exp2, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = refType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Ascribe(exp, expectedTyp, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (constrs, actualTyp, actualPur) <- visitExp(exp)
          resultTyp <- expectTypeM(expected = expectedTyp.getOrElse(Type.freshVar(Kind.Star, loc)), actual = actualTyp, bind = tvar, loc)
          resultEff <- expectTypeM(expected = expectedEff.getOrElse(Type.freshVar(Kind.Eff, loc)), actual = actualPur, loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.InstanceOf(exp, className, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp = Type.Bool
          resultEff <- expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.CheckedCast(cast, exp, tvar, pvar, loc) =>
        cast match {
          case CheckedCastType.TypeCast =>
            for {
              // Ignore the inferred type of exp.
              (constrs, _, eff) <- visitExp(exp)
            } yield (constrs, tvar, eff)

          case CheckedCastType.EffectCast =>
            for {
              // We simply union the purity and effect with a fresh variable.
              (constrs, tpe, eff) <- visitExp(exp)
              resultEff = Type.mkUnion(eff, pvar, loc)
            } yield (constrs, tpe, resultEff)
        }

      case KindedAst.Expr.UncheckedCast(exp, declaredTyp, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        for {
          (constrs, actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, declaredTyp.getOrElse(actualTyp), loc)
          resultEff = declaredEff.getOrElse(actualEff)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.UncheckedMaskingCast(exp, _) =>
        // A mask expression is unsound; the type system assumes the expression is pure.
        for {
          (constrs, tpe, eff) <- visitExp(exp)
        } yield (constrs, tpe, Type.Pure)

      case KindedAst.Expr.Without(exp, effUse, loc) =>
        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        //        val expected = Type.mkDifference(Type.freshVar(Kind.Bool, loc), effType, loc)
        // TODO EFF-MIGRATION use expected
        for {
          (tconstrs, tpe, eff) <- visitExp(exp)
        } yield (tconstrs, tpe, eff)

      case KindedAst.Expr.TryCatch(exp, rules, loc) =>
        val rulesType = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          (ruleConstrs, ruleTypes, ruleEffs) <- seqM(rulesType).map(_.unzip3)
          ruleType <- unifyTypeM(ruleTypes, loc)
          resultTyp <- unifyTypeM(tpe, ruleType, loc)
          resultEff = Type.mkUnion(eff :: ruleEffs, loc)
        } yield (constrs ++ ruleConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.TryWith(exp, effUse, rules, tvar, loc) =>
        val effect = root.effects(effUse.sym)
        val ops = effect.ops.map(op => op.sym -> op).toMap

        def unifyFormalParams(op: Symbol.OpSym, expected: List[KindedAst.FormalParam], actual: List[KindedAst.FormalParam]): InferMonad[Unit] = {
          if (expected.length != actual.length) {
            InferMonad.errPoint(TypeError.MismatchedOpArity(op, expected = expected.length, actual = actual.length, loc))
          } else {
            traverseM(expected zip actual) {
              case (ex, ac) =>
                for {
                  _ <- expectTypeM(expected = ex.tpe, actual = ac.tpe, ac.loc)
                } yield ()
            }.map(_ => ())
          }
        }

        def visitHandlerRule(rule: KindedAst.HandlerRule, tryBlockTpe: Type, tryBlockEff: Type): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = rule match {
          case KindedAst.HandlerRule(op, actualFparams0, body, opTvar) =>
            // Don't need to generalize since ops are monomorphic
            // Don't need to handle unknown op because resolver would have caught this
            val (actualFparams, List(resumptionFparam)) = actualFparams0.splitAt(actualFparams0.length - 1)
            ops(op.sym) match {
              case KindedAst.Op(_, KindedAst.Spec(_, _, _, _, expectedFparams, _, opTpe, _, _, _, _)) =>
                val resumptionArgType = opTpe
                val resumptionResType = tryBlockTpe
                val resumptionEff = tryBlockEff
                val expectedResumptionType = Type.mkArrowWithEffect(resumptionArgType, resumptionEff, resumptionResType, loc.asSynthetic)
                for {
                  _ <- unifyFormalParams(op.sym, expected = expectedFparams, actual = actualFparams)
                  _ <- expectTypeM(expected = expectedResumptionType, actual = resumptionFparam.tpe, resumptionFparam.loc)
                  (actualTconstrs, actualTpe, actualEff) <- visitExp(body)

                  // unify the operation return type with its tvar
                  _ <- unifyTypeM(opTpe, opTvar, body.loc)

                  // unify the handler result type with the whole block's tvar
                  resultTpe <- expectTypeM(expected = tvar, actual = actualTpe, body.loc)
                } yield (actualTconstrs, resultTpe, actualEff)
            }
        }

        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        for {
          (tconstrs, tpe, bodyEff) <- visitExp(exp)
          (tconstrss, _, effs) <- traverseM(rules)(visitHandlerRule(_, tpe, bodyEff)).map(_.unzip3)
          resultTconstrs = (tconstrs :: tconstrss).flatten
          resultTpe <- unifyTypeM(tvar, tpe, loc)
          compositeEff = Type.mkUnion(bodyEff :: effs, bodyEff.loc.asSynthetic)
          resultEff = Type.mkDifference(compositeEff, effType, loc)
        } yield (resultTconstrs, resultTpe, resultEff)

      case KindedAst.Expr.Do(op, args, tvar, loc) =>
        val effect = root.effects(op.sym.eff)
        val operation = effect.ops.find(_.sym == op.sym)
          .getOrElse(throw InternalCompilerException(s"Unexpected missing operation $op in effect ${op.sym.eff}", loc))
        val effTpe = Type.Cst(TypeConstructor.Effect(op.sym.eff), loc)

        def visitArg(arg: KindedAst.Expr, fparam: KindedAst.FormalParam): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
          for {
            (tconstrs, tpe, eff) <- visitExp(arg)
            _ <- expectTypeM(expected = fparam.tpe, tpe, arg.loc)
          } yield (tconstrs, tpe, eff)
        }

        if (operation.spec.fparams.length != args.length) {
          InferMonad.errPoint(TypeError.MismatchedOpArity(op.sym, expected = operation.spec.fparams.length, actual = args.length, loc))
        } else {
          val argM = (args zip operation.spec.fparams) map {
            case (arg, fparam) => visitArg(arg, fparam)
          }
          for {
            (tconstrss, _, effs) <- seqM(argM).map(_.unzip3)
            resultTconstrs = tconstrss.flatten
            resultTpe <- unifyTypeM(operation.spec.tpe, tvar, loc)
            resultEff = Type.mkUnion(effTpe :: operation.spec.eff :: effs, loc)
          } yield (resultTconstrs, resultTpe, resultEff)
        }

      case KindedAst.Expr.InvokeConstructor(constructor, args, loc) =>
        val classType = getFlixType(constructor.getDeclaringClass)
        for {
          (constrs, _, _) <- traverseM(args)(visitExp).map(_.unzip3)
          resultTyp = classType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.InvokeMethod(method, clazz, exp, args, loc) =>
        val classType = getFlixType(clazz)
        val returnType = getFlixType(method.getReturnType)
        for {
          (baseConstrs, baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          (constrs, tpes, effs) <- traverseM(args)(visitExp).map(_.unzip3)
          resultTyp = getFlixType(method.getReturnType)
          resultEff = Type.Impure
        } yield (baseConstrs ++ constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.InvokeStaticMethod(method, args, loc) =>
        val returnType = getFlixType(method.getReturnType)
        for {
          (constrs, tpes, effs) <- traverseM(args)(visitExp).map(_.unzip3)
          resultTyp = returnType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.GetField(field, clazz, exp, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        for {
          (constrs, tpe, _) <- visitExp(exp)
          objectTyp <- expectTypeM(expected = classType, actual = tpe, exp.loc)
          resultTyp = fieldType
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.PutField(field, clazz, exp1, exp2, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = classType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = fieldType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.GetStaticField(field, loc) =>
        val fieldType = getFlixType(field.getType)
        val resultTyp = fieldType
        val resultEff = Type.Impure
        liftM(List.empty, resultTyp, resultEff)

      case KindedAst.Expr.PutStaticField(field, exp, loc) =>
        for {
          (valueConstrs, valueTyp, _) <- visitExp(exp)
          fieldTyp <- expectTypeM(expected = getFlixType(field.getType), actual = valueTyp, exp.loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (valueConstrs, resultTyp, resultEff)

      case KindedAst.Expr.NewObject(_, clazz, methods, loc) =>

        /**
          * Performs type inference on the given JVM `method`.
          */
        def inferJvmMethod(method: KindedAst.JvmMethod): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = method match {
          case KindedAst.JvmMethod(ident, fparams, exp, returnTpe, eff, loc) =>

            /**
              * Constrains the given formal parameter to its declared type.
              */
            def inferParam(fparam: KindedAst.FormalParam): InferMonad[Unit] = fparam match {
              case KindedAst.FormalParam(sym, _, tpe, _, loc) =>
                unifyTypeM(sym.tvar, tpe, loc).map(_ => ())
            }

            for {
              _ <- traverseM(fparams)(inferParam)
              (constrs, bodyTpe, bodyEff) <- visitExp(exp)
              _ <- expectTypeM(expected = returnTpe, actual = bodyTpe, exp.loc)
            } yield (constrs, returnTpe, bodyEff)
        }

        for {
          (constrs, _, _) <- traverseM(methods)(inferJvmMethod).map(_.unzip3)
          resultTyp = getFlixType(clazz)
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)


      case KindedAst.Expr.NewChannel(exp1, exp2, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = regionType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultTyp <- liftM(tvar)
          resultEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.GetChannel(exp, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkReceiver(elmVar, regionVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = channelType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultEff = Type.mkUnion(eff, regionVar, loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.PutChannel(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkSender(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = channelType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.mkUnit(loc)
          resultEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.SelectChannel(rules, default, tvar, loc) =>

        val regionVar = Type.freshVar(Kind.Eff, loc)

        /**
          * Performs type inference on the given select rule `sr0`.
          */
        def inferSelectRule(sr0: KindedAst.SelectChannelRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          sr0 match {
            case KindedAst.SelectChannelRule(sym, chan, body) => for {
              (chanConstrs, chanType, eff1) <- visitExp(chan)
              (bodyConstrs, bodyType, eff2) <- visitExp(body)
              _ <- unifyTypeM(chanType, Type.mkReceiver(sym.tvar, regionVar, sym.loc), sym.loc)
              resultCon = chanConstrs ++ bodyConstrs
              resultTyp = bodyType
              resultEff = Type.mkUnion(eff1, eff2, regionVar, loc)
            } yield (resultCon, resultTyp, resultEff)
          }

        /**
          * Performs type inference on the given optional default expression `exp0`.
          */
        def inferDefaultRule(exp0: Option[KindedAst.Expr]): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          exp0 match {
            case None => liftM(Nil, Type.freshVar(Kind.Star, loc), Type.Pure)
            case Some(exp) => visitExp(exp)
          }

        for {
          (ruleConstrs, ruleTypes, ruleEffs) <- traverseM(rules)(inferSelectRule).map(_.unzip3)
          (defaultConstrs, defaultType, eff2) <- inferDefaultRule(default)
          resultCon = ruleConstrs.flatten ++ defaultConstrs
          resultTyp <- unifyTypeM(tvar :: defaultType :: ruleTypes, loc)
          resultEff = Type.mkUnion(regionVar :: eff2 :: ruleEffs, loc)
        } yield (resultCon, resultTyp, resultEff)

      case KindedAst.Expr.Spawn(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.mkUnion(Type.Impure, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.ParYield(frags, exp, loc) =>
        val patterns = frags.map(_.pat)
        val parExps = frags.map(_.exp)
        val patLocs = frags.map(_.loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          (fragConstrs, fragTypes, fragEffs) <- seqM(parExps map visitExp).map(_.unzip3)
          _ <- seqM(patternTypes.zip(fragTypes).zip(patLocs).map { case ((patTpe, expTpe), l) => unifyTypeM(List(patTpe, expTpe), l) })
          _ <- seqM(fragEffs.zip(patLocs) map { case (p, l) => expectTypeM(expected = Type.Pure, actual = p, l) })
        } yield (constrs ++ fragConstrs.flatten, tpe, eff)

      case KindedAst.Expr.Lazy(exp, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp = Type.mkLazy(tpe, loc)
          resultEff <- expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.Force(exp, tvar, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          lazyTyp <- expectTypeM(expected = Type.mkLazy(tvar, loc), actual = tpe, exp.loc)
          resultTyp = tvar
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointConstraintSet(cs, tvar, loc) =>
        for {
          (constrs, constraintTypes) <- traverseM(cs)(visitConstraint).map(_.unzip)
          schemaRow <- unifyTypeAllowEmptyM(constraintTypes, Kind.SchemaRow, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(schemaRow, loc), loc)
        } yield (constrs.flatten, resultTyp, Type.Pure)

      case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>

        def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
          case KindedAst.PredicateParam(pred, tpe, loc) => Type.mkSchemaRowExtend(pred, tpe, restRow, tpe.loc)
        }

        def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

        val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))
        val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- unifyTypeM(tpe, Type.mkSchema(expectedRowType, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(resultRowType, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tpe1, tpe2, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.FixpointSolve(exp, loc) =>
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

      case KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : #{ P : a  | b }
        //  -------------------------------------------
        //  project P exp2 : #{ P : a | c }
        //
        val freshPredicateTypeVar = Type.freshVar(Kind.Predicate, loc)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)
        val freshResultSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshRestSchemaTypeVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshResultSchemaTypeVar, loc), loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointInject(exp, pred, tvar, loc) =>
        //
        //  exp : F[freshElmType] where F is Foldable
        //  -------------------------------------------
        //  project exp into A: #{A(freshElmType) | freshRestSchemaType}
        //
        val freshTypeConstructorVar = Type.freshVar(Kind.Star ->: Kind.Star, loc)
        val freshElmTypeVar = Type.freshVar(Kind.Star, loc)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

        // Require Order and Foldable instances.
        val orderSym = PredefinedClasses.lookupClassSym("Order", root)
        val foldableSym = PredefinedClasses.lookupClassSym("Foldable", root)
        val order = Ast.TypeConstraint(Ast.TypeConstraint.Head(orderSym, loc), freshElmTypeVar, loc)
        val foldable = Ast.TypeConstraint(Ast.TypeConstraint.Head(foldableSym, loc), freshTypeConstructorVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkApply(freshTypeConstructorVar, List(freshElmTypeVar), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(List(freshElmTypeVar), loc), freshRestSchemaTypeVar, loc), loc), loc)
          resultEff = eff
        } yield (order :: foldable :: constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc) =>
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
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe1, expectedSchemaType, loc)
          _ <- unifyTypeM(tpe2, Type.mkSchema(freshRestSchemaVar, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkVector(freshTupleVar, loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Error(m, tvar, pvar) =>
        InferMonad.point((Nil, tvar, pvar))

    }

    /**
      * Infers the type of the given constraint `con0` inside the inference monad.
      */
    def visitConstraint(con0: KindedAst.Constraint)(implicit level: Level): InferMonad[(List[Ast.TypeConstraint], Type)] = {
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

    visitExp(exp0)(level0)
  }

  /**
    * Infers the type and effect of the expression, and checks that they match the expected type and effect.
    */
  private def inferExpectedExp(exp: KindedAst.Expr, tpe0: Type, eff0: Type, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
    for {
      (tconstrs, tpe, eff) <- inferExp(exp, root, Level.Top)
      _ <- expectTypeM(expected = tpe0, actual = tpe, exp.loc)
      _ <- expectEffectM(expected = eff0, actual = eff, exp.loc)
    } yield (tconstrs, tpe, eff)
  }

  /**
    * Infers the type of the given pattern `pat0`.
    */
  private def inferPattern(pat0: KindedAst.Pattern, root: KindedAst.Root)(implicit level: Level, flix: Flix): InferMonad[Type] = {

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

      case KindedAst.Pattern.Cst(Ast.Constant.Regex(s), loc) => liftM(Type.Regex)

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


      case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc.asSynthetic)
        val freshRecord = Type.mkRecord(freshRowVar, loc.asSynthetic)

        def mkRecordType(patTypes: List[(Name.Label, Type, SourceLocation)]): Type = {
          val ps = patTypes.foldRight(freshRowVar: Type) {
            case ((lbl, t, l), acc) => Type.mkRecordRowExtend(
              lbl, t, acc, l)
          }
          Type.mkRecord(ps, loc)
        }

        for {
          recordTail <- visit(pat)
          _recordExtension <- unifyTypeM(freshRecord, recordTail, loc.asSynthetic)
          patTypes <- traverseM(pats)(visitRecordLabelPattern(_, root))
          resultType = mkRecordType(patTypes)
          _ <- unifyTypeM(resultType, tvar, loc)
        } yield resultType

      case KindedAst.Pattern.RecordEmpty(loc) => liftM(Type.mkRecord(Type.RecordRowEmpty, loc))

      case KindedAst.Pattern.Error(tvar, _) => liftM(tvar)

    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[KindedAst.Pattern], root: KindedAst.Root)(implicit level: Level, flix: Flix): InferMonad[List[Type]] = {
    traverseM(pats0)(inferPattern(_, root))
  }

  /**
    * Infers the type of the given [[KindedAst.Pattern.Record.RecordLabelPattern]] `pat`.
    */
  private def visitRecordLabelPattern(pat: KindedAst.Pattern.Record.RecordLabelPattern, root: KindedAst.Root)(implicit level: Level, flix: Flix): InferMonad[(Name.Label, Type, SourceLocation)] = pat match {
    case KindedAst.Pattern.Record.RecordLabelPattern(label, tvar, p, loc) =>
      // { Label = Pattern ... }
      for {
        patType <- inferPattern(p, root)
        _ <- unifyTypeM(patType, tvar, loc)
      } yield (label, patType, loc)
  }

  /**
    * Infers the type of the given head predicate.
    */
  private def inferHeadPredicate(head: KindedAst.Predicate.Head, root: KindedAst.Root)(implicit level: Level, flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = head match {
    case KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc) =>
      // Adds additional type constraints if the denotation is a lattice.
      val restRow = Type.freshVar(Kind.SchemaRow, loc)
      for {
        (termConstrs, termTypes, termEffs) <- traverseM(terms)(inferExp(_, root, level)).map(_.unzip3)
        pureTermEffs <- unifyEffM(Type.Pure, Type.mkUnion(termEffs, loc), loc)
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
      } yield (termConstrs.flatten ++ tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: KindedAst.Predicate.Body, root: KindedAst.Root)(implicit level: Level, flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = {

    body0 match {
      case KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, tvar, loc) =>
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        for {
          termTypes <- traverseM(terms)(inferPattern(_, root))
          predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
          tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
        } yield (tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))

      case KindedAst.Predicate.Body.Functional(outVars, exp, loc) =>
        val tupleType = Type.mkTuplish(outVars.map(_.tvar), loc)
        val expectedType = Type.mkVector(tupleType, loc)
        for {
          (constrs, tpe, eff) <- inferExp(exp, root, level)
          expTyp <- unifyTypeM(expectedType, tpe, loc)
          expEff <- unifyEffM(Type.Pure, eff, loc)
        } yield (constrs, mkAnySchemaRowType(loc))

      case KindedAst.Predicate.Body.Guard(exp, loc) =>
        for {
          (constrs, tpe, eff) <- inferExp(exp, root, level)
          expEff <- unifyEffM(Type.Pure, eff, loc)
          expTyp <- unifyTypeM(Type.Bool, tpe, loc)
        } yield (constrs, mkAnySchemaRowType(loc))
    }
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
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Order", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Constructs the type class constraints for the given lattice term type `tpe`.
    */
  private def mkTypeClassConstraintsForLatticeTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Order", root),
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
        macc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(declaredType)(Level.Top, flix))
    }
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit level: Level, flix: Flix): Type = {
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
  private def getRigidityFromSpec(spec: KindedAst.Spec)(implicit flix: Flix): RigidityEnv = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc) =>
      // TODO ideally this should just use tparams, but we have to use other fields here
      // TODO because tparams do not include the wildcards
      val tvars = fparams.flatMap(_.tpe.typeVars) ++ tpe.typeVars ++ eff.typeVars
      tvars.foldLeft(RigidityEnv.empty) {
        case (renv, Type.Var(sym, _)) => renv.markRigid(sym)
      }
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaRowType(loc: SourceLocation)(implicit level: Level, flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc)

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
      val mean = StatUtils.average(sizes)
      val median = StatUtils.median(sizes)
      val total = sizes.sum
      t.mkRow(List(sym.toString, size, f"$mean%2.1f", median, total))
    }
    t.write(new PrintWriter(System.out))
  }
}
