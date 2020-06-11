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

import java.io.PrintWriter

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Stratification}
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.InferMonad.seqM
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification.{InferMonad, Substitution}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util._


object Typer extends Phase[ResolvedAst.Root, TypedAst.Root] {

  /**
    * Type checks the given AST root.
    */
  def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Typer") {
    val defsVal = visitDefs(root)
    val enumsVal = visitEnums(root)
    val latticeOpsVal = visitLatticeOps(root)
    val propertiesVal = visitProperties(root)

    Validation.mapN(defsVal, enumsVal, latticeOpsVal, propertiesVal) {
      case (defs, enums, latticeOps, properties) =>
        val specialOps = Map.empty[SpecialOperator, Map[Type, Symbol.DefnSym]]
        TypedAst.Root(defs, enums, latticeOps, properties, specialOps, root.reachable, root.sources)
    }
  }

  /**
    * Performs type inference and reassembly on all definitions in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitDefs(root: ResolvedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    /**
      * Performs type inference and reassembly on the given definition `defn`.
      */
    def visitDefn(defn: ResolvedAst.Def): Validation[TypedAst.Def, TypeError] =
      typeCheckDef(defn, root) map {
        case (defn, subst) => defn
      }

    // Compute the results in parallel.
    val results = ParOps.parMap(root.defs.values, visitDefn)

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
  private def typeCheckDef(defn0: ResolvedAst.Def, root: ResolvedAst.Root)(implicit flix: Flix): Validation[(TypedAst.Def, Substitution), TypeError] = defn0 match {
    case ResolvedAst.Def(doc, ann, mod, sym, tparams0, fparam0, exp0, declaredScheme, declaredEff, loc) =>

      ///
      /// Infer the type of the expression `exp0`.
      ///
      val result = for {
        (inferredTyp, inferredEff) <- inferExp(exp0, root)
      } yield Type.mkArrow(fparam0.tpe, inferredEff, inferredTyp)

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
          val initialSubst = getSubstFromParams(fparam0 :: Nil)

          run(initialSubst) match {
            case Ok((subst, partialType)) =>
              ///
              /// The partial type returned by the inference monad does not have the substitution applied.
              ///
              val inferredType = subst(partialType)

              ///
              /// Check that the inferred type is at least as general as the declared type.
              ///
              /// NB: Because the inferredType is always a function type, the effect is always implicitly accounted for.
              ///
              val sc = Scheme.generalize(inferredType)
              if (!Scheme.lessThanEqual(sc, declaredScheme)) {
                return Validation.Failure(LazyList(TypeError.GeneralizationError(declaredScheme, sc, loc)))
              }

              ///
              /// Compute the expression, type parameters, and formal parameters with the substitution applied everywhere.
              ///
              val exp = reassembleExp(exp0, root, subst)
              val tparams = getTypeParams(tparams0)
              val fparams = getFormalParams(fparam0 :: Nil, subst)

              ///
              /// Compute a type scheme that matches the type variables that appear in the expression body.
              ///
              /// NB: It is very important to understand that: The type scheme a function is declared with must match the inferred type scheme.
              /// However, we require an even stronger property for the implementation to work. The inferred type scheme used in the rest of the
              /// compiler must *use the same type variables* in the scheme as in the body expression. Otherwise monomorphization et al. will break.
              ///
              val inferredScheme = Scheme(inferredType.typeVars.toList, inferredType)

              ///
              /// Reassemble everything.
              ///
              Validation.Success((TypedAst.Def(doc, ann, mod, sym, tparams, fparams, exp, declaredScheme, inferredScheme, declaredEff, loc), subst))

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
      case ResolvedAst.Enum(doc, mod, enumSym, tparams, cases0, tpe, loc) =>
        val tparams = getTypeParams(enum.tparams)
        val cases = cases0 map {
          case (name, ResolvedAst.Case(_, tagName, tagType)) =>
            name -> TypedAst.Case(enumSym, tagName, tagType, tagName.loc)
        }

        Validation.Success(enumSym -> TypedAst.Enum(doc, mod, enumSym, tparams, cases, enum.tpe, loc))
    }

    // Visit every enum in the ast.
    val result = root.enums.toList.map {
      case (_, enum) => visitEnum(enum)
    }

    // Sequence the results and convert them back to a map.
    Validation.sequence(result).map(_.toMap)
  }

  /**
    * Performs type inference and reassembly on all lattices in the given AST root.
    *
    * Returns [[Err]] if a type error occurs.
    */
  private def visitLatticeOps(root: ResolvedAst.Root)(implicit flix: Flix): Validation[Map[Type, TypedAst.LatticeOps], TypeError] = {

    /**
      * Performs type inference and reassembly on the given `lattice`.
      */
    def visitLatticeOps(lattice: ResolvedAst.LatticeOps): Validation[(Type, TypedAst.LatticeOps), TypeError] = lattice match {
      case ResolvedAst.LatticeOps(tpe, e1, e2, e3, e4, e5, e6, ns, loc) =>
        // Perform type resolution on the declared type.
        val declaredType = lattice.tpe

        // Perform type inference on each of the lattice components.
        val m = for {
          // Type check each expression:
          (botType, botEff) <- inferExp(e1, root)
          (topType, topEff) <- inferExp(e2, root)
          (equType, equEff) <- inferExp(e3, root)
          (leqType, leqEff) <- inferExp(e4, root)
          (lubType, lubEff) <- inferExp(e5, root)
          (glbType, glbEff) <- inferExp(e6, root)
          // Enforce that each component is pure:
          _______ <- unifyEffM(botEff, Type.Pure, loc)
          _______ <- unifyEffM(topEff, Type.Pure, loc)
          _______ <- unifyEffM(equEff, Type.Pure, loc)
          _______ <- unifyEffM(leqEff, Type.Pure, loc)
          _______ <- unifyEffM(lubEff, Type.Pure, loc)
          _______ <- unifyEffM(glbEff, Type.Pure, loc)
          // Check the type of each component:
          _______ <- unifyTypM(botType, declaredType, loc)
          _______ <- unifyTypM(topType, declaredType, loc)
          _______ <- unifyTypM(equType, Type.mkArrow(List(declaredType, declaredType), Type.Pure, Type.Bool), loc)
          _______ <- unifyTypM(leqType, Type.mkArrow(List(declaredType, declaredType), Type.Pure, Type.Bool), loc)
          _______ <- unifyTypM(lubType, Type.mkArrow(List(declaredType, declaredType), Type.Pure, declaredType), loc)
          _______ <- unifyTypM(glbType, Type.mkArrow(List(declaredType, declaredType), Type.Pure, declaredType), loc)
        } yield declaredType

        // Evaluate the type inference monad with the empty substitution
        m.run(Substitution.empty) match {
          case Result.Ok((subst, _)) =>
            // Reassemble the lattice components.
            val bot = reassembleExp(e1, root, subst)
            val top = reassembleExp(e2, root, subst)
            val equ = reassembleExp(e3, root, subst)
            val leq = reassembleExp(e4, root, subst)
            val lub = reassembleExp(e5, root, subst)
            val glb = reassembleExp(e6, root, subst)
            Validation.Success(declaredType -> TypedAst.LatticeOps(declaredType, bot, top, equ, leq, lub, glb, loc))

          case Result.Err(e) => Validation.Failure(LazyList(e))
        }

    }


    // Visit every lattice in the ast.
    val result = root.latticeOps.toList.map {
      case (_, lattice) => visitLatticeOps(lattice)
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
    * Infers the type of the given expression `exp0`.
    */
  private def inferExp(exp0: ResolvedAst.Expression, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[(Type, Type)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: ResolvedAst.Expression): InferMonad[(Type, Type)] = e0 match {

      case ResolvedAst.Expression.Wild(tvar, loc) =>
        liftM(tvar, Type.Pure)

      case ResolvedAst.Expression.Var(sym, tpe, loc) =>
        for {
          resultTyp <- unifyTypM(sym.tvar, tpe, loc)
        } yield (resultTyp, Type.Pure)

      case ResolvedAst.Expression.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        for {
          resultTyp <- unifyTypM(tvar, Scheme.instantiate(defn.sc, InstantiateMode.Flexible), loc)
        } yield (resultTyp, Type.Pure)

      case ResolvedAst.Expression.Hole(sym, tvar, evar, loc) =>
        liftM(tvar, evar)

      case ResolvedAst.Expression.Unit(loc) =>
        liftM(Type.Unit, Type.Pure)

      case ResolvedAst.Expression.True(loc) =>
        liftM(Type.Bool, Type.Pure)

      case ResolvedAst.Expression.False(loc) =>
        liftM(Type.Bool, Type.Pure)

      case ResolvedAst.Expression.Char(lit, loc) =>
        liftM(Type.Char, Type.Pure)

      case ResolvedAst.Expression.Float32(lit, loc) =>
        liftM(Type.Float32, Type.Pure)

      case ResolvedAst.Expression.Float64(lit, loc) =>
        liftM(Type.Float64, Type.Pure)

      case ResolvedAst.Expression.Int8(lit, loc) =>
        liftM(Type.Int8, Type.Pure)

      case ResolvedAst.Expression.Int16(lit, loc) =>
        liftM(Type.Int16, Type.Pure)

      case ResolvedAst.Expression.Int32(lit, loc) =>
        liftM(Type.Int32, Type.Pure)

      case ResolvedAst.Expression.Int64(lit, loc) =>
        liftM(Type.Int64, Type.Pure)

      case ResolvedAst.Expression.BigInt(lit, loc) =>
        liftM(Type.BigInt, Type.Pure)

      case ResolvedAst.Expression.Str(lit, loc) =>
        liftM(Type.Str, Type.Pure)

      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val argType = fparam.tpe
        for {
          (bodyType, bodyEff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, Type.mkArrow(argType, bodyEff, bodyType), loc)
        } yield (resultTyp, Type.Pure)

      case ResolvedAst.Expression.Apply(exp1, exp2, tvar, evar, loc) =>
        val lambdaBodyType = Type.freshTypeVar()
        val lambdaBodyEff = Type.freshTypeVar()
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          lambdaType <- unifyTypM(tpe1, Type.mkArrow(tpe2, lambdaBodyEff, lambdaBodyType), loc)
          resultTyp <- unifyTypM(tvar, lambdaBodyType, loc)
          resultEff <- unifyEffM(evar, mkAnd(eff1, eff2, lambdaBodyEff), loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Unary(op, exp, tvar, loc) => op match {
        case UnaryOperator.LogicalNot =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, Type.Bool, loc)
            resultEff = eff
          } yield (resultTyp, resultEff)

        case UnaryOperator.Plus =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, loc)
            resultEff = eff
          } yield (resultTyp, resultEff)

        case UnaryOperator.Minus =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, loc)
            resultEff = eff
          } yield (resultTyp, resultEff)

        case UnaryOperator.BitwiseNegate =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, loc)
            resultEff = eff
          } yield (resultTyp, resultEff)
      }

      case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, loc) => op match {
        case BinaryOperator.Plus =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Minus =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Times =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Divide =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Modulo =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Exponentiate =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Equal | BinaryOperator.NotEqual =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypM(tpe1, tpe2, loc)
            resultTyp <- unifyTypM(tvar, Type.Bool, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypM(tpe1, tpe2, loc)
            resultTyp <- unifyTypM(tvar, Type.Bool, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Spaceship =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypM(tpe1, tpe2, loc)
            resultTyp <- unifyTypM(tvar, Type.Int32, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultType <- unifyTypM(tvar, tpe1, tpe2, Type.Bool, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultType, resultEff)

        case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (resultTyp, resultEff)

        case BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            lhsType <- unifyTypM(tvar, tpe1, loc)
            rhsType <- unifyTypM(tpe2, Type.Int32, loc)
            resultEff = mkAnd(eff1, eff2)
          } yield (lhsType, resultEff)
      }

      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          (tpe3, eff3) <- visitExp(exp3)
          condType <- unifyTypM(Type.Bool, tpe1, loc)
          resultTyp <- unifyTypM(tpe2, tpe3, loc)
          resultEff = mkAnd(eff1, eff2, eff3)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Stm(exp1, exp2, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = mkAnd(eff1, eff2)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Let(sym, exp1, exp2, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          boundVar <- unifyTypM(sym.tvar, tpe1, loc)
          resultTyp = tpe2
          resultEff = mkAnd(eff1, eff2)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Match(exp, rules, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.map(_.guard)
        val bodies = rules.map(_.exp)

        for {
          (tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          patternType <- unifyTypM(tpe :: patternTypes, loc)
          (guardTypes, guardEffects) <- seqM(guards map visitExp).map(_.unzip)
          guardType <- unifyTypM(Type.Bool :: guardTypes, loc)
          (bodyTypes, bodyEffects) <- seqM(bodies map visitExp).map(_.unzip)
          resultTyp <- unifyTypM(bodyTypes, loc)
          resultEff = mkAnd(eff :: guardEffects ::: bodyEffects)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        // TODO: Use a type scheme?

        // Lookup the enum declaration.
        val decl = root.enums(sym)

        // Generate a fresh type variable for each type parameters.
        val subst = Substitution(decl.tparams.map {
          case param => param.tpe -> Type.freshTypeVar()
        }.toMap)

        // Retrieve the enum type.
        val enumType = decl.tpe

        // Substitute the fresh type variables into the enum type.
        val freshEnumType = subst(enumType)

        // Retrieve the case type.
        val caseType = decl.cases(tag).tpe

        // Substitute the fresh type variables into the case type.
        val freshCaseType = subst(caseType)
        for {
          (tpe, eff) <- visitExp(exp)
          _________ <- unifyTypM(tpe, freshCaseType, loc)
          resultTyp <- unifyTypM(tvar, freshEnumType, loc)
          resultEff = eff
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Tuple(elms, loc) =>
        for {
          (elementTypes, elementEffects) <- seqM(elms.map(visitExp)).map(_.unzip)
          resultEff = mkAnd(elementEffects)
        } yield (Type.mkTuple(elementTypes), resultEff)

      case ResolvedAst.Expression.RecordEmpty(tvar, loc) =>
        //
        //  ---------
        //  { } : { }
        //
        for {
          resultType <- unifyTypM(tvar, Type.RecordEmpty, loc)
        } yield (resultType, Type.Pure)

      case ResolvedAst.Expression.RecordSelect(exp, label, tvar, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        // r.label : tpe
        //
        val freshRowVar = Type.freshTypeVar()
        val expectedType = Type.mkRecordExtend(label, tvar, freshRowVar)
        for {
          (tpe, eff) <- visitExp(exp)
          recordType <- unifyTypM(tpe, expectedType, loc)
          resultEff = eff
        } yield (tvar, resultEff)

      case ResolvedAst.Expression.RecordExtend(label, exp1, exp2, tvar, loc) =>
        //
        // exp1 : tpe
        // ---------------------------------------------
        // { label = exp1 | exp2 } : { label : tpe | r }
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, Type.mkRecordExtend(label, tpe1, tpe2), loc)
          resultEff = mkAnd(eff1, eff2)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.RecordRestrict(label, exp, tvar, loc) =>
        //
        // ----------------------
        // { -label | r } : { r }
        //
        val freshFieldType = Type.freshTypeVar()
        val freshRowVar = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          recordType <- unifyTypM(tpe, Type.mkRecordExtend(label, freshFieldType, freshRowVar), loc)
          resultTyp <- unifyTypM(tvar, freshRowVar, loc)
          resultEff = eff
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
        //
        //  e1 : t ... en: t
        //  --------------------------------
        //  [e1,..., en] : Array[t] @ Impure
        //
        if (elms.isEmpty) {
          for {
            resultTyp <- unifyTypM(tvar, mkArray(Type.freshTypeVar()), loc)
            resultEff = Type.Impure
          } yield (resultTyp, resultEff)
        } else {
          for {
            (elementTypes, _) <- seqM(elms.map(visitExp)).map(_.unzip)
            elementType <- unifyTypM(elementTypes, loc)
            resultTyp <- unifyTypM(tvar, mkArray(elementType), loc)
            resultEff = Type.Impure
          } yield (resultTyp, resultEff)
        }

      case ResolvedAst.Expression.ArrayNew(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : t @ _    exp2: Int @ _
        //  ---------------------------------
        //  [exp1 ; exp2] : Array[t] @ Impure
        //
        for {
          (tpe1, _) <- visitExp(exp1)
          (tpe2, _) <- visitExp(exp2)
          lengthType <- unifyTypM(tpe2, Type.Int32, loc)
          resultTyp <- unifyTypM(tvar, mkArray(tpe1), loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : Array[t] @ _   exp2: Int @ _
        //  -----------------------------------
        //  exp1[exp2] : t @ Impure
        //
        for {
          (tpe1, _) <- visitExp(exp1)
          (tpe2, _) <- visitExp(exp2)
          arrayType <- unifyTypM(tpe1, mkArray(tvar), loc)
          indexType <- unifyTypM(tpe2, Type.Int32, loc)
          resultEff = Type.Impure
        } yield (tvar, resultEff)

      case ResolvedAst.Expression.ArrayLength(exp, loc) =>
        //
        //  exp : Array[t] @ e
        //  --------------------
        //  exp.length : Int @ e
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          arrayType <- unifyTypM(tpe, mkArray(elementType), loc)
          resultEff = eff
        } yield (Type.Int32, resultEff)

      case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, loc) =>
        //
        //  exp1 : Array[t] @ _   exp2 : Int @ _   exp3 : t @ _
        //  ---------------------------------------------------
        //  exp1[exp2] = exp3 : Unit @ Impure
        //
        for {
          (tpe1, _) <- visitExp(exp1)
          (tpe2, _) <- visitExp(exp2)
          (tpe3, _) <- visitExp(exp3)
          arrayType <- unifyTypM(tpe1, mkArray(tpe3), loc)
          indexType <- unifyTypM(tpe2, Type.Int32, loc)
          resultEff = Type.Impure
        } yield (Type.Unit, resultEff)

      case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, loc) =>
        //
        //  exp1 : Array[t] @ _   exp2 : Int @ _   exp3 : Int @ _
        //  -----------------------------------------------------
        //  exp1[exp2..exp3] : Array[t] @ Impure
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe1, _) <- visitExp(exp1)
          (tpe2, _) <- visitExp(exp2)
          (tpe3, _) <- visitExp(exp3)
          fstIndexType <- unifyTypM(tpe2, Type.Int32, loc)
          lstIndexType <- unifyTypM(tpe3, Type.Int32, loc)
          resultTyp <- unifyTypM(tpe1, mkArray(elementType), loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Ref(exp, tvar, loc) =>
        //
        //  exp : t @ eff
        //  -------------------------
        //  ref exp : Ref[t] @ Impure
        //
        for {
          (tpe, _) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, mkRefType(tpe), loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
        //
        //  exp : Ref[t] @ eff
        //  -------------------
        //  deref exp : t @ Impure
        //
        val elementType = Type.freshTypeVar()
        for {
          (typ, _) <- visitExp(exp)
          refType <- unifyTypM(typ, mkRefType(elementType), loc)
          resultTyp <- unifyTypM(tvar, elementType, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Assign(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : Ref[t] @ eff1   exp2: t @ eff2
        //  -------------------------------------
        //  exp1 := exp2 : Unit @ Impure
        //
        for {
          (tpe1, _) <- visitExp(exp1)
          (tpe2, _) <- visitExp(exp2)
          refType <- unifyTypM(tpe1, mkRefType(tpe2), loc)
          resultTyp <- unifyTypM(tvar, Type.Unit, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
        for {
          paramTyp <- unifyTypM(fparam.sym.tvar, fparam.tpe, loc)
          (typ, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(typ, Type.Bool, loc)
        } yield (resultTyp, Type.Pure)

      case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
        for {
          paramTyp <- unifyTypM(fparam.sym.tvar, fparam.tpe, loc)
          (typ, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(typ, Type.Bool, loc)
        } yield (resultTyp, Type.Pure)

      case ResolvedAst.Expression.Ascribe(exp, expectedTyp, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, actualTyp, expectedTyp.getOrElse(tvar), loc)
          resultEff <- unifyEffM(actualEff, expectedEff.getOrElse(Type.freshEffectVar()), loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Cast(exp, declaredTyp, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        for {
          (actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, declaredTyp.getOrElse(actualTyp), loc)
          resultEff = declaredEff.getOrElse(actualEff)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.TryCatch(exp, rules, tvar, loc) =>
        val rulesType = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (tpe, eff) <- visitExp(exp)
          (ruleTypes, ruleEffects) <- seqM(rulesType).map(_.unzip)
          ruleType <- unifyTypM(ruleTypes, loc)
          resultTyp <- unifyTypM(tvar, tpe, ruleType, loc)
          resultEff = mkAnd(eff :: ruleEffects)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        val classType = getFlixType(constructor.getDeclaringClass)
        for {
          argTypesAndEffects <- seqM(args.map(visitExp))
          resultTyp = classType
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.InvokeMethod(method, exp, args, loc) =>
        val classType = getFlixType(method.getDeclaringClass)
        val returnType = getFlixType(method.getReturnType)
        for {
          (baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypM(baseTyp, classType, loc)
          argTypesAndEffects <- seqM(args.map(visitExp))
          resultTyp = getFlixType(method.getReturnType)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        val returnType = getFlixType(method.getReturnType)
        for {
          argTypesAndEffects <- seqM(args.map(visitExp))
          resultTyp = returnType
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.GetField(field, exp, tvar, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(field.getDeclaringClass)
        for {
          (baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypM(baseTyp, classType, loc)
          resultTyp <- unifyTypM(tvar, fieldType, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.PutField(field, exp1, exp2, tvar, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(field.getDeclaringClass)
        for {
          (baseTyp, _) <- visitExp(exp1)
          (valueType, _) <- visitExp(exp2)
          objectTyp <- unifyTypM(baseTyp, classType, loc)
          valueTyp <- unifyTypM(valueType, fieldType, loc)
          resultTyp <- unifyTypM(tvar, Type.Unit, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.GetStaticField(field, tvar, loc) =>
        val fieldType = getFlixType(field.getType)
        for {
          resultTyp <- unifyTypM(tvar, fieldType, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.PutStaticField(field, exp, tvar, loc) =>
        for {
          (valueTyp, _) <- visitExp(exp)
          fieldTyp <- unifyTypM(getFlixType(field.getType), valueTyp, loc)
          resultTyp <- unifyTypM(tvar, Type.Unit, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.NewChannel(exp, declaredType, loc) =>
        //
        //  exp: Int @ _
        //  ---------------------------------
        //  channel exp : Channel[t] @ Impure
        //
        for {
          (tpe, _) <- visitExp(exp)
          lengthType <- unifyTypM(tpe, Type.Int32, loc)
          resultTyp <- liftM(mkChannel(declaredType))
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.GetChannel(exp, tvar, loc) =>
        //
        //  exp: Channel[t] @ _
        //  -------------------
        //  <- exp : t @ Impure
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe, _) <- visitExp(exp)
          channelType <- unifyTypM(tpe, mkChannel(elementType), loc)
          resultTyp <- unifyTypM(tvar, elementType, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, loc) =>
        //
        //  exp1: Channel[t] @ _   exp2: t @ _
        //  ----------------------------------
        //  exp1 <- exp2 : Channel[t] @ Impure
        //
        for {
          (tpe1, _) <- visitExp(exp1)
          (tpe2, _) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, tpe1, mkChannel(tpe2), loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      /*
       * Select Channel Expression.
       */
      case ResolvedAst.Expression.SelectChannel(rules, default, tvar, loc) =>
        //  SelectChannelRule
        //
        //  chan: Channel[t1],   exp: t2
        //  ------------------------------------------------
        //  case sym <- chan => exp : t2
        //
        //
        //  SelectChannel
        //  rule_i: t,     default: t
        //  ------------------------------------------------
        //  select { rule_i; (default) } : t
        //
        // check that each rules channel expression is a channel
        def inferSelectChannelRule(rule: ResolvedAst.SelectChannelRule): InferMonad[Unit] = {
          rule match {
            case ResolvedAst.SelectChannelRule(sym, chan, exp) => for {
              (channelType, _) <- visitExp(chan)
              _ <- unifyTypM(channelType, mkChannel(Type.freshTypeVar()), loc)
            } yield liftM(Type.Unit)
          }
        }

        // check that default case has same type as bodies (the same as result type)
        def inferSelectChannelDefault(rtpe: Type, defaultCase: Option[ResolvedAst.Expression]): InferMonad[Type] = {
          defaultCase match {
            case None => liftM(Type.Unit)
            case Some(exp) =>
              for {
                (tpe, _) <- visitExp(exp)
                _ <- unifyTypM(rtpe, tpe, loc)
              } yield Type.Unit
          }
        }

        val bodies = rules.map(_.exp)
        for {
          _ <- seqM(rules.map(inferSelectChannelRule))
          (bodyTypes, _) <- seqM(bodies map visitExp).map(_.unzip)
          actualResultType <- unifyTypM(bodyTypes, loc)
          _ <- inferSelectChannelDefault(actualResultType, default)
          resultTyp <- unifyTypM(tvar, actualResultType, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Spawn(exp, tvar, loc) =>
        //
        //  exp: t @ _
        //  -------------------------
        //  spawn exp : Unit @ Impure
        //
        for {
          (tpe, _) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, Type.Unit, loc)
          resultEff = Type.Impure
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointConstraintSet(cs, tvar, loc) =>
        for {
          constraintTypes <- seqM(cs.map(visitConstraint))
          resultTyp <- unifyTypAllowEmptyM(tvar :: constraintTypes, loc)
        } yield (resultTyp, Type.Pure)

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, tpe1, tpe2, mkAnySchemaType(), loc)
          resultEff = mkAnd(eff1, eff2)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointSolve(exp, tvar, loc) =>
        //
        //  exp : #{...}
        //  ---------------
        //  solve exp : tpe
        //
        for {
          (tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, tpe, mkAnySchemaType(), loc)
          resultEff = eff
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointProject(name, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : #{ P : a  | b }
        //  -------------------------------------------
        //  project P exp2 : #{ P : a | c }
        //
        val freshPredicateTypeVar = Type.freshTypeVar()
        val freshRestSchemaTypeVar = Type.freshTypeVar()
        val freshResultSchemaTypeVar = Type.freshTypeVar()

        for {
          (tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypM(tpe, Type.mkSchemaExtend(name, freshPredicateTypeVar, freshRestSchemaTypeVar), loc)
          resultTyp <- unifyTypM(tvar, Type.mkSchemaExtend(name, freshPredicateTypeVar, freshResultSchemaTypeVar), loc)
          resultEff = eff
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 |= exp2 : Bool
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          schemaType <- unifyTypM(tpe1, tpe2, mkAnySchemaType(), loc)
          resultTyp <- unifyTypM(tvar, Type.Bool, loc)
          resultEff = mkAnd(eff1, eff2)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointFold(name, exp1, exp2, exp3, tvar, loc) =>
        //
        // exp3 : #{P : a | c}    init : b   exp2 : a' -> b -> b
        // where a' is the tuple reification of relation a
        // ---------------------------------------------------
        // fold P exp1 exp2 exp3 : b
        //
        val freshPredicateNameTypeVar = Type.freshTypeVar()
        val tupleType = Type.freshTypeVar()
        val freshRestTypeVar = Type.freshTypeVar()
        for {
          (initType, eff1) <- visitExp(exp1)
          (fType, eff2) <- visitExp(exp2)
          (constraintsType, eff3) <- visitExp(exp3)
          // constraints should have the form {pred.sym : R(tupleType) | freshRestTypeVar}
          constraintsType2 <- unifyTypM(constraintsType, Type.mkSchemaExtend(name, Type.Apply(freshPredicateNameTypeVar, tupleType), freshRestTypeVar), loc)
          // f is of type tupleType -> initType -> initType. It cannot have any effect.
          fType2 <- unifyTypM(fType, Type.mkPureArrow(tupleType, Type.mkPureArrow(initType, initType)), loc)
          resultTyp <- unifyTypM(tvar, initType, loc) // the result of the fold is the same type as init
          resultEff = mkAnd(eff1, eff2, eff3)
        } yield (resultTyp, resultEff)
    }

    /**
      * Infers the type of the given constraint `con0` inside the inference monad.
      */
    def visitConstraint(con0: ResolvedAst.Constraint): InferMonad[Type] = {
      val ResolvedAst.Constraint(cparams, head0, body0, loc) = con0
      //
      //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
      //  -----------------------------------
      //  A_0 :- A_1, ..., A_n : tpe
      //
      for {
        headPredicateType <- inferHeadPredicate(head0, root)
        bodyPredicateTypes <- seqM(body0.map(b => inferBodyPredicate(b, root)))
        bodyPredicateType <- unifyTypAllowEmptyM(bodyPredicateTypes, loc)
        resultType <- unifyTypM(headPredicateType, bodyPredicateType, loc)
      } yield resultType
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

      case ResolvedAst.Expression.Hole(sym, tpe, evar, loc) =>
        TypedAst.Expression.Hole(sym, subst0(tpe), subst0(evar), loc)

      case ResolvedAst.Expression.Unit(loc) => TypedAst.Expression.Unit(loc)

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

      case ResolvedAst.Expression.Apply(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Apply(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val p = visitParam(fparam)
        val e = visitExp(exp, subst0)
        val t = subst0(tvar)
        TypedAst.Expression.Lambda(p, e, t, loc)

      case ResolvedAst.Expression.Unary(op, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Unary(op, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.Binary(op, e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e2.tpe
        val eff = mkAnd(e1.eff, e2.eff, e3.eff)
        TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case ResolvedAst.Expression.Stm(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.Stm(e1, e2, tpe, eff, loc)

      case ResolvedAst.Expression.Let(sym, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = mkAnd(e1.eff, e2.eff)
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
          case (acc, TypedAst.MatchRule(_, g, b)) => mkAnd(g.eff, b.eff, acc)
        }
        TypedAst.Expression.Match(e1, rs, tpe, eff, loc)

      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Tag(sym, tag, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Tuple(elms, loc) =>
        val es = elms.map(visitExp(_, subst0))
        val tpe = Type.mkTuple(es.map(_.tpe))
        val eff = mkAnd(es.map(_.eff))
        TypedAst.Expression.Tuple(es, tpe, eff, loc)

      case ResolvedAst.Expression.RecordEmpty(tvar, loc) =>
        TypedAst.Expression.RecordEmpty(subst0(tvar), loc)

      case ResolvedAst.Expression.RecordSelect(exp, label, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.RecordSelect(e, label, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.RecordExtend(label, value, rest, tvar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        val eff = mkAnd(v.eff, r.eff)
        TypedAst.Expression.RecordExtend(label, v, r, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.RecordRestrict(label, rest, tvar, loc) =>
        val r = visitExp(rest, subst0)
        val eff = r.eff
        TypedAst.Expression.RecordRestrict(label, r, subst0(tvar), eff, loc)

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

      case ResolvedAst.Expression.Ref(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.Ref(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.Deref(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Assign(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.Impure
        TypedAst.Expression.Assign(e1, e2, subst0(tvar), eff, loc)

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

      case ResolvedAst.Expression.Cast(exp, _, declaredEff, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = declaredEff.getOrElse(e.eff)
        TypedAst.Expression.Cast(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.TryCatch(exp, rules, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        val eff = mkAnd(rs.map(_.exp.eff))
        TypedAst.Expression.TryCatch(e, rs, subst0(tvar), eff, loc)

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

      case ResolvedAst.Expression.GetField(field, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.GetField(field, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.PutField(field, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.Impure
        TypedAst.Expression.PutField(field, e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.GetStaticField(field, tvar, loc) =>
        val eff = Type.Impure
        TypedAst.Expression.GetStaticField(field, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.PutStaticField(field, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.PutStaticField(field, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expression.NewChannel(e, mkChannel(tpe), eff, loc)

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
          case ResolvedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule(sym, c, b)
        }
        val d = default.map(visitExp(_, subst0))
        val eff = Type.Impure
        TypedAst.Expression.SelectChannel(rs, d, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.Spawn(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.Spawn(e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.FixpointConstraintSet(cs0, tvar, loc) =>
        val cs = cs0.map(visitConstraint)
        TypedAst.Expression.FixpointConstraintSet(cs, subst0(tvar), loc)

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.FixpointCompose(e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.FixpointSolve(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.FixpointSolve(e, Stratification.Empty, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.FixpointProject(name, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expression.FixpointProject(name, e, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = mkAnd(e1.eff, e2.eff)
        TypedAst.Expression.FixpointEntails(e1, e2, subst0(tvar), eff, loc)

      case ResolvedAst.Expression.FixpointFold(name, init, f, constraints, tvar, loc) =>
        val e1 = visitExp(init, subst0)
        val e2 = visitExp(f, subst0)
        val e3 = visitExp(constraints, subst0)
        val eff = mkAnd(e1.eff, e2.eff, e3.eff)
        TypedAst.Expression.FixpointFold(name, e1, e2, e3, subst0(tvar), eff, loc)
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
      case ResolvedAst.Pattern.Var(sym, tvar, loc) => unifyTypM(sym.tvar, tvar, loc)
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

        // Generate a fresh type variable for each type parameters.
        val subst = Substitution(decl.tparams.map {
          case param => param.tpe -> Type.freshTypeVar()
        }.toMap)

        // Retrieve the enum type.
        val enumType = decl.tpe

        // Substitute the fresh type variables into the enum type.
        val freshEnumType = subst(enumType)

        // Retrieve the case type.
        val caseType = decl.cases(tag).tpe

        // Substitute the fresh type variables into the case type.
        val freshCaseType = subst(caseType)
        for (
          innerType <- visit(pat);
          _________ <- unifyTypM(innerType, freshCaseType, loc);
          resultType <- unifyTypM(tvar, freshEnumType, loc)
        ) yield resultType

      case ResolvedAst.Pattern.Tuple(elms, loc) =>
        for {
          elementTypes <- seqM(elms map visit)
        } yield Type.mkTuple(elementTypes)

      case ResolvedAst.Pattern.Array(elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          elementType <- unifyTypAllowEmptyM(elementTypes, loc);
          resultType <- unifyTypM(tvar, mkArray(elementType), loc)
        ) yield resultType

      case ResolvedAst.Pattern.ArrayTailSpread(elms, varSym, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          elementType <- unifyTypAllowEmptyM(elementTypes, loc);
          arrayType <- unifyTypM(tvar, mkArray(elementType), loc);
          resultType <- unifyTypM(varSym.tvar, arrayType, loc)
        ) yield resultType

      case ResolvedAst.Pattern.ArrayHeadSpread(varSym, elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          elementType <- unifyTypAllowEmptyM(elementTypes, loc);
          arrayType <- unifyTypM(tvar, mkArray(elementType), loc);
          resultType <- unifyTypM(varSym.tvar, arrayType, loc)
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
  private def inferHeadPredicate(head: ResolvedAst.Predicate.Head, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[Type] = head match {
    case ResolvedAst.Predicate.Head.Atom(name, den, terms, tvar, loc) =>
      //
      //  t_1 : tpe_1, ..., t_n: tpe_n
      //  ------------------------------------------------------------
      //  P(t_1, ..., t_n): #{ P = P(tpe_1, ..., tpe_n) | fresh }
      //

      for {
        (termTypes, termEffects) <- seqM(terms.map(inferExp(_, root))).map(_.unzip)
        pureTermEffects <- unifyEffM(Type.Pure, mkAnd(termEffects), loc)
        predicateType <- unifyTypM(tvar, mkRelationOrLatticeType(name, den, termTypes, root), loc)
      } yield Type.mkSchemaExtend(name, predicateType, Type.freshTypeVar())

    case ResolvedAst.Predicate.Head.Union(exp, tvar, loc) =>
      //
      //  exp : typ
      //  ------------------------------------------------------------
      //  union exp : #{ ... }
      //
      for {
        (typ, eff) <- inferExp(exp, root)
        pureEff <- unifyEffM(Type.Pure, eff, loc)
        resultType <- unifyTypM(tvar, typ, loc)
      } yield resultType
  }

  /**
    * Applies the given substitution `subst0` to the given head predicate `head0`.
    */
  private def reassembleHeadPredicate(head0: ResolvedAst.Predicate.Head, root: ResolvedAst.Root, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case ResolvedAst.Predicate.Head.Atom(name, den0, terms, tvar, loc) =>
      val ts = terms.map(t => reassembleExp(t, root, subst0))
      TypedAst.Predicate.Head.Atom(name, den0, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Head.Union(exp, tvar, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Head.Union(e, subst0(tvar), loc)
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: ResolvedAst.Predicate.Body, root: ResolvedAst.Root)(implicit flix: Flix): InferMonad[Type] = body0 match {
    //
    //  t_1 : tpe_1, ..., t_n: tpe_n
    //  ------------------------------------------------------------
    //  P(t_1, ..., t_n): Schema{ P = P(tpe_1, ..., tpe_n) | fresh }
    //
    case ResolvedAst.Predicate.Body.Atom(name, den, polarity, terms, tvar, loc) =>
      for {
        termTypes <- seqM(terms.map(inferPattern(_, root)))
        predicateType <- unifyTypM(tvar, mkRelationOrLatticeType(name, den, termTypes, root), loc)
      } yield Type.mkSchemaExtend(name, predicateType, Type.freshTypeVar())

    //
    //  exp : Bool
    //  ----------
    //  if exp : a
    //
    case ResolvedAst.Predicate.Body.Guard(exp, loc) =>
      // Infer the types of the terms.
      for {
        (tpe, eff) <- inferExp(exp, root)
        expEff <- unifyEffM(Type.Pure, eff, loc)
        expTyp <- unifyTypM(Type.Bool, tpe, loc)
      } yield mkAnySchemaType()
  }

  /**
    * Applies the given substitution `subst0` to the given body predicate `body0`.
    */
  private def reassembleBodyPredicate(body0: ResolvedAst.Predicate.Body, root: ResolvedAst.Root, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case ResolvedAst.Predicate.Body.Atom(name, den0, polarity, terms, tvar, loc) =>
      val ts = terms.map(t => reassemblePattern(t, root, subst0))
      TypedAst.Predicate.Body.Atom(name, den0, polarity, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Body.Guard(exp, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Body.Guard(e, loc)
  }

  /**
    * Returns the relation or lattice type of `name` with the term types `ts`.
    */
  private def mkRelationOrLatticeType(name: String, den: Denotation, ts: List[Type], root: ResolvedAst.Root)(implicit flix: Flix): Type = den match {
    case Denotation.Relational =>
      val base = Type.Cst(TypeConstructor.Relation): Type
      val args = ts match {
        case Nil => Type.Unit
        case x :: Nil => x
        case l => Type.mkTuple(l)
      }
      Type.Apply(base, args)
    case Denotation.Latticenal =>
      val base = Type.Cst(TypeConstructor.Lattice): Type
      val args = ts match {
        case Nil => Type.Unit
        case x :: Nil => x
        case l => Type.mkTuple(l)
      }
      Type.Apply(base, args)
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
    case ResolvedAst.TypeParam(name, tpe, loc) => TypedAst.TypeParam(name, tpe, loc)
  }

  /**
    * Returns the typed version of the given formal parameters `fparams0`.
    */
  private def getFormalParams(fparams0: List[ResolvedAst.FormalParam], subst0: Substitution): List[TypedAst.FormalParam] = fparams0.map {
    case ResolvedAst.FormalParam(sym, mod, tpe, loc) => TypedAst.FormalParam(sym, mod, subst0(sym.tvar), sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaType()(implicit flix: Flix): Type = Type.freshTypeVar()

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
      mkArray(elmType)
    }
    // otherwise native type
    else {
      Type.Cst(TypeConstructor.Native(c))
    }
  }

  /**
    * Returns the type `Array[tpe]`.
    */
  private def mkArray(elmType: Type): Type = Type.Apply(Type.Cst(TypeConstructor.Array), elmType)

  /**
    * Returns the type `Channel[tpe]`.
    */
  private def mkChannel(tpe: Type): Type = Type.Apply(Type.Cst(TypeConstructor.Channel), tpe)

  /**
    * Returns the type `Ref[tpe]`.
    */
  private def mkRefType(tpe: Type): Type = Type.Apply(Type.Cst(TypeConstructor.Ref), tpe)

  /**
    * Returns the type `And(eff1, eff2)`.
    */
  private def mkAnd(eff1: Type, eff2: Type): Type = (eff1, eff2) match {
    case (Type.Cst(TypeConstructor.Pure), _) => eff2
    case (_, Type.Cst(TypeConstructor.Pure)) => eff1
    case (Type.Cst(TypeConstructor.Impure), _) => Type.Impure
    case (_, Type.Cst(TypeConstructor.Impure)) => Type.Impure
    case _ => Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And), eff1), eff2)
  }

  /**
    * Returns the type `And(eff1, And(eff2, eff3))`.
    */
  private def mkAnd(eff1: Type, eff2: Type, eff3: Type): Type = mkAnd(eff1, mkAnd(eff2, eff3))

  /**
    * Returns the type `And(eff1, And(eff2, ...))`.
    */
  private def mkAnd(effs: List[Type]): Type = effs.foldLeft(Type.Pure: Type)(mkAnd)

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
      val mean = StatUtils.mean(sizes)
      val median = StatUtils.median(sizes)
      val total = sizes.sum
      t.mkRow(List(sym.toString, size, f"$mean%2.1f", median, total))
    }
    t.write(new PrintWriter(System.out))
  }

}
