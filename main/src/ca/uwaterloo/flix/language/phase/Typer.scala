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
import ca.uwaterloo.flix.language.ast.Ast.Stratification
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.Unification._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{ParOps, Result, Validation}

object Typer extends Phase[ResolvedAst.Program, TypedAst.Root] {

  /**
    * Type checks the given program.
    */
  def run(program: ResolvedAst.Program)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Typer") {
    val result = for {
      defs <- typeDefs(program)
      effs <- typeEffs(program)
      handlers <- typeHandlers(program)
      enums <- typeEnums(program)
      relations <- typeRelations(program)
      lattices <- typeLattices(program)
      latticeComponents <- typeLatticeComponents(program)
      properties <- typeProperties(program)
    } yield {
      val specialOps = Map.empty[SpecialOperator, Map[Type, Symbol.DefnSym]]
      TypedAst.Root(defs, effs, handlers, enums, relations, lattices, latticeComponents, properties, specialOps, program.reachable, program.sources)
    }

    result match {
      case Ok(p) => p.toSuccess
      case Err(e) => e.toFailure
    }
  }

  /**
    * Performs type inference and reassembly on all definitions in the given program.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def typeDefs(program: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    /**
      * Performs type inference and reassembly on the given definition `defn`.
      */
    def visitDefn(defn: ResolvedAst.Def): Result[(Symbol.DefnSym, TypedAst.Def), TypeError] = defn match {
      case ResolvedAst.Def(doc, ann, mod, sym, tparams, params, exp, tpe, eff, loc) =>
        typeCheckDef(defn, program) map {
          case d => sym -> d
        }
    }

    // Every definition in the program.
    val defs = program.defs.values.toList

    // Visit every definition in parallel.
    val results = ParOps.parMap(visitDefn, defs)

    // Sequence the results and convert them back to a map.
    Result.sequence(results).map(_.toMap)
  }

  /**
    * Infers the types of the effects in the given program.
    */
  private def typeEffs(program0: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.EffSym, TypedAst.Eff], TypeError] = {
    // Typecheck every effect in the program.
    val effs = program0.effs.toList.map {
      case (sym, eff0) => typeCheckEff(eff0) map (e => sym -> e)
    }

    // Sequence the results and convert them back to a map.
    Result.sequence(effs).map(_.toMap)
  }

  /**
    * Infers the types of the handlers in the given program.
    */
  private def typeHandlers(program0: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.EffSym, TypedAst.Handler], TypeError] = {
    // Typecheck every handler in the program.
    val effs = program0.handlers.toList.map {
      case (sym, handler0) => typeCheckHandler(handler0, program0) map (e => sym -> e)
    }

    // Sequence the results and convert them back to a map.
    Result.sequence(effs).map(_.toMap)
  }

  /**
    * Performs type inference and reassembly on all enums in the given program.
    */
  private def typeEnums(program: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.EnumSym, TypedAst.Enum], TypeError] = {
    /**
      * Performs type resolution on the given enum and its cases.
      */
    def visitEnum(enum: ResolvedAst.Enum): Result[(Symbol.EnumSym, TypedAst.Enum), TypeError] = enum match {
      case ResolvedAst.Enum(doc, mod, enumSym, tparams, cases0, tpe, loc) =>
        val tparams = getTypeParams(enum.tparams)
        val cases = cases0 map {
          case (name, ResolvedAst.Case(_, tagName, tagType)) =>
            name -> TypedAst.Case(enumSym, tagName, tagType, tagName.loc)
        }

        Ok(enumSym -> TypedAst.Enum(doc, mod, enumSym, tparams, cases, enum.tpe, loc))
    }

    // Visit every enum in the program.
    val result = program.enums.toList.map {
      case (_, enum) => visitEnum(enum)
    }

    // Sequence the results and convert them back to a map.
    Result.sequence(result).map(_.toMap)
  }

  /**
    * Performs type inference and reassembly on all relations in the given program.
    *
    * Returns [[Err]] if type resolution fails.
    */
  private def typeRelations(program: ResolvedAst.Program): Result[Map[Symbol.RelSym, TypedAst.Relation], TypeError] = {
    // Visit every relation in the program.
    val result = program.relations.toList.map {
      case (_, rel) => typeCheckRel(rel)
    }

    // Sequence the results and convert them back to a map.
    Result.sequence(result).map(_.toMap)
  }

  /**
    * Performs type inference and reassembly on all lattices in the given program.
    *
    * Returns [[Err]] if type resolution fails.
    */
  private def typeLattices(program: ResolvedAst.Program): Result[Map[Symbol.LatSym, TypedAst.Lattice], TypeError] = {
    // Visit every relation in the program.
    val result = program.lattices.toList.map {
      case (_, lat) => typeCheckLat(lat)
    }

    // Sequence the results and convert them back to a map.
    Result.sequence(result).map(_.toMap)
  }

  /**
    * Performs type inference and reassembly on all lattices in the given program.
    *
    * Returns [[Err]] if a type error occurs.
    */
  private def typeLatticeComponents(program: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Type, TypedAst.LatticeComponents], TypeError] = {

    /**
      * Performs type inference and reassembly on the given `lattice`.
      */
    def visitLattice(lattice: ResolvedAst.LatticeComponents): Result[(Type, TypedAst.LatticeComponents), TypeError] = lattice match {
      case ResolvedAst.LatticeComponents(tpe, e1, e2, e3, e4, e5, e6, ns, loc) =>
        // Perform type resolution on the declared type.
        val declaredType = lattice.tpe

        // Perform type inference on each of the lattice components.
        val m = for {
          // Type check each expression:
          (botType, botEff) <- inferExp(e1, program)
          (topType, topEff) <- inferExp(e2, program)
          (equType, equEff) <- inferExp(e3, program)
          (leqType, leqEff) <- inferExp(e4, program)
          (lubType, lubEff) <- inferExp(e5, program)
          (glbType, glbEff) <- inferExp(e6, program)
          // Enforce that each component is pure:
          _______ <- unifyEffM(botEff, Eff.Pure, loc)
          _______ <- unifyEffM(topEff, Eff.Pure, loc)
          _______ <- unifyEffM(equEff, Eff.Pure, loc)
          _______ <- unifyEffM(leqEff, Eff.Pure, loc)
          _______ <- unifyEffM(lubEff, Eff.Pure, loc)
          _______ <- unifyEffM(glbEff, Eff.Pure, loc)
          // Check the type of each component:
          _______ <- unifyTypM(botType, declaredType, loc)
          _______ <- unifyTypM(topType, declaredType, loc)
          _______ <- unifyTypM(equType, Type.mkArrow(List(declaredType, declaredType), mkBoolType()), loc)
          _______ <- unifyTypM(leqType, Type.mkArrow(List(declaredType, declaredType), mkBoolType()), loc)
          _______ <- unifyTypM(lubType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
          _______ <- unifyTypM(glbType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
        } yield declaredType

        // Evaluate the type inference monad with the empty substitution
        m.run(Substitution.empty) map {
          case (subst, _) =>
            // Reassemble the lattice components.
            val bot = reassembleExp(e1, program, subst)
            val top = reassembleExp(e2, program, subst)
            val equ = reassembleExp(e3, program, subst)
            val leq = reassembleExp(e4, program, subst)
            val lub = reassembleExp(e5, program, subst)
            val glb = reassembleExp(e6, program, subst)

            declaredType -> TypedAst.LatticeComponents(declaredType, bot, top, equ, leq, lub, glb, loc)
        }

    }


    // Visit every lattice in the program.
    val result = program.latticeComponents.toList.map {
      case (_, lattice) => visitLattice(lattice)
    }

    // Sequence the results and convert them back to a map.
    Result.sequence(result).map(_.toMap)
  }

  /**
    * Infers the types of all the properties in the given program `prog0`.
    */
  private def typeProperties(prog0: ResolvedAst.Program)(implicit flix: Flix): Result[List[TypedAst.Property], TypeError] = {

    /**
      * Infers the type of the given property `p0`.
      */
    def visitProperty(p0: ResolvedAst.Property): Result[TypedAst.Property, TypeError] = p0 match {
      case ResolvedAst.Property(law, defn, exp0, loc) =>
        val result = inferExp(exp0, prog0)
        result.run(Substitution.empty) map {
          case (subst, tpe) =>
            val exp = reassembleExp(exp0, prog0, subst)
            TypedAst.Property(law, defn, exp, loc)
        }
    }

    // Visit every property in the program.
    val results = prog0.properties.map {
      case property => visitProperty(property)
    }

    // Sequence the results and sort the properties by their source location.
    Result.sequence(results).map(_.sortBy(_.loc))
  }

  /**
    * Infers the type of the given definition `defn0`.
    */
  private def typeCheckDef(defn0: ResolvedAst.Def, program: ResolvedAst.Program)(implicit flix: Flix): Result[TypedAst.Def, TypeError] = {
    // Resolve the declared scheme.
    val declaredScheme = defn0.sc

    // TODO: Some duplication
    val argumentTypes = defn0.fparams.map(_.tpe).map(openSchemaType)

    // TODO: Use resultEff
    val result = for (
      (inferredTyp, inferredEff) <- inferExp(defn0.exp, program);
      unifiedTyp <- unifyTypM(Scheme.instantiate(declaredScheme), Type.mkArrow(argumentTypes, inferredTyp), defn0.loc)
      // unifiedEff <- unifyEffM(defn0.eff, inferredEff, defn0.loc) // TODO
    ) yield unifiedTyp

    // TODO: See if this can be rewritten nicer
    result match {
      case InferMonad(run) =>
        val subst0 = getSubstFromParams(defn0.fparams)
        run(subst0) match {
          case Ok((subst, resultType)) =>
            val exp = reassembleExp(defn0.exp, program, subst)
            val tparams = getTypeParams(defn0.tparams)
            val fparams = getFormalParams(defn0.fparams, subst)
            // TODO: XXX: We should preserve type schemas here to ensure that monomorphization happens correctly. and remove .tpe
            Ok(TypedAst.Def(defn0.doc, defn0.ann, defn0.mod, defn0.sym, tparams, fparams, exp, defn0.sc, resultType, defn0.eff, defn0.loc))

          case Err(e) => Err(e)
        }
    }
  }

  /**
    * Infers the the type of the given handler `handler0`.
    */
  private def typeCheckHandler(handler0: ResolvedAst.Handler, program0: ResolvedAst.Program)(implicit flix: Flix): Result[TypedAst.Handler, TypeError] = handler0 match {
    case ResolvedAst.Handler(doc, ann, mod, sym, tparams0, fparams0, exp0, sc, eff0, loc) =>
      val eff = program0.effs(sym)
      val effectType = Scheme.instantiate(eff.sc)

      val declaredType = Scheme.instantiate(sc)
      val subst0 = getSubstFromParams(fparams0)
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst0)
      val argumentTypes = fparams.map(_.tpe)

      val result = for {
        (resultType, resultEff) <- inferExp(exp0, program0)
        unifiedType <- unifyTypM(declaredType, effectType, Type.mkArrow(argumentTypes, resultType), loc)
      } yield unifiedType

      result.run(Substitution.empty) map {
        case (subst, unifiedType) =>
          val exp = reassembleExp(exp0, program0, subst)
          TypedAst.Handler(doc, ann, mod, sym, tparams, fparams, exp, subst(unifiedType), eff0, loc)
      }
  }

  /**
    * Infers the the type of the given effect `eff0`.
    */
  private def typeCheckEff(eff0: ResolvedAst.Eff)(implicit flix: Flix): Result[TypedAst.Eff, TypeError] = eff0 match {
    case ResolvedAst.Eff(doc, ann, mod, sym, tparams0, fparams0, sc, eff, loc) =>
      val argumentTypes = fparams0.map(_.tpe)
      val tpe = Scheme.instantiate(sc)

      val subst = getSubstFromParams(fparams0)
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst)

      Ok(TypedAst.Eff(doc, ann, mod, sym, tparams, fparams, tpe, eff, loc))
  }

  /**
    * Performs type resolution on the given relation `r`.
    *
    * Returns [[Err]] if a type is unresolved.
    */
  private def typeCheckRel(r: ResolvedAst.Relation): Result[(Symbol.RelSym, TypedAst.Relation), TypeError] = r match {
    case ResolvedAst.Relation(doc, mod, sym, tparams0, attr0, sc0, loc) =>
      val tparams = getTypeParams(tparams0)
      for {
        attr <- Result.sequence(attr0.map(a => typeCheckAttribute(a)))
      } yield sym -> TypedAst.Relation(doc, mod, sym, tparams, attr, loc)
  }

  /**
    * Performs type resolution on the given lattice `l`.
    *
    * Returns [[Err]] if a type is unresolved.
    */
  private def typeCheckLat(r: ResolvedAst.Lattice): Result[(Symbol.LatSym, TypedAst.Lattice), TypeError] = r match {
    case ResolvedAst.Lattice(doc, mod, sym, tparams0, attr0, sc0, loc) =>
      val tparams = getTypeParams(tparams0)
      for {
        attr <- Result.sequence(attr0.map(a => typeCheckAttribute(a)))
      } yield sym -> TypedAst.Lattice(doc, mod, sym, tparams, attr, loc)
  }

  /**
    * Infers the type of the given expression `exp0`.
    */
  private def inferExp(exp0: ResolvedAst.Expression, program: ResolvedAst.Program)(implicit flix: Flix): InferMonad[(Type, Eff)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: ResolvedAst.Expression): InferMonad[(Type, Eff)] = e0 match {

      case ResolvedAst.Expression.Wild(tvar, evar, loc) => liftM((tvar, evar))

      case ResolvedAst.Expression.Var(sym, tvar, evar, loc) =>
        for {
          resultTyp <- unifyTypM(sym.tvar, tvar, loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.Def(sym, tvar, evar, loc) =>
        val defn = program.defs(sym)
        for {
          resultTyp <- unifyTypM(tvar, Scheme.instantiate(defn.sc), loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.Eff(sym, tvar, evar, loc) =>
        val eff = program.effs(sym)
        for {
          resultTyp <- unifyTypM(tvar, Scheme.instantiate(eff.sc), loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.Sig(sym, tvar, evar, loc) =>
        val sig = program.classes(sym.clazz)
        ??? // TODO: Sig

      case ResolvedAst.Expression.Hole(sym, tvar, evar, loc) =>
        liftM((tvar, evar))

      case ResolvedAst.Expression.Unit(loc) =>
        liftM((mkUnitType(), Eff.freshEffVar()))

      case ResolvedAst.Expression.True(loc) =>
        liftM((mkBoolType(), Eff.freshEffVar()))

      case ResolvedAst.Expression.False(loc) =>
        liftM((mkBoolType(), Eff.freshEffVar()))

      case ResolvedAst.Expression.Char(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Char), Eff.freshEffVar()))

      case ResolvedAst.Expression.Float32(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Float32), Eff.freshEffVar()))

      case ResolvedAst.Expression.Float64(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Float64), Eff.freshEffVar()))

      case ResolvedAst.Expression.Int8(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Int8), Eff.freshEffVar()))

      case ResolvedAst.Expression.Int16(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Int16), Eff.freshEffVar()))

      case ResolvedAst.Expression.Int32(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Int32), Eff.freshEffVar()))

      case ResolvedAst.Expression.Int64(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Int64), Eff.freshEffVar()))

      case ResolvedAst.Expression.BigInt(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.BigInt), Eff.freshEffVar()))

      case ResolvedAst.Expression.Str(lit, loc) =>
        liftM((Type.Cst(TypeConstructor.Str), Eff.freshEffVar()))

      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, evar, loc) =>
        val argType = fparam.tpe
        for {
          (bodyType, bodyEff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, Type.mkArrow(argType, bodyEff, bodyType), loc)
          resultEff <- unifyEffM(evar, Eff.freshEffVar(), loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Apply(exp1, exp2, tvar, evar, loc) =>
        val lambdaBodyType = Type.freshTypeVar()
        val lambdaBodyEff = Eff.freshEffVar()
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          lambdaType <- unifyTypM(tpe1, Type.mkArrow(tpe2, lambdaBodyEff, lambdaBodyType), loc)
          resultTyp <- unifyTypM(tvar, lambdaBodyType, loc)
          resultEff <- unifyEffM(evar, eff1, eff2, lambdaBodyEff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Unary(op, exp, tvar, evar, loc) => op match {
        case UnaryOperator.LogicalNot =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, mkBoolType(), loc)
            resultEff <- unifyEffM(evar, eff, loc)
          } yield (resultTyp, resultEff)

        case UnaryOperator.Plus =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, loc)
            resultEff <- unifyEffM(evar, eff, loc)
          } yield (resultTyp, resultEff)

        case UnaryOperator.Minus =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, loc)
            resultEff <- unifyEffM(evar, eff, loc)
          } yield (resultTyp, resultEff)

        case UnaryOperator.BitwiseNegate =>
          for {
            (tpe, eff) <- visitExp(exp)
            resultTyp <- unifyTypM(tvar, tpe, loc)
            resultEff <- unifyEffM(evar, eff, loc)
          } yield (resultTyp, resultEff)
      }

      case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, evar, loc) => op match {
        case BinaryOperator.Plus =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Minus =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Times =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Divide =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Modulo =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Exponentiate =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Equal | BinaryOperator.NotEqual =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypM(tpe1, tpe2, loc)
            resultTyp <- unifyTypM(tvar, mkBoolType(), loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypM(tpe1, tpe2, loc)
            resultTyp <- unifyTypM(tvar, mkBoolType(), loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultType <- unifyTypM(tvar, tpe1, tpe2, mkBoolType(), loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultType, resultEff)

        case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            resultTyp <- unifyTypM(tvar, tpe1, tpe2, loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (resultTyp, resultEff)

        case BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
          for {
            (tpe1, eff1) <- visitExp(exp1)
            (tpe2, eff2) <- visitExp(exp2)
            lhsType <- unifyTypM(tvar, tpe1, loc)
            rhsType <- unifyTypM(tpe2, Type.Cst(TypeConstructor.Int32), loc)
            resultEff <- unifyEffM(evar, eff1, eff2, loc)
          } yield (lhsType, resultEff)
      }

      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, evar, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          (tpe3, eff3) <- visitExp(exp3)
          condType <- unifyTypM(mkBoolType(), tpe1, loc)
          resultTyp <- unifyTypM(tvar, tpe2, tpe3, loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Stm(exp1, exp2, tvar, evar, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, tpe2, loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Let(sym, exp1, exp2, tvar, evar, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          boundVar <- unifyTypM(sym.tvar, tpe1, loc)
          resultTyp <- unifyTypM(tvar, tpe2, loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.LetRec(sym, exp1, exp2, tvar, evar, loc) =>
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          boundVar <- unifyTypM(sym.tvar, tpe1, loc)
          resultTyp <- unifyTypM(tvar, tpe2, loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Match(exp, rules, tvar, evar, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.map(_.guard)
        val bodies = rules.map(_.exp)

        for {
          (tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, program)
          patternType <- unifyTypM(tpe :: patternTypes, loc)
          (guardTypes, guardEffects) <- seqM(guards map visitExp).map(_.unzip)
          guardType <- unifyTypM(mkBoolType() :: guardTypes, loc)
          (bodyTypes, bodyEffects) <- seqM(bodies map visitExp).map(_.unzip)
          resultTyp <- unifyTypM(tvar :: bodyTypes, loc)
          resultEff <- unifyEffM(evar :: guardEffects ::: bodyEffects, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Switch(rules, tvar, evar, loc) =>
        val condExps = rules.map(_._1)
        val bodyExps = rules.map(_._2)
        for {
          (condTypes, condEffects) <- seqM(condExps map visitExp).map(_.unzip)
          (bodyTypes, bodyEffects) <- seqM(bodyExps map visitExp).map(_.unzip)
          condType <- unifyTypM(mkBoolType() :: condTypes, loc)
          resultTyp <- unifyTypM(tvar :: bodyTypes, loc)
          resultEff <- unifyEffM(evar :: condEffects ::: bodyEffects, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, evar, loc) =>
        // TODO: Use a type scheme?

        // Lookup the enum declaration.
        val decl = program.enums(sym)

        // Generate a fresh type variable for each type parameters.
        val subst = Substitution(decl.tparams.map {
          case param => param.tpe -> Type.freshTypeVar()
        }.toMap, Map.empty)

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
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Tuple(elms, tvar, evar, loc) =>
        for {
          (elementTypes, elementEffects) <- seqM(elms.map(visitExp)).map(_.unzip)
          resultTyp <- unifyTypM(tvar, Type.mkTuple(elementTypes), loc)
          resultEff <- unifyEffM(evar :: elementEffects, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.RecordEmpty(tvar, evar, loc) =>
        //
        //  ---------
        //  { } : { }
        //
        for {
          resultType <- unifyTypM(tvar, Type.RecordEmpty, loc)
        } yield (resultType, evar)

      case ResolvedAst.Expression.RecordSelect(exp, label, tvar, evar, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        // r.label : tpe
        //
        val freshRowVar = Type.freshTypeVar()
        val expectedType = Type.RecordExtend(label, tvar, freshRowVar)
        for {
          (tpe, eff) <- visitExp(exp)
          recordType <- unifyTypM(tpe, expectedType, loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (tvar, resultEff)

      case ResolvedAst.Expression.RecordExtend(label, exp1, exp2, tvar, evar, loc) =>
        //
        // exp1 : tpe
        // ---------------------------------------------
        // { label = exp1 | exp2 } : { label : tpe | r }
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, Type.RecordExtend(label, tpe1, tpe2), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.RecordRestrict(label, exp, tvar, evar, loc) =>
        //
        // ----------------------
        // { -label | r } : { r }
        //
        val freshFieldType = Type.freshTypeVar()
        val freshRowVar = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          recordType <- unifyTypM(tpe, Type.RecordExtend(label, freshFieldType, freshRowVar), loc)
          resultTyp <- unifyTypM(tvar, freshRowVar, loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayLit(elms, tvar, evar, loc) =>
        //
        //  e1 : t ... en: t
        //  ----------------------
        //  [e1,...,en] : Array[t]
        //
        if (elms.isEmpty) {
          for {
            resultType <- unifyTypM(tvar, mkArray(Type.freshTypeVar()), loc)
          } yield (resultType, evar)
        } else {
          for {
            (elementTypes, elementEffects) <- seqM(elms.map(visitExp)).map(_.unzip)
            elementType <- unifyTypM(elementTypes, loc)
            resultTyp <- unifyTypM(tvar, mkArray(elementType), loc)
            resultEff <- unifyEffM(evar :: elementEffects, loc)
          } yield (resultTyp, resultEff)
        }

      case ResolvedAst.Expression.ArrayNew(exp1, exp2, tvar, evar, loc) =>
        //
        //  exp1 : t    exp2: Int
        //  ------------------------
        //  [exp1 ; exp2] : Array[t]
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          lengthType <- unifyTypM(tpe2, Type.Cst(TypeConstructor.Int32), loc)
          resultTyp <- unifyTypM(tvar, mkArray(tpe1), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, evar, loc) =>
        //
        //  exp1 : Array[t]    exp2: Int
        //  ----------------------------
        //  exp1[exp2] : t
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          arrayType <- unifyTypM(tpe1, mkArray(tvar), loc)
          indexType <- unifyTypM(tpe2, Type.Cst(TypeConstructor.Int32), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (tvar, resultEff)

      case ResolvedAst.Expression.ArrayLength(exp, tvar, evar, loc) =>
        //
        //  exp : Array[t]
        //  ----------------
        //  exp.length : Int
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          arrayType <- unifyTypM(tpe, mkArray(elementType), loc)
          resultTyp <- unifyTypM(tvar, Type.Cst(TypeConstructor.Int32), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, tvar, evar, loc) =>
        //
        //  exp1 : Array[t]    exp2 : Int    exp3 : t
        //  -----------------------------------------
        //  exp1[exp2] = exp3 : Unit
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          (tpe3, eff3) <- visitExp(exp3)
          arrayType <- unifyTypM(tpe1, mkArray(elementType), loc)
          indexType <- unifyTypM(tpe2, Type.Cst(TypeConstructor.Int32), loc)
          elementType <- unifyTypM(tpe3, elementType, loc)
          resultTyp <- unifyTypM(tvar, mkUnitType(), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, eff3, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, tvar, evar, loc) =>
        //
        //  exp1 : Array[t]    exp2 : Int    exp3 : Int
        //  -------------------------------------------
        //  exp1[exp2..exp3] : Array[t]
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          (tpe3, eff3) <- visitExp(exp3)
          fstIndexType <- unifyTypM(tpe2, Type.Cst(TypeConstructor.Int32), loc)
          lstIndexType <- unifyTypM(tpe3, Type.Cst(TypeConstructor.Int32), loc)
          resultTyp <- unifyTypM(tvar, tpe1, mkArray(elementType), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, eff3, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.VectorLit(elms, tvar, evar, loc) =>
        //
        // elm1: t ...  elm_len: t  len: Succ(n, Zero)
        // -------------------------------------------
        // [|elm1,...,elm_len|] : Vector[t, len]
        //
        if (elms.isEmpty) {
          for {
            resultType <- unifyTypM(tvar, mkVector(Type.freshTypeVar(), Type.Succ(0, Type.Zero)), loc)
          } yield (resultType, evar)
        } else {
          for {
            (elementTypes, elementEffects) <- seqM(elms.map(visitExp)).map(_.unzip)
            elementType <- unifyTypM(elementTypes, loc)
            resultTyp <- unifyTypM(tvar, mkVector(elementType, Type.Succ(elms.length, Type.Zero)), loc)
            resultEff <- unifyEffM(evar :: elementEffects, loc)
          } yield (resultTyp, resultEff)
        }

      case ResolvedAst.Expression.VectorNew(exp, len, tvar, evar, loc) =>
        //
        // exp: t    len: Succ(n, Zero)
        // --------------------------------
        // [|exp1 ; len |] : Vector[t, len]
        //
        for {
          (tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, mkVector(tpe, Type.Succ(len, Type.Zero)), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.VectorLoad(exp, index, tvar, evar, loc) =>
        //
        //  exp : Vector[t, len1]   index: len2   len1: Succ(n1, Zero) len2: Succ(n2, Var)
        //  ------------------------------------------------------------------------------
        //  exp[|index|] : t
        //
        val elementType = Type.freshTypeVar()
        val indexOffsetType = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          vectorType <- unifyTypM(tpe, mkVector(elementType, Type.Succ(index, indexOffsetType)), loc)
          resultTyp <- unifyTypM(tvar, elementType, loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.VectorStore(exp1, index, exp2, tvar, evar, loc) =>
        //
        //  exp1 : Vector[t, len1]   index: len2   exp2: t    len1: Succ(n1, Zero)  len2: Succ(n2, Var)
        //  -------------------------------------------------------------------------------------------
        //  exp1[|index|] = exp2 : Unit
        //
        val elementType = Type.freshTypeVar()
        val indexOffsetType = Type.freshTypeVar()
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          vectorType <- unifyTypM(tpe1, mkVector(elementType, Type.Succ(index, indexOffsetType)), loc)
          elementType <- unifyTypM(tpe2, elementType, loc)
          resultTyp <- unifyTypM(tvar, mkUnitType(), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.VectorLength(exp, tvar, evar, loc) =>
        //
        // exp: Vector[t, len1]   index: len2   len1: Succ(n1, Zero)  len2: Succ(n2, Var)
        // ------------------------------------------------------------------------------
        // exp[|index|] : Int
        //
        val elementType = Type.freshTypeVar()
        val indexOffsetType = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          vectorType <- unifyTypM(tpe, mkVector(elementType, Type.Succ(0, indexOffsetType)), loc)
          resultTyp <- unifyTypM(tvar, Type.Cst(TypeConstructor.Int32), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.VectorSlice(exp, startIndex, optEndIndex, tvar, evar, loc) =>
        //
        //  Case None =
        //  exp : Vector[t, len2]   startIndex : len3
        //  len1 : Succ(n1, Zero) len2 : Succ(n2, Zero) len3 : Succ(n3, Var)
        //  --------------------------------------------------------------------------------------
        //  exp[|startIndex.. |] : Vector[t, len1]
        //
        //
        //  Case Some =
        //  exp : Vector[t, len2]   startIndex : len3   endIndexOpt : len4
        //  len1 : Succ(n1, Zero) len2 : Succ(n2, Zero) len3 : Succ(n3, Var) len 4 : Succ(n4, Var)
        //  --------------------------------------------------------------------------------------
        //  base[startIndex..endIndexOpt] : Vector[t, len1]
        //
        val freshBeginIndex = Type.freshTypeVar()
        val freshEndIndex = Type.freshTypeVar()
        val freshElmType = Type.freshTypeVar()
        optEndIndex match {
          case None =>
            for {
              (tpe, eff) <- visitExp(exp)
              fstIndex <- unifyTypM(tpe, mkVector(freshElmType, Type.Succ(startIndex, freshEndIndex)), loc)
              resultTyp <- unifyTypM(tvar, mkVector(freshElmType, freshEndIndex), loc)
              resultEff <- unifyEffM(evar, eff, loc)
            } yield (resultTyp, resultEff)

          case Some(endIndex) =>
            for {
              (tpe, eff) <- visitExp(exp)
              fstIndex <- unifyTypM(tpe, mkVector(freshElmType, Type.Succ(startIndex, freshBeginIndex)), loc)
              lstIndex <- unifyTypM(tpe, mkVector(freshElmType, Type.Succ(endIndex, freshEndIndex)), loc)
              resultTyp <- unifyTypM(tvar, mkVector(freshElmType, Type.Succ(endIndex - startIndex, Type.Zero)), loc)
              resultEff <- unifyEffM(evar, eff, loc)
            } yield (resultTyp, resultEff)
        }

      case ResolvedAst.Expression.Ref(exp, tvar, evar, loc) =>
        //
        //  exp : t
        //  ----------------
        //  ref exp : Ref[t]
        //
        for {
          (tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, mkRefType(tpe), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Deref(exp, tvar, evar, loc) =>
        //
        //  exp : Ref[t]
        //  -------------
        //  deref exp : t
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          refType <- unifyTypM(tpe, mkRefType(elementType), loc)
          resultTyp <- unifyTypM(tvar, elementType, loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.Assign(exp1, exp2, tvar, evar, loc) =>
        //
        //  exp1 : Ref[t]    exp2: t
        //  ------------------------
        //  exp1 := exp2 : Unit
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          refType <- unifyTypM(tpe1, mkRefType(tpe2), loc)
          resultTyp <- unifyTypM(tvar, mkUnitType(), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.HandleWith(exp, bindings, tvar, evar, loc) =>
        // TODO: Need to check that the return types are consistent.

        // Typecheck each handler binding.
        val bs = bindings map {
          case ResolvedAst.HandlerBinding(sym, handler) =>
            val eff = program.effs(sym)
            val declaredType = Scheme.instantiate(eff.sc)
            for {
              (actualType, eff) <- visitExp(handler) // TODO: Eff
            } yield unifyTypM(declaredType, actualType, loc)
        }

        // Typecheck the expression.
        for {
          (tpe, eff) <- visitExp(exp)
          handlers <- seqM(bs)
          resultTyp <- unifyTypM(tvar, tpe, loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Existential(fparam, exp, evar, loc) =>
        // TODO: Check formal parameter type.
        for {
          (bodyType, bodyEff) <- visitExp(exp)
          resultTyp <- unifyTypM(bodyType, mkBoolType(), loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.Universal(fparam, exp, evar, loc) =>
        // TODO: Check formal parameter type.
        for {
          (bodyType, bodyEff) <- visitExp(exp)
          resultTyp <- unifyTypM(bodyType, mkBoolType(), loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.Ascribe(exp, expectedType, expectedEff, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (actualType, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypM(actualType, expectedType, loc)
          resultEff <- unifyEffM(actualEff, expectedEff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.Cast(exp, declaredType, eff, loc) =>
        // An cast expression is unsound; the type system assumes the declared type is correct.
        for {
          actualType <- visitExp(exp)
        } yield (declaredType, eff)

      case ResolvedAst.Expression.TryCatch(exp, rules, tvar, evar, loc) =>
        val rulesType = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (tpe, eff) <- visitExp(exp)
          (ruleTypes, ruleEffects) <- seqM(rulesType).map(_.unzip) // TODO: Effects.
          ruleType <- unifyTypM(ruleTypes, loc)
          resultTyp <- unifyTypM(tvar, tpe, ruleType, loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.NativeConstructor(constructor, actuals, tvar, evar, loc) =>
        // TODO: Check types.
        val clazz = constructor.getDeclaringClass
        for {
          inferredArgumentTypes <- seqM(actuals.map(visitExp))
          resultTyp <- unifyTypM(tvar, Type.Cst(TypeConstructor.Native(clazz)), loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.NativeField(field, tvar, evar, loc) =>
        // TODO: Check types.
        liftM((tvar, evar))

      case ResolvedAst.Expression.NativeMethod(method, actuals, tvar, evar, loc) =>
        // TODO: Check argument types.
        val returnType = getGenericFlixType(method.getGenericReturnType)
        for {
          inferredArgumentTypes <- seqM(actuals.map(visitExp))
          resultTyp <- unifyTypM(tvar, returnType, loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.NewChannel(exp, declaredType, evar, loc) =>
        //
        //  exp: Int
        //  ------------------------
        //  channel exp : Channel[t]
        //
        for {
          (tpe, eff) <- visitExp(exp)
          lengthType <- unifyTypM(tpe, Type.Cst(TypeConstructor.Int32), loc)
          resultTyp <- liftM(mkChannel(declaredType))
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.GetChannel(exp, tvar, evar, loc) =>
        //
        //  exp: Channel[t]
        //  ---------------
        //  <- exp : tvar
        //
        val elementType = Type.freshTypeVar()
        for {
          (tpe, eff) <- visitExp(exp)
          channelType <- unifyTypM(tpe, mkChannel(elementType), loc)
          resultTyp <- unifyTypM(tvar, elementType, loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, evar, loc) =>
        //
        //  exp1: Channel[t]    exp2: t
        //  ---------------------------
        //  exp1 <- exp2 : Channel[t]
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, tpe1, mkChannel(tpe2), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      /*
       * Select Channel Expression.
       */
      case ResolvedAst.Expression.SelectChannel(rules, default, tvar, evar, loc) =>
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
        liftM(rules.nonEmpty)
        val bodies = rules.map(_.exp)

        // check that each rules channel expression is a channel
        def inferSelectChannelRule(rule: ResolvedAst.SelectChannelRule): InferMonad[Unit] = {
          rule match {
            case ResolvedAst.SelectChannelRule(sym, chan, exp) => for {
              (channelType, channelEffect) <- visitExp(chan) // TODO: Effect
              _ <- unifyTypM(channelType, mkChannel(Type.freshTypeVar()), loc)
            } yield liftM(mkUnitType())
          }
        }

        // check that default case has same type as bodies (the same as result type)
        def inferSelectChannelDefault(rtpe: Type, defaultCase: Option[ResolvedAst.Expression]): InferMonad[Unit] = {
          defaultCase match {
            case Some(exp) =>
              for {
                (tpe, eff) <- visitExp(exp) // TODO: Effect
                _ <- unifyTypM(rtpe, tpe, loc)
              } yield liftM(mkUnitType())
            case None => liftM(mkUnitType())
          }
        }

        for {
          _ <- seqM(rules.map(inferSelectChannelRule))
          (bodyTypes, bodyEffects) <- seqM(bodies map visitExp).map(_.unzip)
          actualResultType <- unifyTypM(bodyTypes, loc)
          _ <- inferSelectChannelDefault(actualResultType, default)
          resultTyp <- unifyTypM(tvar, actualResultType, loc)
          resultEff <- unifyEffM(evar :: bodyEffects, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ProcessSpawn(exp, tvar, evar, loc) =>
        //
        //  exp: t
        //  ----------------
        //  spawn exp : Unit
        //
        for {
          (tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, mkUnitType(), loc)
        } yield (resultTyp, evar)

      case ResolvedAst.Expression.ProcessSleep(exp, tvar, evar, loc) =>
        //
        // exp: Int
        // ----------------
        // sleep exp : Unit
        //
        for {
          (tpe, eff) <- visitExp(exp)
          durationType <- unifyTypM(tpe, Type.Cst(TypeConstructor.Int64), loc)
          resultTyp <- unifyTypM(tvar, mkUnitType(), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.ProcessPanic(msg, tvar, evar, loc) =>
        liftM((tvar, evar))

      case ResolvedAst.Expression.FixpointConstraintSet(cs, tvar, evar, loc) =>
        for {
          constraintTypes <- seqM(cs.map(visitConstraint))
          resultTyp <- unifyTypAllowEmptyM(tvar :: constraintTypes, loc)
          resultEff <- unifyEffM(Eff.freshEffVar(), evar, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, tvar, evar, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypM(tvar, tpe1, tpe2, mkAnySchemaType(), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointSolve(exp, tvar, evar, loc) =>
        //
        //  exp : #{...}
        //  ---------------
        //  solve exp : tpe
        //
        for {
          (tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypM(tvar, tpe, mkAnySchemaType(), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointProject(sym, exp, tvar, evar, loc) =>
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
          expectedType <- unifyTypM(tpe, Type.SchemaExtend(sym, freshPredicateTypeVar, freshRestSchemaTypeVar), loc)
          resultTyp <- unifyTypM(tvar, Type.SchemaExtend(sym, freshPredicateTypeVar, freshResultSchemaTypeVar), loc)
          resultEff <- unifyEffM(evar, eff, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, tvar, evar, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 |= exp2 : Bool
        //
        for {
          (tpe1, eff1) <- visitExp(exp1)
          (tpe2, eff2) <- visitExp(exp2)
          schemaType <- unifyTypM(tpe1, tpe2, mkAnySchemaType(), loc)
          resultTyp <- unifyTypM(tvar, mkBoolType(), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, loc)
        } yield (resultTyp, resultEff)

      case ResolvedAst.Expression.FixpointFold(sym, init, f, constraints, tvar, evar, loc) =>
        //
        // constraints : #{P : a | c}    init : b   f : a' -> b -> b
        // where a' is the tuple reification of relation a
        // ---------------------------------------------------
        // fold P init f constraints : b
        //
        val freshPredicateTypeVar = Type.freshTypeVar()
        val freshRestTypeVar = Type.freshTypeVar()
        for {
          (initType, eff1) <- visitExp(init)
          (fType, eff2) <- visitExp(f)
          (constraintsType, eff3) <- visitExp(constraints)
          // constraints should have the form {pred.sym : freshPredicateTypeVar | freshRestTypeVar}
          constraintsType2 <- unifyTypM(constraintsType, Type.SchemaExtend(sym, freshPredicateTypeVar, freshRestTypeVar), loc)
          tupleType = sym match {
            case rel: Symbol.RelSym =>
              Type.mkTuple(program.relations(rel).attr.map(a => a.tpe))
            case lat: Symbol.LatSym =>
              /* TODO: what should be done in this case? */
              ???
          }
          // f is of type tupleType -> initType -> initType. It cannot have any effect.
          fType2 <- unifyTypM(fType, Type.mkArrow(tupleType, Eff.Pure, Type.mkArrow(initType, Eff.Pure, initType)), loc)
          resultEff <- unifyEffM(evar, eff1, eff2, eff3, loc)
          resultTyp <- unifyTypM(tvar, initType, loc) // the result of the fold is the same type as init
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
        headPredicateType <- inferHeadPredicate(head0, program)
        bodyPredicateTypes <- seqM(body0.map(b => inferBodyPredicate(b, program)))
        bodyPredicateType <- unifyTypAllowEmptyM(bodyPredicateTypes, loc)
        resultType <- unifyTypM(headPredicateType, bodyPredicateType, loc)
      } yield resultType
    }

    visitExp(exp0)
  }

  /**
    * Applies the given substitution `subst0` to the given expression `exp0`.
    */
  private def reassembleExp(exp0: ResolvedAst.Expression, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Expression = {
    /**
      * Applies the given substitution `subst0` to the given expression `exp0`.
      */
    def visitExp(exp0: ResolvedAst.Expression, subst0: Substitution): TypedAst.Expression = exp0 match {

      case ResolvedAst.Expression.Wild(tvar, evar, loc) =>
        TypedAst.Expression.Wild(subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Var(sym, tvar, evar, loc) =>
        TypedAst.Expression.Var(sym, subst0(sym.tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Def(sym, tvar, evar, loc) =>
        TypedAst.Expression.Def(sym, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Eff(sym, tvar, evar, loc) =>
        TypedAst.Expression.Eff(sym, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Sig(sym, tvar, evar, loc) =>
        ??? // TODO

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

      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, evar, loc) =>
        val p = visitParam(fparam)
        val e = visitExp(exp, subst0)
        val t = subst0(tvar)
        TypedAst.Expression.Lambda(p, e, t, subst0(evar), loc)

      case ResolvedAst.Expression.Unary(op, exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Unary(op, e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Binary(op, e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.IfThenElse(e1, e2, e3, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Stm(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Stm(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Let(sym, exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Let(sym, e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.LetRec(sym, exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.LetRec(sym, e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Match(exp1, rules, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val rs = rules map {
          case ResolvedAst.MatchRule(pat, guard, exp) =>
            val p = reassemblePattern(pat, program, subst0)
            val g = visitExp(guard, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.MatchRule(p, g, b)
        }
        TypedAst.Expression.Match(e1, rs, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Switch(rules, tvar, evar, loc) =>
        val rs = rules.map {
          case (cond, body) => (visitExp(cond, subst0), visitExp(body, subst0))
        }
        TypedAst.Expression.Switch(rs, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Tag(sym, tag, e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Tuple(elms, tvar, evar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.Tuple(es, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.RecordEmpty(tvar, evar, loc) =>
        TypedAst.Expression.RecordEmpty(subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.RecordSelect(exp, label, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.RecordSelect(e, label, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.RecordExtend(label, value, rest, tvar, evar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        TypedAst.Expression.RecordExtend(label, v, r, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.RecordRestrict(label, rest, tvar, evar, loc) =>
        val r = visitExp(rest, subst0)
        TypedAst.Expression.RecordRestrict(label, r, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ArrayLit(elms, tvar, evar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.ArrayLit(es, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ArrayNew(elm, len, tvar, evar, loc) =>
        val e = visitExp(elm, subst0)
        val ln = visitExp(len, subst0)
        TypedAst.Expression.ArrayNew(e, ln, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.ArrayLoad(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.ArrayStore(e1, e2, e3, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ArrayLength(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.ArrayLength(e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.ArraySlice(e1, e2, e3, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.VectorLit(elms, tvar, evar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.VectorLit(es, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.VectorNew(elm, len, tvar, evar, loc) =>
        val e = visitExp(elm, subst0)
        TypedAst.Expression.VectorNew(e, len, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.VectorLoad(base, index, tvar, evar, loc) =>
        val b = visitExp(base, subst0)
        TypedAst.Expression.VectorLoad(b, index, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.VectorStore(base, index, elm, tvar, evar, loc) =>
        val b = visitExp(base, subst0)
        val e = visitExp(elm, subst0)
        TypedAst.Expression.VectorStore(b, index, e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.VectorLength(base, tvar, evar, loc) =>
        val b = visitExp(base, subst0)
        TypedAst.Expression.VectorLength(b, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.VectorSlice(base, startIndex, optEndIndex, tvar, evar, loc) =>
        val e = visitExp(base, subst0)
        optEndIndex match {
          case None =>
            val len = TypedAst.Expression.VectorLength(e, Type.Cst(TypeConstructor.Int32), subst0(evar), loc)
            TypedAst.Expression.VectorSlice(e, startIndex, len, subst0(tvar), subst0(evar), loc)
          case Some(endIndex) =>
            val len = TypedAst.Expression.Int32(endIndex, loc)
            TypedAst.Expression.VectorSlice(e, startIndex, len, subst0(tvar), subst0(evar), loc)
        }

      case ResolvedAst.Expression.Ref(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Ref(e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Deref(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Deref(e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Assign(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Assign(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.HandleWith(exp, bindings, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        val bs = bindings map {
          case ResolvedAst.HandlerBinding(sym, handler) => TypedAst.HandlerBinding(sym, visitExp(handler, subst0))
        }
        TypedAst.Expression.HandleWith(e, bs, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.Existential(fparam, exp, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Existential(visitParam(fparam), e, subst0(evar), loc)

      case ResolvedAst.Expression.Universal(fparam, exp, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Universal(visitParam(fparam), e, subst0(evar), loc)

      case ResolvedAst.Expression.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Ascribe(e, tpe, subst0(eff), loc)

      case ResolvedAst.Expression.Cast(exp, tpe, eff, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Cast(e, tpe, subst0(eff), loc)

      case ResolvedAst.Expression.TryCatch(exp, rules, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        TypedAst.Expression.TryCatch(e, rs, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.NativeConstructor(constructor, actuals, tpe, evar, loc) =>
        val es = actuals.map(e => visitExp(e, subst0))
        TypedAst.Expression.NativeConstructor(constructor, es, subst0(tpe), subst0(evar), loc)

      case ResolvedAst.Expression.NativeField(field, tpe, evar, loc) =>
        TypedAst.Expression.NativeField(field, subst0(tpe), subst0(evar), loc)

      case ResolvedAst.Expression.NativeMethod(method, actuals, tpe, evar, loc) =>
        val es = actuals.map(e => visitExp(e, subst0))
        TypedAst.Expression.NativeMethod(method, es, subst0(tpe), subst0(evar), loc)

      case ResolvedAst.Expression.NewChannel(exp, tpe, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.NewChannel(e, mkChannel(tpe), subst0(evar), loc)

      case ResolvedAst.Expression.GetChannel(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.GetChannel(e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.PutChannel(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.SelectChannel(rules, default, tvar, evar, loc) =>
        val rs = rules map {
          case ResolvedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule(sym, c, b)
        }
        val d = default.map(visitExp(_, subst0))
        TypedAst.Expression.SelectChannel(rs, d, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ProcessSpawn(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.ProcessSpawn(e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ProcessSleep(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.ProcessSleep(e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.ProcessPanic(msg, tvar, evar, loc) =>
        TypedAst.Expression.ProcessPanic(msg, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.FixpointConstraintSet(cs0, tvar, evar, loc) =>
        val cs = cs0.map(visitConstraint)
        TypedAst.Expression.FixpointConstraintSet(cs, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.FixpointCompose(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.FixpointSolve(exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.FixpointSolve(e, Stratification.Empty, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.FixpointProject(sym, exp, tvar, evar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.FixpointProject(sym, e, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.FixpointEntails(e1, e2, subst0(tvar), subst0(evar), loc)

      case ResolvedAst.Expression.FixpointFold(sym, init, f, constraints, tvar, evar, loc) =>
        val e1 = visitExp(init, subst0)
        val e2 = visitExp(f, subst0)
        val e3 = visitExp(constraints, subst0)
        TypedAst.Expression.FixpointFold(sym, e1, e2, e3, subst0(tvar), subst0(evar), loc)
    }

    /**
      * Applies the substitution to the given constraint.
      */
    def visitConstraint(c0: ResolvedAst.Constraint): TypedAst.Constraint = {
      // Pattern match on the constraint.
      val ResolvedAst.Constraint(cparams0, head0, body0, loc) = c0

      // Unification was successful. Reassemble the head and body predicates.
      val head = reassembleHeadPredicate(head0, program, subst0)
      val body = body0.map(b => reassembleBodyPredicate(b, program, subst0))

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
  private def inferPattern(pat0: ResolvedAst.Pattern, program: ResolvedAst.Program)(implicit flix: Flix): InferMonad[Type] = {
    /**
      * Local pattern visitor.
      */
    def visit(p: ResolvedAst.Pattern): InferMonad[Type] = p match {
      case ResolvedAst.Pattern.Wild(tvar, loc) => liftM(tvar)
      case ResolvedAst.Pattern.Var(sym, tvar, loc) => unifyTypM(sym.tvar, tvar, loc)
      case ResolvedAst.Pattern.Unit(loc) => liftM(mkUnitType())
      case ResolvedAst.Pattern.True(loc) => liftM(mkBoolType())
      case ResolvedAst.Pattern.False(loc) => liftM(mkBoolType())
      case ResolvedAst.Pattern.Char(c, loc) => liftM(Type.Cst(TypeConstructor.Char))
      case ResolvedAst.Pattern.Float32(i, loc) => liftM(Type.Cst(TypeConstructor.Float32))
      case ResolvedAst.Pattern.Float64(i, loc) => liftM(Type.Cst(TypeConstructor.Float64))
      case ResolvedAst.Pattern.Int8(i, loc) => liftM(Type.Cst(TypeConstructor.Int8))
      case ResolvedAst.Pattern.Int16(i, loc) => liftM(Type.Cst(TypeConstructor.Int16))
      case ResolvedAst.Pattern.Int32(i, loc) => liftM(Type.Cst(TypeConstructor.Int32))
      case ResolvedAst.Pattern.Int64(i, loc) => liftM(Type.Cst(TypeConstructor.Int64))
      case ResolvedAst.Pattern.BigInt(i, loc) => liftM(Type.Cst(TypeConstructor.BigInt))
      case ResolvedAst.Pattern.Str(s, loc) => liftM(Type.Cst(TypeConstructor.Str))
      case ResolvedAst.Pattern.Tag(sym, tag, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = program.enums(sym)

        // Generate a fresh type variable for each type parameters.
        val subst = Substitution(decl.tparams.map {
          case param => param.tpe -> Type.freshTypeVar()
        }.toMap, Map.empty)

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

      case ResolvedAst.Pattern.Tuple(elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          resultType <- unifyTypM(tvar, Type.mkTuple(elementTypes), loc)
        ) yield resultType

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

      case ResolvedAst.Pattern.RecordEmpty(tvar, loc) => liftM(tvar)

      case ResolvedAst.Pattern.RecordExtend(sym, pat, tvar, rest, loc) =>unifyTypM(sym.tvar, tvar, loc)


    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[ResolvedAst.Pattern], program: ResolvedAst.Program)(implicit flix: Flix): InferMonad[List[Type]] = {
    seqM(pats0.map(p => inferPattern(p, program)))
  }

  /**
    * Applies the substitution `subst0` to the given pattern `pat0`.
    */
  private def reassemblePattern(pat0: ResolvedAst.Pattern, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Pattern = {
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
      case ResolvedAst.Pattern.Tuple(elms, tvar, loc) => TypedAst.Pattern.Tuple(elms map visit, subst0(tvar), loc)
      case ResolvedAst.Pattern.Array(elms, tvar, loc) => TypedAst.Pattern.Array(elms map visit, subst0(tvar), loc)
      case ResolvedAst.Pattern.ArrayTailSpread(elms, sym, tvar, loc) => TypedAst.Pattern.ArrayTailSpread(elms map visit, sym, subst0(tvar), loc)
      case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms, tvar, loc) => TypedAst.Pattern.ArrayHeadSpread(sym, elms map visit, subst0(tvar), loc)
      case ResolvedAst.Pattern.RecordEmpty(tvar, loc) => TypedAst.Pattern.RecordEmpty(subst0(tvar), loc)
      case ResolvedAst.Pattern.RecordExtend(sym, pat, tvar, rest, loc) => TypedAst.Pattern.RecordExtend(sym, visit(pat), subst0(tvar), visit(rest), loc)
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given head predicate.
    */
  private def inferHeadPredicate(head: ResolvedAst.Predicate.Head, program: ResolvedAst.Program)(implicit flix: Flix): InferMonad[Type] = head match {
    case ResolvedAst.Predicate.Head.Atom(sym, terms, tvar, loc) =>
      //
      //  t_1 : tpe_1, ..., t_n: tpe_n
      //  ------------------------------------------------------------
      //  P(t_1, ..., t_n): #{ P = P(tpe_1, ..., tpe_n) | fresh }
      //

      // Lookup the declared type of the relation/lattice (if it exists).
      val declaredType = sym match {
        case x: Symbol.RelSym => program.relations.get(x) match {
          case None => Type.freshTypeVar()
          case Some(rel) => Scheme.instantiate(rel.sc)
        }
        case x: Symbol.LatSym => program.lattices.get(x) match {
          case None => Type.freshTypeVar()
          case Some(lat) => Scheme.instantiate(lat.sc)
        }
      }

      for {
        (termTypes, termEffects) <- seqM(terms.map(inferExp(_, program))).map(_.unzip)
        pureTermEffects <- unifyEffM(Eff.Pure :: termEffects, loc)
        predicateType <- unifyTypM(tvar, getRelationOrLatticeType(sym, termTypes, program), declaredType, loc)
      } yield Type.SchemaExtend(sym, predicateType, Type.freshTypeVar())

    case ResolvedAst.Predicate.Head.Union(exp, tvar, loc) =>
      //
      //  exp : typ
      //  ------------------------------------------------------------
      //  union exp : #{ ... }
      //
      for {
        (typ, eff) <- inferExp(exp, program)
        pureEff <- unifyEffM(Eff.Pure, eff, loc)
        resultType <- unifyTypM(tvar, typ, loc)
      } yield resultType
  }

  /**
    * Applies the given substitution `subst0` to the given head predicate `head0`.
    */
  private def reassembleHeadPredicate(head0: ResolvedAst.Predicate.Head, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case ResolvedAst.Predicate.Head.Atom(sym, terms, tvar, loc) =>
      val ts = terms.map(t => reassembleExp(t, program, subst0))
      TypedAst.Predicate.Head.Atom(sym, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Head.Union(exp, tvar, loc) =>
      val e = reassembleExp(exp, program, subst0)
      TypedAst.Predicate.Head.Union(e, subst0(tvar), loc)
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: ResolvedAst.Predicate.Body, program: ResolvedAst.Program)(implicit flix: Flix): InferMonad[Type] = body0 match {
    //
    //  t_1 : tpe_1, ..., t_n: tpe_n
    //  ------------------------------------------------------------
    //  P(t_1, ..., t_n): Schema{ P = P(tpe_1, ..., tpe_n) | fresh }
    //
    case ResolvedAst.Predicate.Body.Atom(sym, polarity, terms, tvar, loc) =>

      // Lookup the declared type of the relation/lattice (if it exists).
      val declaredType = sym match {
        case x: Symbol.RelSym => program.relations.get(x) match {
          case None => Type.freshTypeVar()
          case Some(rel) => Scheme.instantiate(rel.sc)
        }
        case x: Symbol.LatSym => program.lattices.get(x) match {
          case None => Type.freshTypeVar()
          case Some(lat) => Scheme.instantiate(lat.sc)
        }
      }

      for {
        termTypes <- seqM(terms.map(inferPattern(_, program)))
        predicateType <- unifyTypM(tvar, getRelationOrLatticeType(sym, termTypes, program), declaredType, loc)
      } yield Type.SchemaExtend(sym, predicateType, Type.freshTypeVar())

    //
    //  exp : Bool
    //  ----------
    //  if exp : a
    //
    case ResolvedAst.Predicate.Body.Guard(exp, loc) =>
      // Infer the types of the terms.
      for {
        (tpe, eff) <- inferExp(exp, program)
        expEff <- unifyEffM(Eff.Pure, eff, loc)
        expTyp <- unifyTypM(mkBoolType(), tpe, loc)
      } yield mkAnySchemaType()
  }

  /**
    * Applies the given substitution `subst0` to the given body predicate `body0`.
    */
  private def reassembleBodyPredicate(body0: ResolvedAst.Predicate.Body, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case ResolvedAst.Predicate.Body.Atom(sym, polarity, terms, tvar, loc) =>
      val ts = terms.map(t => reassemblePattern(t, program, subst0))
      TypedAst.Predicate.Body.Atom(sym, polarity, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Body.Guard(exp, loc) =>
      val e = reassembleExp(exp, program, subst0)
      TypedAst.Predicate.Body.Guard(e, loc)
  }

  /**
    * Returns the relation or lattice type of `sym` with the term types `ts`.
    */
  private def getRelationOrLatticeType(sym: Symbol.PredSym, ts: List[Type], program: ResolvedAst.Program)(implicit flix: Flix): Type = {
    sym match {
      case sym: Symbol.RelSym =>
        val base = Type.Cst(TypeConstructor.Relation(sym)): Type
        val args = Type.mkTuple(ts)
        Type.Apply(base, args)

      case sym: Symbol.LatSym =>
        val base = Type.Cst(TypeConstructor.Lattice(sym)): Type
        val args = Type.mkTuple(ts)
        Type.Apply(base, args)
    }
  }

  /**
    * Performs type resolution on the given attribute `attr`.
    */
  private def typeCheckAttribute(attr: ResolvedAst.Attribute): Result[TypedAst.Attribute, TypeError] = attr match {
    case ResolvedAst.Attribute(ident, tpe, loc) => Ok(TypedAst.Attribute(ident.name, tpe, loc))
  }

  /**
    * Returns the declared types of the terms of the given relation symbol `sym`.
    */
  private def getRelationSignature(sym: Symbol.RelSym, program: ResolvedAst.Program): Result[List[Type], TypeError] = {
    program.relations(sym) match {
      case ResolvedAst.Relation(_, _, _, _, attr, _, _) => Ok(attr.map(_.tpe))
    }
  }

  /**
    * Returns the declared types of the terms of the given lattice symbol `sym`.
    */
  private def getLatticeSignature(sym: Symbol.LatSym, program: ResolvedAst.Program): Result[List[Type], TypeError] = {
    program.lattices(sym) match {
      case ResolvedAst.Lattice(_, _, _, _, attr, _, _) => Ok(attr.map(_.tpe))
    }
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    */
  private def getSubstFromParams(params: List[ResolvedAst.FormalParam])(implicit flix: Flix): Unification.Substitution = {
    // Compute the substitution by mapping the symbol of each parameter to its declared type.
    val declaredTypes = params.map(_.tpe)
    (params zip declaredTypes).foldLeft(Substitution.empty) {
      case (macc, (ResolvedAst.FormalParam(sym, _, _, _), declaredType)) =>
        macc ++ Substitution.singleton(sym.tvar, openSchemaType(declaredType))
    }
  }

  /**
    * Opens the given schema type `tpe0`.
    */
  // TODO: Open nested schema types?
  // TODO: Replace by a more robust solution once we check type signatures more correctly.
  def openSchemaType(tpe0: Type)(implicit flix: Flix): Type = tpe0 match {
    case Type.SchemaEmpty => Type.freshTypeVar()
    case Type.SchemaExtend(sym, tpe, rest) => Type.SchemaExtend(sym, tpe, openSchemaType(rest))
    case Type.Apply(tpe1, tpe2) => Type.Apply(openSchemaType(tpe1), openSchemaType(tpe2))
    case _ => tpe0
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
  private def getFormalParams(fparams0: List[ResolvedAst.FormalParam], subst0: Unification.Substitution): List[TypedAst.FormalParam] = fparams0.map {
    case ResolvedAst.FormalParam(sym, mod, tpe, loc) => TypedAst.FormalParam(sym, mod, subst0(sym.tvar), sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaType()(implicit flix: Flix): Type = Type.freshTypeVar()

  /**
    * Returns the Flix Type of a Java Type
    */
  private def getGenericFlixType(t: java.lang.reflect.Type)(implicit flix: Flix): Type = {
    t match {
      case arrayType: java.lang.reflect.GenericArrayType =>
        val comp = arrayType.getGenericComponentType
        val elmType = getGenericFlixType(comp)
        mkArray(elmType)
      case c: Class[_] =>
        getFlixType(c)
      case _ =>
        // TODO: Can we do better than this for Parametric Types?
        Type.freshTypeVar()
    }
  }

  /**
    * Returns the Flix Type of a Java Class
    */
  private def getFlixType(c: Class[_]): Type = {
    // handle primitive types first
    if (c == java.lang.Boolean.TYPE) {
      mkBoolType()
    }
    else if (c == java.lang.Byte.TYPE) {
      Type.Cst(TypeConstructor.Int8)
    }
    else if (c == java.lang.Short.TYPE) {
      Type.Cst(TypeConstructor.Int16)
    }
    else if (c == java.lang.Integer.TYPE) {
      Type.Cst(TypeConstructor.Int32)
    }
    else if (c == java.lang.Long.TYPE) {
      Type.Cst(TypeConstructor.Int64)
    }
    else if (c == java.lang.Character.TYPE) {
      Type.Cst(TypeConstructor.Char)
    }
    else if (c == java.lang.Float.TYPE) {
      Type.Cst(TypeConstructor.Float32)
    }
    else if (c == java.lang.Double.TYPE) {
      Type.Cst(TypeConstructor.Float64)
    }
    else if (c == classOf[java.lang.String]) {
      Type.Cst(TypeConstructor.Str)
    }
    else if (c == java.lang.Void.TYPE) {
      mkUnitType()
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
    * Returns the Unit type.
    */
  private def mkUnitType(): Type = Type.Cst(TypeConstructor.Unit)

  /**
    * Returns the Bool type.
    */
  private def mkBoolType(): Type = Type.Cst(TypeConstructor.Bool)

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
    * Returns the type `Vector[tpe, len]`.
    */
  private def mkVector(tpe: Type, len: Type): Type = Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Vector), tpe), len)

}
