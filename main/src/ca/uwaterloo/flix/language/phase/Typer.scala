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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.Unification._
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result, Validation}

object Typer extends Phase[ResolvedAst.Program, TypedAst.Root] {

  /**
    * Type checks the given program.
    */
  def run(program: ResolvedAst.Program)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Typer") {
    implicit val _ = flix.genSym

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
      TypedAst.Root(defs, effs, handlers, enums, relations, lattices, latticeComponents, properties, specialOps, program.reachable)
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
    // Put genSym into implicit scope.
    implicit val genSym = flix.genSym

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
  private def typeEnums(program: ResolvedAst.Program)(implicit genSym: GenSym): Result[Map[Symbol.EnumSym, TypedAst.Enum], TypeError] = {
    /**
      * Performs type resolution on the given enum and its cases.
      */
    def visitEnum(enum: ResolvedAst.Enum): Result[(Symbol.EnumSym, TypedAst.Enum), TypeError] = enum match {
      case ResolvedAst.Enum(doc, mod, enumSym, tparams, cases0, tpe, loc) =>
        val cases = cases0 map {
          case (name, ResolvedAst.Case(_, tagName, tagType)) =>
            name -> TypedAst.Case(enumSym, tagName, tagType, tagName.loc)
        }

        Ok(enumSym -> TypedAst.Enum(doc, mod, enumSym, cases, enum.tpe, loc))
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
  private def typeLatticeComponents(program: ResolvedAst.Program)(implicit genSym: GenSym): Result[Map[Type, TypedAst.LatticeComponents], TypeError] = {

    /**
      * Performs type inference and reassembly on the given `lattice`.
      */
    def visitLattice(lattice: ResolvedAst.LatticeComponents): Result[(Type, TypedAst.LatticeComponents), TypeError] = lattice match {
      case ResolvedAst.LatticeComponents(tpe, e1, e2, e3, e4, e5, e6, ns, loc) =>
        // Perform type resolution on the declared type.
        val declaredType = lattice.tpe

        // Perform type inference on each of the lattice components.
        val m = for {
          botType <- inferExp(e1, program)
          topType <- inferExp(e2, program)
          equType <- inferExp(e3, program)
          leqType <- inferExp(e4, program)
          lubType <- inferExp(e5, program)
          glbType <- inferExp(e6, program)
          _______ <- unifyM(botType, declaredType, loc)
          _______ <- unifyM(topType, declaredType, loc)
          _______ <- unifyM(equType, Type.mkArrow(List(declaredType, declaredType), Type.Cst(TypeConstructor.Bool)), loc)
          _______ <- unifyM(leqType, Type.mkArrow(List(declaredType, declaredType), Type.Cst(TypeConstructor.Bool)), loc)
          _______ <- unifyM(lubType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
          _______ <- unifyM(glbType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
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
  private def typeProperties(prog0: ResolvedAst.Program)(implicit genSym: GenSym): Result[List[TypedAst.Property], TypeError] = {

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
  private def typeCheckDef(defn0: ResolvedAst.Def, program: ResolvedAst.Program)(implicit genSym: GenSym): Result[TypedAst.Def, TypeError] = {
    // Resolve the declared scheme.
    val declaredScheme = defn0.sc

    // TODO: Some duplication
    val argumentTypes = defn0.fparams.map(_.tpe)

    val result = for (
      resultType <- inferExp(defn0.exp, program);
      unifiedType <- unifyM(Scheme.instantiate(declaredScheme), Type.mkArrow(argumentTypes, resultType), defn0.loc)
    ) yield unifiedType

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
      implicit val genSym: GenSym = flix.genSym

      val eff = program0.effs(sym)
      val effectType = Scheme.instantiate(eff.sc)(flix.genSym)

      val declaredType = Scheme.instantiate(sc)(flix.genSym)

      val subst0 = getSubstFromParams(fparams0)
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst0)
      val argumentTypes = fparams.map(_.tpe)

      val result = for {
        actualType <- inferExp(exp0, program0)(flix.genSym)
        unifiedType <- unifyM(declaredType, effectType, Type.mkArrow(argumentTypes, actualType), loc)
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
      val tpe = Scheme.instantiate(sc)(flix.genSym)

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
    case ResolvedAst.Relation(doc, mod, sym, tparams, attr, sc, loc) =>
      for {
        typedAttributes <- Result.sequence(attr.map(a => typeCheckAttribute(a)))
      } yield sym -> TypedAst.Relation(doc, mod, sym, typedAttributes, loc)
  }

  /**
    * Performs type resolution on the given lattice `l`.
    *
    * Returns [[Err]] if a type is unresolved.
    */
  private def typeCheckLat(r: ResolvedAst.Lattice): Result[(Symbol.LatSym, TypedAst.Lattice), TypeError] = r match {
    case ResolvedAst.Lattice(doc, mod, sym, tparams, attr, sc, loc) =>
      for {
        typedAttributes <- Result.sequence(attr.map(a => typeCheckAttribute(a)))
      } yield sym -> TypedAst.Lattice(doc, mod, sym, typedAttributes, loc)
  }

  /**
    * Infers the type of the given expression `exp0`.
    */
  private def inferExp(exp0: ResolvedAst.Expression, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: ResolvedAst.Expression): InferMonad[Type] = e0 match {

      /*
       * Wildcard expression.
       */
      case ResolvedAst.Expression.Wild(tpe, loc) => liftM(tpe)

      /*
       * Variable expression.
       */
      case ResolvedAst.Expression.Var(sym, tpe, loc) => unifyM(sym.tvar, tpe, loc)

      /*
       * Def expression.
       */
      case ResolvedAst.Expression.Def(sym, tvar, loc) =>
        val defn = program.defs(sym)
        unifyM(tvar, Scheme.instantiate(defn.sc), loc)

      /*
       * Eff expression.
       */
      case ResolvedAst.Expression.Eff(sym, tvar, loc) =>
        val eff = program.effs(sym)
        unifyM(tvar, Scheme.instantiate(eff.sc), loc)

      /*
       * Sig expression.
       */
      case ResolvedAst.Expression.Sig(sym, tvar, loc) =>
        val sig = program.classes(sym.clazz)
        ??? // TODO

      /*
       * Hole expression.
       */
      case ResolvedAst.Expression.Hole(sym, tpe, loc) =>
        liftM(tpe)

      /*
       * Literal expression.
       */
      case ResolvedAst.Expression.Unit(loc) => liftM(Type.Cst(TypeConstructor.Unit))
      case ResolvedAst.Expression.True(loc) => liftM(Type.Cst(TypeConstructor.Bool))
      case ResolvedAst.Expression.False(loc) => liftM(Type.Cst(TypeConstructor.Bool))
      case ResolvedAst.Expression.Char(lit, loc) => liftM(Type.Cst(TypeConstructor.Char))
      case ResolvedAst.Expression.Float32(lit, loc) => liftM(Type.Cst(TypeConstructor.Float32))
      case ResolvedAst.Expression.Float64(lit, loc) => liftM(Type.Cst(TypeConstructor.Float64))
      case ResolvedAst.Expression.Int8(lit, loc) => liftM(Type.Cst(TypeConstructor.Int8))
      case ResolvedAst.Expression.Int16(lit, loc) => liftM(Type.Cst(TypeConstructor.Int16))
      case ResolvedAst.Expression.Int32(lit, loc) => liftM(Type.Cst(TypeConstructor.Int32))
      case ResolvedAst.Expression.Int64(lit, loc) => liftM(Type.Cst(TypeConstructor.Int64))
      case ResolvedAst.Expression.BigInt(lit, loc) => liftM(Type.Cst(TypeConstructor.BigInt))
      case ResolvedAst.Expression.Str(lit, loc) => liftM(Type.Cst(TypeConstructor.Str))

      /*
       * Lambda expression.
       */
      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val argumentType = fparam.tpe
        for (
          inferredExpType <- visitExp(exp);
          resultType <- unifyM(tvar, Type.mkArrow(argumentType, inferredExpType), loc)
        ) yield resultType

      /*
       * Apply expression.
       */
      case ResolvedAst.Expression.Apply(exp1, exp2, tvar, loc) =>
        val freshResultType = Type.freshTypeVar()
        for (
          inferredLambdaType <- visitExp(exp1);
          inferredArgumentType <- visitExp(exp2);
          expectedLambdaType <- unifyM(inferredLambdaType, Type.mkArrow(inferredArgumentType, freshResultType), loc);
          resultType <- unifyM(freshResultType, tvar, loc)
        ) yield resultType

      /*
       * Unary expression.
       */
      case ResolvedAst.Expression.Unary(op, exp1, tvar, loc) => op match {
        case UnaryOperator.LogicalNot =>
          for (
            tpe <- visitExp(exp1);
            res <- unifyM(tvar, tpe, Type.Cst(TypeConstructor.Bool), loc)
          ) yield res

        case UnaryOperator.Plus =>
          for (
            tpe <- visitExp(exp1);
            res <- unifyM(tvar, tpe, loc)
          ) yield res

        case UnaryOperator.Minus =>
          for (
            tpe <- visitExp(exp1);
            res <- unifyM(tvar, tpe, loc)
          ) yield res

        case UnaryOperator.BitwiseNegate =>
          for (
            tpe <- visitExp(exp1);
            res <- unifyM(tvar, tpe, loc)
          ) yield res
      }

      /*
       * Binary expression.
       */
      case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, loc) => op match {
        case BinaryOperator.Plus =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.Minus =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.Times =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.Divide =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.Modulo =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.Exponentiate =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.Equal | BinaryOperator.NotEqual =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            ____ <- unifyM(tpe1, tpe2, loc);
            resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Bool), loc)
          ) yield resultType

        case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            ____ <- unifyM(tpe1, tpe2, loc);
            resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Bool), loc)
          ) yield resultType

        case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, Type.Cst(TypeConstructor.Bool), loc)
          ) yield resultType

        case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            resultType <- unifyM(tvar, tpe1, tpe2, loc)
          ) yield resultType

        case BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            lhsType <- unifyM(tvar, tpe1, loc);
            rhsType <- unifyM(tpe2, Type.Cst(TypeConstructor.Int32), loc)
          ) yield lhsType
      }

      /*
       * Let expression.
       */
      case ResolvedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
        for (
          tpe1 <- visitExp(exp1);
          tpe2 <- visitExp(exp2);
          boundVar <- unifyM(sym.tvar, tpe1, loc);
          resultVar <- unifyM(tvar, tpe2, loc)
        ) yield resultVar

      /*
       * LetRec expression.
       */
      case ResolvedAst.Expression.LetRec(sym, exp1, exp2, tvar, loc) =>
        // TODO: Require boundVar to be a function?
        for (
          tpe1 <- visitExp(exp1);
          tpe2 <- visitExp(exp2);
          boundVar <- unifyM(sym.tvar, tpe1, loc);
          resultVar <- unifyM(tvar, tpe2, loc)
        ) yield resultVar

      /*
       * If-then-else expression.
       */
      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
        for (
          tpe1 <- visitExp(exp1);
          tpe2 <- visitExp(exp2);
          tpe3 <- visitExp(exp3);
          ____ <- unifyM(Type.Cst(TypeConstructor.Bool), tpe1, loc);
          rtpe <- unifyM(tvar, tpe2, tpe3, loc)
        ) yield rtpe

      /*
       * Match expression.
       */
      case ResolvedAst.Expression.Match(exp1, rules, tvar, loc) =>
        assert(rules.nonEmpty)
        // Extract the patterns, guards, and body expressions of each rule.
        val patterns = rules.map(_.pat)
        val guards = rules.map(_.guard)
        val bodies = rules.map(_.exp)

        for {
          matchType <- visitExp(exp1)
          patternTypes <- inferPatterns(patterns, program)
          patternType <- unifyM(patternTypes, loc)
          ___________ <- unifyM(matchType, patternType, loc)
          guardTypes <- seqM(guards map visitExp)
          guardType <- unifyM(Type.Cst(TypeConstructor.Bool) :: guardTypes, loc)
          actualBodyTypes <- seqM(bodies map visitExp)
          resultType <- unifyM(tvar :: actualBodyTypes, loc)
        } yield resultType

      /*
         * Switch expression.
         */
      case ResolvedAst.Expression.Switch(rules, tvar, loc) =>
        assert(rules.nonEmpty)
        val condExps = rules.map(_._1)
        val bodyExps = rules.map(_._2)
        for (
          actualCondTypes <- seqM(condExps map visitExp);
          actualBodyTypes <- seqM(bodyExps map visitExp);
          unifiedCondTypes <- unifyM(Type.Cst(TypeConstructor.Bool) :: actualCondTypes, loc);
          unifiedBodyType <- unifyM(actualBodyTypes, loc);
          resultType <- unifyM(tvar, unifiedBodyType, loc)
        ) yield resultType

      /*
       * Tag expression.
       */
      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = program.enums(sym)

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
          innerType <- visitExp(exp);
          _________ <- unifyM(innerType, freshCaseType, loc);
          resultType <- unifyM(tvar, freshEnumType, loc)
        ) yield resultType

      /*
       * Tuple expression.
       */
      case ResolvedAst.Expression.Tuple(elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms.map(visitExp));
          resultType <- unifyM(tvar, Type.mkTuple(elementTypes), loc)
        ) yield resultType

      /*
       * RecordEmpty expression.
       */
      case ResolvedAst.Expression.RecordEmpty(tvar, loc) =>
        //
        //  ------------
        //  %{ } : % { }
        //
        unifyM(tvar, Type.RecordEmpty, loc)

      /*
       * RecordSelect expression.
       */
      case ResolvedAst.Expression.RecordSelect(exp, label, tvar, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        // r.label : tpe
        //
        val freshRowVar = Type.freshTypeVar()
        val expectedType = Type.RecordExtend(label, tvar, freshRowVar)
        for {
          actualType <- visitExp(exp)
          recordType <- unifyM(actualType, expectedType, loc)
        } yield tvar

      /*
       * RecordExtend expression.
       */
      case ResolvedAst.Expression.RecordExtend(label, value, rest, tvar, loc) =>
        //
        // value : tpe
        // -------------------------------------------
        // { label = value | r } : { label : tpe | r }
        //
        for {
          valueType <- visitExp(value)
          restType <- visitExp(rest)
          resultType <- unifyM(tvar, Type.RecordExtend(label, valueType, restType), loc)
        } yield resultType

      /*
       * RecordRestrict expression.
       */
      case ResolvedAst.Expression.RecordRestrict(label, rest, tvar, loc) =>
        //
        // ----------------------
        // { -label | r } : { r }
        //
        val freshFieldType = Type.freshTypeVar()
        val freshRowVar = Type.freshTypeVar()
        for {
          restType <- visitExp(rest)
          recordType <- unifyM(restType, Type.RecordExtend(label, freshFieldType, freshRowVar), loc)
          resultType <- unifyM(tvar, freshRowVar, loc)
        } yield resultType

      /*
       * ArrayLit expression.
       */
      case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
        //
        //  e1 : t ... en: t
        //  ------------------------
        //  [e1,...,en] : Array[t]
        //
        if (elms.isEmpty) {
          for (
            resultType <- unifyM(tvar, Type.mkArray(Type.freshTypeVar()), loc)
          ) yield resultType
        }
        else {
          for (
            elementsTypes <- seqM(elms.map(visitExp));
            elementType <- unifyM(elementsTypes, loc);
            resultType <- unifyM(tvar, Type.mkArray(elementType), loc)
          ) yield resultType
        }

      /*
       * ArrayNew expression.
       */
      case ResolvedAst.Expression.ArrayNew(elm, len, tvar, loc) =>
        //
        //  elm : t      len: Int
        //  ------------------------
        //  [elm ; len] : Array[t]
        //
        for (
          actualElementType <- visitExp(elm);
          actualLengthType <- visitExp(len);
          lengthType <- unifyM(actualLengthType, Type.Cst(TypeConstructor.Int32), loc);
          resultType <- unifyM(tvar, Type.mkArray(actualElementType), loc)
        ) yield resultType

      /*
       * ArrayLoad expression.
       */
      case ResolvedAst.Expression.ArrayLoad(base, index, tvar, loc) =>
        //
        //  base : Array[t]   index: Int
        //  ------------------------
        //  base[index] : t
        //
        for (
          actualBaseType <- visitExp(base);
          actualIndexType <- visitExp(index);
          arrayType <- unifyM(actualBaseType, Type.mkArray(tvar), loc);
          indexType <- unifyM(actualIndexType, Type.Cst(TypeConstructor.Int32), loc)
        ) yield tvar

      /*
       * ArrayLength expression.
       */
      case ResolvedAst.Expression.ArrayLength(base, tvar, loc) =>
        //
        //  base : Array[t]
        //  ------------------------
        //  length[base] : Int
        //
        for (
          baseType <- visitExp(base);
          arrayType <- unifyM(baseType, Type.mkArray(tvar), loc);
          resultType <- unifyM(Type.freshTypeVar(), Type.Cst(TypeConstructor.Int32), loc)
        ) yield resultType

      /*
       * ArrayStore expression.
       */
      case ResolvedAst.Expression.ArrayStore(base, index, elm, tvar, loc) =>
        //
        //  base : Array[t]   index : Int   elm : t
        //  -----------------------------------------
        //  base[index] = elm : Unit
        //
        val elementType = Type.freshTypeVar()
        for (
          actualBaseType <- visitExp(base);
          actualIndexType <- visitExp(index);
          actualElementType <- visitExp(elm);
          arrayType <- unifyM(actualBaseType, Type.mkArray(elementType), loc);
          indexType <- unifyM(actualIndexType, Type.Cst(TypeConstructor.Int32), loc);
          elementType <- unifyM(actualElementType, elementType, loc);
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Unit), loc)
        ) yield resultType

      /*
       * ArraySlice expression.
       */
      case ResolvedAst.Expression.ArraySlice(base, startIndex, endIndex, tvar, loc) =>
        //
        //  base : Array[t]   startIndex : Int   endIndex : Int
        //  -----------------------------------------
        //  base[startIndex..EndIndex] : Array[t]
        //
        for (
          actualBaseType <- visitExp(base);
          actualStartIndexType <- visitExp(startIndex);
          actualEndIndexType <- visitExp(endIndex);
          startIndexType <- unifyM(actualStartIndexType, Type.Cst(TypeConstructor.Int32), loc);
          endIndexType <- unifyM(actualEndIndexType, Type.Cst(TypeConstructor.Int32), loc);
          resultType <- unifyM(tvar, actualBaseType, loc)
        ) yield resultType

      case ResolvedAst.Expression.VectorLit(elms, tvar, loc) =>
        //
        // elm1: t ...  elm_len: t  len: Succ(n, Zero)
        // ---------------------------
        // [|elm1,...,elm_len|] : Vector[t, len]
        //
        if (elms.isEmpty) {
          for (
            resultType <- unifyM(tvar, Type.mkVector(Type.freshTypeVar(), Type.Succ(0, Type.Zero)), loc)
          ) yield resultType
        }
        else {
          for (
            elementTypes <- seqM(elms.map(visitExp));
            elementType <- unifyM(elementTypes, loc);
            resultType <- unifyM(tvar, Type.mkVector(elementType, Type.Succ(elms.length, Type.Zero)), loc)
          ) yield resultType
        }

      case ResolvedAst.Expression.VectorNew(elm, len, tvar, loc) =>
        //
        // elm: t    len: Succ(n, Zero)
        // --------------------------
        // [|elm ; len |] : Vector[t, len]
        //

        for (
          tpe <- visitExp(elm);
          resultType <- unifyM(tvar, Type.mkVector(tpe, Type.Succ(len, Type.Zero)), loc)
        ) yield resultType

      case ResolvedAst.Expression.VectorLoad(base, index, tvar, loc) =>
        //
        //  base : Vector[t, len1]   index: len2   len1: Succ(n1, Zero) len2: Succ(n2, Var)
        //  -------------------------------------------------------------------------------
        //  base[|index|] : t
        //
        val freshElementVar = Type.freshTypeVar()
        for (
          baseType <- visitExp(base);
          resultType <- unifyM(baseType, Type.mkVector(tvar, Type.Succ(index, freshElementVar)), loc)
        ) yield tvar

      case ResolvedAst.Expression.VectorStore(base, index, elm, tvar, loc) =>
        //
        //  base : Vector[t, len1]   index: len2   elm: t    len1: Succ(n1, Zero)  len2: Succ(n2, Var)
        //  ------------------------------------------------------------------------------------------
        //  base[|index|] = elm : Unit
        //
        val freshElementVar = Type.freshTypeVar()
        val elmVar = Type.freshTypeVar()
        for (
          baseType <- visitExp(base);
          recievedObjectType <- visitExp(elm);
          vectorType <- unifyM(baseType, Type.mkVector(elmVar, Type.Succ(index, freshElementVar)), loc);
          objectType <- unifyM(recievedObjectType, elmVar, loc);
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Unit), loc)
        ) yield resultType

      case ResolvedAst.Expression.VectorLength(base, tvar, loc) =>
        //
        // base: Vector[t, len1]   index: len2   len1: Succ(n1, Zero)  len2: Succ(n2, Var)
        // ----------------------------------------------------------------------------------
        // base[|index|] : Int
        //
        val freshResultType = Type.freshTypeVar()
        val freshLengthVar = Type.freshTypeVar()
        for (
          baseType <- visitExp(base);
          _ <- unifyM(baseType, Type.mkVector(freshResultType, Type.Succ(0, freshLengthVar)), loc);
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Int32), loc)
        ) yield resultType

      case ResolvedAst.Expression.VectorSlice(base, startIndex, optEndIndex, tvar, loc) =>
        //
        //  Case None =
        //  base : Vector[t, len2]   startIndex : len3
        //  len1 : Succ(n1, Zero) len2 : Succ(n2, Zero) len3 : Succ(n3, Var)
        //  --------------------------------------------------------------------------------------
        //  base[|startIndex.. |] : Vector[t, len1]
        //
        //
        //  Case Some =
        //  base : Vector[t, len2]   startIndex : len3   endIndexOpt : len4
        //  len1 : Succ(n1, Zero) len2 : Succ(n2, Zero) len3 : Succ(n3, Var) len 4 : Succ(n4, Var)
        //  --------------------------------------------------------------------------------------
        //  base[startIndex..endIndexOpt] : Vector[t, len1]
        //
        val freshEndIndex = Type.freshTypeVar()
        val freshBeginIndex = Type.freshTypeVar()
        val freshElmType = Type.freshTypeVar()
        optEndIndex match {
          case None =>
            for (
              baseType <- visitExp(base);
              firstIndex <- unifyM(baseType, Type.mkVector(freshElmType, Type.Succ(startIndex, freshEndIndex)), loc);
              resultType <- unifyM(tvar, Type.mkVector(freshElmType, freshEndIndex), loc)
            ) yield resultType
          case Some(endIndex) =>
            for (
              baseType <- visitExp(base);
              firstIndex <- unifyM(baseType, Type.mkVector(freshElmType, Type.Succ(startIndex, freshBeginIndex)), loc);
              secondIndex <- unifyM(baseType, Type.mkVector(freshElmType, Type.Succ(endIndex, freshEndIndex)), loc);
              resultType <- unifyM(tvar, Type.mkVector(freshElmType, Type.Succ(endIndex - startIndex, Type.Zero)), loc)
            ) yield resultType
        }

      /*
       * Reference expression.
       */
      case ResolvedAst.Expression.Ref(exp, tvar, loc) =>
        //
        //  exp : t
        //  ----------------
        //  ref exp : Ref[t]
        //
        for {
          tpe <- visitExp(exp)
          resultType <- unifyM(tvar, Type.Apply(Type.Cst(TypeConstructor.Ref), tpe), loc)
        } yield resultType

      /*
       * Dereference expression.
       */
      case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
        //
        //  exp : Ref[t]
        //  -------------
        //  deref exp : t
        //
        for {
          tpe <- visitExp(exp)
          _ <- unifyM(tpe, Type.Apply(Type.Cst(TypeConstructor.Ref), tvar), loc)
        } yield tvar

      /*
       * Assignment expression.
       */
      case ResolvedAst.Expression.Assign(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : Ref[t]    exp2: t
        //  ------------------------
        //  exp1 := exp2 : Unit
        //
        for {
          tpe1 <- visitExp(exp1)
          tpe2 <- visitExp(exp2)
          _ <- unifyM(tpe1, Type.Apply(Type.Cst(TypeConstructor.Ref), tpe2), loc)
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Unit), loc)
        } yield resultType

      /*
       * HandleWith expression.
       */
      case ResolvedAst.Expression.HandleWith(exp, bindings, tvar, loc) =>
        // TODO: Need to check that the return types are consistent.

        // Typecheck each handler binding.
        val bs = bindings map {
          case ResolvedAst.HandlerBinding(sym, handler) =>
            val eff = program.effs(sym)
            val declaredType = Scheme.instantiate(eff.sc)
            for {
              actualType <- visitExp(handler)
            } yield unifyM(declaredType, actualType, loc)
        }

        // Typecheck the expression.
        for {
          tpe <- visitExp(exp)
          handlers <- seqM(bs)
          resultType <- unifyM(tvar, tpe, loc)
        } yield resultType

      /*
       * Existential expression.
       */
      case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
        // NB: An existential behaves very much like a lambda.
        // TODO: Must check the type of the param.
        for {
          bodyType <- visitExp(exp)
          resultType <- unifyM(bodyType, Type.Cst(TypeConstructor.Bool), loc)
        } yield resultType

      /*
       * Universal expression.
       */
      case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
        // NB: An existential behaves very much like a lambda.
        // TODO: Must check the type of the param.
        for {
          bodyType <- visitExp(exp)
          resultType <- unifyM(bodyType, Type.Cst(TypeConstructor.Bool), loc)
        } yield resultType

      /*
       * Ascribe expression.
       */
      case ResolvedAst.Expression.Ascribe(exp, expectedType, eff, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          actualType <- visitExp(exp)
          resultType <- unifyM(actualType, expectedType, loc)
        } yield resultType

      /*
       * Cast expression.
       */
      case ResolvedAst.Expression.Cast(exp, declaredType, eff, loc) =>
        // An cast expression is unsound; the type system assumes the declared type is correct.
        for {
          actualType <- visitExp(exp)
        } yield declaredType

      /*
       * Try Catch
       */
      case ResolvedAst.Expression.TryCatch(exp, rules, tvar, loc) =>
        val rulesType = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          expType <- visitExp(exp)
          ruleTypes <- seqM(rulesType)
          ruleType <- unifyM(ruleTypes, loc)
          resultType <- unifyM(tvar, expType, ruleType, loc)
        } yield resultType

      /*
       * Native Constructor expression.
       */
      case ResolvedAst.Expression.NativeConstructor(constructor, actuals, tvar, loc) =>
        // TODO: Check types.
        val clazz = constructor.getDeclaringClass
        for {
          inferredArgumentTypes <- seqM(actuals.map(visitExp))
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Native(clazz)), loc)
        } yield resultType

      /*
       * Native Field expression.
       */
      case ResolvedAst.Expression.NativeField(field, tvar, loc) =>
        // TODO: Check types.
        liftM(tvar)

      /*
       * Native Method expression.
       */
      case ResolvedAst.Expression.NativeMethod(method, actuals, tvar, loc) =>
        // TODO: Check argument types.
        val returnType = getGenericFlixType(method.getGenericReturnType)
        for {
          inferredArgumentTypes <- seqM(actuals.map(visitExp))
          resultType <- unifyM(tvar, returnType, loc)
        } yield resultType

      /*
     * New Channel expression.
     */
      case ResolvedAst.Expression.NewChannel(exp, tpe, loc) =>
        //
        //  exp: Int
        //  --------------------
        //  newch t exp : Channel[t]
        //
        for {
          actualLengthType <- visitExp(exp)
          lengthType <- unifyM(actualLengthType, Type.Cst(TypeConstructor.Int32), loc)
          resultType <- liftM(Type.mkChannel(tpe))
        } yield resultType

      /*
       * Get Channel expression.
       */
      case ResolvedAst.Expression.GetChannel(exp, tvar, loc) =>
        //
        //  exp: Channel[tpe]
        //  -----------------
        //  <- exp : tpe
        //
        for {
          channelType <- visitExp(exp)
          _ <- unifyM(channelType, Type.mkChannel(tvar), loc)
        } yield tvar

      /*
       * Put Channel expression.
       */
      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, loc) =>
        //
        //  exp1: Channel[t], exp2: t
        //  -------------------------
        //  exp1 <- exp2 : Channel[t]
        //
        val freshElementTypeVar = Type.freshTypeVar()
        for {
          channelType <- visitExp(exp1)
          elementType <- visitExp(exp2)
          _ <- unifyM(channelType, Type.mkChannel(freshElementTypeVar), loc)
          _ <- unifyM(elementType, freshElementTypeVar, loc)
          resultType <- unifyM(tvar, Type.mkChannel(freshElementTypeVar), loc)
        } yield resultType

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
        assert(rules.nonEmpty)
        val bodies = rules.map(_.exp)

        // check that each rules channel expression is a channel
        def inferSelectChannelRule(rule: ResolvedAst.SelectChannelRule): InferMonad[Unit] = {
          rule match {
            case ResolvedAst.SelectChannelRule(sym, chan, exp) => for {
              channelType <- visitExp(chan)
              _ <- unifyM(channelType, Type.mkChannel(Type.freshTypeVar()), loc)
            } yield liftM(Type.Cst(TypeConstructor.Unit))
          }
        }

        // check that default case has same type as bodies (the same as result type)
        def inferSelectChannelDefault(rtpe: Type, defaultCase: Option[ResolvedAst.Expression]): InferMonad[Unit] = {
          defaultCase match {
            case Some(exp) =>
              for {
                tpe <- visitExp(exp)
                _ <- unifyM(rtpe, tpe, loc)
              } yield liftM(Type.Cst(TypeConstructor.Unit))
            case None => liftM(Type.Cst(TypeConstructor.Unit))
          }
        }

        for {
          _ <- seqM(rules.map(inferSelectChannelRule))
          bodyTypes <- seqM(bodies map visitExp)
          actualResultType <- unifyM(bodyTypes, loc)
          _ <- inferSelectChannelDefault(actualResultType, default)
          resultType <- unifyM(tvar, actualResultType, loc)
        } yield resultType

      /*
       * Spawn Expression.
       */
      case ResolvedAst.Expression.Spawn(exp, tvar, loc) =>
        //
        //  exp: t
        //  ------------------
        //  spawn exp : Unit
        //
        for {
          e <- visitExp(exp)
          _ <- unifyM(e, Type.freshTypeVar(), loc)
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Unit), loc)
        } yield resultType

      /*
       * Sleep expression.
       */
      case ResolvedAst.Expression.Sleep(exp, tvar, loc) =>
        //
        // exp: Int
        // ------------------
        // sleep exp : Unit
        //
        for {
          e <- visitExp(exp)
          _ <- unifyM(e, Type.Cst(TypeConstructor.Int64), loc)
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Unit), loc)
        } yield resultType

      /*
       * Constraint expression.
       */
      case ResolvedAst.Expression.FixpointConstraint(cons, tvar, loc) =>
        val ResolvedAst.Constraint(cparams, head0, body0, loc) = cons
        //
        //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
        //  -----------------------------------
        //  A_0 :- A_1, ..., A_n : tpe
        //
        for {
          headPredicateType <- inferHeadPredicate(head0, program)
          bodyPredicateTypes <- seqM(body0.map(b => inferBodyPredicate(b, program)))
          unifiedBodyPredicateType <- unifyAllowEmptyM(bodyPredicateTypes, loc)
          resultType <- unifyM(tvar, headPredicateType, unifiedBodyPredicateType, loc)
        } yield resultType

      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : tpe
        //  ------------------------
        //  exp1 <+> exp2 : tpe
        //
        for {
          tpe1 <- visitExp(exp1)
          tpe2 <- visitExp(exp2)
          resultType <- unifyM(tvar, tpe1, tpe2, loc)
        } yield resultType

      case ResolvedAst.Expression.FixpointSolve(exp, tvar, loc) =>
        //
        //  exp : tpe
        //  ---------------
        //  solve exp : tpe
        //
        for {
          inferredType <- visitExp(exp)
          resultType <- unifyM(tvar, inferredType, mkAnySchemaType(), loc)
        } yield resultType

      case ResolvedAst.Expression.FixpointProject(pred, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : Schema { P : a  | b }
        //  -------------------------------------------
        //  project P<exp1> exp2 : Schema { P : a | c }
        //
        val freshPredicateTypeVar = Type.freshTypeVar() // a
      val freshRestSchemaTypeVar = Type.freshTypeVar() // b
      val freshResultSchemaTypeVar = Type.freshTypeVar() // c

        for {
          parameterType <- visitExp(pred.exp)
          inferredType <- visitExp(exp)
          expectedType <- unifyM(inferredType, Type.SchemaExtend(pred.sym, freshPredicateTypeVar, freshRestSchemaTypeVar), loc)
          resultType <- unifyM(tvar, Type.SchemaExtend(pred.sym, freshPredicateTypeVar, freshResultSchemaTypeVar), loc)
        } yield resultType

      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : tpe
        //  ------------------------
        //  exp1 |= exp2 : tpe
        //
        for {
          tpe1 <- visitExp(exp1)
          tpe2 <- visitExp(exp2)
          schemaType <- unifyM(tpe1, tpe2, mkAnySchemaType(), loc)
          resultType <- unifyM(tvar, Type.Cst(TypeConstructor.Bool), loc)
        } yield resultType

      /*
       * User Error expression.
       */
      case ResolvedAst.Expression.UserError(tvar, loc) => liftM(tvar)

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
      /*
       * Wildcard expression.
       */
      case ResolvedAst.Expression.Wild(tvar, loc) => TypedAst.Expression.Wild(subst0(tvar), Eff.Empty, loc)

      /*
       * Variable expression.
       */
      case ResolvedAst.Expression.Var(sym, _, loc) => TypedAst.Expression.Var(sym, subst0(sym.tvar), Eff.Empty, loc)

      /*
       * Def expression.
       */
      case ResolvedAst.Expression.Def(sym, tvar, loc) =>
        TypedAst.Expression.Def(sym, subst0(tvar), Eff.Empty, loc)

      /*
       * Eff expression.
       */
      case ResolvedAst.Expression.Eff(sym, tvar, loc) =>
        TypedAst.Expression.Eff(sym, subst0(tvar), Eff.Empty, loc)

      /*
       * Eff expression.
       */
      case ResolvedAst.Expression.Sig(sym, tvar, loc) =>
        ??? // TODO

      /*
       * Hole expression.
       */
      case ResolvedAst.Expression.Hole(sym, tpe, loc) =>
        TypedAst.Expression.Hole(sym, subst0(tpe), Eff.Empty, loc)

      /*
       * Literal expression.
       */
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

      /*
       * Apply expression.
       */
      case ResolvedAst.Expression.Apply(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Apply(e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * Lambda expression.
       */
      case ResolvedAst.Expression.Lambda(fparam, exp, tvar, loc) =>
        val p = visitParam(fparam)
        val e = visitExp(exp, subst0)
        val t = subst0(tvar)
        TypedAst.Expression.Lambda(p, e, t, Eff.Empty, loc)

      /*
       * Unary expression.
       */
      case ResolvedAst.Expression.Unary(op, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Unary(op, e, subst0(tvar), Eff.Empty, loc)

      /*
       * Binary expression.
       */
      case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Binary(op, e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * If-then-else expression.
       */
      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.IfThenElse(e1, e2, e3, subst0(tvar), Eff.Empty, loc)

      /*
       * Let expression.
       */
      case ResolvedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Let(sym, e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * LetRec expression.
       */
      case ResolvedAst.Expression.LetRec(sym, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.LetRec(sym, e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * Match expression.
       */
      case ResolvedAst.Expression.Match(exp1, rules, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val rs = rules map {
          case ResolvedAst.MatchRule(pat, guard, exp) =>
            val p = reassemblePattern(pat, program, subst0)
            val g = visitExp(guard, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.MatchRule(p, g, b)
        }
        TypedAst.Expression.Match(e1, rs, subst0(tvar), Eff.Empty, loc)

      /*
       * Switch expression.
       */
      case ResolvedAst.Expression.Switch(rules, tvar, loc) =>
        val rs = rules.map {
          case (cond, body) => (visitExp(cond, subst0), visitExp(body, subst0))
        }
        TypedAst.Expression.Switch(rs, subst0(tvar), Eff.Empty, loc)

      /*
       * Tag expression.
       */
      case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Tag(sym, tag, e, subst0(tvar), Eff.Empty, loc)

      /*
       * Tuple expression.
       */
      case ResolvedAst.Expression.Tuple(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.Tuple(es, subst0(tvar), Eff.Empty, loc)

      /*
       * RecordEmpty expression.
       */
      case ResolvedAst.Expression.RecordEmpty(tvar, loc) =>
        TypedAst.Expression.RecordEmpty(subst0(tvar), Eff.Empty, loc)

      /*
        * RecordSelect expression.
        */
      case ResolvedAst.Expression.RecordSelect(exp, label, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.RecordSelect(e, label, subst0(tvar), Eff.Empty, loc)

      /*
       * RecordExtend expression.
       */
      case ResolvedAst.Expression.RecordExtend(label, value, rest, tvar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        TypedAst.Expression.RecordExtend(label, v, r, subst0(tvar), Eff.Empty, loc)

      /*
       * RecordRestrict expression.
       */
      case ResolvedAst.Expression.RecordRestrict(label, rest, tvar, loc) =>
        val r = visitExp(rest, subst0)
        TypedAst.Expression.RecordRestrict(label, r, subst0(tvar), Eff.Empty, loc)

      /*
       * ArrayLit expression.
       */
      case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.ArrayLit(es, subst0(tvar), Eff.Empty, loc)

      /*
       * ArrayNew expression.
       */
      case ResolvedAst.Expression.ArrayNew(elm, len, tvar, loc) =>
        val e = visitExp(elm, subst0)
        val ln = visitExp(len, subst0)
        TypedAst.Expression.ArrayNew(e, ln, subst0(tvar), Eff.Empty, loc)

      /*
       * ArrayLoad expression.
       */
      case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.ArrayLoad(e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * ArrayStore expression.
       */
      case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.ArrayStore(e1, e2, e3, subst0(tvar), Eff.Empty, loc)

      /*
       * ArrayLength expression.
       */
      case ResolvedAst.Expression.ArrayLength(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.ArrayLength(e, subst0(tvar), Eff.Empty, loc)

      /*
       * ArraySlice expression.
       */
      case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        TypedAst.Expression.ArraySlice(e1, e2, e3, subst0(tvar), Eff.Empty, loc)

      /*
       * VectorLit expression.
       */
      case ResolvedAst.Expression.VectorLit(elms, tvar, loc) =>
        val es = elms.map(e => visitExp(e, subst0))
        TypedAst.Expression.VectorLit(es, subst0(tvar), Eff.Empty, loc)

      /*
       * VectorLoad expression.
       */
      case ResolvedAst.Expression.VectorNew(elm, len, tvar, loc) =>
        val e = visitExp(elm, subst0)
        TypedAst.Expression.VectorNew(e, len, subst0(tvar), Eff.Empty, loc)

      /*
       * VectorLoad expression.
       */
      case ResolvedAst.Expression.VectorLoad(base, index, tvar, loc) =>
        val b = visitExp(base, subst0)
        TypedAst.Expression.VectorLoad(b, index, subst0(tvar), Eff.Empty, loc)

      /*
       * VectorStore expression.
       */
      case ResolvedAst.Expression.VectorStore(base, index, elm, tvar, loc) =>
        val b = visitExp(base, subst0)
        val e = visitExp(elm, subst0)
        TypedAst.Expression.VectorStore(b, index, e, subst0(tvar), Eff.Empty, loc)

      /*
       * VectorLength expression.
       */
      case ResolvedAst.Expression.VectorLength(base, tvar, loc) =>
        val b = visitExp(base, subst0)
        TypedAst.Expression.VectorLength(b, subst0(tvar), Eff.Empty, loc)

      /*
       * VectorSlice expression.
       */
      case ResolvedAst.Expression.VectorSlice(base, startIndex, optEndIndex, tvar, loc) =>
        val e = visitExp(base, subst0)
        optEndIndex match {
          case None =>
            val len = TypedAst.Expression.VectorLength(e, Type.Cst(TypeConstructor.Int32), Eff.Empty, loc)
            TypedAst.Expression.VectorSlice(e, startIndex, len, subst0(tvar), Eff.Empty, loc)
          case Some(endIndex) =>
            val len = TypedAst.Expression.Int32(endIndex, loc)
            TypedAst.Expression.VectorSlice(e, startIndex, len, subst0(tvar), Eff.Empty, loc)
        }

      /*
       * Reference expression.
       */
      case ResolvedAst.Expression.Ref(exp, tvar, loc) =>
        // TODO: Check if tvar is unbound.
        if (!(subst0.m contains tvar))
          throw InternalCompilerException("Unbound tvar in ref.")

        val e = visitExp(exp, subst0)
        TypedAst.Expression.Ref(e, subst0(tvar), Eff.Empty, loc)

      /*
       * Dereference expression.
       */
      case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Deref(e, subst0(tvar), Eff.Empty, loc)

      /*
       * Assignment expression.
       */
      case ResolvedAst.Expression.Assign(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.Assign(e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * HandleWith expression.
       */
      case ResolvedAst.Expression.HandleWith(exp, bindings, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val bs = bindings map {
          case ResolvedAst.HandlerBinding(sym, handler) => TypedAst.HandlerBinding(sym, visitExp(handler, subst0))
        }
        TypedAst.Expression.HandleWith(e, bs, subst0(tvar), Eff.Empty, loc)

      /*
       * Existential expression.
       */
      case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Existential(visitParam(fparam), e, Eff.Empty, loc)

      /*
       * Universal expression.
       */
      case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Universal(visitParam(fparam), e, Eff.Empty, loc)

      /*
       * Ascribe expression.
       */
      case ResolvedAst.Expression.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Ascribe(e, tpe, eff, loc)

      /*
       * Cast expression.
       */
      case ResolvedAst.Expression.Cast(exp, tpe, eff, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Cast(e, tpe, eff, loc)

      /*
       * Try Catch expression.
       */
      case ResolvedAst.Expression.TryCatch(exp, rules, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case ResolvedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        TypedAst.Expression.TryCatch(e, rs, subst0(tvar), Eff.Empty, loc)

      /*
       * Native Constructor expression.
       */
      case ResolvedAst.Expression.NativeConstructor(constructor, actuals, tpe, loc) =>
        val es = actuals.map(e => visitExp(e, subst0))
        TypedAst.Expression.NativeConstructor(constructor, es, subst0(tpe), Eff.Empty, loc)

      /*
       * Native Field expression.
       */
      case ResolvedAst.Expression.NativeField(field, tpe, loc) =>
        TypedAst.Expression.NativeField(field, subst0(tpe), Eff.Empty, loc)

      /*
       * Native Method expression.
       */
      case ResolvedAst.Expression.NativeMethod(method, actuals, tpe, loc) =>
        val es = actuals.map(e => visitExp(e, subst0))
        TypedAst.Expression.NativeMethod(method, es, subst0(tpe), Eff.Empty, loc)

      /*
       * New Channel expression.
       */
      case ResolvedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.NewChannel(e, Type.mkChannel(tpe), Eff.Empty, loc)

      /*
       * Get Channel expression.
       */
      case ResolvedAst.Expression.GetChannel(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.GetChannel(e, subst0(tvar), Eff.Empty, loc)

      /*
       * Put Channel expression.
       */
      case ResolvedAst.Expression.PutChannel(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.PutChannel(e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * Select Channel expression.
       */
      case ResolvedAst.Expression.SelectChannel(rules, default, tvar, loc) =>
        val rs = rules map {
          case ResolvedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule(sym, c, b)
        }

        val d = default.map(visitExp(_, subst0))

        TypedAst.Expression.SelectChannel(rs, d, subst0(tvar), Eff.Empty, loc)

      /*
       * Spawn expression.
       */
      case ResolvedAst.Expression.Spawn(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Spawn(e, subst0(tvar), Eff.Empty, loc)

      /*
       * Sleep expression.
       */
      case ResolvedAst.Expression.Sleep(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.Sleep(e, subst0(tvar), Eff.Empty, loc)

      /*
       * Constraint expression.
       */
      case ResolvedAst.Expression.FixpointConstraint(c0, tvar, loc) =>
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
        val c = TypedAst.Constraint(cparams, head, body, loc)
        TypedAst.Expression.FixpointConstraint(c, subst0(tvar), Eff.Empty, loc)

      /*
       * ConstraintUnion expression.
       */
      case ResolvedAst.Expression.FixpointCompose(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.FixpointCompose(e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * FixpointSolve expression.
       */
      case ResolvedAst.Expression.FixpointSolve(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expression.FixpointSolve(e, subst0(tvar), Eff.Empty, loc)

      /*
       * FixpointProject expression.
       */
      case ResolvedAst.Expression.FixpointProject(pred, exp, tvar, loc) =>
        val p = visitPredicateWithParam(pred)
        val e = visitExp(exp, subst0)
        TypedAst.Expression.FixpointProject(p, e, subst0(tvar), Eff.Empty, loc)

      /*
       * ConstraintUnion expression.
       */
      case ResolvedAst.Expression.FixpointEntails(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        TypedAst.Expression.FixpointEntails(e1, e2, subst0(tvar), Eff.Empty, loc)

      /*
       * User Error expression.
       */
      case ResolvedAst.Expression.UserError(tvar, loc) =>
        TypedAst.Expression.UserError(subst0(tvar), Eff.Empty, loc)
    }

    /**
      * Applies the substitution to the given list of formal parameters.
      */
    def visitParam(param: ResolvedAst.FormalParam): TypedAst.FormalParam =
      TypedAst.FormalParam(param.sym, param.mod, subst0(param.tpe), param.loc)

    /**
      * Applies the substitution to the predicate with parameter `pred`.
      */
    def visitPredicateWithParam(pred: ResolvedAst.PredicateWithParam): TypedAst.PredicateWithParam = pred match {
      case ResolvedAst.PredicateWithParam(sym, exp) =>
        val e = visitExp(exp, subst0)
        TypedAst.PredicateWithParam(sym, e)
    }

    visitExp(exp0, subst0)
  }

  /**
    * Infers the type of the given pattern `pat0`.
    */
  private def inferPattern(pat0: ResolvedAst.Pattern, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = {
    /**
      * Local pattern visitor.
      */
    def visit(p: ResolvedAst.Pattern): InferMonad[Type] = p match {
      case ResolvedAst.Pattern.Wild(tvar, loc) => liftM(tvar)
      case ResolvedAst.Pattern.Var(sym, tvar, loc) => unifyM(sym.tvar, tvar, loc)
      case ResolvedAst.Pattern.Unit(loc) => liftM(Type.Cst(TypeConstructor.Unit))
      case ResolvedAst.Pattern.True(loc) => liftM(Type.Cst(TypeConstructor.Bool))
      case ResolvedAst.Pattern.False(loc) => liftM(Type.Cst(TypeConstructor.Bool))
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
          _________ <- unifyM(innerType, freshCaseType, loc);
          resultType <- unifyM(tvar, freshEnumType, loc)
        ) yield resultType

      case ResolvedAst.Pattern.Tuple(elms, tvar, loc) =>
        for (
          elementTypes <- seqM(elms map visit);
          resultType <- unifyM(tvar, Type.mkTuple(elementTypes), loc)
        ) yield resultType
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[ResolvedAst.Pattern], program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[List[Type]] = {
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
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given head predicate.
    */
  private def inferHeadPredicate(head: ResolvedAst.Predicate.Head, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = head match {
    case ResolvedAst.Predicate.Head.Atom(sym, exp, terms, tvar, loc) =>
      //
      //  t_1 : tpe_1, ..., t_n: tpe_n
      //  ------------------------------------------------------------
      //  P(t_1, ..., t_n): Schema{ P = P(tpe_1, ..., tpe_n) | fresh }
      //

      // Lookup the type scheme.
      val scheme = sym match {
        case x: Symbol.RelSym => program.relations(x).sc
        case x: Symbol.LatSym => program.lattices(x).sc
      }

      // Instantiate the type scheme.
      val declaredType = Scheme.instantiate(scheme)

      // Infer the types of the terms.
      for {
        paramType <- inferExp(exp, program)
        termTypes <- seqM(terms.map(inferExp(_, program)))
        predicateType <- unifyM(tvar, getRelationOrLatticeType(sym, termTypes, program), declaredType, loc)
      } yield Type.SchemaExtend(sym, predicateType, Type.freshTypeVar())
  }

  /**
    * Applies the given substitution `subst0` to the given head predicate `head0`.
    */
  private def reassembleHeadPredicate(head0: ResolvedAst.Predicate.Head, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case ResolvedAst.Predicate.Head.Atom(sym, exp, terms, tvar, loc) =>
      val e = reassembleExp(exp, program, subst0)
      val ts = terms.map(t => reassembleExp(t, program, subst0))
      val pred = TypedAst.PredicateWithParam(sym, e)
      TypedAst.Predicate.Head.Atom(pred, ts, subst0(tvar), loc)
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: ResolvedAst.Predicate.Body, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = body0 match {
    //
    //  t_1 : tpe_1, ..., t_n: tpe_n
    //  ------------------------------------------------------------
    //  P(t_1, ..., t_n): Schema{ P = P(tpe_1, ..., tpe_n) | fresh }
    //
    case ResolvedAst.Predicate.Body.Atom(sym, exp, polarity, terms, tvar, loc) =>
      // Lookup the type scheme.
      val scheme = sym match {
        case x: Symbol.RelSym => program.relations(x).sc
        case x: Symbol.LatSym => program.lattices(x).sc
      }

      // Instantiate the type scheme.
      val declaredType = Scheme.instantiate(scheme)

      // Infer the types of the terms.
      for {
        paramType <- inferExp(exp, program)
        termTypes <- seqM(terms.map(inferPattern(_, program)))
        predicateType <- unifyM(tvar, getRelationOrLatticeType(sym, termTypes, program), declaredType, loc)
      } yield Type.SchemaExtend(sym, predicateType, Type.freshTypeVar())

    //
    //  exp : Bool
    //  ----------
    //  if exp : a
    //
    case ResolvedAst.Predicate.Body.Filter(sym, terms, loc) =>
      val defn = program.defs(sym)
      val declaredType = Scheme.instantiate(defn.sc)
      for {
        argumentTypes <- seqM(terms.map(t => inferExp(t, program)))
        unifiedTypes <- Unification.unifyM(declaredType, Type.mkArrow(argumentTypes, Type.Cst(TypeConstructor.Bool)), loc)
      } yield mkAnySchemaType()

    //
    //  x: t    exp: Array[t]
    //  ---------------------
    //  x <- exp : a
    //
    case ResolvedAst.Predicate.Body.Functional(sym, term, loc) =>
      for {
        termType <- inferExp(term, program)
        arrayType <- unifyM(Type.mkArray(sym.tvar), termType, loc)
      } yield mkAnySchemaType()
  }

  /**
    * Applies the given substitution `subst0` to the given body predicate `body0`.
    */
  private def reassembleBodyPredicate(body0: ResolvedAst.Predicate.Body, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case ResolvedAst.Predicate.Body.Atom(sym, exp, polarity, terms, tvar, loc) =>
      val e = reassembleExp(exp, program, subst0)
      val ts = terms.map(t => reassemblePattern(t, program, subst0))
      val pred = TypedAst.PredicateWithParam(sym, e)
      TypedAst.Predicate.Body.Atom(pred, polarity, ts, subst0(tvar), loc)

    case ResolvedAst.Predicate.Body.Filter(sym, terms, loc) =>
      val defn = program.defs(sym)
      val ts = terms.map(t => reassembleExp(t, program, subst0))
      TypedAst.Predicate.Body.Filter(defn.sym, ts, loc)

    case ResolvedAst.Predicate.Body.Functional(sym, term, loc) =>
      val t = reassembleExp(term, program, subst0)
      TypedAst.Predicate.Body.Functional(sym, t, loc)
  }

  /**
    * Returns the relation or lattice type of `sym` with the term types `ts` applied to the appropriate number of type variables.
    *
    * For example, if there is a relation:
    *
    * rel R[w](x: Int, y: Int, w: w)
    *
    * and this function is called with R and List(Int, Int) then it returns:
    *
    * Apply(R(List(Int, Int)), a)
    *
    * where a is a fresh type variable.
    */
  private def getRelationOrLatticeType(sym: Symbol.PredSym, ts: List[Type], program: ResolvedAst.Program)(implicit genSym: GenSym): Type = {
    val quantifiers = sym match {
      case x: Symbol.RelSym => program.relations(x).sc.quantifiers
      case x: Symbol.LatSym => program.lattices(x).sc.quantifiers
    }

    val zero = Type.mkRelationOrLattice(sym, ts)
    val result = quantifiers.foldLeft(zero) {
      case (tacc, _) => Type.Apply(tacc, Type.freshTypeVar())
    }

    result
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
      case ResolvedAst.Relation(_, _, _, tparams, attr, sc, _) => Ok(attr.map(_.tpe))
    }
  }

  /**
    * Returns the declared types of the terms of the given lattice symbol `sym`.
    */
  private def getLatticeSignature(sym: Symbol.LatSym, program: ResolvedAst.Program): Result[List[Type], TypeError] = {
    program.lattices(sym) match {
      case ResolvedAst.Lattice(_, _, _, _, attr, sc, _) => Ok(attr.map(_.tpe))
    }
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    */
  private def getSubstFromParams(params: List[ResolvedAst.FormalParam]): Unification.Substitution = {
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
  private def getFormalParams(fparams0: List[ResolvedAst.FormalParam], subst0: Unification.Substitution): List[TypedAst.FormalParam] = fparams0.map {
    case ResolvedAst.FormalParam(sym, mod, tpe, loc) => TypedAst.FormalParam(sym, mod, subst0(sym.tvar), sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaType()(implicit genSym: GenSym): Type = {
    val sym = Symbol.mkRelSym("True")
    val tpe = Type.Relation(sym, Nil, Kind.Star)
    Type.SchemaExtend(sym, tpe, Type.freshTypeVar())
  }

  /**
    * Returns the Flix Type of a Java Type
    */
  private def getGenericFlixType(t: java.lang.reflect.Type)(implicit genSym: GenSym): Type = {
    t match {
      case arrayType: java.lang.reflect.GenericArrayType =>
        val comp = arrayType.getGenericComponentType
        val elmType = getGenericFlixType(comp)
        Type.mkArray(elmType)
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
      Type.Cst(TypeConstructor.Bool)
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
      Type.Cst(TypeConstructor.Unit)
    }
    // handle arrays of types
    else if (c.isArray) {
      val comp = c.getComponentType
      val elmType = getFlixType(comp)
      Type.mkArray(elmType)
    }
    // otherwise native type
    else {
      Type.Cst(TypeConstructor.Native(c))
    }
  }

}
