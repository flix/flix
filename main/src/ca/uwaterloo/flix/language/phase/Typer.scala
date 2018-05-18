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

import java.lang.reflect.Field

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
  def run(program: ResolvedAst.Program)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val startTime = System.nanoTime()

    val result = for {
      defs <- Declarations.Definitions.typecheck(program)
      effs <- Declarations.typecheckEffects(program)
      handlers <- Declarations.typecheckHandlers(program)
      enums <- Declarations.Enums.typecheck(program)
      lattices <- Declarations.Lattices.typecheck(program)
      tables <- Declarations.Tables.typecheck(program)
      indexes <- Declarations.Indexes.typecheck(program)
      constraints <- Constraints.typecheck(program)
      properties <- Declarations.Properties.typecheck(program)
    } yield {
      val strata = List(TypedAst.Stratum(constraints))
      val specialOps = Map.empty[SpecialOperator, Map[Type, Symbol.DefnSym]]
      val currentTime = System.nanoTime()
      val time = program.time.copy(typer = currentTime - startTime)
      TypedAst.Root(defs, effs, handlers, enums, lattices, tables, indexes, strata, properties, specialOps, program.reachable, time)
    }

    result match {
      case Ok(p) => p.toSuccess
      case Err(e) => e.toFailure
    }
  }

  object Constraints {

    /**
      * Performs type inference and reassembly on all constraints in the given program.
      *
      * Returns [[Err]] if a constraint fails to type check.
      */
    def typecheck(program: ResolvedAst.Program)(implicit genSym: GenSym): Result[List[TypedAst.Constraint], TypeError] = {

      /**
        * Performs type inference on the given constraint `c`.
        */
      def visitConstraint(c: ResolvedAst.Constraint): Result[TypedAst.Constraint, TypeError] = c match {
        case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>

          // Infer the types of head and body predicates.
          val result = for {
            headType <- Predicates.infer(head0, program)
            bodyTypes <- seqM(body0.map(b => Predicates.infer(b, program)))
          } yield ()

          // Evaluate the monad under the empty substitution.
          result.run(Substitution.empty) map {
            case (subst, _) =>
              // Unification was successful. Reassemble the head and body predicates.
              val head = Predicates.reassemble(head0, program, subst)
              val body = body0.map(b => Predicates.reassemble(b, program, subst))

              // Reassemble the constraint parameters.
              val cparams = cparams0.map {
                case ResolvedAst.ConstraintParam.HeadParam(sym, tpe, l) =>
                  TypedAst.ConstraintParam.HeadParam(sym, subst(tpe), l)
                case ResolvedAst.ConstraintParam.RuleParam(sym, tpe, l) =>
                  TypedAst.ConstraintParam.RuleParam(sym, subst(tpe), l)
              }

              // Reassemble the constraint.
              TypedAst.Constraint(cparams, head, body, loc)
          }
      }

      // Perform type inference on every constraint in the program.
      val result = program.constraints.map {
        case constraint => visitConstraint(constraint)
      }

      // Sequence the results.
      Result.seqM(result)
    }

  }

  object Declarations {

    object Definitions {

      /**
        * Performs type inference and reassembly on all definitions in the given program.
        *
        * Returns [[Err]] if a definition fails to type check.
        */
      def typecheck(program: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
        // Put genSym into implicit scope.
        implicit val genSym = flix.genSym

        /**
          * Performs type inference and reassembly on the given definition `defn`.
          */
        def visitDefn(defn: ResolvedAst.Def): Result[(Symbol.DefnSym, TypedAst.Def), TypeError] = defn match {
          case ResolvedAst.Def(doc, ann, mod, sym, tparams, params, exp, tpe, eff, loc) =>
            infer(defn, program) map {
              case d => sym -> d
            }
        }

        // Every definition in the program.
        val defs = program.defs.values.toList

        // Visit every definition in parallel.
        val results = ParOps.parMap(visitDefn, defs)

        // Sequence the results and convert them back to a map.
        Result.seqM(results).map(_.toMap)
      }

      /**
        * Infers the type of the given definition `defn0`.
        */
      // TODO: Cleanup
      def infer(defn0: ResolvedAst.Def, program: ResolvedAst.Program)(implicit genSym: GenSym): Result[TypedAst.Def, TypeError] = {
        // Resolve the declared scheme.
        val declaredScheme = defn0.sc

        // TODO: Some duplication
        val argumentTypes = defn0.fparams.map(_.tpe)

        val result = for (
          resultType <- Expressions.infer(defn0.exp, program);
          unifiedType <- unifyM(Scheme.instantiate(declaredScheme), Type.mkArrow(argumentTypes, resultType), defn0.loc)
        ) yield unifiedType

        // TODO: See if this can be rewritten nicer
        result match {
          case InferMonad(run) =>
            val subst0 = getSubstFromParams(defn0.fparams)
            run(subst0) match {
              case Ok((subst, resultType)) =>
                val exp = Expressions.reassemble(defn0.exp, program, subst)
                val tparams = getTypeParams(defn0.tparams)
                val fparams = getFormalParams(defn0.fparams, subst)
                Ok(TypedAst.Def(defn0.doc, defn0.ann, defn0.mod, defn0.sym, tparams, fparams, exp, resultType, defn0.eff, defn0.loc))

              case Err(e) => Err(e)
            }
        }
      }

    }

    object Enums {
      /**
        * Performs type inference and reassembly on all enums in the given program.
        */
      def typecheck(program: ResolvedAst.Program)(implicit genSym: GenSym): Result[Map[Symbol.EnumSym, TypedAst.Enum], TypeError] = {
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
        Result.seqM(result).map(_.toMap)
      }
    }

    object Indexes {

      /**
        * Performs type inference and reassembly on all indexes in the given program.
        *
        * Returns [[Err]] if an index refers to a non-existent table or a non-existent attribute in a table.
        */
      def typecheck(program: ResolvedAst.Program): Result[Map[Symbol.TableSym, TypedAst.Index], TypeError] = {

        /**
          * Checks that the referenced table exists and that every attribute used by the index exists.
          */
        def visitIndex(index: ResolvedAst.Index): Result[(Symbol.TableSym, TypedAst.Index), TypeError] = index match {
          case ResolvedAst.Index(sym, indexes, loc) =>
            // Lookup the table using the table symbol.
            val table = program.tables(index.sym)

            val declaredAttributes = table.attr.map(_.ident.name)
            // Iterate through every index in the declaration.
            for (index <- indexes) {
              // Iterate through every attribute name in the current index.
              for (referencedAttribute <- index) {
                if (!(declaredAttributes contains referencedAttribute.name)) {
                  return Err(TypeError.UndefinedAttribute(table.sym.name, referencedAttribute.name, referencedAttribute.loc))
                }
              }
            }
            Ok(table.sym -> TypedAst.Index(table.sym, indexes, loc))
        }

        // Visit every index in the program.
        val result = program.indexes.toList.map {
          case (_, index) => visitIndex(index)
        }

        // Sequence the results and convert them back to a map.
        Result.seqM(result).map(_.toMap)
      }

    }

    object Lattices {

      /**
        * Performs type inference and reassembly on all lattices in the given program.
        *
        * Returns [[Err]] if a type error occurs.
        */
      def typecheck(program: ResolvedAst.Program)(implicit genSym: GenSym): Result[Map[Type, TypedAst.Lattice], TypeError] = {

        /**
          * Performs type inference and reassembly on the given `lattice`.
          */
        def visitLattice(lattice: ResolvedAst.Lattice): Result[(Type, TypedAst.Lattice), TypeError] = lattice match {
          case ResolvedAst.Lattice(tpe, e1, e2, e3, e4, e5, e6, ns, loc) =>
            // Perform type resolution on the declared type.
            val declaredType = lattice.tpe

            // Perform type inference on each of the lattice components.
            val m = for {
              botType <- Expressions.infer(e1, program)
              topType <- Expressions.infer(e2, program)
              equType <- Expressions.infer(e3, program)
              leqType <- Expressions.infer(e4, program)
              lubType <- Expressions.infer(e5, program)
              glbType <- Expressions.infer(e6, program)
              _______ <- unifyM(botType, declaredType, loc)
              _______ <- unifyM(topType, declaredType, loc)
              _______ <- unifyM(equType, Type.mkArrow(List(declaredType, declaredType), Type.Bool), loc)
              _______ <- unifyM(leqType, Type.mkArrow(List(declaredType, declaredType), Type.Bool), loc)
              _______ <- unifyM(lubType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
              _______ <- unifyM(glbType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
            } yield declaredType

            // Evaluate the type inference monad with the empty substitution
            m.run(Substitution.empty) map {
              case (subst, _) =>
                // Reassemble the lattice components.
                val bot = Expressions.reassemble(e1, program, subst)
                val top = Expressions.reassemble(e2, program, subst)
                val equ = Expressions.reassemble(e3, program, subst)
                val leq = Expressions.reassemble(e4, program, subst)
                val lub = Expressions.reassemble(e5, program, subst)
                val glb = Expressions.reassemble(e6, program, subst)

                declaredType -> TypedAst.Lattice(declaredType, bot, top, equ, leq, lub, glb, loc)
            }

        }


        // Visit every lattice in the program.
        val result = program.lattices.toList.map {
          case (_, lattice) => visitLattice(lattice)
        }

        // Sequence the results and convert them back to a map.
        Result.seqM(result).map(_.toMap)
      }

    }

    object Tables {

      /**
        * Performs type inference and reassembly on all tables in the given program.
        *
        * Returns [[Err]] if type resolution fails.
        */
      def typecheck(program: ResolvedAst.Program): Result[Map[Symbol.TableSym, TypedAst.Table], TypeError] = {

        /**
          * Performs type resolution on the given `table`.
          *
          * Returns [[Err]] if a type is unresolved.
          */
        def visitTable(table: ResolvedAst.Table): Result[(Symbol.TableSym, TypedAst.Table), TypeError] = table match {
          case ResolvedAst.Table.Relation(doc, sym, attr, loc) =>
            for (typedAttributes <- Result.seqM(attr.map(a => visitAttribute(a))))
              yield sym -> TypedAst.Table.Relation(doc, sym, typedAttributes, loc)
          case ResolvedAst.Table.Lattice(doc, sym, keys, value, loc) =>
            for {
              typedKeys <- Result.seqM(keys.map(a => visitAttribute(a)))
              typedVal <- visitAttribute(value)
            } yield sym -> TypedAst.Table.Lattice(doc, sym, typedKeys, typedVal, loc)
        }

        /**
          * Performs type resolution on the given attribute `attr`.
          */
        def visitAttribute(attr: ResolvedAst.Attribute): Result[TypedAst.Attribute, TypeError] = attr match {
          case ResolvedAst.Attribute(ident, tpe, loc) => Ok(TypedAst.Attribute(ident.name, tpe, loc))
        }

        // Visit every table in the program.
        val result = program.tables.toList.map {
          case (_, table) => visitTable(table)
        }

        // Sequence the results and convert them back to a map.
        Result.seqM(result).map(_.toMap)
      }

    }

    object Properties {

      /**
        * Infers the types of all the properties in the given program `prog0`.
        */
      def typecheck(prog0: ResolvedAst.Program)(implicit genSym: GenSym): Result[List[TypedAst.Property], TypeError] = {

        /**
          * Infers the type of the given property `p0`.
          */
        def visitProperty(p0: ResolvedAst.Property): Result[TypedAst.Property, TypeError] = p0 match {
          case ResolvedAst.Property(law, defn, exp0, loc) =>
            val result = Expressions.infer(exp0, prog0)
            result.run(Substitution.empty) map {
              case (subst, tpe) =>
                val exp = Expressions.reassemble(exp0, prog0, subst)
                TypedAst.Property(law, defn, exp, loc)
            }
        }

        // Visit every property in the program.
        val results = prog0.properties.map {
          case property => visitProperty(property)
        }

        // Sequence the results and sort the properties by their source location.
        Result.seqM(results).map(_.sortBy(_.loc))
      }

    }

    /**
      * Infers the types of the effects in the given program.
      */
    def typecheckEffects(program0: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.EffSym, TypedAst.Eff], TypeError] = {
      // Typecheck every effect in the program.
      val effs = program0.effs.toList.map {
        case (sym, eff0) => typecheckEff(eff0) map (e => sym -> e)
      }

      // Sequence the results and convert them back to a map.
      Result.seqM(effs).map(_.toMap)
    }

    /**
      * Infers the the type of the given effect `eff0`.
      */
    def typecheckEff(eff0: ResolvedAst.Eff)(implicit flix: Flix): Result[TypedAst.Eff, TypeError] = eff0 match {
      case ResolvedAst.Eff(doc, ann, mod, sym, tparams0, fparams0, sc, eff, loc) =>
        val argumentTypes = fparams0.map(_.tpe)
        val tpe = Scheme.instantiate(sc)(flix.genSym)

        val subst = getSubstFromParams(fparams0)
        val tparams = getTypeParams(tparams0)
        val fparams = getFormalParams(fparams0, subst)

        Ok(TypedAst.Eff(doc, ann, mod, sym, tparams, fparams, tpe, eff, loc))
    }

    /**
      * Infers the types of the handlers in the given program.
      */
    def typecheckHandlers(program0: ResolvedAst.Program)(implicit flix: Flix): Result[Map[Symbol.EffSym, TypedAst.Handler], TypeError] = {
      // Typecheck every handler in the program.
      val effs = program0.handlers.toList.map {
        case (sym, handler0) => typecheckHandler(handler0, program0) map (e => sym -> e)
      }

      // Sequence the results and convert them back to a map.
      Result.seqM(effs).map(_.toMap)
    }

    /**
      * Infers the the type of the given handler `handler0`.
      */
    def typecheckHandler(handler0: ResolvedAst.Handler, program0: ResolvedAst.Program)(implicit flix: Flix): Result[TypedAst.Handler, TypeError] = handler0 match {
      case ResolvedAst.Handler(doc, ann, mod, sym, tparams0, fparams0, exp0, sc, eff0, loc) =>

        val eff = program0.effs(sym)
        val effectType = Scheme.instantiate(eff.sc)(flix.genSym)

        val declaredType = Scheme.instantiate(sc)(flix.genSym)

        val subst0 = getSubstFromParams(fparams0)
        val tparams = getTypeParams(tparams0)
        val fparams = getFormalParams(fparams0, subst0)

        val result = for {
          actualType <- Expressions.infer(exp0, program0)(flix.genSym)
          unifiedType <- unifyM(declaredType, effectType, actualType, loc)
        } yield unifiedType

        result.run(Substitution.empty) map {
          case (subst, unifiedType) =>
            val exp = Expressions.reassemble(exp0, program0, subst)
            TypedAst.Handler(doc, ann, mod, sym, tparams, fparams, exp, subst(unifiedType), eff0, loc)
        }
    }


  }

  object Expressions {

    /**
      * Infers the type of the given expression `exp0`.
      */
    def infer(exp0: ResolvedAst.Expression, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = {

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
        case ResolvedAst.Expression.Var(sym, loc) => liftM(sym.tvar)

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
         * Hole expression.
         */
        case ResolvedAst.Expression.Hole(sym, tpe, loc) =>
          liftM(tpe)

        /*
         * Literal expression.
         */
        case ResolvedAst.Expression.Unit(loc) => liftM(Type.Unit)
        case ResolvedAst.Expression.True(loc) => liftM(Type.Bool)
        case ResolvedAst.Expression.False(loc) => liftM(Type.Bool)
        case ResolvedAst.Expression.Char(lit, loc) => liftM(Type.Char)
        case ResolvedAst.Expression.Float32(lit, loc) => liftM(Type.Float32)
        case ResolvedAst.Expression.Float64(lit, loc) => liftM(Type.Float64)
        case ResolvedAst.Expression.Int8(lit, loc) => liftM(Type.Int8)
        case ResolvedAst.Expression.Int16(lit, loc) => liftM(Type.Int16)
        case ResolvedAst.Expression.Int32(lit, loc) => liftM(Type.Int32)
        case ResolvedAst.Expression.Int64(lit, loc) => liftM(Type.Int64)
        case ResolvedAst.Expression.BigInt(lit, loc) => liftM(Type.BigInt)
        case ResolvedAst.Expression.Str(lit, loc) => liftM(Type.Str)

        /*
         * Lambda expression.
         */
        case ResolvedAst.Expression.Lambda(fparams, body, tvar, loc) =>
          val argumentTypes = fparams.map(_.tpe)
          for (
            inferredBodyType <- visitExp(body);
            resultType <- unifyM(tvar, Type.mkArrow(argumentTypes, inferredBodyType), loc)
          ) yield resultType

        /*
         * Apply expression.
         */
        case ResolvedAst.Expression.Apply(lambda, actuals, tvar, loc) =>
          val freshResultType = Type.freshTypeVar()
          for (
            inferredLambdaType <- visitExp(lambda);
            inferredArgumentTypes <- seqM(actuals.map(visitExp));
            expectedLambdaType <- unifyM(inferredLambdaType, Type.mkArrow(inferredArgumentTypes, freshResultType), loc);
            resultType <- unifyM(freshResultType, tvar, loc)
          ) yield resultType

        /*
         * Unary expression.
         */
        case ResolvedAst.Expression.Unary(op, exp1, tvar, loc) => op match {
          case UnaryOperator.LogicalNot =>
            for (
              tpe <- visitExp(exp1);
              res <- unifyM(tvar, tpe, Type.Bool, loc)
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
              resultType <- unifyM(tvar, Type.Bool, loc)
            ) yield resultType

          case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              ____ <- unifyM(tpe1, tpe2, loc);
              resultType <- unifyM(tvar, Type.Bool, loc)
            ) yield resultType

          case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
            for (
              tpe1 <- visitExp(exp1);
              tpe2 <- visitExp(exp2);
              resultType <- unifyM(tvar, tpe1, tpe2, Type.Bool, loc)
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
              rhsType <- unifyM(tpe2, Type.Int32, loc)
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
            ____ <- unifyM(Type.Bool, tpe1, loc);
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
            patternTypes <- Patterns.inferAll(patterns, program)
            patternType <- unifyM(patternTypes, loc)
            ___________ <- unifyM(matchType, patternType, loc)
            guardTypes <- seqM(guards map visitExp)
            guardType <- unifyM(Type.Bool :: guardTypes, loc)
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
            unifiedCondTypes <- unifyM(Type.Bool :: actualCondTypes, loc);
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
         * ArrayLit expression.
         */
          case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
          //
          //  e1 : t ... en: t
          //  ------------------------
          //  [e1,...,en] : Array[t]
          //
          if(elms.isEmpty){
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
            lengthType <- unifyM(actualLengthType, Type.Int32, loc);
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
            indexType <- unifyM(actualIndexType, Type.Int32, loc)
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
            resultType <- unifyM(Type.freshTypeVar(), Type.Int32, loc)
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
          val elementType = Type.freshTypeVar();
          for (
            actualBaseType <- visitExp(base);
            actualIndexType <- visitExp(index);
            actualElementType <- visitExp(elm);
            arrayType <- unifyM(actualBaseType, Type.mkArray(elementType), loc);
            indexType <- unifyM(actualIndexType, Type.Int32, loc);
            elementType <- unifyM(actualElementType, elementType, loc);
            resultType <- unifyM(tvar, Type.Unit, loc)
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
            startIndexType <- unifyM(actualStartIndexType, Type.Int32, loc);
            endIndexType <- unifyM(actualEndIndexType, Type.Int32, loc);
            resultType <- unifyM(tvar, actualBaseType, loc)
          ) yield resultType

        case ResolvedAst.Expression.VectorLit(elms, tvar, loc) =>
          //
          // elm1: t ...  elm_len: t  len: Succ(n, Zero)
          // ---------------------------
          // [|elm1,...,elm_len|] : Vector[t, len]
          //
          if(elms.isEmpty){
            for (
              resultType <- unifyM(tvar, Type.mkVector(Type.freshTypeVar(), Type.Succ(0, Type.Zero)), loc)
            ) yield resultType
          }
          else{
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
            resultType <- unifyM(tvar, Type.Unit, loc)
          ) yield resultType

        case ResolvedAst.Expression.VectorLength(base, tvar, loc) =>
          //
          // base: Vector[t, len1]   index: len2   len1: Succ(n1, Zero)  len2: Succ(n2, Var)
          // ----------------------------------------------------------------------------------
          // base[|index|] : Int
          //
          val freshResultType = Type.freshTypeVar()
          val freshLengthVar = Type.freshTypeVar()
          for(
            baseType <- visitExp(base);
            _ <- unifyM(baseType, Type.mkVector(freshResultType, Type.Succ(0, freshLengthVar)), loc);
            resultType <- unifyM(tvar, Type.Int32, loc)
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
              for(
                baseType <- visitExp(base);
                firstIndex <- unifyM(baseType, Type.mkVector(freshElmType, Type.Succ(startIndex, freshEndIndex)), loc);
                resultType <- unifyM(tvar, Type.mkVector(freshElmType, freshEndIndex), loc)
              ) yield resultType
            case Some(endIndex) =>
              for(
                baseType <- visitExp(base);
                firstIndex <- unifyM(baseType, Type.mkVector(freshElmType, Type.Succ(startIndex, freshBeginIndex)), loc);
                secondIndex <- unifyM(baseType, Type.mkVector(freshElmType, Type.Succ(endIndex, freshEndIndex)), loc);
                resultType <- unifyM(tvar, Type.mkVector(freshElmType, Type.Succ(endIndex-startIndex, Type.Zero)), loc)
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
            resultType <- unifyM(tvar, Type.Apply(Type.Ref, tpe), loc)
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
            _ <- unifyM(tpe, Type.Apply(Type.Ref, tvar), loc)
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
            _ <- unifyM(tpe1, Type.Apply(Type.Ref, tpe2), loc)
            resultType <- unifyM(tvar, Type.Unit, loc)
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
            resultType <- unifyM(bodyType, Type.Bool, loc)
          } yield resultType

        /*
         * Universal expression.
         */
        case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
          // NB: An existential behaves very much like a lambda.
          // TODO: Must check the type of the param.
          for {
            bodyType <- visitExp(exp)
            resultType <- unifyM(bodyType, Type.Bool, loc)
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
         * Native Constructor expression.
         */
        case ResolvedAst.Expression.NativeConstructor(constructor, actuals, tvar, loc) =>
          // TODO: Check types.
          for {
            inferredArgumentTypes <- seqM(actuals.map(visitExp))
            resultType <- unifyM(tvar, Type.Native, loc)
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
          // TODO: Check types.
          for (
            inferredArgumentTypes <- seqM(actuals.map(visitExp))
          ) yield tvar

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
    def reassemble(exp0: ResolvedAst.Expression, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Expression = {
      /**
        * Applies the given substitution `subst0` to the given expression `exp0`.
        */
      def visitExp(exp0: ResolvedAst.Expression, subst0: Substitution): TypedAst.Expression = exp0 match {
        /*
         * Wildcard expression.
         */
        case ResolvedAst.Expression.Wild(tvar, loc) => TypedAst.Expression.Wild(subst0(tvar), Eff.Bot, loc)

        /*
         * Variable expression.
         */
        case ResolvedAst.Expression.Var(sym, loc) => TypedAst.Expression.Var(sym, subst0(sym.tvar), Eff.Bot, loc)

        /*
         * Def expression.
         */
        case ResolvedAst.Expression.Def(sym, tvar, loc) =>
          TypedAst.Expression.Def(sym, subst0(tvar), Eff.Bot, loc)

        /*
         * Eff expression.
         */
        case ResolvedAst.Expression.Eff(sym, tvar, loc) =>
          TypedAst.Expression.Eff(sym, subst0(tvar), Eff.Bot, loc)

        /*
         * Hole expression.
         */
        case ResolvedAst.Expression.Hole(sym, tpe, loc) =>
          TypedAst.Expression.Hole(sym, subst0(tpe), Eff.Bot, loc)

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
        case ResolvedAst.Expression.Apply(lambda, actuals, tvar, loc) =>
          val l = visitExp(lambda, subst0)
          val as = actuals.map(e => visitExp(e, subst0))
          TypedAst.Expression.Apply(l, as, subst0(tvar), Eff.Bot, loc)

        /*
         * Lambda expression.
         */
        case ResolvedAst.Expression.Lambda(fparams, exp, tvar, loc) =>
          val lambdaParams = fparams map {
            case ResolvedAst.FormalParam(sym, mod, tpe, loc2) => TypedAst.FormalParam(sym, mod, subst0(tpe), loc2)
          }
          val lambdaBody = visitExp(exp, subst0)
          val lambdaType = subst0(tvar)
          TypedAst.Expression.Lambda(lambdaParams, lambdaBody, lambdaType, Eff.Bot, loc)

        /*
         * Unary expression.
         */
        case ResolvedAst.Expression.Unary(op, exp, tvar, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Unary(op, e, subst0(tvar), Eff.Bot, loc)

        /*
         * Binary expression.
         */
        case ResolvedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.Binary(op, e1, e2, subst0(tvar), Eff.Bot, loc)

        /*
         * If-then-else expression.
         */
        case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          val e3 = visitExp(exp3, subst0)
          TypedAst.Expression.IfThenElse(e1, e2, e3, subst0(tvar), Eff.Bot, loc)

        /*
         * Let expression.
         */
        case ResolvedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.Let(sym, e1, e2, subst0(tvar), Eff.Bot, loc)

        /*
         * LetRec expression.
         */
        case ResolvedAst.Expression.LetRec(sym, exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.LetRec(sym, e1, e2, subst0(tvar), Eff.Bot, loc)

        /*
         * Match expression.
         */
        case ResolvedAst.Expression.Match(exp1, rules, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val rs = rules map {
            case ResolvedAst.MatchRule(pat, guard, exp) =>
              val p = Patterns.reassemble(pat, program, subst0)
              val g = visitExp(guard, subst0)
              val b = visitExp(exp, subst0)
              TypedAst.MatchRule(p, g, b)
          }
          TypedAst.Expression.Match(e1, rs, subst0(tvar), Eff.Bot, loc)

        /*
         * Switch expression.
         */
        case ResolvedAst.Expression.Switch(rules, tvar, loc) =>
          val rs = rules.map {
            case (cond, body) => (visitExp(cond, subst0), visitExp(body, subst0))
          }
          TypedAst.Expression.Switch(rs, subst0(tvar), Eff.Bot, loc)

        /*
         * Tag expression.
         */
        case ResolvedAst.Expression.Tag(sym, tag, exp, tvar, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Tag(sym, tag, e, subst0(tvar), Eff.Bot, loc)

        /*
         * Tuple expression.
         */
        case ResolvedAst.Expression.Tuple(elms, tvar, loc) =>
          val es = elms.map(e => visitExp(e, subst0))
          TypedAst.Expression.Tuple(es, subst0(tvar), Eff.Bot, loc)

        /*
         * ArrayLit expression.
         */
        case ResolvedAst.Expression.ArrayLit(elms, tvar, loc) =>
          val es = elms.map(e => visitExp(e, subst0))
          TypedAst.Expression.ArrayLit(es, subst0(tvar), Eff.Bot, loc)

        /*
         * ArrayNew expression.
         */
        case ResolvedAst.Expression.ArrayNew(elm, len, tvar, loc) =>
          val e = visitExp(elm, subst0)
          val ln = visitExp(len, subst0)
          TypedAst.Expression.ArrayNew(e, ln, subst0(tvar), Eff.Bot, loc)

        /*
         * ArrayLoad expression.
         */
        case ResolvedAst.Expression.ArrayLoad(exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.ArrayLoad(e1, e2, subst0(tvar), Eff.Bot, loc)

        /*
         * ArrayStore expression.
         */
        case ResolvedAst.Expression.ArrayStore(exp1, exp2, exp3, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          val e3 = visitExp(exp3, subst0)
          TypedAst.Expression.ArrayStore(e1, e2, e3, subst0(tvar), Eff.Bot, loc)

        /*
         * ArrayLength expression.
         */
        case ResolvedAst.Expression.ArrayLength(exp, tvar, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.ArrayLength(e, subst0(tvar), Eff.Bot, loc)

        /*
         * ArraySlice expression.
         */
        case ResolvedAst.Expression.ArraySlice(exp1, exp2, exp3, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          val e3 = visitExp(exp3, subst0)
          TypedAst.Expression.ArraySlice(e1, e2, e3, subst0(tvar), Eff.Bot, loc)

        /*
         * VectorLit expression.
         */
        case ResolvedAst.Expression.VectorLit(elms, tvar, loc) =>
          val es = elms.map(e => visitExp(e, subst0))
          TypedAst.Expression.VectorLit(es, subst0(tvar), Eff.Bot, loc)

        /*
         * VectorLoad expression.
         */
        case ResolvedAst.Expression.VectorNew(elm, len, tvar, loc) =>
          val e = visitExp(elm, subst0)
          TypedAst.Expression.VectorNew(e, len, subst0(tvar), Eff.Bot, loc)

          /*
           * VectorLoad expression.
           */
         case ResolvedAst.Expression.VectorLoad(base, index, tvar, loc) =>
          val b = visitExp(base, subst0)
          TypedAst.Expression.VectorLoad(b, index, subst0(tvar), Eff.Bot, loc)

          /*
           * VectorStore expression.
           */
        case ResolvedAst.Expression.VectorStore(base, index, elm, tvar, loc) =>
          val b = visitExp(base, subst0)
          val e = visitExp(elm, subst0)
          TypedAst.Expression.VectorStore(b, index, e, subst0(tvar), Eff.Bot, loc)

          /*
           * VectorLength expression.
           */
        case ResolvedAst.Expression.VectorLength(base, tvar, loc) =>
          val b = visitExp(base, subst0)
          TypedAst.Expression.VectorLength(b, subst0(tvar), Eff.Bot, loc)

          /*
           * VectorSlice expression.
           */
        case ResolvedAst.Expression.VectorSlice(base, startIndex, optEndIndex, tvar, loc) =>
          val e = visitExp(base, subst0)
          optEndIndex match {
            case None =>
              val len = TypedAst.Expression.VectorLength(e, Type.Int32, Eff.Bot, loc)
              TypedAst.Expression.VectorSlice(e, startIndex, len , subst0(tvar), Eff.Bot, loc)
            case Some(endIndex) =>
              val len = TypedAst.Expression.Int32(endIndex, loc)
              TypedAst.Expression.VectorSlice(e, startIndex, len, subst0(tvar), Eff.Bot, loc)
          }

        /*
         * Reference expression.
         */
        case ResolvedAst.Expression.Ref(exp, tvar, loc) =>
          // TODO: Check if tvar is unbound.
          if (!(subst0.m contains tvar))
            throw InternalCompilerException("Unbound tvar in ref.")

          val e = visitExp(exp, subst0)
          TypedAst.Expression.Ref(e, subst0(tvar), Eff.Top, loc)

        /*
         * Dereference expression.
         */
        case ResolvedAst.Expression.Deref(exp, tvar, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Deref(e, subst0(tvar), Eff.Top, loc)

        /*
         * Assignment expression.
         */
        case ResolvedAst.Expression.Assign(exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.Assign(e1, e2, subst0(tvar), Eff.Top, loc)

        /*
         * HandleWith expression.
         */
        case ResolvedAst.Expression.HandleWith(exp, bindings, tvar, loc) =>
          val e = visitExp(exp, subst0)
          val bs = bindings map {
            case ResolvedAst.HandlerBinding(sym, handler) => TypedAst.HandlerBinding(sym, visitExp(handler, subst0))
          }
          TypedAst.Expression.HandleWith(e, bs, subst0(tvar), Eff.Top, loc)

        /*
         * Existential expression.
         */
        case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Existential(visitParam(fparam), e, Eff.Bot, loc)

        /*
         * Universal expression.
         */
        case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Universal(visitParam(fparam), e, Eff.Bot, loc)

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
         * Native Constructor expression.
         */
        case ResolvedAst.Expression.NativeConstructor(constructor, actuals, tpe, loc) =>
          val es = actuals.map(e => reassemble(e, program, subst0))
          TypedAst.Expression.NativeConstructor(constructor, es, subst0(tpe), Eff.Bot, loc)

        /*
         * Native Field expression.
         */
        case ResolvedAst.Expression.NativeField(field, tpe, loc) =>
          TypedAst.Expression.NativeField(field, subst0(tpe), Eff.Bot, loc)

        /*
         * Native Method expression.
         */
        case ResolvedAst.Expression.NativeMethod(method, actuals, tpe, loc) =>
          val es = actuals.map(e => reassemble(e, program, subst0))
          TypedAst.Expression.NativeMethod(method, es, subst0(tpe), Eff.Bot, loc)

        /*
         * User Error expression.
         */
        case ResolvedAst.Expression.UserError(tvar, loc) =>
          TypedAst.Expression.UserError(subst0(tvar), Eff.Bot, loc)
      }

      /**
        * Applies the substitution to the given list of formal parameters.
        */
      def visitParam(param: ResolvedAst.FormalParam): TypedAst.FormalParam =
        TypedAst.FormalParam(param.sym, param.mod, subst0(param.tpe), param.loc)

      visitExp(exp0, subst0)
    }
  }

  object Patterns {

    /**
      * Infers the type of the given pattern `pat0`.
      */
    def infer(pat0: ResolvedAst.Pattern, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = {
      /**
        * Local pattern visitor.
        */
      def visit(p: ResolvedAst.Pattern): InferMonad[Type] = p match {
        case ResolvedAst.Pattern.Wild(tvar, loc) => liftM(tvar)
        case ResolvedAst.Pattern.Var(sym, tvar, loc) => unifyM(sym.tvar, tvar, loc)
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
    def inferAll(pats0: List[ResolvedAst.Pattern], program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[List[Type]] = {
      seqM(pats0.map(p => infer(p, program)))
    }

    /**
      * Applies the substitution `subst0` to the given pattern `pat0`.
      */
    def reassemble(pat0: ResolvedAst.Pattern, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Pattern = {
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

  }

  object Predicates {

    /**
      * Infers the type of the given head predicate.
      */
    def infer(head: ResolvedAst.Predicate.Head, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[List[Type]] = head match {
      case ResolvedAst.Predicate.Head.True(loc) => Unification.liftM(Nil)
      case ResolvedAst.Predicate.Head.False(loc) => Unification.liftM(Nil)
      case ResolvedAst.Predicate.Head.Atom(sym, terms, loc) =>
        getTableSignature(sym, program) match {
          case Ok(declaredTypes) => Terms.Head.typecheck(terms, declaredTypes, loc, program)
          case Err(e) => failM(e)
        }
    }

    /**
      * Infers the type of the given body predicate.
      */
    def infer(body0: ResolvedAst.Predicate.Body, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[List[Type]] = body0 match {
      case ResolvedAst.Predicate.Body.Atom(sym, polarity, terms, loc) =>
        getTableSignature(sym, program) match {
          case Ok(declaredTypes) => Terms.Body.typecheck(terms, declaredTypes, loc, program)
          case Err(e) => failM(e)
        }
      case ResolvedAst.Predicate.Body.Filter(sym, terms, loc) =>
        val defn = program.defs(sym)
        val expectedTypes = defn.fparams.map(_.tpe)
        for (
          actualTypes <- seqM(terms.map(t => Expressions.infer(t, program)));
          unifiedTypes <- Unification.unifyM(expectedTypes, actualTypes, loc)
        ) yield unifiedTypes

      case ResolvedAst.Predicate.Body.Loop(pat, term, loc) =>
        // TODO: Assumes that pat is a variable symbol.
        val sym = pat.asInstanceOf[ResolvedAst.Pattern.Var].sym
        for {
          tpe <- Expressions.infer(term, program)
          ___ <- unifyM(Type.mkFSet(sym.tvar), tpe, loc)
        } yield List(tpe)
    }

    /**
      * Applies the given substitution `subst0` to the given head predicate `head0`.
      */
    def reassemble(head0: ResolvedAst.Predicate.Head, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
      case ResolvedAst.Predicate.Head.True(loc) => TypedAst.Predicate.Head.True(loc)
      case ResolvedAst.Predicate.Head.False(loc) => TypedAst.Predicate.Head.False(loc)
      case ResolvedAst.Predicate.Head.Atom(sym, terms, loc) =>
        val ts = terms.map(t => Expressions.reassemble(t, program, subst0))
        TypedAst.Predicate.Head.Atom(sym, ts, loc)
    }

    /**
      * Applies the given substitution `subst0` to the given body predicate `body0`.
      */
    def reassemble(body0: ResolvedAst.Predicate.Body, program: ResolvedAst.Program, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
      case ResolvedAst.Predicate.Body.Atom(sym, polarity, terms, loc) =>
        val ts = terms.map(t => Patterns.reassemble(t, program, subst0))
        TypedAst.Predicate.Body.Atom(sym, polarity, ts, loc)
      case ResolvedAst.Predicate.Body.Filter(sym, terms, loc) =>
        val defn = program.defs(sym)
        val ts = terms.map(t => Expressions.reassemble(t, program, subst0))
        TypedAst.Predicate.Body.Filter(defn.sym, ts, loc)
      case ResolvedAst.Predicate.Body.Loop(pat, term, loc) =>
        // TODO: Assumes that the pattern is a single variable.
        val p = pat.asInstanceOf[ResolvedAst.Pattern.Var]
        val t = Expressions.reassemble(term, program, subst0)
        TypedAst.Predicate.Body.Loop(p.sym, t, loc)
    }

  }

  object Terms {

    object Head {
      /**
        * Infers the type of the given `terms` and checks them against the types `ts`.
        */
      def typecheck(terms: List[ResolvedAst.Expression], ts: List[Type], loc: SourceLocation, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[List[Type]] = {
        for (
          actualTypes <- seqM(terms.map(t => Expressions.infer(t, program)));
          unifiedTypes <- Unification.unifyM(ts, actualTypes, loc)
        ) yield unifiedTypes
      }
    }

    object Body {
      /**
        * Infers the type of the given `terms` and checks them against the types `ts`.
        */
      def typecheck(terms: List[ResolvedAst.Pattern], ts: List[Type], loc: SourceLocation, program: ResolvedAst.Program)(implicit genSym: GenSym): InferMonad[List[Type]] = {
        for (
          actualTypes <- seqM(terms.map(t => Patterns.infer(t, program)));
          unifiedTypes <- Unification.unifyM(ts, actualTypes, loc)
        ) yield unifiedTypes
      }
    }

  }

  /**
    * Returns the declared types of the terms of the given fully-qualified table name `qname`.
    */
  def getTableSignature(sym: Symbol.TableSym, program: ResolvedAst.Program): Result[List[Type], TypeError] = {
    program.tables(sym) match {
      case ResolvedAst.Table.Relation(_, _, attr, _) => Ok(attr.map(_.tpe))
      case ResolvedAst.Table.Lattice(_, _, keys, value, _) => Ok(keys.map(_.tpe) ::: value.tpe :: Nil)
    }
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    */
  def getSubstFromParams(params: List[ResolvedAst.FormalParam]): Unification.Substitution = {
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
  def getTypeParams(tparams0: List[ResolvedAst.TypeParam]): List[TypedAst.TypeParam] = tparams0.map {
    case ResolvedAst.TypeParam(name, tpe, loc) => TypedAst.TypeParam(name, tpe, loc)
  }

  /**
    * Returns the typed version of the given formal parameters `fparams0`.
    */
  def getFormalParams(fparams0: List[ResolvedAst.FormalParam], subst0: Unification.Substitution) = fparams0.map {
    case ResolvedAst.FormalParam(sym, mod, tpe, loc) => TypedAst.FormalParam(sym, mod, subst0(sym.tvar), sym.loc)
  }

  def getTypeFromField(field: Field): Type = ??? // TODO

}
