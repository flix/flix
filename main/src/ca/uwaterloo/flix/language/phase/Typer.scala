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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.NamedAst.Program
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.{ResolutionError, TypeError}
import ca.uwaterloo.flix.language.phase.Disambiguation.RefTarget
import ca.uwaterloo.flix.language.phase.Unification._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object Typer {

  /**
    * Type checks the given program.
    */
  def typer(program: NamedAst.Program)(implicit genSym: GenSym): Validation[TypedAst.Root, TypeError] = {
    val startTime = System.nanoTime()

    val result = for {
      definitions <- Declarations.Definitions.typecheck(program)
      enums <- Declarations.Enums.typecheck(program)
      lattices <- Declarations.Lattices.typecheck(program)
      tables <- Declarations.Tables.typecheck(program)
      indexes <- Declarations.Indexes.typecheck(program)
      facts <- Declarations.Constraints.typecheckFacts(program)
      rules <- Declarations.Constraints.typecheckRules(program)
    } yield {
      val currentTime = System.nanoTime()
      val time = program.time.copy(typer = currentTime - startTime)
      TypedAst.Root(definitions, enums, lattices, tables, indexes, facts, rules, Nil, time)
    }

    result match {
      case Ok(p) => p.toSuccess
      case Err(e) => e.toFailure
    }
  }

  object Declarations {

    object Constraints {

      /**
        * Performs type inference and reassembly on all facts in the given program.
        *
        * Returns [[Err]] if the head predicate fails to type check.
        */
      def typecheckFacts(program: Program)(implicit genSym: GenSym): Result[List[TypedAst.Declaration.Fact], TypeError] = {

        /**
          * Performs type checking on the given `fact` in the given namespace `ns`.
          */
        def visitFact(fact: NamedAst.Declaration.Fact, ns: Name.NName): Result[TypedAst.Declaration.Fact, TypeError] = fact match {
          case NamedAst.Declaration.Fact(head, loc) =>
            Predicates.typecheck(head, ns, program) map {
              case h => TypedAst.Declaration.Fact(h, loc)
            }
        }

        // Visit every fact in the program.
        val result = program.facts.toList.flatMap {
          case (ns, facts) => facts.map(f => visitFact(f, ns))
        }

        // Sequence the results and convert them back to a map.
        Result.seqM(result)
      }

      // TODO: DOC
      def typecheckRules(program: Program)(implicit genSym: GenSym): Result[List[TypedAst.Declaration.Rule], TypeError] = {
        val rules = program.rules.flatMap {
          case (ns, rs) => rs map {
            case NamedAst.Declaration.Rule(head0, body0, loc) =>
              Predicates.infer(head0, ns, program) match {
                case InferMonad(run1) =>
                  val (subst, _) = run1(Substitution.empty).get
                  val head = Predicates.reassemble(head0, ns, program, subst)
                  val body = body0.map {
                    case b => Predicates.infer(b, ns, program) match {
                      case InferMonad(run2) =>
                        val (subst2, _) = run2(Substitution.empty).get
                        Predicates.reassemble(b, ns, program, subst2)
                    }
                  }
                  TypedAst.Declaration.Rule(head, body, loc)
              }
          }
        }
        Ok(rules.toList)
      }

    }

    object Definitions {

      /**
        * Performs type inference and reassembly on all definitions in the given program.
        *
        * Returns [[Err]] if a definition fails to type check.
        */
      def typecheck(program: Program)(implicit genSym: GenSym): Result[Map[Symbol.DefnSym, TypedAst.Declaration.Definition], TypeError] = {
        /**
          * Performs type inference and reassembly on the given definition `defn` in the given namespace `ns`.
          */
        def visitDefn(defn: NamedAst.Declaration.Definition, ns: Name.NName): Result[(Symbol.DefnSym, TypedAst.Declaration.Definition), TypeError] = defn match {
          case NamedAst.Declaration.Definition(doc, ann, sym, tparams, params, exp, tpe, loc) =>
            infer(defn, ns, program) map {
              case d => sym -> d
            }
        }

        // Visit every definition in the program.
        val result = program.definitions.toList.flatMap {
          case (ns, defns) => defns.map {
            case (name, defn) => visitDefn(defn, ns)
          }
        }

        // Sequence the results and convert them back to a map.
        Result.seqM(result).map(_.toMap)
      }

      /**
        * Infers the type of the given definition `defn0` in the given namespace `ns0`.
        */
      // TODO: Cleanup
      def infer(defn0: NamedAst.Declaration.Definition, ns0: Name.NName, program: NamedAst.Program)(implicit genSym: GenSym): Result[TypedAst.Declaration.Definition, TypeError] = {
        // Resolve the declared scheme.
        val declaredScheme = Disambiguation.resolve(defn0.sc, ns0, program) match {
          case Ok(scheme) => scheme
          case Err(e) => return Err(e)
        }

        // TODO: Some duplication
        val argumentTypes = Disambiguation.resolve(defn0.params.map(_.tpe), ns0, program) match {
          case Ok(tpes) => tpes
          case Err(e) => return Err(e)
        }

        val result = for (
          resultType <- Expressions.infer(defn0.exp, ns0, program);
          unifiedType <- unifyM(Scheme.instantiate(declaredScheme), Type.mkArrow(argumentTypes, resultType), defn0.loc)
        ) yield unifiedType

        // TODO: See if this can be rewritten nicer
        result match {
          case InferMonad(run) =>
            val subst = getSubstFromParams(defn0.params, ns0, program).get
            run(subst) match {
              case Ok((subst0, resultType)) =>
                val exp = Expressions.reassemble(defn0.exp, ns0, program, subst0)

                val tparams = defn0.tparams.map {
                  case NamedAst.TypeParam(name, tpe, loc) =>
                    TypedAst.TypeParam(name, tpe, loc)
                }

                // Translate the named formals into typed formals.
                val formals = defn0.params.map {
                  case NamedAst.FormalParam(sym, tpe, loc) =>
                    TypedAst.FormalParam(sym, subst0(sym.tvar), sym.loc)
                }

                Ok(TypedAst.Declaration.Definition(defn0.doc, defn0.ann, defn0.sym, tparams, formals, exp, resultType, defn0.loc))

              case Err(e) => Err(e)
            }
        }
      }

    }

    object Enums {
      /**
        * Performs type inference and reassembly on all enums in the given program.
        */
      def typecheck(program: Program)(implicit genSym: GenSym): Result[Map[Symbol.EnumSym, TypedAst.Declaration.Enum], TypeError] = {
        /**
          * Performs type resolution on the given enum and its cases.
          */
        def visitEnum(enum: NamedAst.Declaration.Enum, ns: Name.NName): Result[(Symbol.EnumSym, TypedAst.Declaration.Enum), TypeError] = enum match {
          case NamedAst.Declaration.Enum(doc, sym, tparams, cases0, tpe, loc) =>
            val casesResult = cases0 map {
              case (name, NamedAst.Case(enumName, tagName, caseType)) =>
                Disambiguation.resolve(caseType, ns, program).map {
                  case t => name -> TypedAst.Case(enumName, tagName, t)
                }
            }

            for {
              enumType <- Disambiguation.resolve(tpe, ns, program)
              cases <- Result.seqM(casesResult.toList)
            } yield sym -> TypedAst.Declaration.Enum(doc, sym, cases.toMap, enumType, loc)
        }

        // Visit every enum in the program.
        val result = program.enums.toList.flatMap {
          case (ns, enums) => enums.map {
            case (name, enum) => visitEnum(enum, ns)
          }
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
      def typecheck(program: Program): Result[Map[Symbol.TableSym, TypedAst.Declaration.Index], TypeError] = {

        /**
          * Checks that the referenced table exists and that every attribute used by the index exists.
          */
        def visitIndex(index: NamedAst.Declaration.Index, ns: Name.NName): Result[(Symbol.TableSym, TypedAst.Declaration.Index), TypeError] = index match {
          case NamedAst.Declaration.Index(qname, indexes, loc) =>
            // Lookup the referenced table.
            Disambiguation.lookupTable(qname, ns, program) flatMap {
              case table =>
                val declaredAttributes = table.attr.map(_.ident.name)
                // Iterate through every index in the declaration.
                for (index <- indexes) {
                  // Iterate through every attribute name in the current index.
                  for (referencedAttribute <- index) {
                    if (!(declaredAttributes contains referencedAttribute.name)) {
                      return Err(ResolutionError.UndefinedAttribute(table.sym.name, referencedAttribute.name, referencedAttribute.loc))
                    }
                  }
                }
                Ok(table.sym -> TypedAst.Declaration.Index(table.sym, indexes, loc))
            }
        }

        // Visit every index in the program.
        val result = program.indexes.toList.flatMap {
          case (ns, indexes) => indexes.map {
            case (name, index) => visitIndex(index, ns)
          }
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
      def typecheck(program: Program)(implicit genSym: GenSym): Result[Map[Type, TypedAst.Declaration.BoundedLattice], TypeError] = {

        /**
          * Performs type inference and reassembly on the given `lattice`.
          */
        def visitLattice(lattice: NamedAst.Declaration.BoundedLattice): Result[(Type, TypedAst.Declaration.BoundedLattice), TypeError] = lattice match {
          case NamedAst.Declaration.BoundedLattice(tpe, e1, e2, e3, e4, e5, ns, loc) =>
            // Perform type resolution on the declared type.
            Disambiguation.resolve(tpe, ns, program) flatMap {
              case declaredType =>

                // Perform type inference on each of the lattice components.
                val m = for {
                  botType <- Expressions.infer(e1, ns, program)
                  topType <- Expressions.infer(e2, ns, program)
                  leqType <- Expressions.infer(e3, ns, program)
                  lubType <- Expressions.infer(e4, ns, program)
                  glbType <- Expressions.infer(e5, ns, program)
                  _______ <- unifyM(botType, declaredType, loc)
                  _______ <- unifyM(topType, declaredType, loc)
                  _______ <- unifyM(leqType, Type.mkArrow(List(declaredType, declaredType), Type.Bool), loc)
                  _______ <- unifyM(lubType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
                  _______ <- unifyM(glbType, Type.mkArrow(List(declaredType, declaredType), declaredType), loc)
                } yield declaredType

                // Evaluate the type inference monad with the empty substitution
                m.run(Substitution.empty) map {
                  case (subst, _) =>
                    // Reassemble the lattice components.
                    val bot = Expressions.reassemble(e1, ns, program, subst)
                    val top = Expressions.reassemble(e2, ns, program, subst)
                    val leq = Expressions.reassemble(e3, ns, program, subst)
                    val lub = Expressions.reassemble(e4, ns, program, subst)
                    val glb = Expressions.reassemble(e5, ns, program, subst)

                    declaredType -> TypedAst.Declaration.BoundedLattice(declaredType, bot, top, leq, lub, glb, loc)
                }
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
      def typecheck(program: Program): Result[Map[Symbol.TableSym, TypedAst.Table], TypeError] = {

        /**
          * Performs type resolution on the given `table` declared in the namespace `ns`.
          *
          * Returns [[Err]] if a type is unresolved.
          */
        def visitTable(table: NamedAst.Table, ns: Name.NName): Result[(Symbol.TableSym, TypedAst.Table), TypeError] = table match {
          case NamedAst.Table.Relation(doc, sym, attr, loc) =>
            for (typedAttributes <- Result.seqM(attr.map(a => visitAttribute(a, ns))))
              yield sym -> TypedAst.Table.Relation(doc, sym, typedAttributes, loc)
          case NamedAst.Table.Lattice(doc, sym, keys, value, loc) =>
            for {
              typedKeys <- Result.seqM(keys.map(a => visitAttribute(a, ns)))
              typedVal <- visitAttribute(value, ns)
            } yield sym -> TypedAst.Table.Lattice(doc, sym, typedKeys, typedVal, loc)
        }

        /**
          * Performs type resolution on the given attribute `attr` declared in the namespace `ns`.
          */
        def visitAttribute(attr: NamedAst.Attribute, ns: Name.NName): Result[TypedAst.Attribute, TypeError] = attr match {
          case NamedAst.Attribute(ident, tpe, loc) =>
            // Resolve the declared type.
            Disambiguation.resolve(tpe, ns, program) map {
              case resolvedType => TypedAst.Attribute(ident.name, resolvedType, loc)
            }
        }

        // Visit every table in the program.
        val result = program.tables.toList.flatMap {
          case (ns, tables) => tables.map {
            case (name, table) => visitTable(table, ns)
          }
        }

        // Sequence the results and convert them back to a map.
        Result.seqM(result).map(_.toMap)
      }

    }

  }

  object Expressions {

    /**
      * Infers the type of the given expression `exp0` in the namespace `ns0` and `program`.
      */
    def infer(exp0: NamedAst.Expression, ns0: Name.NName, program: NamedAst.Program)(implicit genSym: GenSym): InferMonad[Type] = {

      /**
        * Infers the type of the given expression `exp0` inside the inference monad.
        */
      def visitExp(e0: NamedAst.Expression): InferMonad[Type] = e0 match {

        /*
         * Wildcard expression.
         */
        case NamedAst.Expression.Wild(tpe, loc) => liftM(tpe)

        /*
         * Variable expression.
         */
        case NamedAst.Expression.Var(sym, loc) => liftM(sym.tvar)

        /*
         * Reference expression.
         */
        case NamedAst.Expression.Ref(ref, tvar, loc) =>
          Disambiguation.lookupRef(ref, ns0, program) match {
            case Ok(RefTarget.Defn(ns, defn)) =>
              Disambiguation.resolve(defn.sc, ns, program) match {
                case Ok(scheme) => unifyM(tvar, Scheme.instantiate(scheme), loc)
                case Err(e) => failM(e)
              }
            case Ok(RefTarget.Hook(hook)) => liftM(hook.tpe)
            case Err(e) => failM(e)
          }

        /*
         * Literal expression.
         */
        case NamedAst.Expression.Unit(loc) => liftM(Type.Unit)
        case NamedAst.Expression.True(loc) => liftM(Type.Bool)
        case NamedAst.Expression.False(loc) => liftM(Type.Bool)
        case NamedAst.Expression.Char(lit, loc) => liftM(Type.Char)
        case NamedAst.Expression.Float32(lit, loc) => liftM(Type.Float32)
        case NamedAst.Expression.Float64(lit, loc) => liftM(Type.Float64)
        case NamedAst.Expression.Int8(lit, loc) => liftM(Type.Int8)
        case NamedAst.Expression.Int16(lit, loc) => liftM(Type.Int16)
        case NamedAst.Expression.Int32(lit, loc) => liftM(Type.Int32)
        case NamedAst.Expression.Int64(lit, loc) => liftM(Type.Int64)
        case NamedAst.Expression.BigInt(lit, loc) => liftM(Type.BigInt)
        case NamedAst.Expression.Str(lit, loc) => liftM(Type.Str)

        /*
         * Lambda expression.
         */
        case NamedAst.Expression.Lambda(args, body, tvar, loc) =>
          val argumentTypes = args.map(_.tvar)
          for (
            inferredBodyType <- visitExp(body);
            resultType <- unifyM(tvar, Type.mkArrow(argumentTypes, inferredBodyType), loc)
          ) yield resultType

        /*
         * Apply expression.
         */
        case NamedAst.Expression.Apply(lambda, actuals, tvar, loc) =>
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
        case NamedAst.Expression.Unary(op, exp1, tvar, loc) => op match {
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
        case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) => op match {
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

          case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr | BinaryOperator.Implication | BinaryOperator.Biconditional =>
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
        case NamedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            boundVar <- unifyM(sym.tvar, tpe1, loc);
            resultVar <- unifyM(tvar, tpe2, loc)
          ) yield resultVar

        /*
         * If-then-else expression.
         */
        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
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
        case NamedAst.Expression.Match(exp1, rules, tvar, loc) =>
          assert(rules.nonEmpty)
          val patterns = rules.map(_._1)
          val bodies = rules.map(_._2)
          for {
            matchType <- visitExp(exp1)
            patternTypes <- seqM(patterns map visitPat)
            patternType <- unifyM(patternTypes, loc)
            ___________ <- unifyM(matchType, patternType, loc)
            actualBodyTypes <- seqM(bodies map visitExp)
            resultType <- unifyM(tvar :: actualBodyTypes, loc)
          } yield resultType

        /*
           * Switch expression.
           */
        case NamedAst.Expression.Switch(rules, tvar, loc) =>
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
        case NamedAst.Expression.Tag(enum, tag, exp, tvar, loc) =>
          Disambiguation.lookupEnumByTag(enum, tag, ns0, program) match {
            case Ok(decl) =>
              // Generate a fresh type variable for each type parameters.
              val subst = Substitution(decl.tparams.map {
                case param => param.tpe -> Type.freshTypeVar()
              }.toMap)

              // Resolve the enum type.
              Disambiguation.resolve(decl.tpe, ns0, program) match {
                case Ok(enumType) =>
                  // Substitute the fresh type variables into the enum type.
                  val freshEnumType = subst(enumType)
                  // Resolve the case type.
                  Disambiguation.resolve(decl.cases(tag.name).tpe, ns0, program) match {
                    case Ok(caseType) =>
                      // Substitute the fresh type variables into the case type.
                      val freshCaseType = subst(caseType)
                      for (
                        innerType <- visitExp(exp);
                        _________ <- unifyM(innerType, freshCaseType, loc);
                        resultType <- unifyM(tvar, freshEnumType, loc)
                      ) yield resultType
                    case Err(e) => failM(e)
                  }
                case Err(e) => failM(e)
              }
            case Err(e) => failM(e)
          }

        /*
         * Tuple expression.
         */
        case NamedAst.Expression.Tuple(elms, tvar, loc) =>
          for (
            elementTypes <- seqM(elms.map(visitExp));
            resultType <- unifyM(tvar, Type.mkFTuple(elementTypes), loc)
          ) yield resultType

        /*
         * Vector expression.
         */
        case NamedAst.Expression.FVec(elms, tvar, loc) =>
          val elementType = Type.freshTypeVar()
          for (
            actualTypes <- seqM(elms map visitExp);
            unifiedType <- unifyM(elementType :: actualTypes, loc);
            resultType <- unifyM(tvar, Type.mkFVec(unifiedType), loc)
          ) yield resultType

        /*
         * Set expression.
         */
        case NamedAst.Expression.FSet(elms, tvar, loc) =>
          val elementType = Type.freshTypeVar()
          for (
            actualTypes <- seqM(elms map visitExp);
            unifiedType <- unifyM(elementType :: actualTypes, loc);
            resultType <- unifyM(tvar, Type.mkFSet(unifiedType), loc)
          ) yield resultType

        /*
         * Map expression.
         */
        case NamedAst.Expression.FMap(elms, tvar, loc) =>
          val keyType = Type.freshTypeVar()
          val valType = Type.freshTypeVar()

          val keys = elms.map(_._1)
          val vals = elms.map(_._2)

          for (
            actualKeyTypes <- seqM(keys map visitExp);
            actualValTypes <- seqM(vals map visitExp);
            unifiedKeyType <- unifyM(keyType :: actualKeyTypes, loc);
            unifiedValType <- unifyM(valType :: actualValTypes, loc);
            resultType <- unifyM(tvar, Type.mkFMap(unifiedKeyType, unifiedValType), loc)
          ) yield resultType

        /*
         * GetIndex expression.
         */
        case NamedAst.Expression.GetIndex(exp1, exp2, tvar, loc) =>
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            ____ <- unifyM(tpe1, Type.mkFVec(tvar), loc);
            ____ <- unifyM(tpe2, Type.Int32, loc)
          ) yield tvar

        /*
         * PutIndex expression.
         */
        case NamedAst.Expression.PutIndex(exp1, exp2, exp3, tvar, loc) =>
          val elementType = Type.freshTypeVar()
          for (
            tpe1 <- visitExp(exp1);
            tpe2 <- visitExp(exp2);
            tpe3 <- visitExp(exp3);
            ____ <- unifyM(tpe2, Type.Int32, loc);
            ____ <- unifyM(tpe3, elementType, loc);
            resultType <- unifyM(tvar, tpe1, Type.mkFVec(elementType), loc)
          ) yield resultType

        /*
         * Existential expression.
         */
        case NamedAst.Expression.Existential(params, exp, loc) =>
          // NB: An existential behaves very much like a lambda.
          for {
            bodyType <- visitExp(exp)
            resultType <- unifyM(bodyType, Type.Bool, loc)
          } yield resultType

        /*
         * Universal expression.
         */
        case NamedAst.Expression.Universal(params, exp, loc) =>
          // NB: An existential behaves very much like a lambda.
          for {
            bodyType <- visitExp(exp)
            resultType <- unifyM(bodyType, Type.Bool, loc)
          } yield resultType

        /*
         * Ascribe expression.
         */
        case NamedAst.Expression.Ascribe(exp, expectedType, loc) =>
          Disambiguation.resolve(expectedType, ns0, program) match {
            case Ok(resolvedType) =>
              for (
                actualType <- visitExp(exp);
                resultType <- unifyM(actualType, resolvedType, loc)
              ) yield resultType
            case Err(e) => failM(e)
          }

        /*
         * User Error expression.
         */
        case NamedAst.Expression.UserError(tvar, loc) => liftM(tvar)

      }

      /**
        * Infers the type of the given pattern `pat0`.
        */
      def visitPat(pat0: NamedAst.Pattern): InferMonad[Type] = pat0 match {
        case NamedAst.Pattern.Wild(tvar, loc) => liftM(tvar)
        case NamedAst.Pattern.Var(sym, tvar, loc) => unifyM(sym.tvar, tvar, loc)
        case NamedAst.Pattern.Unit(loc) => liftM(Type.Unit)
        case NamedAst.Pattern.True(loc) => liftM(Type.Bool)
        case NamedAst.Pattern.False(loc) => liftM(Type.Bool)
        case NamedAst.Pattern.Char(c, loc) => liftM(Type.Char)
        case NamedAst.Pattern.Float32(i, loc) => liftM(Type.Float32)
        case NamedAst.Pattern.Float64(i, loc) => liftM(Type.Float64)
        case NamedAst.Pattern.Int8(i, loc) => liftM(Type.Int8)
        case NamedAst.Pattern.Int16(i, loc) => liftM(Type.Int16)
        case NamedAst.Pattern.Int32(i, loc) => liftM(Type.Int32)
        case NamedAst.Pattern.Int64(i, loc) => liftM(Type.Int64)
        case NamedAst.Pattern.BigInt(i, loc) => liftM(Type.BigInt)
        case NamedAst.Pattern.Str(s, loc) => liftM(Type.Str)
        case NamedAst.Pattern.Tag(enum, tag, pat, tvar, loc) =>
          Disambiguation.lookupEnumByTag(enum, tag, ns0, program) match {
            case Ok(decl) =>
              // Generate a fresh type variable for each type parameters.
              val subst = Substitution(decl.tparams.map {
                case param => param.tpe -> Type.freshTypeVar()
              }.toMap)

              // Resolve the enum type.
              Disambiguation.resolve(decl.tpe, ns0, program) match {
                case Ok(enumType) =>
                  // Substitute the fresh type variables into the enum type.
                  val freshEnumType = subst(enumType)
                  // Resolve the case type.
                  Disambiguation.resolve(decl.cases(tag.name).tpe, ns0, program) match {
                    case Ok(caseType) =>
                      // Substitute the fresh type variables into the case type.
                      val freshCaseType = subst(caseType)
                      for (
                        innerType <- visitPat(pat);
                        _________ <- unifyM(innerType, freshCaseType, loc);
                        resultType <- unifyM(tvar, freshEnumType, loc)
                      ) yield resultType
                    case Err(e) => failM(e)
                  }
                case Err(e) => failM(e)
              }
            case Err(e) => failM(e)
          }
        case NamedAst.Pattern.Tuple(elms, tvar, loc) =>
          for (
            elementTypes <- seqM(elms map visitPat);
            resultType <- unifyM(tvar, Type.mkFTuple(elementTypes), loc)
          ) yield resultType
        case NamedAst.Pattern.FVec(elms, rest, tvar, loc) =>
          // Introduce a fresh type variable for the type of the elements in the vector.
          val elementType = Type.freshTypeVar()

          // Perform type inference for the elements.
          val m = for {
            inferredElementTypes <- seqM(elms.map(visitPat))
            unifiedElementType <- unifyM(elementType :: inferredElementTypes, loc)
            resultType <- unifyM(tvar, Type.mkFVec(elementType), loc)
          } yield resultType

          // Check if there is a rest, if so unify it with the result type.
          rest match {
            case None => m
            case Some(remainder) =>
              for {
                resultType <- m
                inferredRemainderType <- visitPat(remainder)
                unifiedType <- unifyM(inferredRemainderType, resultType, loc)
              } yield unifiedType
          }
        case NamedAst.Pattern.FSet(elms, rest, tvar, loc) =>
          ??? // TODO: FSet

        case NamedAst.Pattern.FMap(elms, rest, tvar, loc) =>
          ??? // TODO: FMap
      }

      visitExp(exp0)
    }

    /**
      * Applies the given substitution `subst0` to the given expression `exp0` in the given namespace `ns0`.
      */
    def reassemble(exp0: NamedAst.Expression, ns0: Name.NName, program: Program, subst0: Substitution, resolveFreeVars: Boolean = false): TypedAst.Expression = {
      /**
        * Applies the given substitution `subst0` to the given expression `exp0`.
        */
      def visitExp(exp0: NamedAst.Expression, subst0: Substitution): TypedAst.Expression = exp0 match {
        /*
         * Wildcard expression.
         */
        case NamedAst.Expression.Wild(tvar, loc) => TypedAst.Expression.Wild(subst0(tvar), loc)

        /*
         * Variable expression.
         */
        case NamedAst.Expression.Var(sym, loc) =>
          // TODO: Need better strategy for when to resolve free variables.
          if (!resolveFreeVars) {
            TypedAst.Expression.Var(sym, subst0(sym.tvar), loc)
          } else {
            val qname = Name.mkQName(sym.text)
            Disambiguation.lookupRef(qname, ns0, program) match {
              case Ok(RefTarget.Defn(ns, defn)) => TypedAst.Expression.Ref(defn.sym, subst0(sym.tvar), loc)
              case Ok(RefTarget.Hook(hook)) => TypedAst.Expression.Hook(hook, subst0(sym.tvar), loc)
              case Err(e) => TypedAst.Expression.Var(sym, subst0(sym.tvar), loc)
            }
          }

        /*
         * Reference expression.
         */
        case NamedAst.Expression.Ref(qname, tvar, loc) =>
          Disambiguation.lookupRef(qname, ns0, program).get match {
            case RefTarget.Defn(ns, defn) => TypedAst.Expression.Ref(defn.sym, subst0(tvar), loc)
            case RefTarget.Hook(hook) => TypedAst.Expression.Hook(hook, hook.tpe, loc)
          }

        /*
         * Literal expression.
         */
        case NamedAst.Expression.Unit(loc) => TypedAst.Expression.Unit(loc)
        case NamedAst.Expression.True(loc) => TypedAst.Expression.True(loc)
        case NamedAst.Expression.False(loc) => TypedAst.Expression.False(loc)
        case NamedAst.Expression.Char(lit, loc) => TypedAst.Expression.Char(lit, loc)
        case NamedAst.Expression.Float32(lit, loc) => TypedAst.Expression.Float32(lit, loc)
        case NamedAst.Expression.Float64(lit, loc) => TypedAst.Expression.Float64(lit, loc)
        case NamedAst.Expression.Int8(lit, loc) => TypedAst.Expression.Int8(lit, loc)
        case NamedAst.Expression.Int16(lit, loc) => TypedAst.Expression.Int16(lit, loc)
        case NamedAst.Expression.Int32(lit, loc) => TypedAst.Expression.Int32(lit, loc)
        case NamedAst.Expression.Int64(lit, loc) => TypedAst.Expression.Int64(lit, loc)
        case NamedAst.Expression.BigInt(lit, loc) => TypedAst.Expression.BigInt(lit, loc)
        case NamedAst.Expression.Str(lit, loc) => TypedAst.Expression.Str(lit, loc)

        /*
         * Apply expression.
         */
        case NamedAst.Expression.Apply(lambda, actuals, tvar, loc) =>
          val l = visitExp(lambda, subst0)
          val as = actuals.map(e => visitExp(e, subst0))
          TypedAst.Expression.Apply(l, as, subst0(tvar), loc)

        /*
         * Lambda expression.
         */
        case NamedAst.Expression.Lambda(params, exp, tvar, loc) =>
          val lambdaArgs = params map {
            case sym => TypedAst.FormalParam(sym, subst0(sym.tvar), sym.loc)
          }
          val lambdaBody = visitExp(exp, subst0)
          val lambdaType = subst0(tvar)
          TypedAst.Expression.Lambda(lambdaArgs, lambdaBody, lambdaType, loc)

        /*
         * Unary expression.
         */
        case NamedAst.Expression.Unary(op, exp, tvar, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Unary(op, e, subst0(tvar), loc)

        /*
         * Binary expression.
         */
        case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.Binary(op, e1, e2, subst0(tvar), loc)

        /*
         * If-then-else expression.
         */
        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          val e3 = visitExp(exp3, subst0)
          TypedAst.Expression.IfThenElse(e1, e2, e3, subst0(tvar), loc)

        /*
         * Let expression.
         */
        case NamedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.Let(sym, e1, e2, subst0(tvar), loc)

        /*
         * Match expression.
         */
        case NamedAst.Expression.Match(exp1, rules, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val rs = rules map {
            case (pat, exp) => visitPat(pat) -> visitExp(exp, subst0)
          }
          TypedAst.Expression.Match(e1, rs, subst0(tvar), loc)

        /*
         * Switch expression.
         */
        case NamedAst.Expression.Switch(rules, tvar, loc) =>
          val rs = rules.map {
            case (cond, body) => (visitExp(cond, subst0), visitExp(body, subst0))
          }
          TypedAst.Expression.Switch(rs, subst0(tvar), loc)

        /*
         * Tag expression.
         */
        case NamedAst.Expression.Tag(qname, tag, exp, tvar, loc) =>
          Disambiguation.lookupEnumByTag(qname, tag, ns0, program) match {
            case Ok(enum) =>
              val e = visitExp(exp, subst0)
              TypedAst.Expression.Tag(enum.sym, tag.name, e, subst0(tvar), loc)
            case Err(e) => throw InternalCompilerException("Lookup should have failed during type inference.")
          }

        /*
         * Tuple expression.
         */
        case NamedAst.Expression.Tuple(elms, tvar, loc) =>
          val es = elms.map(e => visitExp(e, subst0))
          TypedAst.Expression.Tuple(es, subst0(tvar), loc)

        /*
         * Vec expression.
         */
        case NamedAst.Expression.FVec(elms, tvar, loc) =>
          val es = elms.map(e => visitExp(e, subst0))
          TypedAst.Expression.FVec(es, subst0(tvar), loc)

        /*
         * Set expression.
         */
        case NamedAst.Expression.FSet(elms, tvar, loc) =>
          val es = elms.map(e => visitExp(e, subst0))
          TypedAst.Expression.FSet(es, subst0(tvar), loc)

        /*
         * Map expression.
         */
        case NamedAst.Expression.FMap(elms, tvar, loc) =>
          val es = elms map {
            case (key, value) => (visitExp(key, subst0), visitExp(value, subst0))
          }
          TypedAst.Expression.FMap(es, subst0(tvar), loc)

        /*
         * GetIndex expression.
         */
        case NamedAst.Expression.GetIndex(exp1, exp2, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          TypedAst.Expression.GetIndex(e1, e2, subst0(tvar), loc)

        /*
         * PutIndex expression.
         */
        case NamedAst.Expression.PutIndex(exp1, exp2, exp3, tvar, loc) =>
          val e1 = visitExp(exp1, subst0)
          val e2 = visitExp(exp2, subst0)
          val e3 = visitExp(exp3, subst0)
          TypedAst.Expression.PutIndex(e1, e2, e3, subst0(tvar), loc)

        /*
         * Existential expression.
         */
        case NamedAst.Expression.Existential(params, exp, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Existential(visitParams(params), e, loc)

        /*
         * Universal expression.
         */
        case NamedAst.Expression.Universal(params, exp, loc) =>
          val e = visitExp(exp, subst0)
          TypedAst.Expression.Universal(visitParams(params), e, loc)

        /*
         * Ascribe expression.
         */
        case NamedAst.Expression.Ascribe(exp, tpe, loc) =>
          // simply reassemble the nested expression.
          visitExp(exp, subst0)

        /*
         * User Error expression.
         */
        case NamedAst.Expression.UserError(tvar, loc) =>
          TypedAst.Expression.UserError(subst0(tvar), loc)
      }

      /**
        * Applies the substitution to the given pattern `pat0`.
        */
      def visitPat(pat0: NamedAst.Pattern): TypedAst.Pattern = pat0 match {
        case NamedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst0(tvar), loc)
        case NamedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym, subst0(tvar), loc)
        case NamedAst.Pattern.Unit(loc) => TypedAst.Pattern.Unit(loc)
        case NamedAst.Pattern.True(loc) => TypedAst.Pattern.True(loc)
        case NamedAst.Pattern.False(loc) => TypedAst.Pattern.False(loc)
        case NamedAst.Pattern.Char(lit, loc) => TypedAst.Pattern.Char(lit, loc)
        case NamedAst.Pattern.Float32(lit, loc) => TypedAst.Pattern.Float32(lit, loc)
        case NamedAst.Pattern.Float64(lit, loc) => TypedAst.Pattern.Float64(lit, loc)
        case NamedAst.Pattern.Int8(lit, loc) => TypedAst.Pattern.Int8(lit, loc)
        case NamedAst.Pattern.Int16(lit, loc) => TypedAst.Pattern.Int16(lit, loc)
        case NamedAst.Pattern.Int32(lit, loc) => TypedAst.Pattern.Int32(lit, loc)
        case NamedAst.Pattern.Int64(lit, loc) => TypedAst.Pattern.Int64(lit, loc)
        case NamedAst.Pattern.BigInt(lit, loc) => TypedAst.Pattern.BigInt(lit, loc)
        case NamedAst.Pattern.Str(lit, loc) => TypedAst.Pattern.Str(lit, loc)
        case NamedAst.Pattern.Tag(qname, tag, pat, tvar, loc) =>
          Disambiguation.lookupEnumByTag(qname, tag, ns0, program) match {
            case Ok(enum) => TypedAst.Pattern.Tag(enum.sym, tag.name, visitPat(pat), subst0(tvar), loc)
            case Err(e) => throw InternalCompilerException("Lookup should have failed during type inference.")
          }
        case NamedAst.Pattern.Tuple(elms, tvar, loc) => TypedAst.Pattern.Tuple(elms map visitPat, subst0(tvar), loc)
        case NamedAst.Pattern.FVec(elms, rest, tvar, loc) => TypedAst.Pattern.FVec(elms map visitPat, rest.map(visitPat), subst0(tvar), loc)
        case NamedAst.Pattern.FSet(elms, rest, tvar, loc) => TypedAst.Pattern.FSet(elms map visitPat, rest.map(visitPat), subst0(tvar), loc)
        case NamedAst.Pattern.FMap(elms, rest, tvar, loc) =>
          val es = elms map {
            case (k, v) => (visitPat(k), visitPat(v))
          }
          TypedAst.Pattern.FMap(es, rest.map(visitPat), subst0(tvar), loc)
      }

      /**
        * Applies the substitution to the given list of formal parameters.
        */
      def visitParams(xs: List[NamedAst.FormalParam]): List[TypedAst.FormalParam] = {
        xs map {
          case NamedAst.FormalParam(sym, tpe, loc) =>
            Disambiguation.resolve(tpe, ns0, program) match {
              case Ok(resolvedType) => TypedAst.FormalParam(sym, subst0(resolvedType), loc)
              case Err(e) => throw InternalCompilerException("Never happens. Resolution should have failed during type inference.")
            }
        }
      }

      visitExp(exp0, subst0)
    }
  }

  object Predicates {

    /**
      * Performs type inference and reassembly on the given predicate `head` in the given namespace `ns`.
      */
    def typecheck(head: NamedAst.Predicate.Head, ns: Name.NName, program: Program)(implicit genSym: GenSym): Result[TypedAst.Predicate.Head, TypeError] = head match {
      case NamedAst.Predicate.Head.True(loc) => Ok(TypedAst.Predicate.Head.True(loc))
      case NamedAst.Predicate.Head.False(loc) => Ok(TypedAst.Predicate.Head.False(loc))
      case NamedAst.Predicate.Head.Table(qname, terms, loc) =>
        // Lookup the table associated with the predicate to find the declared types of the terms.
        Disambiguation.lookupTable(qname, ns, program) flatMap {
          table =>
            // Resolve the declared types.
            Disambiguation.resolve(table.attr.map(_.tpe), ns, program) flatMap {
              case expectedTypes =>
                // Compute the inferred types of the terms and unify them with the declared types.
                val result = for (
                  inferredTypes <- seqM(terms.map(t => Expressions.infer(t, ns, program)));
                  unifiedTypes <- unifyM(expectedTypes, inferredTypes, loc)
                ) yield unifiedTypes

                // Evaluate the monad with the empty substitution.
                result.run(Substitution.empty) map {
                  case (subst, _) =>
                    // Reassemble the expressions and predicate.
                    val ts = terms.map(t => Expressions.reassemble(t, ns, program, subst, resolveFreeVars = true))
                    TypedAst.Predicate.Head.Table(table.sym, ts, loc)
                }
            }
        }
    }

    /**
      * Infers the type of the given head predicate.
      */
    def infer(head: NamedAst.Predicate.Head, ns: Name.NName, program: Program)(implicit genSym: GenSym): InferMonad[List[Type]] = head match {
      case NamedAst.Predicate.Head.True(loc) => Unification.liftM(Nil)
      case NamedAst.Predicate.Head.False(loc) => Unification.liftM(Nil)
      case NamedAst.Predicate.Head.Table(qname, terms, loc) =>
        val declaredTypes = Disambiguation.lookupTable(qname, ns, program) match {
          case Ok(NamedAst.Table.Relation(doc, sym, attr, _)) => attr.map(_.tpe)
          case Ok(NamedAst.Table.Lattice(doc, sym, keys, value, _)) => keys.map(_.tpe) ::: value.tpe :: Nil
          case Err(e) => return failM(e)
        }

        Disambiguation.resolve(declaredTypes, ns, program) match {
          case Ok(expectedTypes) =>
            for (
              actualTypes <- seqM(terms.map(t => Expressions.infer(t, ns, program)));
              unifiedTypes <- Unification.unifyM(expectedTypes, actualTypes, loc)
            ) yield unifiedTypes
          case Err(e) => failM(e)
        }
    }

    /**
      * Infers the type of the given body predicate.
      */
    def infer(body0: NamedAst.Predicate.Body, ns0: Name.NName, program: Program)(implicit genSym: GenSym): InferMonad[List[Type]] = body0 match {
      case NamedAst.Predicate.Body.Table(qname, terms, loc) =>
        val declaredTypes = Disambiguation.lookupTable(qname, ns0, program) match {
          case Ok(NamedAst.Table.Relation(doc, sym, attr, _)) => attr.map(_.tpe)
          case Ok(NamedAst.Table.Lattice(doc, sym, keys, value, _)) => keys.map(_.tpe) ::: value.tpe :: Nil
          case Err(e) => return failM(e)
        }
        val expectedTypes = Disambiguation.resolve(declaredTypes, ns0, program) match {
          case Ok(tpes) => tpes
          case Err(e) => return failM(e)
        }
        for (
          actualTypes <- seqM(terms.map(t => Expressions.infer(t, ns0, program)));
          unifiedTypes <- Unification.unifyM(expectedTypes, actualTypes, loc)
        ) yield unifiedTypes
      case NamedAst.Predicate.Body.Filter(qname, terms, loc) =>
        Disambiguation.lookupRef(qname, ns0, program) match {
          case Ok(RefTarget.Defn(ns, defn)) =>
            val expectedTypes = Disambiguation.resolve(defn.params.map(_.tpe), ns, program) match {
              case Ok(tpes) => tpes
              case Err(e) => return failM(e)
            }
            for (
              actualTypes <- seqM(terms.map(t => Expressions.infer(t, ns0, program)));
              unifiedTypes <- Unification.unifyM(expectedTypes, actualTypes, loc)
            ) yield unifiedTypes
          case Ok(RefTarget.Hook(hook)) =>
            val Type.Apply(Type.Arrow(l), ts) = hook.tpe
            val declaredTypes = ts.take(l - 1)
            for (
              actualTypes <- seqM(terms.map(t => Expressions.infer(t, ns0, program)));
              unifiedTypes <- Unification.unifyM(declaredTypes, actualTypes, loc)
            ) yield unifiedTypes
          case Err(e) => failM(e)
        }
      case NamedAst.Predicate.Body.NotEqual(sym1, sym2, loc) => Unification.liftM(Nil) // TODO
      case NamedAst.Predicate.Body.Loop(sym, term, loc) => Unification.liftM(Nil) // TODO
    }

    /**
      * Applies the given substitution `subst0` to the given head predicate `head0` in the given namespace `ns0`.
      */
    def reassemble(head0: NamedAst.Predicate.Head, ns0: Name.NName, program: Program, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
      case NamedAst.Predicate.Head.True(loc) => TypedAst.Predicate.Head.True(loc)
      case NamedAst.Predicate.Head.False(loc) => TypedAst.Predicate.Head.False(loc)
      case NamedAst.Predicate.Head.Table(qname, terms, loc) =>
        Disambiguation.lookupTable(qname, ns0, program) match {
          case Ok(NamedAst.Table.Relation(_, sym, _, _)) =>
            val ts = terms.map(t => Expressions.reassemble(t, ns0, program, subst0, resolveFreeVars = true))
            TypedAst.Predicate.Head.Table(sym, ts, loc)
          case Ok(NamedAst.Table.Lattice(_, sym, _, _, _)) =>
            val ts = terms.map(t => Expressions.reassemble(t, ns0, program, subst0, resolveFreeVars = true))
            TypedAst.Predicate.Head.Table(sym, ts, loc)
          case Err(e) => throw InternalCompilerException("Lookup should have failed during type inference.")
        }
    }

    /**
      * Applies the given substitution `subst0` to the given body predicate `body0` in the given namespace `ns0`.
      */
    def reassemble(body0: NamedAst.Predicate.Body, ns0: Name.NName, program: Program, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
      case NamedAst.Predicate.Body.Table(qname, terms, loc) =>
        Disambiguation.lookupTable(qname, ns0, program) match {
          case Ok(NamedAst.Table.Relation(_, sym, _, _)) => TypedAst.Predicate.Body.Table(sym, terms.map(t => Expressions.reassemble(t, ns0, program, subst0, resolveFreeVars = true)), loc)
          case Ok(NamedAst.Table.Lattice(_, sym, _, _, _)) =>
            val ts = terms.map(t => Expressions.reassemble(t, ns0, program, subst0, resolveFreeVars = true))
            TypedAst.Predicate.Body.Table(sym, ts, loc)
          case Err(e) => throw InternalCompilerException("Lookup should have failed during type inference.")
        }
      case NamedAst.Predicate.Body.Filter(qname, terms, loc) =>
        Disambiguation.lookupRef(qname, ns0, program) match {
          case Ok(RefTarget.Defn(ns, defn)) =>
            val ts = terms.map(t => Expressions.reassemble(t, ns0, program, subst0, resolveFreeVars = true))
            TypedAst.Predicate.Body.ApplyFilter(defn.sym, ts, loc)
          case Ok(RefTarget.Hook(hook)) =>
            val ts = terms.map(t => Expressions.reassemble(t, ns0, program, subst0, resolveFreeVars = true))
            TypedAst.Predicate.Body.ApplyHookFilter(hook, ts, loc)
          case Err(e) => throw InternalCompilerException("Lookup should have failed during type inference.")
        }
      case NamedAst.Predicate.Body.NotEqual(sym1, sym2, loc) =>
        // TODO: Need to retrieve the symbol...
        TypedAst.Predicate.Body.NotEqual(sym1, sym2, loc)
      case NamedAst.Predicate.Body.Loop(sym, term, loc) =>
        // TODO: Need to retrieve the symbol...
        val t = Expressions.reassemble(term, ns0, program, subst0, resolveFreeVars = true)
        TypedAst.Predicate.Body.Loop(sym, t, loc)
    }

  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    *
    * @param params  the formal parameters.
    * @param ns0     the current namespace.
    * @param program the program.
    */
  def getSubstFromParams(params: List[NamedAst.FormalParam], ns0: Name.NName, program: Program): Result[Unification.Substitution, TypeError] = {
    // Perform type resolution on each parameter.
    Disambiguation.resolve(params.map(_.tpe), ns0, program) map {
      case declaredTypes =>
        // Compute the substitution by mapping the symbol of each parameter to its declared type.
        (params zip declaredTypes).foldLeft(Substitution.empty) {
          case (macc, (NamedAst.FormalParam(sym, _, _), declaredType)) =>
            macc ++ Substitution.singleton(sym.tvar, declaredType)
        }
    }
  }

}
