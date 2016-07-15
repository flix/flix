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

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Typer {

  // TODO: when to use inner visit?
  // TODO: use pattern match on rast
  // TODO: Probably need to rewrite this to be unification based.
  // TODO: Check that lattice variables are not bound multiple times.
  // TODO: https://flockler.com/thumbs/1992/truthy_s830x0_q80_noupscale.png

  import TypeError._

  /**
    * A common super-type for type errors.
    */
  sealed trait TypeError extends CompilationError

  object TypeError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate a type mismatch between an `expected` and an `actual` type.
      *
      * @param expected the expected type.
      * @param actual   the actual type.
      * @param loc      the source location.
      */
    case class ExpectedType(expected: Type, actual: Type, loc: SourceLocation) extends TypeError {
      val message =
        s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Expected type '${prettyPrint(expected)}' but actual type is '${prettyPrint(actual)}'.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate that the two given types `tpe1` and `tpe2` were expected to be equal.
      *
      * @param tpe1 the first type.
      * @param tpe2 the second type.
      * @param loc1 the source location of the first type.
      * @param loc2 the source location of the second type.
      */
    case class ExpectedEqualTypes(tpe1: Type, tpe2: Type, loc1: SourceLocation, loc2: SourceLocation) extends TypeError {
      val message =
        s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Expected equal types '${prettyPrint(tpe1)}' and '${prettyPrint(tpe2)}'.")}
           |
            |${loc1.underline}
           |${loc2.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate that the given type `tpe` was expected to be a function type.
      *
      * @param tpe the erroneous type.
      * @param loc the source location.
      */
    // TODO: Pretty print
    case class IllegalApply(tpe: Type, loc: SourceLocation) extends TypeError {
      val message = s"Type Error: The type '${prettyPrint(tpe)}' is not a function type at ${loc.format}.\n"
    }

    /**
      * An error raised to indicate a type mismatch between a pattern `pat` and an expected type `tpe`.
      *
      * @param pat the pattern.
      * @param tpe the type.
      * @param loc the source location.
      */
    // TODO: Pretty print
    case class IllegalPattern(pat: ResolvedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypeError {
      val message = s"Type Error: Pattern '${prettyPrint(pat)}' does not match expected type '${prettyPrint(tpe)}' at ${loc.format}.\n"
    }

    // TODO: Check arity of function calls, predicates, etc.

    /**
      * An error raised to indicate that a type has no associated lattice.
      *
      * @param tpe the type that has no lattice.
      * @param loc the source location.
      */
    case class NoSuchLattice(tpe: Type, loc: SourceLocation) extends TypeError {
      val message =
        s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> No lattice declared for '${prettyPrint(tpe)}'.")}
           |
            |${loc.underline}
           |Tip: Associate a lattice with the type.
         """.stripMargin
    }

  }

  /**
    * Runs the typer on the entire given AST `rast`.
    */
  def typecheck(root: ResolvedAst.Root): Validation[TypedAst.Root, TypeError] = {
    val b = System.nanoTime()

    // constants
    val constantsVal = Validation.fold(root.constants) {
      case (name, constant) => Definition.typer(constant, root) map (defn => name -> defn)
    }

    // lattices
    val latticesVal = Validation.fold(root.lattices) {
      case (tpe, lattice) => Definition.typer(lattice, root) map (defn => tpe -> defn)
    }

    // relations
    val relationsVal = Validation.fold(root.tables) {
      case (name, relation) => Definition.typer(relation, root) map (defn => name -> defn)
    }

    // indexes
    val indexesVal = Validation.fold(root.indexes) {
      case (name, index) => Definition.typer(index, root) map (defn => name -> defn)
    }

    // facts and rules
    val factsVal = @@(root.facts.map(fact => Constraint.typer(fact, root)))
    val rulesVal = @@(root.rules.map(rule => Constraint.typer(rule, root)))

    // putting it all together
    @@(constantsVal, latticesVal, relationsVal, indexesVal, factsVal, rulesVal) map {
      case (constants, lattices, relations, indexes, facts, rules) =>
        val e = System.nanoTime()
        TypedAst.Root(constants, lattices, relations, indexes, facts, rules, root.hooks, Nil, root.time.copy(typer = e - b))
    }
  }

  object Definition {
    /**
      * Types the given constant definition `rast` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Definition.Constant, root: ResolvedAst.Root): Validation[TypedAst.Definition.Constant, TypeError] = {
      val formals = rast.formals.map {
        case ResolvedAst.FormalArg(ident, tpe) => TypedAst.FormalArg(ident, tpe)
      }

      val env = rast.formals.foldLeft(Map.empty[String, Type]) {
        case (macc, ResolvedAst.FormalArg(ident, tpe)) => macc + (ident.name -> tpe)
      }

      Expression.typer(rast.exp, root, env) flatMap {
        case e =>
          if (rast.formals.isEmpty) {
            expect(rast.tpe, Type.Lambda(Nil, e.tpe), rast.loc) map {
              case tpe => TypedAst.Definition.Constant(rast.ann, rast.name, formals, e, tpe, rast.loc)
            }
          } else {
            val lambdaTpe = Type.Lambda(rast.formals.map(_.tpe), e.tpe)
            expect(rast.tpe, lambdaTpe, rast.loc) map {
              case tpe => TypedAst.Definition.Constant(rast.ann, rast.name, formals, e, tpe, rast.loc)
            }
          }
      }
    }

    /**
      * Types the given lattice definition `rast` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Definition.BoundedLattice, root: ResolvedAst.Root): Validation[TypedAst.Definition.BoundedLattice, TypeError] = {
      val tpe = rast.tpe
      val leqType = Type.Lambda(args = List(tpe, tpe), retTpe = Type.Bool)
      val lubType = Type.Lambda(args = List(tpe, tpe), retTpe = tpe)
      val glbType = Type.Lambda(args = List(tpe, tpe), retTpe = tpe)

      val botVal = Expression.typer(rast.bot, root) flatMap {
        case e => expect(tpe, e.tpe, rast.bot.loc) map (_ => e)
      }
      val topVal = Expression.typer(rast.top, root) flatMap {
        case e => expect(tpe, e.tpe, rast.top.loc) map (_ => e)
      }
      val leqVal = Expression.typer(rast.leq, root) flatMap {
        case e => expect(leqType, e.tpe, rast.leq.loc) map (_ => e)
      }
      val lubVal = Expression.typer(rast.lub, root) flatMap {
        case e => expect(lubType, e.tpe, rast.lub.loc) map (_ => e)
      }
      val glbVal = Expression.typer(rast.glb, root) flatMap {
        case e => expect(glbType, e.tpe, rast.glb.loc) map (_ => e)
      }

      @@(leqVal, botVal, topVal, lubVal, glbVal) map {
        case (leq, bot, top, lub, glb) => TypedAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, rast.loc)
      }
    }


    /**
      * Types the given table definition `rast` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Table, root: ResolvedAst.Root): Validation[TypedAst.Table, TypeError] = rast match {
      case d: ResolvedAst.Table.Relation => typer2(d, root)
      case d: ResolvedAst.Table.Lattice => typer2(d, root)
    }

    /**
      * Types the given relation definition `rast` under the given AST `root`.
      */
    def typer2(rast: ResolvedAst.Table.Relation, root: ResolvedAst.Root): Validation[TypedAst.Table.Relation, TypeError] = {
      val attributes = rast.attributes map {
        case ResolvedAst.Attribute(ident, tpe) => TypedAst.Attribute(ident, tpe)
      }
      TypedAst.Table.Relation(rast.sym, attributes, rast.loc).toSuccess
    }

    /**
      * Types the given lattice definition `rast` under the given AST `root`.
      */
    def typer2(rast: ResolvedAst.Table.Lattice, root: ResolvedAst.Root): Validation[TypedAst.Table.Lattice, TypeError] = {
      val keys = rast.keys map {
        case ResolvedAst.Attribute(ident, tpe) => TypedAst.Attribute(ident, tpe)
      }

      val a = rast.value
      val valueVal = root.lattices.get(a.tpe) match {
        case None => NoSuchLattice(a.tpe, a.ident.loc).toFailure
        case Some(_) => TypedAst.Attribute(a.ident, a.tpe).toSuccess
      }

      valueVal map {
        case value => TypedAst.Table.Lattice(rast.sym, keys, value, rast.loc)
      }
    }

    /**
      * Types the given index definition `rast` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Definition.Index, root: ResolvedAst.Root): Validation[TypedAst.Definition.Index, TypeError] = {
      // TODO: any checks?
      TypedAst.Definition.Index(rast.sym, rast.indexes, rast.loc).toSuccess
    }

  }

  object Constraint {

    /**
      * Types the given fact `rast` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Constraint.Fact, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Fact, TypeError] = {
      Predicate.Head.typer(rast.head, root) map TypedAst.Constraint.Fact
    }

    /**
      * Types the given rule `rast` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Constraint.Rule, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Rule, TypeError] = {
      val headVal = Predicate.Head.typer(rast.head, root)
      // TODO: Should check that variables have consistent types?
      val bodyVal = @@(rast.body map (p => Predicate.Body.typer(p, root)))

      @@(headVal, bodyVal) map {
        case (head, body) => TypedAst.Constraint.Rule(head, body)
      }
    }
  }

  object Literal {

    /**
      * Types the given resolved literal `rast`.
      */
    def typer(rast: ResolvedAst.Literal, root: ResolvedAst.Root): TypedAst.Literal = {
      def visit(rast: ResolvedAst.Literal): TypedAst.Literal = rast match {
        case ResolvedAst.Literal.Unit(loc) => TypedAst.Literal.Unit(loc)
        case ResolvedAst.Literal.Bool(b, loc) => TypedAst.Literal.Bool(b, loc)
        case ResolvedAst.Literal.Char(c, loc) => TypedAst.Literal.Char(c, loc)
        case ResolvedAst.Literal.Float32(f, loc) => TypedAst.Literal.Float32(f, loc)
        case ResolvedAst.Literal.Float64(f, loc) => TypedAst.Literal.Float64(f, loc)
        case ResolvedAst.Literal.Int8(i, loc) => TypedAst.Literal.Int8(i, loc)
        case ResolvedAst.Literal.Int16(i, loc) => TypedAst.Literal.Int16(i, loc)
        case ResolvedAst.Literal.Int32(i, loc) => TypedAst.Literal.Int32(i, loc)
        case ResolvedAst.Literal.Int64(i, loc) => TypedAst.Literal.Int64(i, loc)
        case ResolvedAst.Literal.BigInt(i, loc) => TypedAst.Literal.BigInt(i, loc)
        case ResolvedAst.Literal.Str(s, loc) => TypedAst.Literal.Str(s, loc)
      }

      visit(rast)
    }
  }

  object Expression {

    /**
      * Types the given resolved expression `rast` under the given ast `root` and local environment `env`.
      */
    def typer(rast: ResolvedAst.Expression, root: ResolvedAst.Root, env: Map[String, Type] = Map.empty): Validation[TypedAst.Expression, TypeError] = {
      def visit(rast: ResolvedAst.Expression, env: Map[String, Type]): Validation[TypedAst.Expression, TypeError] = rast match {
        case ResolvedAst.Expression.Var(ident, loc) =>
          val tpe = env(ident.name)
          TypedAst.Expression.Var(ident, tpe, loc).toSuccess

        case ResolvedAst.Expression.Ref(name, loc) =>
          val constant = root.constants(name)
          TypedAst.Expression.Ref(name, constant.tpe, loc).toSuccess

        case ResolvedAst.Expression.HookRef(hook, loc) =>
          val tpe = hook.tpe
          TypedAst.Expression.Hook(hook, tpe, loc).toSuccess

        case ResolvedAst.Expression.Lit(rlit, loc) =>
          val lit = Literal.typer(rlit, root)
          TypedAst.Expression.Lit(lit, lit.tpe, loc).toSuccess

        // TODO: Peer review
        case ResolvedAst.Expression.Lambda(rargs, rtpe, rbody, loc) =>
          // compile formal arguments
          val args = rargs map {
            case ResolvedAst.FormalArg(ident, t) => TypedAst.FormalArg(ident, t)
          }
          // return type
          // create extended environment
          val env1 = args.foldLeft(env) {
            case (m, TypedAst.FormalArg(ident, t)) => m + (ident.name -> t)
          }

          // type body
          visit(rbody, env1) flatMap {
            case body => expect(rtpe, body.tpe, loc) map {
              case _ => TypedAst.Expression.Lambda(args, body, Type.Lambda(args map (_.tpe), rtpe), loc)
            }
          }

        // TODO: Peer review
        case ResolvedAst.Expression.Apply(re, rargs, loc) =>
          val lambdaVal = visit(re, env)
          val argsVal = @@(rargs map (arg => visit(arg, env)))

          @@(lambdaVal, argsVal) flatMap {
            case (lambda, args) => lambda.tpe match {
              case Type.Lambda(targs, retTpe) =>
                val argsVal = (targs zip args) map {
                  case (formalType, actualExp) => expect(formalType, actualExp.tpe, actualExp.loc)
                }

                @@(argsVal) map {
                  case _ => TypedAst.Expression.Apply(lambda, args, retTpe, loc)
                }
              case tpe => IllegalApply(tpe, loc).toFailure
            }
          }

        case ResolvedAst.Expression.Unary(op, re, loc) => op match {
          case UnaryOperator.LogicalNot =>
            visit(re, env) flatMap {
              case e => expect(Type.Bool, e.tpe, loc) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe, loc)
              }
            }
          case UnaryOperator.Plus | UnaryOperator.Minus =>
            visit(re, env) flatMap {
              case e => e.tpe match {
                case Type.Float32 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case Type.Float64 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case Type.Int8 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case Type.Int16 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case Type.Int32 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case Type.Int64 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case Type.BigInt => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                case _ => TypeError.ExpectedType(Type.Int32, e.tpe, e.loc).toFailure // TODO: Need more generic error message.
              }
            }
          case UnaryOperator.BitwiseNegate =>
            visit(re, env) flatMap {
              case e =>
                e.tpe match {
                  case Type.Int8 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                  case Type.Int16 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                  case Type.Int32 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                  case Type.Int64 => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                  case Type.BigInt => TypedAst.Expression.Unary(op, e, e.tpe, loc).toSuccess
                  case _ => TypeError.ExpectedType(Type.Int32, e.tpe, e.loc).toFailure // TODO: Need more generic error message.
                }
            }
        }

        case ResolvedAst.Expression.Binary(op, re1, re2, loc) => op match {
          case _: ArithmeticOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => (e1.tpe, e2.tpe) match {
                case (Type.Float32, Type.Float32) => TypedAst.Expression.Binary(op, e1, e2, Type.Float32, loc).toSuccess
                case (Type.Float64, Type.Float64) => TypedAst.Expression.Binary(op, e1, e2, Type.Float64, loc).toSuccess
                case (Type.Int8, Type.Int8) => TypedAst.Expression.Binary(op, e1, e2, Type.Int8, loc).toSuccess
                case (Type.Int16, Type.Int16) => TypedAst.Expression.Binary(op, e1, e2, Type.Int16, loc).toSuccess
                case (Type.Int32, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Int32, loc).toSuccess
                case (Type.Int64, Type.Int64) => TypedAst.Expression.Binary(op, e1, e2, Type.Int64, loc).toSuccess
                case (Type.BigInt, Type.BigInt) => TypedAst.Expression.Binary(op, e1, e2, Type.BigInt, loc).toSuccess
                case (t1, t2) => TypeError.ExpectedEqualTypes(t1, t2, e1.loc, e2.loc).toFailure
              }
            }
          case _: EqualityOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => expectEqual(e1.tpe, e2.tpe, e1.loc, e2.loc) map {
                case tpe => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc)
              }
            }
          case _: ComparisonOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => (e1.tpe, e2.tpe) match {
                case (Type.Char, Type.Char) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.Float32, Type.Float32) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.Float64, Type.Float64) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.Int8, Type.Int8) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.Int16, Type.Int16) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.Int32, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.Int64, Type.Int64) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (Type.BigInt, Type.BigInt) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc).toSuccess
                case (t1, t2) => TypeError.ExpectedEqualTypes(t1, t2, e1.loc, e2.loc).toFailure
              }
            }
          case _: LogicalOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(Type.Bool, e1.tpe, e1.loc), expect(Type.Bool, e2.tpe, e2.loc)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, Type.Bool, loc)
              }
            }
          case _: BitwiseOperator =>
            if (op == BinaryOperator.BitwiseLeftShift || op == BinaryOperator.BitwiseRightShift) {
              // The shift is always Int32.
              @@(visit(re1, env), visit(re2, env)) flatMap {
                case (e1, e2) => (e1.tpe, e2.tpe) match {
                  case (Type.Int8, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Int8, loc).toSuccess
                  case (Type.Int16, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Int16, loc).toSuccess
                  case (Type.Int32, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Int32, loc).toSuccess
                  case (Type.Int64, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Int64, loc).toSuccess
                  case (Type.BigInt, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.BigInt, loc).toSuccess
                  case (t1, t2) => TypeError.ExpectedEqualTypes(t1, t2, e1.loc, e2.loc).toFailure // TODO: Wrong error message.
                }
              }
            } else {
              @@(visit(re1, env), visit(re2, env)) flatMap {
                case (e1, e2) => (e1.tpe, e2.tpe) match {
                  case (Type.Int8, Type.Int8) => TypedAst.Expression.Binary(op, e1, e2, Type.Int8, loc).toSuccess
                  case (Type.Int16, Type.Int16) => TypedAst.Expression.Binary(op, e1, e2, Type.Int16, loc).toSuccess
                  case (Type.Int32, Type.Int32) => TypedAst.Expression.Binary(op, e1, e2, Type.Int32, loc).toSuccess
                  case (Type.Int64, Type.Int64) => TypedAst.Expression.Binary(op, e1, e2, Type.Int64, loc).toSuccess
                  case (Type.BigInt, Type.BigInt) => TypedAst.Expression.Binary(op, e1, e2, Type.BigInt, loc).toSuccess
                  case (t1, t2) => TypeError.ExpectedEqualTypes(t1, t2, e1.loc, e2.loc).toFailure // TODO: Wrong error message.
                }
              }
            }
        }

        case ResolvedAst.Expression.IfThenElse(re1, re2, re3, loc) =>
          @@(visit(re1, env), visit(re2, env), visit(re3, env)) flatMap {
            case (e1, e2, e3) =>
              val conditionType = expect(Type.Bool, e1.tpe, e1.loc)
              val expressionType = expectEqual(e2.tpe, e3.tpe, e2.loc, e3.loc)
              #@(conditionType, expressionType) map {
                case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)
              }
          }

        case ResolvedAst.Expression.Switch(rules, loc) =>
          val (rconds, rbodies) = rules.unzip
          // type the conditions against bool
          val condsVal = @@(rconds map {
            case cond => visit(cond, env) flatMap {
              case e => expect(Type.Bool, e.tpe, e.loc) map {
                case tpe => e
              }
            }
          })

          val bodiesVal = @@(rbodies map {
            case body => visit(body, env)
          })

          @@(condsVal, bodiesVal) flatMap {
            case (conds, bodies) =>
              expectEqual(bodies.map(body => (body.tpe, body.loc))) map {
                case tpe => TypedAst.Expression.Switch(conds zip bodies, tpe, loc)
              }
          }

        case ResolvedAst.Expression.Let(ident, rvalue, rbody, loc) =>
          visit(rvalue, env) flatMap {
            case value => visit(rbody, env + (ident.name -> value.tpe)) map {
              case body => TypedAst.Expression.Let(ident, value, body, body.tpe, loc)
            }
          }

        // TODO: Peer review
        case ResolvedAst.Expression.Match(re, rs, loc) =>
          visit(re, env) flatMap {
            case matchValue =>
              val rulesVal = rs map {
                case (pat, body) =>
                  // type the pattern of the rule against the type of the match value.
                  Pattern.typer(pat, matchValue.tpe, root) flatMap {
                    // type the body of the rule under the extended environment provided by the pattern.
                    case typedPat => visit(body, env ++ typedPat.freeVars) map {
                      case typedBody => (typedPat, typedBody)
                    }
                  }
              }
              @@(rulesVal) flatMap {
                case rules =>
                  // ensure that the body of every rule has the same type.
                  expectEqual(rules.map(p => (p._2.tpe, p._2.loc))) map {
                    case tpe => TypedAst.Expression.Match(matchValue, rules, tpe, loc)
                  }
              }
          }

        case ResolvedAst.Expression.Tag(enumName, tagName, re, loc) =>
          visit(re, env) flatMap {
            case e =>
              val enum = root.enums(enumName)
              val cases = enum.cases.map { case (k, v) => k -> v.asInstanceOf[Type.Tag] }
              val caze = cases(tagName.name)
              expect(caze.tpe, e.tpe, e.loc) map {
                _ => TypedAst.Expression.Tag(enumName, tagName, e, Type.Enum(enumName, cases), loc)
              }
          }

        case ResolvedAst.Expression.Tuple(relms, loc) =>
          @@(relms map (e => visit(e, env))) map {
            case elms => TypedAst.Expression.Tuple(elms, Type.Tuple(elms map (_.tpe)), loc)
          }

        case ResolvedAst.Expression.Set(relms, loc) =>
          @@(relms map (e => visit(e, env))) flatMap {
            case elms =>
              val tpes = elms.map(e => (e.tpe, e.loc))
              expectEqual(tpes) map {
                case tpe => TypedAst.Expression.FSet(elms, Type.FSet(tpe), loc)
              }
          }

        case ResolvedAst.Expression.Ascribe(re, tpe, loc) =>
          visit(re, env) flatMap {
            case e => expect(tpe, e.tpe, loc) map {
              case _ => e
            }
          }

        case ResolvedAst.Expression.Error(tpe, loc) =>
          TypedAst.Expression.Error(tpe, loc).toSuccess

      }

      visit(rast, env)
    }

  }

  object Pattern {
    /**
      * Types the given resolved pattern `rast` against the given type `tpe`.
      *
      * NB: The Weeder ensures that a variable occurs at most once in a pattern.
      */
    def typer(rast: ResolvedAst.Pattern, tpe: Type, root: ResolvedAst.Root): Validation[TypedAst.Pattern, TypeError] = rast match {
      case ResolvedAst.Pattern.Wildcard(loc) =>
        TypedAst.Pattern.Wildcard(tpe, loc).toSuccess
      case ResolvedAst.Pattern.Var(ident, loc) =>
        TypedAst.Pattern.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Pattern.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe, loc) map {
          case _ => TypedAst.Pattern.Lit(lit, tpe, loc)
        }
      case ResolvedAst.Pattern.Tag(enumName, tagName, rpat, loc) => tpe match {
        case enum@Type.Enum(name, cases) => cases.get(tagName.name) match {
          case Some(tag) if enumName == tag.enum => {
            typer(rpat, tag.tpe, root) map {
              case pat => TypedAst.Pattern.Tag(enumName, tagName, pat, enum, loc)
            }
          }
          case _ => IllegalPattern(rast, tpe, loc).toFailure
        }
        case _ => IllegalPattern(rast, tpe, loc).toFailure
      }
      case ResolvedAst.Pattern.Tuple(relms, loc) => tpe match {
        case Type.Tuple(telms) if relms.length == telms.length =>
          val elmsVal = (relms zip telms) map {
            case (rp, tp) => typer(rp, tp, root)
          }
          @@(elmsVal) map {
            case elms => TypedAst.Pattern.Tuple(elms, Type.Tuple(elms map (_.tpe)), loc)
          }
        case _ => IllegalPattern(rast, tpe, loc).toFailure
      }
    }
  }

  object Predicate {

    object Head {

      /**
        * Types the given head predicate `rast` under the given AST `root`.
        */
      def typer(rast: ResolvedAst.Predicate.Head, root: ResolvedAst.Root): Validation[TypedAst.Predicate.Head, TypeError] = rast match {
        case ResolvedAst.Predicate.Head.True(loc) => TypedAst.Predicate.Head.True(loc).toSuccess
        case ResolvedAst.Predicate.Head.False(loc) => TypedAst.Predicate.Head.False(loc).toSuccess
        case ResolvedAst.Predicate.Head.Table(name, rterms, loc) =>
          // lookup the collection.
          root.tables(name) match {
            case ResolvedAst.Table.Relation(_, attributes, _) =>
              // type check the terms against the attributes.
              val termsVal = (rterms zip attributes) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, tpe, root)
              }

              @@(termsVal) map {
                case terms =>
                  TypedAst.Predicate.Head.Table(name, terms, Type.Predicate(terms map (_.tpe)), loc)
              }

            case ResolvedAst.Table.Lattice(_, keys, value, _) =>
              // type check the terms against the keys and values.
              // TODO: More checks?
              val termsVal = (rterms zip (keys ::: value :: Nil)) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, tpe, root)
              }

              @@(termsVal) map {
                case terms =>
                  TypedAst.Predicate.Head.Table(name, terms, Type.Predicate(terms map (_.tpe)), loc)
              }
          }

      }
    }

    object Body {
      /**
        * Types the given body predicate `rast` under the given AST `root`.
        */
      def typer(rast: ResolvedAst.Predicate.Body, root: ResolvedAst.Root): Validation[TypedAst.Predicate.Body, TypeError] = rast match {
        case ResolvedAst.Predicate.Body.Table(sym, rterms, loc) =>
          // lookup the collection.
          root.tables(sym) match {
            case ResolvedAst.Table.Relation(_, attributes, _) =>
              // type check the terms against the attributes.
              val termsVal = (rterms zip attributes) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, tpe, root)
              }

              @@(termsVal) map {
                case terms => TypedAst.Predicate.Body.Table(sym, terms, Type.Predicate(terms map (_.tpe)), loc)
              }
            case ResolvedAst.Table.Lattice(_, keys, value, _) =>
              // type check the terms against the attributes.
              // TODO: more checks?
              val termsVal = (rterms zip (keys ::: value :: Nil)) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, tpe, root)
              }

              @@(termsVal) map {
                case terms => TypedAst.Predicate.Body.Table(sym, terms, Type.Predicate(terms map (_.tpe)), loc)
              }
          }

        case ResolvedAst.Predicate.Body.ApplyFilter(name, rterms, loc) =>
          val constant = root.constants(name)
          // TODO: Check that result type is bool.
          // TODO: Improve the cast here
          val termsVal = (rterms zip constant.tpe.asInstanceOf[Type.Lambda].args) map {
            case (term, tpe) => Term.typer(term, tpe, root)
          }

          @@(termsVal) map {
            case terms => TypedAst.Predicate.Body.ApplyFilter(name, terms, Type.Lambda(terms map (_.tpe), Type.Bool), loc) // TODO Type
          }

        case ResolvedAst.Predicate.Body.ApplyHookFilter(hook, rterms, loc) =>
          // TODO: Check that result type is bool.
          // TODO: Improve the cast here
          val termsVal = (rterms zip hook.tpe.asInstanceOf[Type.Lambda].args) map {
            case (term, tpe) => Term.typer(term, tpe, root)
          }

          @@(termsVal) map {
            case terms => TypedAst.Predicate.Body.ApplyHookFilter(hook, terms, Type.Lambda(terms map (_.tpe), Type.Bool), loc) // TODO Type
          }


        case ResolvedAst.Predicate.Body.NotEqual(ident1, ident2, loc) =>
          TypedAst.Predicate.Body.NotEqual(ident1, ident2, Type.Bool, loc).toSuccess

        case ResolvedAst.Predicate.Body.Loop(ident, rterm, loc) =>
          Term.typer(rterm, Type.Any, root) map {
            case term => TypedAst.Predicate.Body.Loop(ident, term, Type.Bool, loc) // TODO: Type
          }

      }
    }

  }

  object Term {
    // TODO: Introduce head/body.

    /**
      * Types the given head term `rast` according to the (declared) type `tpe` under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Term.Head, tpe: Type, root: ResolvedAst.Root): Validation[TypedAst.Term.Head, TypeError] = rast match {
      case ResolvedAst.Term.Head.Var(ident, loc) => TypedAst.Term.Head.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Term.Head.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe, loc) map {
          case _ => TypedAst.Term.Head.Lit(lit, lit.tpe, loc)
        }

      case ResolvedAst.Term.Head.Tag(enum, tag, term, loc) =>
        val inner = tpe.asInstanceOf[Type.Enum]
        Term.typer(term, inner.cases(tag.name).tpe, root) map {
          case t => TypedAst.Term.Head.Tag(enum, tag, t, tpe.asInstanceOf[Type.Enum], loc)
        }

      case ResolvedAst.Term.Head.Tuple(relms, loc) =>
        val telms = tpe.asInstanceOf[Type.Tuple].elms
        val inner = (relms zip telms) map {
          case (e, t) => Term.typer(e, t, root)
        }

        @@(inner) map {
          case elms => TypedAst.Term.Head.Tuple(elms, tpe.asInstanceOf[Type.Tuple], loc)
        }

      case ResolvedAst.Term.Head.Apply(name, actuals, loc) =>
        // TODO: This needs to be rewritten

        val constant = root.constants(name)
        // TODO: This might actually be slightly problematic, since not every constant may be a fully evalauted lambda.
        // Instead we should focus on the type of the constant, which should be Function.

        // type arguments with the declared formals.
        val argsVal = (actuals zip constant.formals) map {
          case (term, ResolvedAst.FormalArg(_, termType)) => Term.typer(term, termType, root)
        }
        // put everything together and check the return type.
        @@(@@(argsVal), expect(tpe, tpe, constant.exp.loc)) map {
          // TODO: Hack
          case (args, returnType) => TypedAst.Term.Head.Apply(name, args, returnType, constant.exp.loc)
        }

      case ResolvedAst.Term.Head.ApplyHook(hook, actuals, loc) =>
        val formals = hook.tpe.args
        // type arguments with the declared formals.
        val argsVal = (actuals zip formals) map {
          case (term, termType) => Term.typer(term, termType, root)
        }
        // put everything together and check the return type.
        @@(@@(argsVal), expect(tpe, hook.tpe.retTpe, loc)) map {
          case (args, returnType) => TypedAst.Term.Head.ApplyHook(hook, args, returnType, loc)
        }
    }

    /**
      * Types the given body term `rast` according to the given type `tpe`. under the given AST `root`.
      */
    def typer(rast: ResolvedAst.Term.Body, tpe: Type, root: ResolvedAst.Root): Validation[TypedAst.Term.Body, TypeError] = rast match {
      case ResolvedAst.Term.Body.Wildcard(loc) => TypedAst.Term.Body.Wildcard(tpe, loc).toSuccess
      case ResolvedAst.Term.Body.Var(ident, loc) => TypedAst.Term.Body.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Term.Body.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe, loc) map {
          case _ => TypedAst.Term.Body.Lit(lit, lit.tpe, loc)
        }
      case ResolvedAst.Term.Body.Ascribe(rterm, rtpe, loc) =>
        val ascribedType = rtpe
        typer(rterm, ascribedType, root)
    }

  }

  /**
    * Returns the given `expected` type wrapped in [[Success]] if it matches the given `actual` type.
    *
    * @param expected the expected type.
    * @param actual   the actual type.
    * @param loc      the source location.
    */
  def expect(expected: Type, actual: Type, loc: SourceLocation): Validation[Type, TypeError] =
    if (expected == Type.Any)
      actual.toSuccess
    else if (actual == Type.Any)
      actual.toSuccess
    else if (expected == actual)
      actual.toSuccess
    else
      ExpectedType(expected, actual, loc).toFailure

  /**
    * Returns the given `tpe` type wrapped in [[Success]] if it matches the given `tpe2` type.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param loc1 the source location of the first type.
    * @param loc2 the source location of the second type.
    */
  def expectEqual(tpe1: Type, tpe2: Type, loc1: SourceLocation, loc2: SourceLocation): Validation[Type, TypeError] =
    if (tpe1 == Type.Any)
      tpe2.toSuccess
    else if (tpe2 == Type.Any)
      tpe1.toSuccess
    else if (tpe1 == tpe2)
      tpe1.toSuccess
    else
      ExpectedEqualTypes(tpe1, tpe2, loc1, loc2).toFailure

  /**
    * Returns a type wrapped in [[Success]] if all the given `types` are equal.
    */
  def expectEqual(types: List[(Type, SourceLocation)]): Validation[Type, TypeError] = {
    assert(types.nonEmpty)
    val (tpe1, loc1) = types.head
    if (types.forall(t => t._1 == tpe1)) {
      tpe1.toSuccess
    } else {
      val (tpe2, loc2) = types.find(t => t._1 != tpe1 && tpe1 != Type.Any && t._1 != Type.Any).get
      ExpectedEqualTypes(tpe1, tpe2, loc1, loc2).toFailure
    }
  }

  /**
    * Returns a human readable string representation of the given type `tpe`.
    */
  // TODO: Remove this.
  private def prettyPrint(tpe: Type): String = tpe match {
    case Type.Any => "Any"
    case Type.Var(x) => s"Var($x)"
    case Type.Unit => s"()"
    case Type.Bool => s"Bool"
    case Type.Char => s"Char"
    case Type.Float32 => s"Float32"
    case Type.Float64 => s"Float64"
    case Type.Int8 => s"Int8"
    case Type.Int16 => s"Int16"
    case Type.Int32 => s"Int32"
    case Type.Int64 => s"Int64"
    case Type.BigInt => "BigInt"
    case Type.Prop => s"Prop"
    case Type.Str => s"Str"
    case Type.Tag(enumName, tagName, t) =>
      val enumAndTag = enumName.fqn + "." + tagName.name
      val nested = s"(${prettyPrint(t)}})"
      enumAndTag + nested
    case Type.Enum(name, cases) =>
      s"Enum(${cases.head._2.enum})"
    case Type.Tuple(elms) => "(" + elms.map(prettyPrint).mkString(", ") + ")"
    case Type.FSet(elms) => "Set(" + prettyPrint(elms) + ")"
    case Type.Lambda(args, retTpe) =>
      "(" + args.map(prettyPrint).mkString(", ") + ") -> " + prettyPrint(retTpe)
    case Type.Predicate(terms) => s"Predicate(${terms map prettyPrint})"
    case Type.Native => s"native"
    case Type.Abs(name, tpe) => s"Abs($name)"
    case Type.FOpt(tpe) => s"FOpt($tpe)"
    case Type.FList(tpe) => s"FList($tpe)"
    case Type.FVec(tpe) =>  s"FVec($tpe)"
    case Type.FMap(k, v) => s"FMap($k, $v)"
    case Type.Parametric(name, elms) => s"$name[$elms]"
    case Type.Unresolved(name) => name.toString
  }

  private def prettyPrint(pat: ResolvedAst.Pattern): String = pat match {
    case ResolvedAst.Pattern.Wildcard(loc) => "_"
    case ResolvedAst.Pattern.Var(ident, loc) => ident.name
    case ResolvedAst.Pattern.Lit(lit, loc) => lit.toString
    case ResolvedAst.Pattern.Tag(enumName, tagName, pat, loc) =>
      enumName + "." + tagName.name + "(" + prettyPrint(pat) + ")"
    case ResolvedAst.Pattern.Tuple(elms, loc) =>
      "(" + elms.map(prettyPrint).mkString(", ") + ")"
  }
}
