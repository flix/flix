package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.Compiler
import ca.uwaterloo.flix.lang.ast._

import util.Validation
import util.Validation._

// TODO: Check code coverage.
// TODO: when to use inner visit?

object Typer {

  import TypeError._

  sealed trait TypeError {
    def format: String
  }

  object TypeError {

    // TODO: Need nice format of these.
    // TODO: And improve error names etc.

    // TODO: Currently we are a bit lacking for source locations here.
    case class ExpectedType(expected: TypedAst.Type, actual: TypedAst.Type) extends TypeError {
      val format = s"Error: Expected an expression of type '${expected}' but got: ${actual}.\n"
    }

    // TODO: Currently we are a bit lacking for source locations here.
    case class ExpectedEqualTypes(tpe1: TypedAst.Type, tpe2: TypedAst.Type) extends TypeError {
      val format = s"Error: Expected expressions of the same type, but got '${tpe1}' and ${tpe1}.\n"
    }

    case class IllegalPattern(pat: ResolvedAst.Pattern, tpe: TypedAst.Type) extends TypeError {
      val format = s"Error: Pattern '${pat}' does not match expected type '${tpe}'.\n"
    }

    case class IllegalApply(tpe: TypedAst.Type) extends TypeError {
      val format = s"Error: Expected function, but expression has type '${tpe}'.\n"

    }

  }

  /**
   * Runs the typer on the entire given AST `rast`.
   */
  def typecheck(root: ResolvedAst.Root): Validation[TypedAst.Root, TypeError] = {
    // constants
    val constantsVal = Validation.fold(root.constants) {
      case (name, constant) => Definition.typer(constant, root) map (defn => name -> defn)
    }

    // lattices
    val latticesVal = Validation.fold(root.lattices) {
      case (tpe, lattice) => Definition.typer(lattice, root) map (defn => Type.typer(tpe) -> defn)
    }

    //relations
    val relationsVal = Validation.fold(root.relations) {
      case (name, relation) => Definition.typer(relation, root) map (defn => name -> defn)
    }

    // facts and rules
    val factsVal = @@(root.facts.map(fact => Constraint.typer(fact, root)))
    val rulesVal = @@(root.rules.map(rule => Constraint.typer(rule, root)))

    // putting it all together
    @@(constantsVal, latticesVal, relationsVal, factsVal, rulesVal) map {
      case (constants, lattices, relations, facts, rules) => TypedAst.Root(constants, lattices, relations, facts, rules)
    }
  }

  object Definition {
    /**
     * Types the given constant definition `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Definition.Constant, root: ResolvedAst.Root): Validation[TypedAst.Definition.Constant, TypeError] = {
      val declaredType = Type.typer(rast.tpe)
      Expression.typer(rast.exp, root) flatMap {
        case e => expect(declaredType, e.tpe) map {
          case tpe => TypedAst.Definition.Constant(rast.name, e, tpe)
        }
      }
    }

    /**
     * Types the given lattice definition `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Definition.Lattice, root: ResolvedAst.Root): Validation[TypedAst.Definition.Lattice, TypeError] = {
      val tpe = Type.typer(rast.tpe)
      val botType = tpe
      val leqType = TypedAst.Type.Function(args = List(tpe, tpe), retTpe = TypedAst.Type.Bool)
      val lubType = TypedAst.Type.Function(args = List(tpe, tpe), retTpe = tpe)

      val botVal = Expression.typer(rast.bot, root) flatMap {
        case e => expect(botType, e.tpe) map (_ => e)
      }
      val leqVal = Expression.typer(rast.leq, root) flatMap {
        case e => expect(leqType, e.tpe) map (_ => e)
      }
      val lubVal = Expression.typer(rast.lub, root) flatMap {
        case e => expect(lubType, e.tpe) map (_ => e)
      }

      @@(botVal, leqVal, lubVal) map {
        case (bot, leq, lub) => TypedAst.Definition.Lattice(tpe, bot, leq, lub)
      }
    }

    def typer(rast: ResolvedAst.Definition.Relation, root: ResolvedAst.Root): Validation[TypedAst.Definition.Relation, TypeError] = {
      ???
    }

  }

  object Constraint {

    def typer(rast: ResolvedAst.Constraint.Fact, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Fact, TypeError] = {
      ???
    }

    def typer(rast: ResolvedAst.Constraint.Rule, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Rule, TypeError] = {
      ???
    }
  }

  object Literal {

    /**
     * Types the given resolved literal `rast`.
     */
    def typer(rast: ResolvedAst.Literal, root: ResolvedAst.Root): TypedAst.Literal = {
      def visit(rast: ResolvedAst.Literal): TypedAst.Literal = rast match {
        case ResolvedAst.Literal.Unit => TypedAst.Literal.Unit
        case ResolvedAst.Literal.Bool(b) => TypedAst.Literal.Bool(b)
        case ResolvedAst.Literal.Int(i) => TypedAst.Literal.Int(i)
        case ResolvedAst.Literal.Str(s) => TypedAst.Literal.Str(s)
        case ResolvedAst.Literal.Tag(name, ident, rlit) =>
          val defn = root.enums(name)
          val cases = defn.cases.map {
            case (tag, tpe) => tag -> Type.typer(tpe).asInstanceOf[TypedAst.Type.Tag]
          }
          TypedAst.Literal.Tag(name, ident, visit(rlit), TypedAst.Type.Enum(cases))
        case ResolvedAst.Literal.Tuple(relms) =>
          val elms = relms map visit
          TypedAst.Literal.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)))
      }

      visit(rast)
    }
  }


  object Expression {

    /**
     * Types the given resolved expression `rast` under the given ast `root` and local environment `env`.
     */
    def typer(rast: ResolvedAst.Expression, root: ResolvedAst.Root, env: Map[String, TypedAst.Type] = Map.empty): Validation[TypedAst.Expression, TypeError] = {
      def visit(rast: ResolvedAst.Expression, env: Map[String, TypedAst.Type]): Validation[TypedAst.Expression, TypeError] = rast match {
        case ResolvedAst.Expression.Var(ident) =>
          val tpe = env(ident.name)
          TypedAst.Expression.Var(ident, tpe).toSuccess

        case ResolvedAst.Expression.Ref(name) =>
          val constant = root.constants(name)
          val tpe = Type.typer(constant.tpe)
          TypedAst.Expression.Ref(name, tpe).toSuccess

        case ResolvedAst.Expression.Lit(rlit) =>
          val lit = Literal.typer(rlit, root)
          TypedAst.Expression.Lit(lit, lit.tpe).toSuccess

          // TODO: Peer review
        case ResolvedAst.Expression.Lambda(rargs, rtpe, rbody) =>
          // compile formal arguments
          val args = rargs map {
            case ResolvedAst.FormalArg(ident, t) => TypedAst.FormalArg(ident, Type.typer(t))
          }
          // return type
          val tpe = Type.typer(rtpe)
          // create extended environment
          val env1 = args.foldLeft(env) {
            case (m, TypedAst.FormalArg(ident, t)) => m + (ident.name -> t)
          }

          // type body
          visit(rbody, env1) flatMap {
            case body => expect(tpe, body.tpe) map {
              case _ => TypedAst.Expression.Lambda(args, body, TypedAst.Type.Function(args map (_.tpe), tpe))
            }
          }

        // TODO: Peer review
        case ResolvedAst.Expression.Apply(re, rargs) =>
          val lambdaVal = visit(re, env)
          val argsVal = @@(rargs map (arg => visit(arg, env)))

          @@(lambdaVal, argsVal) flatMap {
            case (lambda, args) => lambda.tpe match {
              case TypedAst.Type.Function(targs, retTpe) =>
                val argsVal = (targs zip args) map {
                  case (formalType, actualExp) => expect(formalType, actualExp.tpe)
                }

                @@(argsVal) map {
                  case _ => TypedAst.Expression.Apply(lambda, args, retTpe)
                }
              case tpe => IllegalApply(tpe).toFailure
            }
          }

        case ResolvedAst.Expression.Unary(op, re) => op match {
          case UnaryOperator.Not =>
            visit(re, env) flatMap {
              case e => expect(TypedAst.Type.Bool, e.tpe) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe)
              }
            }
          case UnaryOperator.UnaryPlus | UnaryOperator.UnaryMinus =>
            visit(re, env) flatMap {
              case e => expect(TypedAst.Type.Int, e.tpe) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe)
              }
            }
        }

        case ResolvedAst.Expression.Binary(op, re1, re2) => op match {
          case _: ArithmeticOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Int, e1.tpe), expect(TypedAst.Type.Int, e2.tpe)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Int)
              }
            }
          case _: ComparisonOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Int, e1.tpe), expect(TypedAst.Type.Int, e2.tpe)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool)
              }
            }
          case _: EqualityOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => expectEqual(e1.tpe, e2.tpe) map {
                case tpe => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool)
              }
            }
          case _: LogicalOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Bool, e1.tpe), expect(TypedAst.Type.Bool, e2.tpe)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool)
              }
            }
        }

        case ResolvedAst.Expression.IfThenElse(re1, re2, re3) =>
          @@(visit(re1, env), visit(re2, env), visit(re3, env)) flatMap {
            case (e1, e2, e3) =>
              val conditionType = expect(TypedAst.Type.Bool, e1.tpe)
              val expressionType = expectEqual(e2.tpe, e3.tpe)
              #@(conditionType, expressionType) map {
                case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe)
              }
          }

        case ResolvedAst.Expression.Let(ident, rvalue, rbody) =>
          visit(rvalue, env) flatMap {
            case value => visit(rbody, env + (ident.name -> value.tpe)) map {
              case body => TypedAst.Expression.Let(ident, value, body, body.tpe)
            }
          }

        case ResolvedAst.Expression.Match(re, rules) =>

          ???

        case ResolvedAst.Expression.Tag(enumName, tagName, re) =>
          visit(re, env) flatMap {
            case e =>
              val enum = root.enums(enumName)
              val cases = enum.cases.mapValues(t => Type.typer(t).asInstanceOf[TypedAst.Type.Tag])
              val caze = cases(tagName.name)
              expect(caze.tpe, e.tpe) map {
                _ => TypedAst.Expression.Tag(enumName, tagName, e, TypedAst.Type.Enum(cases))
              }
          }

        case ResolvedAst.Expression.Tuple(relms) =>
          @@(relms map (e => visit(e, env))) map {
            case elms => TypedAst.Expression.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)))
          }

        case ResolvedAst.Expression.Ascribe(re, rtype) =>
          visit(re, env) flatMap {
            case e => expect(Type.typer(rtype), e.tpe) map {
              case _ => e
            }
          }

        case ResolvedAst.Expression.Error(location) =>
          throw Compiler.InternalCompilerError("Error expression not yet supported.")

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
    def typer(rast: ResolvedAst.Pattern, tpe: TypedAst.Type, root: ResolvedAst.Root): Validation[TypedAst.Pattern, TypeError] = rast match {
      case ResolvedAst.Pattern.Wildcard(loc) =>
        TypedAst.Pattern.Wildcard(tpe).toSuccess
      case ResolvedAst.Pattern.Var(ident) =>
        TypedAst.Pattern.Var(ident, tpe).toSuccess
      case ResolvedAst.Pattern.Lit(rlit) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe) map {
          case _ => TypedAst.Pattern.Lit(lit, tpe)
        }
      case ResolvedAst.Pattern.Tag(enumName, tagName, rpat) => tpe match {
        case TypedAst.Type.Tag(enumName2, tagName2, tpe2) if enumName == enumName2 && tagName.name == tagName2.name =>
          typer(rpat, tpe2, root) map {
            case pat => TypedAst.Pattern.Tag(enumName, tagName, pat, TypedAst.Type.Tag(enumName, tagName, pat.tpe))
          }
        case _ => IllegalPattern(rast, tpe).toFailure
      }
      case ResolvedAst.Pattern.Tuple(relms) => tpe match {
        case TypedAst.Type.Tuple(telms) if relms.length == telms.length =>
          val elmsVal = (relms zip telms) map {
            case (rp, tp) => typer(rp, tp, root)
          }
          @@(elmsVal) map {
            case elms => TypedAst.Pattern.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)))
          }
        case _ => IllegalPattern(rast, tpe).toFailure
      }
    }
  }

  object Type {

    /**
     * Translates a type from the resolved AST into one of the typed AST.
     */
    def typer(rast: ResolvedAst.Type): TypedAst.Type = rast match {
      case ResolvedAst.Type.Unit => TypedAst.Type.Unit
      case ResolvedAst.Type.Bool => TypedAst.Type.Bool
      case ResolvedAst.Type.Int => TypedAst.Type.Int
      case ResolvedAst.Type.Str => TypedAst.Type.Str
      case ResolvedAst.Type.Tag(name, ident, tpe) => TypedAst.Type.Tag(name, ident, typer(tpe))
      case ResolvedAst.Type.Tuple(elms) => TypedAst.Type.Tuple(elms map typer)
      case ResolvedAst.Type.Function(args, retTpe) => TypedAst.Type.Function(args map typer, typer(retTpe))
    }

  }

  /**
   * Returns the given `expected` type wrapped in [[Success]] if it matches the given `actual` type.
   *
   * @param expected the expected type.
   * @param actual the actual type.
   */
  def expect(expected: TypedAst.Type, actual: TypedAst.Type): Validation[TypedAst.Type, TypeError] =
    if (expected == actual)
      actual.toSuccess
    else
      ExpectedType(expected, actual).toFailure

  /**
   * Returns the given `tpe` type wrapped in [[Success]] if it matches the given `tpe2` type.
   *
   * @param tpe1 the first type.
   * @param tpe2 the second type.
   */
  def expectEqual(tpe1: TypedAst.Type, tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] =
    if (tpe1 == tpe2)
      tpe1.toSuccess
    else
      ExpectedEqualTypes(tpe1, tpe2).toFailure

}
