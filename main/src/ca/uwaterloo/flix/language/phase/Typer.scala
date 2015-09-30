package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation
import Validation._

// TODO: Check code coverage.
// TODO: when to use inner visit?
// TODO: use pattern match on rast

object Typer {

  import TypeError._

  sealed trait TypeError {
    def format: String
  }

  object TypeError {

    // TODO: Arity Error.
    // TODO: Currently we are a bit lacking for source locations here.
    // TODOL Doc

    case class ExpectedType(expected: TypedAst.Type, actual: TypedAst.Type) extends TypeError {
      val format = s"Type Error: Expected an expression of type '${expected.format}' but got: ${actual.format}.\n"
    }

    case class ExpectedEqualTypes(tpe1: TypedAst.Type, tpe2: TypedAst.Type) extends TypeError {
      val format = s"Type Error: Expected expressions of the same type, but got '${tpe1.format}' and ${tpe2.format}.\n"
    }

    case class IllegalPattern(pat: ResolvedAst.Pattern, tpe: TypedAst.Type) extends TypeError {
      val format = s"Type Error: Pattern '${pat.format}' does not match expected type '${tpe.format}'.\n"
    }

    case class IllegalApply(tpe: TypedAst.Type) extends TypeError {
      val format = s"Type Error: Expected function, but expression has type '${tpe.format}'.\n"

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
          case tpe => TypedAst.Definition.Constant(rast.name, e, tpe, rast.loc)
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
        case (bot, leq, lub) => TypedAst.Definition.Lattice(tpe, bot, leq, lub, rast.loc)
      }
    }

    /**
     * Types the given relation definition `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Definition.Relation, root: ResolvedAst.Root): Validation[TypedAst.Definition.Relation, TypeError] = {
      val attributes = rast.attributes map {
        case ResolvedAst.Attribute(ident, tpe) => TypedAst.Attribute(ident, Type.typer(tpe))
      }
      TypedAst.Definition.Relation(rast.name, attributes, rast.loc).toSuccess
    }

  }

  object Constraint {

    /**
     * Types the given fact `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Constraint.Fact, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Fact, TypeError] = {
      Predicate.typer(rast.head, root) map TypedAst.Constraint.Fact
    }

    /**
     * Types the given rule `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Constraint.Rule, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Rule, TypeError] = {
      val headVal = Predicate.typer(rast.head, root)
      // TODO: Should check that variables have consistent types?
      val bodyVal = @@(rast.body map (p => Predicate.typer(p, root)))

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
        case ResolvedAst.Literal.Int(i, loc) => TypedAst.Literal.Int(i, loc)
        case ResolvedAst.Literal.Str(s, loc) => TypedAst.Literal.Str(s, loc)
        case ResolvedAst.Literal.Tag(name, ident, rlit, loc) =>
          val defn = root.enums(name)
          val cases = defn.cases.map {
            case (tag, tpe) => tag -> Type.typer(tpe).asInstanceOf[TypedAst.Type.Tag]
          }
          TypedAst.Literal.Tag(name, ident, visit(rlit), TypedAst.Type.Enum(cases), loc)
        case ResolvedAst.Literal.Tuple(relms, loc) =>
          val elms = relms map visit
          TypedAst.Literal.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)), loc)
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
        case ResolvedAst.Expression.Var(ident, loc) =>
          val tpe = env(ident.name)
          TypedAst.Expression.Var(ident, tpe, loc).toSuccess

        case ResolvedAst.Expression.Ref(name, loc) =>
          val constant = root.constants(name)
          val tpe = Type.typer(constant.tpe)
          TypedAst.Expression.Ref(name, tpe, loc).toSuccess

        case ResolvedAst.Expression.Lit(rlit, loc) =>
          val lit = Literal.typer(rlit, root)
          TypedAst.Expression.Lit(lit, lit.tpe, loc).toSuccess

        // TODO: Peer review
        case ResolvedAst.Expression.Lambda(rargs, rtpe, rbody, loc) =>
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
              case _ => TypedAst.Expression.Lambda(args, body, TypedAst.Type.Function(args map (_.tpe), tpe), loc)
            }
          }

        // TODO: Peer review
        case ResolvedAst.Expression.Apply(re, rargs, loc) =>
          val lambdaVal = visit(re, env)
          val argsVal = @@(rargs map (arg => visit(arg, env)))

          @@(lambdaVal, argsVal) flatMap {
            case (lambda, args) => lambda.tpe match {
              case TypedAst.Type.Function(targs, retTpe) =>
                val argsVal = (targs zip args) map {
                  case (formalType, actualExp) => expect(formalType, actualExp.tpe)
                }

                @@(argsVal) map {
                  case _ => TypedAst.Expression.Apply(lambda, args, retTpe, loc)
                }
              case tpe => IllegalApply(tpe).toFailure
            }
          }

        case ResolvedAst.Expression.Unary(op, re, loc) => op match {
          case UnaryOperator.Not =>
            visit(re, env) flatMap {
              case e => expect(TypedAst.Type.Bool, e.tpe) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe, loc)
              }
            }
          case UnaryOperator.UnaryPlus | UnaryOperator.UnaryMinus =>
            visit(re, env) flatMap {
              case e => expect(TypedAst.Type.Int, e.tpe) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe, loc)
              }
            }
        }

        case ResolvedAst.Expression.Binary(op, re1, re2, loc) => op match {
          case _: ArithmeticOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Int, e1.tpe), expect(TypedAst.Type.Int, e2.tpe)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Int, loc)
              }
            }
          case _: ComparisonOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Int, e1.tpe), expect(TypedAst.Type.Int, e2.tpe)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool, loc)
              }
            }
          case _: EqualityOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => expectEqual(e1.tpe, e2.tpe) map {
                case tpe => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool, loc)
              }
            }
          case _: LogicalOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Bool, e1.tpe), expect(TypedAst.Type.Bool, e2.tpe)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool, loc)
              }
            }
        }

        case ResolvedAst.Expression.IfThenElse(re1, re2, re3, loc) =>
          @@(visit(re1, env), visit(re2, env), visit(re3, env)) flatMap {
            case (e1, e2, e3) =>
              val conditionType = expect(TypedAst.Type.Bool, e1.tpe)
              val expressionType = expectEqual(e2.tpe, e3.tpe)
              #@(conditionType, expressionType) map {
                case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)
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
                    case typedPat => visit(body, env ++ typedPat.bound) map {
                      case typedBody => (typedPat, typedBody)
                    }
                  }
              }
              @@(rulesVal) flatMap {
                case rules =>
                  // ensure that the body of every rule has the same type.
                  expectEqual(rules.map(_._2.tpe)) map {
                    case tpe => TypedAst.Expression.Match(matchValue, rules, tpe, loc)
                  }
              }
          }

        case ResolvedAst.Expression.Tag(enumName, tagName, re, loc) =>
          visit(re, env) flatMap {
            case e =>
              val enum = root.enums(enumName)
              val cases = enum.cases.mapValues(t => Type.typer(t).asInstanceOf[TypedAst.Type.Tag])
              val caze = cases(tagName.name)
              expect(caze.tpe, e.tpe) map {
                _ => TypedAst.Expression.Tag(enumName, tagName, e, TypedAst.Type.Enum(cases), loc)
              }
          }

        case ResolvedAst.Expression.Tuple(relms, loc) =>
          @@(relms map (e => visit(e, env))) map {
            case elms => TypedAst.Expression.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)), loc)
          }

        case ResolvedAst.Expression.Ascribe(re, rtype, loc) =>
          visit(re, env) flatMap {
            case e => expect(Type.typer(rtype), e.tpe) map {
              case _ => e
            }
          }
        case ResolvedAst.Expression.Error(tpe, loc) =>
          TypedAst.Expression.Error(Type.typer(tpe), loc).toSuccess
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
        TypedAst.Pattern.Wildcard(tpe, loc).toSuccess
      case ResolvedAst.Pattern.Var(ident, loc) =>
        TypedAst.Pattern.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Pattern.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe) map {
          case _ => TypedAst.Pattern.Lit(lit, tpe, loc)
        }
      case ResolvedAst.Pattern.Tag(enumName, tagName, rpat, loc) => tpe match {
        case TypedAst.Type.Enum(cases) => cases.get(tagName.name) match {
          case Some(tag) if enumName == tag.name => {
            typer(rpat, tag.tpe, root) map {
              case pat => TypedAst.Pattern.Tag(enumName, tagName, pat, TypedAst.Type.Tag(enumName, tagName, pat.tpe), loc)
            }
          }
          case _ => IllegalPattern(rast, tpe).toFailure
        }
        case _ => IllegalPattern(rast, tpe).toFailure
      }
      case ResolvedAst.Pattern.Tuple(relms, loc) => tpe match {
        case TypedAst.Type.Tuple(telms) if relms.length == telms.length =>
          val elmsVal = (relms zip telms) map {
            case (rp, tp) => typer(rp, tp, root)
          }
          @@(elmsVal) map {
            case elms => TypedAst.Pattern.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)), loc)
          }
        case _ => IllegalPattern(rast, tpe).toFailure
      }
    }
  }

  object Predicate {

    /**
     * Types the given head predicate `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Predicate.Head, root: ResolvedAst.Root): Validation[TypedAst.Predicate.Head, TypeError] = {
      val relation = root.relations(rast.name)
      val termsVal = (rast.terms zip relation.attributes) map {
        case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, Type.typer(tpe), root)
      }

      @@(termsVal) map {
        case terms =>
          // TODO
          //          val vars = Validation.fold(terms, Map.empty[String, TypedAst.Type]) {
          //            case (macc, term) => ???
          //          }

          TypedAst.Predicate.Head(rast.name, terms, TypedAst.Type.Predicate(terms map (_.tpe)), rast.loc)
      }
    }

    /**
     * Types the given body predicate `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Predicate.Body, root: ResolvedAst.Root): Validation[TypedAst.Predicate.Body, TypeError] = {
      val relation = root.relations(rast.name)
      val termsVal = (rast.terms zip relation.attributes) map {
        case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, Type.typer(tpe), root)
      }

      @@(termsVal) map {
        case terms => TypedAst.Predicate.Body(rast.name, terms, TypedAst.Type.Predicate(terms map (_.tpe)), rast.loc)
      }
    }
  }

  object Term {
    /**
     * Types the given head term `rast` according to the (declared) type `tpe` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Term.Head, tpe: TypedAst.Type, root: ResolvedAst.Root): Validation[TypedAst.Term.Head, TypeError] = rast match {
      case ResolvedAst.Term.Head.Var(ident, loc) => TypedAst.Term.Head.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Term.Head.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe) map {
          case _ => TypedAst.Term.Head.Lit(lit, lit.tpe, loc)
        }
      case ResolvedAst.Term.Head.Ascribe(rterm, rtpe, loc) =>
        val ascribedType = Type.typer(rtpe)
        expect(ascribedType, tpe) flatMap {
          case _ => typer(rterm, tpe, root)
        }
      case ResolvedAst.Term.Head.Apply(name, actuals, loc) =>
        // TODO: This needs to be rewritten

        val constant = root.constants(name)
        // TODO: This might actually be slightly problematic, since not every constant may be a fully evalauted lambda.
        // Instead we should focus on the type of the constant, which should be Function.

        constant.exp match {
          case ResolvedAst.Expression.Lambda(formals, retTpe, _, loc) =>
            // type arguments with the declared formals.
            val argsVal = (actuals zip formals) map {
              case (term, ResolvedAst.FormalArg(_, termType)) => Term.typer(term, Type.typer(termType), root)
            }
            // put everything together and check the return type.
            @@(@@(argsVal), expect(tpe, Type.typer(retTpe))) map {
              case (args, returnType) => TypedAst.Term.Head.Apply(name, args, returnType, loc)
            }
          case _ => ??? // TODO non-function call.
        }
    }

    /**
     * Types the given body term `rast` according to the given type `tpe`. under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Term.Body, tpe: TypedAst.Type, root: ResolvedAst.Root): Validation[TypedAst.Term.Body, TypeError] = rast match {
      case ResolvedAst.Term.Body.Wildcard(loc) => TypedAst.Term.Body.Wildcard(tpe, loc).toSuccess
      case ResolvedAst.Term.Body.Var(ident, loc) => TypedAst.Term.Body.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Term.Body.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe) map {
          case _ => TypedAst.Term.Body.Lit(lit, lit.tpe, loc)
        }
      case ResolvedAst.Term.Body.Ascribe(rterm, rtpe, loc) =>
        val ascribedType = Type.typer(rtpe)
        expect(ascribedType, tpe) flatMap {
          case _ => typer(rterm, tpe, root)
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
      case ResolvedAst.Type.Enum(rcases) =>
        val cases = rcases.mapValues {
          case tpe => typer(tpe).asInstanceOf[TypedAst.Type.Tag]
        }
        TypedAst.Type.Enum(cases)
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

  /**
   * Returns a type wrapped in [[Success]] if all the given `types` are equal.
   */
  def expectEqual(types: List[TypedAst.Type]): Validation[TypedAst.Type, TypeError] = {
    assert(types.nonEmpty)
    val tpe1 = types.head
    if (types.forall(t => t == tpe1)) {
      tpe1.toSuccess
    } else {
      val tpe2 = types.find(t => t != tpe1).get
      ExpectedEqualTypes(tpe1, tpe2).toFailure
    }
  }

}
