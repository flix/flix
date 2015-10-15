package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Typer {

  // TODO: when to use inner visit?
  // TODO: use pattern match on rast
  // TODO: Probably need to rewrite this to be unification based.
  // TODO: Check that lattice variables are not bound multiple times.

  import TypeError._

  /**
   * A common super-type for type errors.
   */
  sealed trait TypeError extends Compiler.CompilationError

  object TypeError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
     * An error raised to indicate a type mismatch between an `expected` and an `actual` type.
     *
     * @param expected the expected type.
     * @param actual the actual type.
     * @param loc the source location.
     */
    case class ExpectedType(expected: TypedAst.Type, actual: TypedAst.Type, loc: SourceLocation) extends TypeError {
      val format =
        s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc.formatSource}")}
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
    case class ExpectedEqualTypes(tpe1: TypedAst.Type, tpe2: TypedAst.Type, loc1: SourceLocation, loc2: SourceLocation) extends TypeError {
      val format =
        s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc1.formatSource}")}
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
    case class IllegalApply(tpe: TypedAst.Type, loc: SourceLocation) extends TypeError {
      val format = s"Type Error: The type '${prettyPrint(tpe)}' is not a function type at ${loc.format}.\n"
    }

    /**
     * An error raised to indicate a type mismatch between a pattern `pat` and an expected type `tpe`.
     *
     * @param pat the pattern.
     * @param tpe the type.
     * @param loc the source location.
     */
    // TODO: Pretty print
    case class IllegalPattern(pat: ResolvedAst.Pattern, tpe: TypedAst.Type, loc: SourceLocation) extends TypeError {
      val format = s"Type Error: Pattern '${prettyPrint(pat)}' does not match expected type '${prettyPrint(tpe)}' at ${loc.format}.\n"
    }

    // TODO: Check arity of function calls, predicates, etc.

  }

  /**
   * Runs the typer on the entire given AST `rast`.
   */
  def typecheck(root: ResolvedAst.Root): Validation[TypedAst.Root, TypeError] = {
    // constants
    val constantsVal = Validation.fold(root.constants) {
      case (name, constant) => Definition.typer(constant, root) map (defn => name -> defn)
    }

    // directives
    val directivesVal = @@(root.directives.map(directive => Directive.typer(directive, root)))

    // lattices
    val latticesVal = Validation.fold(root.lattices) {
      case (tpe, lattice) => Definition.typer(lattice, root) map (defn => Type.typer(tpe) -> defn)
    }

    //relations
    val relationsVal = Validation.fold(root.collections) {
      case (name, relation) => Definition.typer(relation, root) map (defn => name -> defn)
    }

    // facts and rules
    val factsVal = @@(root.facts.map(fact => Constraint.typer(fact, root)))
    val rulesVal = @@(root.rules.map(rule => Constraint.typer(rule, root)))

    // putting it all together
    @@(constantsVal, directivesVal, latticesVal, relationsVal, factsVal, rulesVal) map {
      case (constants, directives, lattices, relations, facts, rules) => TypedAst.Root(constants, TypedAst.Directives(directives), lattices, relations, facts, rules)
    }
  }

  object Definition {
    /**
     * Types the given constant definition `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Definition.Constant, root: ResolvedAst.Root): Validation[TypedAst.Definition.Constant, TypeError] = {
      val declaredType = Type.typer(rast.tpe)
      Expression.typer(rast.exp, root) flatMap {
        case e => expect(declaredType, e.tpe, rast.loc) map {
          case tpe => TypedAst.Definition.Constant(rast.name, e, tpe, rast.loc)
        }
      }
    }

    /**
     * Types the given lattice definition `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Definition.BoundedLattice, root: ResolvedAst.Root): Validation[TypedAst.Definition.BoundedLattice, TypeError] = {
      val tpe = Type.typer(rast.tpe)
      val leqType = TypedAst.Type.Lambda(args = List(tpe, tpe), retTpe = TypedAst.Type.Bool)
      val lubType = TypedAst.Type.Lambda(args = List(tpe, tpe), retTpe = tpe)
      val glbType = TypedAst.Type.Lambda(args = List(tpe, tpe), retTpe = tpe)

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
     * Types the given collection definition `rast` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Definition.Collection, root: ResolvedAst.Root): Validation[TypedAst.Definition.Collection, TypeError] = rast match {
      case d: ResolvedAst.Definition.Relation => typer2(d, root)
      case d: ResolvedAst.Definition.Lattice => typer2(d, root)
    }

    /**
     * Types the given relation definition `rast` under the given AST `root`.
     */
    def typer2(rast: ResolvedAst.Definition.Relation, root: ResolvedAst.Root): Validation[TypedAst.Definition.Relation, TypeError] = {
      val attributes = rast.attributes map {
        case ResolvedAst.Attribute(ident, tpe) => TypedAst.Attribute(ident, Type.typer(tpe))
      }
      TypedAst.Definition.Relation(rast.name, attributes, rast.loc).toSuccess
    }

    /**
     * Types the given lattice definition `rast` under the given AST `root`.
     */
    def typer2(rast: ResolvedAst.Definition.Lattice, root: ResolvedAst.Root): Validation[TypedAst.Definition.Lattice, TypeError] = {
      val keys = rast.keys map {
        case ResolvedAst.Attribute(ident, tpe) => TypedAst.Attribute(ident, Type.typer(tpe))
      }
      val values = rast.values map {
        case ResolvedAst.Attribute(ident, tpe) => TypedAst.Attribute(ident, Type.typer(tpe))
      }

      TypedAst.Definition.Lattice(rast.name, keys, values, rast.loc).toSuccess
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

  object Directive {

    /**
     * Types the given resolved directive `rast`.
     */
    def typer(rast: ResolvedAst.Directive, root: ResolvedAst.Root): Validation[TypedAst.Directive, TypeError] = rast match {
      case ResolvedAst.Directive.AssertFact(fact, loc) => Constraint.typer(fact, root) map {
        case f => TypedAst.Directive.AssertFact(f, loc)
      }
      case ResolvedAst.Directive.AssertRule(rule, loc) => Constraint.typer(rule, root) map {
        case r => TypedAst.Directive.AssertRule(r, loc)
      }
      case ResolvedAst.Directive.Print(name, loc) => TypedAst.Directive.Print(name, loc).toSuccess
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
            case body => expect(tpe, body.tpe, loc) map {
              case _ => TypedAst.Expression.Lambda(args, body, TypedAst.Type.Lambda(args map (_.tpe), tpe), loc)
            }
          }

        // TODO: Peer review
        case ResolvedAst.Expression.Apply(re, rargs, loc) =>
          val lambdaVal = visit(re, env)
          val argsVal = @@(rargs map (arg => visit(arg, env)))

          @@(lambdaVal, argsVal) flatMap {
            case (lambda, args) => lambda.tpe match {
              case TypedAst.Type.Lambda(targs, retTpe) =>
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
          case UnaryOperator.Not =>
            visit(re, env) flatMap {
              case e => expect(TypedAst.Type.Bool, e.tpe, loc) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe, loc)
              }
            }
          case UnaryOperator.UnaryPlus | UnaryOperator.UnaryMinus =>
            visit(re, env) flatMap {
              case e => expect(TypedAst.Type.Int, e.tpe, loc) map {
                case tpe => TypedAst.Expression.Unary(op, e, tpe, loc)
              }
            }
        }

        case ResolvedAst.Expression.Binary(op, re1, re2, loc) => op match {
          case _: ArithmeticOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Int, e1.tpe, e1.loc), expect(TypedAst.Type.Int, e2.tpe, e2.loc)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Int, loc)
              }
            }
          case _: ComparisonOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Int, e1.tpe, e1.loc), expect(TypedAst.Type.Int, e2.tpe, e2.loc)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool, loc)
              }
            }
          case _: EqualityOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => expectEqual(e1.tpe, e2.tpe, e1.loc, e2.loc) map {
                case tpe => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool, loc)
              }
            }
          case _: LogicalOperator =>
            @@(visit(re1, env), visit(re2, env)) flatMap {
              case (e1, e2) => @@(expect(TypedAst.Type.Bool, e1.tpe, e1.loc), expect(TypedAst.Type.Bool, e2.tpe, e2.loc)) map {
                case (tpe1, tpe2) => TypedAst.Expression.Binary(op, e1, e2, TypedAst.Type.Bool, loc)
              }
            }
        }

        case ResolvedAst.Expression.IfThenElse(re1, re2, re3, loc) =>
          @@(visit(re1, env), visit(re2, env), visit(re3, env)) flatMap {
            case (e1, e2, e3) =>
              val conditionType = expect(TypedAst.Type.Bool, e1.tpe, e1.loc)
              val expressionType = expectEqual(e2.tpe, e3.tpe, e2.loc, e3.loc)
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
              val cases = enum.cases.mapValues(t => Type.typer(t).asInstanceOf[TypedAst.Type.Tag])
              val caze = cases(tagName.name)
              expect(caze.tpe, e.tpe, e.loc) map {
                _ => TypedAst.Expression.Tag(enumName, tagName, e, TypedAst.Type.Enum(cases), loc)
              }
          }

        case ResolvedAst.Expression.Tuple(relms, loc) =>
          @@(relms map (e => visit(e, env))) map {
            case elms => TypedAst.Expression.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)), loc)
          }

        case ResolvedAst.Expression.Ascribe(re, rtype, loc) =>
          visit(re, env) flatMap {
            case e => expect(Type.typer(rtype), e.tpe, loc) map {
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
        expect(tpe, lit.tpe, loc) map {
          case _ => TypedAst.Pattern.Lit(lit, tpe, loc)
        }
      case ResolvedAst.Pattern.Tag(enumName, tagName, rpat, loc) => tpe match {
        case TypedAst.Type.Enum(cases) => cases.get(tagName.name) match {
          case Some(tag) if enumName == tag.name => {
            typer(rpat, tag.tpe, root) map {
              case pat => TypedAst.Pattern.Tag(enumName, tagName, pat, TypedAst.Type.Tag(enumName, tagName, pat.tpe), loc)
            }
          }
          case _ => IllegalPattern(rast, tpe, loc).toFailure
        }
        case _ => IllegalPattern(rast, tpe, loc).toFailure
      }
      case ResolvedAst.Pattern.Tuple(relms, loc) => tpe match {
        case TypedAst.Type.Tuple(telms) if relms.length == telms.length =>
          val elmsVal = (relms zip telms) map {
            case (rp, tp) => typer(rp, tp, root)
          }
          @@(elmsVal) map {
            case elms => TypedAst.Pattern.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)), loc)
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
        case ResolvedAst.Predicate.Head.Relation(name, rterms, loc) =>
          // lookup the collection.
          root.collections(name) match {
            case ResolvedAst.Definition.Relation(_, attributes, _) =>
              // type check the terms against the attributes.
              val termsVal = (rterms zip attributes) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, Type.typer(tpe), root)
              }

              @@(termsVal) map {
                case terms =>
                  TypedAst.Predicate.Head.Relation(name, terms, TypedAst.Type.Predicate(terms map (_.tpe)), loc)
              }

            case ResolvedAst.Definition.Lattice(_, keys, values, _) =>
              // type check the terms against the keys and values.
              // TODO: More checks?
              val termsVal = (rterms zip (keys ::: values)) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, Type.typer(tpe), root)
              }

              @@(termsVal) map {
                case terms =>
                  TypedAst.Predicate.Head.Relation(name, terms, TypedAst.Type.Predicate(terms map (_.tpe)), loc)
              }
          }

        case ResolvedAst.Predicate.Head.Trace(rterms, loc) =>
          // TODO: This needs to use proper unification
          @@(rterms map (t => Term.typer(t, TypedAst.Type.Var("x"), root))) map {
            // TODO: The use of Type.Bool here is kind of spurious.
            case terms => TypedAst.Predicate.Head.Trace(terms, TypedAst.Type.Bool, loc)
          }

        case ResolvedAst.Predicate.Head.Write(rterms, rpath, loc) =>
          // TODO: This needs to use proper unification
          @@(@@(rterms map (t => Term.typer(t, TypedAst.Type.Var("x"), root))), Term.typer(rpath, TypedAst.Type.Str, root)) map {
            // TODO: The use of Type.Bool here is kind of spurious.
            case (terms, path) => TypedAst.Predicate.Head.Write(terms, path, TypedAst.Type.Bool, loc)
          }

        case ResolvedAst.Predicate.Head.Error(rterms, loc) =>
          // TODO: This needs to use proper unification
          @@(rterms map (t => Term.typer(t, TypedAst.Type.Var("x"), root))) map {
            // TODO: The use of Type.Bool here is kind of spurious.
            case terms => TypedAst.Predicate.Head.Error(terms, TypedAst.Type.Bool, loc)
          }
      }
    }

    object Body {
      /**
       * Types the given body predicate `rast` under the given AST `root`.
       */
      def typer(rast: ResolvedAst.Predicate.Body, root: ResolvedAst.Root): Validation[TypedAst.Predicate.Body, TypeError] = rast match {
        case ResolvedAst.Predicate.Body.Relation(name, rterms, loc) =>
          // lookup the collection.
          root.collections(name) match {
            case ResolvedAst.Definition.Relation(_, attributes, _) =>
              // type check the terms against the attributes.
              val termsVal = (rterms zip attributes) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, Type.typer(tpe), root)
              }

              @@(termsVal) map {
                case terms => TypedAst.Predicate.Body.Relation(name, terms, TypedAst.Type.Predicate(terms map (_.tpe)), loc)
              }
            case ResolvedAst.Definition.Lattice(_, keys, values, _) =>
              // type check the terms against the attributes.
              // TODO: more checks?
              val termsVal = (rterms zip (keys ::: values)) map {
                case (term, ResolvedAst.Attribute(_, tpe)) => Term.typer(term, Type.typer(tpe), root)
              }

              @@(termsVal) map {
                case terms => TypedAst.Predicate.Body.Relation(name, terms, TypedAst.Type.Predicate(terms map (_.tpe)), loc)
              }
          }

        case ResolvedAst.Predicate.Body.Function(name, rterms, loc) =>
          val constant = root.constants(name)
          // TODO: Check that result type is bool.
          // TODO: Improve the cast here
          val termsVal = (rterms zip constant.tpe.asInstanceOf[ResolvedAst.Type.Function].args) map {
            case (term, tpe) => Term.typer(term, Type.typer(tpe), root)
          }

          @@(termsVal) map {
            case terms => TypedAst.Predicate.Body.Function(name, terms, TypedAst.Type.Lambda(terms map (_.tpe), TypedAst.Type.Bool), loc) // TODO Type
          }

        case ResolvedAst.Predicate.Body.NotEqual(ident1, ident2, loc) =>
          TypedAst.Predicate.Body.NotEqual(ident1, ident2, TypedAst.Type.Bool, loc: SourceLocation).toSuccess

        case ResolvedAst.Predicate.Body.Read(rterms, rpath, loc) =>
          // TODO: This needs to use proper unification
          @@(@@(rterms map (t => Term.typer(t, TypedAst.Type.Var("x"), root))), Term.typer(rpath, TypedAst.Type.Str, root)) map {
            // TODO: The use of Type.Bool here is kind of spurious.
            case (terms, path) => TypedAst.Predicate.Body.Read(terms, path, TypedAst.Type.Bool, loc)
          }

      }
    }

  }

  object Term {
    // TODO: Introduce head/body.

    /**
     * Types the given head term `rast` according to the (declared) type `tpe` under the given AST `root`.
     */
    def typer(rast: ResolvedAst.Term.Head, tpe: TypedAst.Type, root: ResolvedAst.Root): Validation[TypedAst.Term.Head, TypeError] = rast match {
      case ResolvedAst.Term.Head.Var(ident, loc) => TypedAst.Term.Head.Var(ident, tpe, loc).toSuccess
      case ResolvedAst.Term.Head.Lit(rlit, loc) =>
        val lit = Literal.typer(rlit, root)
        expect(tpe, lit.tpe, loc) map {
          case _ => TypedAst.Term.Head.Lit(lit, lit.tpe, loc)
        }
      case ResolvedAst.Term.Head.Ascribe(rterm, rtpe, loc) =>
        val ascribedType = Type.typer(rtpe)
        expect(ascribedType, tpe, loc) flatMap {
          case _ => typer(rterm, tpe, root)
        }
      case ResolvedAst.Term.Head.Apply(name, actuals, loc) =>
        // TODO: This needs to be rewritten

        val constant = root.constants(name)
        // TODO: This might actually be slightly problematic, since not every constant may be a fully evalauted lambda.
        // Instead we should focus on the type of the constant, which should be Function.

        constant.exp match {
          case ResolvedAst.Expression.Lambda(formals, retTpe, _, loc2) =>
            // type arguments with the declared formals.
            val argsVal = (actuals zip formals) map {
              case (term, ResolvedAst.FormalArg(_, termType)) => Term.typer(term, Type.typer(termType), root)
            }
            // put everything together and check the return type.
            @@(@@(argsVal), expect(tpe, Type.typer(retTpe), loc2)) map {
              case (args, returnType) => TypedAst.Term.Head.Apply(name, args, returnType, loc2)
            }
          case _ => IllegalApply(Type.typer(constant.tpe), loc).toFailure
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
        expect(tpe, lit.tpe, loc) map {
          case _ => TypedAst.Term.Body.Lit(lit, lit.tpe, loc)
        }
      case ResolvedAst.Term.Body.Ascribe(rterm, rtpe, loc) =>
        val ascribedType = Type.typer(rtpe)
        expect(ascribedType, tpe, loc) flatMap {
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
      case ResolvedAst.Type.Function(args, retTpe) => TypedAst.Type.Lambda(args map typer, typer(retTpe))
    }

  }

  /**
   * Returns the given `expected` type wrapped in [[Success]] if it matches the given `actual` type.
   *
   * @param expected the expected type.
   * @param actual the actual type.
   * @param loc the source location.
   */
  def expect(expected: TypedAst.Type, actual: TypedAst.Type, loc: SourceLocation): Validation[TypedAst.Type, TypeError] =
    if (expected == actual)
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
  def expectEqual(tpe1: TypedAst.Type, tpe2: TypedAst.Type, loc1: SourceLocation, loc2: SourceLocation): Validation[TypedAst.Type, TypeError] =
    if (tpe1 == tpe2)
      tpe1.toSuccess
    else
      ExpectedEqualTypes(tpe1, tpe2, loc1, loc2).toFailure

  /**
   * Returns a type wrapped in [[Success]] if all the given `types` are equal.
   */
  def expectEqual(types: List[(TypedAst.Type, SourceLocation)]): Validation[TypedAst.Type, TypeError] = {
    assert(types.nonEmpty)
    val (tpe1, loc1) = types.head
    if (types.forall(t => t._1 == tpe1)) {
      tpe1.toSuccess
    } else {
      val (tpe2, loc2) = types.find(t => t._1 != tpe1).get
      ExpectedEqualTypes(tpe1, tpe2, loc1, loc2).toFailure
    }
  }

  /**
   * Returns a human readable string representation of the given type `tpe`.
   */
  private def prettyPrint(tpe: TypedAst.Type): String = tpe match {
    case TypedAst.Type.Var(x) => s"Var($x)"
    case TypedAst.Type.Unit => s"()"
    case TypedAst.Type.Bool => s"Bool"
    case TypedAst.Type.Int => s"Int"
    case TypedAst.Type.Str => s"Str"
    case TypedAst.Type.Tag(enumName, tagName, t) =>
      val enumAndTag = enumName.parts.mkString("::") + "." + tagName.name
      val nested = s"(${prettyPrint(tpe)}})"
      enumAndTag + nested
    case TypedAst.Type.Enum(cases) =>
      s"Enum(${cases.head._2.name})"
    case TypedAst.Type.Tuple(elms) => "(" + elms.map(prettyPrint).mkString(", ") + ")"
    case TypedAst.Type.Lambda(args, retTpe) =>
      "(" + args.map(prettyPrint).mkString(", ") + ") -> " + prettyPrint(retTpe)
    case TypedAst.Type.Predicate(terms) => s"Predicate(${terms map prettyPrint})"
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
