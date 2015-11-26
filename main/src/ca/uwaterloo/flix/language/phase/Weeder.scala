package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.frontend.ast.ParsedAst
import ca.uwaterloo.flix.util.Validation
import Validation._

import scala.collection.mutable

/**
  * The Weeder phase performs simple syntactic checks and rewritings.
  */
object Weeder {

  import WeederError._

  /**
    * A common super-type for weeding errors.
    */
  sealed trait WeederError extends Compiler.CompilationError

  object WeederError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that the alias `name` was defined multiple times.
      *
      * @param name the name of the variable.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateAlias(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate definition of the variable '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the aliases.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the attribute `name` was declared multiple times.
      *
      * @param name the name of the attribute.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateAttribute(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate attribute name '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the attributes.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the formal argument `name` was declared multiple times.
      *
      * @param name the name of the argument.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateFormal(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate formal argument '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the arguments.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the tag `name` was declared multiple times.
      *
      * @param name the name of the tag.
      * @param loc1 the location of the first declaration.
      * @param loc2 the location of the second declaration.
      */
    case class DuplicateTag(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate tag name '$name'.")}
           |
            |First declaration was here:
           |${loc1.underline}
           |Second declaration was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the tags.
         """.stripMargin
    }

    /**
      * An error raised to indicate that a predicate is not allowed in the head of a fact/rule.
      *
      * @param loc the location where the illegal predicate occurs.
      */
    case class IllegalHeadPredicate(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal predicate in the head of a fact/rule.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate that a predicate is not allowed in the body rule.
      *
      * @param loc the location where the illegal predicate occurs.
      */
    case class IllegalBodyPredicate(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal predicate in the body of a rule.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate that a read predicate has too few arguments.
      *
      * @param loc the location where the illegal predicate occurs.
      */
    case class IllegalReadPredicate(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Too few arguments for Read# predicate.")}
           |
            |${loc.underline}
           |A Read# predicate must have atleast two arguments.
         """.stripMargin
    }

    /**
      * An error raised to indicate that a write predicate has too few arguments.
      *
      * @param loc the location where the illegal predicate occurs.
      */
    case class IllegalWritePredicate(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Too few arguments for Write# predicate.")}
           |
            |${loc.underline}
           |A Write# predicate must have atleast two arguments.
         """.stripMargin
    }

    /**
      * An error raised to indicate that an illegal term occurs in a head predicate.
      *
      * @param msg the error message.
      * @param loc the location where the illegal term occurs.
      */
    case class IllegalHeadTerm(msg: String, loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal term in the head of a fact/rule.")}
           |
            |${loc.underline}
           |$msg
         """.stripMargin
    }

    /**
      * An error raised to indicate that an illegal term occurs in a body predicate.
      *
      * @param msg the error message.
      * @param loc the location where the illegal term occurs.
      */
    case class IllegalBodyTerm(msg: String, loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal term in the body of a rule.")}
           |
            |${loc.underline}
           |$msg
         """.stripMargin
    }

    /**
      * An error raised to indicate an illegal bounded lattice definition.
      *
      * @param loc the location where the illegal definition occurs.
      */
    case class IllegalBoundedLattice(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Lattice definition must have exactly five components: bot, top, leq, lub and glb.")}
           |
            |${loc.underline}
           |the 1st component must be the bottom element,
           |the 2nd component must be the top element,
           |the 3rd component must be the partial order function,
           |the 4th component must be the least upper bound function, and
           |the 5th component must be the greatest upper bound function.
         """.stripMargin
    }

    /**
      * An error raised to indicate an illegal lattice attribute occurring in a relation.
      *
      * @param loc the location where the illegal definition occurs.
      */
    case class IllegalLatticeAttributeInRelation(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal lattice attribute in relation.")}
           |
            |${loc.underline}
           |A relation must not contain any lattice interpreted attributes.
           |
            |Tip: Remove the lattice interpretation (<>) or change the relation to a lattice (replace rel by lat).
         """.stripMargin
    }

    /**
      * An error raised to indicate that the last attribute of a lattice definition does not have a lattice interpretation.
      *
      * @param loc the location where the illegal definition occurs.
      */
    case class IllegalNonLatticeAttribute(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal non-lattice attribute.")}
           |
            |${loc.underline}
           |The last attribute of lattice must have a lattice interpretation.
           |
            |Tip: Change the interpretation of the last attribute (adding <>) or change the lattice to a relation (replace lat by rel).
         """.stripMargin
    }


    /**
      * An error raised to indicate that a lattice attribute is followed by a non-lattice attribute.
      *
      * @param loc1 the location where the first lattice attribute occurs.
      * @param loc2 the location where a following non-lattice attribute occurs.
      */
    case class IllegalMixedAttributes(loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal non-lattice attribute follows lattice attribute.")}
           |
           |The first lattice attribute was here:
           |${loc1.underline}
           |The illegal non-lattice attribute was here:
           |${loc2.underline}
           |
            |Tip: Rearrange the attributes or possibly their interpretations.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
      *
      * @param name the name of the variable.
      *
      * @param loc1 the location of the first use of the variable.
      * @param loc2 the location of the second use of the variable.
      */
    case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate definition of the same variable '$name' in pattern.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |
            |A variable is must only occurs once in a pattern.
           |
            |Tip: Remove the duplicate variable and use '==' to test for equality.
         """.stripMargin
    }

    /**
      * An error raised to indicate that a syntactic construct, although successfully parsed, is currently not supported.
      *
      * @param msg the error message.
      * @param loc the location of the syntactic construct.
      */
    case class Unsupported(msg: String, loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unsupported feature: $msg")}
           |
            |${loc.underline}
           |This feature is not yet supported, implemented or considered stable.
           |
            |Tip: Avoid using this feature.
         """.stripMargin
    }

  }

  /**
    * Compiles the given parsed `past` to a weeded ast.
    */
  def weed(past: ParsedAst.Root): Validation[WeededAst.Root, WeederError] = {
    val b = System.nanoTime()
    @@(past.declarations.map(Declaration.compile)) map {
      case decls =>
        val e = System.nanoTime()
        WeededAst.Root(decls, past.time.copy(weeder = e - b))
    }
  }

  object Declaration {

    /**
      * Compiles the given parsed declaration `past` to a weeded declaration.
      */
    def compile(past: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Declaration.Namespace => Declaration.compile(d)
      case d: ParsedAst.Declaration.Fact => Declaration.compile(d)
      case d: ParsedAst.Declaration.Rule => Declaration.compile(d)
      case d: ParsedAst.Definition => Definition.compile(d)
      case d: ParsedAst.Directive => Directive.compile(d)
    }

    /**
      * Compiles the given parsed namespace declaration `past` to a weeded namespace declaration.
      */
    def compile(past: ParsedAst.Declaration.Namespace): Validation[WeededAst.Declaration.Namespace, WeederError] =
      @@(past.body.map(compile)) map {
        case decls => WeededAst.Declaration.Namespace(past.name, decls, past.loc)
      }

    /**
      * Compiles the given parsed fact `past` to a weeded fact.
      */
    def compile(past: ParsedAst.Declaration.Fact): Validation[WeededAst.Declaration.Fact, WeederError] =
      Predicate.Head.compile(past.head) map {
        case p => WeededAst.Declaration.Fact(p, past.loc)
      }

    /**
      * Compiles the parsed rule `past` to a weeded rule.
      */
    def compile(past: ParsedAst.Declaration.Rule): Validation[WeededAst.Declaration.Rule, WeederError] = {
      // compute an map from variable names to alias predicates.
      val aliasesVal = Validation.fold[ParsedAst.Predicate.Alias, Map[String, ParsedAst.Predicate.Alias], WeederError](past.aliases, Map.empty) {
        case (m, p) => m.get(p.ident.name) match {
          case None => (m + (p.ident.name -> p)).toSuccess
          case Some(otherAlias) => DuplicateAlias(p.ident.name, otherAlias.loc, p.loc).toFailure
        }
      }

      aliasesVal flatMap {
        case aliases =>
          val headVal = Predicate.Head.compile(past.head, aliases)
          val bodyVal = @@(past.body.filterNot(_.isInstanceOf[ParsedAst.Predicate.Alias]).map(Predicate.Body.compile))

          @@(headVal, bodyVal) map {
            case (head, body) => WeededAst.Declaration.Rule(head, body, past.loc)
          }
      }
    }

  }

  object Definition {

    /**
      * Compiles the given parsed definition `past` to a weeded definition.
      */
    // TODO: Inline all calls into this function...
    def compile(past: ParsedAst.Definition): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Definition.Value => Definition.compile(d)
      case d: ParsedAst.Definition.Function => Definition.compile(d)
      case d: ParsedAst.Definition.Enum => Definition.compile(d)
      case d: ParsedAst.Definition.BoundedLattice => Definition.compile(d)
      case d: ParsedAst.Definition.Relation => Definition.compile(d)
      case d: ParsedAst.Definition.Lattice => Definition.compile(d)
      case d: ParsedAst.Definition.Index => Definition.compile(d)
    }

    /**
      * Compiles the given parsed value declaration `past` to a weeded definition.
      */
    def compile(past: ParsedAst.Definition.Value): Validation[WeededAst.Definition.Constant, WeederError] =
      @@(Expression.compile(past.e), Type.compile(past.tpe)) map {
        case (exp, tpe) => WeededAst.Definition.Constant(past.ident, exp, tpe, past.loc)
      }

    /**
      * Compiles the given parsed function declaration `past` to a weeded definition.
      */
    def compile(past: ParsedAst.Definition.Function): Validation[WeededAst.Definition.Constant, WeederError] = {
      // check duplicate formals.
      val seen = mutable.Map.empty[String, Name.Ident]
      val formalsVal = @@(past.formals.map {
        case ParsedAst.FormalArg(ident, tpe) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            Type.compile(tpe) map (t => WeededAst.FormalArg(ident, t))
          case Some(otherIdent) =>
            (DuplicateFormal(ident.name, otherIdent.loc, ident.loc): WeederError).toFailure
        }
      })

      @@(formalsVal, Expression.compile(past.body), Type.compile(past.tpe)) map {
        case (args, body, retType) =>
          val exp = WeededAst.Expression.Lambda(args, body, retType, past.body.loc)
          val tpe = WeededAst.Type.Function(args map (_.tpe), retType)
          WeededAst.Definition.Constant(past.ident, exp, tpe, past.loc)
      }
    }

    /**
      * Compiles the given parsed enum declaration `past` to a weeded enum definition.
      *
      * Returns [[Failure]] if the same tag name occurs twice.
      */
    def compile(past: ParsedAst.Definition.Enum): Validation[WeededAst.Definition.Enum, WeederError] = {
      // check duplicate tags.
      Validation.fold[ParsedAst.Type.Tag, Map[String, WeededAst.Type.Tag], WeederError](past.cases, Map.empty) {
        case (macc, tag@ParsedAst.Type.Tag(tagName, _)) => macc.get(tagName.name) match {
          case None => Type.compile(tag) map (tpe => macc + (tagName.name -> tpe.asInstanceOf[WeededAst.Type.Tag]))
          case Some(otherTag) => DuplicateTag(tagName.name, otherTag.tag.loc, tagName.loc).toFailure
        }
      } map {
        case m => WeededAst.Definition.Enum(past.ident, m, past.loc)
      }
    }

    /**
      * Compiles the given parsed lattice `past` to a weeded lattice definition.
      */
    def compile(past: ParsedAst.Definition.BoundedLattice): Validation[WeededAst.Definition.BoundedLattice, WeederError] = {
      // check lattice definition.
      val tpeVal = Type.compile(past.tpe)
      val elmsVal = @@(past.elms.toList.map(Expression.compile))
      @@(tpeVal, elmsVal) flatMap {
        case (tpe, List(bot, top, leq, lub, glb)) => WeededAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, past.loc).toSuccess
        case _ => IllegalBoundedLattice(past.loc).toFailure
      }
    }

    /**
      * Compiles the given parsed relation `past` to a weeded relation definition.
      */
    def compile(past: ParsedAst.Definition.Relation): Validation[WeededAst.Collection.Relation, WeederError] = {
      // check duplicate attributes.
      val seen = mutable.Map.empty[String, Name.Ident]
      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, interp) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            interp match {
              case i: ParsedAst.Interpretation.Set => Type.compile(interp.tpe) map (tpe => WeededAst.Attribute(ident, tpe, WeededAst.Interpretation.Set))
              case i: ParsedAst.Interpretation.Lattice =>
                (IllegalLatticeAttributeInRelation(ident.loc): WeederError).toFailure
            }
          case Some(otherIdent) =>
            (DuplicateAttribute(ident.name, otherIdent.loc, ident.loc): WeederError).toFailure
        }
      }

      @@(attributesVal) map {
        case attributes => WeededAst.Collection.Relation(past.ident, attributes, past.loc)
      }
    }

    /**
      * Compiles the given parsed relation `past` to a weeded lattice definition.
      */
    def compile(past: ParsedAst.Definition.Lattice): Validation[WeededAst.Collection.Lattice, WeederError] = {
      // TODO: Rewrite so we can get rid of WeededAst.Interpretation.

      // check duplicate attributes.
      val seen = mutable.Map.empty[String, Name.Ident]
      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, interp) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            interp match {
              case i: ParsedAst.Interpretation.Set => Type.compile(interp.tpe) map (tpe => WeededAst.Attribute(ident, tpe, WeededAst.Interpretation.Set))
              case i: ParsedAst.Interpretation.Lattice => Type.compile(interp.tpe) map (tpe => WeededAst.Attribute(ident, tpe, WeededAst.Interpretation.Lattice))
            }
          case Some(otherIdent) =>
            (DuplicateAttribute(ident.name, otherIdent.loc, ident.loc): WeederError).toFailure
        }
      }

      @@(attributesVal) flatMap {
        case attributes =>
          // the last attribute of a lattice definition must have a lattice interpretation.
          attributes.last.interp match {
            case WeededAst.Interpretation.Set =>
              (IllegalNonLatticeAttribute(attributes.last.ident.loc): WeederError).toFailure
            case WeededAst.Interpretation.Lattice =>

              val index = attributes.indexWhere(_.interp == WeededAst.Interpretation.Lattice)
              val (keys, values) = attributes.splitAt(index)

              // ensure that no non-lattice attribute occurs after `index`.
              values.find(_.interp == WeededAst.Interpretation.Set) match {
                case None => WeededAst.Collection.Lattice(past.ident, keys, values, past.loc).toSuccess
                case Some(attr) => IllegalMixedAttributes(values.head.ident.loc, attr.ident.loc).toFailure
              }
          }
      }
    }

    /**
      * Compiles the given parsed index definition `past` to a weeded index definition.
      */
    def compile(past: ParsedAst.Definition.Index): Validation[WeededAst.Definition.Index, WeederError] = {
      // TODO: Check duplicated attributes.
      WeededAst.Definition.Index(past.ident, past.indexes, past.loc).toSuccess
    }

  }

  object Directive {

    /**
      * Compiles the given parsed directive `past` to a weeded directive.
      */
    def compile(past: ParsedAst.Directive): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Directive.AssertFact => Declaration.compile(d.fact) map {
        case fact => WeededAst.Directive.AssertFact(fact, d.loc)
      }
      case d: ParsedAst.Directive.AssertRule => Declaration.compile(d.rule) map {
        case rule => WeededAst.Directive.AssertRule(rule, d.loc)
      }
      case d: ParsedAst.Directive.Print => WeededAst.Directive.Print(d.name, d.loc).toSuccess
    }

  }

  object Literal {
    /**
      * Compiles the parsed literal `past` to a weeded literal.
      */
    def compile(past: ParsedAst.Literal): Validation[WeededAst.Literal, WeederError] = past match {
      case plit: ParsedAst.Literal.Unit => WeededAst.Literal.Unit(plit.loc).toSuccess
      case plit: ParsedAst.Literal.Bool => plit.lit match {
        case "true" => WeededAst.Literal.Bool(lit = true, plit.loc).toSuccess
        case "false" => WeededAst.Literal.Bool(lit = false, plit.loc).toSuccess
        case _ => throw Compiler.InternalCompilerError("Impossible non-boolean value.")
      }
      case plit: ParsedAst.Literal.Int => WeededAst.Literal.Int(plit.lit.toInt, plit.loc).toSuccess
      case plit: ParsedAst.Literal.Str => WeededAst.Literal.Str(plit.lit, plit.loc).toSuccess
      case plit: ParsedAst.Literal.Tag => compile(plit.lit) map (lit => WeededAst.Literal.Tag(plit.enum, plit.tag, lit, plit.loc))
      case plit: ParsedAst.Literal.Tuple => @@(plit.elms map compile) map {
        case elms => WeededAst.Literal.Tuple(elms, plit.loc)
      }
      case plit: ParsedAst.Literal.Set => @@(plit.elms map compile) map {
        case elms => WeededAst.Literal.Set(elms, plit.loc)
      }
    }
  }

  object Expression {
    /**
      * Compiles the parsed expression `past` to a weeded expression.
      */
    def compile(past: ParsedAst.Expression): Validation[WeededAst.Expression, WeederError] = past match {
      case exp: ParsedAst.Expression.Lit =>
        Literal.compile(exp.lit) map {
          case lit => WeededAst.Expression.Lit(lit, exp.loc)
        }

      case exp: ParsedAst.Expression.Var =>
        WeededAst.Expression.Var(exp.name, exp.loc).toSuccess

      case exp: ParsedAst.Expression.Apply =>
        @@(compile(exp.lambda), @@(exp.actuals map compile)) map {
          case (lambda, args) => WeededAst.Expression.Apply(lambda, args, exp.loc)
        }

      case exp: ParsedAst.Expression.Lambda =>
        val argsVal = @@(exp.formals map {
          case ParsedAst.FormalArg(ident, tpe) => Type.compile(tpe) map (t => WeededAst.FormalArg(ident, t))
        })
        @@(argsVal, Type.compile(exp.tpe), compile(exp.body)) map {
          case (args, tpe, body) => WeededAst.Expression.Lambda(args, body, tpe, exp.loc)
        }

      case exp: ParsedAst.Expression.Unary =>
        compile(exp.e) map {
          case e => WeededAst.Expression.Unary(exp.op, e, exp.loc)
        }

      case exp: ParsedAst.Expression.Binary =>
        @@(compile(exp.e1), compile(exp.e2)) map {
          case (e1, e2) => WeededAst.Expression.Binary(exp.op, e1, e2, exp.loc)
        }

      case exp: ParsedAst.Expression.IfThenElse =>
        @@(compile(exp.e1), compile(exp.e2), compile(exp.e3)) map {
          case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, exp.loc)
        }

      case exp: ParsedAst.Expression.Let =>
        @@(compile(exp.value), compile(exp.body)) map {
          case (value, body) => WeededAst.Expression.Let(exp.ident, value, body, exp.loc)
        }

      case exp: ParsedAst.Expression.Match =>
        val rulesVal = exp.rules map {
          case (pat, body) => @@(Pattern.compile(pat), compile(body))
        }
        @@(compile(exp.e), @@(rulesVal)) map {
          case (e, rs) => WeededAst.Expression.Match(e, rs, exp.loc)
        }

      case exp: ParsedAst.Expression.Infix =>
        @@(compile(exp.e1), compile(exp.e2)) map {
          case (e1, e2) => exp.name.parts match {
            case List("in") => WeededAst.Expression.Binary(BinaryOperator.Set.Member, e1, e2, exp.loc)
            case List("subsetOf") => WeededAst.Expression.Binary(BinaryOperator.Set.SubsetOf, e1, e2, exp.loc)
            case List("properSubsetOf") => WeededAst.Expression.Binary(BinaryOperator.Set.ProperSubsetOf, e1, e2, exp.loc)
            case List("insert") => WeededAst.Expression.Binary(BinaryOperator.Set.Insert, e1, e2, exp.loc)
            case List("remove") => WeededAst.Expression.Binary(BinaryOperator.Set.Remove, e1, e2, exp.loc)
            case List("union") => WeededAst.Expression.Binary(BinaryOperator.Set.Union, e1, e2, exp.loc)
            case List("intersect") => WeededAst.Expression.Binary(BinaryOperator.Set.Intersection, e1, e2, exp.loc)
            case List("diff") => WeededAst.Expression.Binary(BinaryOperator.Set.Difference, e1, e2, exp.loc)
            case _ => WeededAst.Expression.Apply(WeededAst.Expression.Var(exp.name, exp.loc), List(e1, e2), exp.loc)
          }
        }

      case exp: ParsedAst.Expression.Tag => compile(exp.e) map {
        case e => WeededAst.Expression.Tag(exp.enumName, exp.tagName, e, exp.loc)
      }

      case exp: ParsedAst.Expression.Tuple =>
        @@(exp.elms map compile) map {
          case elms => WeededAst.Expression.Tuple(elms, exp.loc)
        }

      case exp: ParsedAst.Expression.Set =>
        @@(exp.elms map compile) map {
          case elms => WeededAst.Expression.Set(elms, exp.loc)
        }

      case exp: ParsedAst.Expression.Ascribe =>
        @@(compile(exp.e), Type.compile(exp.tpe)) map {
          case (e, tpe) => WeededAst.Expression.Ascribe(e, tpe, exp.loc)
        }

      case exp: ParsedAst.Expression.Error =>
        Type.compile(exp.tpe) map {
          case tpe => WeededAst.Expression.Error(tpe, exp.loc)
        }

      case exp: ParsedAst.Expression.Native =>
        val parts = exp.name.split('.')
        val className = parts.dropRight(1).mkString(".")
        val memberName = parts.last
        WeededAst.Expression.Native(className, memberName, exp.loc).toSuccess
    }
  }

  object Pattern {
    /**
      * Compiles the parsed pattern `past`.
      */
    def compile(past: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
      // check non-linear pattern, i.e. duplicate variable occurrence.
      val seen = mutable.Map.empty[String, Name.Ident]
      def visit(p: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = p match {
        case pat: ParsedAst.Pattern.Wildcard => WeededAst.Pattern.Wildcard(pat.loc).toSuccess
        case pat: ParsedAst.Pattern.Var => seen.get(pat.ident.name) match {
          case None =>
            seen += (pat.ident.name -> pat.ident)
            WeededAst.Pattern.Var(pat.ident, pat.loc).toSuccess
          case Some(otherIdent) =>
            NonLinearPattern(pat.ident.name, otherIdent.loc, pat.ident.loc).toFailure
        }
        case pat: ParsedAst.Pattern.Lit => Literal.compile(pat.lit) map {
          case lit => WeededAst.Pattern.Lit(lit, pat.loc)
        }
        case ppat: ParsedAst.Pattern.Tag => visit(ppat.p) map {
          case pat => WeededAst.Pattern.Tag(ppat.enumName, ppat.tagName, pat, ppat.loc)
        }
        case pat: ParsedAst.Pattern.Tuple => @@(pat.elms map visit) map {
          case elms => WeededAst.Pattern.Tuple(elms, pat.loc)
        }
      }

      visit(past)
    }
  }


  object Predicate {

    object Head {

      /**
        * Compiles the given parsed predicate `p` to a weeded head predicate.
        */
      def compile(past: ParsedAst.Predicate, aliases: Map[String, ParsedAst.Predicate.Alias] = Map.empty): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case p: ParsedAst.Predicate.FunctionOrRelation =>
          @@(p.terms.map(t => Term.Head.compile(t, aliases))) map {
            case terms => WeededAst.Predicate.Head.Relation(p.name, terms, p.loc)
          }

        case p: ParsedAst.Predicate.Trace =>
          @@(p.terms.map(t => Term.Head.compile(t, aliases))) map {
            case terms => WeededAst.Predicate.Head.Trace(terms, p.loc)
          }

        case p: ParsedAst.Predicate.Write =>
          @@(p.terms.map(t => Term.Head.compile(t, aliases))) flatMap {
            case terms if terms.size < 2 => IllegalWritePredicate(p.loc).toFailure
            case terms => WeededAst.Predicate.Head.Write(terms.dropRight(1), terms.last, p.loc).toSuccess
          }

        case p: ParsedAst.Predicate.Error =>
          @@(p.terms map (t => Term.Head.compile(t, aliases))) map {
            case terms => WeededAst.Predicate.Head.Error(terms, p.loc)
          }

        case p: ParsedAst.Predicate.Alias => IllegalHeadPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.Loop => IllegalHeadPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.Read => IllegalHeadPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.NotEqual => IllegalHeadPredicate(p.loc).toFailure
      }

    }

    object Body {

      /**
        * Compiles the given parsed predicate `p` to a weeded body predicate.
        */
      def compile(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case p: ParsedAst.Predicate.FunctionOrRelation =>
          @@(p.terms.map(Term.Body.compile)) map {
            case terms => WeededAst.Predicate.Body.FunctionOrRelation(p.name, terms, past.loc)
          }

        case p: ParsedAst.Predicate.NotEqual =>
          WeededAst.Predicate.Body.NotEqual(p.ident1, p.ident2, p.loc).toSuccess

        case p: ParsedAst.Predicate.Loop => Term.Head.compile(p.term, Map.empty) map {
          case term => WeededAst.Predicate.Body.Loop(p.ident, term, p.loc)
        }

        case p: ParsedAst.Predicate.Read =>
          @@(p.terms.map(t => Term.Body.compile(t))) flatMap {
            case terms if terms.size < 2 => IllegalReadPredicate(p.loc).toFailure
            case terms => WeededAst.Predicate.Body.Read(terms.dropRight(1), terms.last, p.loc).toSuccess
          }

        case p: ParsedAst.Predicate.Trace => IllegalBodyPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.Write => IllegalBodyPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.Error => IllegalBodyPredicate(p.loc).toFailure
        case p: ParsedAst.Predicate.Alias => throw Compiler.InternalCompilerError("Alias predicate should already have been eliminated.")
      }
    }

  }

  object Term {

    object Head {

      /**
        * Compiles the given parsed head term `past` to a weeded term.
        */
      def compile(past: ParsedAst.Term, aliases: Map[String, ParsedAst.Predicate.Alias]): Validation[WeededAst.Term.Head, WeederError] = past match {
        case term: ParsedAst.Term.Wildcard => IllegalHeadTerm("Wildcards may not occur in head predicates.", term.loc).toFailure
        case term: ParsedAst.Term.Var => aliases.get(term.ident.name) match {
          case None => WeededAst.Term.Head.Var(term.ident, term.loc).toSuccess
          case Some(alias) => compile(alias.term, aliases)
        }
        case term: ParsedAst.Term.Lit => Literal.compile(term.lit) map {
          case lit => WeededAst.Term.Head.Lit(lit, term.loc)
        }
        case term: ParsedAst.Term.Ascribe =>
          @@(compile(term.term, aliases), Type.compile(term.tpe)) map {
            case (t, tpe) => WeededAst.Term.Head.Ascribe(t, tpe, term.loc)
          }
        case term: ParsedAst.Term.Apply =>
          @@(term.args map (t => compile(t, aliases))) map {
            case args => WeededAst.Term.Head.Apply(term.name, args, term.loc)
          }
        case term: ParsedAst.Term.Infix =>
          @@(compile(term.t1, aliases), compile(term.t2, aliases)) map {
            case (t1, t2) => WeededAst.Term.Head.Apply(term.name, List(t1, t2), term.loc)
          }
        case term: ParsedAst.Term.Native =>
          val parts = term.name.split('.')
          val className = parts.dropRight(1).mkString(".")
          val memberName = parts.last
          WeededAst.Term.Head.Native(className, memberName, term.loc).toSuccess
      }
    }

    object Body {
      /**
        * Compiles the given parsed body term `past` to a weeded term.
        */
      def compile(past: ParsedAst.Term): Validation[WeededAst.Term.Body, WeederError] = past match {
        case term: ParsedAst.Term.Wildcard => WeededAst.Term.Body.Wildcard(term.loc).toSuccess
        case term: ParsedAst.Term.Var => WeededAst.Term.Body.Var(term.ident, term.loc).toSuccess
        case term: ParsedAst.Term.Lit => Literal.compile(term.lit) map {
          case lit => WeededAst.Term.Body.Lit(lit, term.loc)
        }
        case term: ParsedAst.Term.Ascribe =>
          @@(compile(term.term), Type.compile(term.tpe)) map {
            case (t, tpe) => WeededAst.Term.Body.Ascribe(t, tpe, term.loc)
          }
        case term: ParsedAst.Term.Apply => IllegalBodyTerm("Function calls may not occur in body predicates.", term.loc).toFailure
        case term: ParsedAst.Term.Infix => IllegalBodyTerm("Function calls may not occur in body predicates.", term.loc).toFailure
        case term: ParsedAst.Term.Native => IllegalBodyTerm("Native fields/calls may not occur in body predicates.", term.loc).toFailure // TODO: Allow?
      }
    }

  }

  object Type {
    /**
      * Weeds the given parsed type `past`.
      */
    def compile(past: ParsedAst.Type): Validation[WeededAst.Type, WeederError] = past match {
      case ParsedAst.Type.Unit => WeededAst.Type.Unit.toSuccess
      case ParsedAst.Type.Named(name) => WeededAst.Type.Named(name).toSuccess
      case ParsedAst.Type.Function(pformals, pret) =>
        val formalTypeVal = @@(pformals map compile)
        val returnTypeVal = compile(pret)
        @@(formalTypeVal, returnTypeVal) map {
          case (formals, retTpe) => WeededAst.Type.Function(formals, retTpe)
        }
      case ParsedAst.Type.Tag(ident, ptype) => compile(ptype) map {
        case tpe => WeededAst.Type.Tag(ident, tpe)
      }
      case ParsedAst.Type.Tuple(pelms) => @@(pelms map compile) map {
        case elms => WeededAst.Type.Tuple(elms)
      }
      case ParsedAst.Type.Parametric(Name.Unresolved(_, List("Set"), _), Seq(tpe)) =>
        compile(tpe) map WeededAst.Type.Set
      case ParsedAst.Type.Parametric(name, pelms) =>
        Unsupported("Parametric types are not yet supported.", name.loc).toFailure
      case p@ParsedAst.Type.Native(sp1, name, sp2) =>
        WeededAst.Type.Native(name, p.loc).toSuccess
    }
  }


}
