package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.{WeededAst, SourceLocation, ParsedAst, Name}
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
     * An error raised to indicate that the attribute `name` was declared multiple times.
     *
     * @param name the name of the attribute.
     * @param loc1 the location of the first declaration.
     * @param loc2 the location of the second declaration.
     */
    case class DuplicateAttribute(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.formatSource}")}
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
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.formatSource}")}
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
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.formatSource}")}
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
     * An error raised to indicate that a wildcard occurs in a head term.
     *
     * @param loc the location where the illegal term occurs.
     */
    case class WildcardInHeadTerm(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.formatSource}")}
           |
            |${consoleCtx.red(s">> Illegal wildcard in head of a fact/rule.")}
           |
            |${loc.underline}
           |Wildcards (i.e. implicitly unbound variables) are not allowed to occur in the head of a fact/rule.
           |
            |Tip: Remove the wildcard or replace it by bound variable.
         """.stripMargin
    }

    /**
     * An error raised to indicate that a function application occurs in a body term.
     *
     * @param loc the location where the illegal term occurs.
     */
    case class ApplyInBodyTerm(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.formatSource}")}
           |
            |${consoleCtx.red(s">> Illegal function call in body of a rule.")}
           |
            |${loc.underline}
           |Function calls are not allowed to occur in the body of a rule. Only in its head.
           |
            |Tip: Restructure the rule such that the function call does not occur in its body.
           |Possibly by breaking up the rule into multiple smaller rules.
         """.stripMargin
    }

    /**
     * An error raised to indicate an illegal lattice definition.
     *
     * @param loc the location where the illegal definition occurs.
     */
    case class IllegalLattice(loc: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc.formatSource}")}
           |
            |${consoleCtx.red(s">> Lattice definition must have exactly three components: bot, leq and lub.")}
           |
            |${loc.underline}
           |the first component should be the bottom element,
           |the second component should be the partial order function,
           |and the third component should be the least upper bound function.
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
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${loc1.formatSource}")}
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
     * @param message the error message.
     * @param location the location of the syntactic construct.
     */
    case class Unsupported(message: String, location: SourceLocation) extends WeederError {
      val format =
        s"""${consoleCtx.blue(s"-- SYNTAX ERROR -------------------------------------------------- ${location.formatSource}")}
           |
            |${consoleCtx.red(s">> Unsupported feature: $message")}
           |
            |${location.underline}
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
    @@(past.declarations.map(Declaration.compile)) map WeededAst.Root
  }

  object Declaration {

    /**
     * Compiles the given parsed declaration `past` to a weeded declaration.
     */
    def compile(past: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Declaration.Namespace => compile(d)
      case d: ParsedAst.Declaration.Fact => compile(d)
      case d: ParsedAst.Declaration.Rule => compile(d)
      case d: ParsedAst.Definition => Definition.compile(d)
    }

    /**
     * Compiles the given parsed namespace declaration `past` to a weeded namespace declaration.
     */
    def compile(past: ParsedAst.Declaration.Namespace): Validation[WeededAst.Declaration.Namespace, WeederError] =
      @@(past.body.map(compile)) map (ds => WeededAst.Declaration.Namespace(past.name, ds))

    /**
     * Compiles the given parsed fact `past` to a weeded fact.
     */
    def compile(past: ParsedAst.Declaration.Fact): Validation[WeededAst.Declaration.Fact, WeederError] =
      Predicate.Head.compile(past.head) map {
        case p => WeededAst.Declaration.Fact(p)
      }

    /**
     * Compiles the parsed rule `past` to a weeded rule.
     */
    def compile(past: ParsedAst.Declaration.Rule): Validation[WeededAst.Declaration.Rule, WeederError] = {
      val headVal = Predicate.Head.compile(past.head)
      val bodyVal = @@(past.body.map(Predicate.Body.compile))

      @@(headVal, bodyVal) map {
        case (head, body) => WeededAst.Declaration.Rule(head, body)
      }
    }

  }

  object Definition {

    /**
     * Compiles the given parsed definition `past` to a weeded definition.
     */
    def compile(past: ParsedAst.Definition): Validation[WeededAst.Declaration, WeederError] = past match {
      case d: ParsedAst.Definition.Value => compile(d)
      case d: ParsedAst.Definition.Function => compile(d)
      case d: ParsedAst.Definition.Enum => compile(d)
      case d: ParsedAst.Definition.Lattice => compile(d)
      case d: ParsedAst.Definition.Relation => compile(d)
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
    def compile(past: ParsedAst.Definition.Enum): Validation[WeededAst.Definition.Enum, WeederError] =
      Validation.fold[ParsedAst.Type.Tag, Map[String, WeededAst.Type.Tag], WeederError](past.cases, Map.empty) {
        // loop through each tag declaration.
        case (macc, tag@ParsedAst.Type.Tag(tagName, _)) => macc.get(tagName.name) match {
          // check if the tag was already declared.
          case None => Type.compile(tag) map (tpe => macc + (tagName.name -> tpe.asInstanceOf[WeededAst.Type.Tag]))
          case Some(otherTag) => DuplicateTag(tagName.name, otherTag.tagName.loc, tagName.loc).toFailure
        }
      } map {
        case m => WeededAst.Definition.Enum(past.ident, m, past.loc)
      }

    /**
     * Compiles the given parsed lattice `past` to a weeded lattice definition.
     */
    def compile(past: ParsedAst.Definition.Lattice): Validation[WeededAst.Definition.Lattice, WeederError] = {
      val tpeVal = Type.compile(past.tpe)
      val elmsVal = @@(past.elms.toList.map(Expression.compile))
      @@(tpeVal, elmsVal) flatMap {
        case (tpe, bot :: leq :: lub :: Nil) => WeededAst.Definition.Lattice(tpe, bot, leq, lub, past.loc).toSuccess
        case _ => IllegalLattice(past.loc).toFailure
      }
    }

    /**
     * Compiles the given parsed relation `past` to a weeded relation definition.
     */
    def compile(past: ParsedAst.Definition.Relation): Validation[WeededAst.Definition.Relation, WeederError] = {
      val seen = mutable.Map.empty[String, Name.Ident]

      val attributesVal = past.attributes.map {
        case ParsedAst.Attribute(ident, interp) => seen.get(ident.name) match {
          // check if the attribute name was already declared.
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

      @@(attributesVal) map {
        case attributes => WeededAst.Definition.Relation(past.ident, attributes, past.loc)
      }
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
          case (e1, e2) => WeededAst.Expression.Apply(WeededAst.Expression.Var(exp.name, exp.loc), List(e1, e2), exp.loc)
        }

      case exp: ParsedAst.Expression.Tag => compile(exp.e) map {
        case e => WeededAst.Expression.Tag(exp.enumName, exp.tagName, e, exp.loc)
      }

      case exp: ParsedAst.Expression.Tuple =>
        @@(exp.elms map compile) map {
          case elms => WeededAst.Expression.Tuple(elms, exp.loc)
        }

      case exp: ParsedAst.Expression.Ascribe =>
        @@(compile(exp.e), Type.compile(exp.tpe)) map {
          case (e, tpe) => WeededAst.Expression.Ascribe(e, tpe, exp.loc)
        }

      case exp: ParsedAst.Expression.Error =>
        Type.compile(exp.tpe) map {
          case tpe => WeededAst.Expression.Error(tpe, exp.loc)
        }
    }
  }

  object Pattern {
    /**
     * Compiles the parsed pattern `past`.
     *
     * Returns [[Failure]] if the pattern is non-linear, i.e. if the same variable occurs twice.
     */
    def compile(past: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
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
       * Compiles the given parsed predicate `p` to a weeded predicate.
       */
      def compile(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case p: ParsedAst.Predicate.Unresolved => compile(p)
        case p: ParsedAst.Predicate.Alias => ???
      }

      /**
       * Compiles the given parsed predicate `past` to a weeded predicate.
       */
      def compile(past: ParsedAst.Predicate.Unresolved): Validation[WeededAst.Predicate.Head, WeederError] =
        @@(past.terms.map(Term.Head.compile)) map {
          case terms => WeededAst.Predicate.Head(past.name, terms, past.loc)
        }
    }

    object Body {

      /**
       * Compiles the given parsed predicate `p` to a weeded predicate.
       */
      def compile(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case p: ParsedAst.Predicate.Unresolved => compile(p)
        case p: ParsedAst.Predicate.Alias => ???
      }

      /**
       * Compiles the given parsed predicate `p` to a weeded predicate.
       */
      def compile(past: ParsedAst.Predicate.Unresolved): Validation[WeededAst.Predicate.Body, WeederError] =
        @@(past.terms.map(Term.Body.compile)) map {
          case terms => WeededAst.Predicate.Body(past.name, terms, past.loc)
        }
    }

  }

  object Term {

    object Head {

      /**
       * Compiles the given parsed head term `past` to a weeded term.
       *
       * Returns [[Failure]] if the term contains a wildcard variable.
       */
      def compile(past: ParsedAst.Term): Validation[WeededAst.Term.Head, WeederError] = past match {
        case term: ParsedAst.Term.Wildcard => WildcardInHeadTerm(term.loc).toFailure
        case term: ParsedAst.Term.Var => WeededAst.Term.Head.Var(term.ident, term.loc).toSuccess
        case term: ParsedAst.Term.Lit => Literal.compile(term.lit) map {
          case lit => WeededAst.Term.Head.Lit(lit, term.loc)
        }
        case term: ParsedAst.Term.Ascribe =>
          @@(compile(term.term), Type.compile(term.tpe)) map {
            case (t, tpe) => WeededAst.Term.Head.Ascribe(t, tpe, term.loc)
          }
        case term: ParsedAst.Term.Apply =>
          @@(term.args map compile) map {
            case args => WeededAst.Term.Head.Apply(term.name, args, term.loc)
          }
      }
    }

    object Body {
      /**
       * Compiles the given parsed body term `past` to a weeded term.
       *
       * Returns [[Failure]] if the term contains a function call.
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
        case term: ParsedAst.Term.Apply => ApplyInBodyTerm(term.loc).toFailure
      }
    }

  }

  object Type {
    /**
     * Weeds the given parsed type `past`.
     */
    def compile(past: ParsedAst.Type): Validation[WeededAst.Type, WeederError] = past match {
      case ParsedAst.Type.Unit => WeededAst.Type.Unit.toSuccess
      case ParsedAst.Type.Var(name) => WeededAst.Type.Ambiguous(name).toSuccess
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
      case ParsedAst.Type.Parametric(name, pelms) =>
        Unsupported("Parametric types are not yet supported.", name.loc).toFailure
    }
  }


}
