package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{WeededAst, SourceLocation, ParsedAst}

import util.Validation
import util.Validation._

import scala.collection.mutable

/**
 * The Weeder phase is responsible for:
 *
 * (1) reporting basic syntactic problems, and
 * (2) performing basic syntactic rewritings.
 */

// TODO: JoinSemiLattice vs. CompleteLattice.
// TODO: valid "Traits"
// TODO: rewrite all functions to lambdas of one argument?
// TODO: The weeder could be responsible for dealing with Single tuple expressions and literal/expression conversion.
object Weeder {

  import WeederError._

  /**
   * A common super-type for weeding errors.
   */
  sealed trait WeederError {
    /**
     * Returns human readable error message.
     */
    def format: String
  }

  object WeederError {

    /**
     * An error raised to indicate that the tag `name` was declared multiple times.
     *
     * @param name the name of the tag.
     * @param location1 the location of the first declaration.
     * @param location2 the location of the second declaration.
     */
    case class DuplicateTag(name: String, location1: SourceLocation, location2: SourceLocation) extends WeederError {
      val format =
        s"Error: Duplicate tag name '$name'.\n" +
          s"  First declaration was here: ${location1.format}. This one will be used.\n" +
          s"  Second declaration was here: ${location2.format}. This one will be ignored.\n"
    }

    /**
     * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
     *
     * @param name the name of the variable.
     *
     * @param location1 the location of the first use of the variable.
     * @param location2 the location of the second use of the variable.
     */
    case class NonLinearPattern(name: String, location1: SourceLocation, location2: SourceLocation) extends WeederError {
      val format =
        s"Error: Non-linear pattern: The variable '$name' occurs twice.\n" +
          s"  First occurrence was here: ${location1.format}\n" +
          s"  Second occurrence was here: ${location2.format}\n"
    }

    case class DuplicatedFormalArgument()

    case class DuplicatedAttributeInRelation()

    /**
     * An error raised to indicate that an apply occurs in the body of a rule.
     *
     * @param location the location where the apply occurs.
     */
    case class ApplyNotAllowInBody(location: SourceLocation) extends WeederError {
      val format =
        s"Error: Function calls are not allowed in a term appearing in the body of a rule.\n" +
          s"  Call was here: ${location.format}\n"
    }

  }

  /**
   * Compiles the given parsed `ast` to a weeded ast.
   */
  def weed(ast: ParsedAst.Root): Validation[WeededAst.Root, WeederError] = {
    @@(ast.declarations.map(compileDeclaration)) map WeededAst.Root
  }


  /**
   * Compiles the given parsed declaration `d` to a weeded declaration.
   */
  def compileDeclaration(d: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = d match {
    case d: ParsedAst.Declaration.Namespace => compileNamespace(d)
    case d: ParsedAst.Declaration.Enum => compileEnum(d)
    case d: ParsedAst.Declaration.Fact => compileFact(d)
    case d: ParsedAst.Declaration.Rule => compileRule(d)
  }

  /**
   * Compiles the given parsed namespace `d` to a weeded namespace.
   */
  def compileNamespace(d: ParsedAst.Declaration.Namespace): Validation[WeededAst.Declaration.Namespace, WeederError] =
    @@(d.body.map(compileDeclaration)) map (ds => WeededAst.Declaration.Namespace(d.name, ds))

  /**
   * Compiles the given parsed enum `d` to a weeded enum.
   *
   * Fails if the same tag name is occurs twice.
   */
  def compileEnum(d: ParsedAst.Declaration.Enum): Validation[WeededAst.Declaration.Enum, WeederError] =
    Validation.fold[ParsedAst.Type.Tag, Map[String, ParsedAst.Type.Tag], WeederError](d.cases, Map.empty) {
      // loop through each tag declaration
      case (macc, tag@ParsedAst.Type.Tag(ParsedAst.Ident(name, location2), _)) => macc.get(name) match {
        // check if the tag was already declared
        case None => (macc + (name -> tag)).toSuccess
        case Some(otherTag) => DuplicateTag(name, otherTag.ident.location, location2).toFailure
      }
    } map {
      case m => WeededAst.Declaration.Enum(d.ident, m)
    }

  /**
   * Compiles the parsed fact `d` to a weeded fact.
   */
  def compileFact(d: ParsedAst.Declaration.Fact): Validation[WeededAst.Declaration.Fact, WeederError] =
    compilePredicateWithApply(d.head) map {
      case p => WeededAst.Declaration.Fact(p)
    }

  /**
   * Compiles the parsed rule `d` to a weeded rule.
   */
  def compileRule(d: ParsedAst.Declaration.Rule): Validation[WeededAst.Declaration.Rule, WeederError] = {
    val headVal = compilePredicateWithApply(d.head)
    val bodyVal = @@(d.body.map(compilePredicateNoApply))

    @@(headVal, bodyVal) map {
      case (head, body) => WeededAst.Declaration.Rule(head, body)
    }
  }

  /**
   * Compiles the parsed literal `l` to a weeded literal.
   */
  def compileLiteral(l: ParsedAst.Literal): Validation[WeededAst.Literal, WeederError] = l match {
    case ParsedAst.Literal.Unit => WeededAst.Literal.Unit.toSuccess
    case ParsedAst.Literal.Bool(b) => WeededAst.Literal.Bool(b).toSuccess
    case ParsedAst.Literal.Int(i) => WeededAst.Literal.Int(i).toSuccess
    case ParsedAst.Literal.Str(s) => WeededAst.Literal.Str(s).toSuccess
    case ParsedAst.Literal.Tag(name, ident, literal) => compileLiteral(literal) map (l => WeededAst.Literal.Tag(name, ident, l))
    case ParsedAst.Literal.Tuple(elms) => @@(elms map compileLiteral) map WeededAst.Literal.Tuple
  }

  /**
   * Compiles the parsed expression `e` to a weeded expression.
   */
  def compileExpression(e: ParsedAst.Expression): Validation[WeededAst.Expression, WeederError] = e match {

    case ParsedAst.Expression.Tuple(elms) => @@(elms map compileExpression) map {
      case welms => WeededAst.Expression.Tuple(welms)
    }
    case ParsedAst.Expression.Ascribe(e1, tpe) =>
      @@(compileExpression(e), compileType(tpe)) map {
        case (we1, wtpe) => WeededAst.Expression.Ascribe(we1, wtpe)
      }
    case ParsedAst.Expression.Error(location) => WeededAst.Expression.Error(location).toSuccess
  }

  /**
   * Compiles the parsed pattern `p` to a weeded pattern.
   *
   * Fails if the pattern is non-linear, i.e. if the same variable occurs twice.
   */
  def compilePattern(p: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Ident]

    def visit(p: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = p match {
      case ParsedAst.Pattern.Wildcard(location) => WeededAst.Pattern.Wildcard(location).toSuccess
      case ParsedAst.Pattern.Var(ident@ParsedAst.Ident(name, location)) => seen.get(name) match {
        case None =>
          seen += (name -> ident)
          WeededAst.Pattern.Var(ident).toSuccess
        case Some(otherIdent) => NonLinearPattern(name, otherIdent.location, location).toFailure
      }
      case ParsedAst.Pattern.Lit(literal) => compileLiteral(literal) map WeededAst.Pattern.Lit
      case ParsedAst.Pattern.Tag(name, ident, p2) => visit(p2) map {
        case wp => WeededAst.Pattern.Tag(name, ident, wp)
      }
      case ParsedAst.Pattern.Tuple(elms) => @@(elms map visit) map {
        case welms => WeededAst.Pattern.Tuple(welms)
      }
    }

    visit(p)
  }

  /**
   * Compiles the given parsed predicate `p` to a weeded predicate.
   *
   * Fails if the parsed predicate contains a function call.
   */
  def compilePredicateNoApply(p: ParsedAst.AmbiguousPredicate): Validation[WeededAst.PredicateNoApply, WeederError] =
    @@(p.terms.map(compileTermNoApply)) map {
      case wterms => WeededAst.PredicateNoApply(p.name, wterms)
    }

  /**
   * Compiles the given parsed predicate `p` to a weeded predicate.
   */
  def compilePredicateWithApply(p: ParsedAst.AmbiguousPredicate): Validation[WeededAst.PredicateWithApply, WeederError] =
    @@(p.terms.map(compileTermWithApply)) map {
      case wterms => WeededAst.PredicateWithApply(p.name, wterms)
    }

  /**
   * Compiles the given parsed term `t` to a weeded term.
   *
   * Fails if the parsed term contains a function call.
   */
  def compileTermNoApply(t: ParsedAst.Term): Validation[WeededAst.TermNoApply, WeederError] = t match {
    case ParsedAst.Term.Wildcard(location) => WeededAst.TermNoApply.Wildcard(location).toSuccess
    case ParsedAst.Term.Var(ident) => WeededAst.TermNoApply.Var(ident).toSuccess
    case ParsedAst.Term.Lit(literal) => compileLiteral(literal) map WeededAst.TermNoApply.Lit
    case ParsedAst.Term.Apply(name, args) => ApplyNotAllowInBody(name.location).toFailure
  }

  /**
   * Compiles the given parsed term `t` to a weeded term.
   */
  def compileTermWithApply(t: ParsedAst.Term): Validation[WeededAst.TermWithApply, WeederError] = t match {
    case ParsedAst.Term.Wildcard(location) => WeededAst.TermWithApply.Wildcard(location).toSuccess
    case ParsedAst.Term.Var(ident) => WeededAst.TermWithApply.Var(ident).toSuccess
    case ParsedAst.Term.Lit(literal) => compileLiteral(literal) map WeededAst.TermWithApply.Lit
    case ParsedAst.Term.Apply(name, args) => @@(args map compileTermWithApply) map {
      case wargs => WeededAst.TermWithApply.Apply(name, wargs)
    }
  }

  /**
   * Compiles the given parsed type `t` to a weeded type.
   */
  def compileType(t: ParsedAst.Type): Validation[WeededAst.Type, WeederError] = t match {
    case ParsedAst.Type.Unit => WeededAst.Type.Unit.toSuccess
    case ParsedAst.Type.Ambiguous(name) => WeededAst.Type.Ambiguous(name).toSuccess
    case ParsedAst.Type.Function(t1, t2) =>
      @@(compileType(t1), compileType(t2)) map {
        case (tpe1, tpe2) => WeededAst.Type.Function(tpe1, tpe2)
      }
    case ParsedAst.Type.Tag(ident, tpe) => compileType(tpe) map {
      case t1 => WeededAst.Type.Tag(ident, t1)
    }
    case ParsedAst.Type.Tuple(elms) => @@(elms map compileType) map {
      case ts => WeededAst.Type.Tuple(ts)
    }
    case ParsedAst.Type.Parametric(name, elms) => @@(elms map compileType) map {
      case ts => WeededAst.Type.Parametric(name, ts)
    }
    // TODO: What about nested lattice types, e.g. <<foo>> or even <(Int, <Foo>)>
    case ParsedAst.Type.Lattice(tpe) => compileType(tpe) map {
      case t1 => WeededAst.Type.Lattice(t1)
    }
  }

}
