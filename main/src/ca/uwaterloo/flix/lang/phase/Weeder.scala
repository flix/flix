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
// TODO: consider naming things pe for parsed expression, instead of we for weeded exp.
// TODO: Should the phases be structure in a hierarchy namespace too?

object Weeder {

  import WeederError._

  /**
   * A common super-type for weeding errors.
   */
  sealed trait WeederError {
    /**
     * Returns a human readable error message as a string.
     */
    def format: String
  }

  object WeederError {

    /**
     * An error raised to indicate that an apply occurs in the body of a rule.
     *
     * @param location the location where the apply occurs.
     */
    case class ApplyInBody(location: SourceLocation) extends WeederError {
      val format =
        s"Error: Function calls are not allowed in a term appearing in the body of a rule.\n" +
          s"  Call was here: ${location.format}\n"
    }

    /**
     * An error raised to indicate that the attribute `name` was declared multiple times.
     *
     * @param name the name of the attribute.
     * @param location1 the location of the first declaration.
     * @param location2 the location of the second declaration.
     */
    case class DuplicateAttribute(name: String, location1: SourceLocation, location2: SourceLocation) extends WeederError {
      val format =
        s"Error: Duplicate attribute name: '$name'.\n" +
          s"  First declaration was here: ${location1.format}.\n" +
          s"  Second declaration was here: ${location2.format}\n"
    }

    /**
     * An error raised to indicate that the formal argument `name` was declared multiple times.
     *
     * @param name the name of the argument.
     * @param location1 the location of the first declaration.
     * @param location2 the location of the second declaration.
     */
    case class DuplicateFormal(name: String, location1: SourceLocation, location2: SourceLocation) extends WeederError {
      val format =
        s"Error: Duplicate formal argument: '$name'.\n" +
          s"  First declaration was here ${location1.format}\n" +
          s"  Second declaration was here: ${location2.format}\n"
    }

    /**
     * An error raised to indicate that the tag `name` was declared multiple times.
     *
     * @param name the name of the tag.
     * @param location1 the location of the first declaration.
     * @param location2 the location of the second declaration.
     */
    case class DuplicateTag(name: String, location1: SourceLocation, location2: SourceLocation) extends WeederError {
      val format =
        s"Error: Duplicate tag name: '$name'.\n" +
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
        s"Error: Non-linear pattern: The variable: '$name' occurs twice.\n" +
          s"  First occurrence was here: ${location1.format}\n" +
          s"  Second occurrence was here: ${location2.format}\n"
    }

  }

  /**
   * Compiles the given parsed `ast` to a weeded ast.
   */
  def weed(ast: ParsedAst.Root): Validation[WeededAst.Root, WeederError] = {
    @@(ast.declarations.map(Declaration.weed)) map WeededAst.Root
  }

  object Declaration {

    /**
     * Compiles the given parsed declaration `d` to a weeded declaration.
     */
    def weed(d: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = d match {
      case d: ParsedAst.Declaration.Namespace => weed(d)
      case d: ParsedAst.Declaration.Fact => weed(d)
      case d: ParsedAst.Declaration.Rule => weed(d)
      case dd: ParsedAst.Definition => compileDefinition(dd)
    }

    /**
     * Compiles the given parsed namespace declaration `d` to a weeded namespace declaration.
     */
    def weed(d: ParsedAst.Declaration.Namespace): Validation[WeededAst.Declaration.Namespace, WeederError] =
      @@(d.body.map(weed)) map (ds => WeededAst.Declaration.Namespace(d.name, ds))

    /**
     * Compiles the given parsed fact `d` to a weeded fact.
     */
    def weed(d: ParsedAst.Declaration.Fact): Validation[WeededAst.Declaration.Fact, WeederError] =
      compilePredicateWithApply(d.head) map {
        case p => WeededAst.Declaration.Fact(p)
      }

    /**
     * Compiles the parsed rule `d` to a weeded rule.
     */
    def weed(d: ParsedAst.Declaration.Rule): Validation[WeededAst.Declaration.Rule, WeederError] = {
      val headVal = compilePredicateWithApply(d.head)
      val bodyVal = @@(d.body.map(compilePredicateNoApply))

      @@(headVal, bodyVal) map {
        case (head, body) => WeededAst.Declaration.Rule(head, body)
      }
    }

  }

  // TODO: Make use of these namespaces.
  object Definition {

  }


  /**
   * Compiles the given parsed definition `d` to a weeded definition.
   */
  def compileDefinition(d: ParsedAst.Definition): Validation[WeededAst.Declaration, WeederError] = d match {
    case d: ParsedAst.Definition.Function => compileFunction(d)
    case d: ParsedAst.Definition.Value => compileValue(d)
    case d: ParsedAst.Definition.Enum => compileEnum(d)
    case d: ParsedAst.Definition.Lattice => compileLattice(d)
    case d: ParsedAst.Definition.Relation => compileRelation(d)
  }

  /**
   * Compiles the given parsed function declaration `d` to a weeded function declaration.
   */
  def compileFunction(d: ParsedAst.Definition.Function): Validation[WeededAst.Definition.Value, WeederError] = {
    val formals2 = d.formals.map {
      case (ident, tpe) => Type.weed(tpe) map (t => (ident, t))
    }
    val returnTpeVal = Type.weed(d.tpe)
    val bodyVal = compileExpression(d.body)
    // TODO: Naming
    @@(@@(formals2), returnTpeVal, bodyVal) map {
      case (wformals, wreturnTpe, wbody) => {
        val exp = WeededAst.Expression.Lambda(wformals, wreturnTpe, wbody)
        WeededAst.Definition.Value(d.ident, /*  TODO: incorrect, must construct function type */ wreturnTpe, exp)
      }
    }
  }

  /**
   * Compiles the given parsed value declaration `d` to a weeded declaration.
   */
  def compileValue(d: ParsedAst.Definition.Value): Validation[WeededAst.Definition.Value, WeederError] =
    @@(Type.weed(d.tpe), compileExpression(d.e)) map {
      case (wtpe, we) => WeededAst.Definition.Value(d.ident, wtpe, we)
    }

  /**
   * Compiles the given parsed enum declaration `d` to a weeded enum declaration.
   *
   * Fails if the same tag name is occurs twice.
   */
  def compileEnum(d: ParsedAst.Definition.Enum): Validation[WeededAst.Definition.Enum, WeederError] =
    Validation.fold[ParsedAst.Type.Tag, Map[String, ParsedAst.Type.Tag], WeederError](d.cases, Map.empty) {
      // loop through each tag declaration
      case (macc, tag@ParsedAst.Type.Tag(ParsedAst.Ident(name, location2), _)) => macc.get(name) match {
        // check if the tag was already declared
        case None => (macc + (name -> tag)).toSuccess
        case Some(otherTag) => DuplicateTag(name, otherTag.ident.location, location2).toFailure
      }
    } map {
      case m => WeededAst.Definition.Enum(d.ident, m)
    }

  /**
   * Compiles the given parsed lattice `d` to a weeded lattice.
   */
  def compileLattice(d: ParsedAst.Definition.Lattice): Validation[WeededAst.Definition.Lattice, WeederError] =
    WeededAst.Definition.Lattice(d.ident, d.elms, d.traits).toSuccess

  /**
   * Compiles the given parsed relation `d` to a weeded relation.
   */
  def compileRelation(d: ParsedAst.Definition.Relation): Validation[WeededAst.Definition.Relation, WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Ident]

    val pattributes = d.attributes.map {
      case ParsedAst.Attribute(ident, ptype) => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> ident)
          Type.weed(ptype) map (tpe => WeededAst.Attribute(ident, tpe))
        case Some(otherIdent) =>
          (DuplicateAttribute(ident.name, otherIdent.location, ident.location): WeederError).toFailure
      }
    }

    @@(pattributes) map {
      case attributes => WeededAst.Definition.Relation(d.ident, attributes)
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
    case ParsedAst.Expression.AmbiguousVar(name) =>
      WeededAst.Expression.AmbiguousVar(name).toSuccess
    case ParsedAst.Expression.AmbiguousApply(name, args) =>
      @@(args map compileExpression) map {
        case wargs => WeededAst.Expression.AmbiguousApply(name, wargs)
      }
    case ParsedAst.Expression.Lit(literal) =>
      compileLiteral(literal) map WeededAst.Expression.Lit
    case ParsedAst.Expression.Lambda(formals, tpe, body) =>
      val formals2 = formals map {
        case (ident, formalType) => Type.weed(formalType) map (t => (ident, t))
      }
      // TODO: Naming
      @@(@@(formals2), Type.weed(tpe), compileExpression(body)) map {
        case (wformals, wtpe, wbody) => WeededAst.Expression.Lambda(wformals, wtpe, wbody)
      }
    case ParsedAst.Expression.Unary(op, e1) =>
      compileExpression(e) map {
        case we1 => WeededAst.Expression.Unary(op, we1)
      }
    case ParsedAst.Expression.Binary(e1, op, e2) =>
      @@(compileExpression(e1), compileExpression(e2)) map {
        case (we1, we2) => WeededAst.Expression.Binary(we1, op, we2)
      }
    case ParsedAst.Expression.IfThenElse(e1, e2, e3) =>
      @@(compileExpression(e1), compileExpression(e2), compileExpression(e3)) map {
        case (we1, we2, we3) => WeededAst.Expression.IfThenElse(we1, we2, we3)
      }
    case ParsedAst.Expression.Let(ident, value, body) =>
      @@(compileExpression(value), compileExpression(body)) map {
        case (wvalue, wbody) => WeededAst.Expression.Let(ident, wvalue, wbody)
      }
    case ParsedAst.Expression.Match(e1, rules) =>
      val e1Val = compileExpression(e1)
      val rulesVal = rules map {
        case (pattern, body) => @@(Pattern.weed(pattern), compileExpression(body))
      }
      @@(e1Val, @@(rulesVal)) map {
        case (we1, wrules) => WeededAst.Expression.Match(we1, wrules)
      }
    case ParsedAst.Expression.Infix(e1, name, e2) => @@(compileExpression(e1), compileExpression(e2)) map {
      case (we1, we2) => WeededAst.Expression.AmbiguousApply(name, Seq(we1, we2))
    }
    case ParsedAst.Expression.Tag(name, ident, e1) => compileExpression(e1) map {
      case we1 => WeededAst.Expression.Tag(name, ident, we1)
    }
    case ParsedAst.Expression.Tuple(elms) => @@(elms map compileExpression) map {
      case welms => WeededAst.Expression.Tuple(welms)
    }
    case ParsedAst.Expression.Ascribe(e1, tpe) =>
      @@(compileExpression(e), Type.weed(tpe)) map {
        case (we1, wtpe) => WeededAst.Expression.Ascribe(we1, wtpe)
      }
    case ParsedAst.Expression.Error(location) => WeededAst.Expression.Error(location).toSuccess
  }

  object Pattern {
    /**
     * Weeds the parsed pattern `p`.
     *
     * Fails if the pattern is non-linear, i.e. if the same variable occurs twice.
     */
    def weed(p: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
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
  }


  /**
   * Compiles the given parsed predicate `p` to a weeded predicate.
   *
   * Fails if the parsed predicate contains a function call.
   */
  def compilePredicateNoApply(p: ParsedAst.Predicate): Validation[WeededAst.PredicateNoApply, WeederError] =
    @@(p.terms.map(compileTermNoApply)) map {
      case wterms => WeededAst.PredicateNoApply(p.name, wterms)
    }

  /**
   * Compiles the given parsed predicate `p` to a weeded predicate.
   */
  def compilePredicateWithApply(p: ParsedAst.Predicate): Validation[WeededAst.PredicateWithApply, WeederError] =
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
    case ParsedAst.Term.Apply(name, args) => ApplyInBody(name.location).toFailure
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

  object Type {
    /**
     * Weeds the given parsed type `past`.
     */
    def weed(past: ParsedAst.Type): Validation[WeededAst.Type, WeederError] = past match {
      case ParsedAst.Type.Unit => WeededAst.Type.Unit.toSuccess
      case ParsedAst.Type.Ambiguous(name) => WeededAst.Type.Ambiguous(name).toSuccess
      case ParsedAst.Type.Function(ptype1, ptype2) =>
        @@(weed(ptype1), weed(ptype2)) map {
          case (tpe1, tpe2) => WeededAst.Type.Function(tpe1, tpe2)
        }
      case ParsedAst.Type.Tag(ident, ptype) => weed(ptype) map {
        case tpe => WeededAst.Type.Tag(ident, tpe)
      }
      case ParsedAst.Type.Tuple(pelms) => @@(pelms map weed) map {
        case elms => WeededAst.Type.Tuple(elms)
      }
      case ParsedAst.Type.Parametric(name, pelms) => @@(pelms map weed) map {
        case elms => WeededAst.Type.Parametric(name, elms)
      }
      // TODO: What about nested lattice types, e.g. <<foo>> or even <(Int, <Foo>)>
      case ParsedAst.Type.Lattice(ptype) => weed(ptype) map {
        case tpe => WeededAst.Type.Lattice(tpe)
      }
    }
  }


}
