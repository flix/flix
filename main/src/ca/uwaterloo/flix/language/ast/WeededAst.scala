package ca.uwaterloo.flix.language.ast

trait WeededAst

object WeededAst {

  case class Root(declarations: List[WeededAst.Declaration]) extends WeededAst

  sealed trait Declaration extends WeededAst

  object Declaration {

    case class Namespace(name: Name.Unresolved, body: List[WeededAst.Declaration]) extends WeededAst.Declaration

    case class Fact(head: WeededAst.PredicateWithApply) extends WeededAst.Declaration

    case class Rule(head: WeededAst.PredicateWithApply, body: List[WeededAst.PredicateNoApply]) extends WeededAst.Declaration

  }

  sealed trait Definition extends WeededAst.Declaration

  object Definition {

    case class Constant(ident: Name.Ident, e: WeededAst.Expression, tpe: WeededAst.Type) extends WeededAst.Definition

    case class Enum(ident: Name.Ident, cases: Map[String, WeededAst.Type.Tag]) extends WeededAst.Definition

    case class Lattice(ident: Name.Ident, elms: List[WeededAst.Expression]) extends WeededAst.Definition

    case class Relation(ident: Name.Ident, attributes: List[WeededAst.Attribute]) extends WeededAst.Definition

  }

  sealed trait Literal extends WeededAst

  object Literal {

    case class Unit(loc: SourceLocation) extends WeededAst.Literal

    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends WeededAst.Literal

    case class Int(lit: scala.Int, loc: SourceLocation) extends WeededAst.Literal

    case class Str(lit: java.lang.String, loc: SourceLocation) extends WeededAst.Literal

    case class Tag(enum: Name.Unresolved, tag: Name.Ident, literal: WeededAst.Literal, loc: SourceLocation) extends WeededAst.Literal

    case class Tuple(elms: List[WeededAst.Literal], loc: SourceLocation) extends WeededAst.Literal

  }

  sealed trait Expression extends WeededAst

  object Expression {

    case class Lit(literal: WeededAst.Literal) extends WeededAst.Expression

    case class Var(name: Name.Unresolved) extends WeededAst.Expression

    case class Apply(lambda: WeededAst.Expression, arguments: List[WeededAst.Expression]) extends WeededAst.Expression

    case class Lambda(formals: List[WeededAst.FormalArg], body: WeededAst.Expression, tpe: WeededAst.Type) extends WeededAst.Expression

    case class Unary(op: UnaryOperator, e: WeededAst.Expression) extends WeededAst.Expression

    case class Binary(op: BinaryOperator, e1: WeededAst.Expression, e2: WeededAst.Expression) extends WeededAst.Expression

    case class IfThenElse(e1: WeededAst.Expression, e2: WeededAst.Expression, e3: WeededAst.Expression) extends WeededAst.Expression

    case class Let(ident: Name.Ident, value: WeededAst.Expression, body: WeededAst.Expression) extends WeededAst.Expression

    case class Match(e: WeededAst.Expression, rs: List[(WeededAst.Pattern, WeededAst.Expression)]) extends WeededAst.Expression

    case class Tag(name: Name.Unresolved, ident: Name.Ident, e: WeededAst.Expression) extends WeededAst.Expression

    case class Tuple(elms: List[WeededAst.Expression]) extends WeededAst.Expression

    case class Ascribe(e: WeededAst.Expression, tpe: WeededAst.Type) extends WeededAst.Expression

    case class Error(tpe: WeededAst.Type, loc: SourceLocation) extends WeededAst.Expression

  }

  sealed trait Pattern extends WeededAst {
    /**
     * Returns the set of variables bound by this pattern.
     */
    final def bound: Set[String] = this match {
      case WeededAst.Pattern.Wildcard(_) => Set.empty
      case WeededAst.Pattern.Var(ident) => Set(ident.name)
      case WeededAst.Pattern.Lit(lit) => Set.empty
      case WeededAst.Pattern.Tag(name, ident, p) => p.bound
      case WeededAst.Pattern.Tuple(elms) => elms.foldLeft(Set.empty[String]) {
        case (acc, pat) => acc ++ pat.bound
      }
    }
  }

  object Pattern {

    case class Wildcard(location: SourceLocation) extends WeededAst.Pattern

    case class Var(ident: Name.Ident) extends WeededAst.Pattern

    case class Lit(literal: WeededAst.Literal) extends WeededAst.Pattern

    case class Tag(name: Name.Unresolved, ident: Name.Ident, p: WeededAst.Pattern) extends WeededAst.Pattern

    case class Tuple(elms: List[WeededAst.Pattern]) extends WeededAst.Pattern

  }

  // TODO: Organize these as usually done.

  case class PredicateNoApply(name: Name.Unresolved, terms: List[WeededAst.TermNoApply]) extends WeededAst

  case class PredicateWithApply(name: Name.Unresolved, terms: List[WeededAst.TermWithApply]) extends WeededAst

  sealed trait TermNoApply extends WeededAst

  object TermNoApply {

    case class Wildcard(location: SourceLocation) extends WeededAst.TermNoApply

    case class Var(ident: Name.Ident) extends WeededAst.TermNoApply

    case class Lit(literal: WeededAst.Literal) extends WeededAst.TermNoApply

    case class Ascribe(term: TermNoApply, tpe: WeededAst.Type) extends WeededAst.TermNoApply
  }

  sealed trait TermWithApply extends WeededAst

  object TermWithApply {

    // TODO: Wildcards should not be allowed here...
    case class Wildcard(location: SourceLocation) extends WeededAst.TermWithApply

    case class Var(ident: Name.Ident) extends WeededAst.TermWithApply

    case class Lit(literal: WeededAst.Literal) extends WeededAst.TermWithApply

    case class Ascribe(term: TermWithApply, tpe: WeededAst.Type) extends WeededAst.TermWithApply

    case class Apply(name: Name.Unresolved, args: List[WeededAst.TermWithApply]) extends WeededAst.TermWithApply

  }


  sealed trait Type

  object Type {

    case object Unit extends WeededAst.Type

    case class Ambiguous(name: Name.Unresolved) extends WeededAst.Type

    case class Tag(tagName: Name.Ident, tpe: WeededAst.Type) extends WeededAst.Type

    case class Enum(name: Name.Resolved, cases: Map[String, WeededAst.Type.Tag]) extends WeededAst.Type

    case class Tuple(elms: List[WeededAst.Type]) extends WeededAst.Type

    case class Function(args: List[WeededAst.Type], retType: WeededAst.Type) extends WeededAst.Type

  }

  case class Attribute(ident: Name.Ident, tpe: WeededAst.Type) extends WeededAst

  /**
   * An AST node representing a formal argument of a function.
   *
   * @param ident the name of the argument.
   * @param tpe the type of the argument.
   */
  case class FormalArg(ident: Name.Ident, tpe: WeededAst.Type) extends WeededAst


}