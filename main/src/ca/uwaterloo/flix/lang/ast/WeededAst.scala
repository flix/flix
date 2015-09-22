package ca.uwaterloo.flix.lang.ast

trait WeededAst

// TODO: Ensure that there is no reference to "ParsedAst.X"
// TODO: Convert everything into lists?
// TODO: Summary changes made by this phase.

object WeededAst {
// TODO: Every Seq should be replaced by List
  case class Root(declarations: Seq[WeededAst.Declaration]) extends WeededAst

  sealed trait Declaration

  object Declaration {

    case class Namespace(name: ParsedAst.QName, body: Seq[WeededAst.Declaration]) extends WeededAst.Declaration

    case class Fact(head: WeededAst.PredicateWithApply) extends WeededAst.Declaration

    case class Rule(head: WeededAst.PredicateWithApply, body: Seq[WeededAst.PredicateNoApply]) extends WeededAst.Declaration

  }

  sealed trait Definition extends WeededAst.Declaration {
    // TODO: require all these to have a tpe?
  }

  object Definition {

    case class Value(ident: ParsedAst.Ident, tpe: WeededAst.Type, e: WeededAst.Expression) extends WeededAst.Definition

    // TODO: Decorate with the full type....
    case class Enum(ident: ParsedAst.Ident, cases: Map[String, ParsedAst.Type.Tag]) extends WeededAst.Definition

    // TODO: Improve? or at least do something with traits?
    case class Lattice(ident: ParsedAst.Ident, elms: Seq[ParsedAst.QName], traits: Seq[ParsedAst.Trait]) extends WeededAst.Definition

    // TODO
    case class JoinSemiLattice(ident: ParsedAst.Ident,
                               bot: ParsedAst.QName,
                               leq: ParsedAst.QName,
                               lub: ParsedAst.QName,
                               norm: Option[ParsedAst.QName],
                               widen: Option[ParsedAst.QName]) extends WeededAst.Definition

    // TODO
    case class CompleteLattice(ident: ParsedAst.Ident,
                               bot: ParsedAst.QName,
                               top: ParsedAst.QName,
                               leq: ParsedAst.QName,
                               lub: ParsedAst.QName,
                               glb: ParsedAst.QName,
                               norm: Option[ParsedAst.QName],
                               widen: Option[ParsedAst.QName])

    // TODO: Change signature of attributes.
    case class Relation(ident: ParsedAst.Ident, attributes: Seq[WeededAst.Attribute]) extends WeededAst.Definition

  }

  sealed trait Literal extends WeededAst

  object Literal {

    case object Unit extends WeededAst.Literal

    case class Bool(literal: scala.Boolean) extends WeededAst.Literal

    case class Int(literal: scala.Int) extends WeededAst.Literal

    case class Str(literal: java.lang.String) extends WeededAst.Literal

    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, literal: WeededAst.Literal) extends WeededAst.Literal

    case class Tuple(elms: Seq[WeededAst.Literal]) extends WeededAst.Literal

  }

  sealed trait Expression extends WeededAst

  object Expression {

    case class AmbiguousVar(name: ParsedAst.QName) extends WeededAst.Expression

    case class AmbiguousApply(name: ParsedAst.QName, arguments: Seq[WeededAst.Expression]) extends WeededAst.Expression

    case class Lit(literal: WeededAst.Literal) extends WeededAst.Expression

    case class Lambda(formals: Seq[(ParsedAst.Ident, WeededAst.Type)], tpe: WeededAst.Type, body: WeededAst.Expression) extends WeededAst.Expression

    case class Unary(op: UnaryOperator, e: WeededAst.Expression) extends WeededAst.Expression

    // TODO: Swap arguments
    case class Binary(e1: WeededAst.Expression, op: BinaryOperator, e2: WeededAst.Expression) extends WeededAst.Expression

    case class IfThenElse(e1: WeededAst.Expression, e2: WeededAst.Expression, e3: WeededAst.Expression) extends WeededAst.Expression

    // TODO: Why not just call these e1 and e2?
    case class Let(ident: ParsedAst.Ident, value: WeededAst.Expression, body: WeededAst.Expression) extends WeededAst.Expression

    case class Match(e: WeededAst.Expression, rules: Seq[(WeededAst.Pattern, WeededAst.Expression)]) extends WeededAst.Expression

    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, e: WeededAst.Expression) extends WeededAst.Expression

    case class Tuple(elms: Seq[WeededAst.Expression]) extends WeededAst.Expression

    case class Ascribe(e: WeededAst.Expression, tpe: WeededAst.Type) extends WeededAst.Expression

    case class Error(location: SourceLocation) extends WeededAst.Expression

  }

  sealed trait Pattern extends WeededAst

  object Pattern {

    case class Wildcard(location: SourceLocation) extends WeededAst.Pattern

    case class Var(ident: ParsedAst.Ident) extends WeededAst.Pattern

    case class Lit(literal: WeededAst.Literal) extends WeededAst.Pattern

    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, p: WeededAst.Pattern) extends WeededAst.Pattern

    case class Tuple(elms: Seq[WeededAst.Pattern]) extends WeededAst.Pattern

  }

  case class PredicateNoApply(name: ParsedAst.QName, terms: Seq[WeededAst.TermNoApply]) extends WeededAst

  case class PredicateWithApply(name: ParsedAst.QName, terms: Seq[WeededAst.TermWithApply]) extends WeededAst

  sealed trait TermNoApply extends WeededAst

  object TermNoApply {

    case class Wildcard(location: SourceLocation) extends WeededAst.TermNoApply

    case class Var(ident: ParsedAst.Ident) extends WeededAst.TermNoApply

    case class Lit(literal: WeededAst.Literal) extends WeededAst.TermNoApply

  }

  sealed trait TermWithApply extends WeededAst

  object TermWithApply {

    case class Wildcard(location: SourceLocation) extends WeededAst.TermWithApply

    case class Var(ident: ParsedAst.Ident) extends WeededAst.TermWithApply

    case class Lit(literal: WeededAst.Literal) extends WeededAst.TermWithApply

    case class Apply(name: ParsedAst.QName, args: Seq[WeededAst.TermWithApply]) extends WeededAst.TermWithApply

  }


  sealed trait Type

  object Type {

    case object Unit extends WeededAst.Type

    case class Ambiguous(name: ParsedAst.QName) extends WeededAst.Type

    case class Tag(ident: ParsedAst.Ident, tpe: WeededAst.Type) extends WeededAst.Type

    case class Tuple(elms: Seq[WeededAst.Type]) extends WeededAst.Type

    case class Function(t1: WeededAst.Type, t2: WeededAst.Type) extends WeededAst.Type

    case class Parametric(name: ParsedAst.QName, elms: Seq[WeededAst.Type]) extends WeededAst.Type

    case class Lattice(tpe: WeededAst.Type) extends WeededAst.Type

  }


  case class Attribute(ident: ParsedAst.Ident, tpe: WeededAst.Type) extends WeededAst

}