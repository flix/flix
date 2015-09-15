package ca.uwaterloo.flix.lang.ast

trait WeededAst

// TODO: Ensure that there is no reference to "ParsedAst.X"

object WeededAst {

  case class Root(declarations: Seq[WeededAst.Declaration]) extends WeededAst

  sealed trait Declaration

  object Declaration {

    case class Namespace(name: ParsedAst.QName, body: Seq[WeededAst.Declaration]) extends WeededAst.Declaration

    case class Tpe(ident: ParsedAst.Ident, tpe: WeededAst.Type) extends WeededAst.Declaration

    case class Val(ident: ParsedAst.Ident, tpe: WeededAst.Type, e: WeededAst.Expression) extends WeededAst.Declaration

    case class Enum(ident: ParsedAst.Ident, cases: Map[String, ParsedAst.Type.Tag]) extends WeededAst.Declaration

    case class Fact(head: WeededAst.PredicateWithApply) extends WeededAst.Declaration

    case class Rule(head: WeededAst.PredicateWithApply, body: Seq[WeededAst.PredicateNoApply]) extends WeededAst.Declaration

  }

  sealed trait Literal

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

    case class Binary(e1: WeededAst.Expression, op: BinaryOperator, e2: WeededAst.Expression) extends WeededAst.Expression

    case class IfThenElse(e1: WeededAst.Expression, e2: WeededAst.Expression, e3: WeededAst.Expression) extends WeededAst.Expression

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

    case class Function(t1: WeededAst.Type, t2: WeededAst.Type) extends WeededAst.Type

    case class Tag(ident: ParsedAst.Ident, tpe: WeededAst.Type) extends WeededAst.Type

    case class Tuple(elms: Seq[WeededAst.Type]) extends WeededAst.Type

    case class Parametric(name: ParsedAst.QName, elms: Seq[WeededAst.Type]) extends WeededAst.Type

    case class Lattice(tpe: WeededAst.Type) extends WeededAst.Type

  }

}