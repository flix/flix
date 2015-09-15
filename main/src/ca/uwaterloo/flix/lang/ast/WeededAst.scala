package ca.uwaterloo.flix.lang.ast

trait WeededAst

// TODO: Ensure that there is no reference to "ParsedAst.X"

object WeededAst {

  sealed trait Declaration

  object Declaration {

    case class Enum(ident: ParsedAst.Ident, cases: Map[String, ParsedAst.Type.Tag]) extends WeededAst.Declaration

    case class Fact(head: WeededAst.PredicateWithApply) extends WeededAst.Declaration

    case class Rule(head: WeededAst.PredicateWithApply, body: Seq[WeededAst.PredicateNoApply]) extends WeededAst.Declaration

  }

  sealed trait Pattern extends WeededAst

  object Pattern {

    case class Wildcard(location: SourceLocation) extends WeededAst.Pattern

    case class Var(ident: ParsedAst.Ident) extends WeededAst.Pattern

    case class Lit(literal: ParsedAst.Literal) extends WeededAst.Pattern

    case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, p: WeededAst.Pattern) extends WeededAst.Pattern

    case class Tuple(elms: Seq[WeededAst.Pattern]) extends WeededAst.Pattern

  }

  case class PredicateNoApply(name: ParsedAst.QName, terms: Seq[WeededAst.TermNoApply]) extends WeededAst

  case class PredicateWithApply(name: ParsedAst.QName, terms: Seq[WeededAst.TermWithApply]) extends WeededAst

  sealed trait TermNoApply extends WeededAst

  object TermNoApply {

    case class Wildcard(location: SourceLocation) extends WeededAst.TermNoApply

    case class Var(ident: ParsedAst.Ident) extends WeededAst.TermNoApply

    case class Lit(literal: ParsedAst.Literal) extends WeededAst.TermNoApply

  }

  sealed trait TermWithApply extends WeededAst

  object TermWithApply {

    case class Wildcard(location: SourceLocation) extends WeededAst.TermWithApply

    case class Var(ident: ParsedAst.Ident) extends WeededAst.TermWithApply

    case class Lit(literal: ParsedAst.Literal) extends WeededAst.TermWithApply

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