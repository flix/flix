package ca.uwaterloo.flix.lang.ast

trait ResolvedAst

object ResolvedAst {

  case class Root(declarations: Seq[ResolvedAst.Declaration]) extends ResolvedAst

  // TODO Replace QName by RName.
  // TODO: Move this to Name.Resolved and Name.Parsed?
  case class RName(parts: List[String]) extends ResolvedAst

  sealed trait Declaration extends ResolvedAst

  object Declaration {

    case class Fact(head: ResolvedAst.Predicate) extends Declaration


  }

  sealed trait Definition extends ResolvedAst.Declaration

  object Definition {

    case class Relation() extends ResolvedAst.Definition

  }

  sealed trait Literal

  object Literal {

    case object Unit extends ResolvedAst.Literal

    case class Bool(literal: scala.Boolean) extends ResolvedAst.Literal

    case class Int(literal: scala.Int) extends ResolvedAst.Literal

    case class Str(literal: java.lang.String) extends ResolvedAst.Literal

    // TODO: Enum Def?
    case class Tag(name: ResolvedAst.RName, ident: ParsedAst.Ident, literal: ResolvedAst.Literal, defn: WeededAst.Definition) extends ResolvedAst.Literal

    case class Tuple(elms: Seq[ResolvedAst.Literal]) extends ResolvedAst.Literal

  }

  sealed trait Expression extends Definition

  object Expression {

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Expression

    case class Ref(name: ResolvedAst.RName, defn: WeededAst.Definition) extends ResolvedAst.Expression

    case class Lit(literal: ResolvedAst.Literal) extends ResolvedAst.Expression

    case class Lambda(formals: Seq[(ParsedAst.Ident, ResolvedAst.Type)], returnType: ResolvedAst.Type, body: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Apply(lambda: ResolvedAst.Expression, args: Seq[ResolvedAst.Expression]) extends ResolvedAst.Expression

    case class Unary(op: UnaryOperator, e: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Binary(e1: ResolvedAst.Expression, op: BinaryOperator, e2: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class IfThenElse(e1: ResolvedAst.Expression, e2: ResolvedAst.Expression, e3: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Let(ident: ParsedAst.Ident, value: ResolvedAst.Expression, body: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Match(e: ResolvedAst.Expression, rules: Seq[(ResolvedAst.Pattern, ResolvedAst.Expression)]) extends ResolvedAst.Expression

    case class Tag(name: ResolvedAst.RName, ident: ParsedAst.Ident, e: ResolvedAst.Expression, defn: WeededAst.Definition.Enum) extends ResolvedAst.Expression

    case class Tuple(elms: Seq[ResolvedAst.Expression]) extends ResolvedAst.Expression

    case class Ascribe(e: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Error(location: SourceLocation) extends ResolvedAst.Expression

  }

  sealed trait Pattern extends ResolvedAst

  object Pattern {

    case class Wildcard(location: SourceLocation) extends ResolvedAst.Pattern

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Pattern

    case class Lit(literal: ResolvedAst.Literal) extends ResolvedAst.Pattern

    case class Tag(name: ResolvedAst.RName, ident: ParsedAst.Ident, pat: ResolvedAst.Pattern, defn: WeededAst.Definition) extends ResolvedAst.Pattern

    case class Tuple(elms: Seq[ResolvedAst.Pattern]) extends ResolvedAst.Pattern

  }

  sealed trait Predicate

  object Predicate {

    case class Relational(/* todo: what */) extends ResolvedAst.Predicate

    case class Functional(/*  todo: what */) extends ResolvedAst.Predicate

  }

  sealed trait TermNoApply

  object TermNoApply {

    case class Wildcard(location: SourceLocation) extends ResolvedAst.TermNoApply

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.TermNoApply

    case class Lit(literal: WeededAst.Literal) extends ResolvedAst.TermNoApply

  }

  sealed trait TermWithApply extends WeededAst

  object TermWithApply {

    case class Wildcard(location: SourceLocation) extends ResolvedAst.TermWithApply

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.TermWithApply

    case class Lit(literal: WeededAst.Literal) extends ResolvedAst.TermWithApply

    case class Apply(name: ParsedAst.QName, args: Seq[WeededAst.TermWithApply]) extends ResolvedAst.TermWithApply

  }

  sealed trait Type extends ResolvedAst

  object Type {

    case object Unit extends ResolvedAst.Type

    case object Bool extends ResolvedAst.Type

    case object Int extends ResolvedAst.Type

    case object Str extends ResolvedAst.Type

    case class Tag(ident: ParsedAst.Ident, tpe: ResolvedAst.Type) extends ResolvedAst.Type

    case class Tuple(elms: Seq[ResolvedAst.Type]) extends ResolvedAst.Type

    case class Function(t1: ResolvedAst.Type, t2: ResolvedAst.Type) extends ResolvedAst.Type

    case class Parametric(name: ResolvedAst.RName, elms: Seq[ResolvedAst.Type]) extends ResolvedAst.Type

    case class Lattice(tpe: ResolvedAst.Type) extends ResolvedAst.Type

  }

}