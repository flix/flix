package ca.uwaterloo.flix.lang.ast

trait ResolvedAst

object ResolvedAst {

  // TODO: Split these up or not?
  case class SymbolTable(
                          m: Map[Ref, ResolvedAst.Declaration],
                          relations: Map[Ref, ResolvedAst.Declaration.Relation]
                          )

  sealed trait Declaration

  object Declaration {

    case class Fact(head: ResolvedAst.Predicate) extends Declaration

    case class Relation() extends Declaration

  }

  sealed trait Literal

  sealed trait Expression extends WeededAst

  object Expression {

    case class Var(ident: ParsedAst.Ident) extends ResolvedAst.Expression

    case class Ref(name: ParsedAst.QName, decl: WeededAst.Declaration) extends ResolvedAst.Expression

    case class IfThenElse(e1: ResolvedAst.Expression, e2: ResolvedAst.Expression, e3: ResolvedAst.Expression) extends ResolvedAst.Expression

    case class Let(ident: ParsedAst.Ident, value: ResolvedAst.Expression, body: ResolvedAst.Expression) extends ResolvedAst.Expression

  }

  sealed trait Predicate

  object Predicate {

    case class Relational(/* todo: what */) extends ResolvedAst.Predicate

    case class Functional(/*  todo: what */) extends ResolvedAst.Predicate

  }

  sealed trait Ref

  object Ref {

    // TODO: Need this or not?
    case class FormalArg(location: SourceLocation)

  }


}