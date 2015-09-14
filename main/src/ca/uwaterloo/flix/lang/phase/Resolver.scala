package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{ResolvedAst, ParsedAst}

object Resolver {

  import ResolverError._

  sealed trait ResolverError

  object ResolverError {

    case class UnknownPredicate(name: ParsedAst.QName) extends ResolverError

    case class IllegalReference(expect: String, actual: String) extends ResolverError

  }

  /**
   * - Resolve proposition vs predicate.
   * - Resolve named types
   */

  def buildSymbols(ast: ParsedAst.Root): Map[ParsedAst.QName, ParsedAst] = ???

  def link(p: ParsedAst.AmbiguousPredicate, symbols: Map[ParsedAst.QName, ParsedAst]): ResolvedAst.Predicate =
    symbols.get(p.name) match {
      case None => ??? //UnknownPredicate(p.name)
      case Some(d: ParsedAst.Declaration.Fun) => ResolvedAst.Predicate.Functional()
      case Some(d: ParsedAst.Declaration.Relation) => ResolvedAst.Predicate.Relational()
      case Some(otherDecl) => ??? // IllegalReference("Relation", otherDecl)
    }

  def resolve(ast: ParsedAst.Root): ResolvedAst = ???


}
