package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{WeededAst, ResolvedAst, ParsedAst}
import util.Validation

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

  def symbols(ast: ParsedAst.Root): Map[ParsedAst.QName, ParsedAst] = ???

  def link(p: ParsedAst.Predicate, symbols: Map[ParsedAst.QName, ParsedAst]): ResolvedAst.Predicate =
    symbols.get(p.name) match {
      case None => ??? //UnknownPredicate(p.name)
      case Some(d: ParsedAst.Definition.Function) => ResolvedAst.Predicate.Functional()
      case Some(d: ParsedAst.Definition.Relation) => ResolvedAst.Predicate.Relational()
      case Some(otherDecl) => ??? // IllegalReference("Relation", otherDecl)
    }

  def resolve(ast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {
   ???
  }


}
