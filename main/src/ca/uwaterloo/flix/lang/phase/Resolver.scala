package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.Compiler
import ca.uwaterloo.flix.lang.ast._

import util.Validation
import util.Validation._

object Resolver {

  import ResolverError._

  sealed trait ResolverError {
    def format: String
  }

  object ResolverError {

    // TODO
    case class DuplicateDefinition()

    /**
     * An error raised to indicate that the given `name` in the given `namespace` was not found.
     *
     * @param name the unresolved name.
     * @param namespace the current namespace.
     */
    case class UnresolvedReference(name: ParsedAst.QName, namespace: List[String]) extends ResolverError {
      val format = s"Error: Unresolved reference $name in $namespace at ${name.location}\n"
    }

  }

  def resolve(wast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {
    wast.declarations.map {
      case wd: WeededAst.Declaration => Declaration.symbols(wd, Nil)
    }

    ???
  }

  object Declaration {

    def symbols(wd: WeededAst.Declaration, namespace: List[String]): Validation[Map[ResolvedAst.RName, WeededAst.Definition], ResolverError] = wd match {
      case WeededAst.Declaration.Namespace(ParsedAst.QName(parts, location), body) =>

        ???

      case d: WeededAst.Declaration.Fact => Map.empty[ResolvedAst.RName, WeededAst.Definition].toSuccess // nop
      case d: WeededAst.Declaration.Rule => Map.empty[ResolvedAst.RName, WeededAst.Definition].toSuccess // nop
      case d: WeededAst.Definition => Definition.symbols(d)
    }


    def link(p: WeededAst.PredicateNoApply, globals: Map[ResolvedAst.RName, WeededAst.Definition]): ResolvedAst.Predicate =
      ???

    //      globals.get(p.name) match {
    //        case None => ??? //UnknownPredicate(p.name)
    //        case Some(d: ParsedAst.Definition.Function) => ResolvedAst.Predicate.Functional()
    //        case Some(d: ParsedAst.Definition.Relation) => ResolvedAst.Predicate.Relational()
    //        case Some(otherDecl) => ??? // IllegalReference("Relation", otherDecl)
    //      }
  }

  object Definition {

    def symbols(wd: WeededAst.Definition): Validation[Map[ResolvedAst.RName, WeededAst.Definition], ResolverError] = wd match {
      case WeededAst.Definition.Function(ident, formals, tpe, body) => ???
      case _ => ???
    }

  }

  object Literal {

    def link(wast: WeededAst.Literal, namespace: List[String], globals: Map[ResolvedAst.RName, WeededAst.Definition]): Validation[ResolvedAst.Literal, ResolverError] = wast match {
      case WeededAst.Literal.Unit => ResolvedAst.Literal.Unit.toSuccess
      case WeededAst.Literal.Bool(b) => ResolvedAst.Literal.Bool(b).toSuccess
      case WeededAst.Literal.Int(i) => ResolvedAst.Literal.Int(i).toSuccess
      case WeededAst.Literal.Str(s) => ResolvedAst.Literal.Str(s).toSuccess
      case WeededAst.Literal.Tag(name, ident, literal) => lookupDef(name, namespace, globals) match {
        case None => UnresolvedReference(name, namespace).toFailure
        case Some((rname, defn)) => link(literal, namespace, globals) map {
          case l => ResolvedAst.Literal.Tag(rname, ident, l, defn)
        }
      }
      case WeededAst.Literal.Tuple(welms) => @@(welms map (l => link(l, namespace, globals))) map {
        case elms => ResolvedAst.Literal.Tuple(elms)
      }
    }
  }

  object Expression {

    def link(wast: WeededAst.Expression, globals: Map[ResolvedAst.RName, WeededAst.Definition]): Validation[ResolvedAst.Expression, ResolverError] = wast match {
      case WeededAst.Expression.AmbiguousVar(name) => ???
      case WeededAst.Expression.AmbiguousApply(name, args) => ???
      case WeededAst.Expression.Lit(wlit) => ???
      case WeededAst.Expression.Lambda(formals, tpe, body) => ???
      case WeededAst.Expression.Unary(op, e) => ???
      case WeededAst.Expression.Binary(e1, op, e2) => ???
      case WeededAst.Expression.IfThenElse(e1, e2, e3) => ???
      //case WeededAst.Expression.Let

      //      case class IfThenElse(e1: WeededAst.Expression, e2: WeededAst.Expression, e3: WeededAst.Expression) extends WeededAst.Expression
      //
      //      case class Let(ident: ParsedAst.Ident, value: WeededAst.Expression, body: WeededAst.Expression) extends WeededAst.Expression
      //
      //      case class Match(e: WeededAst.Expression, rules: Seq[(WeededAst.Pattern, WeededAst.Expression)]) extends WeededAst.Expression
      //
      //      case class Tag(name: ParsedAst.QName, ident: ParsedAst.Ident, e: WeededAst.Expression) extends WeededAst.Expression
      //
      //      case class Tuple(elms: Seq[WeededAst.Expression]) extends WeededAst.Expression
      //
      //      case class Ascribe(e: WeededAst.Expression, tpe: WeededAst.Type) extends WeededAst.Expression
      //
      //      case class Error(location: SourceLocation) extends WeededAst.Expression
    }

  }

  object Pattern {

    def link(wast: WeededAst.Pattern, namespace: List[String], globals: Map[ResolvedAst.RName, WeededAst.Definition]): Validation[ResolvedAst.Pattern, ResolverError] = wast match {
      case WeededAst.Pattern.Wildcard(location) => ResolvedAst.Pattern.Wildcard(location).toSuccess
      case WeededAst.Pattern.Var(ident) => ResolvedAst.Pattern.Var(ident).toSuccess
      case WeededAst.Pattern.Lit(literal) => Literal.link(literal, namespace, globals) map ResolvedAst.Pattern.Lit
      case WeededAst.Pattern.Tag(name, ident, wpat) => lookupDef(name, namespace, globals) match {
        case None => UnresolvedReference(name, namespace).toFailure
        case Some((rname, defn)) => link(wpat, namespace, globals) map {
          case pat => ResolvedAst.Pattern.Tag(rname, ident, pat, defn)
        }
      }
      case WeededAst.Pattern.Tuple(welms) => @@(welms map (e => link(e, namespace, globals))) map ResolvedAst.Pattern.Tuple
    }
  }


  def lookupDef(name: ParsedAst.QName, namespace: List[String], globals: Map[ResolvedAst.RName, WeededAst.Definition]): Option[(ResolvedAst.RName, WeededAst.Definition)] =
    name.parts.toList match {
      case Nil => throw Compiler.InternalCompilerError("Unexpected emtpy name.", name.location)
      case simple :: Nil =>
        val rname = ResolvedAst.RName(namespace ::: simple :: Nil)
        globals.get(rname) map {
          case d => (rname, d)
        }
      case fqn =>
        val rname = ResolvedAst.RName(fqn)
        globals.get(rname) map {
          case d => (rname, d)
        }
    }

}
