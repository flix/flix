package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.Compiler
import ca.uwaterloo.flix.lang.ast._
import ca.uwaterloo.flix.lang.phase.Weeder.WeederError

import util.Validation
import util.Validation._

object Resolver {

  import ResolverError._

  sealed trait ResolverError {
    def format: String
  }

  object ResolverError {

    // TODO: Cyclic stuff.
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

  /**
   * Resolves all symbols in the given AST `wast`.
   */
  def resolve(wast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {
    // TODO: Can anyone actually understand this: ??
    val globalsVal = Validation.fold[WeededAst.Declaration, Map[Name.Resolved, WeededAst.Definition], ResolverError](wast.declarations, Map.empty) {
      case (macc, d) => Declaration.symbols(d, List.empty) map {
        case m2 => m2.foldLeft(macc) {
          case (macc2, (rname, defn)) => macc2.get(rname) match {
            case None => macc2 + (rname -> defn)
            case Some(_) => throw new RuntimeException("duplicate name")
          }
        }
      }
    }

    globalsVal flatMap {
      case globals =>

        val factsVal = Declaration.collectFacts(wast, globals)
        val rulesVal = Declaration.collectRules(wast, globals)

        @@(factsVal, rulesVal) map {
          case (facts, rules) => ResolvedAst.Root(facts, rules)
        }
    }
  }

  object Declaration {

    def symbols(wast: WeededAst.Declaration, namespace: List[String]): Validation[Map[Name.Resolved, WeededAst.Definition], ResolverError] = wast match {
      // TODO: Can anyone actually understand this: ??
      case WeededAst.Declaration.Namespace(ParsedAst.QName(parts, location), body) =>
        Validation.fold[WeededAst.Declaration, Map[Name.Resolved, WeededAst.Definition], ResolverError](body, Map.empty) {
          case (macc, d) => Declaration.symbols(d, namespace ::: parts.toList) map {
            case m2 => m2.foldLeft(macc) {
              case (macc2, (rname, defn)) => macc2.get(rname) match {
                case None => macc2 + (rname -> defn)
                case Some(_) => throw new RuntimeException("duplicate name")
              }
            }
          }
        }
      case WeededAst.Declaration.Fact(head) => Map.empty[Name.Resolved, WeededAst.Definition].toSuccess // nop
      case WeededAst.Declaration.Rule(head, body) => Map.empty[Name.Resolved, WeededAst.Definition].toSuccess // nop
      case defn: WeededAst.Definition => symbols(defn, namespace)
    }

    def symbols(wast: WeededAst.Definition, namespace: List[String]): Validation[Map[Name.Resolved, WeededAst.Definition], ResolverError] = wast match {
      case WeededAst.Definition.Value(ident, tpe, e) => Map(toRName(ident, namespace) -> wast).toSuccess
      case WeededAst.Definition.Enum(ident, cases) => Map(toRName(ident, namespace) -> wast).toSuccess
      // TODO: Here we clearly see the need for several namespaces.
      case WeededAst.Definition.Lattice(ident, elms, traits) => Map.empty[Name.Resolved, WeededAst.Definition].toSuccess // TODO
      case WeededAst.Definition.Relation(ident, attributes) => Map.empty[Name.Resolved, WeededAst.Definition].toSuccess // TODO
    }


    // TODO: Can we avoid this toList thing?
    def collectFacts(wast: WeededAst.Root, globals: Map[Name.Resolved, WeededAst.Definition]): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = {
      @@(wast.declarations.collect {
        case WeededAst.Declaration.Fact(head) => Predicate.resolve(head, globals) map ResolvedAst.Constraint.Fact
      }) map (_.toList)
    }

    // TODO: Can we avoid this toList thing?
    def collectRules(wast: WeededAst.Root, globals: Map[Name.Resolved, WeededAst.Definition]): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = {
      @@(wast.declarations.collect {
        case WeededAst.Declaration.Rule(whead, wbody) =>
          @@(Predicate.resolve(whead, globals), @@(wbody map (p => Predicate.resolveBody(p, globals)))) map {
            case (head, body) => ResolvedAst.Constraint.Rule(head, body.toList)
          }
      }) map (_.toList)
    }
  }

  object Definition {

    /**
     * Performs symbol resolution in the given definition under the given `namespace`.
     */
    def resolve(wast: WeededAst.Definition, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Definition, ResolverError] = wast match {
      case WeededAst.Definition.Value(ident, wtype, we) =>
        @@(Expression.resolve(we, namespace, globals), Type.resolve(wtype, namespace, globals)) map {
          case (e, tpe) => ResolvedAst.Definition.Value(Name.Resolved(namespace ::: ident.name :: Nil, ident.location), e, tpe)
        }

    }
  }

  object Literal {
    /**
     * Performs symbol resolution in the given literal `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Literal, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Literal, ResolverError] = {
      def visit(wast: WeededAst.Literal): Validation[ResolvedAst.Literal, ResolverError] = wast match {
        case WeededAst.Literal.Unit => ResolvedAst.Literal.Unit.toSuccess
        case WeededAst.Literal.Bool(b) => ResolvedAst.Literal.Bool(b).toSuccess
        case WeededAst.Literal.Int(i) => ResolvedAst.Literal.Int(i).toSuccess
        case WeededAst.Literal.Str(s) => ResolvedAst.Literal.Str(s).toSuccess
        case WeededAst.Literal.Tag(name, ident, literal) => lookupDef(name, namespace, globals) match {
          case None => UnresolvedReference(name, namespace).toFailure
          case Some((rname, defn)) => visit(literal) map {
            case l => ResolvedAst.Literal.Tag(rname, ident, l, defn)
          }
        }
        case WeededAst.Literal.Tuple(welms) => @@(welms map visit) map {
          case elms => ResolvedAst.Literal.Tuple(elms)
        }
      }

      visit(wast)
    }
  }

  object Expression {

    /**
     * Performs symbol resolution in the given expression `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Expression, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Expression, ResolverError] = {
      def visit(wast: WeededAst.Expression, locals: Set[String]): Validation[ResolvedAst.Expression, ResolverError] = wast match {
        case WeededAst.Expression.AmbiguousVar(name) => name.parts match {
          case Seq(x) =>
            if (locals contains x)
              ResolvedAst.Expression.Var(ParsedAst.Ident(x, name.location)).toSuccess
            else
              UnresolvedReference(name, namespace).toFailure
          case xs => lookupDef(name, namespace, globals) match {
            case None => UnresolvedReference(name, namespace).toFailure
            case Some((rname, defn)) => ResolvedAst.Expression.Ref(rname).toSuccess
          }
        }
        case WeededAst.Expression.AmbiguousApply(name, args) =>
          throw new RuntimeException("Remove this node.")
        case WeededAst.Expression.Lit(wlit) => Literal.resolve(wlit, namespace, globals) map ResolvedAst.Expression.Lit
        case WeededAst.Expression.Lambda(wformals, wtype, wbody) =>
          val formalsVal = @@(wformals map {
            case (ident, tpe) => Type.resolve(tpe, namespace, globals) map (t => (ident, t))
          })
          @@(formalsVal, Type.resolve(wtype, namespace, globals), visit(wbody, locals)) map {
            case (formals, tpe, body) => ResolvedAst.Expression.Lambda(formals, tpe, body)
          }
        case WeededAst.Expression.Unary(op, we) =>
          visit(we, locals) map (e => ResolvedAst.Expression.Unary(op, e))
        case WeededAst.Expression.Binary(we1, op, we2) =>
          val lhsVal = visit(we1, locals)
          val rhsVal = visit(we2, locals)
          @@(lhsVal, rhsVal) map {
            case (e1, e2) => ResolvedAst.Expression.Binary(op, e1, e2)
          }
        case WeededAst.Expression.IfThenElse(we1, we2, we3) =>
          val conditionVal = visit(we1, locals)
          val consequentVal = visit(we2, locals)
          val alternativeVal = visit(we3, locals)

          @@(conditionVal, consequentVal, alternativeVal) map {
            case (e1, e2, e3) => ResolvedAst.Expression.IfThenElse(e1, e2, e3)
          }
        case WeededAst.Expression.Let(ident, wvalue, wbody) => {
          val valueVal = visit(wvalue, locals)
          val bodyVal = visit(wbody, locals + ident.name)
          @@(valueVal, bodyVal) map {
            case (value, body) => ResolvedAst.Expression.Let(ident, value, body)
          }
        }
        case WeededAst.Expression.Match(e, rules) => ???

        case WeededAst.Expression.Tag(name, ident, e) => ???

        case WeededAst.Expression.Tuple(elms) => @@(elms map (e => visit(e, locals))) map ResolvedAst.Expression.Tuple
        case WeededAst.Expression.Ascribe(we, wtype) =>
          @@(visit(we, locals), Type.resolve(wtype, namespace, globals)) map {
            case (e, tpe) => ResolvedAst.Expression.Ascribe(e, tpe)
          }
        case WeededAst.Expression.Error(location) => ResolvedAst.Expression.Error(location).toSuccess
      }

      visit(wast, Set.empty)
    }

  }

  object Pattern {

    /**
     * Performs symbol resolution in the given pattern `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Pattern, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Pattern, ResolverError] = {
      def visit(wast: WeededAst.Pattern): Validation[ResolvedAst.Pattern, ResolverError] = wast match {
        case WeededAst.Pattern.Wildcard(location) => ResolvedAst.Pattern.Wildcard(location).toSuccess
        case WeededAst.Pattern.Var(ident) => ResolvedAst.Pattern.Var(ident).toSuccess
        case WeededAst.Pattern.Lit(literal) => Literal.resolve(literal, namespace, globals) map ResolvedAst.Pattern.Lit
        case WeededAst.Pattern.Tag(name, ident, wpat) => lookupDef(name, namespace, globals) match {
          case None => UnresolvedReference(name, namespace).toFailure
          case Some((rname, defn)) => visit(wpat) map {
            case pat => ResolvedAst.Pattern.Tag(rname, ident, pat, defn)
          }
        }
        case WeededAst.Pattern.Tuple(welms) => @@(welms map (e => resolve(e, namespace, globals))) map ResolvedAst.Pattern.Tuple
      }
      visit(wast)
    }
  }

  object Predicate {

    def resolve(p: WeededAst.PredicateWithApply, globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Predicate.Head, ResolverError] = ???

    def resolveBody(p: WeededAst.PredicateNoApply, globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Predicate.Body, ResolverError] = ???

  }

  object Term {

    def resolve(wast: WeededAst.TermNoApply, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.TermNoApply, ResolverError] = wast match {
      case WeededAst.TermNoApply.Wildcard(location) => ResolvedAst.TermNoApply.Wildcard(location).toSuccess
    }

    def resolve(wast: WeededAst.TermWithApply, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.TermWithApply, ResolverError] = wast match {
      case WeededAst.TermWithApply.Wildcard(location) => ResolvedAst.TermWithApply.Wildcard(location).toSuccess
    }

  }

  object Type {

    /**
     * Performs symbol resolution in the given type `wast` under the given `namespace`.
     */
    // TODO: Consider inner visit?
    def resolve(wast: WeededAst.Type, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Validation[ResolvedAst.Type, ResolverError] = wast match {
      case WeededAst.Type.Unit => ResolvedAst.Type.Unit.toSuccess
      case WeededAst.Type.Ambiguous(name) => name.parts match {
        case Seq("Bool") => ResolvedAst.Type.Bool.toSuccess
        case Seq("Int") => ResolvedAst.Type.Int.toSuccess
        case Seq("Str") => ResolvedAst.Type.Str.toSuccess
        case xs => ??? // TODO: Lookup def.
      }
      case WeededAst.Type.Tag(ident, tpe) => ??? // TODO: Shouldn't a tag include a namespace? E.g. there is a difference between foo.Foo.Tag and bar.Foo.Tag?
      case WeededAst.Type.Tuple(welms) => @@(welms map (e => resolve(e, namespace, globals))) map ResolvedAst.Type.Tuple
      case WeededAst.Type.Function(wtype1, wtype2) =>
        @@(resolve(wtype1, namespace, globals), resolve(wtype2, namespace, globals)) map {
          case (tpe1, tpe2) => ResolvedAst.Type.Function(tpe1, tpe2)
        }
      case WeededAst.Type.Parametric(name, elms) => ???
      case WeededAst.Type.Lattice(tpe) => ???
    }
  }


  def lookupDef(name: ParsedAst.QName, namespace: List[String], globals: Map[Name.Resolved, WeededAst.Definition]): Option[(Name.Resolved, WeededAst.Definition)] =
    name.parts.toList match {
      case Nil => throw Compiler.InternalCompilerError("Unexpected emtpy name.", name.location)
      case simple :: Nil =>
        val rname = Name.Resolved(namespace ::: simple :: Nil, ???)
        globals.get(rname) map {
          case d => (rname, d)
        }
      case fqn =>
        val rname = Name.Resolved(fqn, ???)
        globals.get(rname) map {
          case d => (rname, d)
        }
    }

  // TODO
  def isReserved(name: ParsedAst.QName): Boolean = ???

  def toRName(ident: ParsedAst.Ident, namespace: List[String]): Name.Resolved =
    Name.Resolved(namespace ::: ident.name :: Nil, ident.location)

}
