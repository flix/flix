package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Resolver {

  import ResolverError._

  /**
   * A common super-type for resolver errors.
   */
  sealed trait ResolverError extends Compiler.CompilationError

  object ResolverError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
     * An error raised to indicate that the given `name` is used for multiple definitions.
     *
     * @param name the name
     * @param loc1 the location of the first definition.
     * @param loc2 the location of the second definition.
     */
    case class DuplicateDefinition(name: Name.Resolved, loc1: SourceLocation, loc2: SourceLocation) extends ResolverError {
      val format =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc1.formatSource}")}
            |
            |${consoleCtx.red(s">> Duplicate definition of the name '${name.format}'.")}
            |
            |First definition was here:
            |${loc1.underline}
            |Second definition was here:
            |${loc2.underline}
            |Tip: Consider renaming or removing one of the definitions.
         """.stripMargin
    }

    /**
     * An error raised to indicate that the given `name` in the given `namespace` was not found.
     *
     * @param name the unresolved name.
     * @param namespace the current namespace.
     */
    case class UnresolvedReference(name: Name.Unresolved, namespace: List[String]) extends ResolverError {
      val format: String = s"Error: Unresolved reference to '${name.format}' in namespace '${namespace.mkString("::")}' at: ${name.location.format}\n"
    }

    // TODO: All kinds of arity errors....

    // TODO: Cyclic stuff.

    // TODO: Unused, let, parameters etc.

  }

  object SymbolTable {
    val empty = SymbolTable(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
  }

  case class SymbolTable(enums: Map[Name.Resolved, WeededAst.Definition.Enum],
                         constants: Map[Name.Resolved, WeededAst.Definition.Constant],
                         lattices: Map[Name.Resolved, WeededAst.Definition.Lattice],
                         relations: Map[Name.Resolved, WeededAst.Definition.Relation],
                         types: Map[Name.Resolved, WeededAst.Type]) {
    // TODO: Cleanup
    def lookupEnum(name: Name.Unresolved, namespace: List[String]): Validation[(Name.Resolved, WeededAst.Definition.Enum), ResolverError] = {
      val rname = Name.Resolved(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      enums.get(rname) match {
        case None => UnresolvedReference(name, namespace).toFailure
        case Some(d) => (rname, d).toSuccess
      }
    }

    // TODO: Cleanup
    def lookupConstant(name: Name.Unresolved, namespace: List[String]): Validation[(Name.Resolved, WeededAst.Definition.Constant), ResolverError] = {
      val rname = Name.Resolved(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      constants.get(rname) match {
        case None => UnresolvedReference(name, namespace).toFailure
        case Some(d) => (rname, d).toSuccess
      }
    }

    // TODO: Cleanup
    def lookupRelation(name: Name.Unresolved, namespace: List[String]): Validation[(Name.Resolved, WeededAst.Definition.Relation), ResolverError] = {
      val rname = Name.Resolved(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      relations.get(rname) match {
        case None => UnresolvedReference(name, namespace).toFailure
        case Some(d) => (rname, d).toSuccess
      }
    }

    // TODO: Cleanup
    def lookupType(name: Name.Unresolved, namespace: List[String]): Validation[WeededAst.Type, ResolverError] = {
      val rname = Name.Resolved(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      types.get(rname) match {
        case None => UnresolvedReference(name, namespace).toFailure
        case Some(tpe) => tpe.toSuccess
      }
    }

  }

  // TODO: Introduce ResolvedSymbolTable


  /**
   * Resolves all symbols in the given AST `wast`.
   */
  def resolve(wast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {
    // TODO: Can anyone actually understand this: ??
    val symsVal = Validation.fold[WeededAst.Declaration, SymbolTable, ResolverError](wast.declarations, SymbolTable.empty) {
      case (msyms, d) => Declaration.symbolsOf(d, List.empty, msyms)
    }

    symsVal flatMap {
      case syms =>

        val collectedConstants = Validation.fold[Name.Resolved, WeededAst.Definition.Constant, Name.Resolved, ResolvedAst.Definition.Constant, ResolverError](syms.constants) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

        val collectedEnums = Validation.fold[Name.Resolved, WeededAst.Definition.Enum, Name.Resolved, ResolvedAst.Definition.Enum, ResolverError](syms.enums) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

//        val collectedLattices = Validation.fold[Name.Resolved, WeededAst.Definition.Lattice, Name.Resolved, ResolvedAst.Definition.Lattice, ResolverError](syms.lattices) {
//          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
//        }

        val collectedRelations = Validation.fold[Name.Resolved, WeededAst.Definition.Relation, Name.Resolved, ResolvedAst.Definition.Relation, ResolverError](syms.relations) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

        val collectedFacts = Declaration.collectFacts(wast, syms)
        val collectedRules = Declaration.collectRules(wast, syms)

        @@(collectedConstants, collectedEnums, collectedRelations, collectedFacts, collectedRules) map {
          // TODO: Rest...
          case (constants, enums, relations, facts, rules) => ResolvedAst.Root(constants, enums, Map.empty, relations, facts, rules)
        }
    }
  }

  object Declaration {

    /**
     * Constructs the symbol table for the given definition of `wast`.
     */
    def symbolsOf(wast: WeededAst.Declaration, namespace: List[String], syms: SymbolTable): Validation[SymbolTable, ResolverError] = wast match {
      case WeededAst.Declaration.Namespace(Name.Unresolved(parts, location), body) =>
        Validation.fold[WeededAst.Declaration, SymbolTable, ResolverError](body, syms) {
          case (msyms, d) => symbolsOf(d, namespace ::: parts.toList, msyms)
        }
      case WeededAst.Declaration.Fact(head) => syms.toSuccess
      case WeededAst.Declaration.Rule(head, body) => syms.toSuccess
      case defn: WeededAst.Definition => symbolsOf(defn, namespace, syms)
    }

    /**
     * Constructs the symbol for the given definition `wast`.
     */
    def symbolsOf(wast: WeededAst.Definition, namespace: List[String], syms: SymbolTable): Validation[SymbolTable, ResolverError] = wast match {
      case defn@WeededAst.Definition.Constant(ident, tpe, e, loc) =>
        val rname = toRName(ident, namespace)
        syms.constants.get(rname) match {
          case None => syms.copy(constants = syms.constants + (rname -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.location, ident.location).toFailure
        }

      case defn@WeededAst.Definition.Enum(ident, cases, loc) =>
        val rname = toRName(ident, namespace)
        syms.enums.get(rname) match {
          case None => syms.copy(
            enums = syms.enums + (rname -> defn),
            types = syms.types + (rname -> WeededAst.Type.Enum(rname, defn.cases))
          ).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.location, ident.location).toFailure
        }

      case defn@WeededAst.Definition.Lattice(ident, bot, leq, lub, loc) =>
        val rname = toRName(ident, namespace)
        syms.copy(lattices = syms.lattices + (rname -> defn)).toSuccess

      case defn@WeededAst.Definition.Relation(ident, attributes, loc) =>
        val rname = toRName(ident, namespace)
        syms.relations.get(rname) match {
          case None => syms.copy(relations = syms.relations + (rname -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.location, ident.location).toFailure
        }
    }

    def collectFacts(wast: WeededAst.Root, syms: SymbolTable): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = {
      def visit(wast: WeededAst.Declaration, namespace: List[String]): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = wast match {
        case WeededAst.Declaration.Namespace(name, body) =>
          @@(body map (d => visit(d, namespace ::: name.parts))) map (xs => xs.flatten)
        case WeededAst.Declaration.Fact(whead) => Predicate.Head.resolve(whead, namespace, syms) map (p => List(ResolvedAst.Constraint.Fact(p)))
        case _ => List.empty[ResolvedAst.Constraint.Fact].toSuccess
      }

      @@(wast.declarations map (d => visit(d, List.empty))) map (xs => xs.flatten)
    }

    def collectRules(wast: WeededAst.Root, syms: SymbolTable): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = {
      def visit(wast: WeededAst.Declaration, namespace: List[String]): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = wast match {
        case WeededAst.Declaration.Namespace(name, body) =>
          @@(body map (d => visit(d, namespace ::: name.parts.toList))) map (xs => xs.flatten)
        case WeededAst.Declaration.Rule(whead, wbody) =>
          val headVal = Predicate.Head.resolve(whead, namespace, syms)
          val bodyVal = @@(wbody map (p => Predicate.Body.resolve(p, namespace, syms)))
          @@(headVal, bodyVal) map {
            case (head, body) => List(ResolvedAst.Constraint.Rule(head, body))
          }
        case _ => List.empty[ResolvedAst.Constraint.Rule].toSuccess
      }

      @@(wast.declarations map (d => visit(d, List.empty))) map (xs => xs.flatten)
    }
  }

  object Definition {

    /**
     * Performs symbol resolution for the given value definition `wast`.
     */
    // TODO: Pattern match on wast?
    def resolve(wast: WeededAst.Definition.Constant, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Constant, ResolverError] = {
      val name = Name.Resolved(namespace ::: wast.ident.name :: Nil)

      @@(Expression.resolve(wast.e, namespace, syms), Type.resolve(wast.tpe, namespace, syms)) map {
        case (e, tpe) =>
          ResolvedAst.Definition.Constant(name, e, tpe, wast.loc)
      }
    }

    // TODO: Pattern match on wast?
    def resolve(wast: WeededAst.Definition.Enum, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Enum, ResolverError] = {
      val name = Name.Resolved(namespace ::: wast.ident.name :: Nil)

      val casesVal = Validation.fold[String, WeededAst.Type.Tag, String, ResolvedAst.Type.Tag, ResolverError](wast.cases) {
        (k, tpe) => Type.resolve(tpe, namespace ::: wast.ident.name :: Nil, syms) map (t => k -> t.asInstanceOf[ResolvedAst.Type.Tag])
      }

      casesVal map {
        case cases => ResolvedAst.Definition.Enum(name, cases, wast.loc)
      }
    }

    def resolve(wast: WeededAst.Definition.Lattice, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Lattice, ResolverError] = {
      val elmsVal = ???
      val botVal = Expression.resolve(wast.bot, namespace, syms)
      val leqVal = Expression.resolve(wast.leq, namespace, syms)
      val lubVal = Expression.resolve(wast.lub, namespace, syms)

      @@(elmsVal, botVal, leqVal, lubVal) map {
        case (elms, bot, leq, lub) => ResolvedAst.Definition.Lattice(elms, bot, leq, lub, wast.loc)
      }
    }

    // TODO: Pattern match on wast?
    def resolve(wast: WeededAst.Definition.Relation, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Relation, ResolverError] = {
      val name = Name.Resolved(namespace ::: wast.ident.name :: Nil)

      val attributesVal = wast.attributes.map {
        case WeededAst.Attribute(ident, tpe) => Type.resolve(tpe, namespace, syms) map (t => ResolvedAst.Attribute(ident, t))
      }

      @@(attributesVal) map {
        case attributes => ResolvedAst.Definition.Relation(name, attributes, wast.loc)
      }
    }

  }

  object Literal {
    /**
     * Performs symbol resolution in the given literal `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Literal, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Literal, ResolverError] = {
      def visit(wast: WeededAst.Literal): Validation[ResolvedAst.Literal, ResolverError] = wast match {
        case WeededAst.Literal.Unit(loc) => ResolvedAst.Literal.Unit(loc).toSuccess
        case WeededAst.Literal.Bool(b, loc) => ResolvedAst.Literal.Bool(b, loc).toSuccess
        case WeededAst.Literal.Int(i, loc) => ResolvedAst.Literal.Int(i, loc).toSuccess
        case WeededAst.Literal.Str(s, loc) => ResolvedAst.Literal.Str(s, loc).toSuccess
        case WeededAst.Literal.Tag(name, ident, literal, loc) => syms.lookupEnum(name, namespace) flatMap {
          case (rname, defn) => visit(literal) map {
            case l => ResolvedAst.Literal.Tag(rname, ident, l, loc)
          }
        }
        case WeededAst.Literal.Tuple(welms, loc) => @@(welms map visit) map {
          case elms => ResolvedAst.Literal.Tuple(elms, loc)
        }
      }

      visit(wast)
    }
  }

  object Expression {

    /**
     * Performs symbol resolution in the given expression `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Expression, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Expression, ResolverError] = {
      def visit(wast: WeededAst.Expression, locals: Set[String]): Validation[ResolvedAst.Expression, ResolverError] = wast match {
        case WeededAst.Expression.Lit(wlit, loc) => Literal.resolve(wlit, namespace, syms) map {
          case lit => ResolvedAst.Expression.Lit(lit, loc)
        }

        // TODO: Rewrite this ....
        case WeededAst.Expression.Var(name, loc) => name.parts match {
          case Seq(x) =>
            if (locals contains x)
              ResolvedAst.Expression.Var(Name.Ident(x, name.location), loc).toSuccess
            else
              UnresolvedReference(name, namespace).toFailure
          case xs => syms.lookupConstant(name, namespace) map {
            case (rname, defn) => ResolvedAst.Expression.Ref(rname, loc)
          }
        }

        case WeededAst.Expression.Apply(wlambda, wargs, loc) =>
          val lambdaVal = visit(wlambda, locals)
          val argsVal = @@(wargs map {
            case actual => visit(actual, locals)
          })

          @@(lambdaVal, argsVal) map {
            case (lambda, args) => ResolvedAst.Expression.Apply(lambda, args, loc)
          }

        case WeededAst.Expression.Lambda(wformals, wbody, wtype, loc) =>
          val formalsVal = @@(wformals map {
            case WeededAst.FormalArg(ident, tpe) => Type.resolve(tpe, namespace, syms) map (t => ResolvedAst.FormalArg(ident, t))
          })

          formalsVal flatMap {
            case formals =>
              val bindings = formals map (_.ident.name)
              @@(Type.resolve(wtype, namespace, syms), visit(wbody, locals ++ bindings)) map {
                case (tpe, body) => ResolvedAst.Expression.Lambda(formals, tpe, body, loc)
              }
          }

        case WeededAst.Expression.Unary(op, we, loc) =>
          visit(we, locals) map (e => ResolvedAst.Expression.Unary(op, e, loc))
        case WeededAst.Expression.Binary(op, we1, we2, loc) =>
          val lhsVal = visit(we1, locals)
          val rhsVal = visit(we2, locals)
          @@(lhsVal, rhsVal) map {
            case (e1, e2) => ResolvedAst.Expression.Binary(op, e1, e2, loc)
          }
        case WeededAst.Expression.IfThenElse(we1, we2, we3, loc) =>
          val conditionVal = visit(we1, locals)
          val consequentVal = visit(we2, locals)
          val alternativeVal = visit(we3, locals)

          @@(conditionVal, consequentVal, alternativeVal) map {
            case (e1, e2, e3) => ResolvedAst.Expression.IfThenElse(e1, e2, e3, loc)
          }
        case WeededAst.Expression.Let(ident, wvalue, wbody, loc) =>
          val valueVal = visit(wvalue, locals)
          val bodyVal = visit(wbody, locals + ident.name)
          @@(valueVal, bodyVal) map {
            case (value, body) => ResolvedAst.Expression.Let(ident, value, body, loc)
          }

        case WeededAst.Expression.Match(we, wrules, loc) =>
          val e2 = visit(we, locals)
          val rules2 = wrules map {
            case (rulePat, ruleBody) =>
              val bound = locals ++ rulePat.freeVars
              @@(Pattern.resolve(rulePat, namespace, syms), visit(ruleBody, bound))
          }
          @@(e2, @@(rules2)) map {
            case (e, rules) => ResolvedAst.Expression.Match(e, rules, loc)
          }

        case WeededAst.Expression.Tag(name, ident, we, loc) =>
          syms.lookupEnum(name, namespace) flatMap {
            case (rname, defn) => visit(we, locals) map {
              case e => ResolvedAst.Expression.Tag(rname, ident, e, loc)
            }
          }

        case WeededAst.Expression.Tuple(welms, loc) => @@(welms map (e => visit(e, locals))) map {
          case elms => ResolvedAst.Expression.Tuple(elms, loc)
        }
        case WeededAst.Expression.Ascribe(we, wtype, loc) =>
          @@(visit(we, locals), Type.resolve(wtype, namespace, syms)) map {
            case (e, tpe) => ResolvedAst.Expression.Ascribe(e, tpe, loc)
          }
        case WeededAst.Expression.Error(wtype, loc) =>
          Type.resolve(wtype, namespace, syms) map {
            case tpe => ResolvedAst.Expression.Error(tpe, loc)
          }
      }

      visit(wast, Set.empty)
    }

  }

  object Pattern {

    /**
     * Performs symbol resolution in the given pattern `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Pattern, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Pattern, ResolverError] = {
      def visit(wast: WeededAst.Pattern): Validation[ResolvedAst.Pattern, ResolverError] = wast match {
        case WeededAst.Pattern.Wildcard(location) => ResolvedAst.Pattern.Wildcard(location).toSuccess
        case WeededAst.Pattern.Var(ident, loc) => ResolvedAst.Pattern.Var(ident, loc).toSuccess
        case WeededAst.Pattern.Lit(literal, loc) => Literal.resolve(literal, namespace, syms) map {
          case lit => ResolvedAst.Pattern.Lit(lit, loc)
        }
        case WeededAst.Pattern.Tag(name, ident, wpat, loc) => syms.lookupEnum(name, namespace) flatMap {
          case (rname, enum) => visit(wpat) map {
            case pat => ResolvedAst.Pattern.Tag(rname, ident, pat, loc)
          }
        }
        case WeededAst.Pattern.Tuple(welms, loc) => @@(welms map (e => resolve(e, namespace, syms))) map {
          case elms => ResolvedAst.Pattern.Tuple(elms, loc)
        }
      }
      visit(wast)
    }
  }

  object Predicate {

    object Head {
      /**
       * Performs symbol resolution in the given head predicate `wast` in the given `namespace` with the given symbol table `syms`.
       */
      def resolve(wast: WeededAst.Predicate.Head, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Predicate.Head, ResolverError] = {
        val WeededAst.Predicate.Head(name, wterms, loc) = wast

        syms.lookupRelation(name, namespace) flatMap {
          case (rname, defn) => @@(wterms map (term => Term.Head.resolve(term, namespace, syms))) map {
            case terms => ResolvedAst.Predicate.Head(rname, terms, loc)
          }
        }
      }

    }

    object Body {
      /**
       * Performs symbol resolution in the given body predicate `wast` in the given `namespace` with the given symbol table `syms`.
       */
      def resolve(wast: WeededAst.Predicate.Body, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Predicate.Body, ResolverError] = {
        val WeededAst.Predicate.Body(name, wterms, loc) = wast

        syms.lookupRelation(name, namespace) flatMap {
          case (rname, defn) => @@(wterms map (term => Term.Body.resolve(term, namespace, syms))) map {
            case terms => ResolvedAst.Predicate.Body(rname, terms, loc)
          }
        }
      }
    }

  }

  object Term {

    object Head {

      /**
       * Performs symbol resolution in the given head term `wast` under the given `namespace`.
       */
      def resolve(wast: WeededAst.Term.Head, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Term.Head, ResolverError] = wast match {
        case WeededAst.Term.Head.Var(ident, loc) => ResolvedAst.Term.Head.Var(ident, loc).toSuccess
        case WeededAst.Term.Head.Lit(wlit, loc) => Literal.resolve(wlit, namespace, syms) map {
          case lit => ResolvedAst.Term.Head.Lit(lit, loc)
        }
        case WeededAst.Term.Head.Ascribe(wterm, wtpe, loc) =>
          @@(resolve(wterm, namespace, syms), Type.resolve(wtpe, namespace, syms)) map {
            case (term, tpe) => ResolvedAst.Term.Head.Ascribe(term, tpe, loc)
          }
        case WeededAst.Term.Head.Apply(name, wargs, loc) =>
          syms.lookupConstant(name, namespace) flatMap {
            case (rname, defn) => @@(wargs map (arg => resolve(arg, namespace, syms))) map {
              case args => ResolvedAst.Term.Head.Apply(rname, args.toList, loc)
            }
          }
      }
    }

    object Body {

      /**
       * Performs symbol resolution in the given body term `wast` under the given `namespace`.
       */
      def resolve(wast: WeededAst.Term.Body, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Term.Body, ResolverError] = wast match {
        case WeededAst.Term.Body.Wildcard(loc) => ResolvedAst.Term.Body.Wildcard(loc).toSuccess
        case WeededAst.Term.Body.Var(ident, loc) => ResolvedAst.Term.Body.Var(ident, loc).toSuccess
        case WeededAst.Term.Body.Lit(wlit, loc) => Literal.resolve(wlit, namespace, syms) map {
          case lit => ResolvedAst.Term.Body.Lit(lit, loc)
        }
        case WeededAst.Term.Body.Ascribe(wterm, wtpe, loc) =>
          @@(resolve(wterm, namespace, syms), Type.resolve(wtpe, namespace, syms)) map {
            case (term, tpe) => ResolvedAst.Term.Body.Ascribe(term, tpe, loc)
          }
      }
    }

  }

  object Type {

    /**
     * Performs symbol resolution in the given type `wast` under the given `namespace`.
     */
    def resolve(wast: WeededAst.Type, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Type, ResolverError] = {
      def visit(wast: WeededAst.Type): Validation[ResolvedAst.Type, ResolverError] = wast match {
        case WeededAst.Type.Unit => ResolvedAst.Type.Unit.toSuccess
        case WeededAst.Type.Ambiguous(name) => name.parts match {
          case Seq("Bool") => ResolvedAst.Type.Bool.toSuccess
          case Seq("Int") => ResolvedAst.Type.Int.toSuccess
          case Seq("Str") => ResolvedAst.Type.Str.toSuccess
          case _ => syms.lookupType(name, namespace) flatMap (tpe => visit(tpe))
        }
        case WeededAst.Type.Tag(tagName, tpe) =>
          visit(tpe) map (t => ResolvedAst.Type.Tag(Name.Resolved(namespace), tagName, t))
        case WeededAst.Type.Enum(name, wcases) =>
          val casesVal = Validation.fold(wcases) {
            case (k, v) => resolve(v, name.parts, syms) map (tpe => k -> tpe.asInstanceOf[ResolvedAst.Type.Tag])
          }
          casesVal map ResolvedAst.Type.Enum
        case WeededAst.Type.Tuple(welms) => @@(welms map (e => resolve(e, namespace, syms))) map ResolvedAst.Type.Tuple
        case WeededAst.Type.Function(wargs, wretType) =>
          val argsVal = @@(wargs map visit)
          val retTypeVal = visit(wretType)

          @@(argsVal, retTypeVal) map {
            case (args, retTpe) => ResolvedAst.Type.Function(args, retTpe)
          }
      }

      visit(wast)
    }
  }


  // TODO: Need this?
  def toRName(ident: Name.Ident, namespace: List[String]): Name.Resolved =
    Name.Resolved(namespace ::: ident.name :: Nil)

}
