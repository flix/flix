package ca.uwaterloo.flix.language.phase

import java.lang.reflect.{Field, Method, Modifier}

import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.language.ast.WeededAst.Root
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.misc.Levenshtein

// TODO: Rename to Namer?
//  TODO: Maybe this class should be split into two: Symbols and Namer.
object Resolver {

  import ResolverError._

  /**
    * A common super-type for resolver errors.
    */
  sealed trait ResolverError extends CompilationError

  object ResolverError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that the given `name` is used for multiple definitions.
      *
      * @param name the name.
      * @param loc1 the location of the first definition.
      * @param loc2 the location of the second definition.
      */
    case class DuplicateDefinition(name: Name.Resolved, loc1: SourceLocation, loc2: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate definition of the name '$name'.")}
           |
            |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the definitions.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the given `name` is illegal for a constant definition.
      *
      * @param name the invalid name.
      * @param loc  the location of the name.
      */
    case class IllegalConstantName(name: String, loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal uppercase name '$name'.")}
           |
           |${loc.underline}
           |A value or function definition must start with a lowercase letter.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the given `name` is illegal for a relation definition.
      *
      * @param name the invalid name.
      * @param loc  the location of the name.
      */
    // TODO: Rename to illegal collection name.
    case class IllegalRelationName(name: String, loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal lowercase name '$name'.")}
           |
           |${loc.underline}
           |A relation or lattice definition must start with an uppercase letter.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the given `name` is illegal as a variable name.
      *
      * @param name the invalid name.
      * @param loc  the location of the name.
      */
    case class IllegalVariableName(name: String, loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Illegal uppercase variable name '$name'.")}
           |
           |${loc.underline}
           |A variable name must start with a lowercase letter.
         """.stripMargin
    }

    /**
      * An error raised to indicate that the given `name` in the given `namespace` was not found.
      *
      * @param name      the unresolved name.
      * @param namespace the current namespace.
      */
    // TODO: Split this into multiple different versions:
    @deprecated("", "")
    case class UnresolvedReference(name: Name.Unresolved, namespace: List[String]) extends ResolverError {
      val message: String = s"Error: Unresolved reference to '$name' in namespace '${namespace.mkString("::")}' at: ${name.loc.format}\n"
    }

    /**
      * An error raised to indicate a reference to an unknown constant.
      *
      * @param name      the unresolved name.
      * @param namespace the current namespace.
      * @param loc       the source location of the reference.
      */
    case class UnresolvedConstantReference(name: Name.Unresolved, namespace: List[String], loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- REFERENCE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unresolved reference to constant '$name'.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate a reference to an unknown enum.
      *
      * @param name      the unresolved name.
      * @param namespace the current namespace.
      * @param loc       the source location of the reference.
      */
    case class UnresolvedEnumReference(name: Name.Unresolved, namespace: List[String], loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- REFERENCE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unresolved reference to enum '$name'.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate a reference to an unknown tag in an enum.
      *
      * @param enum the enum.
      * @param tag  the tag name.
      * @param loc  the source location of the reference.
      */
    case class UnresolvedTagReference(enum: WeededAst.Definition.Enum, tag: String, loc: SourceLocation) extends ResolverError {
      val message = {
        val tags = enum.cases.keySet
        val formattedTags = tags.map(t => "'" + t + "'").mkString(", ")
        s"""${consoleCtx.blue(s"-- REFERENCE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unresolved reference to tag '$tag'.")}
           |
            |${loc.underline}
           |${consoleCtx.green(s"Did you mean: '${Levenshtein.bestMatch(tag, tags).getOrElse("<<no suggestion>>")}' ?")}
           |
            |The enum '${enum.ident}' declares the tags: $formattedTags at '${enum.loc.format}'.
         """.stripMargin
      }
    }

    /**
      * An error raised to indicate a reference to an unknown relation.
      *
      * @param name      the unresolved name.
      * @param namespace the current namespace.
      * @param loc       the source location of the reference.
      */
    case class UnresolvedRelationReference(name: Name.Unresolved, namespace: List[String], loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- REFERENCE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unresolved reference to relation '$name'.")}
           |
            |${loc.underline}
         """.stripMargin
    }

    /**
      * An error raised to indicate a reference to an unknown type.
      *
      * @param name      the unresolved name.
      * @param namespace the current namespace.
      * @param loc       the source location of the reference.
      */
    case class UnresolvedTypeReference(name: Name.Unresolved, namespace: List[String], loc: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- REFERENCE ERROR -------------------------------------------------- ${loc.source.format}")}
           |
            |${consoleCtx.red(s">> Unresolved reference to type '$name'.")}
           |
            |${loc.underline}
         """.stripMargin
    }


    // TODO: All kinds of arity errors....
    // TODO: Cyclic stuff.

  }

  object SymbolTable {
    def empty(hooks: Map[Name.Resolved, Ast.Hook]) = SymbolTable(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, hooks)
  }

  // TODO: Come up with a SymbolTable that can give the set of definitions in each namespace.


  case class SymbolTable(enums: Map[Name.Resolved, WeededAst.Definition.Enum],
                         constants: Map[Name.Resolved, WeededAst.Definition.Constant],
                         lattices: Map[Type, (List[String], WeededAst.Definition.BoundedLattice)],
                         relations: Map[Name.Resolved, WeededAst.Collection],
                         indexes: Map[Name.Resolved, WeededAst.Definition.Index],
                         types: Map[Name.Resolved, Type],
                         hooks: Map[Name.Resolved, Ast.Hook]) {

    // TODO: Cleanup
    def lookupConstant(name: Name.Unresolved, namespace: List[String]): Validation[(Name.Resolved, Either[WeededAst.Definition.Constant, Ast.Hook]), ResolverError] = {
      val rname = Name.Resolved.mk(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      constants.get(rname) match {
        case None => {
          // lookup in hooks
          hooks.get(rname) match {
            case None => UnresolvedConstantReference(name, namespace, name.loc).toFailure
            case Some(hook) => (rname, Right(hook)).toSuccess
          }
        }
        case Some(d) => (rname, Left(d)).toSuccess
      }
    }

    // TODO: Cleanup
    def lookupEnum(name: Name.Unresolved, namespace: List[String]): Validation[(Name.Resolved, WeededAst.Definition.Enum), ResolverError] = {
      val rname = Name.Resolved.mk(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      enums.get(rname) match {
        case None => UnresolvedEnumReference(name, namespace, name.loc).toFailure
        case Some(d) => (rname, d).toSuccess
      }
    }

    // TODO: Cleanup
    // TODO: Rename: lookupCollection
    def lookupRelation(name: Name.Unresolved, namespace: List[String]): Validation[(Name.Resolved, WeededAst.Collection), ResolverError] = {
      val rname = Name.Resolved.mk(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      relations.get(rname) match {
        case None => UnresolvedRelationReference(name, namespace, name.loc).toFailure
        case Some(d) => (rname, d).toSuccess
      }
    }

    // TODO: Cleanup
    def lookupType(name: Name.Unresolved, namespace: List[String]): Validation[Type, ResolverError] = {
      val rname = Name.Resolved.mk(
        if (name.parts.size == 1)
          namespace ::: name.parts.head :: Nil
        else
          name.parts
      )
      types.get(rname) match {
        case None => UnresolvedTypeReference(name, namespace, name.loc).toFailure
        case Some(tpe) => tpe.toSuccess
      }
    }

  }

  // TODO: Introduce ResolvedSymbolTable


  /**
    * Resolves all symbols in the given AST `wast`.
    */
  def resolve(wast: WeededAst.Root): Validation[ResolvedAst.Root, ResolverError] = {
    val b = System.nanoTime()

    // TODO: Check that hooks do not overlap with any names in the program.

    // TODO: Can anyone actually understand this: ??
    val symsVal = Validation.fold[WeededAst.Declaration, SymbolTable, ResolverError](wast.declarations, SymbolTable.empty(wast.hooks)) {
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

        val collectedLattices = Validation.fold[Type, (List[String], WeededAst.Definition.BoundedLattice), Type, ResolvedAst.Definition.BoundedLattice, ResolverError](syms.lattices) {
          case (k, (namespace, v)) => Types.resolve(k, namespace, syms) flatMap {
            case tpe => Definition.resolve(v, namespace, syms) map (d => tpe -> d)
          }
        }

        val collectionsVal = Validation.fold[Name.Resolved, WeededAst.Collection, Name.Resolved, ResolvedAst.Collection, ResolverError](syms.relations) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

        val collectedIndexes = Validation.fold[Name.Resolved, WeededAst.Definition.Index, Name.Resolved, ResolvedAst.Definition.Index, ResolverError](syms.indexes) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

        val collectedFacts = Declaration.collectFacts(wast, syms)
        val collectedRules = Declaration.collectRules(wast, syms)

        @@(collectedConstants, collectedEnums, collectedLattices, collectionsVal, collectedIndexes, collectedFacts, collectedRules) map {
          case (constants, enums, lattices, collections, indexes, facts, rules) =>
            val e = System.nanoTime()
            ResolvedAst.Root(constants, enums, lattices, collections, indexes, facts, rules, wast.hooks, wast.time.copy(resolver = e - b))
        }
    }
  }

  object Declaration {

    /**
      * Constructs the symbol table for the given definition of `wast`.
      */
    def symbolsOf(wast: WeededAst.Declaration, namespace: List[String], syms: SymbolTable): Validation[SymbolTable, ResolverError] = wast match {
      case WeededAst.Declaration.Namespace(Name.Unresolved(sp1, parts, sp2), body, loc) =>
        Validation.fold[WeededAst.Declaration, SymbolTable, ResolverError](body, syms) {
          case (msyms, d) => symbolsOf(d, namespace ::: parts.toList, msyms)
        }
      case WeededAst.Declaration.Fact(head, loc) => syms.toSuccess
      case WeededAst.Declaration.Rule(head, body, loc) => syms.toSuccess
      case defn: WeededAst.Definition => symbolsOf(defn, namespace, syms)
    }

    /**
      * Constructs the symbol for the given definition `wast`.
      */
    def symbolsOf(wast: WeededAst.Definition, namespace: List[String], syms: SymbolTable): Validation[SymbolTable, ResolverError] = wast match {
      case defn@WeededAst.Definition.Constant(ident, tpe, e, loc) =>
        val rname = toRName(ident, namespace)
        syms.constants.get(rname) match {
          case None =>
            if (ident.name.head.isUpper)
              IllegalConstantName(ident.name, ident.loc).toFailure
            else
              syms.copy(constants = syms.constants + (rname -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Definition.Enum(ident, cases, loc) =>
        val rname = toRName(ident, namespace)
        val cases = defn.cases.map { case (k, Type.UnresolvedTag(_, tag, tpe)) => k -> Type.Tag(rname, tag, tpe) }
        syms.enums.get(rname) match {
          case None => syms.copy(
            enums = syms.enums + (rname -> defn),
            types = syms.types + (rname -> Type.Enum(rname, cases))
          ).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        syms.copy(lattices = syms.lattices + (tpe ->(namespace, defn))).toSuccess

      case defn@WeededAst.Collection.Relation(ident, attributes, loc) =>
        val rname = toRName(ident, namespace)
        syms.relations.get(rname) match {
          case None =>
            if (ident.name.head.isLower)
              IllegalRelationName(ident.name, ident.loc).toFailure
            else
              syms.copy(relations = syms.relations + (rname -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Collection.Lattice(ident, keys, values, loc) =>
        val rname = toRName(ident, namespace)
        syms.relations.get(rname) match {
          case None =>
            if (ident.name.head.isLower)
              IllegalRelationName(ident.name, ident.loc).toFailure
            else
              syms.copy(relations = syms.relations + (rname -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Definition.Index(ident, indexes, loc) =>
        val rname = toRName(ident, namespace)
        syms.indexes.get(rname) match {
          case None =>
            if (ident.name.head.isLower)
              IllegalRelationName(ident.name, ident.loc).toFailure
            else
              syms.copy(indexes = syms.indexes + (rname -> defn)).toSuccess
          case Some(otherDefn) => throw new RuntimeException() // TODO
        }


    }

    def collectFacts(wast: WeededAst.Root, syms: SymbolTable): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = {
      def visit(wast: WeededAst.Declaration, namespace: List[String]): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = wast match {
        case WeededAst.Declaration.Namespace(name, body, loc) =>
          @@(body map (d => visit(d, namespace ::: name.parts))) map (xs => xs.flatten)
        case fact: WeededAst.Declaration.Fact => Constraint.resolve(fact, namespace, syms) map (f => List(f))
        case _ => List.empty[ResolvedAst.Constraint.Fact].toSuccess
      }

      @@(wast.declarations map (d => visit(d, List.empty))) map (xs => xs.flatten)
    }

    def collectRules(wast: WeededAst.Root, syms: SymbolTable): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = {
      def visit(wast: WeededAst.Declaration, namespace: List[String]): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = wast match {
        case WeededAst.Declaration.Namespace(name, body, loc) =>
          @@(body map (d => visit(d, namespace ::: name.parts))) map (xs => xs.flatten)
        case rule: WeededAst.Declaration.Rule => Constraint.resolve(rule, namespace, syms) map (r => List(r))
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
      val name = Name.Resolved.mk(namespace ::: wast.ident.name :: Nil)

      @@(Expression.resolve(wast.e, namespace, syms), Types.resolve(wast.tpe, namespace, syms)) map {
        case (e, tpe) =>
          ResolvedAst.Definition.Constant(name, e, tpe, wast.loc)
      }
    }

    // TODO: Pattern match on wast?
    def resolve(wast: WeededAst.Definition.Enum, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Enum, ResolverError] = {
      val name = Name.Resolved.mk(namespace ::: wast.ident.name :: Nil)

      val casesVal = Validation.fold[String, Type.UnresolvedTag, String, Type.Tag, ResolverError](wast.cases) {
        case (k, Type.UnresolvedTag(_, tag, wtpe)) => Types.resolve(wtpe, namespace, syms) map {
          case tpe => k -> Type.Tag(name, tag, tpe)
        }
      }

      casesVal map {
        case cases => ResolvedAst.Definition.Enum(name, cases, wast.loc)
      }
    }

    def resolve(wast: WeededAst.Definition.BoundedLattice, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.BoundedLattice, ResolverError] = {
      val tpeVal = Types.resolve(wast.tpe, namespace, syms)
      val botVal = Expression.resolve(wast.bot, namespace, syms)
      val topVal = Expression.resolve(wast.top, namespace, syms)
      val leqVal = Expression.resolve(wast.leq, namespace, syms)
      val lubVal = Expression.resolve(wast.lub, namespace, syms)
      val glbVal = Expression.resolve(wast.glb, namespace, syms)

      @@(tpeVal, botVal, topVal, leqVal, lubVal, glbVal) map {
        case (tpe, bot, top, leq, lub, glb) => ResolvedAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, wast.loc)
      }
    }

    def resolve(wast: WeededAst.Collection, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Collection, ResolverError] = wast match {
      case d: WeededAst.Collection.Relation => resolve2(d, namespace, syms)
      case d: WeededAst.Collection.Lattice => resolve2(d, namespace, syms)
    }

    def resolve2(wast: WeededAst.Collection.Relation, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Collection.Relation, ResolverError] = {
      val name = Name.Resolved.mk(namespace ::: wast.ident.name :: Nil)

      val attributesVal = wast.attributes.map {
        case WeededAst.Attribute(ident, tpe, _) =>
          Types.resolve(tpe, namespace, syms) map (t => ResolvedAst.Attribute(ident, t))
      }

      @@(attributesVal) map {
        case attributes => ResolvedAst.Collection.Relation(name, attributes, wast.loc)
      }
    }

    def resolve2(wast: WeededAst.Collection.Lattice, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Collection.Lattice, ResolverError] = {
      val name = Name.Resolved.mk(namespace ::: wast.ident.name :: Nil)

      val keysVal = wast.keys.map {
        case WeededAst.Attribute(ident, tpe, _) => Types.resolve(tpe, namespace, syms) map (t => ResolvedAst.Attribute(ident, t))
      }

      val valuesVal = wast.values.map {
        case WeededAst.Attribute(ident, tpe, _) => Types.resolve(tpe, namespace, syms) map {
          case t => ResolvedAst.Attribute(ident, t)
        }
      }

      @@(@@(keysVal), @@(valuesVal)) map {
        case (keys, values) => ResolvedAst.Collection.Lattice(name, keys, values, wast.loc)
      }
    }

    def resolve(wast: WeededAst.Definition.Index, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Index, ResolverError] = {
      val name = Name.Resolved.mk(namespace ::: wast.ident.name :: Nil)
      // TODO: check that the attributes exists...

      ResolvedAst.Definition.Index(name, wast.indexes, wast.loc).toSuccess
    }

  }

  object Constraint {

    def resolve(wast: WeededAst.Declaration.Fact, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Constraint.Fact, ResolverError] = {
      Predicate.Head.resolve(wast.head, namespace, syms) map (p => ResolvedAst.Constraint.Fact(p))
    }

    def resolve(wast: WeededAst.Declaration.Rule, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Constraint.Rule, ResolverError] = {
      val headVal = Predicate.Head.resolve(wast.head, namespace, syms)
      val bodyVal = @@(wast.body map (p => Predicate.Body.resolve(p, namespace, syms)))
      @@(headVal, bodyVal) map {
        case (head, body) => ResolvedAst.Constraint.Rule(head, body)
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
        case WeededAst.Literal.Char(c, loc) => ResolvedAst.Literal.Char(c, loc).toSuccess
        case WeededAst.Literal.Int8(i, loc) => ResolvedAst.Literal.Int8(i, loc).toSuccess
        case WeededAst.Literal.Int16(i, loc) => ResolvedAst.Literal.Int16(i, loc).toSuccess
        case WeededAst.Literal.Int32(i, loc) => ResolvedAst.Literal.Int32(i, loc).toSuccess
        case WeededAst.Literal.Int64(i, loc) => ResolvedAst.Literal.Int64(i, loc).toSuccess
        case WeededAst.Literal.Str(s, loc) => ResolvedAst.Literal.Str(s, loc).toSuccess
        case WeededAst.Literal.Tag(enum, tag, literal, loc) => syms.lookupEnum(enum, namespace) flatMap {
          case (rname, defn) => visit(literal) flatMap {
            case l =>
              val tags = defn.cases.keySet
              if (tags contains tag.name)
                ResolvedAst.Literal.Tag(rname, tag, l, loc).toSuccess
              else
                UnresolvedTagReference(defn, tag.name, loc).toFailure
          }
        }
        case WeededAst.Literal.Tuple(welms, loc) => @@(welms map visit) map {
          case elms => ResolvedAst.Literal.Tuple(elms, loc)
        }
        case WeededAst.Literal.Set(welms, loc) => @@(welms map visit) map {
          case elms => ResolvedAst.Literal.Set(elms, loc)
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

        case WeededAst.Expression.Var(name, loc) => name.parts match {
          case Seq(x) if locals contains x => ResolvedAst.Expression.Var(Name.Ident(name.sp1, x, name.sp2), loc).toSuccess
          case _ => syms.lookupConstant(name, namespace) map {
            case (rname, Left(defn)) => ResolvedAst.Expression.Ref(rname, loc)
            case (rname, Right(hook)) => ResolvedAst.Expression.HookRef(hook, loc)
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

        case WeededAst.Expression.Lambda(annotations, wformals, wbody, wtype, loc) =>
          val formalsVal = @@(wformals map {
            case WeededAst.FormalArg(ident, tpe) => Types.resolve(tpe, namespace, syms) flatMap {
              case t =>
                if (ident.name.head.isLower)
                  ResolvedAst.FormalArg(ident, t).toSuccess
                else
                  IllegalVariableName(ident.name, ident.loc).toFailure
            }
          })

          formalsVal flatMap {
            case formals =>
              val bindings = formals map (_.ident.name)
              @@(Types.resolve(wtype, namespace, syms), visit(wbody, locals ++ bindings)) map {
                case (tpe, body) => ResolvedAst.Expression.Lambda(annotations, formals, tpe, body, loc)
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

        case WeededAst.Expression.Switch(wrules, loc) =>
          val result = wrules map {
            case (cond, body) => @@(visit(cond, locals), visit(body, locals))
          }
          @@(result) map {
            case rules => ResolvedAst.Expression.Switch(rules, loc)
          }

        case WeededAst.Expression.Let(ident, wvalue, wbody, loc) =>
          val valueVal = visit(wvalue, locals)
          val bodyVal = visit(wbody, locals + ident.name)
          @@(valueVal, bodyVal) flatMap {
            case (value, body) =>
              if (ident.name.head.isLower)
                ResolvedAst.Expression.Let(ident, value, body, loc).toSuccess
              else
                IllegalVariableName(ident.name, loc).toFailure
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

        case WeededAst.Expression.Tag(enum, tag, we, loc) =>
          syms.lookupEnum(enum, namespace) flatMap {
            case (rname, defn) => visit(we, locals) flatMap {
              case e =>
                val tags = defn.cases.keySet
                if (tags contains tag.name)
                  ResolvedAst.Expression.Tag(rname, tag, e, loc).toSuccess
                else
                  UnresolvedTagReference(defn, tag.name, loc).toFailure
            }
          }

        case WeededAst.Expression.Tuple(welms, loc) => @@(welms map (e => visit(e, locals))) map {
          case elms => ResolvedAst.Expression.Tuple(elms, loc)
        }

        case WeededAst.Expression.Set(welms, loc) => @@(welms map (e => visit(e, locals))) map {
          case elms => ResolvedAst.Expression.Set(elms, loc)
        }

        case WeededAst.Expression.Ascribe(we, wtype, loc) =>
          @@(visit(we, locals), Types.resolve(wtype, namespace, syms)) map {
            case (e, tpe) => ResolvedAst.Expression.Ascribe(e, tpe, loc)
          }

        case WeededAst.Expression.Error(wtype, loc) =>
          Types.resolve(wtype, namespace, syms) map {
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
        case WeededAst.Pattern.Var(ident, loc) =>
          if (ident.name.head.isLower)
            ResolvedAst.Pattern.Var(ident, loc).toSuccess
          else
            IllegalVariableName(ident.name, loc).toFailure
        case WeededAst.Pattern.Lit(literal, loc) => Literal.resolve(literal, namespace, syms) map {
          case lit => ResolvedAst.Pattern.Lit(lit, loc)
        }
        case WeededAst.Pattern.Tag(enum, tag, wpat, loc) => syms.lookupEnum(enum, namespace) flatMap {
          case (rname, defn) => visit(wpat) flatMap {
            case pat =>
              val tags = defn.cases.keySet
              if (tags contains tag.name)
                ResolvedAst.Pattern.Tag(rname, tag, pat, loc).toSuccess
              else
                UnresolvedTagReference(defn, tag.name, loc).toFailure
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
      def resolve(wast: WeededAst.Predicate.Head, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Predicate.Head, ResolverError] = wast match {
        // TODO: What if a function symbol occurs in the head?
        case WeededAst.Predicate.Head.Relation(name, wterms, loc) =>
          syms.lookupRelation(name, namespace) flatMap {
            case (rname, defn) => @@(wterms map (t => Term.Head.resolve(t, namespace, syms))) map {
              case terms => ResolvedAst.Predicate.Head.Relation(rname, terms, loc)
            }
          }
      }
    }

    object Body {
      /**
        * Performs symbol resolution in the given body predicate `wast` in the given `namespace` with the given symbol table `syms`.
        */
      def resolve(wast: WeededAst.Predicate.Body, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Predicate.Body, ResolverError] = wast match {
        case WeededAst.Predicate.Body.Ambiguous(name, wterms, loc) =>
          val termsVal = @@(wterms map (t => Term.Body.resolve(t, namespace, syms)))

          if (name.parts.last.head.isUpper) {
            @@(syms.lookupRelation(name, namespace), termsVal) map {
              case ((rname, defn), terms) => ResolvedAst.Predicate.Body.Relation(rname, terms, loc)
            }
          } else {
            @@(syms.lookupConstant(name, namespace), termsVal) map {
              case ((rname, Left(defn)), terms) => ResolvedAst.Predicate.Body.ApplyFilter(rname, terms, loc)
              case ((rname, Right(hook)), terms) => ResolvedAst.Predicate.Body.ApplyHookFilter(hook, terms, loc)
            }
          }

        case WeededAst.Predicate.Body.NotEqual(ident1, ident2, loc) =>
          ResolvedAst.Predicate.Body.NotEqual(ident1, ident2, loc).toSuccess

        case WeededAst.Predicate.Body.Loop(ident, term, loc) =>
          Term.Head.resolve(term, namespace, syms) map {
            case term => ResolvedAst.Predicate.Body.Loop(ident, term, loc)
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
        case WeededAst.Term.Head.Var(ident, loc) =>
          if (ident.name.head.isLower)
            ResolvedAst.Term.Head.Var(ident, loc).toSuccess
          else
            IllegalVariableName(ident.name, loc).toFailure
        case WeededAst.Term.Head.Lit(wlit, loc) => Literal.resolve(wlit, namespace, syms) map {
          case lit => ResolvedAst.Term.Head.Lit(lit, loc)
        }
        case WeededAst.Term.Head.Ascribe(wterm, wtpe, loc) =>
          @@(resolve(wterm, namespace, syms), Types.resolve(wtpe, namespace, syms)) map {
            case (term, tpe) => ResolvedAst.Term.Head.Ascribe(term, tpe, loc)
          }
        case WeededAst.Term.Head.Apply(name, wargs, loc) =>
          syms.lookupConstant(name, namespace) flatMap {
            case (rname, Left(defn)) => @@(wargs map (arg => resolve(arg, namespace, syms))) map {
              case args => ResolvedAst.Term.Head.Apply(rname, args.toList, loc)
            }
            case (rname, Right(hook)) => @@(wargs map (arg => resolve(arg, namespace, syms))) map {
              case args => ResolvedAst.Term.Head.ApplyHook(hook, args.toList, loc)
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
        case WeededAst.Term.Body.Var(ident, loc) =>
          if (ident.name.head.isLower)
            ResolvedAst.Term.Body.Var(ident, loc).toSuccess
          else
            IllegalVariableName(ident.name, loc).toFailure
        case WeededAst.Term.Body.Lit(wlit, loc) => Literal.resolve(wlit, namespace, syms) map {
          case lit => ResolvedAst.Term.Body.Lit(lit, loc)
        }
        case WeededAst.Term.Body.Ascribe(wterm, wtpe, loc) =>
          @@(resolve(wterm, namespace, syms), Types.resolve(wtpe, namespace, syms)) map {
            case (term, tpe) => ResolvedAst.Term.Body.Ascribe(term, tpe, loc)
          }
      }
    }

  }

  object Types {

    /**
      * Performs symbol resolution in the given type `wast` under the given `namespace`.
      */
    def resolve(wast: Type, namespace: List[String], syms: SymbolTable): Validation[Type, ResolverError] = {
      def visit(wast: Type): Validation[Type, ResolverError] = wast match {
        case Type.Unit => Type.Unit.toSuccess
        case Type.Unresolved(name) => name.parts match {
          case Seq("Bool") => Type.Bool.toSuccess
          case Seq("Char") => Type.Char.toSuccess
          case Seq("Int") => Type.Int32.toSuccess
          case Seq("Int8") => Type.Int8.toSuccess
          case Seq("Int16") => Type.Int16.toSuccess
          case Seq("Int32") => Type.Int32.toSuccess
          case Seq("Int64") => Type.Int64.toSuccess
          case Seq("Str") => Type.Str.toSuccess
          case _ => syms.lookupType(name, namespace) flatMap (tpe => visit(tpe))
        }
        case Type.Tag(_, tagName, tpe) =>
          visit(tpe) map (t => Type.Tag(Name.Resolved.mk(namespace), tagName, t))
        case Type.Enum(name, wcases) =>
          val casesVal = Validation.fold(wcases) {
            case (k, Type.Tag(_, tag, wtpe)) => resolve(wtpe, namespace, syms) map {
              case tpe => k -> Type.Tag(name, tag, tpe)
            }
          }
          casesVal map {
            case cases => Type.Enum(name, cases)
          }
        case Type.Tuple(welms) => @@(welms map (e => resolve(e, namespace, syms))) map Type.Tuple
        case Type.Set(welms) =>
          resolve(welms, namespace, syms) map {
            case elms => Type.Set(elms)
          }
        case Type.Lambda(wargs, wretType) =>
          val argsVal = @@(wargs map visit)
          val retTypeVal = visit(wretType)

          @@(argsVal, retTypeVal) map {
            case (args, retTpe) => Type.Lambda(args, retTpe)
          }
        case Type.Native => Type.Native.toSuccess // TODO: Dont give a name.
      }

      visit(wast)
    }
  }

  // TODO: Need this?
  def toRName(ident: Name.Ident, namespace: List[String]): Name.Resolved =
    Name.Resolved.mk(namespace ::: ident.name :: Nil)


}
