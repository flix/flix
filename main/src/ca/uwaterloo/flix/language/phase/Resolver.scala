/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
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
      * @param sym  the symbol.
      * @param loc1 the location of the first definition.
      * @param loc2 the location of the second definition.
      */
    case class DuplicateDefinition(sym: Any /* TODO: Type */ , loc1: SourceLocation, loc2: SourceLocation) extends ResolverError {
      val message =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
            |${consoleCtx.red(s">> Duplicate definition of the name '$sym'.")}
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
    // TODO: Rename to illegal table name.
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
    case class UnresolvedReference(name: Name.QName, namespace: List[String]) extends ResolverError {
      val message: String = s"Error: Unresolved reference to '$name' in namespace '${namespace.mkString("::")}' at: ${name.loc.format}\n"
    }

    /**
      * An error raised to indicate a reference to an unknown constant.
      *
      * @param name      the unresolved name.
      * @param namespace the current namespace.
      * @param loc       the source location of the reference.
      */
    case class UnresolvedConstantReference(name: Name.QName, namespace: List[String], loc: SourceLocation) extends ResolverError {
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
    case class UnresolvedEnumReference(name: Name.QName, namespace: List[String], loc: SourceLocation) extends ResolverError {
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
    case class UnresolvedTagReference(enum: WeededAst.Declaration.Enum, tag: String, loc: SourceLocation) extends ResolverError {
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
    case class UnresolvedRelationReference(name: Name.QName, namespace: List[String], loc: SourceLocation) extends ResolverError {
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
    case class UnresolvedTypeReference(name: Name.QName, namespace: List[String], loc: SourceLocation) extends ResolverError {
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
    def empty(hooks: Map[Symbol.Resolved, Ast.Hook]) = SymbolTable(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, hooks)
  }

  // TODO: Come up with a SymbolTable that can give the set of definitions in each namespace.


  case class SymbolTable(enums: Map[Symbol.Resolved, WeededAst.Declaration.Enum],
                         constants: Map[Symbol.Resolved, WeededAst.Declaration.Definition],
                         lattices: Map[Type, (List[String], WeededAst.Declaration.BoundedLattice)],
                         tables: Map[Symbol.TableSym, WeededAst.Table],
                         indexes: Map[Symbol.TableSym, WeededAst.Declaration.Index],
                         types: Map[Symbol.Resolved, Type],
                         hooks: Map[Symbol.Resolved, Ast.Hook]) {

    // TODO: Cleanup
    def lookupConstant(name: Name.QName, namespace: List[String]): Validation[(Symbol.Resolved, Either[WeededAst.Declaration.Definition, Ast.Hook]), ResolverError] = {
      val rname = Symbol.Resolved.mk(
        if (!name.isQualified)
          namespace ::: name.ident.name :: Nil
        else
          name.namespace.idents.map(_.name) ::: name.ident.name :: Nil
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
    @deprecated
    def lookupEnum(name: Name.QName, namespace: List[String]): Validation[(Symbol.Resolved, WeededAst.Declaration.Enum), ResolverError] = {
      val rname = Symbol.Resolved.mk(
        if (!name.isQualified)
          namespace ::: name.ident.name :: Nil
        else
          name.namespace.idents.map(_.name) ::: name.ident.name :: Nil
      )
      enums.get(rname) match {
        case None => UnresolvedEnumReference(name, namespace, name.loc).toFailure
        case Some(d) => (rname, d).toSuccess
      }
    }

    def lookupTable(ident: Name.Ident, namespace: List[String]): Validation[(Symbol.TableSym, WeededAst.Table), ResolverError] = {
      val sym = new Symbol.TableSym(namespace, ident.name, ident.loc)
      tables.get(sym) match {
        case None => ??? //UnresolvedRelationReference(ident, namespace, name.loc).toFailure
        case Some(d) => (sym, d).toSuccess
      }
    }

    // TODO: Cleanup
    // TODO: Rename: lookupCollection
    def lookupTable(name: Name.QName, namespace: List[String]): Validation[(Symbol.TableSym, WeededAst.Table), ResolverError] = {
      val ns = if (!name.isQualified)
        namespace
      else
        name.namespace.idents.map(_.name)

      val sym = new Symbol.TableSym(ns, name.ident.name, name.loc)
      tables.get(sym) match {
        case None => UnresolvedRelationReference(name, namespace, name.loc).toFailure
        case Some(d) => (sym, d).toSuccess
      }
    }

    // TODO: Cleanup
    def lookupType(name: Name.QName, namespace: List[String]): Validation[Type, ResolverError] = {
      val rname = Symbol.Resolved.mk(
        if (!name.isQualified)
          namespace ::: name.ident.name :: Nil
        else
          name.namespace.idents.map(_.name) ::: name.ident.name :: Nil
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
  def resolve(wast: WeededAst.Program): Validation[ResolvedAst.Root, ResolverError] = {
    val b = System.nanoTime()

    // TODO: Check that hooks do not overlap with any names in the program.

    val decls = wast.roots.flatMap(_.decls)

    // TODO: Can anyone actually understand this: ??
    val symsVal = Validation.fold[WeededAst.Declaration, SymbolTable, ResolverError](decls, SymbolTable.empty(wast.hooks)) {
      case (msyms, d) => Declaration.symbolsOf(d, List.empty, msyms)
    }

    symsVal flatMap {
      case syms =>

        val collectedConstants = Validation.fold[Symbol.Resolved, WeededAst.Declaration.Definition, Symbol.Resolved, ResolvedAst.Definition.Constant, ResolverError](syms.constants) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

        val collectedEnums = Validation.fold[Symbol.Resolved, WeededAst.Declaration.Enum, Symbol.Resolved, ResolvedAst.Definition.Enum, ResolverError](syms.enums) {
          case (k, v) => Definition.resolve(v, k.parts.dropRight(1), syms) map (d => k -> d)
        }

        val collectedLattices = Validation.fold[Type, (List[String], WeededAst.Declaration.BoundedLattice), Type, ResolvedAst.Definition.BoundedLattice, ResolverError](syms.lattices) {
          case (k, (namespace, v)) => Types.resolve(k, namespace, syms) flatMap {
            case tpe => Definition.resolve(v, namespace, syms) map (d => tpe -> d)
          }
        }

        val collectionsVal = Validation.fold[Symbol.TableSym, WeededAst.Table, Symbol.TableSym, ResolvedAst.Table, ResolverError](syms.tables) {
          case (k, v) => Tables.resolve2(v, k.namespace, syms) map (d => k -> d)
        }

        val collectedIndexes = Validation.fold[Symbol.TableSym, WeededAst.Declaration.Index, Symbol.TableSym, ResolvedAst.Definition.Index, ResolverError](syms.indexes) {
          case (k, v) => Indexes.resolve(v, k.namespace, syms) map (d => k -> d)
        }

        val collectedFacts = @@(wast.roots.map(wast => Declaration.collectFacts(wast, syms)))
        val collectedRules = @@(wast.roots.map(wast => Declaration.collectRules(wast, syms)))

        @@(collectedConstants, collectedEnums, collectedLattices, collectionsVal, collectedIndexes, collectedFacts, collectedRules) map {
          case (constants, enums, lattices, collections, indexes, facts, rules) =>
            val e = System.nanoTime()
            ResolvedAst.Root(constants, enums, lattices, collections, indexes, facts.flatten, rules.flatten, wast.hooks, wast.time.copy(resolver = e - b))
        }
    }
  }

  object Declaration {

    /**
      * Constructs the symbol table for the given definition of `wast`.
      */
    def symbolsOf(wast: WeededAst.Declaration, namespace: List[String], syms: SymbolTable): Validation[SymbolTable, ResolverError] = wast match {
      case WeededAst.Declaration.Namespace(Name.NName(sp1, parts, sp2), body, loc) =>
        Validation.fold[WeededAst.Declaration, SymbolTable, ResolverError](body, syms) {
          case (msyms, d) => symbolsOf(d, namespace ::: parts.map(_.name), msyms)
        }
      case WeededAst.Declaration.Fact(head, loc) => syms.toSuccess
      case WeededAst.Declaration.Rule(head, body, loc) => syms.toSuccess

      case defn@WeededAst.Declaration.Definition(ann, ident, formals, tpe, e, loc) =>
        val rname = toRName(ident, namespace)
        syms.constants.get(rname) match {
          case None =>
            if (ident.name.head.isUpper)
              IllegalConstantName(ident.name, ident.loc).toFailure
            else
              syms.copy(constants = syms.constants + (rname -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Declaration.Enum(ident, cases, loc) =>
        val rname = toRName(ident, namespace)
        val cases = defn.cases.map { case (k, WeededAst.Case(_, tag, tpe)) => k -> Type.Tag(rname, tag, tpe) }
        syms.enums.get(rname) match {
          case None => syms.copy(
            enums = syms.enums + (rname -> defn),
            types = syms.types + (rname -> Type.Enum(rname, cases))
          ).toSuccess
          case Some(otherDefn) => DuplicateDefinition(rname, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        syms.copy(lattices = syms.lattices + (tpe ->(namespace, defn))).toSuccess

      case defn@WeededAst.Declaration.Index(ident, indexes, loc) =>
        val sym = new Symbol.TableSym(namespace, ident.name, loc)

        syms.indexes.get(sym) match {
          case None =>
            if (ident.name.head.isLower)
              IllegalRelationName(ident.name, ident.loc).toFailure // TODO: Rename error
            else
              syms.copy(indexes = syms.indexes + (sym -> defn)).toSuccess
          case Some(otherDefn) => throw new RuntimeException() // TODO
        }

      case t: WeededAst.Table => symbolsOf2(t, namespace, syms)
      case _ => ???
    }

    def symbolsOf2(wast: WeededAst.Table, namespace: List[String], syms: SymbolTable): Validation[SymbolTable, ResolverError] = wast match {
      case defn@WeededAst.Table.Relation(ident, attributes, loc) =>
        val sym = new Symbol.TableSym(namespace, ident.name, loc)
        syms.tables.get(sym) match {
          case None =>
            if (ident.name.head.isLower)
              IllegalRelationName(ident.name, ident.loc).toFailure
            else
              syms.copy(tables = syms.tables + (sym -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(sym, otherDefn.ident.loc, ident.loc).toFailure
        }

      case defn@WeededAst.Table.Lattice(ident, keys, values, loc) =>
        val sym = new Symbol.TableSym(namespace, ident.name, loc)
        syms.tables.get(sym) match {
          case None =>
            if (ident.name.head.isLower)
              IllegalRelationName(ident.name, ident.loc).toFailure
            else
              syms.copy(tables = syms.tables + (sym -> defn)).toSuccess
          case Some(otherDefn) => DuplicateDefinition(sym, otherDefn.ident.loc, ident.loc).toFailure
        }
    }

    def collectFacts(wast: WeededAst.Root, syms: SymbolTable): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = {
      def visit(wast: WeededAst.Declaration, namespace: List[String]): Validation[List[ResolvedAst.Constraint.Fact], ResolverError] = wast match {
        case WeededAst.Declaration.Namespace(ns, body, loc) =>
          @@(body map (d => visit(d, namespace ::: ns.idents.map(_.name)))) map (xs => xs.flatten)
        case fact: WeededAst.Declaration.Fact => Constraint.resolve(fact, namespace, syms) map (f => List(f))
        case _ => List.empty[ResolvedAst.Constraint.Fact].toSuccess
      }

      @@(wast.decls map (d => visit(d, List.empty))) map (xs => xs.flatten)
    }

    def collectRules(wast: WeededAst.Root, syms: SymbolTable): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = {
      def visit(wast: WeededAst.Declaration, namespace: List[String]): Validation[List[ResolvedAst.Constraint.Rule], ResolverError] = wast match {
        case WeededAst.Declaration.Namespace(ns, body, loc) =>
          @@(body map (d => visit(d, namespace ::: ns.idents.map(_.name)))) map (xs => xs.flatten)
        case rule: WeededAst.Declaration.Rule => Constraint.resolve(rule, namespace, syms) map (r => List(r))
        case _ => List.empty[ResolvedAst.Constraint.Rule].toSuccess
      }

      @@(wast.decls map (d => visit(d, List.empty))) map (xs => xs.flatten)
    }
  }

  object Definition {

    /**
      * Performs symbol resolution for the given value definition `wast`.
      */
    // TODO: Pattern match on wast?
    def resolve(wast: WeededAst.Declaration.Definition, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Constant, ResolverError] = {
      val name = Symbol.Resolved.mk(namespace ::: wast.ident.name :: Nil)

      val locals = wast.params.map(_.ident.name).toSet

      @@(Expression.resolve(wast.exp, namespace, syms, locals), Types.resolve(wast.tpe, namespace, syms)) flatMap {
        case (e, tpe) =>
          val formalsVal = wast.params.map {
            case Ast.FormalParam(ident, tpe) => Types.resolve(tpe, namespace, syms) map {
              case t => ResolvedAst.FormalArg(ident, t)
            }
          }
          @@(formalsVal) map {
            case formals => ResolvedAst.Definition.Constant(wast.ann, name, formals, e, tpe, wast.loc)
          }
      }
    }

    // TODO: Pattern match on wast?
    def resolve(wast: WeededAst.Declaration.Enum, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Enum, ResolverError] = {
      val name = Symbol.Resolved.mk(namespace ::: wast.ident.name :: Nil)

      val casesVal = Validation.fold[String, WeededAst.Case, String, Type.Tag, ResolverError](wast.cases) {
        case (k, WeededAst.Case(_, tag, wtpe)) => Types.resolve(wtpe, namespace, syms) map {
          case tpe => k -> Type.Tag(name, tag, tpe)
        }
      }

      casesVal map {
        case cases => ResolvedAst.Definition.Enum(name, cases, wast.loc)
      }
    }

    def resolve(wast: WeededAst.Declaration.BoundedLattice, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.BoundedLattice, ResolverError] = {
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


  }

  object Tables {

    def resolve2(wast: WeededAst.Table, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Table, ResolverError] = wast match {
      case d: WeededAst.Table.Relation => resolve2(d, namespace, syms)
      case d: WeededAst.Table.Lattice => resolve2(d, namespace, syms)
    }

    def resolve2(wast: WeededAst.Table.Relation, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Table.Relation, ResolverError] = {
      val symVal = syms.lookupTable(wast.ident, namespace)

      val attributesVal = wast.attr.map {
        case Ast.Attribute(ident, tpe) =>
          Types.resolve(tpe, namespace, syms) map (t => ResolvedAst.Attribute(ident, t))
      }

      @@(symVal, @@(attributesVal)) map {
        case ((sym, table), attributes) => ResolvedAst.Table.Relation(sym, attributes, wast.loc)
      }
    }

    def resolve2(wast: WeededAst.Table.Lattice, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Table.Lattice, ResolverError] = {
      val symVal = syms.lookupTable(wast.ident, namespace)

      val keysVal = wast.keys.map {
        case Ast.Attribute(ident, tpe) => Types.resolve(tpe, namespace: List[String], syms) map (t => ResolvedAst.Attribute(ident, t))
      }

      val valueVal = Types.resolve(wast.value.tpe, namespace: List[String], syms) map {
        case t => ResolvedAst.Attribute(wast.value.ident, t)
      }

      @@(symVal, @@(keysVal), valueVal) map {
        case ((sym, table), keys, value) => ResolvedAst.Table.Lattice(sym, keys, value, wast.loc)
      }
    }
  }

  object Indexes {

    def resolve(wast: WeededAst.Declaration.Index, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Definition.Index, ResolverError] = {
      syms.lookupTable(wast.ident, namespace) map {
        case (sym, table) => ResolvedAst.Definition.Index(sym, wast.indexes, wast.loc)
      }
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
        case WeededAst.Literal.True(loc) => ResolvedAst.Literal.Bool(true, loc).toSuccess
        case WeededAst.Literal.False(loc) => ResolvedAst.Literal.Bool(false, loc).toSuccess
        case WeededAst.Literal.Char(c, loc) => ResolvedAst.Literal.Char(c, loc).toSuccess
        case WeededAst.Literal.Float32(f, loc) => ResolvedAst.Literal.Float32(f, loc).toSuccess
        case WeededAst.Literal.Float64(f, loc) => ResolvedAst.Literal.Float64(f, loc).toSuccess
        case WeededAst.Literal.Int8(i, loc) => ResolvedAst.Literal.Int8(i, loc).toSuccess
        case WeededAst.Literal.Int16(i, loc) => ResolvedAst.Literal.Int16(i, loc).toSuccess
        case WeededAst.Literal.Int32(i, loc) => ResolvedAst.Literal.Int32(i, loc).toSuccess
        case WeededAst.Literal.Int64(i, loc) => ResolvedAst.Literal.Int64(i, loc).toSuccess
        case WeededAst.Literal.BigInt(i, loc) => ResolvedAst.Literal.BigInt(i, loc).toSuccess
        case WeededAst.Literal.Str(s, loc) => ResolvedAst.Literal.Str(s, loc).toSuccess
      }

      visit(wast)
    }
  }

  object Expression {

    /**
      * Performs symbol resolution in the given expression `wast` under the given `namespace`.
      */
    def resolve(wast: WeededAst.Expression, namespace: List[String], syms: SymbolTable, locals: Set[String] = Set.empty): Validation[ResolvedAst.Expression, ResolverError] = {
      def visit(wast: WeededAst.Expression, locals: Set[String]): Validation[ResolvedAst.Expression, ResolverError] = wast match {
        case WeededAst.Expression.Wild(loc) => ???
        case WeededAst.Expression.Unit(loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit(loc), loc).toSuccess
        case WeededAst.Expression.True(loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, loc), loc).toSuccess
        case WeededAst.Expression.False(loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(false, loc), loc).toSuccess
        case WeededAst.Expression.Char(c, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Char(c, loc), loc).toSuccess
        case WeededAst.Expression.Float32(f, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Float32(f, loc), loc).toSuccess
        case WeededAst.Expression.Float64(f, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Float64(f, loc), loc).toSuccess
        case WeededAst.Expression.Int8(i, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int8(i, loc), loc).toSuccess
        case WeededAst.Expression.Int16(i, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int16(i, loc), loc).toSuccess
        case WeededAst.Expression.Int32(i, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int32(i, loc), loc).toSuccess
        case WeededAst.Expression.Int64(i, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int64(i, loc), loc).toSuccess
        case WeededAst.Expression.BigInt(i, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.BigInt(i, loc), loc).toSuccess
        case WeededAst.Expression.Str(s, loc) => ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str(s, loc), loc).toSuccess

        case WeededAst.Expression.Var(name, loc) =>
          if (!name.isQualified && locals.contains(name.ident.name))
            ResolvedAst.Expression.Var(Name.Ident(name.sp1, name.ident.name, name.sp2), loc).toSuccess
          else
            syms.lookupConstant(name, namespace) map {
              case (rname, Left(defn)) => ResolvedAst.Expression.Ref(rname, loc)
              case (rname, Right(hook)) => ResolvedAst.Expression.HookRef(hook, loc)
            }

        case WeededAst.Expression.Apply(wlambda, wargs, loc) =>
          val lambdaVal = visit(wlambda, locals)
          val argsVal = @@(wargs map {
            case actual => visit(actual, locals)
          })

          @@(lambdaVal, argsVal) map {
            case (lambda, args) => ResolvedAst.Expression.Apply(lambda, args, loc)
          }

        case WeededAst.Expression.Lambda(wformals, wbody, loc) => ???

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
              val bound = locals ++ freeVars(rulePat)
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

        case WeededAst.Expression.FSet(welms, loc) => @@(welms map (e => visit(e, locals))) map {
          case elms => ResolvedAst.Expression.Set(elms, loc)
        }

        case WeededAst.Expression.Ascribe(we, wtype, loc) =>
          @@(visit(we, locals), Types.resolve(wtype, namespace, syms)) map {
            case (e, tpe) => ResolvedAst.Expression.Ascribe(e, tpe, loc)
          }

        case WeededAst.Expression.UserError(loc) => ResolvedAst.Expression.Error(Type.Any, loc).toSuccess // TODO: type

        case _: WeededAst.Expression.FNone => ???
        case _: WeededAst.Expression.FSome => ???
        case _: WeededAst.Expression.FNil => ???
        case _: WeededAst.Expression.FList => ???
        case _: WeededAst.Expression.FVec => ???
        case _: WeededAst.Expression.FMap => ???
        case _: WeededAst.Expression.GetIndex => ???
        case _: WeededAst.Expression.PutIndex => ???
        case _: WeededAst.Expression.Existential => ???
        case _: WeededAst.Expression.Universal => ???

      }

      visit(wast, locals)
    }

  }

  object Pattern {

    /**
      * Performs symbol resolution in the given pattern `wast` under the given `namespace`.
      */
    def resolve(wast: WeededAst.Pattern, namespace: List[String], syms: SymbolTable): Validation[ResolvedAst.Pattern, ResolverError] = {
      def visit(wast: WeededAst.Pattern): Validation[ResolvedAst.Pattern, ResolverError] = wast match {
        case WeededAst.Pattern.Wild(location) => ResolvedAst.Pattern.Wildcard(location).toSuccess
        case WeededAst.Pattern.Var(ident, loc) =>
          if (ident.name.head.isLower)
            ResolvedAst.Pattern.Var(ident, loc).toSuccess
          else
            IllegalVariableName(ident.name, loc).toFailure

        case WeededAst.Pattern.Unit(loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Unit(loc), loc).toSuccess
        case WeededAst.Pattern.True(loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Bool(true, loc), loc).toSuccess
        case WeededAst.Pattern.False(loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Bool(false, loc), loc).toSuccess
        case WeededAst.Pattern.Char(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Char(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Float32(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Float32(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Float64(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Float64(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Int8(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Int8(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Int16(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Int16(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Int32(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Int32(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Int64(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Int64(lit, loc), loc).toSuccess
        case WeededAst.Pattern.BigInt(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.BigInt(lit, loc), loc).toSuccess
        case WeededAst.Pattern.Str(lit, loc) => ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Str(lit, loc), loc).toSuccess

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

        case _ => ??? // TODO

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
        case WeededAst.Predicate.Head.True(loc) => ResolvedAst.Predicate.Head.True(loc).toSuccess
        case WeededAst.Predicate.Head.False(loc) => ResolvedAst.Predicate.Head.False(loc).toSuccess
        case WeededAst.Predicate.Head.Table(name, wterms, loc) =>
          syms.lookupTable(name, namespace) flatMap {
            case (rname, defn) => @@(wterms map (t => Term.Head.resolve(t, namespace, syms))) map {
              case terms => ResolvedAst.Predicate.Head.Table(rname, terms, loc)
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

          if (name.ident.name.head.isUpper) {
            @@(syms.lookupTable(name, namespace), termsVal) map {
              case ((rname, defn), terms) => ResolvedAst.Predicate.Body.Table(rname, terms, loc)
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

        case WeededAst.Term.Head.Tag(enum, tag, t, loc) =>
          syms.lookupEnum(enum, namespace) flatMap {
            case (rname, defn) => resolve(t, namespace, syms) flatMap {
              case e =>
                val tags = defn.cases.keySet
                if (tags contains tag.name)
                  ResolvedAst.Term.Head.Tag(rname, tag, e, loc).toSuccess
                else
                  UnresolvedTagReference(defn, tag.name, loc).toFailure
            }
          }

        case WeededAst.Term.Head.Tuple(welms, loc) =>
          @@(welms map (e => resolve(e, namespace, syms))) map {
            case elms => ResolvedAst.Term.Head.Tuple(elms, loc)
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
        case WeededAst.Term.Body.Wild(loc) => ResolvedAst.Term.Body.Wildcard(loc).toSuccess
        case WeededAst.Term.Body.Var(ident, loc) =>
          if (ident.name.head.isLower)
            ResolvedAst.Term.Body.Var(ident, loc).toSuccess
          else
            IllegalVariableName(ident.name, loc).toFailure
        case WeededAst.Term.Body.Lit(wlit, loc) => Literal.resolve(wlit, namespace, syms) map {
          case lit => ResolvedAst.Term.Body.Lit(lit, loc)
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
        case Type.Unresolved(name) => name.ident.name match {
          case "Unit" => Type.Unit.toSuccess
          case "Bool" => Type.Bool.toSuccess
          case "Char" => Type.Char.toSuccess
          case "Float" => Type.Float64.toSuccess
          case "Float32" => Type.Float32.toSuccess
          case "Float64" => Type.Float64.toSuccess
          case "Int" => Type.Int32.toSuccess
          case "Int8" => Type.Int8.toSuccess
          case "Int16" => Type.Int16.toSuccess
          case "Int32" => Type.Int32.toSuccess
          case "Int64" => Type.Int64.toSuccess
          case "BigInt" => Type.BigInt.toSuccess
          case "Prop" => Type.Prop.toSuccess
          case "Native" => Type.Native.toSuccess
          case "Str" => Type.Str.toSuccess
          case _ => syms.lookupType(name, namespace) flatMap (tpe => visit(tpe))
        }
        case Type.Tag(_, tagName, tpe) =>
          visit(tpe) map (t => Type.Tag(Symbol.Resolved.mk(namespace), tagName, t))
        case Type.Enum(name, wcases) =>
          val casesVal = Validation.fold(wcases) {
            case (k, Type.Tag(_, tag, wtpe)) => resolve(wtpe, namespace, syms) map {
              case tpe => k -> Type.Tag(name, tag, tpe)
            }
          }
          casesVal map {
            case cases => Type.Enum(name, cases)
          }
        case Type.Tuple(welms) => @@(welms map (e => resolve(e, namespace, syms))) map {
          case Nil => Type.Unit
          case x :: Nil => x
          case xs => Type.Tuple(xs)
        }
        case Type.FSet(welms) =>
          resolve(welms, namespace, syms) map {
            case elms => Type.FSet(elms)
          }
        case Type.Lambda(wargs, wretType) =>
          val argsVal = @@(wargs map visit)
          val retTypeVal = visit(wretType)

          @@(argsVal, retTypeVal) map {
            case (args, retTpe) => Type.Lambda(args, retTpe)
          }
        case Type.Native => Type.Native.toSuccess // TODO: Dont give a name.
        case Type.Parametric(qname, welms) =>
          @@(welms map (e => resolve(e, namespace, syms))) map {
            case elms => qname.ident.name match {
              case "Set" => Type.FSet(elms.head)
            }
          }
        case _ => ??? // TODO
      }

      visit(wast)
    }
  }

  // TODO: Need this?
  def toRName(ident: Name.Ident, namespace: List[String]): Symbol.Resolved =
    Symbol.Resolved.mk(namespace ::: ident.name :: Nil)

  /**
    * Returns the set of free variables in the pattern `pat`.
    */
  def freeVars(pat: WeededAst.Pattern): Set[String] = pat match {
    case WeededAst.Pattern.Wild(_) => Set.empty
    case WeededAst.Pattern.Var(ident, _) => Set(ident.name)
    case WeededAst.Pattern.Unit(_) => Set.empty
    case WeededAst.Pattern.True(_) => Set.empty
    case WeededAst.Pattern.False(_) => Set.empty
    case WeededAst.Pattern.Char(_, _) => Set.empty
    case WeededAst.Pattern.Float32(_, _) => Set.empty
    case WeededAst.Pattern.Float64(_, _) => Set.empty
    case WeededAst.Pattern.Int8(_, _) => Set.empty
    case WeededAst.Pattern.Int16(_, _) => Set.empty
    case WeededAst.Pattern.Int32(_, _) => Set.empty
    case WeededAst.Pattern.Int64(_, _) => Set.empty
    case WeededAst.Pattern.BigInt(_, _) => Set.empty
    case WeededAst.Pattern.Str(_, _) => Set.empty
    case WeededAst.Pattern.Tag(_, _, p, _) => freeVars(p)
    case WeededAst.Pattern.Tuple(elms, _) => elms.foldLeft(Set.empty[String]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
    case WeededAst.Pattern.FNone(_) => Set.empty
    case WeededAst.Pattern.FSome(p, _) => freeVars(p)
    case WeededAst.Pattern.FNil(_) => Set.empty
    case WeededAst.Pattern.FList(hd, tl, _) => freeVars(hd) ++ freeVars(tl)
    case WeededAst.Pattern.FVec(elms, rest, _) =>
      elms.flatMap(freeVars).toSet ++ rest.map(freeVars).getOrElse(Set.empty)
    case WeededAst.Pattern.FSet(elms, rest, _) =>
      elms.flatMap(freeVars).toSet ++ rest.map(freeVars).getOrElse(Set.empty)
    case WeededAst.Pattern.FMap(elms, rest, _) =>
      elms.flatMap {
        case (key, value) => freeVars(key) ++ freeVars(value)
      }.toSet
  }
}
