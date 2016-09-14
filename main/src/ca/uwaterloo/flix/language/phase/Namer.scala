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
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * The Namer phase introduces unique symbols for each syntactic entity in the program.
  */
object Namer {

  import NameError._

  /**
    * Introduces unique names for each syntactic entity in the given `program`.
    **/
  def namer(program: WeededAst.Program)(implicit genSym: GenSym): Validation[NamedAst.Program, NameError] = {
    // make an empty program to fold over.
    val prog0 = NamedAst.Program(
      enums = Map.empty,
      definitions = Map.empty,
      classes = Map.empty,
      impls = Map.empty,
      lattices = Map.empty,
      indexes = Map.empty,
      tables = Map.empty,
      facts = Nil,
      rules = Nil,
      hooks = program.hooks,
      time = program.time
    )

    // collect all the declarations.
    val declarations = program.roots.flatMap(_.decls)

    // fold over the top-level declarations.
    Validation.fold(declarations, prog0) {
      case (pacc, decl) => Declarations.namer(decl, Name.RootNS, pacc)
    }
  }

  object Declarations {

    /**
      * Performs naming on the given declaration `decl0` in the given namespace `ns0` under the given (partial) program `prog0`.
      */
    def namer(decl0: WeededAst.Declaration, ns0: Name.NName, prog0: NamedAst.Program)(implicit genSym: GenSym): Validation[NamedAst.Program, NameError] = decl0 match {
      /*
       * Namespace.
       */
      case WeededAst.Declaration.Namespace(ns, decls, loc) => Validation.fold(decls, prog0) {
        case (pacc, decl) => namer(decl, ns, pacc)
      }

      /*
       * Definition.
       */
      case WeededAst.Declaration.Definition(ann, ident, params0, exp, tpe, loc) =>
        // check if the name is legal.
        if (ident.name.head.isUpper) {
          return IllegalDefinitionName(ident.name, loc).toFailure
        }

        // check if the definition already exists.
        prog0.definitions.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the definition does not yet exist.

            // introduce a variable symbols for each formal parameter.
            var pms0 = List.empty[NamedAst.FormalParam]
            var env0 = Map.empty[String, Symbol.VarSym]
            for (Ast.FormalParam(ident, tpe) <- params0) {
              val sym = Symbol.mkVarSym(ident)
              pms0 = NamedAst.FormalParam(sym, tpe) :: pms0
              env0 = env0 + (ident.name -> sym)
            }

            Expressions.namer(exp, env0) map {
              case e =>
                val sym = Symbol.mkDefnSym(ns0, ident)
                val defn = NamedAst.Declaration.Definition(sym, pms0, e, ann, tpe, loc)
                val defns = Map(ident.name -> defn)
                prog0.copy(definitions = prog0.definitions + (ns0 -> defns))
            }
          case Some(defns0) =>
            // Case 2: The namespace exists. Lookup the definition.
            defns0.get(ident.name) match {
              case None =>
                // Case 2.1: The definition does not exist in the namespace. Update it.

                // introduce a variable symbols for each formal parameter.
                var pms0 = List.empty[NamedAst.FormalParam]
                var env0 = Map.empty[String, Symbol.VarSym]
                for (Ast.FormalParam(ident, tpe) <- params0) {
                  val sym = Symbol.mkVarSym(ident)
                  pms0 = NamedAst.FormalParam(sym, tpe) :: pms0
                  env0 = env0 + (ident.name -> sym)
                }

                Expressions.namer(exp, env0) map {
                  case e =>
                    val sym = Symbol.mkDefnSym(ns0, ident)
                    val defn = NamedAst.Declaration.Definition(sym, pms0, e, ann, tpe, loc)
                    val defns = defns0 + (ident.name -> defn)
                    prog0.copy(definitions = prog0.definitions + (ns0 -> defns))
                }
              case Some(defn) =>
                // Case 2.2: Duplicate definition.
                DuplicateDefinition(ident.name, defn.loc, ident.loc).toFailure
            }
        }

      /*
       * Signature.
       */
      case WeededAst.Declaration.Signature(ident, params, tpe, loc) => ??? // TODO

      /*
       * External.
       */
      case WeededAst.Declaration.External(ident, params, tpe, loc) => ??? // TODO

      /*
       * Law.
       */
      case WeededAst.Declaration.Law(ident, tparams, params, tpe, exp, loc) => ??? // TODO

      /*
       * Enum.
       */
      case WeededAst.Declaration.Enum(ident, cases, loc) =>
        // check if the enum already exists.
        prog0.enums.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the enum does not yet exist.
            val enum = NamedAst.Declaration.Enum(ident, casesOf(cases), loc)
            val enums = Map(ident.name -> enum)
            prog0.copy(enums = prog0.enums + (ns0 -> enums)).toSuccess
          case Some(enums0) =>
            // Case 2: The namespace exists. Lookup the enum.
            enums0.get(ident.name) match {
              case None =>
                // Case 2.1: The enum does not exist in the namespace. Update it.
                val enum = NamedAst.Declaration.Enum(ident, casesOf(cases), loc)
                val enums = enums0 + (ident.name -> enum)
                prog0.copy(enums = prog0.enums + (ns0 -> enums)).toSuccess
              case Some(enum) =>
                // Case 2.2: Duplicate definition.
                DuplicateDefinition(ident.name, enum.loc, ident.loc).toFailure
            }
        }

      /*
       * Class.
       */
      case WeededAst.Declaration.Class(ident, tparams, decls, loc) =>
        // check if the class already exists.
        val sym = Symbol.mkClassSym(ident)
        prog0.classes.get(sym) match {
          case None =>
            // Case 1: The class does not exist.
            ??? // TODO
          case Some(clazz) =>
            // Case 2: The class already exists.
            ??? // TODO
        }

      /*
       * Impl.
       */
      case WeededAst.Declaration.Impl(ident, tparams, decls, loc) => ??? // TODO

      /*
       * Fact.
       */
      case WeededAst.Declaration.Fact(h, loc) =>
        val head = Predicates.namer(h)
        val fact = NamedAst.Declaration.Fact(head, loc)
        prog0.copy(facts = fact :: prog0.facts).toSuccess

      /*
       * Rule.
       */
      case WeededAst.Declaration.Rule(h, b, loc) =>
        val head = Predicates.namer(h)
        // TODO: Introduces Predicates.namer for body predicates.
        val body = b map {
          case WeededAst.Predicate.Body.Ambiguous(name, terms, loc) => NamedAst.Predicate.Body.Ambiguous(name, terms, loc)
          case WeededAst.Predicate.Body.NotEqual(ident1, ident2, loc) => NamedAst.Predicate.Body.NotEqual(ident1, ident2, loc)
          case WeededAst.Predicate.Body.Loop(ident, term, loc) => NamedAst.Predicate.Body.Loop(ident, term, loc)
        }
        val rule = NamedAst.Declaration.Rule(head, body, loc)
        prog0.copy(rules = rule :: prog0.rules).toSuccess

      /*
       * Index.
       */
      case WeededAst.Declaration.Index(ident, indexes, loc) =>
        // check if the index already exists.
        val sym = Symbol.mkTableSym(ns0, ident)
        prog0.indexes.get(sym) match {
          case None =>
            // Case 1: No indexes exist for the table. Update the indexes.
            val index = NamedAst.Declaration.Index(ident, indexes.map(_.toList).toList, loc)
            prog0.copy(indexes = prog0.indexes + (sym -> index)).toSuccess
          case Some(index) =>
            // Case 2: Some indexes already exist for the table.
            ??? // TODO
        }

      /*
       * BoundedLattice (deprecated).
       */
      case WeededAst.Declaration.BoundedLattice(tpe, bot0, top0, leq0, lub0, glb0, loc) =>
        val botVal = Expressions.namer(bot0, Map.empty)
        val topVal = Expressions.namer(top0, Map.empty)
        val leqVal = Expressions.namer(leq0, Map.empty)
        val lubVal = Expressions.namer(lub0, Map.empty)
        val glbVal = Expressions.namer(glb0, Map.empty)

        @@(botVal, topVal, leqVal, lubVal, glbVal) map {
          case (bot, top, leq, lub, glb) =>
            val lattice = NamedAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, loc)
            prog0.copy(lattices = prog0.lattices + (tpe -> lattice)) // NB: This just overrides any existing binding.
        }

      /*
       * Relation.
       */
      case WeededAst.Table.Relation(ident, attr, loc) =>
        // check if the name is legal.
        if (ident.name.head.isLower) {
          return IllegalTableName(ident.name, loc).toFailure
        }

        // check if the table already exists.
        prog0.tables.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the table does not yet exist.
            val table = NamedAst.Table.Relation(Symbol.mkTableSym(ns0, ident), attr, loc)
            val tables = Map(ident.name -> table)
            prog0.copy(tables = prog0.tables + (ns0 -> tables)).toSuccess
          case Some(tables0) =>
            // Case 2: The namespace exists. Lookup the table.
            tables0.get(ident.name) match {
              case None =>
                // Case 2.1: The table does not exist in the namespace. Update it.
                val table = NamedAst.Table.Relation(Symbol.mkTableSym(ns0, ident), attr, loc)
                val tables = tables0 + (ident.name -> table)
                prog0.copy(tables = prog0.tables + (ns0 -> tables)).toSuccess
              case Some(table) =>
                // Case 2.2: Duplicate definition.
                DuplicateDefinition(ident.name, table.loc, ident.loc).toFailure
            }
        }

      /*
       * Lattice.
       */
      case WeededAst.Table.Lattice(ident, keys, value, loc) =>
        // check if the name is legal.
        if (ident.name.head.isLower) {
          return IllegalTableName(ident.name, loc).toFailure
        }

        // check if the table already exists.
        prog0.tables.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the table does not yet exist.
            val table = NamedAst.Table.Lattice(Symbol.mkTableSym(ns0, ident), keys, value, loc)
            val tables = Map(ident.name -> table)
            prog0.copy(tables = prog0.tables + (ns0 -> tables)).toSuccess
          case Some(tables0) =>
            // Case 2: The namespace exists. Lookup the table.
            tables0.get(ident.name) match {
              case None =>
                // Case 2.1: The table does not exist in the namespace. Update it.
                val table = NamedAst.Table.Lattice(Symbol.mkTableSym(ns0, ident), keys, value, loc)
                val tables = tables0 + (ident.name -> table)
                prog0.copy(tables = prog0.tables + (ns0 -> tables)).toSuccess
              case Some(table) =>
                // Case 2.2: Duplicate definition.
                DuplicateDefinition(ident.name, table.loc, ident.loc).toFailure
            }
        }

    }

    /**
      * Performs naming on the given `cases` map.
      */
    def casesOf(cases: Map[String, WeededAst.Case]): Map[String, NamedAst.Case] =
    cases.foldLeft(Map.empty[String, NamedAst.Case]) {
      case (macc, (name, WeededAst.Case(enum, tag, tpe))) => macc + (name -> NamedAst.Case(enum, tag, tpe))
    }

  }

  object Expressions {

    /**
      * Performs naming on the given expression `exp0` under the given environment `env0`.
      */
    def namer(exp0: WeededAst.Expression, env0: Map[String, Symbol.VarSym])(implicit genSym: GenSym): Validation[NamedAst.Expression, NameError] = exp0 match {
      /*
       * Variables.
       */
      case WeededAst.Expression.Wild(loc) => NamedAst.Expression.Wild(genSym.freshTypeVar(), loc).toSuccess

      case WeededAst.Expression.Var(name, loc) if name.isUnqualified =>
        // lookup the variable name in the environment.
        env0.get(name.ident.name) match {
          case None =>
            // Case 1: reference.
            NamedAst.Expression.Ref(name, genSym.freshTypeVar(), loc).toSuccess
          case Some(sym) =>
            // Case 2: variable.
            NamedAst.Expression.Var(sym, genSym.freshTypeVar(), loc).toSuccess
        }

      case WeededAst.Expression.Var(name, loc) =>
        NamedAst.Expression.Ref(name, genSym.freshTypeVar(), loc).toSuccess

      /*
       * Literals.
       */
      case WeededAst.Expression.Unit(loc) => NamedAst.Expression.Unit(loc).toSuccess
      case WeededAst.Expression.True(loc) => NamedAst.Expression.True(loc).toSuccess
      case WeededAst.Expression.False(loc) => NamedAst.Expression.False(loc).toSuccess
      case WeededAst.Expression.Char(lit, loc) => NamedAst.Expression.Char(lit, loc).toSuccess
      case WeededAst.Expression.Float32(lit, loc) => NamedAst.Expression.Float32(lit, loc).toSuccess
      case WeededAst.Expression.Float64(lit, loc) => NamedAst.Expression.Float64(lit, loc).toSuccess
      case WeededAst.Expression.Int8(lit, loc) => NamedAst.Expression.Int8(lit, loc).toSuccess
      case WeededAst.Expression.Int16(lit, loc) => NamedAst.Expression.Int16(lit, loc).toSuccess
      case WeededAst.Expression.Int32(lit, loc) => NamedAst.Expression.Int32(lit, loc).toSuccess
      case WeededAst.Expression.Int64(lit, loc) => NamedAst.Expression.Int64(lit, loc).toSuccess
      case WeededAst.Expression.BigInt(lit, loc) => NamedAst.Expression.BigInt(lit, loc).toSuccess
      case WeededAst.Expression.Str(lit, loc) => NamedAst.Expression.Str(lit, loc).toSuccess

      case WeededAst.Expression.Apply(lambda, args, loc) =>
        val lambdaVal = namer(lambda, env0)
        val argsVal = @@(args map (a => namer(a, env0)))
        @@(lambdaVal, argsVal) map {
          case (e, es) => NamedAst.Expression.Apply(e, es, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Lambda(params, exp, loc) =>
        // make a fresh variable symbol for each for parameter.
        val syms = params map (ident => Symbol.mkVarSym(ident))
        val env1 = (params zip syms) map {
          case (ident, sym) => ident.name -> sym
        }
        namer(exp, env0 ++ env1) map {
          case e => NamedAst.Expression.Lambda(syms, e, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Unary(op, exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Unary(op, e, genSym.freshTypeVar(), loc)
      }

      case WeededAst.Expression.Binary(op, exp1, exp2, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0)) map {
          case (e1, e2) => NamedAst.Expression.Binary(op, e1, e2, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0), namer(exp3, env0)) map {
          case (e1, e2, e3) => NamedAst.Expression.IfThenElse(e1, e2, e3, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Let(ident, exp1, exp2, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.mkVarSym(ident)
        @@(namer(exp1, env0), namer(exp2, env0 + (ident.name -> sym))) map {
          case (e1, e2) => NamedAst.Expression.Let(sym, e1, e2, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Match(exp, rules, loc) =>
        val expVal = namer(exp, env0)
        val rulesVal = rules map {
          case (pat, body) =>
            // extend the environment with every variable occurring in the pattern
            // and perform naming on the rule body under the extended environment.
            val (p, env1) = Patterns.namer(pat)
            namer(body, env0 ++ env1) map {
              case b => p -> b
            }
        }
        @@(expVal, @@(rulesVal)) map {
          case (e, rs) => NamedAst.Expression.Match(e, rs, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Switch(rules, loc) => @@(rules map {
        case (cond, body) => @@(namer(cond, env0), namer(body, env0))
      }) map {
        case rs => NamedAst.Expression.Switch(rs, genSym.freshTypeVar(), loc)
      }

      case WeededAst.Expression.Tag(enum, tag, exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Tag(enum, tag, e, genSym.freshTypeVar(), loc)
      }

      case WeededAst.Expression.Tuple(elms, loc) =>
        @@(elms map (e => namer(e, env0))) map {
          case es => NamedAst.Expression.Tuple(es, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.FNone(loc) => NamedAst.Expression.FNone(genSym.freshTypeVar(), loc).toSuccess

      case WeededAst.Expression.FSome(exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.FSome(e, genSym.freshTypeVar(), loc)
      }

      case WeededAst.Expression.FNil(loc) => NamedAst.Expression.FNil(genSym.freshTypeVar(), loc).toSuccess

      case WeededAst.Expression.FList(hd, tl, loc) =>
        @@(namer(hd, env0), namer(tl, env0)) map {
          case (e1, e2) => NamedAst.Expression.FList(e1, e2, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.FVec(elms, loc) =>
        @@(elms map (e => namer(e, env0))) map {
          case es => NamedAst.Expression.FVec(es, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.FSet(elms, loc) =>
        @@(elms map (e => namer(e, env0))) map {
          case es => NamedAst.Expression.FSet(es, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.FMap(elms, loc) => @@(elms map {
        case (key, value) => @@(namer(key, env0), namer(value, env0))
      }) map {
        case es => NamedAst.Expression.FMap(es, genSym.freshTypeVar(), loc)
      }

      case WeededAst.Expression.GetIndex(exp1, exp2, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0)) map {
          case (e1, e2) => NamedAst.Expression.GetIndex(e1, e2, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.PutIndex(exp1, exp2, exp3, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0)) map {
          case (e1, e2) => NamedAst.Expression.GetIndex(e1, e2, genSym.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Existential(params, exp, loc) =>
        namer(exp, env0) map {
          case e => NamedAst.Expression.Existential(???, e, loc) // TODO
        }

      case WeededAst.Expression.Universal(params, exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Universal(???, e, loc) // TODO
      }

      case WeededAst.Expression.Ascribe(exp, tpe, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Ascribe(e, tpe, loc)
      }

      case WeededAst.Expression.UserError(loc) => NamedAst.Expression.UserError(genSym.freshTypeVar(), loc).toSuccess
    }

  }

  object Patterns {

    /**
      * Names the given pattern `pat0` and returns map from variable names to variable symbols.
      */
    def namer(pat0: WeededAst.Pattern)(implicit genSym: GenSym): (NamedAst.Pattern, Map[String, Symbol.VarSym]) = {
      val m = mutable.Map.empty[String, Symbol.VarSym]
      def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
        case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.Var(ident, loc) =>
          // make a fresh variable symbol for the local variable.
          val sym = Symbol.mkVarSym(ident)
          m += (ident.name -> sym)
          NamedAst.Pattern.Var(sym, genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.Unit(loc) => NamedAst.Pattern.Unit(loc)
        case WeededAst.Pattern.True(loc) => NamedAst.Pattern.True(loc)
        case WeededAst.Pattern.False(loc) => NamedAst.Pattern.False(loc)
        case WeededAst.Pattern.Char(lit, loc) => NamedAst.Pattern.Char(lit, loc)
        case WeededAst.Pattern.Float32(lit, loc) => NamedAst.Pattern.Float32(lit, loc)
        case WeededAst.Pattern.Float64(lit, loc) => NamedAst.Pattern.Float64(lit, loc)
        case WeededAst.Pattern.Int8(lit, loc) => NamedAst.Pattern.Int8(lit, loc)
        case WeededAst.Pattern.Int16(lit, loc) => NamedAst.Pattern.Int16(lit, loc)
        case WeededAst.Pattern.Int32(lit, loc) => NamedAst.Pattern.Int32(lit, loc)
        case WeededAst.Pattern.Int64(lit, loc) => NamedAst.Pattern.Int64(lit, loc)
        case WeededAst.Pattern.BigInt(lit, loc) => NamedAst.Pattern.BigInt(lit, loc)
        case WeededAst.Pattern.Str(lit, loc) => NamedAst.Pattern.Str(lit, loc)
        case WeededAst.Pattern.Tag(enum, tag, pat, loc) => NamedAst.Pattern.Tag(enum, tag, visit(pat), genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FNone(loc) => NamedAst.Pattern.FNone(genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FSome(pat, loc) => NamedAst.Pattern.FSome(visit(pat), genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FNil(loc) => NamedAst.Pattern.FNil(genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FList(hd, tl, loc) => NamedAst.Pattern.FList(visit(hd), visit(tl), genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FVec(elms, rest, loc) => NamedAst.Pattern.FVec(elms map visit, rest map visit, genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FSet(elms, rest, loc) => NamedAst.Pattern.FSet(elms map visit, rest map visit, genSym.freshTypeVar(), loc)
        case WeededAst.Pattern.FMap(elms, rest, loc) =>
          val kvs = elms map {
            case (k, v) => visit(k) -> visit(v)
          }
          NamedAst.Pattern.FMap(kvs, rest map visit, genSym.freshTypeVar(), loc)
      }

      (visit(pat0), m.toMap)
    }

  }

  object Predicates {

    def namer(head: WeededAst.Predicate.Head): NamedAst.Predicate.Head = head match {
      case WeededAst.Predicate.Head.True(loc) => NamedAst.Predicate.Head.True(loc)
      case WeededAst.Predicate.Head.False(loc) => NamedAst.Predicate.Head.False(loc)
      case WeededAst.Predicate.Head.Table(name, terms, loc) => NamedAst.Predicate.Head.Table(name, terms, loc)
    }

  }

}
