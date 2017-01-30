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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

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
      lattices = Map.empty,
      indexes = Map.empty,
      tables = Map.empty,
      constraints = Map.empty,
      hooks = program.hooks,
      properties = Map.empty,
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
        case (pacc, decl) =>
          val namespace = Name.NName(ns.sp1, ns0.idents ::: ns.idents, ns.sp2)
          namer(decl, namespace, pacc)
      }

      /*
       * Definition.
       */
      case WeededAst.Declaration.Definition(doc, ann, ident, tparams0, fparams0, exp, tpe, loc) =>
        // check if the definition already exists.
        val defns = prog0.definitions.getOrElse(ns0, Map.empty)
        defns.get(ident.name) match {
          case None =>
            // Case 1: The definition does not already exist. Update it.

            // Compute the type environment from the formal type parameters.
            val tparams = tparams0.map {
              case p =>
                // Generate a fresh type variable for the type parameter.
                val tvar = Type.freshTypeVar()
                // Remember the original textual name.
                tvar.setText(p.name)
                NamedAst.TypeParam(p, tvar, p.loc)
            }
            val tenv0 = tparams.map(p => p.name.name -> p.tpe).toMap

            // Introduce a variable symbols for each formal parameter.
            var pms0 = List.empty[NamedAst.FormalParam]
            var env0 = Map.empty[String, Symbol.VarSym]
            for (WeededAst.FormalParam(ident, tpe, loc) <- fparams0) {
              val sym = Symbol.freshVarSym(ident)
              pms0 = NamedAst.FormalParam(sym, Types.namer(tpe, tenv0), loc) :: pms0
              env0 = env0 + (ident.name -> sym)
            }

            Expressions.namer(exp, env0, tenv0) map {
              case e =>
                val sym = Symbol.mkDefnSym(ns0, ident)
                val sc = NamedAst.Scheme(tparams.map(_.tpe), Types.namer(tpe, tenv0))
                val defn = NamedAst.Declaration.Definition(doc, ann, sym, tparams, pms0.reverse, e, sc, loc)
                prog0.copy(definitions = prog0.definitions + (ns0 -> (defns + (ident.name -> defn))))
            }
          case Some(defn) =>
            // Case 2: Duplicate definition.
            DuplicateDefinition(ident.name, defn.loc, ident.loc).toFailure
        }

      /*
       * Signature.
       */
      case WeededAst.Declaration.Signature(doc, ident, params, tpe, loc) => ??? // TODO: Add support for signature in Namer.

      /*
       * External.
       */
      case WeededAst.Declaration.External(doc, ident, params, tpe, loc) => ??? // TODO: Add support for external in Namer.

      /*
       * Enum.
       */
      case WeededAst.Declaration.Enum(doc, ident, tparams0, cases, loc) =>
        val enums0 = prog0.enums.getOrElse(ns0, Map.empty)
        enums0.get(ident.name) match {
          case None =>
            // Case 2.1: The enum does not exist in the namespace. Update it.
            val sym = Symbol.mkEnumSym(ns0, ident)
            val tparams = tparams0 map {
              case p => NamedAst.TypeParam(p, Type.freshTypeVar(), loc)
            }
            val tenv = tparams.map(kv => kv.name.name -> kv.tpe).toMap
            val quantifiers = tparams.map(_.tpe).map(x => NamedAst.Type.Var(x, loc))
            val enumType = if (quantifiers.isEmpty)
              NamedAst.Type.Enum(sym)
            else
              NamedAst.Type.Apply(NamedAst.Type.Enum(sym), quantifiers, loc)
            val enum = NamedAst.Declaration.Enum(doc, sym, tparams, casesOf(cases, tenv), enumType, loc)
            val enums = enums0 + (ident.name -> enum)
            prog0.copy(enums = prog0.enums + (ns0 -> enums)).toSuccess
          case Some(enum) =>
            // Case 2.2: Duplicate definition.
            DuplicateDefinition(ident.name, enum.loc, ident.loc).toFailure
        }

      /*
       * Property.
       */
      case WeededAst.Declaration.Property(law, defn, exp0, loc) =>
        Expressions.namer(exp0, Map.empty, Map.empty) map {
          case exp =>
            val lawSym = Symbol.mkDefnSym(law.namespace, law.ident)
            val defnSym = Symbol.mkDefnSym(ns0, defn)
            val property = NamedAst.Property(lawSym, defnSym, exp, loc)
            val properties = prog0.properties.getOrElse(ns0, Nil)
            prog0.copy(properties = prog0.properties + (ns0 -> (property :: properties)))
        }

      /*
       * Constraint.
       */
      case WeededAst.Declaration.Constraint(h, bs, loc) =>
        // Find the variables bound in the head and rule scope of the constraint.
        val headVars = bs.flatMap(Predicates.boundInHeadScope)
        val ruleVars = bs.flatMap(Predicates.boundInRuleScope)

        // Introduce a symbol for each variable that is bound in the head scope of the constraint (excluding those bound by the rule scope).
        val headEnv = headVars.foldLeft(Map.empty[String, Symbol.VarSym]) {
          case (macc, ident) => macc.get(ident.name) match {
            // Check if the identifier is bound by the rule scope.
            case None if !ruleVars.exists(_.name == ident.name) =>
              macc + (ident.name -> Symbol.freshVarSym(ident))
            case _ => macc
          }
        }

        // Introduce a symbol for each variable that is bound in the rule scope of the constraint.
        val ruleEnv = ruleVars.foldLeft(Map.empty[String, Symbol.VarSym]) {
          case (macc, ident) => macc.get(ident.name) match {
            case None => macc + (ident.name -> Symbol.freshVarSym(ident))
            case Some(sym) => macc
          }
        }

        // Constraints are non-polymorphic so the type environment is always empty.
        val tenv0 = Map.empty[String, Type.Var]

        // Perform naming on the head and body predicates.
        @@(Predicates.namer(h, headEnv, ruleEnv, tenv0), @@(bs.map(b => Predicates.namer(b, headEnv, ruleEnv, tenv0)))) map {
          case (head, body) =>
            val headParams = headEnv.map {
              case (_, sym) => NamedAst.ConstraintParam.HeadParam(sym, sym.tvar, sym.loc)
            }
            val ruleParam = ruleEnv.map {
              case (_, sym) => NamedAst.ConstraintParam.RuleParam(sym, sym.tvar, sym.loc)
            }
            val cparams = (headParams ++ ruleParam).toList
            val constraint = NamedAst.Declaration.Constraint(cparams, head, body, loc)
            val constraints = constraint :: prog0.constraints.getOrElse(ns0, Nil)
            prog0.copy(constraints = prog0.constraints + (ns0 -> constraints))
        }

      /*
       * Index.
       */
      case WeededAst.Declaration.Index(qname, indexes, loc) =>
        val name = qname.ident.name
        val index = NamedAst.Declaration.Index(qname, indexes.map(_.toList), loc)
        val decls = prog0.indexes.getOrElse(ns0, Map.empty)
        decls.get(name) match {
          case None => prog0.copy(indexes = prog0.indexes + (ns0 -> (decls + (name -> index)))).toSuccess
          case Some(idx) => NameError.DuplicateIndex(name, idx.loc, loc).toFailure
        }

      /*
       * BoundedLattice (deprecated).
       */
      case WeededAst.Declaration.BoundedLattice(tpe, bot0, top0, leq0, lub0, glb0, loc) =>
        val botVal = Expressions.namer(bot0, Map.empty, Map.empty)
        val topVal = Expressions.namer(top0, Map.empty, Map.empty)
        val leqVal = Expressions.namer(leq0, Map.empty, Map.empty)
        val lubVal = Expressions.namer(lub0, Map.empty, Map.empty)
        val glbVal = Expressions.namer(glb0, Map.empty, Map.empty)

        @@(botVal, topVal, leqVal, lubVal, glbVal) map {
          case (bot, top, leq, lub, glb) =>
            val lattice = NamedAst.Declaration.BoundedLattice(Types.namer(tpe, Map.empty), bot, top, leq, lub, glb, ns0, loc)
            prog0.copy(lattices = prog0.lattices + (Types.namer(tpe, Map.empty) -> lattice)) // NB: This just overrides any existing binding.
        }

      /*
       * Relation.
       */
      case WeededAst.Table.Relation(doc, ident, attr, loc) =>
        // check if the table already exists.
        prog0.tables.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the table does not yet exist.
            val table = NamedAst.Table.Relation(doc, Symbol.mkTableSym(ns0, ident), attr.map(a => Attributes.namer(a, Map.empty)), loc)
            val tables = Map(ident.name -> table)
            prog0.copy(tables = prog0.tables + (ns0 -> tables)).toSuccess
          case Some(tables0) =>
            // Case 2: The namespace exists. Lookup the table.
            tables0.get(ident.name) match {
              case None =>
                // Case 2.1: The table does not exist in the namespace. Update it.
                val table = NamedAst.Table.Relation(doc, Symbol.mkTableSym(ns0, ident), attr.map(a => Attributes.namer(a, Map.empty)), loc)
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
      case WeededAst.Table.Lattice(doc, ident, keys, value, loc) =>
        // check if the table already exists.
        prog0.tables.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the table does not yet exist.
            val table = NamedAst.Table.Lattice(doc, Symbol.mkTableSym(ns0, ident), keys.map(k => Attributes.namer(k, Map.empty)), Attributes.namer(value, Map.empty), loc)
            val tables = Map(ident.name -> table)
            prog0.copy(tables = prog0.tables + (ns0 -> tables)).toSuccess
          case Some(tables0) =>
            // Case 2: The namespace exists. Lookup the table.
            tables0.get(ident.name) match {
              case None =>
                // Case 2.1: The table does not exist in the namespace. Update it.
                val table = NamedAst.Table.Lattice(doc, Symbol.mkTableSym(ns0, ident), keys.map(k => Attributes.namer(k, Map.empty)), Attributes.namer(value, Map.empty), loc)
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
    def casesOf(cases: Map[String, WeededAst.Case], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): Map[String, NamedAst.Case] = cases.foldLeft(Map.empty[String, NamedAst.Case]) {
      case (macc, (name, WeededAst.Case(enum, tag, tpe))) =>
        macc + (name -> NamedAst.Case(enum, tag, Types.namer(tpe, tenv0)))
    }

  }

  object Expressions {

    /**
      * Performs naming on the given expression `exp0` under the given environment `env0`.
      */
    def namer(exp0: WeededAst.Expression, env0: Map[String, Symbol.VarSym], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): Validation[NamedAst.Expression, NameError] = exp0 match {
      /*
       * Variables.
       */
      case WeededAst.Expression.Wild(loc) => NamedAst.Expression.Wild(Type.freshTypeVar(), loc).toSuccess

      case WeededAst.Expression.VarOrRef(name, loc) if name.isUnqualified =>
        // lookup the variable name in the environment.
        env0.get(name.ident.name) match {
          case None =>
            // Case 1: reference.
            NamedAst.Expression.Ref(name, Type.freshTypeVar(), loc).toSuccess
          case Some(sym) =>
            // Case 2: variable.
            NamedAst.Expression.Var(sym, loc).toSuccess
        }

      case WeededAst.Expression.VarOrRef(name, loc) =>
        NamedAst.Expression.Ref(name, Type.freshTypeVar(), loc).toSuccess

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
        val lambdaVal = namer(lambda, env0, tenv0)
        val argsVal = @@(args map (a => namer(a, env0, tenv0)))
        @@(lambdaVal, argsVal) map {
          case (e, es) => NamedAst.Expression.Apply(e, es, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Lambda(params, exp, loc) =>
        // make a fresh variable symbol for each for parameter.
        val syms = params map (ident => Symbol.freshVarSym(ident))
        val env1 = (params zip syms) map {
          case (ident, sym) => ident.name -> sym
        }
        namer(exp, env0 ++ env1, tenv0) map {
          case e => NamedAst.Expression.Lambda(syms, e, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Unary(op, exp, loc) => namer(exp, env0, tenv0) map {
        case e => NamedAst.Expression.Unary(op, e, Type.freshTypeVar(), loc)
      }

      case WeededAst.Expression.Binary(op, exp1, exp2, loc) =>
        @@(namer(exp1, env0, tenv0), namer(exp2, env0, tenv0)) map {
          case (e1, e2) => NamedAst.Expression.Binary(op, e1, e2, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        @@(namer(exp1, env0, tenv0), namer(exp2, env0, tenv0), namer(exp3, env0, tenv0)) map {
          case (e1, e2, e3) => NamedAst.Expression.IfThenElse(e1, e2, e3, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Let(ident, exp1, exp2, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.freshVarSym(ident)
        @@(namer(exp1, env0, tenv0), namer(exp2, env0 + (ident.name -> sym), tenv0)) map {
          case (e1, e2) => NamedAst.Expression.Let(sym, e1, e2, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Match(exp, rules, loc) =>
        val expVal = namer(exp, env0, tenv0)
        val rulesVal = rules map {
          case WeededAst.MatchRule(pat, guard, body) =>
            // extend the environment with every variable occurring in the pattern
            // and perform naming on the rule guard and body under the extended environment.
            val (p, env1) = Patterns.namer(pat)
            val extendedEnv = env0 ++ env1
            @@(namer(guard, extendedEnv, tenv0), namer(body, extendedEnv, tenv0)) map {
              case (g, b) => NamedAst.MatchRule(p, g, b)
            }
        }
        @@(expVal, @@(rulesVal)) map {
          case (e, rs) => NamedAst.Expression.Match(e, rs, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Switch(rules, loc) => @@(rules map {
        case (cond, body) => @@(namer(cond, env0, tenv0), namer(body, env0, tenv0))
      }) map {
        case rs => NamedAst.Expression.Switch(rs, Type.freshTypeVar(), loc)
      }

      case WeededAst.Expression.Tag(enum, tag, exp, loc) => namer(exp, env0, tenv0) map {
        case e => NamedAst.Expression.Tag(enum, tag, e, Type.freshTypeVar(), loc)
      }

      case WeededAst.Expression.Tuple(elms, loc) =>
        @@(elms map (e => namer(e, env0, tenv0))) map {
          case es => NamedAst.Expression.Tuple(es, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Existential(param, exp, loc) =>
        val sym = Symbol.freshVarSym(param.ident)
        namer(exp, env0 + (sym.text -> sym), tenv0) map {
          case e =>
            val p = NamedAst.FormalParam(sym, Types.namer(param.tpe, tenv0), param.loc)
            NamedAst.Expression.Existential(p, e, loc)
        }

      case WeededAst.Expression.Universal(param, exp, loc) =>
        val sym = Symbol.freshVarSym(param.ident)
        namer(exp, env0 + (sym.text -> sym), tenv0) map {
          case e =>
            val p = NamedAst.FormalParam(sym, Types.namer(param.tpe, tenv0), param.loc)
            NamedAst.Expression.Universal(p, e, loc)
        }

      case WeededAst.Expression.Ascribe(exp, tpe, loc) => namer(exp, env0, tenv0) map {
        case e => NamedAst.Expression.Ascribe(e, Types.namer(tpe, tenv0), loc)
      }

      case WeededAst.Expression.UserError(loc) => NamedAst.Expression.UserError(Type.freshTypeVar(), loc).toSuccess
    }

    /**
      * Returns all the free variables in the given expression `exp0`.
      */
    def freeVars(exp0: WeededAst.Expression): List[Name.Ident] = exp0 match {
      case WeededAst.Expression.Wild(loc) => Nil
      case WeededAst.Expression.VarOrRef(qname, loc) => List(qname.ident)
      case WeededAst.Expression.Unit(loc) => Nil
      case WeededAst.Expression.True(loc) => Nil
      case WeededAst.Expression.False(loc) => Nil
      case WeededAst.Expression.Char(lit, loc) => Nil
      case WeededAst.Expression.Float32(lit, loc) => Nil
      case WeededAst.Expression.Float64(lit, loc) => Nil
      case WeededAst.Expression.Int8(lit, loc) => Nil
      case WeededAst.Expression.Int16(lit, loc) => Nil
      case WeededAst.Expression.Int32(lit, loc) => Nil
      case WeededAst.Expression.Int64(lit, loc) => Nil
      case WeededAst.Expression.BigInt(lit, loc) => Nil
      case WeededAst.Expression.Str(lit, loc) => Nil
      case WeededAst.Expression.Apply(lambda, args, loc) => freeVars(lambda) ++ args.flatMap(freeVars)
      case WeededAst.Expression.Lambda(params, exp, loc) => filterBoundVars(freeVars(exp), params)
      case WeededAst.Expression.Unary(op, exp, loc) => freeVars(exp)
      case WeededAst.Expression.Binary(op, exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
      case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) => freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
      case WeededAst.Expression.Let(ident, exp1, exp2, loc) => freeVars(exp1) ++ filterBoundVars(freeVars(exp2), List(ident))
      case WeededAst.Expression.Match(exp, rules, loc) => freeVars(exp) ++ rules.flatMap {
        case WeededAst.MatchRule(pat, guard, body) => filterBoundVars(freeVars(guard) ++ freeVars(body), Patterns.freeVars(pat))
      }
      case WeededAst.Expression.Switch(rules, loc) => rules flatMap {
        case (cond, body) => freeVars(cond) ++ freeVars(body)
      }
      case WeededAst.Expression.Tag(enum, tag, exp, loc) => freeVars(exp)
      case WeededAst.Expression.Tuple(elms, loc) => elms.flatMap(freeVars)
      case WeededAst.Expression.Existential(fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
      case WeededAst.Expression.Universal(fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
      case WeededAst.Expression.Ascribe(exp, tpe, loc) => freeVars(exp)
      case WeededAst.Expression.UserError(loc) => Nil
    }

  }

  object Patterns {

    /**
      * Names the given pattern `pat0` and returns map from variable names to variable symbols.
      */
    def namer(pat0: WeededAst.Pattern)(implicit genSym: GenSym): (NamedAst.Pattern, Map[String, Symbol.VarSym]) = {
      val m = mutable.Map.empty[String, Symbol.VarSym]

      def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
        case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(Type.freshTypeVar(), loc)
        case WeededAst.Pattern.Var(ident, loc) =>
          // make a fresh variable symbol for the local variable.
          val sym = Symbol.freshVarSym(ident)
          m += (ident.name -> sym)
          NamedAst.Pattern.Var(sym, Type.freshTypeVar(), loc)
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
        case WeededAst.Pattern.Tag(enum, tag, pat, loc) => NamedAst.Pattern.Tag(enum, tag, visit(pat), Type.freshTypeVar(), loc)
        case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, Type.freshTypeVar(), loc)
        case WeededAst.Pattern.FSet(elms, rest, loc) => NamedAst.Pattern.FSet(elms map visit, rest map visit, Type.freshTypeVar(), loc)
        case WeededAst.Pattern.FMap(elms, rest, loc) =>
          val kvs = elms map {
            case (k, v) => visit(k) -> visit(v)
          }
          NamedAst.Pattern.FMap(kvs, rest map visit, Type.freshTypeVar(), loc)
      }

      (visit(pat0), m.toMap)
    }

    /**
      * Returns all the free variables in the given pattern `pat0`.
      */
    def freeVars(pat0: WeededAst.Pattern): List[Name.Ident] = pat0 match {
      case WeededAst.Pattern.Var(ident, loc) => List(ident)
      case WeededAst.Pattern.Wild(loc) => Nil
      case WeededAst.Pattern.Unit(loc) => Nil
      case WeededAst.Pattern.True(loc) => Nil
      case WeededAst.Pattern.False(loc) => Nil
      case WeededAst.Pattern.Char(lit, loc) => Nil
      case WeededAst.Pattern.Float32(lit, loc) => Nil
      case WeededAst.Pattern.Float64(lit, loc) => Nil
      case WeededAst.Pattern.Int8(lit, loc) => Nil
      case WeededAst.Pattern.Int16(lit, loc) => Nil
      case WeededAst.Pattern.Int32(lit, loc) => Nil
      case WeededAst.Pattern.Int64(lit, loc) => Nil
      case WeededAst.Pattern.BigInt(lit, loc) => Nil
      case WeededAst.Pattern.Str(lit, loc) => Nil
      case WeededAst.Pattern.Tag(enumName, tagName, p, loc) => freeVars(p)
      case WeededAst.Pattern.Tuple(elms, loc) => elms flatMap freeVars
      case WeededAst.Pattern.FSet(elms, rest, loc) => elms.flatMap(freeVars) ++ rest.map(freeVars).getOrElse(Nil)
      case WeededAst.Pattern.FMap(elms, rest, loc) => (elms flatMap {
        case (k, v) => freeVars(k) ++ freeVars(v)
      }) ++ rest.map(freeVars).getOrElse(Nil)
    }


  }

  object Predicates {

    def namer(head: WeededAst.Predicate.Head, headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): Validation[NamedAst.Predicate.Head, NameError] = head match {
      case WeededAst.Predicate.Head.True(loc) => NamedAst.Predicate.Head.True(loc).toSuccess
      case WeededAst.Predicate.Head.False(loc) => NamedAst.Predicate.Head.False(loc).toSuccess
      case WeededAst.Predicate.Head.Positive(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, headEnv0 ++ ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Head.Positive(qname, ts, loc)
        }
      case WeededAst.Predicate.Head.Negative(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, headEnv0 ++ ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Head.Negative(qname, ts, loc)
        }
    }

    def namer(body: WeededAst.Predicate.Body, headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): Validation[NamedAst.Predicate.Body, NameError] = body match {
      case WeededAst.Predicate.Body.Positive(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Body.Positive(qname, ts, loc)
        }
      case WeededAst.Predicate.Body.Negative(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Body.Negative(qname, ts, loc)
        }
      case WeededAst.Predicate.Body.Filter(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, headEnv0 ++ ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Body.Filter(qname, ts, loc)
        }
      case WeededAst.Predicate.Body.Loop(pat, term, loc) =>
        val (p, env) = Patterns.namer(pat)
        Expressions.namer(term, ruleEnv0, tenv0) map {
          case t => NamedAst.Predicate.Body.Loop(p, t, loc)
        }
    }

    /**
      * Returns the identifiers that are bound in the head scope by the given body predicate `p0`.
      */
    def boundInHeadScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
      case WeededAst.Predicate.Body.Positive(qname, terms, loc) => terms.flatMap(Expressions.freeVars)
      case WeededAst.Predicate.Body.Negative(qname, terms, loc) => terms.flatMap(Expressions.freeVars)
      case WeededAst.Predicate.Body.Filter(qname, terms, loc) => Nil
      case WeededAst.Predicate.Body.Loop(pat, term, loc) => Patterns.freeVars(pat)
    }

    /**
      * Returns the identifiers that are bound in the rule scope by the given body predicate `p0`.
      */
    def boundInRuleScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
      case WeededAst.Predicate.Body.Positive(qname, terms, loc) => terms.flatMap(Expressions.freeVars)
      case WeededAst.Predicate.Body.Negative(qname, terms, loc) => terms.flatMap(Expressions.freeVars)
      case WeededAst.Predicate.Body.Filter(qname, terms, loc) => Nil
      case WeededAst.Predicate.Body.Loop(pat, term, loc) => Nil
    }

  }

  object Types {

    /**
      * Translates the given weeded type `tpe` into a named type under the given type environment `tenv0`.
      */
    def namer(tpe: WeededAst.Type, tenv0: Map[String, Type.Var])(implicit genSym: GenSym): NamedAst.Type = {
      /**
        * Inner visitor.
        */
      def visit(tpe: WeededAst.Type, env: Map[String, Type.Var]): NamedAst.Type = tpe match {
        case WeededAst.Type.Unit(loc) => NamedAst.Type.Unit(loc)
        case WeededAst.Type.Var(ident, loc) => NamedAst.Type.Var(env(ident.name), loc)
        case WeededAst.Type.Ref(qname, loc) =>
          if (qname.isUnqualified)
            env.get(qname.ident.name) match {
              case None => NamedAst.Type.Ref(qname, loc)
              case Some(tvar) => NamedAst.Type.Var(tvar, loc)
            }
          else
            NamedAst.Type.Ref(qname, loc)
        case WeededAst.Type.Tuple(elms, loc) => NamedAst.Type.Tuple(elms.map(e => visit(e, env)), loc)
        case WeededAst.Type.Arrow(tparams, tresult, loc) => NamedAst.Type.Arrow(tparams.map(t => visit(t, env)), visit(tresult, env), loc)
        case WeededAst.Type.Apply(base, tparams, loc) => NamedAst.Type.Apply(visit(base, env), tparams.map(t => visit(t, env)), loc)
      }

      visit(tpe, tenv0)
    }

  }

  object Attributes {

    /**
      * Translates the given weeded attribute to a named attribute.
      */
    def namer(attr: WeededAst.Attribute, tenv0: Map[String, Type.Var])(implicit genSym: GenSym): NamedAst.Attribute = attr match {
      case WeededAst.Attribute(ident, tpe, loc) => NamedAst.Attribute(ident, Types.namer(tpe, tenv0), loc)
    }

  }

  /**
    * Returns the given `freeVars` less the `boundVars`.
    */
  def filterBoundVars(freeVars: List[Name.Ident], boundVars: List[Name.Ident]): List[Name.Ident] = {
    freeVars.filter(n1 => !boundVars.exists(n2 => n1.name == n2.name))
  }

}
