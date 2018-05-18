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

import java.lang.reflect.{Constructor, Field, Method, Modifier}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.WeededAst.Declaration
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import scala.collection.mutable

/**
  * The Namer phase introduces unique symbols for each syntactic entity in the program.
  */
object Namer extends Phase[WeededAst.Program, NamedAst.Program] {

  import NameError._

  /**
    * Introduces unique names for each syntactic entity in the given `program`.
    **/
  def run(program: WeededAst.Program)(implicit flix: Flix): Validation[NamedAst.Program, NameError] = {
    implicit val _ = flix.genSym

    val b = System.nanoTime()

    // make an empty program to fold over.
    val prog0 = NamedAst.Program(
      defs = Map.empty,
      effs = Map.empty,
      handlers = Map.empty,
      enums = Map.empty,
      classes = Map.empty,
      impls = Map.empty,
      lattices = Map.empty,
      indexes = Map.empty,
      tables = Map.empty,
      constraints = Map.empty,
      named = Map.empty,
      properties = Map.empty,
      reachable = program.reachable,
      time = program.time
    )

    // collect all the declarations.
    val declarations = program.roots.flatMap(_.decls)

    // fold over the top-level declarations.
    val result = Validation.fold(declarations, prog0) {
      case (pacc, decl) => Declarations.namer(decl, Name.RootNS, pacc)
    }

    // fold over the named expressions.
    val named = @@(program.named.map {
      case (sym, exp) => Expressions.namer(exp, Map.empty, Map.empty).map(e => sym -> e)
    })

    // compute elapsed time.
    val e = System.nanoTime() - b

    @@(result, named) map {
      // update elapsed time.
      case (p, ne) => p.copy(named = ne.toMap, time = p.time.copy(namer = e))
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
      case WeededAst.Declaration.Def(doc, ann, mod, ident, tparams0, fparams0, exp, tpe, eff0, loc) =>
        // Check if the definition already exists.
        val defns = prog0.defs.getOrElse(ns0, Map.empty)
        defns.get(ident.name) match {
          case None =>
            // Case 1: The definition does not already exist. Update it.

            val tparams = getTypeParams(tparams0)
            val tenv0 = getTypeEnv(tparams)
            val fparams = getFormalParams(fparams0, tenv0)
            val env0 = getVarEnv(fparams)

            Expressions.namer(exp, env0, tenv0) map {
              case e =>
                val sym = Symbol.mkDefnSym(ns0, ident)
                val sc = getScheme(tparams, tpe, tenv0)
                val defn = NamedAst.Def(doc, ann, mod, sym, tparams, fparams, e, sc, eff0, loc)
                prog0.copy(defs = prog0.defs + (ns0 -> (defns + (ident.name -> defn))))
            }
          case Some(defn) =>
            // Case 2: Duplicate definition.
            DuplicateDef(ident.name, defn.loc, ident.loc).toFailure
        }

      /*
       * Law.
       */
      case WeededAst.Declaration.Law(doc, ann, mod, ident, tparams0, fparams0, exp, tpe, eff0, loc) => ??? // TODO

      /*
       * Eff.
       */
      case WeededAst.Declaration.Eff(doc, ann, mod, ident, tparams0, fparams0, tpe, eff0, loc) =>
        // Check if the effect already exists.
        val effs = prog0.effs.getOrElse(ns0, Map.empty)
        effs.get(ident.name) match {
          case None =>
            // Case 1: The effect does not already exist. Update it.
            val tparams = getTypeParams(tparams0)
            val tenv0 = getTypeEnv(tparams)
            val fparams = getFormalParams(fparams0, tenv0)
            val env0 = getVarEnv(fparams)

            val sym = Symbol.mkEffSym(ns0, ident)
            val sc = getScheme(tparams, tpe, tenv0)
            val eff = NamedAst.Eff(doc, ann, mod, sym, tparams, fparams, sc, eff0, loc)
            prog0.copy(effs = prog0.effs + (ns0 -> (effs + (ident.name -> eff)))).toSuccess
          case Some(eff) =>
            // Case 2: Duplicate effect.
            DuplicateEff(ident.name, eff.loc, ident.loc).toFailure
        }

      /*
       * Handler.
       */
      case WeededAst.Declaration.Handler(doc, ann, mod, ident, tparams0, fparams0, exp, tpe, eff0, loc) =>
        // Check if the handler already exists.
        val handlers = prog0.handlers.getOrElse(ns0, Map.empty)
        handlers.get(ident.name) match {
          case None =>
            // Case 1: The handler does not already exist. Update it.
            val tparams = getTypeParams(tparams0)
            val tenv0 = getTypeEnv(tparams)
            val fparams = getFormalParams(fparams0, tenv0)
            val env0 = getVarEnv(fparams)

            Expressions.namer(exp, env0, tenv0) map {
              case e =>
                val sym = Symbol.mkEffSym(ns0, ident)
                val sc = getScheme(tparams, tpe, tenv0)
                val handler = NamedAst.Handler(doc, ann, mod, ident, tparams, fparams, e, sc, eff0, loc)
                prog0.copy(handlers = prog0.handlers + (ns0 -> (handlers + (ident.name -> handler))))
            }
          case Some(handler) =>
            // Case 2: Duplicate handler.
            DuplicateHandler(ident.name, handler.loc, ident.loc).toFailure
        }

      /*
       * Sig.
       */
      case WeededAst.Declaration.Sig(doc, ann, mod, ident, tparams0, fparams0, tpe, eff0, loc) => ??? // TODO

      /*
       * Enum.
       */
      case WeededAst.Declaration.Enum(doc, mod, ident, tparams0, cases, loc) =>
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
            else {
              val base = NamedAst.Type.Enum(sym)
              quantifiers.foldLeft(base: NamedAst.Type) {
                case (tacc, tvar) => NamedAst.Type.Apply(tacc, tvar, loc)
              }
            }
            val enum = NamedAst.Enum(doc, mod, sym, tparams, casesOf(cases, tenv), enumType, loc)
            val enums = enums0 + (ident.name -> enum)
            prog0.copy(enums = prog0.enums + (ns0 -> enums)).toSuccess
          case Some(enum) =>
            // Case 2.2: Duplicate definition.
            DuplicateDef(ident.name, enum.sym.loc, ident.loc).toFailure
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
            val constraint = NamedAst.Constraint(cparams, head, body, loc)
            val constraints = constraint :: prog0.constraints.getOrElse(ns0, Nil)
            prog0.copy(constraints = prog0.constraints + (ns0 -> constraints))
        }

      /*
       * Index.
       */
      case WeededAst.Declaration.Index(qname, indexes, loc) =>
        val name = qname.ident.name
        val index = NamedAst.Index(qname, indexes.map(_.toList), loc)
        val decls = prog0.indexes.getOrElse(ns0, Map.empty)
        decls.get(name) match {
          case None => prog0.copy(indexes = prog0.indexes + (ns0 -> (decls + (name -> index)))).toSuccess
          case Some(idx) => NameError.DuplicateIndex(name, idx.loc, loc).toFailure
        }

      /*
       * BoundedLattice (deprecated).
       */
      case WeededAst.Declaration.Lattice(tpe, bot0, top0, equ0, leq0, lub0, glb0, loc) =>
        val botVal = Expressions.namer(bot0, Map.empty, Map.empty)
        val topVal = Expressions.namer(top0, Map.empty, Map.empty)
        val equVal = Expressions.namer(equ0, Map.empty, Map.empty)
        val leqVal = Expressions.namer(leq0, Map.empty, Map.empty)
        val lubVal = Expressions.namer(lub0, Map.empty, Map.empty)
        val glbVal = Expressions.namer(glb0, Map.empty, Map.empty)

        @@(botVal, topVal, equVal, leqVal, lubVal, glbVal) map {
          case (bot, top, equ, leq, lub, glb) =>
            val lattice = NamedAst.Lattice(Types.namer(tpe, Map.empty), bot, top, equ, leq, lub, glb, ns0, loc)
            prog0.copy(lattices = prog0.lattices + (Types.namer(tpe, Map.empty) -> lattice)) // NB: This just overrides any existing binding.
        }

      //
      // Class.
      //
      case decl@WeededAst.Declaration.Class(doc, mod, head0, body0, sigs0, laws0, loc) =>
        // Check that the class name is not qualified.
        if (head0.qname.isQualified) {
          throw InternalCompilerException(s"Qualified class names are currently unsupported.")
        }

        // Retrieve the class name.
        val ident = head0.qname.ident

        // Check if the class already exists.
        prog0.classes.get(ns0) match {
          case None =>
            // Case 1: The namespace does not yet exist. So the class does not yet exist.
            val clazz = visitClassDecl(decl, ns0)
            val classes = Map(ident.name -> clazz)
            prog0.copy(classes = prog0.classes + (ns0 -> classes)).toSuccess
          case Some(classes0) =>
            // Case 2: The namespace exists. Lookup the class.
            classes0.get(ident.name) match {
              case None =>
                // Case 2.1: The class does not exist in the namespace. Update it.
                val clazz = visitClassDecl(decl, ns0)
                val classes = classes0 + (ident.name -> clazz)
                prog0.copy(classes = prog0.classes + (ns0 -> classes)).toSuccess
              case Some(clazz) =>
                // Case 2.2: Duplicate class.
                val loc1 = clazz.head.qname.ident.loc
                val loc2 = ident.loc
                NameError.DuplicateClass(ident.name, loc1, loc2).toFailure
            }
        }

      //
      // Impl.
      //
      case WeededAst.Declaration.Impl(doc, mod, head0, body0, defs0, loc) =>
        // Compute the free type variables in the head and body atoms.
        val freeTypeVars: List[Name.Ident] = freeVars(head0) ++ (body0 flatMap freeVars)

        // Introduce a fresh type variable for each free identifier.
        val tenv0 = typeEnvFromFreeVars(freeTypeVars)

        // Perform naming on the head and body class atoms.
        val head = visitComplexClass(head0, ns0, tenv0)
        val body = body0.map(b => visitComplexClass(b, ns0, tenv0))

        // Perform naming on the definitions in the implementation.
        val defs = Nil // TODO: must compute the defs.

        // Reassemble the implementation.
        val impl = NamedAst.Impl(doc, mod, head, body, defs, loc)

        // Reassemble the implementations in the namespace.
        val implsInNs = impl :: prog0.impls.getOrElse(ns0, Nil)
        prog0.copy(impls = prog0.impls + (ns0 -> implsInNs)).toSuccess

      //
      // Disallow.
      //
      case WeededAst.Declaration.Disallow(doc, body0, loc) =>
        // Compute the free type variables in the body atoms.
        val freeTypeVars: List[Name.Ident] = body0 flatMap freeVars

        // Introduce a fresh type variable for each free identifier.
        val tenv0 = typeEnvFromFreeVars(freeTypeVars)

        // Perform naming on the body class atoms.
        val body = body0.map(b => visitComplexClass(b, ns0, tenv0))

        // TODO: Decide if these should be separate or go with the impls?
        prog0.toSuccess

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
                DuplicateDef(ident.name, table.loc, ident.loc).toFailure
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
                DuplicateDef(ident.name, table.loc, ident.loc).toFailure
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

    /**
      * Performs naming on the given class declaration `decl0` in the given namespace `ns0`.
      */
    def visitClassDecl(decl0: Declaration.Class, ns0: Name.NName)(implicit genSym: GenSym): NamedAst.Class = decl0 match {
      case Declaration.Class(doc, mod, head0, body0, sigs0, laws0, loc) =>
        // Compute the free type variables in the head and body atoms.
        val freeTypeVars = freeVars(head0) ::: (body0 flatMap freeVars)

        // Introduce a fresh type variable for each free identifier.
        val tenv0 = typeEnvFromFreeVars(freeTypeVars)

        // TODO
        val ident = decl0.head.qname.ident
        val sym = Symbol.mkClassSym(ns0, ident)
        val quantifiers = tenv0.values.toList
        val head = visitSimpleClass(head0, ns0, tenv0)
        val body = body0.map(visitSimpleClass(_, ns0, tenv0))
        val sigs = sigs0.map(visitSig(_, sym, tenv0, ns0))
        val laws = Nil
        NamedAst.Class(doc, mod, sym, quantifiers, head, body, sigs, laws, loc)
    }

    /**
      * Performs naming on the given signature declaration `sig0` in the given namespace `ns0`.
      */
    def visitSig(sig0: WeededAst.Declaration.Sig, classSym: Symbol.ClassSym, tenv0: Map[String, Type.Var], ns0: Name.NName)(implicit genSym: GenSym): NamedAst.Sig = sig0 match {
      case WeededAst.Declaration.Sig(doc, ann, mod, ident, tparams0, fparams0, tpe, eff, loc) =>
        val sym = Symbol.mkSigSym(classSym, ident)
        val tparams = getTypeParams(tparams0)
        val tenv = tenv0 ++ getTypeEnv(tparams)
        val fparams = getFormalParams(fparams0, tenv)
        val sc = getScheme(tparams, tpe, tenv)
        NamedAst.Sig(doc, ann, mod, sym, tparams, fparams, sc, eff, loc)
    }

    /**
      * Performs naming on the given simple class atom `a`.
      */
    def visitSimpleClass(a: WeededAst.SimpleClass, ns0: Name.NName, tenv0: Map[String, Type.Var]): NamedAst.SimpleClass = a match {
      case WeededAst.SimpleClass(qname, targs0, loc) =>
        val targs = targs0.map(ident => tenv0(ident.name))
        NamedAst.SimpleClass(qname, targs, loc)
    }

    /**
      * Performs naming on the given complex class atom `a`.
      */
    def visitComplexClass(a: WeededAst.ComplexClass, ns0: Name.NName, tenv0: Map[String, Type.Var])(implicit genSym: GenSym): NamedAst.ComplexClass = a match {
      case WeededAst.ComplexClass(qname, polarity, targs0, loc) =>
        val targs = targs0.map(t => Types.namer(t, tenv0))
        NamedAst.ComplexClass(qname, polarity, targs, loc)
    }

    /**
      * Returns the free variables in the given simple class atom `a`.
      */
    def freeVars(a: WeededAst.SimpleClass): List[Name.Ident] = a.args

    /**
      * Returns the free variables in the given complex class atom `a`.
      */
    def freeVars(a: WeededAst.ComplexClass): List[Name.Ident] = a.args.flatMap(freeVars)

    /**
      * Returns the free variables in the given type `tpe`.
      */
    def freeVars(tpe: WeededAst.Type): List[Name.Ident] = tpe match {
      case WeededAst.Type.Var(ident, loc) => ident :: Nil
      case WeededAst.Type.Ambiguous(qname, loc) => Nil
      case WeededAst.Type.Unit(loc) => Nil
      case WeededAst.Type.Tuple(elms, loc) => elms.flatMap(freeVars)
      case WeededAst.Type.Nat(n, loc) => Nil
      case WeededAst.Type.Native(fqm, loc) => Nil
      case WeededAst.Type.Arrow(tparams, retType, loc) => tparams.flatMap(freeVars) ::: freeVars(retType)
      case WeededAst.Type.Apply(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    }

    /**
      * Returns a fresh type environment constructed from the given identifiers `idents`.
      */
    def typeEnvFromFreeVars(idents: List[Name.Ident])(implicit genSym: GenSym): Map[String, Type.Var] =
      idents.foldLeft(Map.empty[String, Type.Var]) {
        case (macc, ident) => macc.get(ident.name) match {
          case None => macc + (ident.name -> Type.freshTypeVar())
          case Some(tvar) => macc
        }
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

      case WeededAst.Expression.VarOrDef(name, loc) if name.isUnqualified =>
        // lookup the variable name in the environment.
        env0.get(name.ident.name) match {
          case None =>
            // Case 1: reference.
            NamedAst.Expression.Def(name, Type.freshTypeVar(), loc).toSuccess
          case Some(sym) =>
            // Case 2: variable.
            NamedAst.Expression.Var(sym, loc).toSuccess
        }

      case WeededAst.Expression.VarOrDef(name, loc) =>
        NamedAst.Expression.Def(name, Type.freshTypeVar(), loc).toSuccess

      /*
       * Holes.
       */
      case WeededAst.Expression.Hole(name, loc) =>
        val tpe = Type.freshTypeVar()
        NamedAst.Expression.Hole(name, tpe, loc).toSuccess

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

      case WeededAst.Expression.Lambda(fparams0, exp, loc) =>
        val fparams = fparams0.map(p => Params.namer(p, tenv0))
        val env1 = fparams.map(p => p.sym.text -> p.sym)
        namer(exp, env0 ++ env1, tenv0) map {
          case e => NamedAst.Expression.Lambda(fparams, e, Type.freshTypeVar(), loc)
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

      case WeededAst.Expression.LetRec(ident, exp1, exp2, loc) =>
        // make a fresh variable symbol for the local recursive variable.
        val sym = Symbol.freshVarSym(ident)
        val env1 = env0 + (ident.name -> sym)
        @@(namer(exp1, env1, tenv0), namer(exp2, env1, tenv0)) map {
          case (e1, e2) => NamedAst.Expression.LetRec(sym, e1, e2, Type.freshTypeVar(), loc)
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

      case WeededAst.Expression.Tag(enum, tag, expOpt, loc) => expOpt match {
        case None =>
          // Case 1: The tag does not have an expression. Nothing more to be done.
          NamedAst.Expression.Tag(enum, tag, None, Type.freshTypeVar(), loc).toSuccess
        case Some(exp) =>
          // Case 2: The tag has an expression. Perform naming on it.
          namer(exp, env0, tenv0) map {
            case e => NamedAst.Expression.Tag(enum, tag, Some(e), Type.freshTypeVar(), loc)
          }
      }

      case WeededAst.Expression.Tuple(elms, loc) =>
        @@(elms map (e => namer(e, env0, tenv0))) map {
          case es => NamedAst.Expression.Tuple(es, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.ArrayLit(elms, loc) =>
        @@(elms map (e => namer(e, env0, tenv0))) map {
          case es => NamedAst.Expression.ArrayLit(es, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.ArrayNew(elm, len, loc) =>
        @@(namer(elm, env0, tenv0), namer(len, env0, tenv0)) map {
          case (es, ln) => NamedAst.Expression.ArrayNew(es, ln, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.ArrayLoad(base, index, loc) =>
        @@(namer(base, env0, tenv0), namer(index, env0, tenv0)) map {
          case(b, i) => NamedAst.Expression.ArrayLoad(b, i, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.ArrayStore(base, index, elm, loc) =>
        @@(namer(base, env0, tenv0), namer(index,env0,tenv0), namer(elm, env0, tenv0)) map {
          case(b, i, e) => NamedAst.Expression.ArrayStore(b, i, e, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.ArrayLength(base, loc) =>
        namer(base, env0, tenv0) map {
          case(b) => NamedAst.Expression.ArrayLength(b, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.ArraySlice(base, startIndex, endIndex, loc) =>
        @@(namer(base, env0, tenv0), namer(startIndex, env0, tenv0), namer(endIndex, env0, tenv0)) map {
          case(b, i1, i2) => NamedAst.Expression.ArraySlice(b, i1, i2, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.VectorLit(elms, loc) =>
      @@(elms map (e => namer(e, env0, tenv0))) map{
        case es => NamedAst.Expression.VectorLit(es, Type.freshTypeVar(), loc)
      }

      case WeededAst.Expression.VectorNew(elm, len, loc) =>
        namer(elm, env0, tenv0) map {
          case e => NamedAst.Expression.VectorNew(e, len, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.VectorLoad(base, index, loc) =>
        namer(base, env0, tenv0) map {
          case b => NamedAst.Expression.VectorLoad(b, index, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.VectorStore(base, index, elm, loc) =>
        @@(namer(base, env0, tenv0), namer(elm, env0, tenv0)) map {
          case(b, e) => NamedAst.Expression.VectorStore(b, index, e, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.VectorLength(base, loc) =>
        namer(base, env0, tenv0) map {
          case b => NamedAst.Expression.VectorLength(b, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.VectorSlice(base, startIndex, optEndIndex, loc) =>
        namer(base, env0, tenv0) map {
          case b => NamedAst.Expression.VectorSlice(b, startIndex, optEndIndex, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Ref(exp, loc) =>
        namer(exp, env0, tenv0) map {
          case e => NamedAst.Expression.Ref(e, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Deref(exp, loc) =>
        namer(exp, env0, tenv0) map {
          case e => NamedAst.Expression.Deref(e, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Assign(exp1, exp2, loc) =>
        @@(namer(exp1, env0, tenv0), namer(exp2, env0, tenv0)) map {
          case (e1, e2) => NamedAst.Expression.Assign(e1, e2, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.HandleWith(exp, bindings, loc) =>
        val baseVal = namer(exp, env0, tenv0)
        val bindingsVal = @@(bindings map {
          case WeededAst.HandlerBinding(qname, exp) =>
            namer(exp, env0, tenv0) map {
              case e => NamedAst.HandlerBinding(qname, e)
            }
        })
        @@(baseVal, bindingsVal) map {
          case (b, bs) => NamedAst.Expression.HandleWith(b, bs, Type.freshTypeVar(), loc)
        }

      case WeededAst.Expression.Existential(param, exp, loc) =>
        val p = Params.namer(param, tenv0)
        namer(exp, env0 + (p.sym.text -> p.sym), tenv0) map {
          case e =>
            NamedAst.Expression.Existential(p, e, loc)
        }

      case WeededAst.Expression.Universal(param, exp, loc) =>
        val p = Params.namer(param, tenv0)
        namer(exp, env0 + (p.sym.text -> p.sym), tenv0) map {
          case e =>
            NamedAst.Expression.Universal(p, e, loc)
        }

      case WeededAst.Expression.Ascribe(exp, tpe, eff, loc) => namer(exp, env0, tenv0) map {
        case e => NamedAst.Expression.Ascribe(e, Types.namer(tpe, tenv0), eff, loc)
      }

      case WeededAst.Expression.Cast(exp, tpe, eff, loc) => namer(exp, env0, tenv0) map {
        case e => NamedAst.Expression.Cast(e, Types.namer(tpe, tenv0), eff, loc)
      }

      case WeededAst.Expression.NativeConstructor(className, args, loc) =>
        lookupNativeConstructor(className, args, loc) match {
          case Ok(constructor) => @@(args.map(e => namer(e, env0, tenv0))) map {
            case es => NamedAst.Expression.NativeConstructor(constructor, es, Type.freshTypeVar(), loc)
          }
          case Err(e) => e.toFailure
        }

      case WeededAst.Expression.NativeField(className, fieldName, loc) =>
        lookupNativeField(className, fieldName, loc) match {
          case Ok(field) => NamedAst.Expression.NativeField(field, Type.freshTypeVar(), loc).toSuccess
          case Err(e) => e.toFailure
        }

      case WeededAst.Expression.NativeMethod(className, methodName, args, loc) =>
        lookupNativeMethod(className, methodName, args, loc) match {
          case Ok(method) => @@(args.map(e => namer(e, env0, tenv0))) map {
            case es => NamedAst.Expression.NativeMethod(method, es, Type.freshTypeVar(), loc)
          }
          case Err(e) => e.toFailure
        }

      case WeededAst.Expression.UserError(loc) => NamedAst.Expression.UserError(Type.freshTypeVar(), loc).toSuccess
    }

    /**
      * Returns all the free variables in the given expression `exp0`.
      */
    def freeVars(exp0: WeededAst.Expression): List[Name.Ident] = exp0 match {
      case WeededAst.Expression.Wild(loc) => Nil
      case WeededAst.Expression.VarOrDef(qname, loc) => List(qname.ident)
      case WeededAst.Expression.Hole(name, loc) => Nil
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
      case WeededAst.Expression.Lambda(fparams, exp, loc) => filterBoundVars(freeVars(exp), fparams.map(_.ident))
      case WeededAst.Expression.Unary(op, exp, loc) => freeVars(exp)
      case WeededAst.Expression.Binary(op, exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
      case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) => freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
      case WeededAst.Expression.Let(ident, exp1, exp2, loc) => freeVars(exp1) ++ filterBoundVars(freeVars(exp2), List(ident))
      case WeededAst.Expression.LetRec(ident, exp1, exp2, loc) => filterBoundVars(freeVars(exp1), List(ident)) ++ filterBoundVars(freeVars(exp2), List(ident))
      case WeededAst.Expression.Match(exp, rules, loc) => freeVars(exp) ++ rules.flatMap {
        case WeededAst.MatchRule(pat, guard, body) => filterBoundVars(freeVars(guard) ++ freeVars(body), Patterns.freeVars(pat))
      }
      case WeededAst.Expression.Switch(rules, loc) => rules flatMap {
        case (cond, body) => freeVars(cond) ++ freeVars(body)
      }
      case WeededAst.Expression.Tag(enum, tag, expOpt, loc) => expOpt.map(freeVars).getOrElse(Nil)
      case WeededAst.Expression.Tuple(elms, loc) => elms.flatMap(freeVars)
      case WeededAst.Expression.ArrayLit(elms, loc) => elms.flatMap(freeVars)
      case WeededAst.Expression.ArrayNew(elm, len, loc) =>  freeVars(elm) ++ freeVars(len)
      case WeededAst.Expression.ArrayLoad(base, index, loc) => freeVars(base) ++ freeVars(index)
      case WeededAst.Expression.ArrayStore(base, index, elm, loc) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)
      case WeededAst.Expression.ArrayLength(base, loc) => freeVars(base)
      case WeededAst.Expression.ArraySlice(base, startIndex, endIndex, loc) => freeVars(base) ++ freeVars(startIndex) ++ freeVars(endIndex)
      case WeededAst.Expression.VectorLit(elms, loc) => elms.flatMap(freeVars)
      case WeededAst.Expression.VectorNew(elm, len, loc) => freeVars(elm)
      case WeededAst.Expression.VectorLoad(base, index, loc) => freeVars(base)
      case WeededAst.Expression.VectorStore(base, index, elm, loc) => freeVars(base) ++ freeVars(elm)
      case WeededAst.Expression.VectorLength(base, loc) => freeVars(base)
      case WeededAst.Expression.VectorSlice(base, startIndex, endIndexOpt, loc) => freeVars(base)
      case WeededAst.Expression.Ref(exp, loc) => freeVars(exp)
      case WeededAst.Expression.Deref(exp, loc) => freeVars(exp)
      case WeededAst.Expression.Assign(exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
      case WeededAst.Expression.HandleWith(exp, bindings, loc) => freeVars(exp) ++ bindings.flatMap(b => freeVars(b.exp))
      case WeededAst.Expression.Existential(fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
      case WeededAst.Expression.Universal(fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
      case WeededAst.Expression.Ascribe(exp, tpe, eff, loc) => freeVars(exp)
      case WeededAst.Expression.Cast(exp, tpe, eff, loc) => freeVars(exp)
      case WeededAst.Expression.NativeField(className, fieldName, loc) => Nil
      case WeededAst.Expression.NativeMethod(className, methodName, args, loc) => args.flatMap(freeVars)
      case WeededAst.Expression.NativeConstructor(className, args, loc) => args.flatMap(freeVars)
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
      }

      (visit(pat0), m.toMap)
    }

    /**
      * Names the given pattern `pat0` under the given environment `env0`.
      *
      * Every variable in the pattern must be bound by the environment.
      */
    def namer(pat0: WeededAst.Pattern, env0: Map[String, Symbol.VarSym])(implicit genSym: GenSym): NamedAst.Pattern = {
      def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
        case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(Type.freshTypeVar(), loc)
        case WeededAst.Pattern.Var(ident, loc) =>
          val sym = env0(ident.name)
          NamedAst.Pattern.Var(sym, sym.tvar, loc)
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
      }

      visit(pat0)
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
    }


  }

  object Predicates {

    def namer(head: WeededAst.Predicate.Head, headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): Validation[NamedAst.Predicate.Head, NameError] = head match {
      case WeededAst.Predicate.Head.True(loc) => NamedAst.Predicate.Head.True(loc).toSuccess
      case WeededAst.Predicate.Head.False(loc) => NamedAst.Predicate.Head.False(loc).toSuccess
      case WeededAst.Predicate.Head.Atom(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, headEnv0 ++ ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Head.Atom(qname, ts, loc)
        }
    }

    def namer(body: WeededAst.Predicate.Body, headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): Validation[NamedAst.Predicate.Body, NameError] = body match {
      case WeededAst.Predicate.Body.Atom(qname, polarity, terms, loc) =>
        val ts = terms.map(t => Patterns.namer(t, ruleEnv0))
        NamedAst.Predicate.Body.Atom(qname, polarity, ts, loc).toSuccess

      case WeededAst.Predicate.Body.Filter(qname, terms, loc) =>
        @@(terms.map(t => Expressions.namer(t, headEnv0 ++ ruleEnv0, tenv0))) map {
          case ts => NamedAst.Predicate.Body.Filter(qname, ts, loc)
        }
      case WeededAst.Predicate.Body.Loop(pat, term, loc) =>
        val p = Patterns.namer(pat, headEnv0)
        Expressions.namer(term, ruleEnv0, tenv0) map {
          case t => NamedAst.Predicate.Body.Loop(p, t, loc)
        }
    }

    /**
      * Returns the identifiers that are bound in the head scope by the given body predicate `p0`.
      */
    def boundInHeadScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
      case WeededAst.Predicate.Body.Atom(polarity, qname, terms, loc) => terms.flatMap(Patterns.freeVars)
      case WeededAst.Predicate.Body.Filter(qname, terms, loc) => Nil
      case WeededAst.Predicate.Body.Loop(pat, term, loc) => Patterns.freeVars(pat)
    }

    /**
      * Returns the identifiers that are bound in the rule scope by the given body predicate `p0`.
      */
    def boundInRuleScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
      case WeededAst.Predicate.Body.Atom(polarity, qname, terms, loc) => terms.flatMap(Patterns.freeVars)
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
        case WeededAst.Type.Ambiguous(qname, loc) =>
          if (qname.isUnqualified)
            env.get(qname.ident.name) match {
              case None => NamedAst.Type.Ambiguous(qname, loc)
              case Some(tvar) => NamedAst.Type.Var(tvar, loc)
            }
          else
            NamedAst.Type.Ambiguous(qname, loc)
        case WeededAst.Type.Tuple(elms, loc) => NamedAst.Type.Tuple(elms.map(e => visit(e, env)), loc)
        case WeededAst.Type.Nat(len, loc) => NamedAst.Type.Nat(len, loc)
        case WeededAst.Type.Native(fqn, loc) => NamedAst.Type.Native(fqn, loc)
        case WeededAst.Type.Arrow(tparams, tresult, loc) => NamedAst.Type.Arrow(tparams.map(t => visit(t, env)), visit(tresult, env), loc)
        case WeededAst.Type.Apply(tpe1, tpe2, loc) => NamedAst.Type.Apply(visit(tpe1, env), visit(tpe2, env), loc)
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

  object Params {

    /**
      * Translates the given weeded formal parameter to a named formal parameter.
      */
    def namer(fparam: WeededAst.FormalParam, tenv0: Map[String, Type.Var])(implicit genSym: GenSym): NamedAst.FormalParam = fparam match {
      case WeededAst.FormalParam(ident, mod, optType, loc) =>
        // Generate a fresh variable symbol for the identifier.
        val freshSym = Symbol.freshVarSym(ident)

        // Compute the type of the formal parameter or use the type variable of the symbol.
        val tpe = optType match {
          case None => NamedAst.Type.Var(freshSym.tvar, loc)
          case Some(t) => Types.namer(t, tenv0)
        }

        // Construct the formal parameter.
        NamedAst.FormalParam(freshSym, mod, tpe, loc)
    }

  }

  /**
    * Returns the given `freeVars` less the `boundVars`.
    */
  def filterBoundVars(freeVars: List[Name.Ident], boundVars: List[Name.Ident]): List[Name.Ident] = {
    freeVars.filter(n1 => !boundVars.exists(n2 => n1.name == n2.name))
  }

  /**
    * Returns the result of looking up the given `fieldName` on the given `className`.
    */
  def lookupNativeField(className: String, fieldName: String, loc: SourceLocation): Result[Field, NameError] = try {
    // retrieve class object.
    val clazz = Class.forName(className)

    // retrieve the matching static fields.
    val fields = clazz.getDeclaredFields.toList.filter {
      case field => field.getName == fieldName && Modifier.isStatic(field.getModifiers)
    }

    // match on the number of fields.
    fields.size match {
      case 0 => Err(UndefinedNativeField(className, fieldName, loc))
      case 1 => Ok(fields.head)
      case _ => throw InternalCompilerException("Ambiguous native field?")
    }
  } catch {
    case ex: ClassNotFoundException => Err(UndefinedNativeClass(className, loc))
  }

  /**
    * Returns the result of looking up the given `methodName` on the given `className` with the given `arity`.
    */
  def lookupNativeMethod(className: String, methodName: String, args: List[WeededAst.Expression], loc: SourceLocation): Result[Method, NameError] = try {
    // TODO: Possibly all this needs to take place in the resolver.

    // Compute the argument types.
    val argumentTypes = args map {
      case WeededAst.Expression.Ascribe(_, tpe, _, _) =>
        // The argument is ascribed. Try to determine its type.
        lookupNativeType(tpe)
      case _ =>
        // The argument is not ascribed. We do not know its type.
        None
    }

    // compute the arity.
    val arity = argumentTypes.length

    // retrieve class object.
    val clazz = Class.forName(className)

    // retrieve the matching methods.
    val matchedMethods = clazz.getDeclaredMethods.toList.filter {
      case method => method.getName == methodName
    }

    // retrieve the static methods.
    val staticMethods = matchedMethods.filter {
      case method => Modifier.isStatic(method.getModifiers) &&
        method.getParameterCount == arity &&
        parameterTypeMatch(argumentTypes, method.getParameterTypes.toList)
    }

    // retrieve the object methods.
    val objectMethods = matchedMethods.filter {
      case method => !Modifier.isStatic(method.getModifiers) && method.getParameterCount == (arity - 1) &&
        parameterTypeMatch(argumentTypes.tail, method.getParameterTypes.toList)
    }

    // static and object methods.
    val methods = staticMethods ::: objectMethods

    // match on the number of methods.
    methods.size match {
      case 0 => Err(UndefinedNativeMethod(className, methodName, arity, loc))
      case 1 => Ok(methods.head)
      case _ => Err(AmbiguousNativeMethod(className, methodName, arity, loc))
    }
  } catch {
    case ex: ClassNotFoundException => Err(UndefinedNativeClass(className, loc))
  }

  /**
    * Returns the result of looking up the constructor on the given `className` with the given `arity`.
    */
  def lookupNativeConstructor(className: String, args: List[WeededAst.Expression], loc: SourceLocation): Result[Constructor[_], NameError] = try {
    // Compute the argument types.
    val argumentTypes = args map {
      case WeededAst.Expression.Ascribe(_, tpe, _, _) =>
        // The argument is ascribed. Try to determine its type.
        lookupNativeType(tpe)
      case _ =>
        // The argument is not ascribed. We do not know its type.
        None
    }

    // compute the arity.
    val arity = argumentTypes.length

    // retrieve class object.
    val clazz = Class.forName(className)

    // retrieve the constructors of the appropriate arity.
    val constructors = clazz.getDeclaredConstructors.toList.filter {
      case constructor => constructor.getParameterCount == arity && parameterTypeMatch(argumentTypes, constructor.getParameterTypes.toList)
    }

    // match on the number of methods.
    constructors.size match {
      case 0 => Err(UndefinedNativeConstructor(className, arity, loc))
      case 1 => Ok(constructors.head)
      case _ => Err(AmbiguousNativeConstructor(className, arity, loc))
    }
  } catch {
    case ex: ClassNotFoundException => Err(UndefinedNativeClass(className, loc))
  }

  /**
    * Optionally returns the native type of the given type `tpe`.
    *
    * May return `None` if not information about `tpe` is known.
    */
  def lookupNativeType(tpe: WeededAst.Type): Option[Class[_]] = tpe match {
    case WeededAst.Type.Native(fqn, loc) => lookupClass(fqn.mkString("."))
    case WeededAst.Type.Ambiguous(qname, loc) =>
      // TODO: Ugly incorrect hack. Must take place in the resolver.
      if (qname.ident.name == "Str") Some(classOf[String]) else None
    // TODO: Would be useful to handle primitive types too.
    case _ => None
  }

  /**
    * Optionally returns the class reflection object for the given `className`.
    */
  def lookupClass(className: String): Option[Class[_]] = try {
    Some(Class.forName(className))
  } catch {
    case ex: ClassNotFoundException => None // TODO: Need to return a proper validation instead?
  }

  /**
    * Returns `true` if the class types present in `expected` equals those in `actual`.
    */
  def parameterTypeMatch(expected: List[Option[Class[_]]], actual: List[Class[_]]): Boolean =
    (expected zip actual) forall {
      case (None, _) => true
      case (Some(clazz1), clazz2) => clazz1 == clazz2
    }

  /**
    * Performs naming on the given formal parameters `fparam0` under the given type environment `tenv0`.
    */
  def getFormalParams(fparams0: List[WeededAst.FormalParam], tenv0: Map[String, Type.Var])(implicit genSym: GenSym): List[NamedAst.FormalParam] = {
    fparams0.map(p => Params.namer(p, tenv0))
  }

  /**
    * Performs naming on the given type parameters `tparams0`.
    */
  def getTypeParams(tparams0: List[Name.Ident])(implicit genSym: GenSym): List[NamedAst.TypeParam] = tparams0 map {
    case p =>
      // Generate a fresh type variable for the type parameter.
      val tvar = Type.freshTypeVar()
      // Remember the original textual name.
      tvar.setText(p.name)
      NamedAst.TypeParam(p, tvar, p.loc)
  }

  /**
    * Returns a variable environment constructed from the given formal parameters `fparams0`.
    */
  def getVarEnv(fparams0: List[NamedAst.FormalParam]): Map[String, Symbol.VarSym] = {
    fparams0.map(p => p.sym.text -> p.sym).toMap
  }

  /**
    * Returns a type environment constructed from the given type parameters `tparams0`.
    */
  def getTypeEnv(tparams0: List[NamedAst.TypeParam]): Map[String, Type.Var] = {
    tparams0.map(p => p.name.name -> p.tpe).toMap
  }

  /**
    * Returns the type scheme for the given type parameters `tparams0` and type `tpe` under the given type environment `tenv0`.
    */
  def getScheme(tparams0: List[NamedAst.TypeParam], tpe: WeededAst.Type, tenv0: Map[String, Type.Var])(implicit genSym: GenSym): NamedAst.Scheme = {
    NamedAst.Scheme(tparams0.map(_.tpe), Types.namer(tpe, tenv0))
  }

}
