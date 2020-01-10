/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.collection.parallel.CollectionConverters._

/**
  * The Redundancy phase checks that declarations and expressions within the AST are used in a meaningful way.
  *
  * For example, the redundancy phase ensures that there are no:
  *
  * - unused local variables.
  * - unused enums, definitions, relations, lattices, ...
  * - useless expressions.
  *
  * and so on.
  *
  * The phase performs no AST rewrites; it can be disabled without affecting the runtime semantics.
  */
object Redundancy extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Checks the given AST `root` for redundancies.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, RedundancyError] = flix.phase("Redundancy") {
    // Return early if the redundancy phase is disabled.
    if (flix.options.xallowredundancies) {
      return root.toSuccess
    }

    // Computes all used symbols in all defs (in parallel).
    val usedDefs = root.defs.par.aggregate(Used.empty)({
      case (acc, (sym, decl)) => acc ++ visitDef(decl)(root, flix)
    }, _ ++ _)

    // Computes all used symbols in all lattices.
    val usedLats = root.latticeComponents.values.foldLeft(Used.empty) {
      case (acc, LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc)) =>
        acc ++ visitExps(bot :: top :: equ :: leq :: lub :: glb :: Nil, Env.empty)
    }

    // Computes all used symbols.
    val usedAll = usedLats ++ usedDefs

    // Check for unused symbols.
    val usedRes =
      checkUnusedDefs(usedAll)(root) ++
        checkUnusedEnumsAndTags(usedAll)(root) ++
        checkUnusedRelations(usedAll)(root) ++
        checkUnusedLattices(usedAll)(root) ++
        checkUnusedTypeParamsEnums()(root) ++
        checkUnusedTypeParamsRelations()(root) ++
        checkUnusedTypeParamsLattices()(root)

    // Return the root if successful, otherwise returns all redundancy errors.
    usedRes.toValidation(root)
  }

  /**
    * Checks for unused symbols in the given definition and returns all used symbols.
    */
  private def visitDef(defn: Def)(implicit root: Root, flix: Flix): Used = {

    /**
      * Checks for unused formal parameters.
      */
    def checkUnusedFormalParameters(used: Used): Used = {
      val unusedParams = defn.fparams.collect {
        case fparam if deadVarSym(fparam.sym, used) => UnusedFormalParam(fparam.sym)
      }
      used ++ unusedParams
    }

    /**
      * Checks for unused type parameters.
      */
    def checkUnusedTypeParameters(used: Used): Used = {
      val unusedParams = defn.tparams.collect {
        case tparam if deadTypeVar(tparam.tpe, defn.sc.base.typeVars) => UnusedTypeParam(tparam.name)
      }
      used ++ unusedParams
    }

    // Compute the used symbols inside the definition.
    val usedExp = visitExp(defn.exp, Env.of(defn.fparams.map(_.sym)))

    // Check for unused parameters and remove all variable symbols.
    val usedAll = (usedExp ++ checkUnusedFormalParameters(usedExp) ++ checkUnusedTypeParameters(usedExp)).copy(varSyms = Set.empty)

    // Check if the used symbols contains holes. If so, strip out all error messages.
    if (usedAll.holeSyms.isEmpty) usedAll else usedAll.copy(errors = Set.empty)
  }

  /**
    * Checks for unused definition symbols.
    */
  private def checkUnusedDefs(used: Used)(implicit root: Root): Used = {
    root.defs.foldLeft(used) {
      case (acc, (_, decl)) if deadDef(decl, used) => acc + UnusedDefSym(decl.sym)
      case (acc, _) => acc
    }
  }

  /**
    * Checks for unused enum symbols and tags.
    */
  private def checkUnusedEnumsAndTags(used: Used)(implicit root: Root): Used = {
    root.enums.foldLeft(used) {
      case (acc, (sym, decl)) if decl.mod.isPublic =>
        // Enum is public. No usage requirements.
        acc
      case (acc, (sym, decl)) =>
        // Enum is non-public.
        // Lookup usage information for this specific enum.
        used.enumSyms.get(sym) match {
          case None =>
            // Case 1: Enum is never used.
            acc + UnusedEnumSym(sym)
          case Some(usedTags) =>
            // Case 2: Enum is used and here are its used tags.
            // Check if there is any unused tag.
            decl.cases.values.find(caze => !usedTags.contains(caze.tag.name)) match {
              case None => acc
              case Some(caze) => acc + UnusedEnumTag(sym, caze.tag)
            }
        }
    }
  }

  /**
    * Checks for unused type parameters in enums.
    */
  private def checkUnusedTypeParamsEnums()(implicit root: Root): Used = {
    root.enums.foldLeft(Used.empty) {
      case (acc, (_, decl)) =>
        val usedTypeVars = decl.cases.foldLeft(Set.empty[Type.Var]) {
          case (sacc, (_, Case(_, _, tpe, _))) => sacc ++ tpe.typeVars
        }
        val unusedTypeParams = decl.tparams.filter(tparam => !usedTypeVars.contains(tparam.tpe))
        acc ++ unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name))
    }
  }

  /**
    * Checks for unused type parameters in relations.
    */
  private def checkUnusedTypeParamsRelations()(implicit root: Root): Used = {
    root.relations.foldLeft(Used.empty) {
      case (acc, (_, decl)) =>
        val usedTypeVars = decl.attr.foldLeft(Set.empty[Type.Var]) {
          case (sacc, Attribute(_, tpe, _)) => sacc ++ tpe.typeVars
        }
        val unusedTypeParams = decl.tparams.filter(tparam => !usedTypeVars.contains(tparam.tpe))
        acc ++ unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name))
    }
  }

  /**
    * Checks for unused type parameters in lattices.
    */
  private def checkUnusedTypeParamsLattices()(implicit root: Root): Used = {
    root.lattices.foldLeft(Used.empty) {
      case (acc, (_, decl)) =>
        val usedTypeVars = decl.attr.foldLeft(Set.empty[Type.Var]) {
          case (sacc, Attribute(_, tpe, _)) => sacc ++ tpe.typeVars
        }
        val unusedTypeParams = decl.tparams.filter(tparam => !usedTypeVars.contains(tparam.tpe))
        acc ++ unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name))
    }
  }

  /**
    * Checks for unused relation symbols.
    */
  private def checkUnusedRelations(used: Redundancy.Used)(implicit root: Root): Used = {
    val unusedRelSyms = root.relations.collect {
      case (sym, decl) if deadRel(decl, used) => UnusedRelSym(sym)
    }
    used ++ unusedRelSyms
  }

  /**
    * Checks for unused lattice symbols.
    */
  private def checkUnusedLattices(used: Redundancy.Used)(implicit root: Root): Used = {
    val unusedLatSyms = root.lattices.collect {
      case (sym, decl) if deadLat(decl, used) => UnusedLatSym(sym)
    }
    used ++ unusedLatSyms
  }

  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: Expression, env0: Env): Used = e0 match {
    case Expression.Unit(_) => Used.empty

    case Expression.True(_) => Used.empty

    case Expression.False(_) => Used.empty

    case Expression.Char(_, _) => Used.empty

    case Expression.Float32(_, _) => Used.empty

    case Expression.Float64(_, _) => Used.empty

    case Expression.Int8(_, _) => Used.empty

    case Expression.Int16(_, _) => Used.empty

    case Expression.Int32(_, _) => Used.empty

    case Expression.Int64(_, _) => Used.empty

    case Expression.BigInt(_, _) => Used.empty

    case Expression.Str(_, _) => Used.empty

    case Expression.Wild(_, _, _) => Used.empty

    case Expression.Var(sym, _, _, loc) =>
      if (!sym.isWild())
        Used.of(sym)
      else
        Used.empty + HiddenVarSym(sym, loc)

    case Expression.Def(sym, _, _, _) => Used.of(sym)

    case Expression.Eff(sym, _, _, _) => Used.empty

    case Expression.Hole(sym, _, _, _) => Used.of(sym)

    case Expression.Lambda(fparam, exp, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + fparam.sym

      // Visit the expression with the extended environment.
      val innerUsed = visitExp(exp, env1)

      // Check if the formal parameter is shadowing.
      val shadowedVar = shadowing(fparam.sym, env0)

      // Check if the lambda parameter symbol is dead.
      if (deadVarSym(fparam.sym, innerUsed))
        innerUsed ++ shadowedVar - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        innerUsed ++ shadowedVar - fparam.sym

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      Used.empty ++ us1 ++ us2

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the two expressions under the extended environment.
      val innerUsed1 = visitExp(exp1, env1)
      val innerUsed2 = visitExp(exp2, env1)

      // Check for shadowing.
      val shadowedVar = shadowing(sym, env0)

      // Check if the let-bound variable symbol is dead in exp2.
      if (deadVarSym(sym, innerUsed2))
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym

    case Expression.LetRec(sym, exp1, exp2, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the two expressions under the extended environment.
      val innerUsed1 = visitExp(exp1, env1)
      val innerUsed2 = visitExp(exp2, env1)

      // Check for shadowing.
      val shadowedVar = shadowing(sym, env0)

      // Check if the let-bound variable symbol is dead in exp1 and exp2.
      if (deadVarSym(sym, innerUsed1 ++ innerUsed2))
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      us1 ++ us2 ++ us3

    case Expression.Stm(exp1, exp2, _, _, _) =>
      // TODO: Ensure that `exp1` is non-pure.
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.Match(exp, rules, _, _, _) =>
      // Visit the match expression.
      val usedMatch = visitExp(exp, env0)

      // Visit each match rule.
      val usedRules = rules map {
        case MatchRule(pat, guard, body) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the guard and body.
          val usedGuard = visitExp(guard, extendedEnv)
          val usedBody = visitExp(body, extendedEnv)
          val usedGuardAndBody = usedGuard ++ usedBody

          // Check for unused variable symbols.
          val unusedVarSyms = fvs.filter(sym => deadVarSym(sym, usedGuardAndBody)).map(UnusedVarSym)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = fvs.map(sym => shadowing(sym, env0)).foldLeft(Used.empty)(_ ++ _)

          // Combine everything together.
          usedGuardAndBody -- fvs ++ unusedVarSyms ++ shadowedVarSyms
      }

      usedRules.foldLeft(usedMatch) {
        case (acc, u) => acc ++ u
      }

    case Expression.Switch(rules, _, _, _) =>
      rules.foldLeft(Used.empty) {
        case (acc, (cond, body)) => acc ++ visitExp(cond, env0) ++ visitExp(body, env0)
      }

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      val us = visitExp(exp, env0)
      Used.of(sym, tag) ++ us

    case Expression.Tuple(elms, _, _, _) =>
      visitExps(elms, env0)

    case Expression.RecordEmpty(_, _, _) =>
      Used.empty

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp, env0)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0)
      val us2 = visitExp(rest, env0)
      us1 ++ us2

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest, env0)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      Used.empty ++ visitExps(elms, env0)

    case Expression.ArrayNew(elm, len, _, _, _) =>
      val us1 = visitExp(elm, env0)
      val us2 = visitExp(len, env0)
      Used.empty ++ us1 ++ us2

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      Used.empty ++ us1 ++ us2

    case Expression.ArrayLength(base, _, _, _) =>
      Used.empty ++ visitExp(base, env0)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      val us3 = visitExp(elm, env0)
      Used.empty ++ us1 ++ us2 ++ us3

    case Expression.ArraySlice(base, begin, end, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(begin, env0)
      val us3 = visitExp(end, env0)
      Used.empty ++ us1 ++ us2 ++ us3

    case Expression.VectorLit(elms, _, _, _) =>
      Used.empty ++ visitExps(elms, env0)

    case Expression.VectorNew(elm, _, _, _, _) =>
      Used.empty ++ visitExp(elm, env0)

    case Expression.VectorLoad(base, _, _, _, _) =>
      Used.empty ++ visitExp(base, env0)

    case Expression.VectorStore(base, _, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(elm, env0)
      Used.empty ++ us1 ++ us2

    case Expression.VectorLength(base, _, _, _) =>
      Used.empty ++ visitExp(base, env0)

    case Expression.VectorSlice(base, _, _, _, _, _) =>
      Used.empty ++ visitExp(base, env0)

    case Expression.Ref(exp, _, _, _) =>
      Used.empty ++ visitExp(exp, env0)

    case Expression.Deref(exp, _, _, _) =>
      Used.empty ++ visitExp(exp, env0)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      Used.empty ++ us1 ++ us2

    case Expression.HandleWith(exp, bindings, _, _, _) =>
      val usedExp = visitExp(exp, env0)
      val usedBindings = bindings.foldLeft(Used.empty) {
        case (acc, HandlerBinding(_, body)) => acc ++ visitExp(body, env0)
      }
      usedExp ++ usedBindings

    case Expression.Existential(fparam, exp, _, _) =>
      // Check for variable shadowing.
      val us1 = shadowing(fparam.sym, env0)

      // Visit the expression under an extended environment.
      val us2 = visitExp(exp, env0 + fparam.sym)

      // Check if the quantified variable is dead.
      if (deadVarSym(fparam.sym, us2))
        us1 ++ us2 - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us1 ++ us2 - fparam.sym

    case Expression.Universal(fparam, exp, _, _) =>
      // Check for variable shadowing.
      val us1 = shadowing(fparam.sym, env0)

      // Visit the expression under an extended environment.
      val us2 = visitExp(exp, env0 + fparam.sym)

      // Check if the quantified variable is dead.
      if (deadVarSym(fparam.sym, us2))
        us1 ++ us2 - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us1 ++ us2 - fparam.sym

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.NativeConstructor(_, args, _, _, _) =>
      visitExps(args, env0)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      val usedExp = visitExp(exp, env0)
      val usedRules = rules.foldLeft(Used.empty) {
        case (acc, CatchRule(sym, _, body)) =>
          val usedBody = visitExp(body, env0)
          if (deadVarSym(sym, usedBody))
            acc ++ usedBody + UnusedVarSym(sym)
          else
            acc ++ usedBody
      }
      usedExp ++ usedRules

    case Expression.NativeField(_, _, _, _) =>
      Used.empty

    case Expression.NativeMethod(_, args, _, _, _) =>
      Used.empty ++ visitExps(args, env0)

    case Expression.NewChannel(exp, _, _, _) =>
      Used.empty ++ visitExp(exp, env0)

    case Expression.GetChannel(exp, _, _, _) =>
      Used.empty ++ visitExp(exp, env0)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      Used.empty ++ us1 ++ us2

    case Expression.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultUsed = defaultOpt match {
        case None => Used.empty
        case Some(default) => visitExp(default, env0)
      }

      val rulesUsed = rules map {
        case SelectChannelRule(sym, chan, body) =>
          // Extend the environment with the symbol.
          val env1 = env0 + sym

          // Check for shadowing.
          val shadowedVar = shadowing(sym, env0)

          // Visit the channel and body expressions.
          val chanUsed = visitExp(chan, env1)
          val bodyUsed = visitExp(body, env1)

          // Check if the variable symbol is dead in the body.
          if (deadVarSym(sym, bodyUsed))
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym + UnusedVarSym(sym)
          else
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym
      }

      rulesUsed.foldLeft(Used.empty ++ defaultUsed) {
        case (acc, used) => acc ++ used
      }

    case Expression.ProcessSpawn(exp, _, _, _) => Used.empty ++ visitExp(exp, env0)

    case Expression.ProcessSleep(exp, _, _, _) => Used.empty ++ visitExp(exp, env0)

    case Expression.ProcessPanic(msg, _, _, _) => Used.empty

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Used.empty) {
        case (used, con) => used ++ visitConstraint(con, env0)
      }

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp, env0)

    case Expression.FixpointProject(sym, exp, _, _, _) =>
      val us = visitExp(exp, env0)
      Used.of(sym) ++ us

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      val used1 = visitExp(exp1, env0)
      val used2 = visitExp(exp2, env0)
      used1 ++ used2

    case Expression.FixpointFold(sym, exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      Used.of(sym) ++ us1 ++ us2 ++ us3
  }

  /**
    * Returns the symbols used in the given list of expressions `es` under the given environment `env0`.
    */
  private def visitExps(es: List[Expression], env0: Env): Used =
    es.foldLeft(Used.empty) {
      case (acc, exp) => acc ++ visitExp(exp, env0)
    }

  /**
    * Returns the symbols used in the given constraint `c0` under the given environment `env0`.
    */
  private def visitConstraint(c0: Constraint, env0: Env): Used = {
    val zero = visitHeadPred(c0.head, env0)
    val used = c0.body.foldLeft(zero) {
      case (acc, b) => acc ++ visitBodyPred(b, env0)
    }
    used -- c0.cparams.map(_.sym)
  }

  /**
    * Returns the symbols used in the given head predicate `h0` under the given environment `env0`.
    */
  private def visitHeadPred(h0: Predicate.Head, env0: Env): Used = h0 match {
    case Head.Atom(sym, _, terms, _, _) =>
      Used.of(sym) ++ visitExps(terms, env0)

    case Head.Union(exp, _, _) =>
      visitExp(exp, env0)
  }

  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env): Used = b0 match {
    case Body.Atom(sym, _, _, terms, _, _) =>
      Used.of(sym)

    case Body.Guard(exp, _) =>
      visitExp(exp, env0)
  }

  /**
    * Returns the free variables in the pattern `p0`.
    */
  private def freeVars(p0: Pattern): Set[Symbol.VarSym] = p0 match {
    case Pattern.Wild(_, _) => Set.empty
    case Pattern.Var(sym, _, _) => Set(sym)
    case Pattern.Unit(loc) => Set.empty
    case Pattern.True(loc) => Set.empty
    case Pattern.False(loc) => Set.empty
    case Pattern.Char(_, _) => Set.empty
    case Pattern.Float32(_, _) => Set.empty
    case Pattern.Float64(_, _) => Set.empty
    case Pattern.Int8(_, _) => Set.empty
    case Pattern.Int16(_, _) => Set.empty
    case Pattern.Int32(_, _) => Set.empty
    case Pattern.Int64(_, _) => Set.empty
    case Pattern.BigInt(_, _) => Set.empty
    case Pattern.Str(_, _) => Set.empty
    case Pattern.Tag(_, _, pat, _, _) => freeVars(pat)
    case Pattern.Tuple(pats, _, _) => pats.foldLeft(Set.empty[Symbol.VarSym]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
    case Pattern.Array(elms, _, _) => elms.foldLeft(Set.empty[Symbol.VarSym]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
    case Pattern.ArrayTailSpread(elms, _, _, _) => elms.foldLeft(Set.empty[Symbol.VarSym]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
    case Pattern.ArrayHeadSpread(_, elms, _, _) => elms.foldLeft(Set.empty[Symbol.VarSym]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
  }

  /**
    * Checks whether the variable symbol `sym` shadows another variable in the environment `env`.
    */
  private def shadowing(sym: Symbol.VarSym, env: Env): Used =
    env.varSyms.get(sym.text) match {
      case None =>
        Used.empty
      case Some(shadowingVar) =>
        if (sym.isWild())
          Used.empty
        else
          Used.empty + ShadowedVar(shadowingVar, sym)
    }

  /**
    * Returns `true` if the given definition `decl` is unused according to `used`.
    */
  private def deadDef(decl: Def, used: Used)(implicit root: Root): Boolean =
    !decl.ann.isTest &&
      !decl.ann.isTheorem &&
      !decl.mod.isPublic &&
      !decl.sym.name.equals("main") &&
      !decl.sym.name.startsWith("_") &&
      !used.defSyms.contains(decl.sym) &&
      !root.reachable.contains(decl.sym)

  /**
    * Returns `true` if the given relation `decl` is unused according to `used`.
    */
  private def deadRel(decl: Relation, used: Used): Boolean =
    !decl.mod.isPublic &&
      !decl.sym.name.startsWith("_") &&
      !used.predSyms.contains(decl.sym)

  /**
    * Returns `true` if the given lattice `decl` is unused according to `used`.
    */
  private def deadLat(decl: Lattice, used: Used): Boolean =
    !decl.mod.isPublic &&
      !decl.sym.name.startsWith("_") &&
      !used.predSyms.contains(decl.sym)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def deadTypeVar(tvar: Type.Var, used: Set[Type.Var]): Boolean =
    !used.contains(tvar)

  /**
    * Returns `true` if the local variable `tvar` is unused according to the argument `used`.
    */
  private def deadVarSym(sym: Symbol.VarSym, used: Used): Boolean =
    !sym.text.startsWith("_") &&
      !used.varSyms.contains(sym)

  /**
    * Returns `true` if the given effect `eff` is pure.
    */
  private def isPure(eff: ast.Eff): Boolean = eff match {
    case ast.Eff.Var(_) => true
    case ast.Eff.Pure => true
    case ast.Eff.Impure => false
  }

  /**
    * Companion object for the [[Env]] class.
    */
  private object Env {
    /**
      * Represents the empty environment.
      */
    val empty: Env = Env(Map.empty)

    /**
      * Returns an environment with the given variable symbols `varSyms` in it.
      */
    def of(varSyms: Iterable[Symbol.VarSym]): Env = varSyms.foldLeft(Env.empty) {
      case (acc, sym) => acc + sym
    }
  }

  /**
    * Represents an environment.
    */
  private case class Env(varSyms: Map[String, Symbol.VarSym]) {
    /**
      * Updates `this` environment with a new variable symbol `sym`.
      */
    def +(sym: Symbol.VarSym): Env = {
      copy(varSyms = varSyms + (sym.text -> sym))
    }

    /**
      * Updates `this` environment with a set of new variable symbols `varSyms`.
      */
    def ++(vs: Iterable[Symbol.VarSym]): Env = vs.foldLeft(Env.empty) {
      case (acc, sym) => acc + sym
    }
  }

  private object Used {

    /**
      * Represents the empty set of used symbols.
      */
    val empty: Used = Used(MultiMap.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)

    /**
      * Returns an object where the given enum symbol `sym` and `tag` are marked as used.
      */
    def of(sym: Symbol.EnumSym, tag: String): Used = empty.copy(enumSyms = MultiMap.empty + (sym, tag))

    /**
      * Returns an object where the given defn symbol `sym` is marked as used.
      */
    def of(sym: Symbol.DefnSym): Used = empty.copy(defSyms = Set(sym))

    /**
      * Returns an object where the given hole symbol `sym` is marked as used.
      */
    def of(sym: Symbol.HoleSym): Used = empty.copy(holeSyms = Set(sym))

    /**
      * Returns an object where the given predicate symbol `sym` is marked as used.
      */
    def of(sym: Symbol.PredSym): Used = empty.copy(predSyms = Set(sym))

    /**
      * Returns an object where the given variable symbol `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Used = empty.copy(varSyms = Set(sym))

  }

  /**
    * A representation of used symbols.
    */
  private case class Used(enumSyms: MultiMap[Symbol.EnumSym, String],
                          defSyms: Set[Symbol.DefnSym],
                          predSyms: Set[Symbol.PredSym],
                          holeSyms: Set[Symbol.HoleSym],
                          varSyms: Set[Symbol.VarSym],
                          errors: Set[RedundancyError]) {
    /**
      * Merges `this` and `that`.
      */
    def ++(that: Used): Used =
      if (this eq that) {
        this
      } else if (this eq Used.empty) {
        that
      } else if (that eq Used.empty) {
        this
      } else {
        Used(
          this.enumSyms ++ that.enumSyms,
          this.defSyms ++ that.defSyms,
          this.predSyms ++ that.predSyms,
          this.holeSyms ++ that.holeSyms,
          this.varSyms ++ that.varSyms,
          this.errors ++ that.errors
        )
      }

    /**
      * Adds the given redundancy error `e` to `this` object.
      */
    def +(e: RedundancyError): Used = copy(errors = errors + e)

    /**
      * Adds the given traversable of redundancy errors `es` to `this` object.
      */
    def ++(es: Iterable[RedundancyError]): Used =
      if (es.isEmpty) this else copy(errors = errors ++ es)

    /**
      * Marks the given variable symbol `sym` as used.
      */
    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)

    /**
      * Marks all the given variable symbols `syms` as used.
      */
    def --(syms: Iterable[Symbol.VarSym]): Used =
      if (syms.isEmpty) this else copy(varSyms = varSyms -- syms)

    /**
      * Returns Successful(a) unless `this` contains errors.
      */
    def toValidation[A](a: A): Validation[A, RedundancyError] = if (errors.isEmpty) Success(a) else Failure(errors.to(LazyList))
  }

  /**
    * Companion object of the [[Substitution]] class.
    */
  private object Substitution {
    /**
      * The empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)
  }

  /**
    * A substitution is a map from variable symbols to patterns.
    */
  private case class Substitution(m: Map[Symbol.VarSym, Pattern]) {
    /**
      * Applies `this` substitution to the given pattern `pat0`.
      */
    def apply(pat0: Pattern): Pattern = pat0 match {
      case Pattern.Wild(_, _) => pat0
      case Pattern.Var(sym, _, _) => m.get(sym) match {
        case None => pat0
        case Some(pat) => pat
      }
      case Pattern.Unit(_) => pat0
      case Pattern.True(_) => pat0
      case Pattern.False(_) => pat0
      case Pattern.Char(_, _) => pat0
      case Pattern.Float32(_, _) => pat0
      case Pattern.Float64(_, _) => pat0
      case Pattern.Int8(_, _) => pat0
      case Pattern.Int16(_, _) => pat0
      case Pattern.Int32(_, _) => pat0
      case Pattern.Int64(_, _) => pat0
      case Pattern.BigInt(_, _) => pat0
      case Pattern.Str(_, _) => pat0
      case Pattern.Tag(sym, tag, pat, tpe, loc) => Pattern.Tag(sym, tag, apply(pat), tpe, loc)
      case Pattern.Tuple(elms, tpe, loc) => Pattern.Tuple(apply(elms), tpe, loc)
      case Pattern.Array(elms, tpe, loc) => Pattern.Array(apply(elms), tpe, loc)
      //TODO: The sym is not handled correctly.
      case Pattern.ArrayTailSpread(elms, sym, tpe, loc) => m.get(sym) match {
        case None => Pattern.ArrayTailSpread(apply(elms), sym, tpe, loc)
        case Some(pat) => Pattern.ArrayTailSpread(apply(elms), sym, tpe, loc)
      }
      case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) => m.get(sym) match {
        case None => Pattern.ArrayHeadSpread(sym, apply(elms), tpe, loc)
        case Some(pat) => Pattern.ArrayHeadSpread(sym, apply(elms), tpe, loc)
      }
    }

    /**
      * Applies `this` substitution to the given patterns `ps`.
      */
    def apply(ps: List[Pattern]): List[Pattern] = ps map apply

    /**
      * Returns the left-biased composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      Substitution(this.m ++ that.m.filter(kv => !this.m.contains(kv._1)))
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      val m = that.m.foldLeft(Map.empty[Symbol.VarSym, Pattern]) {
        case (macc, (x, t)) => macc.updated(x, this.apply(t))
      }
      Substitution(m) ++ this
    }
  }

}
