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
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.MultiMap
import ca.uwaterloo.flix.util.{InternalRuntimeException, Result, Validation}

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
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Redundancy") {
    // Return early if the redundancy phase is disabled.
    if (flix.options.xallowredundancies) {
      return root.toSuccess
    }

    // Computes all used symbols in all defs (in parallel).
    val usedDefs = root.defs.par.aggregate(Used.Neutral)({
      case (acc, (sym, decl)) => acc ++ visitDef(decl)(root)
    }, _ ++ _)

    // Computes all used symbols in all lattices.
    val usedLats = root.latticeComponents.values.foldLeft(Used.Neutral) {
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
  private def visitDef(defn: TypedAst.Def)(implicit root: Root): Used = {

    /**
      * Checks for unused formal parameters.
      */
    def checkUnusedFormalParameters(used: Used): Used = {
      val unusedParams = defn.fparams.collect {
        case fparam if dead(fparam.sym, used) => UnusedFormalParam(fparam.sym)
      }
      used ++ unusedParams
    }

    /**
      * Checks for unused type parameters.
      */
    def checkUnusedTypeParameters(used: Used): Used = {
      val unusedParams = defn.tparams.collect {
        case tparam if dead(tparam.tpe, defn.sc.base.typeVars) => UnusedTypeParam(tparam.name)
      }
      used ++ unusedParams
    }

    // Compute the used symbols inside the definition.
    val usedExp = visitExp(defn.exp, Env.of(defn.fparams.map(_.sym)))

    // TODO: Check the expression for redundant patterns:
    val flaf = checkExp(RedundantPatternsCatalog.Id, defn.exp)
    if (flaf) {
      println("Found redundancy!")
    }

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
      case (acc, (_, decl)) if dead(decl, used) => acc + UnusedDefSym(decl.sym)
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
    root.enums.foldLeft(Used.Neutral) {
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
    root.relations.foldLeft(Used.Neutral) {
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
    root.lattices.foldLeft(Used.Neutral) {
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
      case (sym, decl) if dead(decl, used) => UnusedRelSym(sym)
    }
    used ++ unusedRelSyms
  }

  /**
    * Checks for unused lattice symbols.
    */
  private def checkUnusedLattices(used: Redundancy.Used)(implicit root: Root): Used = {
    val unusedLatSyms = root.lattices.collect {
      case (sym, decl) if dead(decl, used) => UnusedLatSym(sym)
    }
    used ++ unusedLatSyms
  }

  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: TypedAst.Expression, env0: Env): Used = e0 match {
    case Expression.Unit(_) => Used.Pure

    case Expression.True(_) => Used.Pure

    case Expression.False(_) => Used.Pure

    case Expression.Char(_, _) => Used.Pure

    case Expression.Float32(_, _) => Used.Pure

    case Expression.Float64(_, _) => Used.Pure

    case Expression.Int8(_, _) => Used.Pure

    case Expression.Int16(_, _) => Used.Pure

    case Expression.Int32(_, _) => Used.Pure

    case Expression.Int64(_, _) => Used.Pure

    case Expression.BigInt(_, _) => Used.Pure

    case Expression.Str(_, _) => Used.Pure

    case Expression.Wild(_, _, _) => Used.Pure

    case Expression.Var(sym, _, _, loc) =>
      if (!sym.isWild())
        Used.of(sym)
      else
        Used.Pure + HiddenVarSym(sym, loc)

    case Expression.Def(sym, _, _, _) => Used.of(sym)

    case Expression.Eff(sym, _, _, _) => Used.Impure

    case Expression.Hole(sym, _, _, _) => Used.of(sym)

    case Expression.Lambda(fparam, exp, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + fparam.sym

      // Visit the expression with the extended environment.
      val innerUsed = visitExp(exp, env1)

      // Check if the formal parameter is shadowing.
      val shadowedVar = shadowing(fparam.sym, env0)

      // Check if the lambda parameter symbol is dead.
      if (dead(fparam.sym, innerUsed))
        innerUsed ++ shadowedVar - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        innerUsed ++ shadowedVar - fparam.sym

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      // TODO: Check the effect of exp1
      us1 ++ us2

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
      if (dead(sym, innerUsed2))
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
      if (dead(sym, innerUsed1 ++ innerUsed2))
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      us1 ++ us2 ++ us3

    case Expression.Stm(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      if (us1.pure)
        us1 ++ us2 + UselessExpression(exp1.loc)
      else
        us1 ++ us2

    case Expression.Match(exp, rules, _, _, _) =>
      // Visit the match expression.
      val usedMatch = visitExp(exp, env0)

      // Determine if the match expression is a stable path.
      val stablePath = getStablePath(exp)

      // Visit each match rule.
      val usedRules = rules map {
        case MatchRule(pat, guard, body) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables and the stable path.
          val extendedEnv = env0 ++ fvs + (stablePath -> pat)

          // Visit the guard and body.
          val usedGuard = visitExp(guard, extendedEnv)
          val usedBody = visitExp(body, extendedEnv)
          val usedGuardAndBody = usedGuard ++ usedBody

          // Check for a useless pattern match.
          val uselessMatch = stablePath match {
            case None => Used.Neutral // nop
            case Some(sp) => checkUselessPatternMatch(sp, env0, pat)
          }

          // Check for unused variable symbols.
          val unusedVarSyms = fvs.filter(sym => dead(sym, usedGuardAndBody)).map(UnusedVarSym)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = fvs.map(sym => shadowing(sym, env0)).foldLeft(Used.Neutral)(_ ++ _)

          // Combine everything together.
          usedGuardAndBody -- fvs ++ uselessMatch ++ unusedVarSyms ++ shadowedVarSyms
      }

      usedRules.foldLeft(usedMatch) {
        case (acc, u) => acc ++ u
      }

    case Expression.Switch(rules, _, _, _) =>
      rules.foldLeft(Used.Neutral) {
        case (acc, (cond, body)) => acc ++ visitExp(cond, env0) ++ visitExp(body, env0)
      }

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      val us = visitExp(exp, env0)
      Used.of(sym, tag) ++ us

    case Expression.Tuple(elms, _, _, _) =>
      visitExps(elms, env0)

    case Expression.RecordEmpty(_, _, _) =>
      Used.Pure

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp, env0)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0)
      val us2 = visitExp(rest, env0)
      us1 ++ us2

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest, env0)

    // TODO: Technically which of these have side-effects/are purty/referencentially transparent?
    // TODO: Need observable distinction?

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      Used.Impure ++ visitExps(elms, env0)

    case Expression.ArrayNew(elm, len, _, _, _) =>
      val us1 = visitExp(elm, env0)
      val us2 = visitExp(len, env0)
      Used.Impure ++ us1 ++ us2

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      Used.Impure ++ us1 ++ us2

    case Expression.ArrayLength(base, _, _, _) =>
      Used.Impure ++ visitExp(base, env0)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      val us3 = visitExp(elm, env0)
      Used.Impure ++ us1 ++ us2 ++ us3

    case Expression.ArraySlice(base, begin, end, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(begin, env0)
      val us3 = visitExp(end, env0)
      Used.Impure ++ us1 ++ us2 ++ us3

    case Expression.VectorLit(elms, _, _, _) =>
      Used.Impure ++ visitExps(elms, env0)

    case Expression.VectorNew(elm, _, _, _, _) =>
      Used.Impure ++ visitExp(elm, env0)

    case Expression.VectorLoad(base, _, _, _, _) =>
      Used.Impure ++ visitExp(base, env0)

    case Expression.VectorStore(base, _, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(elm, env0)
      Used.Impure ++ us1 ++ us2

    case Expression.VectorLength(base, _, _, _) =>
      Used.Impure ++ visitExp(base, env0)

    case Expression.VectorSlice(base, _, _, _, _, _) =>
      Used.Impure ++ visitExp(base, env0)

    case Expression.Ref(exp, _, _, _) =>
      Used.Impure ++ visitExp(exp, env0)

    case Expression.Deref(exp, _, _, _) =>
      Used.Impure ++ visitExp(exp, env0)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      Used.Impure ++ us1 ++ us2

    case Expression.HandleWith(exp, bindings, _, _, _) =>
      val usedExp = visitExp(exp, env0)
      val usedBindings = bindings.foldLeft(Used.Neutral) {
        case (acc, HandlerBinding(_, body)) => acc ++ visitExp(body, env0)
      }
      usedExp ++ usedBindings

    case Expression.Existential(fparam, exp, _, _) =>
      // Check for variable shadowing.
      val us1 = shadowing(fparam.sym, env0)

      // Visit the expression under an extended environment.
      val us2 = visitExp(exp, env0 + fparam.sym)

      // Check if the quantified variable is dead.
      if (dead(fparam.sym, us2))
        us1 ++ us2 - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us1 ++ us2 - fparam.sym

    case Expression.Universal(fparam, exp, _, _) =>
      // Check for variable shadowing.
      val us1 = shadowing(fparam.sym, env0)

      // Visit the expression under an extended environment.
      val us2 = visitExp(exp, env0 + fparam.sym)

      // Check if the quantified variable is dead.
      if (dead(fparam.sym, us2))
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
      val usedRules = rules.foldLeft(Used.Neutral) {
        case (acc, CatchRule(sym, _, body)) =>
          val usedBody = visitExp(body, env0)
          if (dead(sym, usedBody))
            acc ++ usedBody + UnusedVarSym(sym)
          else
            acc ++ usedBody
      }
      usedExp ++ usedRules

    case Expression.NativeField(_, _, _, _) =>
      Used.Pure

    case Expression.NativeMethod(_, args, _, _, _) =>
      Used.Impure ++ visitExps(args, env0)

    // TODO: Observerable?
    case Expression.NewChannel(exp, _, _, _) =>
      Used.Impure ++ visitExp(exp, env0)

    case Expression.GetChannel(exp, _, _, _) =>
      Used.Impure ++ visitExp(exp, env0)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      Used.Impure ++ us1 ++ us2

    case Expression.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultUsed = defaultOpt match {
        case None => Used.Neutral
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
          if (dead(sym, bodyUsed))
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym + UnusedVarSym(sym)
          else
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym
      }

      rulesUsed.foldLeft(defaultUsed) {
        case (acc, used) => acc ++ used
      }

    // TODO: Observervable?
    case Expression.ProcessSpawn(exp, _, _, _) => Used.Impure ++ visitExp(exp, env0)

    case Expression.ProcessSleep(exp, _, _, _) => Used.Impure ++ visitExp(exp, env0)

    case Expression.ProcessPanic(msg, _, _, _) => Used.Impure

    case Expression.FixpointConstraint(c, _, _, _) =>
      visitConstraint(c, env0)

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.FixpointSolve(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.FixpointProject(pred, exp, _, _, _) =>
      val us1 = visitExp(pred.exp, env0)
      val us2 = visitExp(exp, env0)
      Used.of(pred.sym) ++ us1 ++ us2

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      val used1 = visitExp(exp1, env0)
      val used2 = visitExp(exp2, env0)
      used1 ++ used2
  }

  /**
    * Returns the symbols used in the given list of expressions `es` under the given environment `env0`.
    */
  private def visitExps(es: List[TypedAst.Expression], env0: Env): Used =
    es.foldLeft(Used.Neutral) {
      case (acc, exp) => acc ++ visitExp(exp, env0)
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
    case Head.Atom(pred, terms, _, _) =>
      Used.of(pred.sym) ++ visitExp(pred.exp, env0) ++ visitExps(terms, env0)
  }

  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env): Used = b0 match {
    case Body.Atom(pred, _, terms, _, _) =>
      Used.of(pred.sym) ++ visitExp(pred.exp, env0)

    case Body.Filter(sym, terms, _) =>
      Used.of(sym) ++ visitExps(terms, env0)

    case Body.Functional(sym, term, _) =>
      Used.of(sym) ++ visitExp(term, env0)
  }

  /**
    * Checks whether the given stable path `sp0` under the environment `env0` is useless when matched against the pattern `pat0`.
    */
  private def checkUselessPatternMatch(sp0: StablePath, env0: Env, pat0: Pattern): Used = {
    env0.pats.get(sp0) match {
      case None =>
        // The stable path is free. The pattern match is never useless.
        Used.Neutral
      case Some(pat2) =>
        // The stable path a pattern, check if `pat0` and `pat2` are compatible.
        unify(pat0, pat2) match {
          case Ok(subst) =>
            // TODO: Should the subst not be applied to env0?
            // The patterns unify, the pattern match is not useless.
            Used.Neutral

          case Err(error) =>
            // The patterns do not unify, the pattern match is useless.
            Used.Neutral + error
        }
    }
  }

  /**
    * Checks whether the variable symbol `sym` shadows another variable in the environment `env`.
    */
  private def shadowing(sym: Symbol.VarSym, env: Env): Used =
    env.varSyms.get(sym.text) match {
      case None =>
        Used.Neutral
      case Some(shadowingVar) =>
        if (sym.isWild())
          Used.Neutral
        else
          Used.Neutral + ShadowedVar(shadowingVar, sym)
    }

  /**
    * Returns `true` if the given definition `decl` is unused according to `used`.
    */
  private def dead(decl: Def, used: Used)(implicit root: Root): Boolean =
    !decl.ann.isTest &&
      !decl.mod.isPublic &&
      !decl.sym.name.equals("main") &&
      !decl.sym.name.startsWith("_") &&
      !used.defSyms.contains(decl.sym) &&
      !root.reachable.contains(decl.sym)

  /**
    * Returns `true` if the given relation `decl` is unused according to `used`.
    */
  private def dead(decl: Relation, used: Used): Boolean =
    !decl.mod.isPublic &&
      !decl.sym.name.startsWith("_") &&
      !used.predSyms.contains(decl.sym)

  /**
    * Returns `true` if the given lattice `decl` is unused according to `used`.
    */
  private def dead(decl: Lattice, used: Used): Boolean =
    !decl.mod.isPublic &&
      !decl.sym.name.startsWith("_") &&
      !used.predSyms.contains(decl.sym)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def dead(tvar: Type.Var, used: Set[Type.Var]): Boolean =
    !used.contains(tvar)

  /**
    * Returns `true` if the local variable `tvar` is unused according to the argument `used`.
    */
  private def dead(sym: Symbol.VarSym, used: Used): Boolean =
    !sym.text.startsWith("_") &&
      !used.varSyms.contains(sym)

  /**
    * Attempts to unify the two given patterns `p1` and `p2`.
    *
    * Returns a substitution if successful. Otherwise returns a redundancy error.
    */
  private def unify(p1: Pattern, p2: Pattern): Result[Substitution, RedundancyError] = (p1, p2) match {
    case (Pattern.Wild(_, _), _) => Ok(Substitution.empty)

    case (_, Pattern.Wild(_, _)) => Ok(Substitution.empty)

    // TODO: Have to check that there is no infinite recursion here...
    case (Pattern.Var(sym1, _, _), Pattern.Var(sym2, _, _)) => Ok(Substitution(Map(sym1 -> p2)))

    case (Pattern.Var(sym, _, _), _) => Ok(Substitution(Map(sym -> p2)))

    case (_, Pattern.Var(sym, _, _)) => Ok(Substitution(Map(sym -> p1)))

    case (Pattern.Unit(_), Pattern.Unit(_)) => Ok(Substitution.empty)

    case (Pattern.True(_), Pattern.True(_)) => Ok(Substitution.empty)

    case (Pattern.False(_), Pattern.False(_)) => Ok(Substitution.empty)

    case (Pattern.Char(lit1, _), Pattern.Char(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Float32(lit1, _), Pattern.Float32(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Float64(lit1, _), Pattern.Float64(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Int8(lit1, _), Pattern.Int8(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Int16(lit1, _), Pattern.Int16(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Int32(lit1, _), Pattern.Int32(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Int64(lit1, _), Pattern.Int64(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.BigInt(lit1, _), Pattern.BigInt(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Str(lit1, _), Pattern.Str(lit2, _)) if lit1 == lit2 => Ok(Substitution.empty)

    case (Pattern.Tag(_, tag1, pat1, _, _), Pattern.Tag(_, tag2, pat2, _, _)) if tag1 == tag2 => unify(pat1, pat2)

    case (Pattern.Tuple(elms1, _, _), Pattern.Tuple(elms2, _, _)) => unifyAll(elms1, elms2)

    case _ => Err(RedundancyError.UselessPatternMatch(p2.toString, p1.loc, p2.loc))
  }

  /**
    * Attempts to unify the two given list of patterns `ps1` and `ps2`.
    */
  private def unifyAll(ps1: List[Pattern], ps2: List[Pattern]): Result[Substitution, RedundancyError] = (ps1, ps2) match {
    case (Nil, Nil) => Ok(Substitution.empty)
    case (x :: xs, y :: ys) =>
      unify(x, y) flatMap {
        case subst => unifyAll(subst(xs), subst(ys))
      }
    case _ => throw InternalRuntimeException(s"Unexpected patterns: '$ps1' and '$ps2'.")
  }

  /**
    * Optionally returns the given expression `e0` as a stable path.
    */
  private def getStablePath(e0: Expression): Option[StablePath] = e0 match {
    // Variables are always stable.
    case Expression.Var(sym, _, _, _) =>
      Some(StablePath.Var(sym))

    // Record selections is stable if the underlying expression is stable.
    case Expression.RecordSelect(exp, label, _, _, _) =>
      for {
        sp <- getStablePath(exp)
      } yield StablePath.RecordSelect(sp, label)

    // Nothing else is stable.
    case _ => None
  }

  /**
    * Represents stable paths.
    */
  sealed trait StablePath

  object StablePath {

    /**
      * A variable expression is a stable path.
      */
    case class Var(sym: Symbol.VarSym) extends StablePath

    /**
      * A record select expression is a stable path.
      */
    case class RecordSelect(sp: StablePath, label: String) extends StablePath

  }

  /**
    * Companion object for the [[Env]] class.
    */
  object Env {
    /**
      * Represents the empty environment.
      */
    val empty: Env = Env(Map.empty, Map.empty)

    /**
      * Returns an environment with the given variable symbols `varSyms` in it.
      */
    def of(varSyms: Traversable[Symbol.VarSym]): Env = varSyms.foldLeft(Env.empty) {
      case (acc, sym) => acc + sym
    }
  }

  /**
    * Represents an environment.
    */
  case class Env(varSyms: Map[String, Symbol.VarSym], pats: Map[StablePath, Pattern]) {
    /**
      * Updates `this` environment with a new variable symbol `sym`.
      */
    def +(sym: Symbol.VarSym): Env = {
      copy(varSyms = varSyms + (sym.text -> sym))
    }

    /**
      * Updates `this` environment with a new pattern for the given stable path.
      */
    def +(p: (Option[StablePath], Pattern)): Env = p match {
      case (None, _) => this
      case (Some(sp), pat) => copy(pats = pats + (sp -> pat))
    }

    /**
      * Updates `this` environment with a set of new variable symbols `varSyms`.
      */
    def ++(vs: Traversable[Symbol.VarSym]): Env = vs.foldLeft(Env.empty) {
      case (acc, sym) => acc + sym
    }
  }

  object Used {

    /**
      * Represents something pure that has no uses.
      */
    val Pure: Used = Used(MultiMap.empty, Set.empty, Set.empty, Set.empty, Set.empty, pure = true, Set.empty)

    /**
      * Represents something impure that has no uses.
      */
    val Impure: Used = Used(MultiMap.empty, Set.empty, Set.empty, Set.empty, Set.empty, pure = false, Set.empty)

    /**
      * The neutral element.
      */
    val Neutral: Used = Pure

    /**
      * Returns an object where the given enum symbol `sym` and `tag` are marked as used.
      */
    def of(sym: Symbol.EnumSym, tag: String): Used = Pure.copy(enumSyms = MultiMap.empty + (sym, tag))

    /**
      * Returns an object where the given defn symbol `sym` is marked as used.
      */
    def of(sym: Symbol.DefnSym): Used = Pure.copy(defSyms = Set(sym))

    /**
      * Returns an object where the given hole symbol `sym` is marked as used.
      */
    def of(sym: Symbol.HoleSym): Used = Pure.copy(holeSyms = Set(sym))

    /**
      * Returns an object where the given predicate symbol `sym` is marked as used.
      */
    def of(sym: Symbol.PredSym): Used = Pure.copy(predSyms = Set(sym))

    /**
      * Returns an object where the given variable symbol `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Used = Pure.copy(varSyms = Set(sym))

  }

  /**
    * A representation of used symbols.
    */
  case class Used(enumSyms: MultiMap[Symbol.EnumSym, String],
                  defSyms: Set[Symbol.DefnSym],
                  predSyms: Set[Symbol.PredSym],
                  holeSyms: Set[Symbol.HoleSym],
                  varSyms: Set[Symbol.VarSym],
                  pure: Boolean,
                  errors: Set[RedundancyError]) {
    /**
      * Merges `this` and `that`.
      */
    def ++(that: Used): Used =
      if (this eq that) {
        this
      } else if (this eq Used.Pure) {
        that
      } else if (that eq Used.Pure) {
        this
      } else {
        Used(
          this.enumSyms ++ that.enumSyms,
          this.defSyms ++ that.defSyms,
          this.predSyms ++ that.predSyms,
          this.holeSyms ++ that.holeSyms,
          this.varSyms ++ that.varSyms,
          this.pure && that.pure,
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
    def ++(es: Traversable[RedundancyError]): Used =
      if (es.isEmpty) this else copy(errors = errors ++ es)

    /**
      * Marks the given variable symbol `sym` as used.
      */
    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)

    /**
      * Marks all the given variable symbols `syms` as used.
      */
    def --(syms: Traversable[Symbol.VarSym]): Used =
      if (syms.isEmpty) this else copy(varSyms = varSyms -- syms)

    /**
      * Returns Successful(a) unless `this` contains errors.
      */
    def toValidation[A](a: A): Validation[A, RedundancyError] = if (errors.isEmpty) Success(a) else Failure(errors.toStream)
  }

  sealed trait Purity

  object Purity {

    case object Pure extends Purity

    case object Ephemeral extends Purity

    case object Impure extends Purity

  }

  /**
    * Companion object of the [[Substitution]] class.
    */
  object Substitution {
    /**
      * The empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)
  }

  /**
    * A substitution is a map from variable symbols to patterns.
    */
  case class Substitution(m: Map[Symbol.VarSym, Pattern]) {
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


  /////////////////////////////////////////////////////////////////////////////
  // TODOs
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Compile to automaton or similar?

  sealed trait RedundantPat

  object RedundantPat {

    case object Wildcard extends RedundantPat

    case object Identity extends RedundantPat

    case class Def(sym: Symbol.VarSym) extends RedundantPat

    case class Apply(pat1: RedundantPat, pat2: RedundantPat) extends RedundantPat

  }

  object RedundantPatternsCatalog {

    import RedundantPat._

    val Id: RedundantPat = Identity

    val ApplyId: RedundantPat = Apply(Identity, Wildcard)

  }

  // TODO: Might be better to do the opposite: Parse an exression into a pattern. If succesfull then check
  // the non context-free variables to see if there is a problem or not.

  private def checkExp(r: RedundantPat, e0: TypedAst.Expression): Boolean = r match {
    case RedundantPat.Wildcard => true
    case RedundantPat.Identity => e0 match {
      case Expression.Lambda(fparam, exp, _, _, _) =>
        exp match {
          case Expression.Var(sym, _, _, _) => fparam.sym == sym
          case _ => false
        }
      case _ => false
    }
    case RedundantPat.Def(sym) => false
    case RedundantPat.Apply(pat1, pat2) => false
  }

  // TODO: Use cases to find:
  // TODO:  - List.map(x -> x, _)
  // TODO:  - List.map(_, Nil)
  // TODO:  - List.length(Nil)

  // TODO: Should we also consider tricky cases such as:
  // match s with {
  // case Circle(Red) =>
  // case Circle(x) =>
  // where we know that x cannot be red, because that would have matched?
  // What about nested patterns like:
  // def main(): Int =
  //    let s = Circle(Red);
  //    match s with {
  //        case Circle(Red) => 123
  //        case Square(Blu) => match s with {
  //            case Square(Red) =>
  //        }
  //        case _ => Square(Blu)
  //    }

  // TODO: Define a notion of contradiction:
  // P(x) and Q(x) cannot be true at the same time.
  // E.g. x == 0 and x == 1, or isEmpty(xs) and nonEmpty(xs).
  // But isEmpty(r.l) and nonEmpty(r.l) cannot be true at the same time.
  // TODO: Question is how to deal with the grammar. And how to represent these things.
  // TODO: How to deal with conjunctions and disjunctions?

  // TODO: We want to find computations that are always true or always false.

  // TODO: Why not move shadowing checks in here?

  // TODO: Add more test cases for UselessExpression

  // TODO: How to deal with ArrayNew and ArrayLoad as these are impure, but should still have their result observed.

  /////////////////////////////////////////////////////////////////////////////
  // Paper Notes
  /////////////////////////////////////////////////////////////////////////////

  // Papers:
  // - Finding Application Errors and Security Flaws Using PQL: a Program Query Language
  // - Using SCL to Specify and Check Design Intent in Source Code
  // - A Framework for Source Code Search using Program Patterns

  // Notes for the paper:
  // - We disallow shadowing (because its confusing in the presence of pattern matching).
  // - We disallow both implicit widening and narrowing of integers.
  // - We disallow all forms of implicit coercions.
  // - We disallow linear patterns.
  // - We treat holes (and ???) as using all local variables (but not anything else?)
  // - We implement the checker using a fork-join style monoid thingy.
  // - If we allow shadowing then that might lead to "mysterious" unused variable warnings.

  // Questions:
  // - When is an enum used? Is it enough to (a) mention its type, (b) to use it in a pat match, or (c) to actually construct a value.
  //     (What if you match on a value of that type, but use a wildcard?)
  //     (What is consistent with the Void enum and the singleton enum?)
  // - When is a predicate used? Is it enough to use it in a rule, or must it also appear in a head predicate?
  // - How do we appropriately distinguish between the effect of NewChannel and e.g. PutChannel?
  //     (How do we deal with return values that must be used, e.g. deleteFile?)

  // Bugs found:
  // - Missing @test on def testArrayLength42(): Int = let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]]; length[x]

  // Shadowing gone wrong:
  //     case Expression.FixpointProject(pred, exp, _, _, _) =>
  //      val PredicateWithParam(sym, exp) = pred
  //      mapN(visitExp(pred.exp, env0), visitExp(exp, env0)) {
  //        case (used1, used2) => Used.of(sym) ++ used1 ++ used2
  //      }

  // Shadowing in action:
  // let childList : List[Path] = Nil;
  // let childList = childrenHelper(dirIterator, childList);

  // Count impacted test cases?

  // thm \forall f: List.filter(f, Nil) = Nil
  // false <= List.isEmpty(xs), List.nonEmpty(xs).

  // Ideas from: Using Redundancies to Find Errors

  // [Idempotent operations]: (1) Assign to self, (2) divide by itself, (3) bitwise xord, (4) bitwise and,
  // (Assignment to self could account for record update), there are also refs.

  // [Redundant Assignments]

  // [Dead Code] (early returns, so not really relevant).

  // [Redundant Conditionals]: Detects branches that are always dead.
  // Implemented as a combination of (1) integer propagation, (2) set of known predicates, and (3) bounds on integers.

  // Compile the theorems/bugpatterns to an automaton. Union could be fast.

}
