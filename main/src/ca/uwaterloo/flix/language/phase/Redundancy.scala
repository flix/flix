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
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError._
import ca.uwaterloo.flix.language.phase.unification.ClassEnvironment
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.{ListMap, MultiMap}
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * The Redundancy phase checks that declarations and expressions within the AST are used in a meaningful way.
  *
  * For example, the redundancy phase ensures that there are no:
  *
  * - unused local variables.
  * - unused enums, definitions, ...
  * - useless expressions.
  *
  * and so on.
  *
  * The phase performs no AST rewrites; it can be disabled without affecting the runtime semantics.
  */
object Redundancy {

  /**
    * Checks the given AST `root` for redundancies.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, RedundancyError] = flix.phase("Redundancy") {
    // Return early if the redundancy phase is disabled.
    if (flix.options.xallowredundancies) {
      return root.toSuccess
    }

    // Computes all used symbols in all top-level defs (in parallel).
    val usedDefs = ParOps.parAgg(root.defs, Used.empty)({
      case (acc, (_, decl)) => acc ++ visitDef(decl)(root, flix)
    }, _ ++ _)

    // Compute all used symbols in all instance defs (in parallel).
    val usedInstDefs = ParOps.parAgg(TypedAstOps.instanceDefsOf(root), Used.empty)({
      case (acc, decl) => acc ++ visitDef(decl)(root, flix)
    }, _ ++ _)

    val usedSigs = ParOps.parAgg(root.sigs, Used.empty)({
      case (acc, (_, decl)) => acc ++ visitSig(decl)(root, flix)
    }, _ ++ _)

    val usedAll = usedDefs ++ usedInstDefs ++ usedSigs

    // Check for unused symbols.
    val usedRes =
      checkUnusedDefs(usedAll)(root) ++
        checkUnusedEnumsAndTags(usedAll)(root) ++
        checkUnusedTypeParamsEnums()(root) ++
        checkRedundantTypeConstraints()(root)

    // Return the root if successful, otherwise returns all redundancy errors.
    usedRes.toValidation(root)
  }

  /**
    * Checks for unused symbols in the given signature and returns all used symbols.
    */
  private def visitSig(sig: Sig)(implicit root: Root, flix: Flix): Used = {

    // Compute the used symbols inside the signature.
    val usedExp = sig.impl match {
      case None => Used.empty
      case Some(impl) =>
        visitExp(impl.exp, Env.empty ++ sig.spec.fparams.map(_.sym), RecursionContext.ofSig(sig.sym))
    }

    // Check for unused parameters and remove all variable symbols.
    val unusedFormalParams = sig.impl.toList.flatMap(_ => findUnusedFormalParameters(sig.spec, usedExp))
    val unusedTypeParams = findUnusedTypeParameters(sig.spec)

    val usedAll = (usedExp ++
      unusedFormalParams ++
      unusedTypeParams).copy(varSyms = Set.empty)

    // Check if the expression contains holes.
    // If it does, we discard all unused local variable errors.
    if (usedAll.holeSyms.isEmpty)
      usedAll
    else
      usedAll.withoutUnusedVars
  }

  /**
    * Checks for unused symbols in the given definition and returns all used symbols.
    */
  private def visitDef(defn: Def)(implicit root: Root, flix: Flix): Used = {

    // Compute the used symbols inside the definition.
    val usedExp = visitExp(defn.impl.exp, Env.empty ++ defn.spec.fparams.map(_.sym), RecursionContext.ofDef(defn.sym))

    val unusedFormalParams = findUnusedFormalParameters(defn.spec, usedExp)
    val unusedTypeParams = findUnusedTypeParameters(defn.spec)

    // Check for unused parameters and remove all variable symbols.
    val usedAll = (usedExp ++
      unusedFormalParams ++
      unusedTypeParams).copy(varSyms = Set.empty)

    // Check if the expression contains holes.
    // If it does, we discard all unused local variable errors.
    if (usedAll.holeSyms.isEmpty)
      usedAll
    else
      usedAll.withoutUnusedVars
  }

  /**
    * Finds unused formal parameters.
    */
  private def findUnusedFormalParameters(spec: Spec, used: Used): List[UnusedFormalParam] = {
    spec.fparams.collect {
      case fparam if deadVarSym(fparam.sym, used) => UnusedFormalParam(fparam.sym)
    }
  }

  /**
    * Finds unused type parameters.
    */
  private def findUnusedTypeParameters(spec: Spec): List[UnusedTypeParam] = {
    spec.tparams.collect {
      case tparam if deadTypeVar(tparam.sym, spec.declaredScheme.base.typeVars.map(_.sym)) => UnusedTypeParam(tparam.name)
    }
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
            decl.cases.foldLeft(acc) {
              case (innerAcc, (tag, caze)) if deadTag(tag, usedTags) => innerAcc + UnusedEnumTag(sym, caze.tag)
              case (innerAcc, _) => innerAcc
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
        val usedTypeVars = decl.cases.foldLeft(Set.empty[Symbol.TypeVarSym]) {
          case (sacc, (_, Case(_, _, tpe, _, _))) => sacc ++ tpe.typeVars.map(_.sym)
        }
        val unusedTypeParams = decl.tparams.filter(tparam => !usedTypeVars.contains(tparam.sym) && !tparam.name.name.startsWith("_"))
        acc ++ unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name))
    }
  }

  /**
    * Checks for redundant type constraints in the given `root`.
    */
  private def checkRedundantTypeConstraints()(implicit root: Root): List[RedundancyError] = {
    def findRedundantTypeConstraints(tconstrs: List[Ast.TypeConstraint]): List[RedundancyError] = {
      for {
        (tconstr1, i1) <- tconstrs.zipWithIndex
        (tconstr2, i2) <- tconstrs.zipWithIndex
        // don't compare a constraint against itself
        if i1 != i2 && ClassEnvironment.entails(tconstr1, tconstr2, root.classEnv)
      } yield RedundancyError.RedundantTypeConstraint(tconstr1, tconstr2, tconstr2.loc)
    }

    val instErrors = root.instances.values.flatten.flatMap {
      inst => findRedundantTypeConstraints(inst.tconstrs)
    }

    val defErrors = root.defs.values.flatMap {
      defn => findRedundantTypeConstraints(defn.spec.declaredScheme.constraints)
    }

    val classErrors = root.classes.values.flatMap {
      clazz =>
        findRedundantTypeConstraints(clazz.superClasses)
    }

    val sigErrors = root.sigs.values.flatMap {
      sig => findRedundantTypeConstraints(sig.spec.declaredScheme.constraints)
    }

    (instErrors ++ defErrors ++ classErrors ++ sigErrors).toList
  }


  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: Expression, env0: Env, rc: RecursionContext): Used = e0 match {
    case Expression.Unit(_) => Used.empty

    case Expression.Null(_, _) => Used.empty

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

    case Expression.Default(_, _) => Used.empty

    case Expression.Wild(_, _) => Used.empty

    case Expression.Var(sym, _, loc) => (sym.isWild, rc.vars.contains(sym)) match {
      // Case 1: Non-wild, non-recursive use of sym.
      case (false, false) => Used.of(sym)
      // Case 2: Non-wild, recursive use of sym. Does not count as a use.
      case (false, true) => Used.empty
      // Case 3: Wild, non-recursive use of sym.
      case (true, false) => Used.empty + HiddenVarSym(sym, loc)
      // Case 4: Wild, recursive use of sym.
      case (true, true) => Used.empty + HiddenVarSym(sym, loc)
    }

    case Expression.Def(sym, _, _) =>
      // Recursive calls do not count as uses.
      if (!rc.defn.contains(sym))
        Used.of(sym)
      else
        Used.empty

    case Expression.Sig(sym, _, _) =>
      // Recursive calls do not count as uses.
      if (!rc.sig.contains(sym))
        Used.of(sym)
      else
        Used.empty

    case Expression.Hole(sym, _, _) => Used.of(sym)

    case Expression.Lambda(fparam, exp, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + fparam.sym

      // Visit the expression with the extended environment.
      val innerUsed = visitExp(exp, env1, rc)

      // Check if the formal parameter is shadowing.
      val shadowedVar = shadowing(fparam.sym, env0)

      // Check if the lambda parameter symbol is dead.
      if (deadVarSym(fparam.sym, innerUsed))
        innerUsed ++ shadowedVar - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        innerUsed ++ shadowedVar - fparam.sym

    case Expression.Apply(exp, exps, _, _, _) =>
      val us1 = visitExp(exp, env0, rc)
      val us2 = visitExps(exps, env0, rc)
      us1 ++ us2

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expression.Let(sym, _, exp1, exp2, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the two expressions one under the original environment and one under the extended environment.
      val innerUsed1 = visitExp(exp1, env0, rc)
      val innerUsed2 = visitExp(exp2, env1, rc)

      // Check for shadowing.
      val shadowedVar = shadowing(sym, env0)

      // Check if the let-bound variable symbol is dead in exp2.
      if (deadVarSym(sym, innerUsed2))
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym

    case Expression.LetRec(sym, _, exp1, exp2, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the two expressions under the extended environment.
      // Add the variable to the recursion context only in the first expression.
      val innerUsed1 = visitExp(exp1, env1, rc.withVar(sym))
      val innerUsed2 = visitExp(exp2, env1, rc)
      val used = innerUsed1 ++ innerUsed2

      // Check for shadowing.
      val shadowedVar = shadowing(sym, env0)

      // Check if the let-bound variable symbol is dead in exp1 + exp2.
      if (deadVarSym(sym, used))
        (used ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (used ++ shadowedVar) - sym

    case Expression.Scope(sym, exp, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the expression under the extended environment.
      val innerUsed = visitExp(exp, env0, rc)

      // Check for shadowing.
      val shadowedVar = shadowing(sym, env0)

      // Check if the let-bound variable symbol is dead in exp.
      if (deadVarSym(sym, innerUsed))
        (innerUsed ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed ++ shadowedVar) - sym

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      val us3 = visitExp(exp3, env0, rc)
      us1 ++ us2 ++ us3

    case Expression.Stm(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)

      // Check for useless pure expressions.
      if (exp1.eff == Type.Pure)
        (us1 ++ us2) + UselessExpression(exp1.loc)
      else
        us1 ++ us2

    case Expression.Match(exp, rules, _, _, _) =>
      // Visit the match expression.
      val usedMatch = visitExp(exp, env0, rc)

      // Visit each match rule.
      val usedRules = rules map {
        case MatchRule(pat, guard, body) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the pattern, guard and body.
          val usedPat = visitPat(pat)
          val usedGuard = visitExp(guard, extendedEnv, rc)
          val usedBody = visitExp(body, extendedEnv, rc)
          val usedPatGuardAndBody = usedPat ++ usedGuard ++ usedBody

          // Check for unused variable symbols.
          val unusedVarSyms = fvs.filter(sym => deadVarSym(sym, usedPatGuardAndBody)).map(UnusedVarSym)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = fvs.map(sym => shadowing(sym, env0)).foldLeft(Used.empty)(_ ++ _)

          // Combine everything together.
          (usedPatGuardAndBody -- fvs) ++ unusedVarSyms ++ shadowedVarSyms
      }

      usedMatch ++ usedRules.reduceLeft(_ ++ _)

    case Expression.Choose(exps, rules, _, _, _) =>
      val usedMatch = visitExps(exps, env0, rc)
      val usedRules = rules.map {
        case ChoiceRule(pat, exp) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the body.
          val usedBody = visitExp(exp, extendedEnv, rc)

          // Check for unused variable symbols.
          val unusedVarSyms = fvs.filter(sym => deadVarSym(sym, usedBody)).map(UnusedVarSym)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = fvs.map(sym => shadowing(sym, env0)).foldLeft(Used.empty)(_ ++ _)

          // Combine everything together.
          (usedBody -- fvs ++ unusedVarSyms) ++ shadowedVarSyms
      }
      usedMatch ++ usedRules.reduceLeft(_ ++ _)

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      val us = visitExp(exp, env0, rc)
      Used.of(sym, tag) ++ us

    case Expression.Tuple(elms, _, _, _) =>
      visitExps(elms, env0, rc)

    case Expression.RecordEmpty(_, _) =>
      Used.empty

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0, rc)
      val us2 = visitExp(rest, env0, rc)
      us1 ++ us2

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest, env0, rc)

    case Expression.ArrayLit(exps, exp, tpe, eff, loc) =>
      visitExps(exps, env0, rc) ++ visitExp(exp, env0, rc)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      val us3 = visitExp(exp3, env0, rc)
      us1 ++ us2 ++ us3

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0, rc)
      val us2 = visitExp(index, env0, rc)
      us1 ++ us2

    case Expression.ArrayLength(base, _, _) =>
      visitExp(base, env0, rc)

    case Expression.ArrayStore(base, index, elm, _) =>
      val us1 = visitExp(base, env0, rc)
      val us2 = visitExp(index, env0, rc)
      val us3 = visitExp(elm, env0, rc)
      us1 ++ us2 ++ us3

    case Expression.ArraySlice(base, begin, end, _, _) =>
      val us1 = visitExp(base, env0, rc)
      val us2 = visitExp(begin, env0, rc)
      val us3 = visitExp(end, env0, rc)
      us1 ++ us2 ++ us3

    case Expression.Ref(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.Cast(exp, _, declaredEff, _, _, loc) =>
      declaredEff match {
        case None => visitExp(exp, env0, rc)
        case Some(eff) =>
          (eff, exp.eff) match {
            case (Type.Pure, Type.Pure) =>
              visitExp(exp, env0, rc) + RedundantPurityCast(loc)
            case (tvar1: Type.KindedVar, tvar2: Type.KindedVar) if tvar1.sym == tvar2.sym =>
              visitExp(exp, env0, rc) + RedundantEffectCast(loc)
            case _ => visitExp(exp, env0, rc)
          }
      }

    case Expression.TryCatch(exp, rules, _, _, _) =>
      val usedExp = visitExp(exp, env0, rc)
      val usedRules = rules.foldLeft(Used.empty) {
        case (acc, CatchRule(sym, _, body)) =>
          val usedBody = visitExp(body, env0, rc)
          if (deadVarSym(sym, usedBody))
            acc ++ usedBody + UnusedVarSym(sym)
          else
            acc ++ usedBody
      }
      usedExp ++ usedRules

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args, env0, rc)

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp, env0, rc) ++ visitExps(args, env0, rc)

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args, env0, rc)

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1, env0, rc) ++ visitExp(exp2, env0, rc)

    case Expression.GetStaticField(_, _, _, _) =>
      Used.empty

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expression.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultUsed = defaultOpt match {
        case None => Used.empty
        case Some(default) => visitExp(default, env0, rc)
      }

      val rulesUsed = rules map {
        case SelectChannelRule(sym, chan, body) =>
          // Extend the environment with the symbol.
          val env1 = env0 + sym

          // Check for shadowing.
          val shadowedVar = shadowing(sym, env0)

          // Visit the channel and body expressions.
          val chanUsed = visitExp(chan, env1, rc)
          val bodyUsed = visitExp(body, env1, rc)

          // Check if the variable symbol is dead in the body.
          if (deadVarSym(sym, bodyUsed))
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym + UnusedVarSym(sym)
          else
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym
      }

      rulesUsed.foldLeft(defaultUsed) {
        case (acc, used) => acc ++ used
      }

    case Expression.Spawn(exp, _, _, _) => visitExp(exp, env0, rc)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.Force(exp, _, _, _) => visitExp(exp, env0, rc)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Used.empty) {
        case (used, con) => used ++ visitConstraint(con, env0, rc: RecursionContext)
      }

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expression.Reify(_, _, _, _) =>
      Used.empty

    case Expression.ReifyType(_, _, _, _, _) =>
      Used.empty

    case Expression.ReifyEff(sym, exp1, exp2, exp3, tpe, _, _) =>
      Used.of(sym) ++ visitExp(exp1, env0, rc) ++ visitExp(exp2, env0, rc) ++ visitExp(exp3, env0, rc)
  }

  /**
    * Returns the symbols used in the given list of expressions `es` under the given environment `env0`.
    */
  private def visitExps(es: List[Expression], env0: Env, rc: RecursionContext): Used =
    es.foldLeft(Used.empty) {
      case (acc, exp) => acc ++ visitExp(exp, env0, rc)
    }

  /**
    * Returns the symbols used in the given pattern `pat`.
    */
  private def visitPat(pat0: Pattern): Used = pat0 match {
    case Pattern.Wild(_, _) => Used.empty
    case Pattern.Var(_, _, _) => Used.empty
    case Pattern.Unit(_) => Used.empty
    case Pattern.True(_) => Used.empty
    case Pattern.False(_) => Used.empty
    case Pattern.Char(_, _) => Used.empty
    case Pattern.Float32(_, _) => Used.empty
    case Pattern.Float64(_, _) => Used.empty
    case Pattern.Int8(_, _) => Used.empty
    case Pattern.Int16(_, _) => Used.empty
    case Pattern.Int32(_, _) => Used.empty
    case Pattern.Int64(_, _) => Used.empty
    case Pattern.BigInt(_, _) => Used.empty
    case Pattern.Str(_, _) => Used.empty
    case Pattern.Tag(sym, tag, _, _, _) => Used.of(sym, tag)
    case Pattern.Tuple(elms, _, _) => visitPats(elms)
    case Pattern.Array(elms, _, _) => visitPats(elms)
    case Pattern.ArrayTailSpread(elms, _, _, _) => visitPats(elms)
    case Pattern.ArrayHeadSpread(_, elms, _, _) => visitPats(elms)
  }

  /**
    * Returns the symbols used in the given list of pattern `ps`.
    */
  private def visitPats(ps: List[Pattern]): Used = ps.foldLeft(Used.empty) {
    case (acc, pat) => acc ++ visitPat(pat)
  }

  /**
    * Returns the symbols used in the given constraint `c0` under the given environment `env0`.
    */
  private def visitConstraint(c0: Constraint, env0: Env, rc: RecursionContext): Used = {
    val head = visitHeadPred(c0.head, env0, rc: RecursionContext)
    val body = c0.body.foldLeft(Used.empty) {
      case (acc, b) => acc ++ visitBodyPred(b, env0, rc: RecursionContext)
    }
    val total = head ++ body

    // Check that no variable is used only once except if they are wild (_ prefix).
    // Check additionally that there is only one mention of a wild variable.
    // This error is already checked in a program like `A(_x) :- B(_x).` but
    // not for `A(12) :- B(_x), C(_x).`.
    val errors = c0.cparams.flatMap(constraintParam => {
      val sym = constraintParam.sym
      val occurrences = total.occurrencesOf.apply(sym)
      if (occurrences.size == 1 && !sym.isWild) {
        // Check that no variable is only used once
        List(RedundancyError.IllegalSingleVariable(sym, occurrences.iterator.next()))
      } else if (body.occurrencesOf.apply(sym).size > 1 && sym.isWild) {
        // Check that wild variables are not used multiple times in the body
        occurrences.map(loc => RedundancyError.HiddenVarSym(sym, loc))
      } else Nil
    })

    (total -- c0.cparams.map(_.sym)) ++ errors
  }

  /**
    * Returns the symbols used in the given head predicate `h0` under the given environment `env0`.
    */
  private def visitHeadPred(h0: Predicate.Head, env0: Env, rc: RecursionContext): Used = h0 match {
    case Head.Atom(_, _, terms, _, _) =>
      visitExps(terms, env0, rc)
  }

  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env, rc: RecursionContext): Used = b0 match {
    case Body.Atom(_, _, _, _, terms, _, _) =>
      terms.foldLeft(Used.empty) {
        case (acc, term) => acc ++ Used.of(freeVars(term))
      }

    case Body.Guard(exp, _) =>
      visitExp(exp, env0, rc)

    case Body.Loop(varSyms, exp, _) =>
      varSyms.foldLeft(visitExp(exp, env0, rc: RecursionContext)) {
        case (acc, varSym) => acc ++ Used.of(varSym)
      }
  }

  /**
    * Returns the free variables in the pattern `p0`.
    */
  private def freeVars(p0: Pattern): Set[Symbol.VarSym] = p0 match {
    case Pattern.Wild(_, _) => Set.empty
    case Pattern.Var(sym, _, _) => Set(sym)
    case Pattern.Unit(_) => Set.empty
    case Pattern.True(_) => Set.empty
    case Pattern.False(_) => Set.empty
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
    * Returns the free variables in the list of choice patterns `ps`.
    */
  private def freeVars(ps: List[ChoicePattern]): Set[Symbol.VarSym] = ps.collect {
    case ChoicePattern.Present(sym, _, _) => sym
  }.toSet

  /**
    * Checks whether the variable symbol `sym` shadows another variable in the environment `env`.
    */
  private def shadowing(sym: Symbol.VarSym, env: Env): Used =
    env.varSyms.get(sym.text) match {
      case None =>
        Used.empty
      case Some(shadowingVar) =>
        if (sym.isWild)
          Used.empty
        else
          Used.empty + ShadowedVar(shadowingVar, sym)
    }

  /**
    * Returns `true` if the given definition `decl` is unused according to `used`.
    */
  private def deadDef(decl: Def, used: Used)(implicit root: Root): Boolean =
    !isTest(decl.spec.ann) &&
      !decl.spec.mod.isPublic &&
      !isMain(decl.sym) &&
      !decl.sym.name.startsWith("_") &&
      !used.defSyms.contains(decl.sym) &&
      !root.reachable.contains(decl.sym)

  /**
    * Returns `true` if the given symbol `sym` either is `main` or is an entry point.
    */
  private def isMain(sym: Symbol.DefnSym)(implicit root: Root): Boolean =
    sym.toString == "main" || root.entryPoint.contains(sym)

  /**
    * Returns `true` if the given `tag` is unused according to the `usedTags`.
    */
  private def deadTag(tag: Name.Tag, usedTags: Set[Name.Tag]): Boolean =
    !tag.name.startsWith("_") &&
      !usedTags.contains(tag)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def deadTypeVar(tvar: Symbol.KindedTypeVarSym, used: Set[Symbol.KindedTypeVarSym]): Boolean = {
    !tvar.text.exists(_.startsWith("_")) &&
      !used.contains(tvar)
  }

  /**
    * Returns `true` if the local variable `tvar` is unused according to the argument `used`.
    */
  private def deadVarSym(sym: Symbol.VarSym, used: Used): Boolean =
    !sym.text.startsWith("_") &&
      !used.varSyms.contains(sym)

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
    val empty: Used = Used(MultiMap.empty, Set.empty, Set.empty, Set.empty, Set.empty, ListMap.empty, Set.empty)

    /**
      * Returns an object where the given enum symbol `sym` and `tag` are marked as used.
      */
    def of(sym: Symbol.EnumSym, tag: Name.Tag): Used = empty.copy(enumSyms = MultiMap.singleton(sym, tag))

    /**
      * Returns an object where the given defn symbol `sym` is marked as used.
      */
    def of(sym: Symbol.DefnSym): Used = empty.copy(defSyms = Set(sym))

    /**
      * Returns an object where the given sig symbol `sym` is marked as used.
      */
    def of(sym: Symbol.SigSym): Used = empty.copy(sigSyms = Set(sym))

    /**
      * Returns an object where the given hole symbol `sym` is marked as used.
      */
    def of(sym: Symbol.HoleSym): Used = empty.copy(holeSyms = Set(sym))

    /**
      * Returns an object where the given variable symbol `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Used = empty.copy(varSyms = Set(sym), occurrencesOf = ListMap.singleton(sym, sym.loc))

    /**
      * Returns an object where the given variable symbols `syms` are marked as used.
      */
    def of(syms: Set[Symbol.VarSym]): Used = empty.copy(
      varSyms = syms,
      occurrencesOf = syms.foldLeft(ListMap.empty[Symbol.VarSym, SourceLocation]) {
        case (mm, sym) => mm + (sym, sym.loc)
      })

  }

  /**
    * A representation of used symbols.
    */
  private case class Used(enumSyms: MultiMap[Symbol.EnumSym, Name.Tag],
                          defSyms: Set[Symbol.DefnSym],
                          sigSyms: Set[Symbol.SigSym],
                          holeSyms: Set[Symbol.HoleSym],
                          varSyms: Set[Symbol.VarSym],
                          occurrencesOf: ListMap[Symbol.VarSym, SourceLocation],
                          errors: Set[RedundancyError]) {

    /**
      * Merges `this` and `that` where one of the two branches is executed
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
          this.sigSyms ++ that.sigSyms,
          this.holeSyms ++ that.holeSyms,
          this.varSyms ++ that.varSyms,
          this.occurrencesOf ++ that.occurrencesOf,
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
      * Returns `this` without any unused variable errors.
      */
    def withoutUnusedVars: Used = copy(errors = errors.filter {
      case e: RedundancyError.UnusedFormalParam => false
      case e: RedundancyError.UnusedVarSym => false
      case _ => true
    })

    /**
      * Returns Successful(a) unless `this` contains errors.
      */
    def toValidation[A](a: A): Validation[A, RedundancyError] = if (errors.isEmpty) Success(a) else Failure(errors.to(LazyList))
  }

  /**
    * Tracks the context of the explored expression, recalling the definition, signature,
    * or recursive variable under which it is defined.
    */
  private case class RecursionContext(defn: Option[Symbol.DefnSym], sig: Option[Symbol.SigSym], vars: Set[Symbol.VarSym]) {
    /**
      * Adds the given variable to the context.
      */
    def withVar(v: Symbol.VarSym): RecursionContext = this.copy(vars = this.vars + v)
  }

  private object RecursionContext {
    /**
      * Initializes a context under the given definition.
      */
    def ofDef(defn: Symbol.DefnSym): RecursionContext = RecursionContext(defn = Some(defn), sig = None, vars = Set.empty)

    /**
      * Initializes a context under the given signature.
      */
    def ofSig(sig: Symbol.SigSym): RecursionContext = RecursionContext(defn = None, sig = Some(sig), vars = Set.empty)
  }
}
