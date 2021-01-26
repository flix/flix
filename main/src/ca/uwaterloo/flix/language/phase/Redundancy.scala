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
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.MultiMap
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
    val usedDefs = ParOps.parAgg(root.defs, Used.empty)({
      case (acc, (sym, decl)) => acc and visitDef(decl)(root, flix)
    }, _ and _)

    // Computes all used symbols in all lattices.
    val usedLats = root.latticeOps.values.foldLeft(Used.empty) {
      case (acc, LatticeOps(tpe, bot, equ, leq, lub, glb)) =>
        acc and (Used.of(bot) and Used.of(equ) and Used.of(leq) and Used.of(lub) and Used.of(glb))
    }

    // Computes all used symbols.
    val usedAll = usedLats and usedDefs

    // Check for unused symbols.
    val usedRes =
      checkUnusedDefs(usedAll)(root) and
        checkUnusedEnumsAndTags(usedAll)(root) and
        checkUnusedTypeParamsEnums()(root)

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
        case tparam if deadTypeVar(tparam.tpe, defn.declaredScheme.base.typeVars) => UnusedTypeParam(tparam.name)
      }
      used ++ unusedParams
    }

    // Compute the used symbols inside the definition.
    val recursionContext = RecursionContext.Recursable(defn.sym, arity(defn))
    val usedExp = visitExp(defn.exp, Env.of(recursionContext) ++ defn.fparams.map(_.sym))

    // Check for unused parameters and remove all variable symbols.
    val usedAll = (usedExp and checkUnusedFormalParameters(usedExp) and checkUnusedTypeParameters(usedExp)).copy(varSyms = Set.empty)

    val usedAllWithUnconditionalRecursions = if (usedAll.unconditionallyRecurses) usedAll + UnconditionalRecursion(defn.sym) else usedAll

    // Check if the used symbols contains holes. If so, strip out all error messages.
    if (usedAllWithUnconditionalRecursions.holeSyms.isEmpty)
      usedAllWithUnconditionalRecursions
    else
      usedAllWithUnconditionalRecursions.copy(errors = Set.empty)
  }

  /**
    * Finds the arity of the Def.
    */
  private def arity(defn: Def): Int = defn.fparams.size

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
              case (innerAcc, (tag, caze)) if deadTag(tag, caze, usedTags) => acc + UnusedEnumTag(sym, caze.tag)
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
        val usedTypeVars = decl.cases.foldLeft(Set.empty[Type.Var]) {
          case (sacc, (_, Case(_, _, tpe, _, _))) => sacc ++ tpe.typeVars
        }
        val unusedTypeParams = decl.tparams.filter(tparam => !usedTypeVars.contains(tparam.tpe) && !tparam.name.name.startsWith("_"))
        acc ++ unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name))
    }
  }

  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: Expression, env0: Env): Used = e0 match {
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

    case Expression.Var(sym, _, loc) =>
      if (!sym.isWild())
        Used.of(sym)
      else
        Used.empty + HiddenVarSym(sym, loc)

    case Expression.Def(sym, _, _) => Used.of(sym)

    case Expression.Sig(sym, _, _) => Used.empty

    case Expression.Hole(sym, _, _, _) => Used.of(sym)

    case Expression.Lambda(fparam, exp, _, _) =>
      // Extend the environment with the variable symbol.
      // Remove the recursion context for abstraction
      val env1 = env0.withoutRecursionContext + fparam.sym

      // Visit the expression with the extended environment.
      val innerUsed = visitExp(exp, env1)

      // Check if the formal parameter is shadowing.
      val shadowedVar = shadowing(fparam.sym, env0)

      // Check if the lambda parameter symbol is dead.
      if (deadVarSym(fparam.sym, innerUsed))
        innerUsed and shadowedVar - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        innerUsed and shadowedVar - fparam.sym

    case Expression.Apply(defn@Expression.Def(sym, _, _), exps, _, _, _) if env0.recursionContext.isRecursiveCall(sym, exps.length) =>
      // Check for unconditional recursion.
      // NB: A function that calls itself recursively is not used.
      val us1 = Used.empty
      val us2 = visitExps(exps, env0)
      (us1 and us2).withUnconditionalRecursion

    case Expression.Apply(exp, exps, _, _, _) =>
      val us1 = visitExp(exp, env0)
      val us2 = visitExps(exps, env0)
      us1 and us2

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 and us2

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
        (innerUsed1 and innerUsed2 and shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed1 and innerUsed2 and shadowedVar) - sym

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      us1 and (us2 or us3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)

      // Check for useless pure expressions.
      if (exp1.eff == Type.Pure)
        (us1 and us2) + UselessExpression(exp1.loc)
      else
        us1 and us2

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

          // Visit the pattern, guard and body.
          val usedPat = visitPat(pat)
          val usedGuard = visitExp(guard, extendedEnv)
          val usedBody = visitExp(body, extendedEnv)
          val usedPatGuardAndBody = usedPat and usedGuard and usedBody

          // Check for unused variable symbols.
          val unusedVarSyms = fvs.filter(sym => deadVarSym(sym, usedPatGuardAndBody)).map(UnusedVarSym)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = fvs.map(sym => shadowing(sym, env0)).foldLeft(Used.empty)(_ and _)

          // Combine everything together.
          (usedPatGuardAndBody -- fvs) ++ unusedVarSyms and shadowedVarSyms
      }

      usedMatch and usedRules.reduceLeft(_ or _)

    case Expression.Choose(exps, rules, _, _, _) =>
      val usedMatch = visitExps(exps, env0)
      val usedRules = rules.map {
        case ChoiceRule(pat, exp) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the body.
          val usedBody = visitExp(exp, extendedEnv)

          // Check for unused variable symbols.
          val unusedVarSyms = fvs.filter(sym => deadVarSym(sym, usedBody)).map(UnusedVarSym)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = fvs.map(sym => shadowing(sym, env0)).foldLeft(Used.empty)(_ and _)

          // Combine everything together.
          (usedBody -- fvs ++ unusedVarSyms) and shadowedVarSyms
      }
      usedMatch and usedRules.reduceLeft(_ or _)

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      val us = visitExp(exp, env0)
      Used.of(sym, tag) and us

    case Expression.Tuple(elms, _, _, _) =>
      visitExps(elms, env0)

    case Expression.RecordEmpty(_, _) =>
      Used.empty

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp, env0)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0)
      val us2 = visitExp(rest, env0)
      us1 and us2

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest, env0)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      visitExps(elms, env0)

    case Expression.ArrayNew(elm, len, _, _, _) =>
      val us1 = visitExp(elm, env0)
      val us2 = visitExp(len, env0)
      us1 and us2

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      us1 and us2

    case Expression.ArrayLength(base, _, _) =>
      visitExp(base, env0)

    case Expression.ArrayStore(base, index, elm, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      val us3 = visitExp(elm, env0)
      us1 and us2 and us3

    case Expression.ArraySlice(base, begin, end, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(begin, env0)
      val us3 = visitExp(end, env0)
      us1 and us2 and us3

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 and us2

    case Expression.Existential(fparam, exp, _) =>
      // Check for variable shadowing.
      val us1 = shadowing(fparam.sym, env0)

      // Visit the expression under an extended environment.
      val us2 = visitExp(exp, env0 + fparam.sym)

      // Check if the quantified variable is dead.
      if (deadVarSym(fparam.sym, us2))
        us1 and us2 - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us1 and us2 - fparam.sym

    case Expression.Universal(fparam, exp, _) =>
      // Check for variable shadowing.
      val us1 = shadowing(fparam.sym, env0)

      // Visit the expression under an extended environment.
      val us2 = visitExp(exp, env0 + fparam.sym)

      // Check if the quantified variable is dead.
      if (deadVarSym(fparam.sym, us2))
        us1 and us2 - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us1 and us2 - fparam.sym

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      val usedExp = visitExp(exp, env0)
      val usedRules = rules.foldLeft(Used.empty) {
        case (acc, CatchRule(sym, _, body)) =>
          val usedBody = visitExp(body, env0)
          if (deadVarSym(sym, usedBody))
            acc or usedBody + UnusedVarSym(sym)
          else
            acc or usedBody
      }
      usedExp and usedRules

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args, env0)

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp, env0) and visitExps(args, env0)

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args, env0)

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1, env0) and visitExp(exp2, env0)

    case Expression.GetStaticField(_, _, _, _) =>
      Used.empty

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 and us2

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
            (chanUsed and bodyUsed and shadowedVar) - sym + UnusedVarSym(sym)
          else
            (chanUsed and bodyUsed and shadowedVar) - sym
      }

      rulesUsed.foldLeft(defaultUsed) {
        case (acc, used) => acc or used
      }

    case Expression.Spawn(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Lazy(exp, _, _) =>
      // Remove the recursion context as `exp` will not necessarily be evaluated.
      visitExp(exp, env0.withoutRecursionContext)

    case Expression.Force(exp, _, _, _) => visitExp(exp, env0)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Used.empty) {
        case (used, con) => used and visitConstraint(con, env0)
      }

    case Expression.FixpointCompose(exp1, exp2, _, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 and us2

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp, env0)

    case Expression.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      val used1 = visitExp(exp1, env0)
      val used2 = visitExp(exp2, env0)
      used1 and used2

    case Expression.FixpointFold(_, exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      us1 and us2 and us3
  }

  /**
    * Returns the symbols used in the given list of expressions `es` under the given environment `env0`.
    */
  private def visitExps(es: List[Expression], env0: Env): Used =
    es.foldLeft(Used.empty) {
      case (acc, exp) => acc and visitExp(exp, env0)
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
    case (acc, pat) => acc and visitPat(pat)
  }

  /**
    * Returns the symbols used in the given constraint `c0` under the given environment `env0`.
    */
  private def visitConstraint(c0: Constraint, env0: Env): Used = {
    val zero = visitHeadPred(c0.head, env0)
    val used = c0.body.foldLeft(zero) {
      case (acc, b) => acc and visitBodyPred(b, env0)
    }
    used -- c0.cparams.map(_.sym)
  }

  /**
    * Returns the symbols used in the given head predicate `h0` under the given environment `env0`.
    */
  private def visitHeadPred(h0: Predicate.Head, env0: Env): Used = h0 match {
    case Head.Atom(_, _, terms, _, _) =>
      visitExps(terms, env0)

    case Head.Union(exp, _, _) =>
      visitExp(exp, env0)
  }

  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env): Used = b0 match {
    case Body.Atom(_, _, _, terms, _, _) =>
      Used.empty

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
        if (sym.isWild())
          Used.empty
        else
          Used.empty + ShadowedVar(shadowingVar, sym)
    }

  /**
    * Returns `true` if the given definition `decl` is unused according to `used`.
    */
  private def deadDef(decl: Def, used: Used)(implicit root: Root): Boolean =
    !isTest(decl.ann) &&
      !isLint(decl.ann) &&
      !decl.mod.isPublic &&
      !decl.sym.isMain &&
      !decl.sym.name.startsWith("_") &&
      !used.defSyms.contains(decl.sym) &&
      !root.reachable.contains(decl.sym)

  /**
    * Returns `true` if the given tag `decl` is unused according to `used`.
    */
  private def deadTag(tag: Name.Tag, caze: Case, usedTags: Set[Name.Tag]): Boolean =
    !tag.name.startsWith("_") &&
      !usedTags.contains(tag)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def deadTypeVar(tvar: Type.Var, used: Set[Type.Var]): Boolean = {
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

    def of(recursionContext: RecursionContext): Env = Env.empty.copy(recursionContext = recursionContext)
  }

  /**
    * Represents an environment.
    */
  private case class Env(varSyms: Map[String, Symbol.VarSym], recursionContext: RecursionContext = RecursionContext.NoContext) {
    /**
      * Updates `this` environment with a new variable symbol `sym`.
      */
    def +(sym: Symbol.VarSym): Env = {
      copy(varSyms = varSyms + (sym.text -> sym))
    }

    /**
      * Updates `this` environment with a set of new variable symbols `varSyms`.
      */
    def ++(vs: Iterable[Symbol.VarSym]): Env = vs.foldLeft(Env.of(recursionContext)) {
      case (acc, sym) => acc + sym
    }

    /**
      * Removes the recursion context from this env.
      */
    def withoutRecursionContext: Env =
      if (recursionContext == RecursionContext.NoContext) this else copy(recursionContext = RecursionContext.NoContext)
  }

  private object Used {

    /**
      * Represents the empty set of used symbols.
      */
    val empty: Used = Used(MultiMap.empty, Set.empty, Set.empty, Set.empty, unconditionallyRecurses = false, Set.empty)

    /**
      * Returns an object where the given enum symbol `sym` and `tag` are marked as used.
      */
    def of(sym: Symbol.EnumSym, tag: Name.Tag): Used = empty.copy(enumSyms = MultiMap.empty + (sym, tag))

    /**
      * Returns an object where the given defn symbol `sym` is marked as used.
      */
    def of(sym: Symbol.DefnSym): Used = empty.copy(defSyms = Set(sym))

    /**
      * Returns an object where the given hole symbol `sym` is marked as used.
      */
    def of(sym: Symbol.HoleSym): Used = empty.copy(holeSyms = Set(sym))

    /**
      * Returns an object where the given variable symbol `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Used = empty.copy(varSyms = Set(sym))

  }

  /**
    * A representation of used symbols.
    */
  private case class Used(enumSyms: MultiMap[Symbol.EnumSym, Name.Tag],
                          defSyms: Set[Symbol.DefnSym],
                          holeSyms: Set[Symbol.HoleSym],
                          varSyms: Set[Symbol.VarSym],
                          unconditionallyRecurses: Boolean,
                          errors: Set[RedundancyError]) {

    /**
      * Merges `this` and `that` where one of the two branches is executed
      */
    def or(that: Used): Used =
      if (this eq that) {
        this
      } else if (this eq Used.empty) {
        that.copy(unconditionallyRecurses = false)
      } else if (that eq Used.empty) {
        this.copy(unconditionallyRecurses = false)
      } else {
        Used(
          this.enumSyms ++ that.enumSyms,
          this.defSyms ++ that.defSyms,
          this.holeSyms ++ that.holeSyms,
          this.varSyms ++ that.varSyms,
          this.unconditionallyRecurses && that.unconditionallyRecurses,
          this.errors ++ that.errors
        )
      }

    /**
      * Merges `this` and `that` where both branches are executed
      */
    def and(that: Used): Used =
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
          this.holeSyms ++ that.holeSyms,
          this.varSyms ++ that.varSyms,
          this.unconditionallyRecurses || that.unconditionallyRecurses,
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
      * Marks this Used as unconditionally recursive.
      */
    def withUnconditionalRecursion: Used =
      if (unconditionallyRecurses) this else copy(unconditionallyRecurses = true)

    /**
      * Returns Successful(a) unless `this` contains errors.
      */
    def toValidation[A](a: A): Validation[A, RedundancyError] = if (errors.isEmpty) Success(a) else Failure(errors.to(LazyList))
  }

  /**
    * Describes the context where a def call might be recursive.
    *
    * Tracks the def, its arity, and the number of applications made to allow for detection of [[UnconditionalRecursion]]
    */
  private sealed trait RecursionContext {
    /**
      * True iff the call is recursive in this context.
      */
    def isRecursiveCall(call: Symbol.DefnSym, nParams: Int): Boolean
  }

  private object RecursionContext {

    /**
      * Context without a definition to be recursed on.
      */
    case object NoContext extends RecursionContext {
      override def isRecursiveCall(call: Symbol.DefnSym, nParams: Int): Boolean = false
    }

    /**
      * Context where a recursive call is possible.
      */
    case class Recursable(call: Symbol.DefnSym, nParams: Int) extends RecursionContext {
      override def isRecursiveCall(call: Symbol.DefnSym, nParams: Int): Boolean = {
        call == this.call && nParams == this.nParams
      }
    }

  }

}
