/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
 * Copyright 2025 Jakob Schneider Villumsen
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

package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst.{Expr, FormalParam, Occur, Pattern}
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.collection.ListOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

/**
  * Rewrites the body of each def using, using the following transformations:
  *   - Copy Propagation:
  * {{{
  *     let x = 1;
  *     f(x)
  * }}}
  *     becomes
  * {{{
  *     let x = 1;
  *     f(1)
  * }}}
  *   - Dead Code Elimination
  * {{{
  *     let x = 1;
  *     f(1)
  * }}}
  *     becomes
  * {{{
  *     f(1)
  * }}}
  *   - Inline Expansion
  * {{{
  *     f(1)
  * }}}
  *     becomes (where the definition of `f` is `x + 2`)
  * {{{
  *     (x -> x + 2)(1)
  * }}}
  *   - Beta Reduction
  * {{{
  *     (x -> x + 2)(1)
  * }}}
  *     becomes
  * {{{
  *     let x = 1;
  *     x + 2
  * }}}
  */
object Inliner {

  /** Performs inlining on the given AST `root`. */
  def run(root: MonoAst.Root)(implicit flix: Flix): (MonoAst.Root, Set[Symbol.DefnSym]) = {
    val sctx: SharedContext = SharedContext.mk()
    val defs = ParOps.parMapValues(root.defs)(visitDef(_)(sctx, root, flix))
    val newDelta = sctx.changed.asScala.keys.toSet
    val liveSyms = root.entryPoints ++ sctx.live.asScala.keys.toSet
    val liveDefs = defs.filter(kv => liveSyms.contains(kv._1))
    (root.copy(defs = liveDefs), newDelta)
  }

  /** Performs inlining on the body of `def0`. */
  private def visitDef(def0: MonoAst.Def)(implicit sctx: SharedContext, root: MonoAst.Root, flix: Flix): MonoAst.Def = def0 match {
    case MonoAst.Def(sym, spec, exp, loc) =>
      val e = visitExp(exp, LocalContext.Empty)(sym, sctx, root, flix)
      MonoAst.Def(sym, spec, e, loc)
  }

  /**
    * Performs inlining on the expression `exp0`.
    *
    * To avoid duplicating variable names, [[visitExp]] unconditionally
    * assigns new names to all variables.
    * When a binder is visited, it replaces it with a fresh variable and adds
    * it to the variable substitution `varSubst` in th LocalContext `ctx0`.
    * When a variable is visited, it replaces the old variable with the fresh one.
    * Top-level function parameters are not substituted unless inlined, in which case
    * the parameters are let-bound and added to the variable substitution.
    *
    * Within `ctx0` [[visitExp]] also maintains an 'expression substitution' `subst` mapping symbols
    * to expressions ([[SubstRange]]) which it uses unconditionally replace some variable occurrences (see below).
    * It also maintains a set of in-scope variables `inScopeVars` mapping symbols to [[BoundKind]], i.e.,
    * information on how a variable is bound. This is used to consider inlining at a variable occurrence.
    * If a let-bound variable has been visited and is not in `subst`, then it must always be in `inScopeVars`.
    * Importantly, only fresh variables are mapped in both `subst` and `inScopeVars`, so when a variable
    * is encountered, `varSubst` must always be applied first to obtain the corresponding fresh variable.
    *
    * When [[visitExp]] encounters a variable `x` it first applies the substitution `varSubst` to
    * obtain the fresh variable. If it is not in the substitution then the variable is a function parameter
    * occurrence bound by the defining function with symbol `sym0`.
    * If it obtains `x'` from the substitution, it does the following three things:
    *   1. If `x'` is in the expression substitution `subst` it replaces the variable with
    *      the expression, recursively calling [[visitExp]] with the substitution from the definition site
    *      if it is a [[SubstRange.SuspendedExpr]].
    *      This means that [[visitExp]] previously decided to unconditionally inline the let-binding
    *   1. If it is a [[SubstRange.DoneExpr]] then it decided to do copy-propagation of that binding (see below).
    *      It then recursively visits the expression using the empty substitution since the current substitution
    *      is invalid in that scope.
    *   1. If `x'` is not in the expression substitution, it must be in the set of in-scope variable
    *      definitions and considers it for inlining if its definition is pure.
    *
    * When [[visitExp]] encounters a let-binding `let sym = e1; e2` it considers four cases
    * (note that it always refreshes `sym` to `sym'` as mentioned above):
    *   1. If the binding is dead and pure, it drops the binding and returns `visitExp(e2)`.
    *   1. If the binding is dead and impure, it rewrites the binding to a statement.
    *   1. If the binding occurs once and is pure, it adds the unvisited `e1` to the substitution `subst`,
    *      drops the binding and unconditionally inlines it at the occurrence of `sym`.
    *   1. If the binding occurs more than once, it first visits `e1` and considers the following:
    *      (a) If the visited `e1` is trivial and pure, it removes the let-binding and unconditionally inlines
    *      the visited `e1` at every occurrence of `sym`. This corresponds to copy-propagation.
    *      (b) If the visited `e1` is nontrivial, it keeps the let-binding, adds the visited `e1` to the set
    *      of in-scope variable definitions and considers it for inlining at every occurrence.
    */
  private def visitExp(exp0: Expr, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, sctx: SharedContext, root: MonoAst.Root, flix: Flix): Expr = exp0 match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      // Replace with fresh variable if it is not a parameter
      ctx0.varSubst.get(sym) match {
        case None => // Function parameter occurrence
          Expr.Var(sym, tpe, loc)

        case Some(freshVarSym) =>
          // Check for unconditional inlining / copy-propagation
          ctx0.subst.get(freshVarSym) match {
            case Some(SubstRange.SuspendedExpr(exp, subst)) =>
              // Unconditional inline of variable that occurs once.
              // Use the expression substitution from the definition site.
              sctx.changed.putIfAbsent(sym0, ())
              visitExp(exp, ctx0.withSubst(subst))

            case Some(SubstRange.DoneExpr(exp)) =>
              // Copy-propagation of visited expr.
              // Use the empty expression substitution since this has already been visited
              // and the context might indicate that if exp is a var, it should be inlined again.
              sctx.changed.putIfAbsent(sym0, ())
              visitExp(exp, ctx0.withSubst(Map.empty))

            case None =>
              // It was not unconditionally inlined, so consider inlining at this occurrence site
              useSiteInline(freshVarSym, ctx0) match {
                case Some(exp) =>
                  sctx.changed.putIfAbsent(sym0, ())
                  visitExp(exp, ctx0.withSubst(Map.empty))

                case None =>
                  Expr.Var(freshVarSym, tpe, loc)
              }
          }
      }

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val (fp, varSubst1) = freshFormalParam(fparam)
      val ctx = ctx0.addVarSubsts(varSubst1).addInScopeVar(fp.sym, BoundKind.ParameterOrPattern)
      val e = visitExp(exp, ctx)
      Expr.Lambda(fp, e, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1, ctx0) match {
        case Expr.Lambda(fparam, e1, _, _) =>
          sctx.changed.putIfAbsent(sym0, ())
          val e2 = visitExp(exp2, ctx0)
          val letBinding = bindArgs(e1, List(fparam), List(e2), loc)
          visitExp(letBinding, ctx0)

        case e1 =>
          val e2 = visitExp(exp2, ctx0)
          Expr.ApplyClo(e1, e2, tpe, eff, loc)
      }

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      if (shouldInlineDef(root.defs(sym), es, ctx0)) {
        sctx.changed.putIfAbsent(sym0, ())
        val defn = root.defs(sym)
        val ctx = ctx0.withSubst(Map.empty).enableInliningMode
        val letBinding = bindArgs(defn.exp, defn.spec.fparams, es, loc)
        visitExp(letBinding, ctx)
      } else {
        sctx.live.putIfAbsent(sym, ())
        Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)
      }

    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      // Refresh the symbol
      val sym1 = ctx0.varSubst.getOrElse(sym, sym)
      // Check if it was unconditionally inlined
      ctx0.subst.get(sym1) match {
        case Some(SubstRange.SuspendedExpr(Expr.LocalDef(_, fparams, exp, _, _, _, _, _), subst)) =>
          val es = exps.map(visitExp(_, ctx0))
          val letBinding = bindArgs(exp, fparams, es, loc)
          visitExp(letBinding, ctx0.withSubst(subst))

        case None | Some(_) =>
          // It was not unconditionally inlined, so return same expr with visited subexpressions
          val es = exps.map(visitExp(_, ctx0))
          Expr.ApplyLocalDef(sym1, es, tpe, eff, loc)
      }

    case Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.ApplyOp(sym, es, tpe, eff, loc)

    case Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) => (occur, exp1.eff) match {
      case (Occur.Dead, Type.Pure) =>
        // Eliminate dead binder
        sctx.changed.putIfAbsent(sym0, ())
        visitExp(exp2, ctx0)

      case (Occur.Dead, _) =>
        // Rewrite to Stm to preserve effect
        sctx.changed.putIfAbsent(sym0, ())
        val e1 = visitExp(exp1, ctx0)
        val e2 = visitExp(exp2, ctx0)
        Expr.Stm(e1, e2, tpe, eff, loc)

      case (Occur.Once, Type.Pure) =>
        // Unconditionally inline
        sctx.changed.putIfAbsent(sym0, ())
        val freshVarSym = Symbol.freshVarSym(sym)
        val ctx = ctx0.addVarSubst(sym, freshVarSym)
          .addSubst(freshVarSym, SubstRange.SuspendedExpr(exp1, ctx0.subst))
        visitExp(exp2, ctx)

      case _ =>
        // Simplify and maybe do copy-propagation
        val e1 = visitExp(exp1, ctx0)
        if (isSimple(e1) && e1.eff == Type.Pure) {
          // Do copy propagation and drop let-binding
          sctx.changed.putIfAbsent(sym0, ())
          val freshVarSym = Symbol.freshVarSym(sym)
          val ctx = ctx0.addVarSubst(sym, freshVarSym).addSubst(freshVarSym, SubstRange.DoneExpr(e1))
          visitExp(exp2, ctx)
        } else {
          // Keep let-binding, add binding freshVarSym -> e1 to the set of in-scope
          // variables and consider inlining at each occurrence.
          val freshVarSym = Symbol.freshVarSym(sym)
          val ctx = ctx0.addVarSubst(sym, freshVarSym).addInScopeVar(freshVarSym, BoundKind.LetBound(e1, occur, exp1.eff))
          val e2 = visitExp(exp2, ctx)
          Expr.Let(freshVarSym, e1, e2, tpe, eff, occur, loc)
        }
    }

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) => occur match {
      case Occur.Dead =>
        // A function declaration is always pure so we do not care about the effect of exp1
        sctx.changed.putIfAbsent(sym0, ())
        visitExp(exp2, ctx0)

      case Occur.Once =>
        // It occurs exactly once in exp2, otherwise it would be OnceInLocalDef,
        // so unconditionally inline
        sctx.changed.putIfAbsent(sym0, ())
        val freshVarSym = Symbol.freshVarSym(sym)
        val exp = Expr.LocalDef(freshVarSym, fparams, exp1, exp2, tpe, eff, occur, loc)
        val ctx = ctx0.addVarSubst(sym, freshVarSym)
          .addSubst(freshVarSym, SubstRange.SuspendedExpr(exp, ctx0.subst))
        visitExp(exp2, ctx)

      case _ =>
        val freshVarSym = Symbol.freshVarSym(sym)
        val ctx1 = ctx0.addVarSubst(sym, freshVarSym)
        val e2 = visitExp(exp2, ctx1)
        val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
        val ctx2 = ctx1.addVarSubsts(varSubsts)
          .addInScopeVar(sym, BoundKind.ParameterOrPattern)
          .addInScopeVars(fps.map(fp => fp.sym -> BoundKind.ParameterOrPattern))
        val e1 = visitExp(exp1, ctx2)
        Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, occur, loc)
    }

    case Expr.Region(sym, rvar, exp, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val ctx = ctx0.addVarSubst(sym, freshVarSym).addInScopeVar(freshVarSym, BoundKind.ParameterOrPattern)
      val e = visitExp(exp, ctx)
      Expr.Region(freshVarSym, rvar, e, tpe, eff, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, ctx0)
      evalBoolExpression(e1) match {
        case FuzzyBool.True =>
          sctx.changed.putIfAbsent(sym0, ())
          visitExp(exp2, ctx0)
        case FuzzyBool.False =>
          sctx.changed.putIfAbsent(sym0, ())
          visitExp(exp3, ctx0)
        case FuzzyBool.Unknown =>
          val e2 = visitExp(exp2, ctx0)
          val e3 = visitExp(exp3, ctx0)
          Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)
      }

    case Expr.Stm(exp1, exp2, tpe, eff, loc) => exp1.eff match {
      case Type.Pure =>
        // Exp1 has no side effect and is unused
        sctx.changed.putIfAbsent(sym0, ())
        visitExp(exp2, ctx0)

      case _ =>
        val e1 = visitExp(exp1, ctx0)
        val e2 = visitExp(exp2, ctx0)
        Expr.Stm(e1, e2, tpe, eff, loc)
    }

    case Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.Discard(e, eff, loc)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      val rs = rules.map(visitMatchRule(_, ctx0))
      reduceMatch(e, rs, tpe, eff, loc)

    case Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      val rs = rules.map(visitExtMatchRule(_, ctx0))
      Expr.ExtMatch(e, rs, tpe, eff, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.VectorLit(es, tpe, eff, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, ctx0)
      val e2 = visitExp(exp2, ctx0)
      Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.VectorLength(e, loc)

    case Expr.Cast(exp, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.Cast(e, tpe, eff, loc)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      val rs = rules.map(visitCatchRule(_, ctx0))
      Expr.TryCatch(e, rs, tpe, eff, loc)

    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      val rs = rules.map(visitHandlerRule(_, ctx0))
      Expr.RunWith(e, effUse, rs, tpe, eff, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod(_, ctx0))
      Expr.NewObject(name, clazz, tpe, eff, methods, loc)
  }

  /**
    * Evaluate match if possible. The logic is rule based, for example `case _ => ` is always chosen no matter how
    * complicated the scrutinee is. If the pattern says `case Some(..) => ..` then the scrutinee is inspected to see if
    * it matches, does not match, or may match.
    *
    * The match rules are iterated top to bottom, at each step either:
    *   - Remove the rule if it can never match (either the patten cannot match or the guard is guaranteed false)
    *     and continue the rule iteration.
    *   - Remove the rule if it is known to match (either `Some(12)` matching `Some(x)` or a lenient pattern like `_`)
    *   - Otherwise, leave the rules as is.
    *
    * It is assumed that patterns do not contain duplicate variables (e.g. `case (x, x) => x`)
    * and that tuples and enums do not have mismatched arity (e.g. `case Some(x, y) => x + y`).
    */
  @tailrec
  private def reduceMatch(exp: MonoAst.Expr, rules: List[MonoAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation)(implicit sym0: Symbol.DefnSym, sctx: SharedContext): Expr = {
    rules match {
      case MonoAst.MatchRule(pat, guardOpt, ruleExp) :: rest =>
        matchRule(exp, pat, guardOpt) match {
          case MatchResult.Match(binders) =>
            // Guaranteed match - convert to let binders.
            sctx.changed.putIfAbsent(sym0, ())
            bindPatterns(binders.toSeq, ruleExp, loc)
          case MatchResult.NoMatch =>
            // Impossible match - delete and continue.
            sctx.changed.putIfAbsent(sym0, ())
            reduceMatch(exp, rest, tpe, eff, loc)
          case MatchResult.Unknown =>
            // Unknown match - do nothing.
            Expr.Match(exp, rules, tpe, eff, loc)
        }
      case Nil =>
        // Do nothing.
        Expr.Match(exp, rules, tpe, eff, loc)
    }
  }

  private sealed trait FuzzyBool

  private object FuzzyBool {

    case object True extends FuzzyBool

    case object False extends FuzzyBool

    case object Unknown extends FuzzyBool

  }

  /** Returns the fuzzy evaluation of `exp` as a boolean. */
  private def evalBoolExpression(exp: MonoAst.Expr): FuzzyBool = exp match {
    case Expr.Cst(Constant.Bool(true), _, _) => FuzzyBool.True
    case Expr.Cst(Constant.Bool(false), _, _) => FuzzyBool.False
    case _ => FuzzyBool.Unknown
  }

  private sealed trait MatchResult

  private object MatchResult {

    /**
      * An expression matches a pattern, corresponding to the given binders.
      *
      * {{{
      *   match Cons(12, tail) {
      *     case Cons(x, _) => ..
      *   }
      * }}}
      *
      * This would return `Match(Chain(Some(x) => 12), None => tail)`
      */
    case class Match(binders: Chain[(Option[Pattern.Var], MonoAst.Expr)]) extends MatchResult

    /** An expression does not match a pattern. */
    case object NoMatch extends MatchResult

    /** An expression might match a pattern - it cannot be determined at compile time. */
    case object Unknown extends MatchResult

    /**
      * A match without any binders. E.g.:
      *
      * {{{
      *   match 12 {
      *     case 12 => ..
      *   }
      * }}}
      */
    def emptyMatch(): MatchResult =
      Match(Chain.empty)

    /**
      * A match of a single binder. E.g.:
      *
      * {{{
      *   match 12 {
      *     case x => ..
      *   }
      * }}}
      */
    def singleMatch(pat: Option[Pattern.Var], exp: MonoAst.Expr): MatchResult =
      Match(Chain((pat, exp)))

    /**
      * Returns a match with no binder if `b` is true (see [[emptyMatch]]).
      *
      * Returns [[NoMatch]] if `b` is false.
      */
    def matchFromBool(b: Boolean): MatchResult =
      if (b) emptyMatch() else NoMatch

    /**
      * Concatenates two match results.
      *
      * If either result is [[NoMatch]], the output is [[NoMatch]].
      *
      * Then if either result is [[Unknown]], the output is [[Unknown]].
      *
      * Then (both results now being [[Match]]) the binders are concatenated.
      */
    def concat(mr1: MatchResult, mr2: MatchResult): MatchResult = (mr1, mr2) match {
      case (MatchResult.NoMatch, _) | (_, MatchResult.NoMatch) =>
        MatchResult.NoMatch
      case (MatchResult.Unknown, _) | (_, MatchResult.Unknown) =>
        MatchResult.Unknown
      case (MatchResult.Match(binders1), MatchResult.Match(binders2)) =>
        MatchResult.Match(binders1 ++ binders2)
    }

  }

  /** Returns the match result of `exp` against `pat` with the guard `guardOpt`. */
  private def matchRule(exp: MonoAst.Expr, pat: Pattern, guardOpt: Option[MonoAst.Expr]): MatchResult = {
    val guardVal = guardOpt.map(evalBoolExpression).getOrElse(FuzzyBool.True)
    (matchPat(exp, pat), guardVal) match {
      case (_, FuzzyBool.False) | (MatchResult.NoMatch, _) =>
        MatchResult.NoMatch
      case (MatchResult.Unknown, _) | (MatchResult.Match(_), FuzzyBool.Unknown) =>
        MatchResult.Unknown
      case (resultMatch@MatchResult.Match(_), FuzzyBool.True) =>
        resultMatch
    }
  }

  /** Returns the match result of `exp` against `pat`. */
  private def matchPat(exp: MonoAst.Expr, pat: MonoAst.Pattern): MatchResult = pat match {
    case Pattern.Wild(_, _) =>
      // Preserve the expression in case it is impure.
      MatchResult.singleMatch(None, exp)

    case v@Pattern.Var(_, _, _, _) =>
      MatchResult.singleMatch(Some(v), exp)

    case Pattern.Cst(cst, _, _) => exp match {
      case Expr.Cst(expCst, _, _) =>
        matchConstant(cst, expCst)
      case _ =>
        MatchResult.Unknown
    }

    case Pattern.Tag(symUse, pats, _, _) => exp match {
      case Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, _, _, _) =>
        if (symUse.sym != caseSym) MatchResult.NoMatch
        else {
          // `exps` and `pats` have same length for well-typed programs.
          matchPatterns(exps, pats)
        }
      case _ =>
        MatchResult.Unknown
    }

    case Pattern.Tuple(pats, _, _) => exp match {
      case Expr.ApplyAtomic(AtomicOp.Tuple, exps, _, _, _) =>
        // `exps` and `pats` have same length for well-typed programs.
        matchPatterns(exps, pats.toList)
      case _ =>
        MatchResult.Unknown
    }

    case Pattern.Record(_, _, _, _) =>
      MatchResult.Unknown

  }

  /**
    * Returns the match result for matching `exps` to the patterns of `pats`.
    *
    * If the two lists are of different lengths, [[MatchResult.Unknown]] is returned.
    *
    * N.B.: `exps` and `pats` must have the same length, otherwise [[InternalCompilerException]] is thrown.
    */
  private def matchPatterns(exps: List[MonoAst.Expr], pats: List[MonoAst.Pattern]): MatchResult = {
    if (exps.lengthCompare(pats) != 0) {
      throw InternalCompilerException(
        s"Match rule has arity ${pats.size} against ${exps.size} expressions.",
        exps.headOption.map(_.loc).getOrElse(SourceLocation.Unknown)
      )
    } else {
      // Keep the order of `exps` to maintain evaluation order.
      val res = pats.zip(exps).foldLeft(MatchResult.emptyMatch()) {
        case (acc, (innerPat, innerExp)) =>
          MatchResult.concat(acc, matchPat(innerExp, innerPat))
      }
      res
    }
  }

  /**
    * Returns the match result for two constants.
    *
    * Constants contain no binders so the function is symmetric.
    */
  private def matchConstant(cst1: Constant, cst2: Constant): MatchResult = {
    import Constant.*
    import MatchResult.*
    (cst1, cst2) match {
      case (Unit, Unit) => emptyMatch()
      case (Null, Null) => emptyMatch()
      case (Bool(v1), Bool(v2)) => matchFromBool(v1 == v2)
      case (Char(v1), Char(v2)) => matchFromBool(v1 == v2)
      case (Float32(v1), Float32(v2)) => matchFromBool(v1 == v2)
      case (Float64(v1), Float64(v2)) => matchFromBool(v1 == v2)
      case (BigDecimal(_), BigDecimal(_)) => Unknown // Avoiding static checking of object types.
      case (Int8(v1), Int8(v2)) => matchFromBool(v1 == v2)
      case (Int16(v1), Int16(v2)) => matchFromBool(v1 == v2)
      case (Int32(v1), Int32(v2)) => matchFromBool(v1 == v2)
      case (Int64(v1), Int64(v2)) => matchFromBool(v1 == v2)
      case (BigInt(_), BigInt(_)) => Unknown // Avoiding static checking of object types.
      case (Str(_), Str(_)) => Unknown // Avoiding static checking of object types.
      case (Regex(_), Regex(_)) => Unknown // Avoiding static checking of object types.
      case (RecordEmpty, RecordEmpty) => emptyMatch()
      case _ =>
        // Unrelated constants are impossible for well-typed programs.
        // Returning unknown is the safe "do nothing" choice.
        Unknown
    }
  }

  /**
    * Returns a pattern with fresh variables and a substitution mapping the old variables the fresh variables.
    *
    * If a variable is unused it is rewritten to a wildcard pattern.
    */
  private def visitPattern(pat0: Pattern)(implicit flix: Flix): (Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case Pattern.Wild(tpe, loc) =>
      (Pattern.Wild(tpe, loc), Map.empty)

    case Pattern.Var(sym, tpe, occur, loc) => occur match {
      case Occur.Unknown => throw InternalCompilerException("unexpected unknown occurrence information", loc)

      case Occur.Dead =>
        (Pattern.Wild(tpe, loc), Map.empty)

      case Occur.Once
           | Occur.OnceInLambda
           | Occur.OnceInLocalDef
           | Occur.ManyBranch
           | Occur.Many =>
        val freshVarSym = Symbol.freshVarSym(sym)
        (Pattern.Var(freshVarSym, tpe, occur, loc), Map(sym -> freshVarSym))
    }

    case Pattern.Cst(cst, tpe, loc) =>
      (Pattern.Cst(cst, tpe, loc), Map.empty)

    case Pattern.Tag(symUse, pats, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitPattern).unzip
      val varSubst = varSubsts.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (Pattern.Tag(symUse, ps, tpe, loc), varSubst)

    case Pattern.Tuple(pats, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitPattern).unzip
      val varSubst = varSubsts.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (Pattern.Tuple(ps, tpe, loc), varSubst)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitRecordLabelPattern).unzip
      val (p, varSubst1) = visitPattern(pat)
      val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
      (Pattern.Record(ps, p, tpe, loc), varSubst2)
  }

  private def visitRecordLabelPattern(pat0: Pattern.Record.RecordLabelPattern)(implicit flix: Flix): (Pattern.Record.RecordLabelPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, subst) = visitPattern(pat)
      (Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
  }

  private def visitExtPattern(pat0: MonoAst.ExtPattern)(implicit flix: Flix): (MonoAst.ExtPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case MonoAst.ExtPattern.Default(loc) =>
      (MonoAst.ExtPattern.Default(loc), Map.empty)

    case MonoAst.ExtPattern.Tag(label, pats, loc) =>
      val (ps, varSubsts) = pats.map(visitExtTagPattern).unzip
      val varSubst = varSubsts.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (MonoAst.ExtPattern.Tag(label, ps, loc), varSubst)
  }

  private def visitExtTagPattern(pat0: MonoAst.ExtTagPattern)(implicit flix: Flix): (MonoAst.ExtTagPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case MonoAst.ExtTagPattern.Wild(tpe, loc) =>
      (MonoAst.ExtTagPattern.Wild(tpe, loc), Map.empty)

    case MonoAst.ExtTagPattern.Var(sym, tpe, occur, loc) => occur match {
      case Occur.Unknown => throw InternalCompilerException("unexpected unknown occurrence information", loc)

      case Occur.Dead =>
        (MonoAst.ExtTagPattern.Wild(tpe, loc), Map.empty)

      case Occur.Once
           | Occur.OnceInLambda
           | Occur.OnceInLocalDef
           | Occur.ManyBranch
           | Occur.Many =>
        val freshVarSym = Symbol.freshVarSym(sym)
        (MonoAst.ExtTagPattern.Var(freshVarSym, tpe, occur, loc), Map(sym -> freshVarSym))
    }

    case MonoAst.ExtTagPattern.Unit(tpe, loc) =>
      (MonoAst.ExtTagPattern.Unit(tpe, loc), Map.empty)
  }

  /** Returns a formal param with a fresh symbol and a substitution mapping the old variable the fresh variable. */
  private def freshFormalParam(fp0: MonoAst.FormalParam)(implicit flix: Flix): (MonoAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = fp0 match {
    case MonoAst.FormalParam(sym, tpe, occur, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val varSubst = Map(sym -> freshVarSym)
      (MonoAst.FormalParam(freshVarSym, tpe, occur, loc), varSubst)
  }

  private def visitMatchRule(rule: MonoAst.MatchRule, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, sctx: SharedContext, root: MonoAst.Root, flix: Flix): MonoAst.MatchRule = rule match {
    case MonoAst.MatchRule(pat, guard, exp1) =>
      val (p, varSubst1) = visitPattern(pat)
      val ctx = ctx0.addVarSubsts(varSubst1).addInScopeVars(varSubst1.values.map(sym => sym -> BoundKind.ParameterOrPattern))
      val g = guard.map(visitExp(_, ctx))
      val e1 = visitExp(exp1, ctx)
      MonoAst.MatchRule(p, g, e1)
  }

  private def visitExtMatchRule(rule: MonoAst.ExtMatchRule, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, sctx: SharedContext, root: MonoAst.Root, flix: Flix): MonoAst.ExtMatchRule = rule match {
    case MonoAst.ExtMatchRule(pat, exp, loc) =>
      val (p, varSubst1) = visitExtPattern(pat)
      val ctx = ctx0.addVarSubsts(varSubst1).addInScopeVars(varSubst1.values.map(sym => sym -> BoundKind.ParameterOrPattern))
      val e = visitExp(exp, ctx)
      MonoAst.ExtMatchRule(p, e, loc)
  }

  private def visitCatchRule(rule: MonoAst.CatchRule, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, sctx: SharedContext, root: MonoAst.Root, flix: Flix): MonoAst.CatchRule = rule match {
    case MonoAst.CatchRule(sym, clazz, exp1) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val ctx = ctx0.addVarSubst(sym, freshVarSym).addInScopeVar(freshVarSym, BoundKind.ParameterOrPattern)
      val e1 = visitExp(exp1, ctx)
      MonoAst.CatchRule(freshVarSym, clazz, e1)
  }

  private def visitHandlerRule(rule: MonoAst.HandlerRule, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, sctx: SharedContext, root: MonoAst.Root, flix: Flix): MonoAst.HandlerRule = rule match {
    case MonoAst.HandlerRule(op, fparams, exp1) =>
      val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
      val ctx = ctx0.addVarSubsts(varSubsts).addInScopeVars(fps.map(fp => fp.sym -> BoundKind.ParameterOrPattern))
      val e1 = visitExp(exp1, ctx)
      MonoAst.HandlerRule(op, fps, e1)
  }

  private def visitJvmMethod(method: MonoAst.JvmMethod, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, sctx: SharedContext, root: MonoAst.Root, flix: Flix): MonoAst.JvmMethod = method match {
    case MonoAst.JvmMethod(ident, fparams, exp, retTpe, eff1, loc1) =>
      val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
      val ctx = ctx0.addVarSubsts(varSubsts).addInScopeVars(fps.map(fp => fp.sym -> BoundKind.ParameterOrPattern))
      val e = visitExp(exp, ctx)
      MonoAst.JvmMethod(ident, fps, e, retTpe, eff1, loc1)
  }

  /**
    * Performs beta-reduction, binding `exps` as let-bindings.
    *
    * The caller must visit the returned expression.
    *
    * [[bindArgs]] creates a series of let-bindings
    * {{{
    *   let sym1 = exp1;
    *   // ...
    *   let symn = expn;
    *   exp
    * }}}
    * where `symi` is the symbol of the i-th formal parameter and `exp` is the body of the function.
    *
    */
  private def bindArgs(exp: Expr, fparams: List[FormalParam], exps: List[Expr], loc: SourceLocation): Expr = {
    ListOps.zip(fparams, exps).foldRight(exp) {
      case ((fparam, arg), acc) =>
        val eff = Type.mkUnion(arg.eff, acc.eff, loc)
        Expr.Let(fparam.sym, arg, acc, acc.tpe, eff, fparam.occur, loc)
    }
  }

  /** Returns a nested let expression where the leftmost binder is the outermost let. */
  private def bindPatterns(binders: Iterable[(Option[Pattern.Var], MonoAst.Expr)], exp: MonoAst.Expr, loc: SourceLocation): MonoAst.Expr = {
    binders.foldRight(exp) {
      case ((Some(v), binderExp), acc) =>
        val eff = Type.mkUnion(binderExp.eff, acc.eff, loc)
        Expr.Let(v.sym, binderExp, acc, acc.tpe, eff, v.occur, loc)
      case ((None, binderExp), acc) =>
        val eff = Type.mkUnion(binderExp.eff, acc.eff, loc)
        Expr.Stm(binderExp, acc, acc.tpe, eff, loc)
    }
  }

  /**
    * Returns `true` if the given `defn` should be inlined.
    *
    * It is the responsibility of the caller to visit `exps` first.
    *
    * @param defn the definition of the function.
    * @param exps the arguments to the function.
    * @param ctx0 the local context.
    */
  private def shouldInlineDef(defn: MonoAst.Def, exps: List[Expr], ctx0: LocalContext)(implicit sym0: Symbol.DefnSym): Boolean = {
    if (ctx0.currentlyInlining) {
      return false
    }

    if (defn.spec.ann.isDontInline) {
      return false
    }

    if (defn.spec.ann.isInline) {
      if (defn.sym == sym0) {
        return false
      }

      return true
    }

    !defn.spec.defContext.isSelfRef &&
      (isSingleAction(defn.exp) || isSimple(defn.exp) || hasKnownLambda(exps))
  }

  /**
    * Returns `true` if there exists [[Expr.Lambda]] in `exps`.
    */
  private def hasKnownLambda(exps: List[Expr]): Boolean = {
    exps.exists(isLambda)
  }

  /**
    * Returns a [[Some]] with the definition of `sym` if it is let-bound and the [[shouldInlineVar]] predicate holds.
    * The caller should visit the expression with an empty `subst`, i.e., `visitExp(exp, ctx0.withSubst(Map.empty))`.
    *
    * Returns [[None]] otherwise.
    *
    * Throws an error if `sym` is not in scope. This also implies that it is the responsibility of the caller
    * to replace any symbol occurrence with the corresponding fresh symbol in the variable substitution.
    */
  private def useSiteInline(sym: Symbol.VarSym, ctx0: LocalContext): Option[Expr] = {
    ctx0.inScopeVars.get(sym) match {
      case Some(BoundKind.LetBound(exp, occur, eff)) if shouldInlineVar(sym, exp, occur, eff) =>
        Some(exp)

      case Some(_) =>
        None

      case None =>
        throw InternalCompilerException(s"unexpected evaluated var not in scope $sym", sym.loc)
    }
  }

  /**
    * Returns `true` if `exp` is pure and should be inlined at the occurrence of `sym`.
    *
    * A lambda should be inlined if it has occurrence information [[Occur.OnceInLambda]] or [[Occur.OnceInLocalDef]].
    *
    * `eff` is the original effect of `exp`. See [[BoundKind.LetBound]] for more information.
    */
  private def shouldInlineVar(sym: Symbol.VarSym, exp: Expr, occur: Occur, eff: Type): Boolean = (occur, eff) match {
    case (Occur.Dead, _) => throw InternalCompilerException(s"unexpected call site inline of dead variable $sym", exp.loc)
    case (Occur.Once, Type.Pure) => throw InternalCompilerException(s"unexpected call site inline of pre-inlined variable $sym", exp.loc)
    case (Occur.OnceInLambda, Type.Pure) => isLambda(exp)
    case (Occur.OnceInLocalDef, Type.Pure) => isLambda(exp)
    case (Occur.ManyBranch, Type.Pure) => false
    case (Occur.Many, Type.Pure) => false
    case _ => false // Impure so do not move expression
  }

  /** Returns `true` if `exp` is [[Expr.Cst]] and the constant is not a [[Constant.Regex]]. */
  private def isCst(exp: Expr): Boolean = exp match {
    case Expr.Cst(Constant.Regex(_), _, _) => false
    case Expr.Cst(_, _, _) => true
    case _ => false
  }

  /** Returns `true` if `exp` is [[Expr.Lambda]]. */
  private def isLambda(exp: MonoAst.Expr): Boolean = exp match {
    case Expr.Lambda(_, _, _, _) => true
    case _ => false
  }

  /**
    * Returns `true` if `exp0` is considered a trivial expression.
    *
    * A trivial expression is one of the following:
    *   - [[Expr.Var]]
    *   - Is a [[Expr.Cast]] of a trivial expression.
    *   - Any expression where [[isCst]] holds.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  @tailrec
  private def isTrivial(exp0: Expr): Boolean = exp0 match {
    case Expr.Var(_, _, _) => true
    case Expr.Cast(exp, _, _, _) => isTrivial(exp)
    case exp => isCst(exp)
  }

  /**
    * Returns `true` if `exp0` is a simple expression.
    *
    * A simple expression is a value-like expression where sub-expressions are trivial.
    */
  @tailrec
  private def isSimple(exp0: Expr): Boolean = exp0 match {
    case Expr.Lambda(_, _, _, _) => true
    case Expr.ApplyAtomic(AtomicOp.Unary(_), exps, _, _, _) => exps.forall(isTrivial)
    case Expr.ApplyAtomic(AtomicOp.Binary(_), exps, _, _, _) => exps.forall(isTrivial)
    case Expr.ApplyAtomic(AtomicOp.Tag(_), exps, _, _, _) => exps.forall(isTrivial)
    case Expr.ApplyAtomic(AtomicOp.Tuple, exps, _, _, _) => exps.forall(isTrivial)
    case Expr.ApplyAtomic(AtomicOp.ArrayLit, exps, _, _, _) => exps.forall(isTrivial)
    case Expr.ApplyAtomic(AtomicOp.StructNew(_, _, _), exps, _, _, _) => exps.forall(isTrivial)
    case Expr.Cast(exp, _, _, _) => isSimple(exp)
    case exp => isTrivial(exp)
  }

  /**
    * Returns `true` if `rule0` is a simple match rule.
    *
    * A match rule is simple if both the guard (if present) and expression are simple.
    */
  private def isSimpleMatchRule(rule0: MonoAst.MatchRule): Boolean = rule0 match {
    case MonoAst.MatchRule(_, guard, exp) =>
      guard.forall(isSimple) && isSimple(exp)
  }

  /**
    * Returns `true` if `exp0` is a single action expression.
    *
    * An expression is a single action if it performs one computational step. For example:
    * - A single call with simple arguments.
    * - A single arithmetic operation with simple arguments.
    * - A single match expression with simple scrutinee and only simple match rules.
    * - A single array operation with simple arguments.
    * - A single JVM operation with simple arguments.
    */
  @tailrec
  private def isSingleAction(exp0: Expr): Boolean = exp0 match {
    case Expr.ApplyClo(exp1, exp2, _, _, _) => isSimple(exp1) && isSimple(exp2)
    case Expr.ApplyDef(_, exps, _, _, _, _) => exps.forall(isSimple)
    case Expr.LocalDef(_, _, _, Expr.ApplyLocalDef(_, exps, _, _, _), _, _, _, _) => exps.forall(isSimple)
    case Expr.Cast(exp, _, _, _) => isSingleAction(exp)
    case Expr.Match(exp, rules, _, _, _) => isSimple(exp) && rules.forall(isSimpleMatchRule)
    case Expr.ApplyAtomic(op, exps, _, _, _) => op match {
      case AtomicOp.ArrayNew => exps.forall(isSimple)
      case AtomicOp.ArrayLoad => exps.forall(isSimple)
      case AtomicOp.ArrayStore => exps.forall(isSimple)
      case AtomicOp.ArrayLength => exps.forall(isSimple)
      case AtomicOp.InvokeMethod(_) => exps.forall(isSimple)
      case AtomicOp.InvokeStaticMethod(_) => exps.forall(isSimple)
      case AtomicOp.GetField(_) => exps.forall(isSimple)
      case AtomicOp.PutField(_) => exps.forall(isSimple)
      case AtomicOp.GetStaticField(_) => exps.forall(isSimple)
      case AtomicOp.PutStaticField(_) => exps.forall(isSimple)
      case _ => false
    }
    case _ => false
  }

  /** Represents the range of a substitution from variables to expressions. */
  private sealed trait SubstRange

  private object SubstRange {

    /**
      * An expression that will be inlined but is not yet visited.
      * We must capture the substitution from its definition site to ensure
      * we substitute the variables the inliner may have previously decided
      * to inline.
      */
    case class SuspendedExpr(exp: MonoAst.Expr, subst: Map[Symbol.VarSym, SubstRange]) extends SubstRange

    /** An expression that will be inlined but has already been visited. */
    case class DoneExpr(exp: MonoAst.Expr) extends SubstRange

  }

  /** Contains information on a variable's definition. */
  private sealed trait BoundKind

  private object BoundKind {

    /** Variable is bound by either a parameter or a pattern. Its value is unknown. */
    object ParameterOrPattern extends BoundKind

    /**
      * The right-hand side of a let-bound variable along with its occurrence information.
      *
      * Due to sub-effecting `eff` is required to store the effect of the non-visited exp.
      * When sub-effecting is enabled, the purity of an expression may change when inlining
      * a function. Consider the following example:
      * {{{
      *   def f(): Int32 \ IO = { ... non-simple pure body ... }
      *   def g(): Unit \ IO = {
      *     let res: Int32 = f();
      *     println(res)
      *   }
      * }}}
      * The definition of `f` is pure but upcast to `IO` do to sub-effecting. Inlining
      * `f` makes the right-hand side of `res` pure, but is no longer simple. However,
      * when the inliner consider `res` in the last line, the inliner will think that
      * `res` was inlined because it is used once and is pure.
      * Storing the original effect in `eff` allows the inliner to correctly consider its
      * previous decisions, since the purity / effect did not change during inlining.
      * The subsequent round of inlining may then choose to inline the let-bound variable.
      */
    case class LetBound(expr: MonoAst.Expr, occur: Occur, eff: Type) extends BoundKind

  }

  private object LocalContext {
    /** Returns the empty context with `currentlyInlining` set to `false`. */
    val Empty: LocalContext = LocalContext(Map.empty, Map.empty, Map.empty, currentlyInlining = false)
  }

  /**
    * A wrapper class for all the different inlining environments.
    *
    * @param varSubst          a substitution on variables to variables.
    * @param subst             a substitution on variables to expressions.
    * @param inScopeVars       a set of variables considered to be in scope.
    * @param currentlyInlining a flag denoting whether the current traversal is part of an inline-expansion process.
    */
  private case class LocalContext(varSubst: Map[Symbol.VarSym, Symbol.VarSym], subst: Map[Symbol.VarSym, SubstRange], inScopeVars: Map[Symbol.VarSym, BoundKind], currentlyInlining: Boolean) {

    /** Returns a [[LocalContext]] with the mapping `old -> fresh` added to [[varSubst]]. */
    def addVarSubst(oldVar: Symbol.VarSym, freshVar: Symbol.VarSym): LocalContext = {
      this.copy(varSubst = this.varSubst.updated(oldVar, freshVar))
    }

    /** Returns a [[LocalContext]] with the mappings of `subst` added to [[varSubst]]. */
    def addVarSubsts(subst: Map[Symbol.VarSym, Symbol.VarSym]): LocalContext = {
      this.copy(varSubst = this.varSubst ++ subst)
    }

    /** Returns a [[LocalContext]] with the mappings of `l` added to [[varSubst]]. */
    def addVarSubsts(l: List[Map[Symbol.VarSym, Symbol.VarSym]]): LocalContext = {
      this.copy(varSubst = l.foldLeft(this.varSubst)(_ ++ _))
    }

    /** Returns a [[LocalContext]] with the mapping `sym -> substExpr` added to [[subst]]. */
    def addSubst(sym: Symbol.VarSym, substExpr: SubstRange): LocalContext = {
      this.copy(subst = this.subst.updated(sym, substExpr))
    }

    /** Returns a [[LocalContext]] with [[subst]] overwritten by `newSubst`. */
    def withSubst(newSubst: Map[Symbol.VarSym, SubstRange]): LocalContext = {
      this.copy(subst = newSubst)
    }

    /** Returns a [[LocalContext]] with the mapping `sym -> boundKind` added to [[inScopeVars]]. */
    def addInScopeVar(sym: Symbol.VarSym, boundKind: BoundKind): LocalContext = {
      this.copy(inScopeVars = this.inScopeVars.updated(sym, boundKind))
    }

    /** Returns a [[LocalContext]] with the mappings of `mappings` added to [[inScopeVars]]. */
    def addInScopeVars(xs: Iterable[(Symbol.VarSym, BoundKind)]): LocalContext = {
      this.copy(inScopeVars = this.inScopeVars ++ xs)
    }

    /** Returns a [[LocalContext]] where [[currentlyInlining]] is set to `true`. */
    def enableInliningMode: LocalContext = {
      this.copy(currentlyInlining = true)
    }

  }

  private object SharedContext {

    /**
      * Returns a fresh [[SharedContext]].
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentHashMap(), new ConcurrentHashMap())

  }

  /**
    * A globally shared thread-safe context.
    *
    * @param changed the set of symbols of changed functions.
    * @param live    the set of symbols of live functions.
    */
  private case class SharedContext(changed: ConcurrentHashMap[Symbol.DefnSym, Unit], live: ConcurrentHashMap[Symbol.DefnSym, Unit])

}
