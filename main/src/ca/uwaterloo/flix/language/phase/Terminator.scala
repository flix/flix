/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.shared.QualifiedSym
import ca.uwaterloo.flix.language.ast.{ChangeSet, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The Terminator phase verifies that functions annotated with `@Terminates` are structurally
  * recursive — meaning every recursive call is made on a strict substructure of a formal parameter.
  *
  * It also checks:
  * - Strict positivity of data types used for structural recursion.
  * - Absence of features that could break the termination guarantee.
  * - Self-recursive local defs inside `@Terminates` functions.
  *
  * Known limitations (sound — may reject valid programs, but never accepts non-terminating ones):
  * - A local def that calls the enclosing `@Terminates` function is checked for structural
  *   decrease, but the sub-environment does not flow across the local def boundary. This means
  *   a local def that passes a strict substructure of its own parameter to the enclosing function
  *   is rejected even though it terminates. Mutual recursion between local defs is impossible
  *   because local defs have sequential scoping.
  */
object Terminator {

  /**
    * Checks termination properties of `root`.
    */
  def run(root: Root, oldRoot: Root, changeSet: ChangeSet)(implicit flix: Flix): (Root, List[TerminationError]) =
    flix.phaseNew("Terminator") {
      implicit val sctx: SharedContext = SharedContext.mk()
      implicit val _r: Root = root
      val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(
        ParOps.parMapValues(_)(visitDef))
      val traits = changeSet.updateStaleValues(root.traits, oldRoot.traits)(
        ParOps.parMapValues(_)(visitTrait))
      val instances = changeSet.updateStaleValueLists(root.instances, oldRoot.instances,
        (i1: TypedAst.Instance, i2: TypedAst.Instance) => i1.tpe.typeConstructor == i2.tpe.typeConstructor)(
        ParOps.parMapValueList(_)(visitInstance))
      (root.copy(defs = defs, traits = traits, instances = instances), sctx.errors.asScala.toList)
    }

  /**
    * Classifies the structural depth of a variable relative to a formal parameter.
    *
    *  - `Alias`: the variable is equal to, or a renaming of, the formal parameter.
    *    It has not been destructured, so passing it to a recursive call does ''not''
    *    constitute a decrease.
    *  - `StrictSub`: the variable was extracted from inside a data constructor
    *    (e.g. via a `Tag` pattern), so it is strictly smaller than the formal parameter
    *    and may be passed to a recursive call.
    */
  private sealed trait Strictness

  /** The variable is equal to or a renaming of the formal parameter — not yet destructured. */
  private case object Alias extends Strictness

  /** The variable is a strict substructure of the formal parameter. */
  private case object StrictSub extends Strictness

  /**
    * Describes the structural relationship between a local variable and a formal parameter
    * of the enclosing `@Terminates` function.
    *
    * During the termination check, every local variable that can be traced back to a formal
    * parameter is assigned a `ParamRelation`. The `rootParam` field identifies which formal
    * parameter the variable descends from, and `strictness` records whether the variable is
    * known to be a ''strict'' substructure (i.e. extracted from inside a constructor) or merely
    * an alias (i.e. equal to or a renaming of the parameter).
    *
    * @param rootParam  the formal parameter this variable is structurally related to.
    * @param strictness whether the variable is a strict substructure or merely an alias.
    */
  private case class ParamRelation(rootParam: Symbol.VarSym, strictness: Strictness)

  /**
    * Represents the "self" symbol of the enclosing `@Terminates` function, used
    * to detect self-recursive calls. Wraps either a `DefnSym` or a `SigSym`.
    */
  private sealed trait SelfSym {
    def sym: QualifiedSym
  }

  private case class SelfDef(defnSym: Symbol.DefnSym) extends SelfSym {
    def sym: QualifiedSym = defnSym
  }

  private case class SelfSig(sigSym: Symbol.SigSym) extends SelfSym {
    def sym: QualifiedSym = sigSym
  }

  /** An instance def that implements a trait sig — matches both `ApplyDef` and `ApplySig`. */
  private case class SelfInstanceDef(defnSym: Symbol.DefnSym, sigSym: Symbol.SigSym) extends SelfSym {
    def sym: QualifiedSym = defnSym
  }

  /** A self-recursive local def inside a `@Terminates` function. */
  private case class SelfLocalDef(varSym: Symbol.VarSym, parentSym: QualifiedSym) extends SelfSym {
    def sym: QualifiedSym = parentSym
  }

  /**
    * Groups the state needed to check structural recursion at one nesting level.
    *
    * When visiting a local def body, a new context for the local def is pushed
    * onto the front of the context list, so that calls to both the local def
    * ''and'' any enclosing `@Terminates` function are checked.
    */
  private case class RecursionContext(selfSym: SelfSym, fparams: List[FormalParam], env: SubEnv)

  /**
    * Tracks the structural relationship between local variables and the formal parameters
    * of the enclosing `@Terminates` function.
    *
    * As the termination checker walks the expression tree it maintains a `SubEnv` that grows
    * whenever a pattern match or let-binding introduces a new variable that can be traced
    * back to a formal parameter. The environment is then queried at every self-recursive
    * call site to verify that at least one argument is a strict substructure of the
    * corresponding formal parameter.
    */
  private case class SubEnv(m: Map[Symbol.VarSym, ParamRelation]) {

    /** Looks up the structural relation of `sym`, if tracked. */
    def lookup(sym: Symbol.VarSym): Option[ParamRelation] = m.get(sym)

    /** Binds `sym` with the given relation, returning an extended environment. */
    def bind(sym: Symbol.VarSym, rel: ParamRelation): SubEnv = SubEnv(m + (sym -> rel))

    /**
      * If `from` is a tracked variable, binds `to` with the same relation.
      *
      * Used for let-bindings like `let y = x` where `x` is tracked — `y` inherits
      * whatever structural relationship `x` has with a formal parameter.
      */
    def propagateAlias(from: Symbol.VarSym, to: Symbol.VarSym): SubEnv =
      m.get(from) match {
        case Some(rel) => SubEnv(m + (to -> rel))
        case None => this
      }
  }

  private object SubEnv {
    /**
      * Creates the initial environment for a `@Terminates` function.
      *
      * Each formal parameter is mapped to itself with `Alias` strictness,
      * meaning "this variable ''is'' the parameter, not a substructure of it".
      */
    def init(fparams: List[FormalParam]): SubEnv =
      SubEnv(fparams.map(fp => fp.bnd.sym -> ParamRelation(fp.bnd.sym, Alias)).toMap)
  }

  /** Checks a trait's default sig implementations for termination properties. */
  private def visitTrait(trt: Trait)(implicit sctx: SharedContext, root: Root): Trait = {
    trt.sigs.foreach(visitSig)
    trt
  }

  /** Checks an instance's def implementations for termination properties. */
  private def visitInstance(inst: Instance)(implicit sctx: SharedContext, root: Root): Instance = {
    val traitSym = inst.trt.sym
    root.traits.get(traitSym) match {
      case Some(trt) =>
        inst.defs.foreach { defn =>
          if (defn.spec.ann.isTerminates) {
            trt.sigs.find(_.sym.name == defn.sym.text) match {
              case Some(sig) =>
                checkStrictPositivity(defn.spec.fparams, defn.sym)
                val fparams = defn.spec.fparams
                visitExp(List(RecursionContext(SelfInstanceDef(defn.sym, sig.sym), fparams, SubEnv.init(fparams))), defn.exp)
              case None =>
                visitDef(defn)
            }
          }
        }
      case None =>
        inst.defs.foreach(visitDef)
    }
    inst
  }

  /** Checks a trait default implementation for termination properties if annotated with @Terminates. */
  private def visitSig(sig: Sig)(implicit sctx: SharedContext, root: Root): Unit = {
    sig.exp.foreach { exp =>
      if (sig.spec.ann.isTerminates) {
        checkStrictPositivity(sig.spec.fparams, sig.sym)
        val fparams = sig.spec.fparams
        visitExp(List(RecursionContext(SelfSig(sig.sym), fparams, SubEnv.init(fparams))), exp)
      }
    }
  }

  /** Checks a definition for termination properties if annotated with @Terminates. */
  private def visitDef(defn: Def)(implicit sctx: SharedContext, root: Root): Def = {
    if (defn.spec.ann.isTerminates) {
      checkStrictPositivity(defn.spec.fparams, defn.sym)
      val fparams = defn.spec.fparams
      visitExp(List(RecursionContext(SelfDef(defn.sym), fparams, SubEnv.init(fparams))), defn.exp)
    }
    defn
  }

  // =========================================================================
  // Self-call matching
  // =========================================================================

  /**
    * If `exp` is a self-recursive call matching `selfSym`, returns the call arguments and location.
    */
  private def matchSelf(selfSym: SelfSym, exp: Expr): Option[(List[Expr], SourceLocation)] =
    (selfSym, exp) match {
      case (SelfDef(sym), Expr.ApplyDef(symUse, exps, _, _, _, _, loc)) if symUse.sym == sym => Some((exps, loc))
      case (SelfSig(sym), Expr.ApplySig(symUse, exps, _, _, _, _, _, loc)) if symUse.sym == sym => Some((exps, loc))
      case (SelfInstanceDef(defSym, _), Expr.ApplyDef(symUse, exps, _, _, _, _, loc)) if symUse.sym == defSym => Some((exps, loc))
      case (SelfInstanceDef(_, sigSym), Expr.ApplySig(symUse, exps, _, _, _, _, _, loc)) if symUse.sym == sigSym => Some((exps, loc))
      case (SelfLocalDef(varSym, _), Expr.ApplyLocalDef(symUse, exps, _, _, _, loc)) if symUse.sym == varSym => Some((exps, loc))
      case _ => None
    }

  // =========================================================================
  // Unified expression visitor — structural recursion + forbidden features
  // =========================================================================

  /**
    * Recursively visits `exp`, performing two checks simultaneously:
    *
    *   - **Forbidden features**: Every expression is checked regardless of recursion.
    *   - **Structural recursion**: Self-recursive calls are verified to pass a strict
    *     substructure of a formal parameter, and the substructure environment `env` is
    *     threaded through pattern matches and let-bindings.
    *
    * The `contexts` list contains one [[RecursionContext]] per nesting level (innermost first).
    * When a local def body is entered, a new context for the local def is pushed onto the front;
    * calls to both the local def and any enclosing function are checked against their respective
    * contexts.
    *
    * @param contexts the recursion contexts, innermost first.
    * @param exp0     the expression to check.
    */
  private def visitExp(contexts: List[RecursionContext], exp0: Expr)(implicit sctx: SharedContext, root: Root): Unit = {

    /** The symbol of the top-level `@Terminates` function (used for forbidden-expression errors). */
    val topSym = contexts.last.selfSym.sym

    def findSelfRecursiveCall(exp: Expr): Option[(RecursionContext, List[Expr], SourceLocation)] =
      contexts.iterator.flatMap { ctx =>
        matchSelf(ctx.selfSym, exp).map { case (exps, loc) => (ctx, exps, loc) }
      }.nextOption()

    def visit(exp0: Expr): Unit = findSelfRecursiveCall(exp0) match {
      // --- Self-recursive call (structural recursion check) ---
      case Some((ctx, exps, loc)) =>
        val argInfos = exps.zip(ctx.fparams).map {
          case (Expr.Var(sym, _, _), fp) =>
            val paramName = fp.bnd.sym.text
            val argText = sym.text
            val status = ctx.env.lookup(sym) match {
              case None =>
                TerminationError.ArgStatus.Untracked
              case Some(ParamRelation(rp, StrictSub)) if rp == fp.bnd.sym =>
                TerminationError.ArgStatus.Decreasing
              case Some(ParamRelation(rp, StrictSub)) =>
                TerminationError.ArgStatus.WrongParam(rp)
              case Some(ParamRelation(rp, Alias)) =>
                TerminationError.ArgStatus.AliasOf(rp)
            }
            TerminationError.ArgInfo(paramName, argText, status)
          case (_, fp) =>
            TerminationError.ArgInfo(fp.bnd.sym.text, "<expr>", TerminationError.ArgStatus.NotAVariable)
        }
        val hasDecreasingArg = argInfos.exists(_.status == TerminationError.ArgStatus.Decreasing)
        if (!hasDecreasingArg) {
          sctx.errors.add(TerminationError.NonStructuralRecursion(ctx.selfSym.sym, argInfos, loc))
        }
        exps.foreach(visit)

      case None => exp0 match {
      // --- Match: extend env in all contexts ---
      case Expr.Match(scrutinee, rules, _, _, _) =>
        visit(scrutinee)
        rules.foreach {
          case MatchRule(pat, guard, body, _) =>
            val extContexts = contexts.map(ctx =>
              ctx.copy(env = extendEnvFromScrutinee(ctx.env, scrutinee, pat)))
            guard.foreach(visitExp(extContexts, _))
            visitExp(extContexts, body)
        }

      // --- Let: propagate alias in all contexts ---
      case Expr.Let(bnd, exp1, exp2, _, _, _) =>
        visit(exp1)
        val extContexts = exp1 match {
          case Expr.Var(sym, _, _) =>
            contexts.map(ctx => ctx.copy(env = ctx.env.propagateAlias(from = sym, to = bnd.sym)))
          case _ => contexts
        }
        visitExp(extContexts, exp2)

      // --- LocalDef: push new context, visit body with extended list ---
      case Expr.LocalDef(bnd, localFparams, exp1, exp2, _, _, _) =>
        val localCtx = RecursionContext(
          SelfLocalDef(bnd.sym, contexts.head.selfSym.sym),
          localFparams,
          SubEnv.init(localFparams))
        checkStrictPositivity(localFparams, contexts.head.selfSym.sym)
        visitExp(localCtx :: contexts, exp1)
        visit(exp2)

      // --- All other expressions (TypedAst declaration order) ---

      case Expr.Cst(_, _, _) => ()
      case Expr.Var(_, _, _) => ()
      case Expr.Hole(_, _, _, _, _) => ()
      case Expr.HoleWithExp(e, _, _, _, _) => visit(e)
      case Expr.OpenAs(_, e, _, _) => visit(e)
      case Expr.Use(_, _, e, _) => visit(e)
      case Expr.Lambda(_, e, _, _) => visit(e)

      case Expr.ApplyClo(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)

      case Expr.ApplyDef(_, exps, _, _, _, _, _) => exps.foreach(visit)
      case Expr.ApplyLocalDef(_, exps, _, _, _, _) => exps.foreach(visit)
      case Expr.ApplyOp(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.ApplySig(_, exps, _, _, _, _, _, _) => exps.foreach(visit)
      case Expr.Unary(_, e, _, _, _) => visit(e)
      case Expr.Binary(_, e1, e2, _, _, _) =>
        visit(e1)
        visit(e2)
      case Expr.Region(_, _, e, _, _, _) => visit(e)
      case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
        visit(e1)
        visit(e2)
        visit(e3)
      case Expr.Stm(e1, e2, _, _, _) =>
        visit(e1)
        visit(e2)
      case Expr.Discard(e, _, _) => visit(e)
      case Expr.TypeMatch(e, rules, _, _, _) =>
        visit(e)
        rules.foreach(r => visit(r.exp))
      case Expr.RestrictableChoose(_, e, rules, _, _, _) =>
        visit(e)
        rules.foreach(r => visit(r.exp))
      case Expr.ExtMatch(e, rules, _, _, _) =>
        visit(e)
        rules.foreach(r => visit(r.exp))
      case Expr.Tag(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.RestrictableTag(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.ExtTag(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.Tuple(exps, _, _, _) => exps.foreach(visit)
      case Expr.RecordSelect(e, _, _, _, _) => visit(e)
      case Expr.RecordExtend(_, e1, e2, _, _, _) =>
        visit(e1)
        visit(e2)
      case Expr.RecordRestrict(_, e, _, _, _) => visit(e)
      case Expr.ArrayLit(exps, e, _, _, _) =>
        exps.foreach(visit)
        visit(e)

      case Expr.ArrayNew(e1, e2, e3, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)
        visit(e3)

      case Expr.ArrayLoad(e1, e2, _, _, _) =>
        visit(e1)
        visit(e2)
      case Expr.ArrayLength(e, _, _) => visit(e)

      case Expr.ArrayStore(e1, e2, e3, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)
        visit(e3)

      case Expr.StructNew(_, fields, region, _, _, _) =>
        fields.foreach(f => visit(f._2))
        region.foreach(visit)
      case Expr.StructGet(e, _, _, _, _) => visit(e)

      case Expr.StructPut(e1, _, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)

      case Expr.VectorLit(exps, _, _, _) => exps.foreach(visit)
      case Expr.VectorLoad(e1, e2, _, _, _) =>
        visit(e1)
        visit(e2)
      case Expr.VectorLength(e, _) => visit(e)
      case Expr.Ascribe(e, _, _, _, _, _) => visit(e)
      case Expr.InstanceOf(e, _, _) => visit(e)
      case Expr.CheckedCast(_, e, _, _, _) => visit(e)

      case Expr.UncheckedCast(e, _, _, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.Unsafe(e, _, _, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.Without(e, _, _, _, _) => visit(e)
      case Expr.TryCatch(e, rules, _, _, _) =>
        visit(e)
        rules.foreach(r => visit(r.exp))

      case Expr.Throw(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.Handler(_, rules, _, _, _, _, _) => rules.foreach(r => visit(r.exp))
      case Expr.RunWith(e1, e2, _, _, _) =>
        visit(e1)
        visit(e2)

      case Expr.InvokeConstructor(_, exps, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        exps.foreach(visit)

      case Expr.InvokeMethod(_, e, exps, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)
        exps.foreach(visit)

      case Expr.InvokeStaticMethod(_, exps, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        exps.foreach(visit)

      case Expr.GetField(_, e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.PutField(_, e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)

      case Expr.GetStaticField(_, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))

      case Expr.PutStaticField(_, e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.NewObject(_, _, _, _, methods, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        methods.foreach(m => visit(m.exp))

      case Expr.NewChannel(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.GetChannel(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.PutChannel(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)

      case Expr.SelectChannel(rules, default, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        rules.foreach { r =>
          visit(r.chan)
          visit(r.exp)
        }
        default.foreach(visit)

      case Expr.Spawn(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)

      case Expr.ParYield(frags, e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        frags.foreach(f => visit(f.exp))
        visit(e)

      case Expr.Lazy(e, _, _) => visit(e)

      case Expr.Force(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e)

      case Expr.FixpointConstraintSet(_, _, _) => ()
      case Expr.FixpointLambda(_, e, _, _, _) => visit(e)

      case Expr.FixpointMerge(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        visit(e1)
        visit(e2)

      case Expr.FixpointQueryWithProvenance(exps, _, _, _, _, _) => exps.foreach(visit)
      case Expr.FixpointQueryWithSelect(exps, q, sels, _, where, _, _, _, _) =>
        exps.foreach(visit)
        visit(q)
        sels.foreach(visit)
        where.foreach(visit)

      case Expr.FixpointSolveWithProject(exps, _, _, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        exps.foreach(visit)

      case Expr.FixpointInjectInto(exps, _, _, _, _) => exps.foreach(visit)
      case Expr.Error(_, _, _) => ()
      }
    }

    visit(exp0)
  }

  // =========================================================================
  // Pattern environment helpers
  // =========================================================================

  /**
    * Pairs a scrutinee expression with its pattern and extends the sub-env accordingly.
    *
    * Handles two cases recursively:
    *  - `Expr.Var` + any pattern — looks up the var in the env and delegates to `extendEnvFromPattern`.
    *  - `Expr.Tuple` + `Pattern.Tuple` — zips the tuple elements with the sub-patterns and recurses
    *    on each pair, accumulating env extensions. This naturally supports nested tuples.
    */
  private def extendEnvFromScrutinee(env: SubEnv, scrutinee: Expr, pat: Pattern): SubEnv =
    (scrutinee, pat) match {
      case (Expr.Var(sym, _, _), _) =>
        env.lookup(sym) match {
          case Some(rel) => extendEnvFromPattern(env, pat, rel.rootParam)
          case None      => env
        }
      case (Expr.Tuple(exps, _, _, _), Pattern.Tuple(pats, _, _)) =>
        exps.zip(pats.toList).foldLeft(env) {
          case (acc, (expr, subPat)) => extendEnvFromScrutinee(acc, expr, subPat)
        }
      case _ => env
    }

  /**
    * Extends the substructure environment based on a pattern match.
    *
    * Variables bound inside `Pattern.Tag` sub-patterns are strict substructures of the root parameter.
    * Variables bound by `Pattern.Var` at the top level are aliases (not strict substructures).
    */
  private def extendEnvFromPattern(env: SubEnv, pat: Pattern, rootParam: Symbol.VarSym): SubEnv = pat match {
    case Pattern.Tag(_, pats, _, _) =>
      pats.foldLeft(env)((acc, p) => collectStrictSubstructures(acc, p, rootParam))
    case Pattern.Tuple(pats, _, _) =>
      pats.toList.foldLeft(env)((acc, p) => extendEnvFromPattern(acc, p, rootParam))
    case Pattern.Var(bnd, _, _) =>
      env.bind(bnd.sym, ParamRelation(rootParam, Alias))
    case _ => env
  }

  /**
    * Collects strict substructure bindings from a pattern.
    * All variables found inside a Tag pattern are strict substructures.
    */
  private def collectStrictSubstructures(env: SubEnv, pat: Pattern, rootParam: Symbol.VarSym): SubEnv = pat match {
    case Pattern.Var(bnd, _, _) =>
      env.bind(bnd.sym, ParamRelation(rootParam, StrictSub))
    case Pattern.Tag(_, pats, _, _) =>
      pats.foldLeft(env)((acc, p) => collectStrictSubstructures(acc, p, rootParam))
    case Pattern.Tuple(pats, _, _) =>
      pats.toList.foldLeft(env)((acc, p) => collectStrictSubstructures(acc, p, rootParam))
    case Pattern.Wild(_, _) => env
    case Pattern.Cst(_, _, _) => env
    case Pattern.Record(pats, restPat, _, _) =>
      val env1 = pats.foldLeft(env)((acc, rp) => collectStrictSubstructures(acc, rp.pat, rootParam))
      collectStrictSubstructures(env1, restPat, rootParam)
    case Pattern.Error(_, _) => env
  }

  // =========================================================================
  // Strict Positivity Check
  // =========================================================================

  /**
    * Checks that the types of formal parameters are strictly positive.
    */
  private def checkStrictPositivity(fparams: List[FormalParam], sym: QualifiedSym)(implicit sctx: SharedContext, root: Root): Unit = {
    for (fparam <- fparams) {
      fparam.tpe.typeConstructor match {
        case Some(TypeConstructor.Enum(enumSym, _)) =>
          root.enums.get(enumSym).foreach { enm =>
            findNonPositiveCase(enumSym, enm).foreach { caseSym =>
              sctx.errors.add(TerminationError.NonStrictlyPositiveType(sym, caseSym, fparam.loc))
            }
          }
        case _ => // Not an enum type, nothing to check
      }
    }
  }

  /**
    * Returns the first enum case that violates strict positivity, if any.
    * A case violates strict positivity if the enum's recursive type constructor
    * appears in a negative position in one of its fields.
    */
  private def findNonPositiveCase(enumSym: Symbol.EnumSym, enm: TypedAst.Enum): Option[Symbol.CaseSym] = {
    enm.cases.values.find { caze =>
      !caze.tpes.forall(checkPositivity(enumSym, _, positive = true))
    }.map(_.sym)
  }

  /**
    * Checks whether `enumSym` appears only in positive positions within `tpe`.
    */
  private def checkPositivity(enumSym: Symbol.EnumSym, tpe: Type, positive: Boolean): Boolean = {
    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) if sym == enumSym =>
        positive

      case Some(TypeConstructor.Arrow(_)) =>
        val argTypes = tpe.arrowArgTypes
        val resultType = tpe.arrowResultType
        argTypes.forall(checkPositivity(enumSym, _, !positive)) &&
          checkPositivity(enumSym, resultType, positive)

      case _ =>
        tpe.typeArguments.forall(checkPositivity(enumSym, _, positive))
    }
  }

  // =========================================================================
  // SharedContext
  // =========================================================================

  private object SharedContext {
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  private case class SharedContext(errors: ConcurrentLinkedQueue[TerminationError])

}
