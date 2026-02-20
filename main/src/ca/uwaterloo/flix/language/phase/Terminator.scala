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
import scala.collection.mutable
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
      val updatedSigs = traits.values.flatMap(_.sigs).map(s => s.sym -> s).toMap
      (root.copy(defs = defs, traits = traits, instances = instances, sigs = root.sigs ++ updatedSigs), sctx.errors.asScala.toList)
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

  private object LocalContext {
    def mk(): LocalContext = new LocalContext(mutable.HashMap.empty)
  }

  private case class LocalContext(decreasingParams: mutable.Map[SelfSym, mutable.Set[Int]]) {
    def addDecreasing(selfSym: SelfSym, idx: Int): Unit =
      decreasingParams.getOrElseUpdate(selfSym, mutable.Set.empty) += idx

    def getDecreasing(selfSym: SelfSym): Set[Int] =
      decreasingParams.getOrElse(selfSym, Set.empty).toSet
  }

  /** Checks a trait's default sig implementations for termination properties. */
  private def visitTrait(trt: Trait)(implicit sctx: SharedContext, root: Root): Trait = {
    val updatedSigs = trt.sigs.map(visitSig)
    trt.copy(sigs = updatedSigs)
  }

  /** Checks an instance's def implementations for termination properties. */
  private def visitInstance(inst: Instance)(implicit sctx: SharedContext, root: Root): Instance = {
    val traitSym = inst.trt.sym
    val updatedDefs = root.traits.get(traitSym) match {
      case Some(trt) =>
        inst.defs.map { defn =>
          if (defn.spec.ann.isTerminates) {
            trt.sigs.find(_.sym.name == defn.sym.text) match {
              case Some(sig) =>
                implicit val lctx: LocalContext = LocalContext.mk()
                checkStrictPositivity(defn.spec.fparams, defn.sym)
                val fparams = defn.spec.fparams
                val selfSym = SelfInstanceDef(defn.sym, sig.sym)
                val newExp = visitExp(List(RecursionContext(selfSym, fparams, SubEnv.init(fparams))), defn.exp)
                val decreasingIndices = lctx.getDecreasing(selfSym)
                val newFparams = fparams.zipWithIndex.map { case (fp, i) =>
                  if (decreasingIndices.contains(i)) fp.copy(isDecreasing = true) else fp
                }
                defn.copy(spec = defn.spec.copy(fparams = newFparams), exp = newExp)
              case None => visitDef(defn)
            }
          } else defn
        }
      case None => inst.defs.map(visitDef)
    }
    inst.copy(defs = updatedDefs)
  }

  /** Checks a trait default implementation for termination properties if annotated with @Terminates. */
  private def visitSig(sig: Sig)(implicit sctx: SharedContext, root: Root): Sig = {
    sig.exp match {
      case Some(exp) if sig.spec.ann.isTerminates =>
        implicit val lctx: LocalContext = LocalContext.mk()
        checkStrictPositivity(sig.spec.fparams, sig.sym)
        val fparams = sig.spec.fparams
        val selfSym = SelfSig(sig.sym)
        val newExp = visitExp(List(RecursionContext(selfSym, fparams, SubEnv.init(fparams))), exp)
        val decreasingIndices = lctx.getDecreasing(selfSym)
        val newFparams = fparams.zipWithIndex.map { case (fp, i) =>
          if (decreasingIndices.contains(i)) fp.copy(isDecreasing = true) else fp
        }
        sig.copy(spec = sig.spec.copy(fparams = newFparams), exp = Some(newExp))
      case _ => sig
    }
  }

  /** Checks a definition for termination properties if annotated with @Terminates. */
  private def visitDef(defn: Def)(implicit sctx: SharedContext, root: Root): Def = {
    if (defn.spec.ann.isTerminates) {
      implicit val lctx: LocalContext = LocalContext.mk()
      checkStrictPositivity(defn.spec.fparams, defn.sym)
      val fparams = defn.spec.fparams
      val selfSym = SelfDef(defn.sym)
      val newExp = visitExp(List(RecursionContext(selfSym, fparams, SubEnv.init(fparams))), defn.exp)
      val decreasingIndices = lctx.getDecreasing(selfSym)
      val newFparams = fparams.zipWithIndex.map { case (fp, i) =>
        if (decreasingIndices.contains(i)) fp.copy(isDecreasing = true) else fp
      }
      defn.copy(spec = defn.spec.copy(fparams = newFparams), exp = newExp)
    } else {
      defn
    }
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
  private def visitExp(contexts: List[RecursionContext], exp0: Expr)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): Expr = {

    /** The symbol of the top-level `@Terminates` function (used for forbidden-expression errors). */
    val topSym = contexts.last.selfSym.sym

    def findSelfRecursiveCall(exp: Expr): Option[(RecursionContext, List[Expr], SourceLocation)] =
      contexts.iterator.flatMap { ctx =>
        matchSelf(ctx.selfSym, exp).map { case (exps, loc) => (ctx, exps, loc) }
      }.nextOption()

    def visit(exp0: Expr): Expr = findSelfRecursiveCall(exp0) match {
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
        argInfos.zipWithIndex.foreach {
          case (info, idx) if info.status == TerminationError.ArgStatus.Decreasing =>
            lctx.addDecreasing(ctx.selfSym, idx)
          case _ => ()
        }
        exps.foreach(visit)
        exp0

      case None => exp0 match {
      // --- Match: extend env in all contexts ---
      case Expr.Match(scrutinee, rules, tpe, eff, loc) =>
        val newScrutinee = visit(scrutinee)
        val newRules = rules.map {
          case MatchRule(pat, guard, body, ruleLoc) =>
            val extContexts = contexts.map(ctx =>
              ctx.copy(env = extendEnvFromScrutinee(ctx.env, scrutinee, pat)))
            val newGuard = guard.map(visitExp(extContexts, _))
            val newBody = visitExp(extContexts, body)
            MatchRule(pat, newGuard, newBody, ruleLoc)
        }
        Expr.Match(newScrutinee, newRules, tpe, eff, loc)

      // --- Let: propagate alias in all contexts ---
      case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
        val newExp1 = visit(exp1)
        val extContexts = exp1 match {
          case Expr.Var(sym, _, _) =>
            contexts.map(ctx => ctx.copy(env = ctx.env.propagateAlias(from = sym, to = bnd.sym)))
          case _ => contexts
        }
        val newExp2 = visitExp(extContexts, exp2)
        Expr.Let(bnd, newExp1, newExp2, tpe, eff, loc)

      // --- LocalDef: push new context, visit body with extended list ---
      case Expr.LocalDef(bnd, localFparams, exp1, exp2, tpe, eff, loc) =>
        val localSelfSym = SelfLocalDef(bnd.sym, contexts.head.selfSym.sym)
        val localCtx = RecursionContext(localSelfSym, localFparams, SubEnv.init(localFparams))
        checkStrictPositivity(localFparams, contexts.head.selfSym.sym)
        val newExp1 = visitExp(localCtx :: contexts, exp1)
        val decreasingIndices = lctx.getDecreasing(localSelfSym)
        val newFparams = localFparams.zipWithIndex.map { case (fp, i) =>
          if (decreasingIndices.contains(i)) fp.copy(isDecreasing = true) else fp
        }
        val newExp2 = visit(exp2)
        Expr.LocalDef(bnd, newFparams, newExp1, newExp2, tpe, eff, loc)

      // --- All other expressions (TypedAst declaration order) ---

      case Expr.Cst(_, _, _) => exp0
      case Expr.Var(_, _, _) => exp0
      case Expr.Hole(_, _, _, _, _) => exp0
      case Expr.HoleWithExp(e, tpe, eff, purity, loc) => Expr.HoleWithExp(visit(e), tpe, eff, purity, loc)
      case Expr.OpenAs(sym, e, tpe, loc) => Expr.OpenAs(sym, visit(e), tpe, loc)
      case Expr.Use(sym, alias, e, loc) => Expr.Use(sym, alias, visit(e), loc)
      case Expr.Lambda(fparam, e, tpe, loc) => Expr.Lambda(fparam, visit(e), tpe, loc)

      case Expr.ApplyClo(e1, e2, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.ApplyClo(visit(e1), visit(e2), tpe, eff, loc)

      case Expr.ApplyDef(symUse, exps, itpe, tpe, eff, purity, loc) =>
        root.defs.get(symUse.sym) match {
          case Some(defn) if !defn.spec.ann.isTerminates =>
            sctx.errors.add(TerminationError.NonTerminatingCall(topSym, symUse.sym, loc))
          case _ => ()
        }
        Expr.ApplyDef(symUse, exps.map(visit), itpe, tpe, eff, purity, loc)
      case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, loc) =>
        Expr.ApplyLocalDef(symUse, exps.map(visit), arrowTpe, tpe, eff, loc)

      case Expr.ApplyOp(symUse, exps, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.ApplyOp(symUse, exps.map(visit), tpe, eff, loc)

      case Expr.ApplySig(symUse, exps, itpe, tpe, eff, purity, isEq, loc) =>
        Expr.ApplySig(symUse, exps.map(visit), itpe, tpe, eff, purity, isEq, loc) // TODO: More complex.

      case Expr.Unary(sop, e, tpe, eff, loc) => Expr.Unary(sop, visit(e), tpe, eff, loc)
      case Expr.Binary(sop, e1, e2, tpe, eff, loc) => Expr.Binary(sop, visit(e1), visit(e2), tpe, eff, loc)
      case Expr.Region(kind, sym, e, tpe, eff, loc) => Expr.Region(kind, sym, visit(e), tpe, eff, loc)
      case Expr.IfThenElse(e1, e2, e3, tpe, eff, loc) => Expr.IfThenElse(visit(e1), visit(e2), visit(e3), tpe, eff, loc)
      case Expr.Stm(e1, e2, tpe, eff, loc) => Expr.Stm(visit(e1), visit(e2), tpe, eff, loc)
      case Expr.Discard(e, eff, loc) => Expr.Discard(visit(e), eff, loc)
      case Expr.TypeMatch(e, rules, tpe, eff, loc) =>
        Expr.TypeMatch(visit(e), rules.map(r => r.copy(exp = visit(r.exp))), tpe, eff, loc)
      case Expr.RestrictableChoose(star, e, rules, tpe, eff, loc) =>
        Expr.RestrictableChoose(star, visit(e), rules.map(r => r.copy(exp = visit(r.exp))), tpe, eff, loc)
      case Expr.ExtMatch(e, rules, tpe, eff, loc) =>
        Expr.ExtMatch(visit(e), rules.map(r => r.copy(exp = visit(r.exp))), tpe, eff, loc)
      case Expr.Tag(sym, exps, tpe, eff, loc) => Expr.Tag(sym, exps.map(visit), tpe, eff, loc)
      case Expr.RestrictableTag(sym, exps, tpe, eff, loc) => Expr.RestrictableTag(sym, exps.map(visit), tpe, eff, loc)
      case Expr.ExtTag(sym, exps, tpe, eff, loc) => Expr.ExtTag(sym, exps.map(visit), tpe, eff, loc)
      case Expr.Tuple(exps, tpe, eff, loc) => Expr.Tuple(exps.map(visit), tpe, eff, loc)
      case Expr.RecordSelect(e, field, tpe, eff, loc) => Expr.RecordSelect(visit(e), field, tpe, eff, loc)
      case Expr.RecordExtend(field, e1, e2, tpe, eff, loc) => Expr.RecordExtend(field, visit(e1), visit(e2), tpe, eff, loc)
      case Expr.RecordRestrict(field, e, tpe, eff, loc) => Expr.RecordRestrict(field, visit(e), tpe, eff, loc)
      case Expr.ArrayLit(exps, e, tpe, eff, loc) =>
        Expr.ArrayLit(exps.map(visit), visit(e), tpe, eff, loc)

      case Expr.ArrayNew(e1, e2, e3, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.ArrayNew(visit(e1), visit(e2), visit(e3), tpe, eff, loc)

      case Expr.ArrayLoad(e1, e2, tpe, eff, loc) => Expr.ArrayLoad(visit(e1), visit(e2), tpe, eff, loc)
      case Expr.ArrayLength(e, tpe, loc) => Expr.ArrayLength(visit(e), tpe, loc)

      case Expr.ArrayStore(e1, e2, e3, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.ArrayStore(visit(e1), visit(e2), visit(e3), eff, loc)

      case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
        Expr.StructNew(sym, fields.map { case (f, e) => (f, visit(e)) }, region.map(visit), tpe, eff, loc)
      case Expr.StructGet(e, field, tpe, eff, loc) => Expr.StructGet(visit(e), field, tpe, eff, loc)

      case Expr.StructPut(e1, field, e2, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.StructPut(visit(e1), field, visit(e2), tpe, eff, loc)

      case Expr.VectorLit(exps, tpe, eff, loc) => Expr.VectorLit(exps.map(visit), tpe, eff, loc)
      case Expr.VectorLoad(e1, e2, tpe, eff, loc) => Expr.VectorLoad(visit(e1), visit(e2), tpe, eff, loc)
      case Expr.VectorLength(e, loc) => Expr.VectorLength(visit(e), loc)
      case Expr.Ascribe(e, tpe, eff, purity, expectedEff, loc) => Expr.Ascribe(visit(e), tpe, eff, purity, expectedEff, loc)
      case Expr.InstanceOf(e, clazz, loc) => Expr.InstanceOf(visit(e), clazz, loc)
      case Expr.CheckedCast(cast, e, tpe, eff, loc) => Expr.CheckedCast(cast, visit(e), tpe, eff, loc)

      case Expr.UncheckedCast(e, declaredType, declaredEff, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.UncheckedCast(visit(e), declaredType, declaredEff, tpe, eff, loc)

      case Expr.Unsafe(e, sym, tpe, eff, purity, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.Unsafe(visit(e), sym, tpe, eff, purity, loc)

      case Expr.Without(e, effUse, tpe, eff, loc) => Expr.Without(visit(e), effUse, tpe, eff, loc)
      case Expr.TryCatch(e, rules, tpe, eff, loc) =>
        Expr.TryCatch(visit(e), rules.map(r => r.copy(exp = visit(r.exp))), tpe, eff, loc)

      case Expr.Throw(e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.Throw(visit(e), tpe, eff, loc)

      case Expr.Handler(sym, rules, tpe, eff, purity, evar, loc) =>
        Expr.Handler(sym, rules.map(r => r.copy(exp = visit(r.exp))), tpe, eff, purity, evar, loc)
      case Expr.RunWith(e1, e2, tpe, eff, loc) => Expr.RunWith(visit(e1), visit(e2), tpe, eff, loc)

      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.InvokeConstructor(constructor, exps.map(visit), tpe, eff, loc)

      case Expr.InvokeMethod(method, e, exps, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.InvokeMethod(method, visit(e), exps.map(visit), tpe, eff, loc)

      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.InvokeStaticMethod(method, exps.map(visit), tpe, eff, loc)

      case Expr.GetField(field, e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.GetField(field, visit(e), tpe, eff, loc)

      case Expr.PutField(field, e1, e2, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.PutField(field, visit(e1), visit(e2), tpe, eff, loc)

      case Expr.GetStaticField(_, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        exp0

      case Expr.PutStaticField(field, e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.PutStaticField(field, visit(e), tpe, eff, loc)

      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.NewObject(name, clazz, tpe, eff, methods.map(m => m.copy(exp = visit(m.exp))), loc)

      case Expr.NewChannel(e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.NewChannel(visit(e), tpe, eff, loc)

      case Expr.GetChannel(e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.GetChannel(visit(e), tpe, eff, loc)

      case Expr.PutChannel(e1, e2, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.PutChannel(visit(e1), visit(e2), tpe, eff, loc)

      case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.SelectChannel(
          rules.map(r => r.copy(chan = visit(r.chan), exp = visit(r.exp))),
          default.map(visit), tpe, eff, loc)

      case Expr.Spawn(e1, e2, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.Spawn(visit(e1), visit(e2), tpe, eff, loc)

      case Expr.ParYield(frags, e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.ParYield(frags.map(f => f.copy(exp = visit(f.exp))), visit(e), tpe, eff, loc)

      case Expr.Lazy(e, tpe, loc) => Expr.Lazy(visit(e), tpe, loc)

      case Expr.Force(e, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.Force(visit(e), tpe, eff, loc)

      case Expr.FixpointConstraintSet(_, _, _) => exp0
      case Expr.FixpointLambda(pparams, e, tpe, eff, loc) => Expr.FixpointLambda(pparams, visit(e), tpe, eff, loc)

      case Expr.FixpointMerge(e1, e2, tpe, eff, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.FixpointMerge(visit(e1), visit(e2), tpe, eff, loc)

      case Expr.FixpointQueryWithProvenance(exps, selects, from, tpe, eff, loc) =>
        Expr.FixpointQueryWithProvenance(exps.map(visit), selects, from, tpe, eff, loc)
      case Expr.FixpointQueryWithSelect(exps, q, sels, guard, where, tpe, eff, from, loc) =>
        Expr.FixpointQueryWithSelect(exps.map(visit), visit(q), sels.map(visit), guard, where.map(visit), tpe, eff, from, loc)

      case Expr.FixpointSolveWithProject(exps, optNames, tpe, eff, stf, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(topSym, loc))
        Expr.FixpointSolveWithProject(exps.map(visit), optNames, tpe, eff, stf, loc)

      case Expr.FixpointInjectInto(exps, idents, tpe, eff, loc) => Expr.FixpointInjectInto(exps.map(visit), idents, tpe, eff, loc)
      case Expr.Error(_, _, _) => exp0
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
