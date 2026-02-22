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
import ca.uwaterloo.flix.language.ast.TypedAst.ApplyPosition
import ca.uwaterloo.flix.language.ast.shared.{Decreasing, QualifiedSym}
import ca.uwaterloo.flix.language.ast.{ChangeSet, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The Terminator phase performs two classes of verification and decorates all Apply nodes
  * with their expression position (tail or non-tail) for use by later phases.
  *
  * **Structural termination** (`@Terminates` functions):
  *   - **Structural recursion**: Every self-recursive call must pass a strict substructure of a
  *     formal parameter (obtained via pattern matching on a `Tag`) in the corresponding argument
  *     position. Aliases (let-bindings, variable patterns) are tracked but do not count as decreasing.
  *     Tuple scrutinees are supported: `match (x, y)` with `(Pat1, Pat2)` extends the environment
  *     element-wise.
  *   - **Strict positivity**: Enum types used as formal parameters must be strictly positive — the
  *     enum's own type constructor must not appear in a negative (contravariant) position in any case.
  *   - **Callee restriction**: Calls to non-`@Terminates` defs are rejected.
  *   - **Closure restriction**: Closure applications (`ApplyClo`) are only allowed when the callee
  *     is a formal parameter (or a let-alias thereof) of the top-level `@Terminates` function.
  *   - **Forbidden features**: Expressions that could break the termination guarantee (e.g. mutable
  *     state, Java interop, concurrency, channels, unchecked casts, `unsafe`) are rejected.
  *   - **Local defs**: Self-recursive local defs inside `@Terminates` functions are checked
  *     independently for structural decrease. Local defs calling the enclosing function are
  *     checked against the enclosing function's sub-environment.
  *   - **Traits and instances**: The checks apply to trait default implementations and instance
  *     defs annotated with `@Terminates`. Instance defs recognize self-recursion via both
  *     `ApplyDef` and `ApplySig`.
  *
  * **Tail-call verification** (`@Tailrec` functions):
  *   - Every self-recursive call in a `@Tailrec` function must be in tail position.
  *   - **Non-recursive rejection**: A `@Tailrec` function (or local def) that contains no
  *     self-recursive calls is rejected. The annotation is only meaningful on functions that
  *     actually call themselves.
  *   - `@Tailrec` is independent of `@Terminates`: a function may have either or both annotations.
  *
  * Known unsoundness (may accept non-terminating programs):
  *   - **Trait sig calls (`ApplySig`) are unchecked**: Calls to trait signatures are not subject
  *     to the callee restriction because ubiquitous operations like `+`, `-`, and `==` are trait
  *     sigs. A `@Terminates` function can therefore call a trait sig whose instance implementation
  *     does not terminate.
  *   - **Mutual recursion among top-level defs**: Two `@Terminates` defs can call each other
  *     without any structural decrease, since the structural recursion check only tracks
  *     self-recursive calls. Local defs are not affected because they are not in scope of
  *     each other.
  *
  * Known limitations (sound — may reject valid programs, but never accepts non-terminating ones):
  *   - A local def that calls the enclosing `@Terminates` function is checked for structural
  *     decrease, but the sub-environment does not flow across the local def boundary. This means
  *     a local def that passes a strict substructure of its own parameter to the enclosing function
  *     is rejected even though it terminates.
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

  /** Wraps a `VarSym` as a `QualifiedSym` for standalone `@Terminates` local defs. */
  private case class VarSymAsQualified(varSym: Symbol.VarSym) extends QualifiedSym {
    def namespace: List[String] = Nil
    def name: String = varSym.text
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
                val newExp = visitExp(List(RecursionContext(selfSym, fparams, SubEnv.init(fparams))), defn.exp, ApplyPosition.OtherTail)
                val decreasingIndices = lctx.getDecreasing(selfSym)
                val newFparams = fparams.zipWithIndex.map { case (fp, i) =>
                  if (decreasingIndices.contains(i)) fp.copy(decreasing = Decreasing.StrictlyDecreasing) else fp
                }
                defn.copy(spec = defn.spec.copy(fparams = newFparams), exp = newExp)
              case None => visitDef(defn)
            }
          } else {
            implicit val lctx: LocalContext = LocalContext.mk()
            defn.copy(exp = visitExp(Nil, defn.exp, ApplyPosition.OtherTail))
          }
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
        val newExp = visitExp(List(RecursionContext(selfSym, fparams, SubEnv.init(fparams))), exp, ApplyPosition.OtherTail)
        val decreasingIndices = lctx.getDecreasing(selfSym)
        val newFparams = fparams.zipWithIndex.map { case (fp, i) =>
          if (decreasingIndices.contains(i)) fp.copy(decreasing = Decreasing.StrictlyDecreasing) else fp
        }
        sig.copy(spec = sig.spec.copy(fparams = newFparams), exp = Some(newExp))
      case Some(exp) =>
        implicit val lctx: LocalContext = LocalContext.mk()
        sig.copy(exp = Some(visitExp(Nil, exp, ApplyPosition.OtherTail)))
      case _ => sig
    }
  }

  /** Checks a definition for termination properties if annotated with @Terminates. */
  private def visitDef(defn: Def)(implicit sctx: SharedContext, root: Root): Def = {
    if (defn.spec.ann.isTerminates) {
      implicit val lctx: LocalContext =
        if (defn.spec.ann.isTailRecursive) LocalContext.mkTailRec(defn.sym)
        else LocalContext.mk()
      checkStrictPositivity(defn.spec.fparams, defn.sym)
      val fparams = defn.spec.fparams
      val selfSym = SelfDef(defn.sym)
      val newExp = visitExp(List(RecursionContext(selfSym, fparams, SubEnv.init(fparams))), defn.exp, ApplyPosition.OtherTail)
      if (defn.spec.ann.isTailRecursive && !lctx.selfCallObservedDef.contains(defn.sym)) {
        sctx.errors.add(TerminationError.NonRecursiveTailRec(defn.sym, defn.sym.loc))
      }
      val decreasingIndices = lctx.getDecreasing(selfSym)
      val newFparams = fparams.zipWithIndex.map { case (fp, i) =>
        if (decreasingIndices.contains(i)) fp.copy(decreasing = Decreasing.StrictlyDecreasing) else fp
      }
      defn.copy(spec = defn.spec.copy(fparams = newFparams), exp = newExp)
    } else {
      implicit val lctx: LocalContext =
        if (defn.spec.ann.isTailRecursive) LocalContext.mkTailRec(defn.sym)
        else LocalContext.mk()
      val newExp = visitExp(Nil, defn.exp, ApplyPosition.OtherTail)
      if (defn.spec.ann.isTailRecursive && !lctx.selfCallObservedDef.contains(defn.sym)) {
        sctx.errors.add(TerminationError.NonRecursiveTailRec(defn.sym, defn.sym.loc))
      }
      defn.copy(exp = newExp)
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Self-call matching
  ////////////////////////////////////////////////////////////////////////////

  /**
    * If `exp` is a self-recursive call matching `selfSym`, returns the call arguments and location.
    */
  private def matchSelf(selfSym: SelfSym, exp: Expr): Option[(List[Expr], SourceLocation)] =
    (selfSym, exp) match {
      case (SelfDef(sym), Expr.ApplyDef(symUse, exps, _, _, _, _, _, loc)) if symUse.sym == sym => Some((exps, loc))
      case (SelfSig(sym), Expr.ApplySig(symUse, exps, _, _, _, _, _, _, loc)) if symUse.sym == sym => Some((exps, loc))
      case (SelfInstanceDef(defSym, _), Expr.ApplyDef(symUse, exps, _, _, _, _, _, loc)) if symUse.sym == defSym => Some((exps, loc))
      case (SelfInstanceDef(_, sigSym), Expr.ApplySig(symUse, exps, _, _, _, _, _, _, loc)) if symUse.sym == sigSym => Some((exps, loc))
      case (SelfLocalDef(varSym, _), Expr.ApplyLocalDef(symUse, exps, _, _, _, _, loc)) if symUse.sym == varSym => Some((exps, loc))
      case _ => None
    }

  ////////////////////////////////////////////////////////////////////////////
  // Unified expression visitor — structural recursion + forbidden features
  ////////////////////////////////////////////////////////////////////////////

  /**
    * Recursively visits `exp`, performing two checks simultaneously:
    *
    *   - **Forbidden features**: Every expression is checked regardless of recursion.
    *   - **Structural recursion**: Self-recursive calls are verified to pass a strict
    *     substructure of a formal parameter, and the substructure environment `env` is
    *     threaded through pattern matches and let-bindings.
    *
    * Additionally, propagates expression position (`pos`) through the tree and decorates
    * Apply nodes with their position (tail or non-tail).
    *
    * The `contexts` list contains one [[RecursionContext]] per nesting level (innermost first).
    * When a local def body is entered, a new context for the local def is pushed onto the front;
    * calls to both the local def and any enclosing function are checked against their respective
    * contexts.
    *
    * @param contexts the recursion contexts, innermost first.
    * @param exp0     the expression to check.
    * @param pos      the expression position (tail or non-tail).
    */
  private def visitExp(contexts: List[RecursionContext], exp0: Expr, pos: ApplyPosition)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): Expr = {

    val topSymOpt = contexts.lastOption.map(_.selfSym.sym)

    findSelfRecursiveCall(contexts, exp0) match {
      // --- Self-recursive call (structural recursion check) ---
      case Some((ctx, args, loc)) =>
        lctx.tailRecSym.foreach { trSym =>
          ctx.selfSym match {
            case SelfDef(defnSym) if defnSym == trSym && pos == ApplyPosition.NonTail =>
              sctx.errors.add(TerminationError.NonTailRecursiveCall(trSym, loc))
            case SelfInstanceDef(defnSym, _) if defnSym == trSym && pos == ApplyPosition.NonTail =>
              sctx.errors.add(TerminationError.NonTailRecursiveCall(trSym, loc))
            case _ => ()
          }
        }
        // Record the self-call observation
        ctx.selfSym match {
          case SelfDef(defnSym) => lctx.selfCallObservedDef += defnSym
          case SelfInstanceDef(defnSym, _) => lctx.selfCallObservedDef += defnSym
          case SelfLocalDef(varSym, _) => lctx.selfCallObservedLocal += varSym
          case _ => ()
        }
        // Check @Tailrec on local defs
        ctx.selfSym match {
          case SelfLocalDef(varSym, _) if lctx.tailRecLocalSyms.contains(varSym) && pos == ApplyPosition.NonTail =>
            sctx.errors.add(TerminationError.NonTailRecursiveLocalCall(varSym, loc))
          case _ => ()
        }
        val argInfos = args.zip(ctx.fparams).map {
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
        args.foreach(visitExp(contexts, _, ApplyPosition.NonTail))
        decorateApply(exp0, if (pos == ApplyPosition.NonTail) ApplyPosition.NonTail else ApplyPosition.SelfTail)

      case None => exp0 match {
        // --- Match: extend env in all contexts ---
        case Expr.Match(exp1, rules0, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val rs = rules0.map(visitMatchRule(contexts, exp1, _, pos))
          Expr.Match(e, rs, tpe, eff, loc)

        // --- Let: propagate alias in all contexts ---
        case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val extContexts = exp1 match {
            case Expr.Var(sym, _, _) =>
              contexts.map(ctx => ctx.copy(env = ctx.env.propagateAlias(from = sym, to = bnd.sym)))
            case _ => contexts
          }
          val e2 = visitExp(extContexts, exp2, pos)
          Expr.Let(bnd, e1, e2, tpe, eff, loc)

        // --- LocalDef: push new context, visit body with extended list ---
        case Expr.LocalDef(ann, bnd, fparams0, exp1, exp2, tpe, eff, loc) =>
          val isTerminates = ann.isTerminates
          val isTailRec = ann.isTailRecursive

          // Register tail-rec tracking for this local def
          if (isTailRec) lctx.tailRecLocalSyms += bnd.sym

          val prevLocalDefSym = lctx.currentLocalDefSym
          lctx.currentLocalDefSym = Some(bnd.sym)
          val (e1, fps) = if (contexts.nonEmpty || isTerminates) {
            val parentSym = contexts.headOption.map(_.selfSym.sym).getOrElse(VarSymAsQualified(bnd.sym))
            val localSelfSym = SelfLocalDef(bnd.sym, parentSym)
            val localCtx = RecursionContext(localSelfSym, fparams0, SubEnv.init(fparams0))
            checkStrictPositivity(fparams0, parentSym)
            val body = visitExp(localCtx :: contexts, exp1, ApplyPosition.OtherTail)
            val decreasingIndices = lctx.getDecreasing(localSelfSym)
            val newFps = fparams0.zipWithIndex.map { case (fp, i) =>
              if (decreasingIndices.contains(i)) fp.copy(decreasing = Decreasing.StrictlyDecreasing) else fp
            }
            (body, newFps)
          } else {
            (visitExp(Nil, exp1, ApplyPosition.OtherTail), fparams0)
          }
          if (isTailRec && !lctx.selfCallObservedLocal.contains(bnd.sym)) {
            sctx.errors.add(TerminationError.NonRecursiveLocalTailRec(bnd.sym, loc))
          }
          lctx.currentLocalDefSym = prevLocalDefSym
          val e2 = visitExp(contexts, exp2, pos)
          Expr.LocalDef(ann, bnd, fps, e1, e2, tpe, eff, loc)

        // --- ApplyClo: check closure restriction ---
        case Expr.ApplyClo(exp1, exp2, tpe, eff, _, loc) =>
          topSymOpt match {
            case Some(_) =>
              if (!isTopLevelFormalParam(contexts, exp1)) {
                checkForbidden(contexts, loc)
              }
            case None => () // Not inside a @Terminates function — no closure restriction to check.
          }
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.ApplyClo(e1, e2, tpe, eff, pos, loc)

        // --- ApplyDef: check callee restriction ---
        case Expr.ApplyDef(symUse, exps0, itpe, tpe, eff, purity, _, loc) =>
          lctx.tailRecSym.foreach { trSym =>
            if (symUse.sym == trSym) {
              lctx.selfCallObservedDef += trSym
              if (pos == ApplyPosition.NonTail) {
                sctx.errors.add(TerminationError.NonTailRecursiveCall(trSym, loc))
              }
            }
          }
          topSymOpt match {
            case Some(topSym) =>
              root.defs.get(symUse.sym) match {
                case Some(defn) if !defn.spec.ann.isTerminates =>
                  sctx.errors.add(TerminationError.NonTerminatingCall(topSym, symUse.sym, loc))
                case _ => ()
              }
            case None => () // Not inside a @Terminates function — no callee restriction to check.
          }
          val isSelfCall = lctx.tailRecSym.contains(symUse.sym)
          val ap = if (isSelfCall && pos != ApplyPosition.NonTail) ApplyPosition.SelfTail else pos
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.ApplyDef(symUse, es, itpe, tpe, eff, purity, ap, loc)

        // --- All other expressions (TypedAst declaration order) ---

        case Expr.Cst(_, _, _) => exp0

        case Expr.Var(_, _, _) => exp0

        case Expr.Hole(_, _, _, _, _) => exp0

        case Expr.HoleWithExp(exp1, tpe, eff, purity, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.HoleWithExp(e, tpe, eff, purity, loc)

        case Expr.OpenAs(sym, exp1, tpe, loc) =>
          val e = visitExp(contexts, exp1, pos)
          Expr.OpenAs(sym, e, tpe, loc)

        case Expr.Use(sym, alias, exp1, loc) =>
          val e = visitExp(contexts, exp1, pos)
          Expr.Use(sym, alias, e, loc)

        case Expr.Lambda(fparam, exp1, tpe, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Lambda(fparam, e, tpe, loc)

        case Expr.ApplyLocalDef(symUse, exps0, arrowTpe, tpe, eff, _, loc) =>
          if (lctx.currentLocalDefSym.contains(symUse.sym)) {
            lctx.selfCallObservedLocal += symUse.sym
            if (lctx.tailRecLocalSyms.contains(symUse.sym) && pos == ApplyPosition.NonTail) {
              sctx.errors.add(TerminationError.NonTailRecursiveLocalCall(symUse.sym, loc))
            }
          }
          val isSelfCall = lctx.currentLocalDefSym.contains(symUse.sym)
          val ap = if (isSelfCall && pos != ApplyPosition.NonTail) ApplyPosition.SelfTail else pos
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.ApplyLocalDef(symUse, es, arrowTpe, tpe, eff, ap, loc)

        case Expr.ApplyOp(symUse, exps0, tpe, eff, _, loc) =>
          checkForbidden(contexts, loc)
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.ApplyOp(symUse, es, tpe, eff, pos, loc)

        case Expr.ApplySig(symUse, exps0, itpe, tpe, eff, purity, isEq, _, loc) =>
          // TODO: Difficult to disallow due to e.g. +, -, == and so on.
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.ApplySig(symUse, es, itpe, tpe, eff, purity, isEq, pos, loc)

        case Expr.Unary(sop, exp1, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Unary(sop, e, tpe, eff, loc)

        case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.Binary(sop, e1, e2, tpe, eff, loc)

        case Expr.Region(kind, sym, exp1, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, pos)
          Expr.Region(kind, sym, e, tpe, eff, loc)

        case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, pos)
          val e3 = visitExp(contexts, exp3, pos)
          Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

        case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, pos)
          Expr.Stm(e1, e2, tpe, eff, loc)

        case Expr.Discard(exp1, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Discard(e, eff, loc)

        case Expr.TypeMatch(exp1, rules0, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val rs = rules0.map(visitTypeMatchRule(contexts, _, pos))
          Expr.TypeMatch(e, rs, tpe, eff, loc)

        case Expr.RestrictableChoose(star, exp1, rules0, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val rs = rules0.map(visitRestrictableChooseRule(contexts, _, pos))
          Expr.RestrictableChoose(star, e, rs, tpe, eff, loc)

        case Expr.ExtMatch(exp1, rules0, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val rs = rules0.map(visitExtMatchRule(contexts, _, pos))
          Expr.ExtMatch(e, rs, tpe, eff, loc)

        case Expr.Tag(sym, exps0, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.Tag(sym, es, tpe, eff, loc)

        case Expr.RestrictableTag(sym, exps0, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.RestrictableTag(sym, es, tpe, eff, loc)

        case Expr.ExtTag(sym, exps0, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.ExtTag(sym, es, tpe, eff, loc)

        case Expr.Tuple(exps0, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.Tuple(es, tpe, eff, loc)

        case Expr.RecordSelect(exp1, field, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.RecordSelect(e, field, tpe, eff, loc)

        case Expr.RecordExtend(field, exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.RecordExtend(field, e1, e2, tpe, eff, loc)

        case Expr.RecordRestrict(field, exp1, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.RecordRestrict(field, e, tpe, eff, loc)

        case Expr.ArrayLit(exps0, exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.ArrayLit(es, e, tpe, eff, loc)

        case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          val e3 = visitExp(contexts, exp3, ApplyPosition.NonTail)
          Expr.ArrayNew(e1, e2, e3, tpe, eff, loc)

        case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.ArrayLoad(e1, e2, tpe, eff, loc)

        case Expr.ArrayLength(exp1, tpe, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.ArrayLength(e, tpe, loc)

        case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          val e3 = visitExp(contexts, exp3, ApplyPosition.NonTail)
          Expr.ArrayStore(e1, e2, e3, eff, loc)

        case Expr.StructNew(sym, fields0, region0, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val fs = fields0.map { case (f, exp1) => (f, visitExp(contexts, exp1, ApplyPosition.NonTail)) }
          val r = region0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.StructNew(sym, fs, r, tpe, eff, loc)

        case Expr.StructGet(exp1, field, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.StructGet(e, field, tpe, eff, loc)

        case Expr.StructPut(exp1, field, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.StructPut(e1, field, e2, tpe, eff, loc)

        case Expr.VectorLit(exps0, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.VectorLit(es, tpe, eff, loc)

        case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.VectorLoad(e1, e2, tpe, eff, loc)

        case Expr.VectorLength(exp1, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.VectorLength(e, loc)

        case Expr.Ascribe(exp1, tpe, eff, purity, expectedEff, loc) =>
          val e = visitExp(contexts, exp1, pos)
          Expr.Ascribe(e, tpe, eff, purity, expectedEff, loc)

        case Expr.InstanceOf(exp1, clazz, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.InstanceOf(e, clazz, loc)

        case Expr.CheckedCast(cast, exp1, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, pos)
          Expr.CheckedCast(cast, e, tpe, eff, loc)

        case Expr.UncheckedCast(exp1, declaredType, declaredEff, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.UncheckedCast(e, declaredType, declaredEff, tpe, eff, loc)

        case Expr.Unsafe(exp1, sym, tpe, eff, purity, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Unsafe(e, sym, tpe, eff, purity, loc)

        case Expr.Without(exp1, effUse, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, pos)
          Expr.Without(e, effUse, tpe, eff, loc)

        case Expr.TryCatch(exp1, rules0, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, pos)
          val rs = rules0.map(visitCatchRule(contexts, _, pos))
          Expr.TryCatch(e, rs, tpe, eff, loc)

        case Expr.Throw(exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Throw(e, tpe, eff, loc)

        case Expr.Handler(sym, rules0, tpe, eff, purity, evar, loc) =>
          checkForbidden(contexts, loc)
          val rs = rules0.map(visitHandlerRule(contexts, _))
          Expr.Handler(sym, rs, tpe, eff, purity, evar, loc)

        case Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.RunWith(e1, e2, tpe, eff, loc)

        case Expr.InvokeConstructor(constructor, exps0, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.InvokeConstructor(constructor, es, tpe, eff, loc)

        case Expr.InvokeMethod(method, exp1, exps0, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.InvokeMethod(method, e, es, tpe, eff, loc)

        case Expr.InvokeStaticMethod(method, exps0, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.InvokeStaticMethod(method, es, tpe, eff, loc)

        case Expr.GetField(field, exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.GetField(field, e, tpe, eff, loc)

        case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.PutField(field, e1, e2, tpe, eff, loc)

        case Expr.GetStaticField(_, _, _, loc) =>
          checkForbidden(contexts, loc)
          exp0

        case Expr.PutStaticField(field, exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.PutStaticField(field, e, tpe, eff, loc)

        case Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
          checkForbidden(contexts, loc)
          val ms = methods0.map(visitJvmMethod(contexts, _))
          Expr.NewObject(name, clazz, tpe, eff, ms, loc)

        case Expr.NewChannel(exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.NewChannel(e, tpe, eff, loc)

        case Expr.GetChannel(exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.GetChannel(e, tpe, eff, loc)

        case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.PutChannel(e1, e2, tpe, eff, loc)

        case Expr.SelectChannel(rules0, default0, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val rs = rules0.map(visitSelectChannelRule(contexts, _))
          val d = default0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.SelectChannel(rs, d, tpe, eff, loc)

        case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.Spawn(e1, e2, tpe, eff, loc)

        case Expr.ParYield(frags0, exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val fs = frags0.map(visitParYieldFrag(contexts, _))
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.ParYield(fs, e, tpe, eff, loc)

        case Expr.Lazy(exp1, tpe, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Lazy(e, tpe, loc)

        case Expr.Force(exp1, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.Force(e, tpe, eff, loc)

        case Expr.FixpointConstraintSet(_, _, _) => exp0

        case Expr.FixpointLambda(pparams, exp1, tpe, eff, loc) =>
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          Expr.FixpointLambda(pparams, e, tpe, eff, loc)

        case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
          checkForbidden(contexts, loc)
          val e1 = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val e2 = visitExp(contexts, exp2, ApplyPosition.NonTail)
          Expr.FixpointMerge(e1, e2, tpe, eff, loc)

        case Expr.FixpointQueryWithProvenance(exps0, selects, from, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.FixpointQueryWithProvenance(es, selects, from, tpe, eff, loc)

        case Expr.FixpointQueryWithSelect(exps0, exp1, sels0, guard, where0, tpe, eff, from, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          val e = visitExp(contexts, exp1, ApplyPosition.NonTail)
          val ss = sels0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          val w = where0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.FixpointQueryWithSelect(es, e, ss, guard, w, tpe, eff, from, loc)

        case Expr.FixpointSolveWithProject(exps0, optNames, tpe, eff, stf, loc) =>
          checkForbidden(contexts, loc)
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.FixpointSolveWithProject(es, optNames, tpe, eff, stf, loc)

        case Expr.FixpointInjectInto(exps0, idents, tpe, eff, loc) =>
          val es = exps0.map(visitExp(contexts, _, ApplyPosition.NonTail))
          Expr.FixpointInjectInto(es, idents, tpe, eff, loc)

        case Expr.Error(_, _, _) => exp0
      }
    }
  }

  /** Sets the `pos` field on an Apply node. */
  private def decorateApply(exp: Expr, pos: ApplyPosition): Expr = exp match {
    case e: Expr.ApplyDef      => e.copy(pos = pos)
    case e: Expr.ApplySig      => e.copy(pos = pos)
    case e: Expr.ApplyLocalDef => e.copy(pos = pos)
    case e: Expr.ApplyClo      => e.copy(pos = pos)
    case e: Expr.ApplyOp       => e.copy(pos = pos)
    case _                     => exp
  }

  /** Reports a [[TerminationError.ForbiddenExpression]] when `contexts` is non-empty. */
  private def checkForbidden(contexts: List[RecursionContext], loc: SourceLocation)(implicit sctx: SharedContext): Unit =
    contexts.lastOption.foreach(ctx => sctx.errors.add(TerminationError.ForbiddenExpression(ctx.selfSym.sym, loc)))


  /**
    * Returns `true` if `exp` is a (possibly curried) closure application whose root callee
    * is a formal parameter (or let-alias thereof) of the top-level `@Terminates` function.
    */
  private def isTopLevelFormalParam(contexts: List[RecursionContext], exp: Expr): Boolean = {
    /** Returns the root callee VarSym if exp is a (possibly curried) application on a Var. */
    @tailrec
    def rootCalleeVar(exp: Expr): Option[Symbol.VarSym] = exp match {
      case Expr.Var(sym, _, _)                 => Some(sym)
      case Expr.ApplyClo(inner, _, _, _, _, _) => rootCalleeVar(inner)
      case _                                   => None
    }
    rootCalleeVar(exp).exists(sym => contexts.last.env.lookup(sym).isDefined)
  }

  /**
    * If `exp` is a self-recursive call matching any context, returns the context, call arguments, and location.
    */
  private def findSelfRecursiveCall(contexts: List[RecursionContext], exp: Expr): Option[(RecursionContext, List[Expr], SourceLocation)] =
    contexts.iterator.flatMap { ctx =>
      matchSelf(ctx.selfSym, exp).map { case (exps, loc) => (ctx, exps, loc) }
    }.nextOption()

  /** Visits a match rule, extending the sub-environment based on the scrutinee. */
  private def visitMatchRule(contexts: List[RecursionContext], scrutinee: Expr, rule0: MatchRule, pos: ApplyPosition)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): MatchRule = rule0 match {
    case MatchRule(pat, guard0, body0, loc) =>
      val extContexts = contexts.map(ctx =>
        ctx.copy(env = extendEnvFromScrutinee(ctx.env, scrutinee, pat)))
      val guard = guard0.map(visitExp(extContexts, _, ApplyPosition.NonTail))
      val body = visitExp(extContexts, body0, pos)
      MatchRule(pat, guard, body, loc)
  }

  /** Visits a type match rule. */
  private def visitTypeMatchRule(contexts: List[RecursionContext], rule0: TypeMatchRule, pos: ApplyPosition)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): TypeMatchRule = {
    val e = visitExp(contexts, rule0.exp, pos)
    rule0.copy(exp = e)
  }

  /** Visits a restrictable choose rule. */
  private def visitRestrictableChooseRule(contexts: List[RecursionContext], rule0: RestrictableChooseRule, pos: ApplyPosition)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): RestrictableChooseRule = {
    val e = visitExp(contexts, rule0.exp, pos)
    rule0.copy(exp = e)
  }

  /** Visits an ext match rule. */
  private def visitExtMatchRule(contexts: List[RecursionContext], rule0: ExtMatchRule, pos: ApplyPosition)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): ExtMatchRule = {
    val e = visitExp(contexts, rule0.exp, pos)
    rule0.copy(exp = e)
  }

  /** Visits a catch rule. */
  private def visitCatchRule(contexts: List[RecursionContext], rule0: CatchRule, pos: ApplyPosition)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): CatchRule = {
    val e = visitExp(contexts, rule0.exp, pos)
    rule0.copy(exp = e)
  }

  /** Visits a handler rule. Body starts fresh at Tail. */
  private def visitHandlerRule(contexts: List[RecursionContext], rule0: HandlerRule)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): HandlerRule = {
    val e = visitExp(contexts, rule0.exp, ApplyPosition.OtherTail)
    rule0.copy(exp = e)
  }

  /** Visits a select channel rule. Bodies are NonTail. */
  private def visitSelectChannelRule(contexts: List[RecursionContext], rule0: SelectChannelRule)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): SelectChannelRule = {
    val e1 = visitExp(contexts, rule0.chan, ApplyPosition.NonTail)
    val e2 = visitExp(contexts, rule0.exp, ApplyPosition.NonTail)
    rule0.copy(chan = e1, exp = e2)
  }

  /** Visits a par yield fragment. Bodies are NonTail. */
  private def visitParYieldFrag(contexts: List[RecursionContext], frag0: ParYieldFragment)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): ParYieldFragment = {
    val e = visitExp(contexts, frag0.exp, ApplyPosition.NonTail)
    frag0.copy(exp = e)
  }

  /** Visits a JVM method. Body starts fresh at Tail. */
  private def visitJvmMethod(contexts: List[RecursionContext], method0: JvmMethod)(implicit lctx: LocalContext, sctx: SharedContext, root: Root): JvmMethod = {
    val e = visitExp(contexts, method0.exp, ApplyPosition.OtherTail)
    method0.copy(exp = e)
  }

  ////////////////////////////////////////////////////////////////////////////
  // Pattern environment helpers
  ////////////////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////////////////
  // Strict Positivity Check
  ////////////////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////////////////
  // LocalContext
  ////////////////////////////////////////////////////////////////////////////

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = new LocalContext(mutable.HashMap.empty, None, mutable.Set.empty, mutable.Set.empty, mutable.Set.empty)
    def mkTailRec(sym: Symbol.DefnSym): LocalContext = new LocalContext(mutable.HashMap.empty, Some(sym), mutable.Set.empty, mutable.Set.empty, mutable.Set.empty)
  }

  /**
    * Mutable per-function context that accumulates which formal parameter positions
    * have been observed as decreasing across all self-recursive call sites.
    *
    * After the expression visitor finishes, the accumulated indices are used to
    * annotate the corresponding formal parameters with `Decreasing.StrictlyDecreasing`.
    *
    * @param decreasingParams  maps each [[SelfSym]] to the set of parameter indices
    *                          that have been passed a strict substructure at some call site.
    * @param tailRecSym        the top-level `@Tailrec` def symbol, if any.
    * @param tailRecLocalSyms  local defs annotated with `@Tailrec`.
    */
  private case class LocalContext(decreasingParams: mutable.Map[SelfSym, mutable.Set[Int]], tailRecSym: Option[Symbol.DefnSym], tailRecLocalSyms: mutable.Set[Symbol.VarSym], selfCallObservedDef: mutable.Set[Symbol.DefnSym], selfCallObservedLocal: mutable.Set[Symbol.VarSym], var currentLocalDefSym: Option[Symbol.VarSym] = None) {

    /** Records that parameter at `idx` is decreasing for `selfSym`. */
    def addDecreasing(selfSym: SelfSym, idx: Int): Unit =
      decreasingParams.getOrElseUpdate(selfSym, mutable.Set.empty) += idx

    /** Returns the set of parameter indices observed as decreasing for `selfSym`. */
    def getDecreasing(selfSym: SelfSym): Set[Int] =
      decreasingParams.getOrElse(selfSym, Set.empty).toSet
  }

  ////////////////////////////////////////////////////////////////////////////
  // SharedContext
  ////////////////////////////////////////////////////////////////////////////

  private object SharedContext {
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  private case class SharedContext(errors: ConcurrentLinkedQueue[TerminationError])

}
