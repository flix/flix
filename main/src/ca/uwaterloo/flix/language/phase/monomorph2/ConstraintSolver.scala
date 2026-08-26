/*
 * Copyright 2026 Flix Authors
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{RegionScope, SymUse}
import ca.uwaterloo.flix.language.phase.monomorph2.MonomorphHelpers.lowerChannelType
import ca.uwaterloo.flix.language.phase.monomorph2.Symbols.Defs
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.ListOps

import scala.collection.mutable

/**
  * Solves the flow constraints produced by [[ConstraintGen]] to a fixpoint: starting from
  * the ground (all-constant) flows, each newly solved instantiation is substituted into the
  * flows that depend on it until no new instantiations appear.
  *
  * Sig destinations are additionally dispatched to their implementing (or default) def.
  *
  * The result is, per polymorphic symbol, the set of ground instantiations it must be
  * specialized at.
  */
private[monomorph2] object ConstraintSolver {

  /**
    * Solves `flows` to a fixpoint and returns the set of required specializations.
    *
    * Callers must run [[NonMonomorphizableCheck.checkMonomorphizable]] first to make
    * sure that the fixpoint loop will not grow without bound.
    */
  private[monomorph2] def solve(flows: List[FlowConstraint], root: TypedAst.Root)(implicit flix: Flix): Solution = {
    val instanceMap = MonomorphHelpers.mkInstanceMap(root.instances)
    val dependents  = buildDependents(flows)

    val solution  = mutable.Map.empty[MonoVar, mutable.ListBuffer[GroundInstantiation]]
    val worklist  = mutable.Queue.empty[(MonoVar, GroundInstantiation)]

    def enqueue(dst: MonoVar, inst0: GroundInstantiation): Unit = {
      val inst = dst match {
        case MonoVar.Def(sym) =>
          if (Defs.Concurrent.Channel.NeedsRelowering.contains(sym)) {
            GroundInstantiation(inst0.args.map(lowerChannelType))
          } else {
            inst0
          }

        case MonoVar.Enum(_)              => inst0

        case MonoVar.Sig(_)               => inst0

        case MonoVar.RestrictableEnum(_)  => inst0

        case MonoVar.Struct(_)            => inst0
      }
      // Only add genuinely new instantiations, i.e. ones not already in the solution nor in the worklist.
      if (!solution.get(dst).exists(_.contains(inst))) {
        if (!worklist.contains((dst, inst))) {
          worklist.enqueue((dst, inst))
        }
      }
    }

    // Seed: ground flows (all-Const instantiations) become initial worklist entries.
    for (fc <- flows) {
      for (t <- groundArgs(fc, Map.empty, root)) {
        enqueue(fc.dst, t)
      }
    }

    // Fixpoint loop.
    while (worklist.nonEmpty) {
      val (dst, inst) = worklist.dequeue()

      val seen = solution.getOrElseUpdate(dst, mutable.ListBuffer.empty)
      if (!seen.contains(inst)) {
        seen += inst

        // Sig dispatch: resolve to impl def and forward the instantiation.
        dst match {
          case MonoVar.Sig(sigSym) =>
            for (case (implSym, implArgs) <- resolveSig(sigSym, inst, root, instanceMap)) {
              enqueue(MonoVar.Def(implSym), implArgs)
            }

          case MonoVar.Def(_)              => ()

          case MonoVar.Enum(_)             => ()

          case MonoVar.RestrictableEnum(_) => ()

          case MonoVar.Struct(_)           => ()
        }

        // Propagate: substitute this MonoVar's new instantiation into all dependent flows.
        for (fc <- dependents.getOrElse(dst, Nil)) {
          for (groundInstantiation <- groundArgs(fc, Map(dst -> inst), root)) {
            enqueue(fc.dst, groundInstantiation)
          }
        }
      }
    }

    Solution(
      defs = solution.collect { case (MonoVar.Def(sym), insts) => sym -> insts.toList }.toMap,
      enums = solution.collect { case (MonoVar.Enum(sym), insts) => sym -> insts.toList }.toMap,
      structs = solution.collect { case (MonoVar.Struct(sym), insts) => sym -> insts.toList }.toMap,
      restrictableEnums = solution.collect { case (MonoVar.RestrictableEnum(sym), insts) => sym -> insts.toList }.toMap
    )
  }

  /** Returns every distinct MonoVar referenced by a `Param` in `fc`'s args. */
  private def paramVars(fc: FlowConstraint): Set[MonoVar] =
    fc.args.args.flatMap(MonoArg.collectParams).map(_._1).toSet

  /** Return for each MonoVar, the flows whose args contain `Param(mvar, _)`. */
  private def buildDependents(flows: List[FlowConstraint]): Map[MonoVar, List[FlowConstraint]] = {
    val edges = for {
      fc <- flows
      v  <- paramVars(fc)
    } yield v -> fc

    edges.groupMap(_._1)(_._2)
  }

  /**
    * Substitutes `bindings` into `arg`'s `Param`s.
    * Returns `None` if some `Param`'s var isn't bound (yet).
    */
  private def substArg(arg: MonoArg, bindings: Map[MonoVar, GroundInstantiation]): Option[Type] = arg match {
    case MonoArg.Const(t) => Some(t)

    case MonoArg.Param(v, i) =>
      for {
        inst <- bindings.get(v)
        tpe  <- Some(inst.args(i))
      } yield tpe

    case MonoArg.App(head, args) =>
      for {
        h  <- substArg(head, bindings)
        as <- ListOps.traverse(args)(substArg(_, bindings))
      } yield Type.mkApply(h, as, h.loc)

    case MonoArg.Assoc(sym, a, kind, loc) =>
      substArg(a, bindings).map {
        t =>
          Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), t, kind, loc)
      }
  }

  /** Grounds `fc`'s args to a ground instantiation, or `None` if any position is not ready. */
  private def groundArgs(fc: FlowConstraint, bindings: Map[MonoVar, GroundInstantiation], root: TypedAst.Root)
                         (implicit flix: Flix): Option[GroundInstantiation] =
    ListOps.traverse(fc.args.args)(groundArg(_, bindings, root)).map(GroundInstantiation(_))

  /**
    * Grounds `arg` via [[substArg]] and the shared [[Canonicalization]] pipeline.
    */
  private def groundArg(arg: MonoArg, bindings: Map[MonoVar, GroundInstantiation], root: TypedAst.Root)
                          (implicit flix: Flix): Option[Type] =
    substArg(arg, bindings).map {
      raw =>
        val result = MonomorphHelpers.groundType(raw)(root, flix)
        if (result.typeVars.nonEmpty) {
          throw InternalCompilerException(s"Defaulted arg did not fully ground: $result", result.loc)
        }
        result
    }

  /**
    * Resolves a sig call with `instantiation` to the impl def sym and its type args.
    * Returns `None` if the instance cannot be found.
    */
  private def resolveSig(
    sigSym: Symbol.SigSym,
    instantiation: GroundInstantiation,
    root: TypedAst.Root,
    instanceMap: Map[(Symbol.TraitSym, TypeConstructor), TypedAst.Instance]
  )(implicit flix: Flix): Option[(Symbol.DefnSym, GroundInstantiation)] = {
    val traitType = instantiation.args.head
    for {
      tyCon    <- traitType.typeConstructor
      instance <- instanceMap.get((sigSym.trt, tyCon))
      result   <- instance.defs.find(_.sym.text == sigSym.name) match {
        case Some(implDef) =>
          val implOwnArgs = dropEconstrArgs(sigSym, instantiation, root)
          Some((implDef.sym, GroundInstantiation(instanceArgsFor(instance, traitType, root) ++ implOwnArgs)))

        case None =>
          // No impl def: sig has a default impl. Synthesize a trait-level sym and forward the
          // instantiation as-is (the default belongs to the trait, not the instance).
          root.sigs(sigSym).exp.map {
            _ =>
              val ns = sigSym.trt.namespace :+ sigSym.trt.name
              (new Symbol.DefnSym(None, ns, sigSym.name, sigSym.loc), instantiation)
          }
      }
    } yield result
  }

  /** Drops `sigSym`'s own args that equality constraints introduced, leaving the impl def's. */
  private def dropEconstrArgs(sigSym: Symbol.SigSym, instantiation: GroundInstantiation, root: TypedAst.Root): List[Type] = {
    val sigSpec = root.sigs(sigSym).spec
    val econstrVars = sigSpec.econstrs.flatMap(ec => ec.tpe1.typeVars ++ ec.tpe2.typeVars).map(_.sym).toSet
    ListOps.zip(sigSpec.tparams.map(_.sym), instantiation.args.tail).collect {
      case (sym, arg) if !econstrVars.contains(sym) => arg
    }
  }

  /** Unifies `instance`'s type against `traitType`, returning its tparams' values in order. */
  private def instanceArgsFor(instance: TypedAst.Instance, traitType: Type, root: TypedAst.Root)
                              (implicit flix: Flix): List[Type] = {
    val subst = ConstraintSolver2.fullyUnify(instance.tpe, traitType, RegionScope.Top, RigidityEnv.empty)(root.eqEnv, flix).get
    instance.tparams.map(tp => subst.m(tp.sym))
  }
}
