/*
 * Copyright 2026 Simon Lykke Andersen
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
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, FormalParam, MatchRule, TypeParam}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
  * Constraint generation for constraint-based monomorphization: emits `FlowConstraint` constraints
  * describing how concrete types propagate through the program, for [[ConstraintSolver]] to solve.
  */
object ConstraintGen {

  private object SharedContext {
    /** Returns a fresh shared context. */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * The mutable data used throughout constraint generation.
    *
    * This class is thread-safe.
    */
  private case class SharedContext(flows: ConcurrentLinkedQueue[FlowConstraint]) {
    /** Emits `flow` as one of the generated constraints. */
    def addFlow(flow: FlowConstraint): Unit = flows.add(flow)

    /** Returns every flow emitted so far. */
    def result: List[FlowConstraint] = flows.asScala.toList
  }

  /**
    * Generates specialization constraints for every top-level declaration in `root0`.
    */
  def generate(root0: TypedAst.Root)(implicit flix: Flix): List[FlowConstraint] = {
    implicit val sctx: SharedContext = SharedContext.mk()
    implicit val root: TypedAst.Root = root0

    ParOps.parMap(root.defs.values) { defn =>
      flix.profile(defn.sym, defn.loc) {
        val mvar = MonoVar.Def(defn.sym)
        implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, defn.spec.tparams)
        visitDef(defn)
      }
    }

    ParOps.parMap(root.enums.values) { enm =>
      val mvar = MonoVar.Enum(enm.sym)
      implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, enm.tparams)
      visitEnum(enm)
    }

    ParOps.parMap(root.instances.values) { inst =>
      for (instDef <- inst.defs) {
        val mvar = MonoVar.Def(instDef.sym)
        val allTparams = inst.tparams ++ instDef.spec.tparams
        implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, allTparams)
        flix.profile(instDef.sym, instDef.loc) {
          visitDef(instDef)
        }
      }
    }

    ParOps.parMap(root.restrictableEnums.values) { enm =>
      val mvar = MonoVar.RestrictableEnum(enm.sym)
      implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, enm.index :: enm.tparams)
      visitRestrictableEnum(enm)
    }

    ParOps.parMap(root.structs.values) { struct =>
      val mvar = MonoVar.Struct(struct.sym)
      implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, struct.tparams)
      visitStruct(struct)
    }

    ParOps.parMap(root.sigs.values.filter(_.exp.isDefined)) { sig =>
      // The trait's type parameter (e.g. `t` in `Foldable[t]`) is prepended since it isn't in sig.spec.tparams
      // but it is free in the default impl body (which are the only ones we call `visitExp` on).
      val trt = root.traits(sig.sym.trt)
      val traitTparam = trt.tparam
      val allTparams = traitTparam :: sig.spec.tparams
      // We synthesize a DefnSym so their own tparams classify as Param, not  wrongly-ground Const. (As for fromEffects)
      val ns = sig.sym.trt.namespace :+ sig.sym.trt.name
      val defnSym = new Symbol.DefnSym(None, ns, sig.sym.name, sig.sym.loc)
      val mvar = MonoVar.Def(defnSym)
      implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, allTparams)
      flix.profile(defnSym, sig.sym.loc) {
        for (exp <- sig.exp) {
          visitExp(exp)
        }
      }
    }

    ParOps.parMap(root.effects.values.flatMap(_.ops)) { op =>
      // We need a Synthetic DefnSym to tie the tparams to
      val defnSym = new Symbol.DefnSym(None, op.sym.namespace, op.sym.name, op.sym.loc)
      val mvar = MonoVar.Def(defnSym)
      implicit val tparamEnv: TparamEnv = mkTparamEnv(mvar, op.spec.tparams)
      visitOp(op)
    }

    sctx.result
  }

  /** Maps the current declaration's own type parameters to their `MonoArg.Param` binding. */
  private case class TparamEnv(m: Map[Symbol.KindedTypeVarSym, MonoArg]) {
    def get(sym: Symbol.KindedTypeVarSym): Option[MonoArg] = m.get(sym)
  }

  /** Returns the `MonoArg.Param` bindings for `mvar`'s type parameters, in declared order. */
  private def mkTparamEnv(mvar: MonoVar, tparams: List[TypeParam]): TparamEnv =
    TparamEnv(tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap)

  /**
    * Emits flow constraints for enum type applications occurring in `tpe`.
    */
  private def visitType(tpe0: Type)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    def dealiasedVisitType(tpe: Type): Unit = tpe match {
      case at @ Type.AssocType(_, arg, _, _) =>
        if (at.typeVars.isEmpty)
          visitType(Canonicalization.reduceAssocType(at)(root, flix))
        else
          dealiasedVisitType(arg)
      case app @ Type.Apply(_, _, _)    =>
        val args = app.typeArguments
        for (arg <- args) {
          dealiasedVisitType(arg)
        }
        for (mvar <- declMonoVar(app.baseType)) {
          sctx.addFlow(FlowConstraint(Instantiation(args.map(t => dealiasedTypeToMonoArg(t))), mvar))
        }
      case Type.Var(_, _)               => ()
      case Type.Cst(_, _)               => ()
      case Type.JvmToEff(_, _)          => ()
      case Type.JvmToType(_, _)         => ()
      case Type.UnresolvedJvmType(_, _) => ()
      case Type.Alias(_, _, _, _)       =>
        throw InternalCompilerException(s"Unexpected type alias (should have been erased): $tpe", tpe.loc)
    }
    dealiasedVisitType(Type.eraseAliases(tpe0))
  }

  /** The MonoVar of `baseType`'s enum/restrictable-enum/struct constructor, or None if it's neither. */
  private def declMonoVar(baseType: Type): Option[MonoVar] = baseType match {
    case Type.Cst(TypeConstructor.Enum(sym, _), _)             => Some(MonoVar.Enum(sym))
    case Type.Cst(TypeConstructor.RestrictableEnum(sym, _), _) => Some(MonoVar.RestrictableEnum(sym))
    case Type.Cst(TypeConstructor.Struct(sym, _), _)           => Some(MonoVar.Struct(sym))
    case _                                                     => None
  }

  /**
    * Emits flow constraints for all case field types in `enumDecl`.
    */
  private def visitEnum(enumDecl: TypedAst.Enum)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (cas <- enumDecl.cases.values) {
      for (tpe <- cas.tpes) {
        visitType(tpe)
      }
    }
  }

  /**
    * Emits flow constraints for all case field types in `restrictableEnumDecl`.
    */
  private def visitRestrictableEnum(restrictableEnumDecl: TypedAst.RestrictableEnum)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (cas <- restrictableEnumDecl.cases.values) {
      for (tpe <- cas.tpes) {
        visitType(tpe)
      }
    }
  }

  /**
    * Emits flow constraints for all field types in `structDecl`.
    */
  private def visitStruct(structDecl: TypedAst.Struct)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (field <- structDecl.fields.values) {
      visitType(field.tpe)
    }
  }

  /**
    * Emits flow constraints for the formal parameter and return types of `op`.
    */
  private def visitOp(op: TypedAst.Op)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (case FormalParam(_, tpe, _, _, _) <- op.spec.fparams) {
      visitType(tpe)
    }
    visitType(op.spec.retTpe)
  }

  /**
    * Emits flow constraints for the formal parameter types, return type, and body of `defn`.
    */
 private def visitDef(defn: TypedAst.Def)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (case FormalParam(_, tpe, _, _, _) <- defn.spec.fparams) {
      visitType(tpe)
    }
    visitType(defn.spec.retTpe)
    visitExp(defn.exp)
    entryPointHandlerFlows(defn)
  }

  /**
    * Emits flow constraints for the default-handler calls that
    * [[SpecializeAndLower.wrapDefWithDefaultHandlers]] synthesizes around entry points.
    */
  private def entryPointHandlerFlows(defn: TypedAst.Def)(implicit tparamEnv: TparamEnv, sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit =
    if (TypedAstOps.isEntryPoint(defn)(root)) {
      val loc = defn.spec.eff.loc
      val defEffects = Canonicalization.evalEff(defn.spec.eff)
      val requiredHandlers = root.defaultHandlers.filter(h => defEffects.contains(h.handledSym))
      requiredHandlers.foldLeft(defn.spec.eff) { case (eff, handler) =>
        val handlerDef = root.defs(handler.handlerSym)
        val handlerTparams = handlerDef.spec.tparams
        // Handler signature is `pub def h(f: Unit -> a \ ef): a \ ...`, but `ef` may itself be e.g
        // a sum of several free effect tparams (e.g. `ef1 + ef2`), therefore we unify the handler's
        // declared parameter type against the concrete call site.
        val concreteParamTpe = Type.mkArrowWithEffect(Type.Unit, eff, defn.spec.retTpe, loc)
        val subst = ConstraintSolver2.fullyUnify(handlerDef.spec.fparams.head.tpe, concreteParamTpe, RegionScope.Top, RigidityEnv.empty)(root.eqEnv, flix)
          .getOrElse(throw InternalCompilerException(s"Could not unify default handler '${handler.handlerSym}' against its call site.", loc))
        val args = handlerTparams.map(tp => typeToMonoArg(subst(Type.Var(tp.sym, loc))))
        sctx.addFlow(FlowConstraint(Instantiation(args), MonoVar.Def(handler.handlerSym)))
        Canonicalization.canonicalEffect(Type.mkUnion(Type.mkDifference(eff, handler.handledEff, loc), Type.IO, loc))
      }
      ()
    }

  /**
    * Emits flow constraints for all call sites and enum/struct construction sites in `exp`.
    * Datalog and channel nodes additionally emit constraints for the stdlib calls
    * [[SpecializeAndLower]] will synthesize for them.
    */
  private def visitExp(exp0: Expr)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = exp0 match {
    case Expr.Cst(_, _, _)        => ()
    case Expr.Var(_, _, _)        => ()
    case Expr.Hole(_, _, _, _, _) => ()
    case Expr.ApplyDef(symUse, exps, targs, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlow(FlowConstraint(Instantiation(targs.map(typeToMonoArg)), MonoVar.Def(symUse.sym)))
    case Expr.ApplySig(symUse, exps, targ, targs, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlow(FlowConstraint(Instantiation((targ :: targs).map(typeToMonoArg)), MonoVar.Sig(symUse.sym)))
    case Expr.ApplyOp(_, exps, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.ApplyClo(exp1, exp2, _, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.Unary(_, exp, _, _, _) => visitExp(exp)
    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.Lambda(_, exp, _, _) => visitExp(exp)
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
    case Expr.Stm(exps, exp, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
      visitExp(exp)
    case Expr.Discard(exp, _, _) => visitExp(exp)
    case Expr.Region(_, _, exp, _, _, _) => visitExp(exp)
    case Expr.Use(_, _, exp, _) => visitExp(exp)
    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp)
      for (case MatchRule(pat, guardOpt, body, _) <- rules) {
        visitPat(pat)
        for (exp0 <- guardOpt) {
          visitExp(exp0)
        }
        visitExp(body)
      }
    case Expr.Tag(_, exps, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlow(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
    case Expr.RestrictableTag(_, exps, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlow(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp)
      for (rule <- rules) {
        visitExp(rule.exp)
      }
    case Expr.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp)
      for (rule <- rules) {
        visitExp(rule.exp)
      }
    case Expr.ExtTag(_, exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.OpenAs(_, exp, _, _) => visitExp(exp)
    case Expr.Tuple(exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.LocalDef(_, bnd, _, exp1, exp2, _, _, _) =>
      visitType(bnd.tpe)
      visitExp(exp1)
      visitExp(exp2)
    case Expr.ApplyLocalDef(_, exps, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.HoleWithExp(exp, _, _, _, _) => visitExp(exp)
    case Expr.RecordSelect(exp, _, _, _, _) => visitExp(exp)
    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.RecordRestrict(_, exp, _, _, _) => visitExp(exp)
    case Expr.ArrayLit(exps, exp, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
      visitExp(exp)
    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.ArrayLength(exp, _, _) => visitExp(exp)
    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
    case Expr.VectorLit(exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.VectorLength(exp, _) => visitExp(exp)
    case Expr.StructNew(_, fields, region, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      for (case (_, exp) <- fields) {
        visitExp(exp)
      }
      for (exp <- region) {
        visitExp(exp)
      }
      sctx.addFlow(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
    case Expr.StructGet(exp, _, _, _, _) => visitExp(exp)
    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.Lazy(exp, _, _) => visitExp(exp)
    case Expr.Force(exp, _, _, _) => visitExp(exp)
    case Expr.Ascribe(exp, _, _, _, _, _) => visitExp(exp)
    case Expr.InstanceOf(exp, _, _) => visitExp(exp)
    case Expr.CheckedCast(_, exp, _, _, _) => visitExp(exp)
    case Expr.UncheckedCast(exp, _, _, _, _, _) => visitExp(exp)
    case Expr.Unsafe(exp, _, _, _, _, _) => visitExp(exp)
    case Expr.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp)
      for (rule <- rules) {
        visitExp(rule.exp)
      }
    case Expr.Throw(exp, _, _, _) => visitExp(exp)
    case Expr.Handler(_, rules, _, _, _, _, _) =>
      for (rule <- rules) {
        visitExp(rule.exp)
      }
    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    // Lowering synthesizes Channel.get/put/newChannel calls for each non-last fragment.
    case Expr.ParYield(_, _, _, _, _) => ???

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.InvokeSuperConstructor(_, exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      visitExp(exp)
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.InvokeSuperMethod(_, exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    case Expr.GetField(_, exp, _, _, _) => visitExp(exp)
    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.GetStaticField(_, _, _, _) => ()
    case Expr.PutStaticField(_, exp, _, _, _) => visitExp(exp)
    case Expr.NewObject(_, _, _, _, constructors, methods, _) =>
      for (c <- constructors) {
        visitExp(c.exp)
      }
      for (m <- methods) {
        visitExp(m.exp)
      }

    // Lowering synthesizes Channel.get/put/newChannelTuple calls for GetChannel/PutChannel/
    // NewChannel respectively.
    case Expr.GetChannel(_, _, _, _) => ???

    case Expr.PutChannel(_, _, _, _, _) => ???

    case Expr.NewChannel(_, _, _, _) => ???

    // Lowering synthesizes Channel.mpmcAdmin/unsafeGetAndUnlock calls per rule (not Channel.get),
    // plus one fixed List[ChannelMpmcAdmin] built via mkTag/mkList.
    case Expr.SelectChannel(_, _, _, _, _) => ???

    // Lowering synthesizes every Box/Unbox/liftN/lattice/Facts/ProjectInto/ProvenanceOf call for
    // Datalog fixpoint nodes, mirroring the TypedAst structure lowering itself inspects.
    case Expr.FixpointConstraintSet(_, _, _) => ???

    // Lowering synthesizes a List[PredSym] directly via mkTag/mkList (bypassing the ordinary
    // rewrite path), so its instantiation must be predicted here.
    case Expr.FixpointLambda(_, _, _, _, _) => ???

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
    // Lowering synthesizes Fixpoint3.Solver.injectIntoN(p, ts), generic over the container
    // constructor and the tuple's component types.
    case Expr.FixpointInjectInto(_, _, _, _, _) => ???

    // Lowering synthesizes Fixpoint3.Solver.factsN(p, d), generic over the N selected terms'
    // types.
    case Expr.FixpointQueryWithSelect(_, _, _, _, _, _, _, _, _) => ???

    // Lowering synthesizes a box call per goal term, an unbox call per term type the
    // extensible-variant result can carry, and Solver.provenanceOf/Vector.get calls at a fixed
    // Boxed type.
    case Expr.FixpointQueryWithProvenance(_, _, _, _, _, _) => ???

    case Expr.Error(_, _, _) => ()
  }

  /**
    * Emits flow constraints for enum instantiations mentioned by patterns.
    *
    * Caution: a pattern can mention an instantiation nothing ever constructs, e.g.
    * `match None { case Some(_) => ... }` — its tparams default to `AnyType`. We still emit
    * a flow and specialize it as dead code so lookups stay strict; never let a defaulted
    * `AnyType` reach `Fixpoint.Boxable`'s box/unbox, whose unchecked casts would turn it into a
    * silently wrong runtime value instead.
    */
  private def visitPat(pat0: TypedAst.Pattern)(implicit tparamEnv: TparamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = pat0 match {
    case TypedAst.Pattern.Tag(_, pats, tpe, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      for (pat <- pats) {
        visitPat(pat)
      }
      sctx.addFlow(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
    case TypedAst.Pattern.Tuple(elms, _, _) =>
      for (elm <- elms) {
        visitPat(elm)
      }
    case TypedAst.Pattern.Record(pats, pat, _, _) =>
      for (lp <- pats) {
        visitPat(lp.pat)
      }
      visitPat(pat)
    case TypedAst.Pattern.Wild(_, _)   => ()
    case TypedAst.Pattern.Var(_, _, _) => ()
    case TypedAst.Pattern.Cst(_, _, _) => ()
    case TypedAst.Pattern.Error(_, _)  => ()
  }

  /** Converts `tpe0` to a `MonoArg` relative to the current declaration context. */
  private def typeToMonoArg(tpe0: Type)(implicit tparamEnv: TparamEnv, root: TypedAst.Root, flix: Flix): MonoArg =
    dealiasedTypeToMonoArg(Type.eraseAliases(tpe0))

  /** Like [[typeToMonoArg]], but `tpe` must already have its aliases erased (deeply). */
  private def dealiasedTypeToMonoArg(tpe: Type)(implicit tparamEnv: TparamEnv, root: TypedAst.Root, flix: Flix): MonoArg =
    tpe match {
      case Type.Var(sym, _) =>
        // A type variable that is not a tparam of the current decl (absent from tparamEnv) — e.g.
        // a region var introduced by `region r { ... }`. We record it as an opaque constant so the
        // flow is still emitted but the solver does not propagate it.
        tparamEnv.get(sym).getOrElse(MonoArg.Const(tpe))
      case at @ Type.AssocType(symUse, arg, kind, assocLoc) =>
        if (tpe.typeVars.isEmpty)
          MonoArg.Const(Canonicalization.reduceAssocType(at)(root, flix))
        else
          MonoArg.Assoc(symUse.sym, dealiasedTypeToMonoArg(arg), kind, assocLoc)
      case Type.Cst(_, _) =>
        MonoArg.Const(tpe)
      case Type.Apply(_, _, _) =>
        if (tpe.kind == Kind.Eff && tpe.typeVars.isEmpty)
          MonoArg.Const(Canonicalization.simplify(tpe, isGround = true)(root, flix))
        else {
          MonoArg.App(dealiasedTypeToMonoArg(tpe.baseType), tpe.typeArguments.map(arg => dealiasedTypeToMonoArg(arg)))
        }
      case Type.Alias(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected type alias (should have been erased): $tpe", tpe.loc)
      case Type.JvmToType(_, loc) =>
        throw InternalCompilerException("Unexpected JVM type", loc)
      case Type.JvmToEff(_, loc) =>
        throw InternalCompilerException("Unexpected JVM eff", loc)
      case Type.UnresolvedJvmType(_, loc) =>
        throw InternalCompilerException("Unexpected JVM type", loc)
    }

  /** Returns the enum/restrictable-enum/struct `MonoVar` and type arguments of `tpe0`. */
  private def getMonoVarAndTypeArgs(tpe0: Type): (MonoVar, List[Type]) = {
    val tpe = Type.eraseAliases(tpe0)
    val mvar = declMonoVar(tpe.baseType).getOrElse(
      throw InternalCompilerException(s"Expected an Enum, RestrictableEnum, or Struct type, but got $tpe", tpe0.loc))
    (mvar, tpe.typeArguments)
  }
}
