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
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, FormalParam, MatchRule, Predicate}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.{Denotation, PredicateAndArity, RegionScope}
import ca.uwaterloo.flix.language.phase.monomorph2.Symbols.{Defs, Enums, Types}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable

/**
  * Constraint generation for constraint-based monomorphization: emits `Flow` constraints
  * describing how concrete types propagate through the program, for [[ConstraintSolver]] to solve.
  */
object ConstraintCollection {

  /**
    * The mutable data used throughout constraint generation.
    *
    * This class is thread-safe.
    */
  private class SharedContext {
    private val flows: mutable.ArrayBuffer[Flow] = mutable.ArrayBuffer.empty

    /** Emits `flow` as one of the generated constraints. */
    def addFlow(flow: Flow): Unit = synchronized { flows.addOne(flow) }

    /** Returns every flow emitted so far. */
    def result: Set[Flow] = synchronized { flows.toSet }
  }

  /**
    * Generates specialization constraints for every top-level declaration in `root0`.
    */
  def generate(root0: TypedAst.Root)(implicit flix: Flix): Set[Flow] = {
    implicit val sctx: SharedContext = new SharedContext()
    implicit val root: TypedAst.Root = root0

    ParOps.parMap(root.defs.values) { defn =>
      flix.profile(defn.sym, defn.loc) {
        val mvar = MonoVar.Def(defn.sym)
        implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = defn.spec.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
        visitDef(defn)
      }
    }

    ParOps.parMap(root.enums.values) { enm =>
      val mvar = MonoVar.Enum(enm.sym)
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = enm.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      visitEnum(enm)
    }

    ParOps.parMap(root.instances.values) { inst =>
      inst.defs.foreach { instDef =>
        val mvar = MonoVar.Def(instDef.sym)
        val allTparams = inst.tparams ++ instDef.spec.tparams
        implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = allTparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
        flix.profile(instDef.sym, instDef.loc) {
          visitDef(instDef)
        }
      }
    }

    ParOps.parMap(root.restrictableEnums.values) { enm =>
      val mvar = MonoVar.RestrictableEnum(enm.sym)
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = (enm.index :: enm.tparams).zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      visitRestrictableEnum(enm)
    }

    ParOps.parMap(root.structs.values) { struct =>
      val mvar = MonoVar.Struct(struct.sym)
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = struct.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
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
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = allTparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      flix.profile(defnSym, sig.sym.loc) {
        sig.exp.foreach(visitExp)
      }
    }

    ParOps.parMap(root.effects.values.flatMap(_.ops)) { op =>
      // We need a Synthetic DefnSym to tie the tparams to
      val defnSym = new Symbol.DefnSym(None, op.sym.namespace, op.sym.name, op.sym.loc)
      val mvar = MonoVar.Def(defnSym)
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = op.spec.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      visitOp(op)
    }

    sctx.result
  }

  /**
    * Emits flow constraints for enum type applications occurring in `tpe`.
    */
  private def visitType(tpe0: Type)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = {
    def dealiasedVisitType(tpe: Type): Unit = tpe match {
      case at @ Type.AssocType(_, arg, _, _) =>
        if (at.typeVars.isEmpty)
          visitType(MonomorphCanon.reduceAssocType(at)(root, flix))
        else
          dealiasedVisitType(arg)
      case Type.Var(_, _)
           | Type.Cst(_, _)
           | Type.JvmToEff(_, _)
           | Type.JvmToType(_, _)
           | Type.UnresolvedJvmType(_, _)=> ()
      case Type.Alias(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected type alias (should have been erased): $tpe", tpe.loc)
      case app @ Type.Apply(_, _, _) =>
        val args = app.typeArguments
        args.foreach(dealiasedVisitType)
        declMonoVar(app.baseType).foreach(mvar => sctx.addFlow(Flow(args.map(t => dealiasedTypeToMonoArg(t)), mvar)))
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
  private def visitEnum(enumDecl: TypedAst.Enum)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    enumDecl.cases.values.foreach(cas => cas.tpes.foreach(visitType))

  /**
    * Emits flow constraints for all case field types in `restrictableEnumDecl`.
    */
  private def visitRestrictableEnum(restrictableEnumDecl: TypedAst.RestrictableEnum)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    restrictableEnumDecl.cases.values.foreach(cas => cas.tpes.foreach(visitType))

  /**
    * Emits flow constraints for all field types in `structDecl`.
    */
  private def visitStruct(structDecl: TypedAst.Struct)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    structDecl.fields.values.foreach(field => visitType(field.tpe))

  /**
    * Emits flow constraints for the formal parameter and return types of `op`.
    */
  private def visitOp(op: TypedAst.Op)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = {
    op.spec.fparams.foreach { case FormalParam(_, tpe, _, _, _) => visitType(tpe) }
    visitType(op.spec.retTpe)
  }

  /**
    * Emits flow constraints for the formal parameter types, return type, and body of `defn`.
    */
  private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = {
    defn.spec.fparams.foreach { case FormalParam(_, tpe, _, _, _) => visitType(tpe) }
    visitType(defn.spec.retTpe)
    visitExp(defn.exp)
    entryPointHandlerFlows(defn)
  }

  /**
    * Emits flow constraints for the default-handler calls that
    * `SolutionLowering.wrapDefWithDefaultHandlers` synthesizes around entry points.
    */
  private def entryPointHandlerFlows(defn: TypedAst.Def)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    if (TypedAstOps.isEntryPoint(defn)(root)) {
      val loc = defn.spec.eff.loc
      val defEffects = MonomorphCanon.evalEffect(defn.spec.eff)
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
        sctx.addFlow(Flow(args, MonoVar.Def(handler.handlerSym)))
        MonomorphCanon.canonicalEffect(Type.mkUnion(Type.mkDifference(eff, handler.handledEff, loc), Type.IO, loc))
      }
      ()
    }

  /**
    * Emits flow constraints for all call sites and enum/struct construction sites in `exp`.
    * Datalog and channel nodes additionally emit constraints for the stdlib calls
    * [[SolutionLowering]] will synthesize for them.
    */
  private def visitExp(exp0: Expr)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = exp0 match {
    case Expr.Cst(_, _, _) => ()
    case Expr.Var(_, _, _) => ()
    case Expr.Hole(_, _, _, _, _) => ()

    case Expr.ApplyDef(symUse, exps, targs, _, _, _, _, _) =>
      exps.foreach(visitExp)
      sctx.addFlow(Flow(targs.map(typeToMonoArg), MonoVar.Def(symUse.sym)))

    case Expr.ApplySig(symUse, exps, targ, targs, _, _, _, _, _) =>
      exps.foreach(visitExp)
      sctx.addFlow(Flow((targ :: targs).map(typeToMonoArg), MonoVar.Sig(symUse.sym)))

    case Expr.ApplyOp(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

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
      exps.foreach(visitExp)
      visitExp(exp)

    case Expr.Discard(exp, _, _) => visitExp(exp)

    case Expr.Region(_, _, exp, _, _, _) => visitExp(exp)

    case Expr.Use(_, _, exp, _) => visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach {
        case MatchRule(pat, guardOpt, body, _) =>
          visitPat(pat)
          guardOpt.foreach(visitExp)
          visitExp(body)
      }

    case Expr.Tag(_, exps, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      exps.foreach(visitExp)
      sctx.addFlow(Flow(tpArgs.map(typeToMonoArg), mvar))

    case Expr.RestrictableTag(_, exps, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      exps.foreach(visitExp)
      sctx.addFlow(Flow(tpArgs.map(typeToMonoArg), mvar))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach(r => visitExp(r.exp))

    case Expr.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach(r => visitExp(r.exp))

    case Expr.ExtTag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.OpenAs(_, exp, _, _) => visitExp(exp)

    case Expr.Tuple(exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.LocalDef(_, bnd, _, exp1, exp2, _, _, _) =>
      visitType(bnd.tpe)
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.HoleWithExp(exp, _, _, _, _) => visitExp(exp)

    case Expr.RecordSelect(exp, _, _, _, _) => visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) => visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      exps.foreach(visitExp)
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
      exps.foreach(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLength(exp, _) => visitExp(exp)

    case Expr.StructNew(_, fields, region, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      fields.foreach { case (_, e) => visitExp(e) }
      region.foreach(visitExp)
      sctx.addFlow(Flow(tpArgs.map(typeToMonoArg), mvar))

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
      rules.foreach(r => visitExp(r.exp))

    case Expr.Throw(exp, _, _, _) => visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      rules.foreach(r => visitExp(r.exp))

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    // Lowering synthesizes Channel.get/put/newChannel calls for each non-last fragment.
    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.foreach { f =>
        visitPat(f.pat)
        visitExp(f.exp)
      }
      visitExp(exp)
      frags.init.foreach { frag =>
        val elmType = frag.exp.tpe
        val elmArg = typeToMonoArg(MonomorphHelpers.lowerChannelType(elmType))
        sctx.addFlow(Flow(List(elmArg), MonoVar.Def(Defs.ChannelNew)))
        sctx.addFlow(Flow(List(elmArg), MonoVar.Def(Defs.ChannelPut)))
        sctx.addFlow(Flow(List(elmArg), MonoVar.Def(Defs.ChannelGet)))
      }

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.InvokeSuperConstructor(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      visitExp(exp)
      exps.foreach(visitExp)

    case Expr.InvokeSuperMethod(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.GetField(_, exp, _, _, _) => visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) => ()

    case Expr.PutStaticField(_, exp, _, _, _) => visitExp(exp)

    case Expr.NewObject(_, _, _, _, constructors, methods, _) =>
      constructors.foreach(c => visitExp(c.exp))
      methods.foreach(m => visitExp(m.exp))

    // Lowering synthesizes a Channel.get call, generic over the channel's element type.
    case Expr.GetChannel(exp, tpe, _, _) =>
      visitExp(exp)
      sctx.addFlow(Flow(List(typeToMonoArg(MonomorphHelpers.lowerChannelType(tpe))), MonoVar.Def(Defs.ChannelGet)))

    // Lowering synthesizes a Channel.put call, generic over the element type.
    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      sctx.addFlow(Flow(List(typeToMonoArg(MonomorphHelpers.lowerChannelType(exp2.tpe))), MonoVar.Def(Defs.ChannelPut)))

    // Lowering synthesizes a Channel.newChannelTuple call, generic over the element type.
    case Expr.NewChannel(exp, tpe, _, _) =>
      val elmType = extractChannelElm(tpe)
      visitExp(exp)
      sctx.addFlow(Flow(List(typeToMonoArg(MonomorphHelpers.lowerChannelType(elmType))), MonoVar.Def(Defs.ChannelNewTuple)))

    // Lowering synthesizes Channel.mpmcAdmin/unsafeGetAndUnlock calls per rule (not Channel.get),
    // plus one fixed List[ChannelMpmcAdmin] built via mkTag/mkList.
    case Expr.SelectChannel(rules, default, _, _, _) =>
      rules.foreach { r =>
        val elmType = r.chan.tpe match {
          // Only possible shape since ConstraintGen.visitSelectRule unifies every rule's channel with Receiver[_]
          case Type.Apply(Type.Cst(TypeConstructor.Receiver, _), e, _) => e
          case t => throw InternalCompilerException(s"Expected Receiver[_], but got $t", r.chan.loc)
        }
        val elmArg = typeToMonoArg(MonomorphHelpers.lowerChannelType(elmType))
        visitExp(r.chan)
        visitExp(r.exp)
        sctx.addFlow(Flow(List(elmArg), MonoVar.Def(Defs.ChannelUnsafeGetAndUnlock)))
        sctx.addFlow(Flow(List(elmArg), MonoVar.Def(Defs.ChannelMpmcAdmin)))
      }
      default.foreach(visitExp)
      sctx.addFlow(Flow(List(typeToMonoArg(Types.ChannelMpmcAdmin)), MonoVar.Enum(Enums.FList)))

    // Lowering synthesizes every Box/Unbox/liftN/lattice/Facts/ProjectInto/ProvenanceOf call for
    // Datalog fixpoint nodes, mirroring the TypedAst structure lowering itself inspects.
    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foreach { c =>
        val cparams0 = c.cparams
        c.head match {
          case Predicate.Head.Atom(_, den, terms, _, loc) =>
            terms.foreach(visitExp)
            terms.foreach(t => headTermFlows(cparams0, t))
            latticeFlows(den, terms.lastOption.map(_.tpe), loc)
        }
        c.body.foreach {
          case Predicate.Body.Guard(e, _) =>
            visitExp(e)
            guardLiftFlow(cparams0, e)
          case Predicate.Body.Functional(outBnds, e, _) =>
            functionalLiftFlow(cparams0, outBnds.length, e)
            visitExp(e)
          case Predicate.Body.Atom(_, den, _, _, terms, _, loc) =>
            bodyAtomTermFlows(cparams0, terms)
            latticeFlows(den, terms.lastOption.map(_.tpe), loc)
        }
      }

    // Lowering synthesizes a List[PredSym] directly via mkTag/mkList (bypassing the ordinary
    // rewrite path), so its instantiation must be predicted here.
    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)
      sctx.addFlow(Flow(List(typeToMonoArg(Types.PredSym)), MonoVar.Enum(Enums.FList)))

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      exps.foreach(visitExp)

    // Lowering synthesizes Fixpoint3.Solver.injectIntoN(p, ts), generic over the container
    // constructor and the tuple's component types.
    case Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
      exps.zip(predsAndArities).foreach { case (e, PredicateAndArity(_, arity)) =>
        Type.eraseAliases(e.tpe) match {
          case Type.Apply(tc, innerTpe, _) =>
            val argTypes = unmkTuplish(arity, innerTpe)
            val flowArgs = (tc :: argTypes).map(typeToMonoArg)
            visitExp(e)
            sctx.addFlow(Flow(flowArgs, MonoVar.Def(Defs.ProjectInto(arity))))
          case t => throw InternalCompilerException(s"Unexpected non-foldable type: '$t'.", loc)
        }
      }

    // Lowering synthesizes Fixpoint3.Solver.factsN(p, d), generic over the N selected terms'
    // types. Facts(arity)'s flow args must come from the resolved result type `tpe0`, NOT from
    // `selects`' own term types, which may still carry locally-scoped type vars.
    case Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, _, tpe0, _, _) =>
      val arity = selects.length
      val innerTpe = unwrapVectorType(tpe0)
      val argTypes = unmkTuplish(arity, innerTpe)
      exps.foreach(visitExp)
      visitExp(queryExp)
      selects.foreach(visitExp)
      from.foreach {
        case Predicate.Body.Guard(e, _)         => visitExp(e)
        case Predicate.Body.Functional(_, e, _) => visitExp(e)
        case _: Predicate.Body.Atom             => ()
      }
      where.foreach(visitExp)
      sctx.addFlow(Flow(argTypes.map(typeToMonoArg), MonoVar.Def(Defs.Facts(arity))))

    // Lowering synthesizes a box call per goal term, an unbox call per term type the
    // extensible-variant result can carry, and Solver.provenanceOf/Vector.get calls at a fixed
    // Boxed type.
    case Expr.FixpointQueryWithProvenance(exps, select, _, tpe0, _, _) =>
      exps.foreach(visitExp)
      select match {
        case Predicate.Head.Atom(_, _, terms, _, _) =>
          terms.foreach(visitExp)
          terms.foreach(t => boxFlow(t.tpe))
      }
      val extVarType = unwrapVectorType(tpe0)
      predicatesOfExtVar(extVarType).flatMap(_._2).foreach { t =>
        sctx.addFlow(Flow(List(typeToMonoArg(t)), MonoVar.Def(Defs.Unbox)))
      }
      sctx.addFlow(Flow(List(typeToMonoArg(Types.Boxed)), MonoVar.Def(Defs.VectorGet)))
      sctx.addFlow(Flow(List(typeToMonoArg(extVarType)), MonoVar.Def(Defs.ProvenanceOf)))

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
  private def visitPat(pat0: TypedAst.Pattern)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = pat0 match {
    case TypedAst.Pattern.Tag(_, pats, tpe, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      pats.foreach(visitPat)
      sctx.addFlow(Flow(tpArgs.map(typeToMonoArg), mvar))
    case TypedAst.Pattern.Tuple(elms, _, _) =>
      elms.foreach(visitPat)
    case TypedAst.Pattern.Record(pats, pat, _, _) =>
      pats.foreach(lp => visitPat(lp.pat))
      visitPat(pat)
    case TypedAst.Pattern.Wild(_, _) | TypedAst.Pattern.Var(_, _, _) | TypedAst.Pattern.Cst(_, _, _) | TypedAst.Pattern.Error(_, _) =>
      ()
  }

  /** A flow for `Fixpoint3.Boxable.box` at type `tpe` — mirrors `SolutionLowering.box`. */
  private def boxFlow(tpe: Type)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    sctx.addFlow(Flow(List(typeToMonoArg(tpe)), MonoVar.Def(Defs.Box)))

  /** Flows for a head term — mirrors `SolutionLowering.lowerHeadTerm`. */
  private def headTermFlows(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = exp0 match {
    case Expr.Var(sym, tpe, _) =>
      if (!MonomorphHelpers.isQuantifiedVar(sym, cparams0)) boxFlow(tpe)
    case _ =>
      val fvs = MonomorphHelpers.quantifiedVars(cparams0, exp0)
      if (fvs.isEmpty) boxFlow(exp0.tpe)
      else sctx.addFlow(Flow((fvs.map(_._2) :+ exp0.tpe).map(typeToMonoArg), MonoVar.Def(Defs.Lift(fvs.length))))
  }

  /** Flows for a body atom's terms — mirrors `SolutionLowering.lowerBodyTerm`. */
  private def bodyAtomTermFlows(cparams0: List[TypedAst.ConstraintParam], terms: List[TypedAst.Pattern])(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    terms.foreach {
      case TypedAst.Pattern.Wild(_, _) => ()
      case TypedAst.Pattern.Var(bnd, tpe, _) =>
        if (!MonomorphHelpers.isQuantifiedVar(bnd.sym, cparams0)) boxFlow(tpe)
      case TypedAst.Pattern.Cst(_, tpe, _) => boxFlow(tpe)
      case _ => ()
    }

  /**
    * A flow for `lift{arity}b` — mirrors `SolutionLowering.mkGuard`. Arity 0 emits nothing:
    * there is no `lift0b`.
    */
  private def guardLiftFlow(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = {
    val fvs = MonomorphHelpers.quantifiedVars(cparams0, exp0)
    if (fvs.nonEmpty) sctx.addFlow(Flow(fvs.map(kv => typeToMonoArg(kv._2)), MonoVar.Def(Defs.LiftB(fvs.length))))
  }

  /**
    * A flow for `lift{inArity}X{outArity}` — mirrors `SolutionLowering.mkFunctional`.
    */
  private def functionalLiftFlow(cparams0: List[TypedAst.ConstraintParam], outArity: Int, exp0: TypedAst.Expr)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = {
    val inVars = MonomorphHelpers.quantifiedVars(cparams0, exp0)
    val inner = Type.eraseAliases(exp0.tpe) match {
      case Type.Apply(Type.Cst(TypeConstructor.Vector, _), t, _) => t
      case t => throw InternalCompilerException(s"Expected Vector[_], but got $t", exp0.loc)
    }
    val outTypes = unmkTuplish(outArity, inner)
    sctx.addFlow(Flow((inVars.map(_._2) ++ outTypes).map(typeToMonoArg), MonoVar.Def(Defs.LiftXM(inVars.length, outArity))))
  }

  /** Flows for `lattice`/`box`/`Denotation` — mirrors `SolutionLowering.mkDenotation`. */
  private def latticeFlows(den: Denotation, lastTermType: Option[Type], loc: SourceLocation)(implicit sctx: SharedContext, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = den match {
    case Denotation.Relational =>
      sctx.addFlow(Flow(List(typeToMonoArg(Types.Boxed)), MonoVar.Enum(Enums.Denotation)))
    case Denotation.Latticenal =>
      val tpe = lastTermType.getOrElse(throw InternalCompilerException("Unexpected nullary lattice predicate.", loc))
      sctx.addFlow(Flow(List(typeToMonoArg(tpe)), MonoVar.Def(Defs.Lattice)))
      sctx.addFlow(Flow(List(typeToMonoArg(tpe)), MonoVar.Def(Defs.LatticeBox)))
  }

  /** Returns `t` from `Vector[t]` — mirrors `SolutionLowering.unwrapVectorType`. */
  private def unwrapVectorType(tpe0: Type): Type = Type.eraseAliases(tpe0) match {
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), extType, _) => extType
    case t => throw InternalCompilerException(s"Expected Type.Apply(Type.Cst(TypeConstructor.Vector, _), _, _), but got $t", tpe0.loc)
  }

  /** Mirrors `SolutionLowering.predicatesOfExtVar`. */
  private def predicatesOfExtVar(tpe0: Type): List[(Name.Pred, List[Type])] = Type.eraseAliases(tpe0) match {
    case Type.Apply(Type.Cst(TypeConstructor.Extensible, _), tpe1, _) => predicatesOfSchemaRow(tpe1)
    case t => throw InternalCompilerException(s"Expected Type.Apply(Type.Cst(TypeConstructor.Extensible, _), _, _), but got $t", tpe0.loc)
  }

  /** Mirrors `SolutionLowering.predicatesOfSchemaRow`. */
  private def predicatesOfSchemaRow(row: Type): List[(Name.Pred, List[Type])] = row match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), rel, _), tpe2, _) =>
      (pred, termTypesOfRelation(rel)) :: predicatesOfSchemaRow(tpe2)
    case Type.Var(_, _) | Type.SchemaRowEmpty => Nil
    case t => throw InternalCompilerException(s"Got unexpected $t", t.loc)
  }

  /**
    * Mirrors `SolutionLowering.termTypesOfRelation`. Unlike it, a stray `Type.Var` (pre-solve)
    * is skipped, not defaulted — unsound for Box/Unbox/liftN.
    */
  private def termTypesOfRelation(rel: Type): List[Type] = {
    def flattenApply(rel0: Type): List[Type] = rel0 match {
      case Type.Cst(TypeConstructor.Relation(_), _) => Nil
      case Type.Apply(rest, t, _) => t :: flattenApply(rest)
      case _ if rel0.typeConstructor.contains(TypeConstructor.AnyType) => Nil
      case Type.Var(_, _) => Nil
      case t => throw InternalCompilerException(s"Expected Type.Apply(_, _, _), but got $t", rel0.loc)
    }
    flattenApply(rel).reverse
  }

  /** Extracts T from NewChannel's `(Sender[T], Receiver[T])` type — see ConstraintGen's NewChannel rule. */
  private def extractChannelElm(tpe: Type): Type = tpe.typeArguments match {
    case List(Type.Apply(Type.Cst(TypeConstructor.Sender | TypeConstructor.Receiver, _), elm, _), _) => elm
    case _ => throw InternalCompilerException(s"Expected (Sender[_], Receiver[_]), but got $tpe", tpe.loc)
  }

  /**
    * Inverse of `Type.mkTuplish`. `arity` can't be derived from `tpe` alone — mkTuplish leaves
    * an arity-1 result bare, so a single value can itself be tuple-typed.
    */
  private def unmkTuplish(arity: Int, tpe: Type): List[Type] =
    if (arity <= 1) List(tpe) else tpe.typeArguments

  /** Converts `tpe0` to a `MonoArg` relative to the current declaration context. */
  private def typeToMonoArg(tpe0: Type)(implicit tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): MonoArg =
    dealiasedTypeToMonoArg(Type.eraseAliases(tpe0))

  /** Like [[typeToMonoArg]], but `tpe` must already have its aliases erased (deeply). */
  private def dealiasedTypeToMonoArg(tpe: Type)(implicit tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): MonoArg =
    tpe match {
      case Type.Var(sym, _) =>
        // A type variable that is not a tparam of the current decl (absent from tparamEnv) — e.g.
        // a region var introduced by `region r { ... }`. We record it as an opaque constant so the
        // flow is still emitted but the solver does not propagate it.
        tparamEnv.getOrElse(sym, MonoArg.Const(tpe))
      case at @ Type.AssocType(symUse, arg, kind, assocLoc) =>
        if (tpe.typeVars.isEmpty)
          MonoArg.Const(MonomorphCanon.reduceAssocType(at)(root, flix))
        else
          MonoArg.Assoc(symUse.sym, dealiasedTypeToMonoArg(arg), kind, assocLoc)
      case Type.Cst(_, _) =>
        MonoArg.Const(tpe)
      case Type.Apply(_, _, _) =>
        if (tpe.kind == Kind.Eff && tpe.typeVars.isEmpty)
          MonoArg.Const(MonomorphCanon.simplify(tpe, isGround = true)(root, flix))
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
