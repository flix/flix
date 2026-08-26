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
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, FormalParam, MatchRule, Predicate, TypeParam}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.{Denotation, PredicateAndArity, RegionScope}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.language.phase.monomorph2.Symbols.{Defs, Enums, Types}
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
  private case class SharedContext(flowConstraints: ConcurrentLinkedQueue[FlowConstraint]) {
    /** Emits `flow` as one of the generated constraints. */
    def addFlowConstraint(flow: FlowConstraint): Unit = flowConstraints.add(flow)

    /** Returns every flow constraint emitted so far. */
    def result: List[FlowConstraint] = flowConstraints.asScala.toList
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
        implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, defn.spec.tparams)
        visitDef(defn)
      }
    }

    ParOps.parMap(root.enums.values) { enm =>
      val mvar = MonoVar.Enum(enm.sym)
      implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, enm.tparams)
      visitEnum(enm)
    }

    ParOps.parMap(root.instances.values) { inst =>
      for (instDef <- inst.defs) {
        val mvar = MonoVar.Def(instDef.sym)
        val allTparams = inst.tparams ++ instDef.spec.tparams
        implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, allTparams)
        flix.profile(instDef.sym, instDef.loc) {
          visitDef(instDef)
        }
      }
    }

    ParOps.parMap(root.restrictableEnums.values) { enm =>
      val mvar = MonoVar.RestrictableEnum(enm.sym)
      implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, enm.index :: enm.tparams)
      visitRestrictableEnum(enm)
    }

    ParOps.parMap(root.structs.values) { struct =>
      val mvar = MonoVar.Struct(struct.sym)
      implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, struct.tparams)
      visitStruct(struct)
    }

    ParOps.parMap(root.sigs.values.filter(_.exp.isDefined)) { sig =>
      // The trait's type parameter (e.g. `t` in `Foldable[t]`) is prepended since it isn't in sig.spec.tparams
      // but it is free in the default impl body (which are the only ones we call `visitExp` on).
      val trt = root.traits(sig.sym.trt)
      val traitTparam = trt.tparam
      val allTparams = traitTparam :: sig.spec.tparams
      // We synthesize a DefnSym so their own tparams classify as Param, not  wrongly-ground Const. (As for fromEffects)
      val defnSym = MonomorphHelpers.defaultSigImplSym(sig)
      val mvar = MonoVar.Def(defnSym)
      implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, allTparams)
      flix.profile(defnSym, sig.sym.loc) {
        for (exp <- sig.exp) {
          visitExp(exp)
        }
      }
    }

    ParOps.parMap(root.effects.values.flatMap(_.ops)) { op =>
      val defnSym = new Symbol.DefnSym(None, op.sym.namespace, op.sym.name, op.sym.loc)
      val mvar = MonoVar.Def(defnSym)
      implicit val tparamEnv: TypeParamEnv = mkTypeParamEnv(mvar, op.spec.tparams)
      visitOp(op)
    }

    sctx.result
  }

  /** Maps the current declaration's own type parameters to their `MonoArg.Param` binding. */
  private case class TypeParamEnv(m: Map[Symbol.KindedTypeVarSym, MonoArg]) {
    def get(sym: Symbol.KindedTypeVarSym): Option[MonoArg] = m.get(sym)
  }

  /** Returns the `MonoArg.Param` bindings for `mvar`'s type parameters, in declared order. */
  private def mkTypeParamEnv(mvar: MonoVar, tparams: List[TypeParam]): TypeParamEnv =
    TypeParamEnv(tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap)

  /**
    * Emits flow constraints for enum type applications occurring in `tpe`.
    */
  private def visitType(tpe0: Type)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    def dealiasedVisitType(tpe: Type): Unit = tpe match {
      case at @ Type.AssocType(_, arg, _, _) =>
        if (at.typeVars.isEmpty) {
          visitType(Canonicalization.reduceAssocType(at)(root, flix))
        } else {
          dealiasedVisitType(arg)
        }
      case app @ Type.Apply(_, _, _)    =>
        val args = app.typeArguments
        for (arg <- args) {
          dealiasedVisitType(arg)
        }
        for (mvar <- declMonoVar(app.baseType)) {
          sctx.addFlowConstraint(FlowConstraint(Instantiation(args.map(t => dealiasedTypeToMonoArg(t))), mvar))
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
  private def visitEnum(enumDecl: TypedAst.Enum)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (cas <- enumDecl.cases.values) {
      for (tpe <- cas.tpes) {
        visitType(tpe)
      }
    }
  }

  /**
    * Emits flow constraints for all case field types in `restrictableEnumDecl`.
    */
  private def visitRestrictableEnum(restrictableEnumDecl: TypedAst.RestrictableEnum)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (cas <- restrictableEnumDecl.cases.values) {
      for (tpe <- cas.tpes) {
        visitType(tpe)
      }
    }
  }

  /**
    * Emits flow constraints for all field types in `structDecl`.
    */
  private def visitStruct(structDecl: TypedAst.Struct)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (field <- structDecl.fields.values) {
      visitType(field.tpe)
    }
  }

  /**
    * Emits flow constraints for the formal parameter and return types of `op`.
    */
  private def visitOp(op: TypedAst.Op)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (case FormalParam(_, tpe, _, _, _) <- op.spec.fparams) {
      visitType(tpe)
    }
    visitType(op.spec.retTpe)
  }

  /**
    * Emits flow constraints for the formal parameter types, return type, and body of `defn`.
    */
  private def visitDef(defn: TypedAst.Def)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (case FormalParam(_, tpe, _, _, _) <- defn.spec.fparams) {
      visitType(tpe)
    }
    visitType(defn.spec.retTpe)
    visitExp(defn.exp)
    entryPointHandlerConstraints(defn)
  }

  /**
    * Emits flow constraints for the default-handler calls that
    * [[SpecializeAndLower.wrapDefWithDefaultHandlers]] synthesizes around entry points.
    */
  private def entryPointHandlerConstraints(defn: TypedAst.Def)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit =
    if (TypedAstOps.isEntryPoint(defn)(root)) {
      val loc = defn.spec.eff.loc
      val defEffects = Canonicalization.evalEff(defn.spec.eff)
      val requiredHandlers = root.defaultHandlers.filter(h => defEffects.contains(h.handledSym))
      requiredHandlers.foldLeft(defn.spec.eff) {
        case (eff, handler) =>
          val handlerDef = root.defs(handler.handlerSym)
          val handlerTparams = handlerDef.spec.tparams
          // E.g. imagine we have this effect declaration:
          // {{{
          //   eff Ask {
          //       def ask(): Int32
          //   }
          // }}}
          // with this default handler declaration:
          // {{{
          //   mod Ask {
          //       @DefaultHandler
          //       def handle(f: Unit -> a \ ef): a \ (ef - Ask) + IO = ...
          //   }
          // }}}
          // which will then be (implicitly) used at this entry point:
          // {{{
          //   def main(): Unit \ Ask + IO = println(Ask.ask())
          // }}}
          // `SpecializeAndLower` will synthesize a call `Ask.handle(() -> <main's body>)` around
          // `main`, so we must create the flow:
          // {{{
          //   [Unit, Ask + IO] ~> Ask.handle
          // }}}
          //
          // N.B. We use full unification rather than reading `a`/`ef` off fixed positions because
          // a default handler's parameter type only has to be *equal* to `Unit -> a \ ef`, not
          // written that way syntactically — e.g. `f: Unit -> a \ (ef + Pure)` is a valid handler
          // parameter type too.
          val concreteParamTpe = Type.mkArrowWithEffect(Type.Unit, eff, defn.spec.retTpe, loc)
          val subst = ConstraintSolver2.fullyUnify(handlerDef.spec.fparams.head.tpe, concreteParamTpe, RegionScope.Top, RigidityEnv.empty)(root.eqEnv, flix)
            .getOrElse(throw InternalCompilerException(s"Could not unify default handler '${handler.handlerSym}' against its call site.", loc))
          val args = handlerTparams.map(tp => typeToMonoArg(subst(Type.Var(tp.sym, loc))))
          sctx.addFlowConstraint(FlowConstraint(Instantiation(args), MonoVar.Def(handler.handlerSym)))
          Canonicalization.canonicalEffect(Type.mkUnion(Type.mkDifference(eff, handler.handledEff, loc), Type.IO, loc))
      }
      ()
    }

  /**
    * Emits flow constraints for all call sites and enum/struct construction sites in `exp`.
    * Datalog and channel nodes additionally emit constraints for the stdlib calls
    * [[SpecializeAndLower]] will synthesize for them.
    */
  private def visitExp(exp0: Expr)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = exp0 match {
    case Expr.Cst(_, _, _)        => ()
    case Expr.Var(_, _, _)        => ()
    case Expr.Hole(_, _, _, _, _) => ()
    case Expr.ApplyDef(symUse, exps, targs, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlowConstraint(FlowConstraint(Instantiation(targs.map(typeToMonoArg)), MonoVar.Def(symUse.sym)))
    case Expr.ApplySig(symUse, exps, targ, targs, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlowConstraint(FlowConstraint(Instantiation((targ :: targs).map(typeToMonoArg)), MonoVar.Sig(symUse.sym)))
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
      sctx.addFlowConstraint(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
    case Expr.RestrictableTag(_, exps, tpe, _, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      for (exp <- exps) {
        visitExp(exp)
      }
      sctx.addFlowConstraint(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
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
      sctx.addFlowConstraint(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
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

    case Expr.ParYield(frags, exp, _, _, _) =>
      // Generates, for each non-last fragment (element type `a`):
      //   Concurrent.Channel.newChannel(bufferSize: Int32): Mpmc[a, Static] \ IO
      //   Concurrent.Channel.put(e: a, c: Mpmc[a, Static]): Unit \ IO
      //   Concurrent.Channel.get(c: Mpmc[a, Static]): a \ IO
      for (f <- frags) {
        visitPat(f.pat)
        visitExp(f.exp)
      }
      visitExp(exp)
      for (frag <- frags.init) {
        val elmType = frag.exp.tpe
        val elmArg = typeToMonoArg(MonomorphHelpers.lowerChannelType(elmType))
        sctx.addFlowConstraint(FlowConstraint(Instantiation(List(elmArg)), MonoVar.Def(Defs.Concurrent.Channel.NewChannel)))
        sctx.addFlowConstraint(FlowConstraint(Instantiation(List(elmArg)), MonoVar.Def(Defs.Concurrent.Channel.Put)))
        sctx.addFlowConstraint(FlowConstraint(Instantiation(List(elmArg)), MonoVar.Def(Defs.Concurrent.Channel.Get)))
      }

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

    case Expr.GetChannel(exp, tpe, _, _) =>
      // Generates: Concurrent.Channel.get(c: Mpmc[a, Static]): a \ IO
      visitExp(exp)
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(MonomorphHelpers.lowerChannelType(tpe)))), MonoVar.Def(Defs.Concurrent.Channel.Get)))

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      // Generates: Concurrent.Channel.put(e: a, c: Mpmc[a, Static]): Unit \ IO
      visitExp(exp1)
      visitExp(exp2)
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(MonomorphHelpers.lowerChannelType(exp2.tpe)))), MonoVar.Def(Defs.Concurrent.Channel.Put)))

    case Expr.NewChannel(exp, tpe, _, _) =>
      // Generates: Concurrent.Channel.newChannelTuple(bufferSize: Int32): (Mpmc[a, Static], Mpmc[a, Static]) \ IO
      val elmType = extractChannelElm(tpe)
      visitExp(exp)
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(MonomorphHelpers.lowerChannelType(elmType)))), MonoVar.Def(Defs.Concurrent.Channel.NewChannelTuple)))

    case Expr.SelectChannel(rules, default, _, _, _) =>
      // Generates, per rule (element type `a`, not a Channel.get call):
      //   Concurrent.Channel.unsafeGetAndUnlock(c: Mpmc[a, Static], locks: List[ReentrantLock]): a \ IO
      //   Concurrent.Channel.mpmcAdmin(c: Mpmc[a, Static]): MpmcAdmin
      // Plus one fixed `List[MpmcAdmin]` value, built via mkTag/mkList.
      for (rule <- rules) {
        val elmType = rule.chan.tpe match {
          // Only possible shape since ConstraintGen.visitSelectRule unifies every rule's channel with Receiver[_]
          case Type.Apply(Type.Cst(TypeConstructor.Receiver, _), e, _) => e
          case t => throw InternalCompilerException(s"Expected Receiver[_], but got $t", rule.chan.loc)
        }
        val elmArg = typeToMonoArg(MonomorphHelpers.lowerChannelType(elmType))
        visitExp(rule.chan)
        visitExp(rule.exp)
        sctx.addFlowConstraint(FlowConstraint(Instantiation(List(elmArg)), MonoVar.Def(Defs.Concurrent.Channel.UnsafeGetAndUnlock)))
        sctx.addFlowConstraint(FlowConstraint(Instantiation(List(elmArg)), MonoVar.Def(Defs.Concurrent.Channel.MpmcAdmin)))
      }
      for (exp <- default) {
        visitExp(exp)
      }
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(Types.Concurrent.Channel.MpmcAdmin))), MonoVar.Enum(Enums.List.List)))

    case Expr.FixpointConstraintSet(cs, _, _) =>
      // Generates the Box/Unbox/liftN/lattice/Facts/ProjectInto/ProvenanceOf calls for Datalog
      // fixpoint nodes, mirroring the TypedAst structure lowering itself inspects — see
      // `boxConstraint`/`headTermConstraints`/`guardLiftConstraint`/`functionalLiftConstraint`/`latticeConstraints` below for the
      // concrete signature each one predicts.
      for (c <- cs) {
        val cparams0 = c.cparams
        c.head match {
          case Predicate.Head.Atom(_, den, terms, _, loc) =>
            for (term <- terms) {
              visitExp(term)
              headTermConstraints(cparams0, term)
            }
            latticeConstraints(den, terms.lastOption.map(_.tpe), loc)
        }
        for (p <- c.body) {
          p match {
            case Predicate.Body.Guard(e, _) =>
              visitExp(e)
              guardLiftConstraint(cparams0, e)
            case Predicate.Body.Functional(outBnds, e, _) =>
              functionalLiftConstraint(cparams0, outBnds.length, e)
              visitExp(e)
            case Predicate.Body.Atom(_, den, _, _, terms, _, loc) =>
              bodyAtomTermConstraints(cparams0, terms)
              latticeConstraints(den, terms.lastOption.map(_.tpe), loc)
          }
        }
      }

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      // Generates a `List[PredSym]` value directly via mkTag/mkList (bypassing the ordinary
      // rewrite path), so its instantiation must be predicted here.
      visitExp(exp)
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(Types.Fixpoint.Ast.Shared.PredSym))), MonoVar.Enum(Enums.List.List)))

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
    case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      for (exp <- exps) {
        visitExp(exp)
      }

    case Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
      // Generates: Fixpoint3.Solver.injectIntoN(p: PredSym, ts: f[(t1, ..., tN)]): Datalog \ ...
      // with Order[t1], ..., Order[tN], Foldable[f] — `f` is the container constructor, `t1..tN`
      // the tuple's component types.
      for (case (e, PredicateAndArity(_, arity)) <- exps.zip(predsAndArities)) {
        Type.eraseAliases(e.tpe) match {
          case Type.Apply(tc, innerTpe, _) =>
            val argTypes = Type.unmkTuplish(arity, innerTpe)
            val injectIntoArgs = (tc :: argTypes).map(typeToMonoArg)
            visitExp(e)
            sctx.addFlowConstraint(FlowConstraint(Instantiation(injectIntoArgs), MonoVar.Def(Defs.Fixpoint.Solver.InjectInto(arity))))
          case t => throw InternalCompilerException(s"Unexpected non-foldable type: '$t'.", loc)
        }
      }
    case Expr.FixpointQueryWithSelect(exps, queryExp, selects, _, _, _, tpe0, _, _) =>
      // Generates: Fixpoint3.Solver.factsN(p: PredSym, d: Datalog): Vector[(t1, ..., tN)] with
      // Order[t1], ..., Order[tN] — the `t1..tN` flow args must come from the resolved result type
      // `tpe0`, NOT from `selects`' own term types, which may still carry locally-scoped type vars.
      val arity = selects.length
      val innerTpe = unwrapVectorType(tpe0)
      val argTypes = Type.unmkTuplish(arity, innerTpe)
      for (exp <- exps) {
        visitExp(exp)
      }
      visitExp(queryExp)
      sctx.addFlowConstraint(FlowConstraint(Instantiation(argTypes.map(typeToMonoArg)), MonoVar.Def(Defs.Fixpoint.Solver.Facts(arity))))

    case Expr.FixpointQueryWithProvenance(exps, select, _, tpe0, _, _) =>
      // Generates, per goal term (type `a`): Fixpoint3.Boxable.box(x: a): Boxed with Order[a]
      // Generates, per term type `t` the extensible-variant result can carry:
      //   Fixpoint3.Boxable.unbox(x: Boxed): t
      // Plus, at a fixed `Boxed` type:
      //   Fixpoint3.Solver.provenanceOf(p: PredSym, f: Vector[Boxed], withh: Vector[PredSym], mkExtVar: PredSym -> Vector[Boxed] -> t, d: Datalog): Vector[t]
      //   Vector.get(i: Int32, v: Vector[Boxed]): Boxed
      for (exp <- exps) {
        visitExp(exp)
      }
      select match {
        case Predicate.Head.Atom(_, _, terms, _, _) =>
          for (term <- terms) {
            visitExp(term)
            boxConstraint(term.tpe)
          }
      }
      val extVarType = unwrapVectorType(tpe0)
      for (t <- predicatesOfExtVar(extVarType).flatMap(_._2)) {
        sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(t))), MonoVar.Def(Defs.Fixpoint.Boxable.Unbox)))
      }
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(Types.Fixpoint.Boxed))), MonoVar.Def(Defs.Vector.Get)))
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(extVarType))), MonoVar.Def(Defs.Fixpoint.Solver.ProvenanceOf)))

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
  private def visitPat(pat0: TypedAst.Pattern)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = pat0 match {
    case TypedAst.Pattern.Tag(_, pats, tpe, _) =>
      val (mvar, tpArgs) = getMonoVarAndTypeArgs(tpe)
      for (pat <- pats) {
        visitPat(pat)
      }
      sctx.addFlowConstraint(FlowConstraint(Instantiation(tpArgs.map(typeToMonoArg)), mvar))
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

  /** Generates: Fixpoint3.Boxable.box(x: a): Boxed with Order[a] — mirrors [[SpecializeAndLower.box]]. */
  private def boxConstraint(tpe: Type)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit =
    sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(tpe))), MonoVar.Def(Defs.Fixpoint.Boxable.Box)))

  /**
    * Flows for a head term — mirrors [[SpecializeAndLower.lowerHeadTerm]]. A bare quantified var
    * generates nothing (it flows through as-is); anything else generates either `boxConstraint` (no
    * free vars) or:
    *   Fixpoint3.Boxable.liftN(f: t1 -> ... -> tN -> t): Boxed -> ... -> Boxed
    *     with Order[t1], ..., Order[tN], Order[t]
    */
  private def headTermConstraints(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = exp0 match {
    case Expr.Var(sym, tpe, _) =>
      if (!MonomorphHelpers.isQuantifiedVar(sym, cparams0)) {
        boxConstraint(tpe)
      }
    case _ =>
      val fvs = MonomorphHelpers.quantifiedVars(cparams0, exp0)
      if (fvs.isEmpty) {
        boxConstraint(exp0.tpe)
      } else {
        val argTypes = fvs.map(_._2) // t1, ..., tN
        val liftArgs = (argTypes :+ exp0.tpe).map(typeToMonoArg)
        sctx.addFlowConstraint(FlowConstraint(Instantiation(liftArgs), MonoVar.Def(Defs.Fixpoint.Boxable.Lift(fvs.length))))
      }
  }

  /** Flows for a body atom's terms — mirrors [[SpecializeAndLower.lowerBodyTerm]]. */
  private def bodyAtomTermConstraints(cparams0: List[TypedAst.ConstraintParam], terms: List[TypedAst.Pattern])(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    for (term <- terms) {
      term match {
        case TypedAst.Pattern.Wild(_, _)           => ()
        case TypedAst.Pattern.Var(bnd, tpe, _)     =>
          if (!MonomorphHelpers.isQuantifiedVar(bnd.sym, cparams0)) {
            boxConstraint(tpe)
          } else {
            ()
          }
        case TypedAst.Pattern.Cst(_, tpe, _)       => boxConstraint(tpe)
        case TypedAst.Pattern.Tag(_, _, _, loc)    => throw InternalCompilerException(s"Unexpected pattern: '$term'.", loc)
        case TypedAst.Pattern.Tuple(_, _, loc)     => throw InternalCompilerException(s"Unexpected pattern: '$term'.", loc)
        case TypedAst.Pattern.Error(_, loc)        => throw InternalCompilerException(s"Unexpected pattern: '$term'.", loc)
        case TypedAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$term'.", loc)
      }
    }
  }

  /**
    * Generates: Fixpoint3.Boxable.liftNb(f: t1 -> ... -> tN -> Bool): Boxed -> ... -> Boxed -> Bool
    *   with Order[t1], ..., Order[tN]
    * Mirrors [[SpecializeAndLower.mkGuard]]. Arity 0 emits nothing: there is no `lift0b`.
    */
  private def guardLiftConstraint(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    val fvs = MonomorphHelpers.quantifiedVars(cparams0, exp0)
    if (fvs.nonEmpty) {
      val argTypes = fvs.map(_._2) // t1, ..., tN
      val liftArgs = argTypes.map(typeToMonoArg)
      sctx.addFlowConstraint(FlowConstraint(Instantiation(liftArgs), MonoVar.Def(Defs.Fixpoint.Boxable.LiftB(fvs.length))))
    }
  }

  /**
    * Generates: Fixpoint3.Boxable.liftMXN(f: i1 -> ... -> iM -> Vector[(o1, ..., oN)]):
    *   Vector[Boxed] -> Vector[Vector[Boxed]]
    * Mirrors [[SpecializeAndLower.mkFunctional]].
    */
  private def functionalLiftConstraint(cparams0: List[TypedAst.ConstraintParam], outArity: Int, exp0: TypedAst.Expr)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
    val inVars = MonomorphHelpers.quantifiedVars(cparams0, exp0)
    val inner = Type.eraseAliases(exp0.tpe) match {
      case Type.Apply(Type.Cst(TypeConstructor.Vector, _), t, _) => t
      case t => throw InternalCompilerException(s"Expected Vector[_], but got $t", exp0.loc)
    }
    val outTypes = Type.unmkTuplish(outArity, inner)
    val inTypes = inVars.map(_._2) // i1, ..., iM
    val liftArgs = (inTypes ++ outTypes).map(typeToMonoArg)
    sctx.addFlowConstraint(FlowConstraint(Instantiation(liftArgs), MonoVar.Def(Defs.Fixpoint.Boxable.LiftXM(inTypes.length, outArity))))
  }

  /**
    * Generates, for `Denotation.Relational`, the value `Denotation[Boxed]`; for
    * `Denotation.Latticenal` (lattice term type `v`):
    *   Fixpoint3.Ast.Shared.lattice(): Denotation[v] with LowerBound[v], JoinLattice[v], MeetLattice[v]
    *   Fixpoint3.Ast.Shared.box(d: Denotation[v]): Denotation[Boxed] with Order[v]
    * Mirrors [[SpecializeAndLower.mkDenotation]].
    */
  private def latticeConstraints(den: Denotation, lastTermType: Option[Type], loc: SourceLocation)(implicit tparamEnv: TypeParamEnv,  sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = den match {
    case Denotation.Relational =>
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(Types.Fixpoint.Boxed))), MonoVar.Enum(Enums.Fixpoint.Ast.Shared.Denotation)))
    case Denotation.Latticenal =>
      val tpe = lastTermType.getOrElse(throw InternalCompilerException("Unexpected nullary lattice predicate.", loc))
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(tpe))), MonoVar.Def(Defs.Fixpoint.Ast.Shared.Lattice)))
      sctx.addFlowConstraint(FlowConstraint(Instantiation(List(typeToMonoArg(tpe))), MonoVar.Def(Defs.Fixpoint.Ast.Shared.Box)))
  }

  /** Returns `t` from `Vector[t]` — mirrors [[SpecializeAndLower.unwrapVectorType]]. */
  private def unwrapVectorType(tpe0: Type): Type = Type.eraseAliases(tpe0) match {
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), extType, _) => extType
    case t => throw InternalCompilerException(s"Expected Type.Apply(Type.Cst(TypeConstructor.Vector, _), _, _), but got $t", tpe0.loc)
  }

  /** Mirrors [[SpecializeAndLower.predicatesOfExtVar]]. */
  private def predicatesOfExtVar(tpe0: Type): List[(Name.Pred, List[Type])] = Type.eraseAliases(tpe0) match {
    case Type.Apply(Type.Cst(TypeConstructor.Extensible, _), tpe1, _) => predicatesOfSchemaRow(tpe1)
    case t => throw InternalCompilerException(s"Expected Type.Apply(Type.Cst(TypeConstructor.Extensible, _), _, _), but got $t", tpe0.loc)
  }

  /** Mirrors [[SpecializeAndLower.predicatesOfSchemaRow]]. */
  private def predicatesOfSchemaRow(row: Type): List[(Name.Pred, List[Type])] = row match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), rel, _), tpe2, _) =>
      (pred, termTypesOfRelation(rel)) :: predicatesOfSchemaRow(tpe2)
    case Type.Var(_, _) => Nil
    case Type.SchemaRowEmpty => Nil
    case t => throw InternalCompilerException(s"Got unexpected $t", t.loc)
  }

  /**
    * Mirrors [[SpecializeAndLower.termTypesOfRelation]]. Unlike it, a `Type.Var` tail — the
    * relation's own arity trailing off unconstrained, not a term type — is skipped, not an error.
    */
  private def termTypesOfRelation(rel: Type): List[Type] = {
    def flattenApply(rel0: Type): List[Type] = rel0 match {
      case Type.Cst(TypeConstructor.Relation(_), _) => Nil
      case Type.Apply(rest, t, _) => t :: flattenApply(rest)
      case Type.Var(_, _) => Nil
      case t => throw InternalCompilerException(s"Expected Type.Apply(_, _, _), but got $t", rel0.loc)
    }
    flattenApply(rel).reverse
  }

  /** Extracts T from NewChannel's `(Sender[T], Receiver[T])` type — see ConstraintGen's NewChannel rule. */
  private def extractChannelElm(tpe: Type): Type = tpe.typeArguments match {
    case List(Type.Apply(Type.Cst(TypeConstructor.Sender, _), elm, _), _) => elm
    case List(Type.Apply(Type.Cst(TypeConstructor.Receiver, _), elm, _), _) => elm
    case _ => throw InternalCompilerException(s"Expected (Sender[_], Receiver[_]), but got $tpe", tpe.loc)
  }

  /** Converts `tpe0` to a `MonoArg` relative to the current declaration context. */
  private def typeToMonoArg(tpe0: Type)(implicit tparamEnv: TypeParamEnv, root: TypedAst.Root, flix: Flix): MonoArg =
    dealiasedTypeToMonoArg(Type.eraseAliases(tpe0))

  /** Like [[typeToMonoArg]], but `tpe` must already have its aliases erased (deeply). */
  private def dealiasedTypeToMonoArg(tpe: Type)(implicit tparamEnv: TypeParamEnv, root: TypedAst.Root, flix: Flix): MonoArg = tpe match {
    case Type.Var(sym, _) =>
      // A type variable that is not a tparam of the current decl (absent from tparamEnv) — e.g.
      // a region var introduced by `region r { ... }`. We record it as an opaque constant so the
      // flow is still emitted but the solver does not propagate it.
      tparamEnv.get(sym).getOrElse(MonoArg.Const(tpe))
    case at @ Type.AssocType(symUse, arg, kind, assocLoc) =>
      if (tpe.typeVars.isEmpty) {
        MonoArg.Const(Canonicalization.reduceAssocType(at)(root, flix))
      } else {
        MonoArg.Assoc(symUse.sym, dealiasedTypeToMonoArg(arg), kind, assocLoc)
      }
    case Type.Cst(_, _) =>
      MonoArg.Const(tpe)
    case Type.Apply(_, _, _) =>
      if (tpe.kind == Kind.Eff && tpe.typeVars.isEmpty) {
        MonoArg.Const(Canonicalization.simplify(tpe, isGround = true)(root, flix))
      } else {
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
