/*
 * Copyright 2021 Magnus Madsen
 *           2025 Casper Dalgaard Nielsen
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
import ca.uwaterloo.flix.language.ast.MonoAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.TypedAst.{ApplyPosition, DefaultHandler}
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Decreasing, Mutability, RegionScope, SymUse, TypeSource}
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.monomorph2.Specialize.{SpecializationTables, StrictSubstitution, lookupCaseSym, lookupRestrictableCaseSym, lookupStructSym, lookupSym, resolveSigSym, specializeFormalParam, specializeFormalParams}
import ca.uwaterloo.flix.language.phase.monomorph2.Symbols.{Defs, Enums, Types}
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmUtils, Result}
import ca.uwaterloo.flix.util.collection.{CofiniteSet, ListOps, Nel}

/**
  * Fuses specialization and lowering into a single AST walk: instantiates a declaration's
  * types/symbols to their concrete (ground) form and lowers Datalog, channel, JVM-interop, and
  * other high-level constructs to runtime primitives in the same pass.
  *
  * [[visitDef]]/[[lowerEnum]]/[[lowerStruct]]/[[lowerEffect]] are the four entry points
  * [[Specialize.run]] calls.
  *  Every call/tag/struct site here is resolved via [[Specialize]]'s `lookupSym`/`lookupCaseSym`/
  *  `lookupStructSym`/`resolveSigSym`.
  *
  * N.B.: [[ConstraintGen]] needs to predict every specialized symbol synthesized here.
  */
object SpecializeAndLower {

  /**
    * Lowers the given type `tpe0`.
    *
    * Replaces schema types with the Datalog enum type and channel-related types with the channel enum type.
    */
  private def lowerType(tpe0: Type): Type = tpe0.typeConstructor match {
    case Some(TypeConstructor.Schema) =>
      // Erase every Schema type, regardless of its polymorphic type applications, to the Datalog type.
      Types.Fixpoint.Ast.Datalog.Datalog
    case _ => lowerTypeNonSchema(tpe0)
  }

  private def lowerTypeNonSchema(tpe0: Type): Type = tpe0 match {
    case Type.Cst(_, _) => tpe0 // Reuse tpe0.

    case Type.Var(_, _) => tpe0

    // Sender[t] -> Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe, _) =>
      val t = lowerType(tpe)
      mkChannelTpe(t, loc)

    // Receiver[t] -> Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Receiver, loc), tpe, _) =>
      val t = lowerType(tpe)
      mkChannelTpe(t, loc)

    case Type.Apply(tpe1, tpe2, loc) =>
      val t1 = lowerType(tpe1)
      val t2 = lowerType(tpe2)
      // Performance: Reuse tpe0, if possible.
      if ((t1 eq tpe1) && (t2 eq tpe2)) {
        tpe0
      } else {
        Type.Apply(t1, t2, loc)
      }

    case Type.Alias(_, _, _, loc) => throw InternalCompilerException("unexpected type alias", loc)

    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException("unexpected associated type", loc)

    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)

    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

  }

  /** Grounds, lowers, and rewrites `t` to its specialized form. */
  private def visitType(t: Type, subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): Type =
    Specialize.rewriteEnumStructType(lowerType(subst(t)))

  /** Lowers and rewrites `t` that has already been grounded. */
  private def visitTypeSubstituted(t: Type)(implicit tables: SpecializationTables): Type =
    Specialize.rewriteEnumStructType(lowerType(t))

  /** Specializes and lowers `defn0` under `subst` into a `MonoAst.Def` with the specialized symbol `freshSym`. */
  protected[monomorph2] def visitDef(freshSym: Symbol.DefnSym, defn0: TypedAst.Def, subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Def = {
    implicit val lctx: LocalContext = LocalContext.empty
    val defn = wrapIfEntryPoint(defn0, subst)
    defn match {
      case TypedAst.Def(_, spec0, exp, loc) =>
        val (fparams, env0) = specializeFormalParams(spec0.fparams, subst)
        val fs = fparams.map(lowerFormalParam).map(Specialize.rewriteFormalParam)
        val spec = spec0 match {
          case TypedAst.Spec(doc, ann, mod, _, _, declaredScheme, retTpe, eff, _, _) =>
            MonoAst.Spec(doc, ann, mod, fs, visitType(declaredScheme.base, subst), visitType(retTpe, subst), subst(eff), DefContext.Unknown)
        }
        val e = visitExp(exp, env0, subst)
        MonoAst.Def(freshSym, spec, e, loc)
    }
  }

  /**
    * If `defn0` is an entry point, substitutes its spec's types and wraps it with its required
    * default handlers before the rest of lowering; otherwise returns `defn0` unchanged.
    */
  private def wrapIfEntryPoint(defn0: TypedAst.Def, subst: StrictSubstitution)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Def =
    if (!TypedAstOps.isEntryPoint(defn0)) {
      defn0
    } else {
      defn0 match {
        case TypedAst.Def(sym, spec0, exp, loc) =>
          val spec = spec0 match {
            case TypedAst.Spec(doc, ann, mod, tparams, fparams0, declaredScheme0, retTpe, eff, tconstrs, econstrs) =>
              val fparams = fparams0.map {
                case TypedAst.FormalParam(bnd, tpe, src, decreasing, floc) =>
                  TypedAst.FormalParam(bnd, subst(tpe), src, decreasing, floc)
              }
              val declaredScheme = declaredScheme0 match {
                case Scheme(quantifiers, tconstrs1, econstrs1, base) =>
                  Scheme(quantifiers, tconstrs1, econstrs1, subst(base))
              }
              TypedAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, subst(retTpe), subst(eff), tconstrs, econstrs)
          }
          wrapDefWithDefaultHandlers(TypedAst.Def(sym, spec, exp, loc))
      }
    }

  /**
    * Specializes and lowers `exp0` in one fused walk:
    *   - variables are renamed via `env0`,
    *   - every type is ground-instantiated via `subst`,
    *   - def/sig/case/struct symbols are resolved against the solver solution,
    *   - types are lowered and Datalog/channel expressions are lowered to the primitives.
    */
  private def visitExp(exp0: TypedAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case TypedAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe, subst)
      MonoAst.Expr.Cst(cst, t, loc)

    case TypedAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe, subst)
      MonoAst.Expr.Var(env0(sym), t, loc)

    case TypedAst.Expr.Hole(sym, _, tpe, eff, loc) =>
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, subst(eff), loc)

    case TypedAst.Expr.HoleWithExp(_, _, tpe, _, loc) =>
      val sym = Symbol.freshHoleSym(loc)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp, env0, subst) // TODO RESTR-VARS maybe add to monoAST

    case TypedAst.Expr.Use(_, _, exp, _) =>
      visitExp(exp, env0, subst)

    case TypedAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val (fp, binding) = specializeFormalParam(fparam, subst)
      val p = Specialize.rewriteFormalParam(lowerFormalParam(fp))
      val e = visitExp(exp, env0 + binding, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.Lambda(p, e, t, loc)

    case TypedAst.Expr.ApplyClo(exp1, exp2, tpe, eff, _, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyClo(e1, e2, t, subst(eff), loc)

    case TypedAst.Expr.ApplyDef(symUse, exps, _, itpe0, tpe, eff, _, loc) =>
      val groundArrowTpe = subst(itpe0)
      val newSym = lookupSym(symUse.sym, groundArrowTpe)
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, visitTypeSubstituted(groundArrowTpe), visitType(tpe, subst), subst(eff), loc)

    case TypedAst.Expr.ApplyLocalDef(symUse, exps, _, tpe, eff, _, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyLocalDef(env0(symUse.sym), es, t, subst(eff), loc)

    case TypedAst.Expr.ApplyOp(symUse, exps, tpe, eff, _, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyOp(symUse.sym, es, t, subst(eff), loc)

    case TypedAst.Expr.Unary(_, _, _, _, _) => ???

    case TypedAst.Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env1, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.Let(freshSym, e1, e2, t, subst(eff), Occur.Unknown, loc)

    case TypedAst.Expr.LocalDef(_, bnd, fparams, exp1, exp2, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val (fparams1, env2) = specializeFormalParams(fparams, subst)
      val fps = fparams1.map(lowerFormalParam).map(Specialize.rewriteFormalParam)
      val e1 = visitExp(exp1, env1 ++ env2, subst)
      val e2 = visitExp(exp2, env1, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.LocalDef(freshSym, fps, e1, e2, t, subst(eff), Occur.Unknown, loc)

    case TypedAst.Expr.Region(bnd, regSym, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val e = visitExp(exp, env1, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.Region(freshSym, regSym, e, t, subst(eff), loc)

    case TypedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val e3 = visitExp(exp3, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.IfThenElse(e1, e2, e3, t, subst(eff), loc)

    case TypedAst.Expr.Stm(_, _, _, _, _) => ???
    case TypedAst.Expr.Discard(_, _, _) => ???

    case TypedAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules.map(visitMatchRule(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.Match(e, rs, t, subst(eff), loc)

    case TypedAst.Expr.RestrictableChoose(_, exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules.map(visitRestrictableChooseRule(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.Match(e, rs, t, subst(eff), loc)

    case TypedAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules.map(visitExtMatchRule(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ExtMatch(e, rs, t, subst(eff), loc)
    case TypedAst.Expr.Tag(symUse, exps, tpe, eff, loc) =>
      val t = subst(tpe)
      val newSym = lookupCaseSym(symUse.sym, t)
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(newSym), es, visitTypeSubstituted(t), subst(eff), loc)

    case TypedAst.Expr.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      val t = subst(tpe)
      val newSym = lookupRestrictableCaseSym(symUse.sym, t)
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(newSym), es, visitTypeSubstituted(t), subst(eff), loc)

    case TypedAst.Expr.ExtTag(label, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ExtTag(label), es, t, subst(eff), loc)

    case TypedAst.Expr.Tuple(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tuple, es, t, subst(eff), loc)

    case TypedAst.Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.RecordSelect(label), List(e), t, subst(eff), loc)

    case TypedAst.Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.RecordExtend(label), List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.RecordRestrict(label), List(e), t, subst(eff), loc)

    case TypedAst.Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayLit, e :: es, t, subst(eff), loc)

    case TypedAst.Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val e3 = visitExp(exp3, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayNew, List(e1, e2, e3), t, subst(eff), loc)

    case TypedAst.Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayLoad, List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.ArrayLength(exp, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayLength, List(e), Type.Int32, subst(eff), loc)

    case TypedAst.Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val e3 = visitExp(exp3, env0, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayStore, List(e1, e2, e3), Type.Unit, subst(eff), loc)

    case TypedAst.Expr.StructNew(sym, fields0, region0, tpe, eff, loc) =>
      val t = subst(tpe)
      val newStructSym = lookupStructSym(sym, t)
      val fields = fields0.map {
        case (symUse, v) =>
          (new Symbol.StructFieldSym(newStructSym, symUse.sym.name, symUse.loc), visitExp(v, env0, subst))
      }
      val (names, es) = fields.unzip
      val tLow = visitTypeSubstituted(t)
      region0.map(visitExp(_, env0, subst)) match {
        case Some(region) =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.StructNew(newStructSym, Mutability.Mutable, names), region :: es, tLow, subst(eff), loc)
        case None =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.StructNew(newStructSym, Mutability.Immutable, names), es, tLow, subst(eff), loc)
      }

    case TypedAst.Expr.StructGet(exp, field, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val newStructSym = lookupStructSym(field.sym.structSym, subst(exp.tpe))
      val newFieldSym = new Symbol.StructFieldSym(newStructSym, field.sym.name, field.loc)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.StructGet(newFieldSym), List(e), t, subst(eff), loc)

    case TypedAst.Expr.StructPut(exp, field, exp1, tpe, eff, loc) =>
      val struct = visitExp(exp, env0, subst)
      val newStructSym = lookupStructSym(field.sym.structSym, subst(exp.tpe))
      val newFieldSym = new Symbol.StructFieldSym(newStructSym, field.sym.name, field.loc)
      val rhs = visitExp(exp1, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.StructPut(newFieldSym), List(struct, rhs), t, subst(eff), loc)

    case TypedAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.VectorLit, es, t, subst(eff), loc)

    case TypedAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.VectorLoad, List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, env0, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.VectorLength, List(e), Type.Int32, e.eff, loc)

    case TypedAst.Expr.Ascribe(exp, _, _, _, _, _) =>
      visitExp(exp, env0, subst)
    case TypedAst.Expr.InstanceOf(_, _, _) => ???
    case TypedAst.Expr.CheckedCast(_, _, _, _, _) => ???
    case TypedAst.Expr.UncheckedCast(_, _, _, _, _, _) => ???
    case TypedAst.Expr.Unsafe(_, _, _, _, _, _) => ???
    case TypedAst.Expr.Throw(exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Throw, List(e), t, subst(eff), loc)

    case TypedAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      val rs = rules.map(visitCatchRule(_, env0, subst))
      MonoAst.Expr.TryCatch(e, rs, t, subst(eff), loc)

    case TypedAst.Expr.Handler(symUse, rules, bodyTpe, bodyEff0, handledEff, tpe, loc) =>
      // `handler sym { rules }` lowers to `handlerBody -> try handlerBody() with sym { rules }`.
      val bodySym = Symbol.freshVarSym("handlerBody", BoundBy.FormalParam, loc.asSynthetic)(RegionScope.Top, flix)
      val bodyEff = subst(bodyEff0)
      val bt = visitType(bodyTpe, subst)
      val bodyThunkType = Type.mkArrowWithEffect(Type.Unit, bodyEff, bt, loc.asSynthetic)
      val param = MonoAst.FormalParam(bodySym, bodyThunkType, Occur.Unknown, loc.asSynthetic)

      val bodyVar = MonoAst.Expr.Var(bodySym, bodyThunkType, loc.asSynthetic)
      val body = MonoAst.Expr.ApplyClo(bodyVar, MonoAst.Expr.Cst(Constant.Unit, Type.Unit, loc.asSynthetic), bt, bodyEff, loc.asSynthetic)
      val rs = rules.map(visitHandlerRule(_, env0, subst))
      val runWith = MonoAst.Expr.RunWith(body, symUse, rs, bt, subst(handledEff), loc)

      val t = visitType(tpe, subst)

      MonoAst.Expr.Lambda(param, runWith, t, loc)

    case TypedAst.Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      // `run exp1 with exp2` lowers to `exp2(_runWith -> exp1)`.
      val e1 = visitExp(exp1, env0, subst)
      val unitParam = MonoAst.FormalParam(Symbol.freshVarSym("_runWith", BoundBy.FormalParam, loc.asSynthetic)(RegionScope.Top, flix), Type.Unit, Occur.Unknown, loc.asSynthetic)
      val thunkType = Type.mkArrowWithEffect(Type.Unit, e1.eff, e1.tpe, loc.asSynthetic)
      val thunk = MonoAst.Expr.Lambda(unitParam, e1, thunkType, loc.asSynthetic)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyClo(visitExp(exp2, env0, subst), thunk, t, subst(eff), loc)

    case TypedAst.Expr.InvokeConstructor(_, _, _, _, _) => ???

    case TypedAst.Expr.InvokeSuperConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeSuperConstructor(constructor), es, t, subst(eff), loc)

    case TypedAst.Expr.InvokeMethod(_, _, _, _, _, _) => ???

    case TypedAst.Expr.InvokeSuperMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      (lctx.sym, lctx.thisRef) match {
        case (Some(sym), Some(thisRef)) =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeSuperMethod(sym, method), thisRef :: es, t, subst(eff), loc)
        case _ =>
          throw InternalCompilerException("InvokeSuperMethod outside NewObject context", loc)
      }

    case TypedAst.Expr.InvokeStaticMethod(_, _, _, _, _) => ???

    case TypedAst.Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.GetField(field), List(e), t, subst(eff), loc)

    case TypedAst.Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.PutField(field), List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.GetStaticField(field), List.empty, t, subst(eff), loc)

    case TypedAst.Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.PutStaticField(field), List(e), t, subst(eff), loc)

    case TypedAst.Expr.NewObject(_, _, _, _, _, _, _) => ???

    case TypedAst.Expr.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      lowerNewChannel(e, subst(tpe), subst(eff), loc)

    case TypedAst.Expr.GetChannel(innerExp, tpe, eff, loc) =>
      // N.B.: innerExp.tpe is threaded in RAW, since e.tpe is already enum/struct-rewritten.
      val e = visitExp(innerExp, env0, subst)
      mkGetChannel(e, subst(innerExp.tpe), subst(tpe), subst(eff), loc)

    case TypedAst.Expr.PutChannel(innerExp1, innerExp2, _, eff, loc) =>
      // N.B.: innerExp1/2.tpe is threaded in RAW, since exp1/2.tpe is already enum/struct-rewritten.
      val exp1 = visitExp(innerExp1, env0, subst)
      val exp2 = visitExp(innerExp2, env0, subst)
      SpecializeAndLower.mkPutChannel(exp1, exp2, subst(innerExp1.tpe), subst(innerExp2.tpe), subst(eff), loc)
    case TypedAst.Expr.SelectChannel(rules0, default0, tpe, eff, loc) =>
      // N.B.: Each rule's RAW substituted channel type is threaded alongside the visited `chan` expr,
      // since `chan`'s own `.tpe` is already enum/struct-rewritten.
      val rules = rules0.map {
        case TypedAst.SelectChannelRule(bnd, chan, exp, _) =>
          val freshSym = Symbol.freshVarSym(bnd.sym)
          val env1 = env0 + (bnd.sym -> freshSym)
          (freshSym, visitExp(chan, env1, subst), visitExp(exp, env1, subst), lowerType(subst(chan.tpe)))
      }
      val default = default0.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      SpecializeAndLower.mkSelectChannel(rules, default, t, subst(eff), loc)

    case TypedAst.Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.ParYield(frags, exp, tpe, eff, loc) =>
      // N.B.: Each fragment's RAW substituted type is threaded alongside the visited expr,
      // since the visited expr's own `.tpe` is already rewritten.
      var curEnv = env0
      val fs = frags.map {
        case TypedAst.ParYieldFragment(pat, fragExp, fragLoc) =>
          val (p, env1) = visitPat(pat, Map.empty, subst)
          curEnv ++= env1
          (p, visitExp(fragExp, curEnv, subst), subst(fragExp.tpe), fragLoc)
      }
      val e = visitExp(exp, curEnv, subst)
      val t = visitType(tpe, subst)
      SpecializeAndLower.mkParYield(fs, e, t, subst(eff), loc)

    case TypedAst.Expr.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Lazy, List(e), t, Type.Pure, loc)

    case TypedAst.Expr.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Force, List(e), t, subst(eff), loc)
    case TypedAst.Expr.FixpointConstraintSet(_, _, _) => ???
    case TypedAst.Expr.FixpointLambda(_, _, _, _, _) => ???
    case TypedAst.Expr.FixpointMerge(_, _, _, _, _) => ???
    case TypedAst.Expr.FixpointQueryWithProvenance(_, _, _, _, _, _) => ???
    case TypedAst.Expr.FixpointQueryWithSelect(_, _, _, _, _, _, _, _, _) => ???
    case TypedAst.Expr.FixpointSolveWithProject(_, _, _, _, _, _) => ???
    case TypedAst.Expr.FixpointInjectInto(_, _, _, _, _) => ???

    case TypedAst.Expr.ApplySig(symUse, exps, _, _, itpe0, tpe, eff, _, loc) =>
      val groundArrowTpe = subst(itpe0)
      val newSym = resolveSigSym(symUse.sym, groundArrowTpe)
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, visitTypeSubstituted(groundArrowTpe), visitType(tpe, subst), subst(eff), loc)

    case TypedAst.Expr.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)
  }

  /**
    * Specializes and lowers the given catch rule `rule0` (fresh binder).
    */
  private def visitCatchRule(rule: TypedAst.CatchRule, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.CatchRule = rule match {
    case TypedAst.CatchRule(bnd, clazz, exp, _) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val e = visitExp(exp, env1, subst)
      MonoAst.CatchRule(freshSym, clazz, e)
  }

  /**
    * Specializes and lowers the given handler rule `rule0` (fresh formal params).
    */
  private def visitHandlerRule(rule0: TypedAst.HandlerRule, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.HandlerRule = rule0 match {
    case TypedAst.HandlerRule(opSymUse, fparams0, body0, _) =>
      val (fparams1, env1) = specializeFormalParams(fparams0, subst)
      val fparams = fparams1.map(lowerFormalParam).map(Specialize.rewriteFormalParam)
      val body = visitExp(body0, env0 ++ env1, subst)
      MonoAst.HandlerRule(opSymUse, fparams, body)
  }

  /**
    * Specializes and lowers the given match rule `rule0`. The pattern's fresh binders extend the
    * env for both the guard and the body.
    */
  private def visitMatchRule(rule0: TypedAst.MatchRule, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.MatchRule = rule0 match {
    case TypedAst.MatchRule(pat, guard, body, _) =>
      val (p, env1) = visitPat(pat, Map.empty, subst)
      val extendedEnv = env0 ++ env1
      val g = guard.map(visitExp(_, extendedEnv, subst))
      val b = visitExp(body, extendedEnv, subst)
      MonoAst.MatchRule(p, g, b)
  }

  /**
    * Specializes and lowers the given pattern `pat0`, returning the fresh-binder env extension
    * alongside.
    */
  private def visitPat(pat0: TypedAst.Pattern, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): (MonoAst.Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case TypedAst.Pattern.Wild(tpe, loc) =>
      (MonoAst.Pattern.Wild(visitType(tpe, subst), loc), env0)

    case TypedAst.Pattern.Var(bnd, tpe, loc) =>
      val newSym = Symbol.freshVarSym(bnd.sym)
      val env =
        if (env0.contains(bnd.sym)) {
          env0
        } else {
          env0 + (bnd.sym -> newSym)
        }
      (MonoAst.Pattern.Var(newSym, visitType(tpe, subst), Occur.Unknown, loc), env)

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      (MonoAst.Pattern.Cst(cst, visitType(tpe, subst), loc), env0)

    case TypedAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val (ps, env) = visitPats(pats, env0, subst)
      val t = subst(tpe)
      val newSym = lookupCaseSym(symUse.sym, t)
      (MonoAst.Pattern.Tag(SymUse.CaseSymUse(newSym, symUse.loc), ps, visitTypeSubstituted(t), loc), env)

    case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
      val (ps, env) = visitPats(elms.toList, env0, subst)
      (MonoAst.Pattern.Tuple(Nel(ps.head, ps.tail), visitType(tpe, subst), loc), env)

    case TypedAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (psVal, envs) = pats.map {
        case TypedAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val (p1, env1) = visitPat(pat1, env0, subst)
          (MonoAst.Pattern.Record.RecordLabelPattern(label, p1, visitType(tpe1, subst), loc1), env1)
      }.unzip
      val (patVal, env1) = visitPat(pat, env0, subst)
      val env = (env1 :: envs).flatten.toMap
      (MonoAst.Pattern.Record(psVal, patVal, visitType(tpe, subst), loc), env)

    case TypedAst.Pattern.Error(_, loc) =>
      throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)
  }

  /**
    * Specializes and lowers `ps`, threading the env through binder freshening.
    */
  private def visitPats(ps: List[TypedAst.Pattern], env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): (List[MonoAst.Pattern], Map[Symbol.VarSym, Symbol.VarSym]) =
    ps.foldRight((Nil: List[MonoAst.Pattern], env0)) {
      case (pat0, (res, env1)) =>
        val (pat, env) = visitPat(pat0, env1, subst)
        (pat :: res, env)
    }

  /**
    * Specializes and lowers the given restrictable choice rule `rule0` to a match rule.
    */
  private def visitRestrictableChooseRule(rule0: TypedAst.RestrictableChooseRule, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.MatchRule = rule0 match {
    case TypedAst.RestrictableChooseRule(pat, exp) =>
      pat match {
        case TypedAst.RestrictableChoosePattern.Tag(symUse, pat0, tpe, loc) =>
          val env = pat0.foldLeft(env0) {
            case (env1, TypedAst.RestrictableChoosePattern.Var(bnd, _, _)) =>
              env1 + (bnd.sym -> Symbol.freshVarSym(bnd.sym))
            case (env1, TypedAst.RestrictableChoosePattern.Wild(_, _)) => env1
            case (_, TypedAst.RestrictableChoosePattern.Error(_, errLoc)) => throw InternalCompilerException("unexpected restrictable choose variable", errLoc)
          }
          val termPatterns = pat0.map {
            case TypedAst.RestrictableChoosePattern.Var(TypedAst.Binder(varSym, _), varTpe, varLoc) => MonoAst.Pattern.Var(env(varSym), subst(varTpe), Occur.Unknown, varLoc)
            case TypedAst.RestrictableChoosePattern.Wild(wildTpe, wildLoc) => MonoAst.Pattern.Wild(subst(wildTpe), wildLoc)
            case TypedAst.RestrictableChoosePattern.Error(_, errLoc) => throw InternalCompilerException("unexpected restrictable choose variable", errLoc)
          }
          val t = subst(tpe)
          val newSym = lookupRestrictableCaseSym(symUse.sym, t)
          val p = MonoAst.Pattern.Tag(SymUse.CaseSymUse(newSym, symUse.loc), termPatterns, visitTypeSubstituted(t), loc)
          MonoAst.MatchRule(p, None, visitExp(exp, env, subst))

        case TypedAst.RestrictableChoosePattern.Error(_, loc) => throw InternalCompilerException("unexpected error restrictable choose pattern", loc)
      }
  }

  /**
    * Specializes and lowers the given `ematch` pattern `pat0`, returning the fresh-binder env
    * extension alongside.
    */
  private def visitExtPat(pat0: TypedAst.ExtPattern, subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): (MonoAst.ExtPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case TypedAst.ExtPattern.Default(loc) =>
      (MonoAst.ExtPattern.Default(loc), Map.empty)

    case TypedAst.ExtPattern.Tag(label, pats, loc) =>
      val (ps, symMaps) = pats.map(visitExtTagPat(_, subst)).unzip
      (MonoAst.ExtPattern.Tag(label, ps, loc), symMaps.flatten.toMap)

    case TypedAst.ExtPattern.Error(loc) =>
      throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  /**
    * Specializes and lowers the given `ematch` tag-argument pattern `pat0`, returning the
    * fresh-binder env extension alongside.
    */
  private def visitExtTagPat(pat0: TypedAst.ExtTagPattern, subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): (MonoAst.ExtTagPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case TypedAst.ExtTagPattern.Wild(tpe, loc) =>
      (MonoAst.ExtTagPattern.Wild(visitType(tpe, subst), loc), Map.empty)

    case TypedAst.ExtTagPattern.Var(bnd, tpe, loc) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      (MonoAst.ExtTagPattern.Var(freshSym, visitType(tpe, subst), Occur.Unknown, loc), Map(bnd.sym -> freshSym))

    case TypedAst.ExtTagPattern.Unit(tpe, loc) =>
      (MonoAst.ExtTagPattern.Unit(visitType(tpe, subst), loc), Map.empty)

    case TypedAst.ExtTagPattern.Error(_, loc) =>
      throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  /**
    * Specializes and lowers the given `ematch` rule `rule0`.
    */
  private def visitExtMatchRule(rule0: TypedAst.ExtMatchRule, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.ExtMatchRule = rule0 match {
    case TypedAst.ExtMatchRule(pat, exp, loc) =>
      val (p, env1) = visitExtPat(pat, subst)
      val e = visitExp(exp, env0 ++ env1, subst)
      MonoAst.ExtMatchRule(p, e, loc)
  }

  /**
    * Wraps an entry point function with calls to the default handlers of each of the effects appearing in
    * its signature. The order in which the handlers are applied is not defined and should not be relied upon.
    */
  private def wrapDefWithDefaultHandlers(currentDef: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Def = {
    // Entry points are expected to have a concrete (ground) effect set.
    val defEffects: CofiniteSet[Symbol.EffSym] = Type.eval(currentDef.spec.eff) match {
      case Result.Ok(s) => s
      case Result.Err(_) => throw InternalCompilerException("Unexpected illegal effect set on entry point", currentDef.spec.eff.loc)
    }
    // Order of application follows the order of root.defaultHandlers and is otherwise unspecified.
    val requiredHandlers = root.defaultHandlers.filter(h => defEffects.contains(h.handledSym))
    requiredHandlers.foldLeft(currentDef)((defn, handler) => wrapInHandler(defn, handler))
  }

  /**
    * Wraps `defn` with `defaultHandler`: `def f(...): tpe \ ef = exp` becomes
    * `def f(...): tpe \ (ef - handledEffect) + IO = handler(_ -> exp)`.
    */
  private def wrapInHandler(defn: TypedAst.Def, defaultHandler: DefaultHandler)(implicit flix: Flix): TypedAst.Def = defn match {
    case TypedAst.Def(sym, spec0, exp, defLoc) =>
      val effLoc = spec0.eff.loc.asSynthetic
      val baseTypeLoc = spec0.declaredScheme.base.loc.asSynthetic
      val expLoc = exp.loc.asSynthetic
      val effDif = Type.mkDifference(spec0.eff, defaultHandler.handledEff, effLoc)
      // Canonicalized to match defTable's canonicalized keys.
      val eff = Canonicalization.canonicalEffect(Type.mkUnion(effDif, Type.IO, effLoc))
      val tpe = Type.mkCurriedArrowWithEffect(spec0.fparams.map(_.tpe), eff, spec0.retTpe, baseTypeLoc)
      val spec = spec0 match {
        case TypedAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme0, retTpe, _, tconstrs, econstrs) =>
          val declaredScheme = declaredScheme0 match {
            case Scheme(quantifiers, tconstrs1, econstrs1, _) => Scheme(quantifiers, tconstrs1, econstrs1, tpe)
          }
          TypedAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, econstrs)
      }
      val innerLambda =
        TypedAst.Expr.Lambda(
          TypedAst.FormalParam(
            TypedAst.Binder(Symbol.freshVarSym("_", BoundBy.FormalParam, expLoc)(RegionScope.Top, flix), Type.Unit),
            Type.Unit,
            TypeSource.Inferred,
            Decreasing.NonDecreasing,
            expLoc
          ),
          exp,
          Type.mkArrowWithEffect(Type.Unit, spec0.eff, spec0.retTpe, expLoc),
          expLoc
        )
      val handlerArrowType = Type.mkArrowWithEffect(innerLambda.tpe, eff, spec0.retTpe, expLoc)
      // Left unresolved: visitExp's ApplyDef case resolves it later, like any other call site —
      // pre-resolving here would crash, since root.defs has no entry for a fresh sym.
      val handlerDefSymUse = SymUse.DefSymUse(defaultHandler.handlerSym, expLoc)
      val handlerCall = TypedAst.Expr.ApplyDef(handlerDefSymUse, List(innerLambda), List(innerLambda.tpe), handlerArrowType, spec0.retTpe, eff, ApplyPosition.NonTail, expLoc)
      TypedAst.Def(sym, spec, handlerCall, defLoc)
  }

  /**
    * A local context threaded through `visitExp` to carry information from an
    * enclosing `NewObject` to nested `InvokeSuperMethod` expressions.
    *
    * @param sym       The internal name of the enclosing anonymous class.
    *                  Set to `Some` when lowering a `NewObject` method body; `None` otherwise.
    *                  Injected into `AtomicOp.InvokeSuperMethod` so the backend can generate
    *                  the `CHECKCAST` and `INVOKEVIRTUAL super$methodName` instructions.
    * @param thisRef   A `Var` expression referencing the `_this` parameter (the first formal
    *                  parameter of the JvmMethod). Prepended to `InvokeSuperMethod` arguments
    *                  so the backend receives the receiver object as the first expression.
    */
  private case class LocalContext(sym: Option[Symbol.AnonClassSym], thisRef: Option[MonoAst.Expr])

  private object LocalContext {
    val empty: LocalContext = LocalContext(None, None)
  }

  /** Lowers the given enum `enum0`. */
  protected[monomorph2] def lowerEnum(enum0: TypedAst.Enum)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Enum = enum0 match {
    case TypedAst.Enum(doc, ann, mod, sym, tparams0, _, cases0, loc) =>
      val tparams = tparams0.map(lowerTypeParam)
      val cases = cases0.map {
        case (_, TypedAst.Case(caseSym, tpes0, _, caseLoc)) =>
          val tpes = tpes0.map(tpe => visitTypeSubstituted(Canonicalization.simplify(tpe, isGround = false)))
          (caseSym, MonoAst.Case(caseSym, tpes, caseLoc))
      }
      MonoAst.Enum(doc, ann, mod, sym, tparams, cases, loc)
  }

  /** Lowers the given `effect`. */
  protected[monomorph2] def lowerEffect(effect: TypedAst.Effect)(implicit tables: SpecializationTables): MonoAst.Effect = effect match {
    case TypedAst.Effect(doc, ann, mod, sym, _, ops0, loc) =>
      // TODO EFFECT-TPARAMS use tparams
      val ops = ops0.map(lowerOp)
      MonoAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /** Lowers the given struct `struct0`. */
  protected[monomorph2] def lowerStruct(struct0: TypedAst.Struct)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Struct = struct0 match {
    case TypedAst.Struct(doc, ann, mod, sym, tparams0, _, fields0, loc) =>
      val tparams = tparams0.map(lowerTypeParam)
      val fields = fields0.map {
        case (fieldSym, field) => MonoAst.StructField(fieldSym, visitTypeSubstituted(Canonicalization.simplify(field.tpe, isGround = false)), loc)
      }
      MonoAst.Struct(doc, ann, mod, sym, tparams, fields.toList, loc)
  }

  /** Lowers the given `op`. */
  private def lowerOp(op: TypedAst.Op)(implicit tables: SpecializationTables): MonoAst.Op = op match {
    case TypedAst.Op(sym, spec0, loc) =>
      val spec = lowerSpec(spec0)
      MonoAst.Op(sym, spec, loc)
  }

  /** Lowers the given `spec0`. */
  private def lowerSpec(spec0: TypedAst.Spec)(implicit tables: SpecializationTables): MonoAst.Spec = spec0 match {
    case TypedAst.Spec(doc, ann, mod, _, fparams0, declaredScheme, retTpe, eff, _, _) =>
      val fs = fparams0.map(lowerFormalParam).map(Specialize.rewriteFormalParam)
      val fType = visitTypeSubstituted(declaredScheme.base)
      val rType = visitTypeSubstituted(retTpe)
      val e = visitTypeSubstituted(eff)
      MonoAst.Spec(doc, ann, mod, fs, fType, rType, e, DefContext.Unknown)
  }

  /** Lowers the given formal parameter `fparam`. */
  private def lowerFormalParam(fparam: TypedAst.FormalParam): MonoAst.FormalParam = fparam match {
    case TypedAst.FormalParam(bnd, tpe, _, _, loc0) => MonoAst.FormalParam(bnd.sym, lowerType(tpe), Occur.Unknown, loc0)
  }

  /** Lowers the given type parameter `tparam`. */
  private def lowerTypeParam(tparam: TypedAst.TypeParam): MonoAst.TypeParam = tparam match {
    case TypedAst.TypeParam(name, sym, loc) =>
      MonoAst.TypeParam(name, sym, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe`.
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Apply(Types.Concurrent.Channel.Mpmc, tpe, loc), Type.IO, loc)
  }

  /**
    * Returns a new channel tuple (sender, receiver) expression:
    * {{{ %%CHANNEL_NEW%%(m) }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.newChannel(10) }}}
    *
    * @param tpe The specialized type of the result.
    */
  private def lowerNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val groundArrowTpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookupSym(Defs.Concurrent.Channel.NewChannelTuple, groundArrowTpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, Specialize.rewriteEnumStructType(groundArrowTpe), visitTypeSubstituted(tpe), eff, loc)
  }

  /**
    * Returns a channel get expression:
    * {{{ <- c }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.get(c) }}}
    */
  private def mkGetChannel(exp: MonoAst.Expr, chanTpe: Type, tpe: Type, eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val groundArrowTpe = lowerType(Type.mkIoArrow(chanTpe, tpe, loc))
    val defnSym = lookupSym(Defs.Concurrent.Channel.Get, groundArrowTpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, Specialize.rewriteEnumStructType(groundArrowTpe), visitTypeSubstituted(tpe), eff, loc)
  }

  /**
    * Returns a channel put expression:
    * {{{ c <- 42 }}}
    * becomes a call to the standard library function:
    * {{{ let chan = c; let value = 42; Concurrent/Channel.put(value, chan) }}}
    *
    * Here `exp1` is the channel and `exp2` is the value (i.e. `exp1 <- exp2`). In source order
    * the channel is evaluated before the value, but `Channel.put` takes the value before the
    * channel. We let-bind both expressions in source order so that reordering them into the
    * argument list does not change their evaluation order. See:
    * https://github.com/flix/flix/issues/10378
    */
  private def mkPutChannel(exp1: MonoAst.Expr, exp2: MonoAst.Expr, chanTpe: Type, valTpe: Type, eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val groundArrowTpe = lowerType(Type.mkIoUncurriedArrow(Nel.of(valTpe, chanTpe), Type.Unit, loc))
    val defnSym = lookupSym(Defs.Concurrent.Channel.Put, groundArrowTpe)
    val chanSym = mkLetSym("chan", loc)
    val valueSym = mkLetSym("value", loc)
    val chanVar = MonoAst.Expr.Var(chanSym, exp1.tpe, loc)
    val valueVar = MonoAst.Expr.Var(valueSym, exp2.tpe, loc)
    val putExp = MonoAst.Expr.ApplyDef(defnSym, List(valueVar, chanVar), Specialize.rewriteEnumStructType(groundArrowTpe), Type.Unit, eff, loc)
    // The channel binding is the outermost let, so the channel is evaluated before the value.
    val valueLet = MonoAst.Expr.Let(valueSym, exp2, putExp, Type.Unit, eff, Occur.Unknown, loc)
    MonoAst.Expr.Let(chanSym, exp1, valueLet, Type.Unit, eff, Occur.Unknown, loc)
  }

  /**
    * Returns a new `VarSym` for use in a let-binding.
    *
    * This function is called `mkLetSym` to avoid confusion with [[mkVarSym]].
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)(RegionScope.Top, flix)
  }

  /**
    * Returns the cast of `e` to `tpe` and `eff`.
    *
    * If `exp` and `tpe` is bytecode incompatible, a runtime crash is inserted to appease the
    * bytecode verifier.
    */
  private def mkCast(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): MonoAst.Expr = {
    (exp.tpe, tpe) match {
      case (Type.Char, Type.Char) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Char, Type.Int16) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int16, Type.Char) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Bool, Type.Bool) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int8, Type.Int8) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int16, Type.Int16) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int32, Type.Int32) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Int64, Type.Int64) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Float32, Type.Float32) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (Type.Float64, Type.Float64) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (x, y) if !isPrimType(x) && !isPrimType(y) => MonoAst.Expr.Cast(exp, tpe, eff, loc)
      case (x, y) =>
        val crash = MonoAst.Expr.ApplyAtomic(AtomicOp.CastError(erasedString(x), erasedString(y)), Nil, tpe, eff, loc)
        MonoAst.Expr.Stm(List(exp), crash, tpe, eff, loc)
    }
  }

  /**
    * Returns `true` if `tpe` is a primitive type.
    *
    * N.B.: `tpe` must be normalized.
    */
  private def isPrimType(tpe: Type): Boolean = tpe match {
    case Type.Char => true
    case Type.Bool => true
    case Type.Int8 => true
    case Type.Int16 => true
    case Type.Int32 => true
    case Type.Int64 => true
    case Type.Float32 => true
    case Type.Float64 => true
    case Type.Cst(_, _) => false
    case Type.Apply(_, _, _) => false
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
  }

  /**
    * Returns the erased string representation of `tpe`
    *
    * N.B.: `tpe` must be normalized.
    */
  private def erasedString(tpe: Type): String = tpe match {
    case Type.Char => "Char"
    case Type.Bool => "Bool"
    case Type.Int8 => "Int8"
    case Type.Int16 => "Int16"
    case Type.Int32 => "Int32"
    case Type.Int64 => "Int64"
    case Type.Float32 => "Float32"
    case Type.Float64 => "Float64"
    case Type.Cst(_, _) => "Object"
    case Type.Apply(_, _, _) => "Object"
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
  }

  /**
    * Returns the `JvmType`/`JvmValue` case name for the primitive base type `tpe`, or `"JvmObject"`
    * for anything else.
    */
  private def jvmTypeCaseName(tpe: Type): String = tpe match {
    case Type.Cst(TypeConstructor.Bool, _)    => "JvmBool"
    case Type.Cst(TypeConstructor.Char, _)    => "JvmChar"
    case Type.Cst(TypeConstructor.Int8, _)    => "JvmInt8"
    case Type.Cst(TypeConstructor.Int16, _)   => "JvmInt16"
    case Type.Cst(TypeConstructor.Int32, _)   => "JvmInt32"
    case Type.Cst(TypeConstructor.Int64, _)   => "JvmInt64"
    case Type.Cst(TypeConstructor.Float32, _) => "JvmFloat32"
    case Type.Cst(TypeConstructor.Float64, _) => "JvmFloat64"
    case _                                    => "JvmObject"
  }

  /**
    * Returns the `valueOf` boxing method for a Flix primitive type.
    * This is the same mechanism javac uses to implement autoboxing.
    */
  private def javaBoxMethod(tpe: Type): java.lang.reflect.Method = tpe match {
    case Type.Bool => classOf[java.lang.Boolean].getMethod("valueOf", java.lang.Boolean.TYPE)
    case Type.Char => classOf[java.lang.Character].getMethod("valueOf", java.lang.Character.TYPE)
    case Type.Int8 => classOf[java.lang.Byte].getMethod("valueOf", java.lang.Byte.TYPE)
    case Type.Int16 => classOf[java.lang.Short].getMethod("valueOf", java.lang.Short.TYPE)
    case Type.Int32 => classOf[java.lang.Integer].getMethod("valueOf", java.lang.Integer.TYPE)
    case Type.Int64 => classOf[java.lang.Long].getMethod("valueOf", java.lang.Long.TYPE)
    case Type.Float32 => classOf[java.lang.Float].getMethod("valueOf", java.lang.Float.TYPE)
    case Type.Float64 => classOf[java.lang.Double].getMethod("valueOf", java.lang.Double.TYPE)
    case _ => throw InternalCompilerException(s"Unexpected non-primitive type '$tpe'", tpe.loc)
  }

  /**
    * Returns the unboxing method (e.g., `intValue`) for a Flix primitive type.
    * This is the same mechanism javac uses to implement auto-unboxing.
    */
  private def javaUnboxMethod(tpe: Type): java.lang.reflect.Method = tpe match {
    case Type.Bool => classOf[java.lang.Boolean].getMethod("booleanValue")
    case Type.Char => classOf[java.lang.Character].getMethod("charValue")
    case Type.Int8 => classOf[java.lang.Byte].getMethod("byteValue")
    case Type.Int16 => classOf[java.lang.Short].getMethod("shortValue")
    case Type.Int32 => classOf[java.lang.Integer].getMethod("intValue")
    case Type.Int64 => classOf[java.lang.Long].getMethod("longValue")
    case Type.Float32 => classOf[java.lang.Float].getMethod("floatValue")
    case Type.Float64 => classOf[java.lang.Double].getMethod("doubleValue")
    case _ => throw InternalCompilerException(s"Unexpected non-primitive type '$tpe'", tpe.loc)
  }

  /**
    * Returns the Flix Type for the Java wrapper class of a primitive type.
    * E.g., `Bool` -> `Native(java.lang.Boolean)`, `Int32` -> `Native(java.lang.Integer)`.
    */
  private def boxedWrapperType(tpe: Type, loc: SourceLocation): Type = tpe match {
    case Type.Bool => Type.Cst(TypeConstructor.Native(classOf[java.lang.Boolean]), loc)
    case Type.Char => Type.Cst(TypeConstructor.Native(classOf[java.lang.Character]), loc)
    case Type.Int8 => Type.Cst(TypeConstructor.Native(classOf[java.lang.Byte]), loc)
    case Type.Int16 => Type.Cst(TypeConstructor.Native(classOf[java.lang.Short]), loc)
    case Type.Int32 => Type.Cst(TypeConstructor.Native(classOf[java.lang.Integer]), loc)
    case Type.Int64 => Type.Cst(TypeConstructor.Native(classOf[java.lang.Long]), loc)
    case Type.Float32 => Type.Cst(TypeConstructor.Native(classOf[java.lang.Float]), loc)
    case Type.Float64 => Type.Cst(TypeConstructor.Native(classOf[java.lang.Double]), loc)
    case _ => throw InternalCompilerException(s"Unexpected non-primitive type '$tpe'", tpe.loc)
  }

  /**
    * Boxes `arg` if the actual arg type (Flix primitive) mismatches the expected param type (Object).
    * E.g., in `m.put("k", 42)` on a `HashMap[String, Int32]`, the actual type is `Int32`
    * but the expected type is `Object` (erased), so `42` is boxed via `Integer.valueOf(42)`.
    */
  private def boxIfNecessary(arg: MonoAst.Expr, expectedParamType: Class[?]): MonoAst.Expr = {
    val actualArgType = arg.tpe
    if (isPrimType(actualArgType) && !expectedParamType.isPrimitive) {
      MonoAst.Expr.ApplyAtomic(
        AtomicOp.InvokeStaticMethod(javaBoxMethod(actualArgType)),
        List(arg),
        boxedWrapperType(actualArgType, arg.loc),
        arg.eff,
        arg.loc.asSynthetic
      )
    } else arg
  }

  /** Returns the erased return type of the Java method on `clazz` matching `name` and `arity` (excluding the receiver). */
  private def overriddenJavaReturnType(clazz: Class[?], name: String, arity: Int): Option[Class[?]] =
    JvmUtils.getOverridableInstanceMethods(clazz).collectFirst {
      case m if m.getName == name && m.getParameterCount == arity => m.getReturnType
    }

  /**
    * Unboxes `expr` if the expected return type (Flix primitive) mismatches the actual return type (Object).
    * E.g., in `let v: Int32 = m.get("k")` on a `HashMap[String, Int32]`, the expected type is
    * `Int32` but the actual Java return type is `Object` (erased), so the result is unboxed via `intValue()`.
    */
  private def unboxIfNecessary(expr: MonoAst.Expr, expectedReturnType: Type, actualReturnType: Class[?]): MonoAst.Expr = {
    if (isPrimType(expectedReturnType) && !actualReturnType.isPrimitive) {
      MonoAst.Expr.ApplyAtomic(
        AtomicOp.InvokeMethod(javaUnboxMethod(expectedReturnType)),
        List(expr),
        expectedReturnType,
        expr.eff,
        expr.loc.asSynthetic
      )
    } else expr
  }

  /**
    * Returns a call to Java `method`, boxing `args` and unboxing the result symmetrically.
    */
  private def mkJavaInvoke(method: java.lang.reflect.Method, receiver: List[MonoAst.Expr], args: List[MonoAst.Expr], t: Type, eff: Type, loc: SourceLocation, mkOp: java.lang.reflect.Method => AtomicOp): MonoAst.Expr = {
    val boxedArgs = ListOps.zip(args, method.getParameterTypes.toList).map { case (arg, paramType) => boxIfNecessary(arg, paramType) }
    val javaReturnType = method.getReturnType
    val needsUnbox = isPrimType(t) && !javaReturnType.isPrimitive
    val invokeType = if (needsUnbox) boxedWrapperType(t, loc) else t
    val invoke = MonoAst.Expr.ApplyAtomic(mkOp(method), receiver ++ boxedArgs, invokeType, eff, loc)
    unboxIfNecessary(invoke, t, javaReturnType)
  }

  /**
    * Strips an auto-unboxing wrapper (e.g., `intValue()`) from `expr` if present.
    */
  private def stripAutoUnbox(expr: MonoAst.Expr): MonoAst.Expr = expr match {
    case MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeMethod(method), List(inner), _, _, _)
      if isAutoUnboxMethod(method) => inner
    case _ => expr
  }

  /** Returns `true` if `method` is a Java auto-unboxing method (e.g., `intValue`, `booleanValue`). */
  private def isAutoUnboxMethod(method: java.lang.reflect.Method): Boolean = {
    method.getParameterCount == 0 && (method.getName match {
      case "booleanValue" => true
      case "charValue"    => true
      case "byteValue"    => true
      case "shortValue"   => true
      case "intValue"     => true
      case "longValue"    => true
      case "floatValue"   => true
      case "doubleValue"  => true
      case _              => false
    })
  }

  /**
    * Returns the case symbol named `name` in the enum `sym`.
    */
  private def findCaseSym(sym: Symbol.EnumSym, name: String)(implicit root: TypedAst.Root): Symbol.CaseSym =
    root.enums(sym).cases.values.find(_.sym.name == name).get.sym

  /**
    * Returns a channel select expression:
    *
    * Channel select expressions are rewritten as follows:
    * {{{
    *  select {
    *    case x <- ?ch1 => ?handlech1
    *    case y <- ?ch2 => ?handlech2
    *    case _ => ?default
    *  }
    * }}}
    * becomes
    * {{{
    *   let ch1 = ?ch1;
    *   let ch2 = ?ch2;
    *   match selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: Nil, false) {  // true if no default
    *     case (0, locks) =>
    *       let x = unsafeGetAndUnlock(ch1, locks);
    *       ?handlech1
    *     case (1, locks) =>
    *       let y = unsafeGetAndUnlock(ch2, locks);
    *       ?handlech2
    *     case (-1, _) =>                                                  // Omitted if no default
    *      ?default                                                   // Unlock is handled by selectFrom
    * }}}
    * Note: match is not exhaustive: we're relying on the simplifier to handle this for us
    */
  private def mkSelectChannel(rules: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr, Type)], default: Option[MonoAst.Expr], tpe: Type, eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val t = lowerType(tpe)

    val channels = rules.map { case (_, c, _, _) => (mkLetSym("chan", loc), c) }
    val admins = mkChannelAdminList(rules, channels, loc)
    val selectExp = mkChannelSelect(admins, default, loc)
    val cases = mkChannelCases(rules, channels, eff, loc)
    val defaultCase = mkSelectDefaultCase(default, loc)
    val matchExp = MonoAst.Expr.Match(selectExp, cases ++ defaultCase, t, eff, loc)

    channels.foldRight[MonoAst.Expr](matchExp) {
      case ((sym, c), e) => MonoAst.Expr.Let(sym, c, e, t, eff, Occur.Unknown, loc)
    }
  }

  /**
    * Make the list of MpmcAdmin objects which will be passed to `selectFrom`.
    *
    * For each case like
    * {{{ x <- ?ch1 => ?handlech1 }}}
    * we generate
    * {{{ mpmcAdmin(x) }}}
    */
  private def mkChannelAdminList(rs: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr, Type)], channels: List[(Symbol.VarSym, MonoAst.Expr)], loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val admins = ListOps.zip(rs, channels) map {
      case ((_, _, _, rawChanTpe), (chanSym, _)) =>
        val groundArrowTpe = lowerType(Type.mkPureArrow(rawChanTpe, Types.Concurrent.Channel.MpmcAdmin, loc))
        val defnSym = lookupSym(Defs.Concurrent.Channel.MpmcAdmin, groundArrowTpe)
        MonoAst.Expr.ApplyDef(defnSym, List(MonoAst.Expr.Var(chanSym, visitTypeSubstituted(rawChanTpe), loc)), Specialize.rewriteEnumStructType(groundArrowTpe), Types.Concurrent.Channel.MpmcAdmin, Type.Pure, loc)
    }
    mkList(admins, Types.Concurrent.Channel.MpmcAdmin, loc)
  }

  /**
    * Construct a call to `selectFrom` given a list of MpmcAdmin objects and optional default.
    *
    * Transforms
    * {{{ mpmcAdmin(ch1), mpmcAdmin(ch1), ... }}}
    * Into
    * {{{ selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: ... :: Nil, false) }}}
    *
    * The second parameter is `true` (blocking) when `default` is `None`, `false` otherwise.
    */
  private def mkChannelSelect(admins: MonoAst.Expr, default: Option[MonoAst.Expr], loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val locksType = Types.List.mkList(Types.Concurrent.ReentrantLock.ReentrantLock, loc)

    val selectRetTpe = Type.mkTuple(List(Type.Int32, locksType), loc)
    val groundArrowTpe = Type.mkIoUncurriedArrow(Nel.of(admins.tpe, Type.Bool), selectRetTpe, loc)
    val blocking = default match {
      case Some(_) => MonoAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc)
      case None => MonoAst.Expr.Cst(Constant.Bool(true), Type.Bool, loc)
    }
    val defnSym = lookupSym(Defs.Concurrent.Channel.SelectFrom, groundArrowTpe)
    MonoAst.Expr.ApplyDef(defnSym, List(admins, blocking), Specialize.rewriteEnumStructType(lowerType(groundArrowTpe)), Specialize.rewriteEnumStructType(selectRetTpe), Type.IO, loc)
  }

  /**
    * Construct a sequence of MatchRules corresponding to the given SelectChannelRules
    *
    * Transforms the `i`'th
    * {{{ case x <- ?ch1 => ?handlech1 }}}
    * into
    * {{{
    * case (i, locks) =>
    *   let x = unsafeGetAndUnlock(ch1, locks);
    *   ?handlech1
    * }}}
    */
  private def mkChannelCases(rs: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr, Type)], channels: List[(Symbol.VarSym, MonoAst.Expr)], eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): List[MonoAst.MatchRule] = {
    val locksTypeRaw = Types.List.mkList(Types.Concurrent.ReentrantLock.ReentrantLock, loc)
    val locksType = Specialize.rewriteEnumStructType(locksTypeRaw)

    ListOps.zip(rs, channels).zipWithIndex map {
      case (((sym, _, exp, rawChanTpe), (chSym, _)), i) =>
        val locksSym = mkLetSym("locks", loc)
        val pat = mkTuplePattern(Nel(MonoAst.Pattern.Cst(Constant.Int32(i), Type.Int32, loc), List(MonoAst.Pattern.Var(locksSym, locksType, Occur.Unknown, loc))), loc)
        val getTpe = extractChannelTpe(rawChanTpe)
        val groundArrowTpe = lowerType(Type.mkIoUncurriedArrow(Nel.of(rawChanTpe, locksTypeRaw), getTpe, loc))
        val args = List(MonoAst.Expr.Var(chSym, visitTypeSubstituted(rawChanTpe), loc), MonoAst.Expr.Var(locksSym, locksType, loc))
        val defnSym = lookupSym(Defs.Concurrent.Channel.UnsafeGetAndUnlock, groundArrowTpe)
        val getExp = MonoAst.Expr.ApplyDef(defnSym, args, Specialize.rewriteEnumStructType(groundArrowTpe), visitTypeSubstituted(getTpe), eff, loc)
        val e = MonoAst.Expr.Let(sym, getExp, exp, exp.tpe, eff, Occur.Unknown, loc)
        MonoAst.MatchRule(pat, None, e)
    }
  }

  /**
    * Construct additional MatchRule to handle the (optional) default case
    * NB: Does not need to unlock because that is handled inside Concurrent/Channel.selectFrom.
    *
    * If `default` is `None` returns an empty list. Otherwise produces
    * {{{ case (-1, _) => ?default }}}
    */
  private def mkSelectDefaultCase(default: Option[MonoAst.Expr], loc: SourceLocation)(implicit tables: SpecializationTables): List[MonoAst.MatchRule] = {
    default match {
      case Some(defaultExp) =>
        val locksType = Specialize.rewriteEnumStructType(Types.List.mkList(Types.Concurrent.ReentrantLock.ReentrantLock, loc))
        val pat = mkTuplePattern(Nel(MonoAst.Pattern.Cst(Constant.Int32(-1), Type.Int32, loc), List(MonoAst.Pattern.Wild(locksType, loc))), loc)
        val defaultMatch = MonoAst.MatchRule(pat, None, defaultExp)
        List(defaultMatch)
      case None =>
        List()
    }
  }

  /**
    * Returns a desugared [[TypedAst.Expr.ParYield]] expression as a nested match-expression.
    */
  private def mkParYield(frags: List[(MonoAst.Pattern, MonoAst.Expr, Type, SourceLocation)], exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Only generate channels for n-1 fragments. We use the current thread for the last fragment.
    val fs = frags.init
    val last = frags.last

    // Generate symbols for each channel.
    val chanSymsWithPatAndExp = fs.map { case (p, e, rawTpe, l) => (p, mkLetSym("channel", l.asSynthetic), e, rawTpe) }

    // Make `GetChannel` exps for the spawnable exps.
    val waitExps = mkBoundParWaits(chanSymsWithPatAndExp, exp)

    // Evaluate the last expression in the current thread (so just make let-binding)
    val desugaredYieldExp = mkLetMatch(last._1, last._2, waitExps)

    // Generate channels and spawn exps.
    val chanSymsWithExp = chanSymsWithPatAndExp.map { case (_, s, e, rawTpe) => (s, e, rawTpe) }
    val blockExp = mkParChannels(desugaredYieldExp, chanSymsWithExp)

    // Wrap everything in a purity cast.
    MonoAst.Expr.Cast(blockExp, lowerType(tpe), eff, loc.asSynthetic)
  }

  /**
    * Returns a full `par yield` expression.
    */
  private def mkParChannels(exp: MonoAst.Expr, chanSymsWithExps: List[(Symbol.VarSym, MonoAst.Expr, Type)])(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Make spawn expressions `spawn ch <- exp`.
    val spawns = chanSymsWithExps.foldRight(exp: MonoAst.Expr) {
      case ((sym, e, rawTpe), acc) =>
        val loc = e.loc.asSynthetic
        val e1 = mkChannelExp(sym, rawTpe, loc) // The channel `ch`
        val e2 = mkPutChannel(e1, e, mkChannelTpe(rawTpe, loc), rawTpe, Type.IO, loc) // The put exp: `ch <- exp0`.
        val e3 = MonoAst.Expr.Cst(Constant.Static, Type.mkRegionToStar(Type.IO, loc), loc)
        val e4 = MonoAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e2, e3), Type.Unit, Type.IO, loc) // Spawn the put expression from above i.e. `spawn ch <- exp0`.
        MonoAst.Expr.Stm(List(e4), acc, acc.tpe, Type.mkUnion(e4.eff, acc.eff, loc), loc) // Return a statement expression containing the other spawn expressions along with this one.
    }

    // Make let bindings `let ch = chan 1;`.
    chanSymsWithExps.foldRight(spawns: MonoAst.Expr) {
      case ((sym, e, rawTpe), acc) =>
        val loc = e.loc.asSynthetic
        val chan = mkNewChannel(MonoAst.Expr.Cst(Constant.Int32(1), Type.Int32, loc), mkChannelTpe(rawTpe, loc), Type.IO, loc)
        MonoAst.Expr.Let(sym, chan, acc, acc.tpe, Type.mkUnion(e.eff, acc.eff, loc), Occur.Unknown, loc)
    }
  }

  /**
    * Make a new channel expression
    */
  private def mkNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val groundArrowTpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookupSym(Defs.Concurrent.Channel.NewChannel, groundArrowTpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, Specialize.rewriteEnumStructType(groundArrowTpe), Specialize.rewriteEnumStructType(tpe), eff, loc)
  }

  /**
    * Returns an expression where the pattern variables used in `exp` are
    * bound to [[TypedAst.Expr.GetChannel]] expressions,
    * i.e.
    * {{{
    *   let pat1 = <- ch1;
    *   let pat2 = <- ch2;
    *   let pat3 = <- ch3;
    *   ...
    *   let patn = <- chn;
    *   exp
    * }}}
    */
  private def mkBoundParWaits(patSymExps: List[(MonoAst.Pattern, Symbol.VarSym, MonoAst.Expr, Type)], exp: MonoAst.Expr)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr =
    patSymExps.map {
      case (p, sym, e, rawTpe) =>
        val loc = e.loc.asSynthetic
        val chExp = mkChannelExp(sym, rawTpe, loc)
        (p, mkGetChannel(chExp, mkChannelTpe(rawTpe, loc), rawTpe, Type.IO, loc))
    }.foldRight(exp) {
      case ((pat, chan), e) => mkLetMatch(pat, chan, e)
    }

  /**
    * Returns a desugared let-match expression, i.e.
    * {{{
    *   let pattern = exp;
    *   body
    * }}}
    * is desugared to
    * {{{
    *   match exp {
    *     case pattern => body
    *   }
    * }}}
    */
  private def mkLetMatch(pat: MonoAst.Pattern, exp: MonoAst.Expr, body: MonoAst.Expr): MonoAst.Expr = {
    val loc = exp.loc.asSynthetic
    val rule = List(MonoAst.MatchRule(pat, None, body))
    val eff = Type.mkUnion(exp.eff, body.eff, loc)
    MonoAst.Expr.Match(exp, rule, body.tpe, eff, loc)
  }

  /**
    * An expression for a channel variable called `sym`.
    */
  private def mkChannelExp(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation)(implicit tables: SpecializationTables): MonoAst.Expr = {
    MonoAst.Expr.Var(sym, Specialize.rewriteEnumStructType(mkChannelTpe(tpe, loc)), loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    *
    * @param elmType is assumed to be specialized and lowered. (Not yet struct/enum type rewritten)
    */
  private def mkList(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, elmType, loc)
    }
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    *
    * @param elmType is assumed to be specialized and lowered. (Not yet struct/enum type rewritten)
    */
  private def mkNil(elmType: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    mkTag(Enums.List.List, "Nil", Nil, Types.List.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type list of `elmType`.
    */
  private def mkCons(hd: MonoAst.Expr, tail: MonoAst.Expr, elmType: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    mkTag(Enums.List.List, "Cons", List(hd, tail), Types.List.mkList(elmType, loc), loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    *
    * @param tpe is assumed to be specialized and lowered. (Not yet struct/enum type rewritten)
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exps: List[MonoAst.Expr], tpe: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val caseSym0 = findCaseSym(sym, tag)
    val caseSym = Specialize.lookupCaseSym(caseSym0, tpe)
    val t = Specialize.rewriteEnumStructType(tpe)
    MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, t, Type.Pure, loc)
  }

  /**
    * Returns `(t1, t2)` where `tpe = Concurrent.Channel.Mpmc[t1, t2]`.
    *
    * @param tpe is assumed to be specialized, but not lowered.
    */
  private def extractChannelTpe(tpe: Type): Type = tpe match {
    case Type.Apply(Type.Apply(Types.Concurrent.Channel.Mpmc, elmType, _), _, _) => elmType
    case _ => throw InternalCompilerException(s"Cannot interpret '$tpe' as a channel type", tpe.loc)
  }

  /**
    * Returns a TypedAst.Pattern representing a tuple of patterns.
    *
    * @param patterns are assumed to contain specialized and lowered types.
    */
  private def mkTuplePattern(patterns: Nel[MonoAst.Pattern], loc: SourceLocation): MonoAst.Pattern = {
    MonoAst.Pattern.Tuple(patterns, Type.mkTuple(patterns.map(_.tpe), loc), loc)
  }

  /**
    * Lowers `sym` from a restrictable enum sym into a regular enum sym.
    */
  private[monomorph2] def lowerRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): Symbol.EnumSym =
    new Symbol.EnumSym(None, sym.namespace, sym.name, sym.loc)
}
