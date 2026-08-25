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
import ca.uwaterloo.flix.language.ast.MonoAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.TypedAst.{ApplyPosition, DefaultHandler, Predicate}
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Decreasing, Denotation, Fixity, JClass, JConstructor, JField, JMethod, Mutability, Polarity, PredicateAndArity, RegionScope, SolveMode, SymUse, TypeSource}
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, Name, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.monomorph2.Specialize.{SpecializationTables, StrictSubstitution, lookupCaseSym, lookupRestrictableCaseSym, lookupStructSym, lookupSym, resolveSigSym, specializeFormalParam, specializeFormalParams}
import ca.uwaterloo.flix.language.phase.monomorph2.Symbols.{Defs, Enums, Types}
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException, JvmUtils, Result}
import ca.uwaterloo.flix.util.collection.{CofiniteSet, ListOps, Nel}

import java.lang.constant.{ClassDesc, MethodTypeDesc}

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

    case TypedAst.Expr.Unary(sop, exp, tpe, eff, loc) => sop match {
      // ReflectOps are resolved here at specialization time since the reflected type is only known when it is grounded.
      case SemanticOp.ReflectOp.ReflectEff =>
        val expTpe = subst(exp.tpe)
        val typeArg = expTpe.typeArguments.headOption.getOrElse(
          throw InternalCompilerException(s"Expected ProxyEff[ef] type, got $expTpe", loc))
        val purityEnumSym = Enums.Reflect.Purity
        val caseName = typeArg match {
          case Type.Cst(TypeConstructor.Pure, _) => "Pure"
          case _                                 => "Impure"
        }
        val caseSym = findCaseSym(purityEnumSym, caseName)
        MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), Nil, Type.mkEnum(purityEnumSym, Nil, loc), Type.Pure, loc)

      case SemanticOp.ReflectOp.ReflectType =>
        val expTpe = subst(exp.tpe)
        val typeArg = expTpe.typeArguments.headOption.getOrElse(
          throw InternalCompilerException(s"Expected Proxy[t] type, got $expTpe", loc))
        val jvmTypeEnumSym = Enums.Reflect.JvmType
        val caseName = jvmTypeCaseName(typeArg.baseType)
        val caseSym = findCaseSym(jvmTypeEnumSym, caseName)
        MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), Nil, Type.mkEnum(jvmTypeEnumSym, Nil, loc), Type.Pure, loc)

      case SemanticOp.ReflectOp.ReflectValue =>
        val e = visitExp(exp, env0, subst)
        val expTpe = subst(exp.tpe)
        val jvmValueEnumSym = Enums.Reflect.JvmValue
        val resultType = Type.mkEnum(jvmValueEnumSym, Nil, loc)
        val caseName = jvmTypeCaseName(expTpe.baseType)
        val caseSym = findCaseSym(jvmValueEnumSym, caseName)
        val tagArg = if (caseName == "JvmObject") {
          val objType = Type.mkNative(classOf[java.lang.Object], loc)
          mkCast(e, objType, Type.Pure, loc)
        } else {
          e
        }
        MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), List(tagArg), resultType, subst(eff), loc)

      case _ =>
        val e = visitExp(exp, env0, subst)
        val t = visitType(tpe, subst)
        MonoAst.Expr.ApplyAtomic(AtomicOp.Unary(sop), List(e), t, subst(eff), loc)
    }

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

    case TypedAst.Expr.Stm(exps, exp, tpe, eff, loc) =>
      // Strip auto-unboxing: `m.put("k", 42);` discards the result, so we must not unbox the
      // null that `HashMap.put` returns on first insert (would NPE).
      val es = exps.map(e => stripAutoUnbox(visitExp(e, env0, subst)))
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.Stm(es, e, t, subst(eff), loc)

    case TypedAst.Expr.Discard(exp, eff, loc) =>
      // Strip auto-unboxing: same reason as Stm above — discarded results must not be unboxed.
      val e = stripAutoUnbox(visitExp(exp, env0, subst))
      MonoAst.Expr.Discard(e, subst(eff), loc)

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
    case TypedAst.Expr.InstanceOf(exp, clazz, loc) =>
      // Primitives never satisfy an instanceof check: evaluate for side effects and return false.
      val e = visitExp(exp, env0, subst)
      if (isPrimType(e.tpe)) {
        // If it's a primitive type, evaluate the expression but return false
        MonoAst.Expr.Stm(List(e), MonoAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc), Type.Bool, e.eff, loc)
      } else {
        // If it's a reference type, then do the instanceof check
        MonoAst.Expr.ApplyAtomic(AtomicOp.InstanceOf(ClassDescs.of(clazz)), List(e), Type.Bool, e.eff, loc)
      }

    case TypedAst.Expr.CheckedCast(_, exp, tpe, eff, loc) =>
      // Note: We do *NOT* erase checked (i.e. safe) casts.
      // In Java, `String` is a subtype of `Object`, but the Flix IR makes this upcast _explicit_.
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      mkCast(e, t, subst(eff), loc)

    case TypedAst.Expr.UncheckedCast(exp, _, _, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      mkCast(e, t, subst(eff), loc)

    case TypedAst.Expr.Unsafe(exp, _, _, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      mkCast(e, t, subst(eff), loc)
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

    case TypedAst.Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      // Box primitive args to match the constructor's Object-typed parameters.
      val javaParamTypes = constructor.getParameterTypes
      val boxedArgs = ListOps.zip(es, javaParamTypes.toList).map { case (arg, paramType) => boxIfNecessary(arg, paramType) }
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeConstructor(JConstructor.of(constructor)), boxedArgs, t, subst(eff), loc)

    case TypedAst.Expr.InvokeSuperConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeSuperConstructor(JConstructor.of(constructor)), es, t, subst(eff), loc)

    case TypedAst.Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      mkJavaInvoke(method, List(e), es, t, subst(eff), loc, m => AtomicOp.InvokeMethod(JMethod.of(m)))

    case TypedAst.Expr.InvokeSuperMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      (lctx.sym, lctx.thisRef) match {
        case (Some(sym), Some(thisRef)) =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeSuperMethod(sym, JMethod.of(method)), thisRef :: es, t, subst(eff), loc)
        case _ =>
          throw InternalCompilerException("InvokeSuperMethod outside NewObject context", loc)
      }

    case TypedAst.Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      val t = visitType(tpe, subst)
      mkJavaInvoke(method, Nil, es, t, subst(eff), loc, m => AtomicOp.InvokeStaticMethod(JMethod.of(m)))

    case TypedAst.Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.GetField(JField.of(field)), List(e), t, subst(eff), loc)

    case TypedAst.Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.PutField(JField.of(field)), List(e1, e2), t, subst(eff), loc)

    case TypedAst.Expr.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.GetStaticField(JField.of(field)), List.empty, t, subst(eff), loc)

    case TypedAst.Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val t = visitType(tpe, subst)
      MonoAst.Expr.ApplyAtomic(AtomicOp.PutStaticField(JField.of(field)), List(e), t, subst(eff), loc)

    case TypedAst.Expr.NewObject(sym, clazz, tpe, eff, constructors, methods, loc) =>
      // Mint a fresh anonymous class symbol for each specialization. Otherwise distinct
      // specializations of an enclosing generic def (e.g. `mk[String]` and `mk[Int32]`)
      // would reuse the same anonymous class name and collide, so one specialization would
      // run with the other's generated class.
      val freshSym = Symbol.mkFreshAnonClassSym(sym.loc)
      val cs = constructors.map {
        case TypedAst.JvmConstructor(cExp, cRetTpe, cEff, cLoc) =>
          MonoAst.JvmConstructor(visitExp(cExp, env0, subst), visitType(cRetTpe, subst), subst(cEff), cLoc)
      }
      val ms = methods.map {
        case TypedAst.JvmMethod(mAnn, mIdent, mFparams0, mExp, mRetTpe, mEff, mLoc) =>
          val (mFparams, env1) = specializeFormalParams(mFparams0, subst)
          val fs = mFparams.map(lowerFormalParam).map(Specialize.rewriteFormalParam)
          val thisParam = fs.head
          val thisRef = MonoAst.Expr.Var(thisParam.sym, thisParam.tpe, loc)
          implicit val lctx: LocalContext = LocalContext(Some(freshSym), Some(thisRef))
          val e0 = visitExp(mExp, env0 ++ env1, subst)
          // If this overrides a Java method whose erased return type is a reference (e.g. `Object`
          // for a generic interface method) but the Flix result is primitive, box it to match the
          // erased signature. This mirrors the boxing applied to generic Java method calls (see
          // `boxIfNecessary` in `mkJavaInvoke`), and the call site unboxes the result symmetrically.
          val overridden = overriddenJavaMethod(clazz, mIdent.name, fs.tail.length)
          val e = overridden match {
            case Some(m) => boxIfNecessary(e0, m.getReturnType)
            case None => e0
          }
          MonoAst.JvmMethod(mAnn, mIdent, fs, e, e.tpe, subst(mEff), overridden.map(JMethod.of), mLoc)
      }
      val t = visitType(tpe, subst)
      MonoAst.Expr.NewObject(freshSym, JClass.of(clazz), t, subst(eff), cs, ms, loc)

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
    case TypedAst.Expr.FixpointConstraintSet(cs, _, loc) =>
      lowerConstraintSet(cs, loc, env0, subst)

    case TypedAst.Expr.FixpointLambda(pparams, exp, _, eff, loc) =>
      val resultType = Types.Fixpoint.Ast.Datalog.Datalog
      val defn = lookupSym(Defs.Fixpoint.Solver.Rename, resultType)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.Fixpoint.Ast.Shared.PredSym, loc)
      val argExps = predExps :: visitExp(exp, env0, subst) :: Nil
      MonoAst.Expr.ApplyDef(defn, argExps, Types.Fixpoint.Solver.RenameType, resultType, subst(eff), loc)

    case TypedAst.Expr.FixpointMerge(exp1, exp2, _, eff, loc) =>
      val resultType = Types.Fixpoint.Ast.Datalog.Datalog
      val defn = lookupSym(Defs.Fixpoint.Solver.Union, resultType)
      val argExps = visitExp(exp1, env0, subst) :: visitExp(exp2, env0, subst) :: Nil
      MonoAst.Expr.ApplyDef(defn, argExps, Types.Fixpoint.Solver.MergeType, resultType, subst(eff), loc)

    case TypedAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tpe0, eff, loc) =>
      lowerQueryWithProvenance(exps, select, withh, subst(tpe0), subst(eff), loc, env0, subst)

    case TypedAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, _, _, pred, tpe, eff, loc) =>
      lowerQueryWithSelect(exps, queryExp, selects.length, pred, subst(tpe), subst(eff), loc, env0, subst)

    case TypedAst.Expr.FixpointSolveWithProject(exps, optPreds, mode, _, eff, loc) =>
      lowerSolveWithProject(exps, optPreds, mode, subst(eff), loc, env0, subst)

    case TypedAst.Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
      lowerInjectInto(exps, predsAndArities, loc, env0, subst)

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
    case TypedAst.CatchRule(bnd, clazz0, exp, _) =>
      val freshSym = Symbol.freshVarSym(bnd.sym)
      val env1 = env0 + (bnd.sym -> freshSym)
      val e = visitExp(exp, env1, subst)
      MonoAst.CatchRule(freshSym, ClassDescs.of(clazz0), e)
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
  private def javaBoxMethod(tpe: Type): JMethod = {
    import java.lang.constant.ConstantDescs.*
    def valueOf(box: ClassDesc, prim: ClassDesc): JMethod =
      JMethod(box, "valueOf", MethodTypeDesc.of(box, prim), isInterface = false)
    tpe match {
      case Type.Bool => valueOf(CD_Boolean, CD_boolean)
      case Type.Char => valueOf(CD_Character, CD_char)
      case Type.Int8 => valueOf(CD_Byte, CD_byte)
      case Type.Int16 => valueOf(CD_Short, CD_short)
      case Type.Int32 => valueOf(CD_Integer, CD_int)
      case Type.Int64 => valueOf(CD_Long, CD_long)
      case Type.Float32 => valueOf(CD_Float, CD_float)
      case Type.Float64 => valueOf(CD_Double, CD_double)
      case _ => throw InternalCompilerException(s"Unexpected non-primitive type '$tpe'", tpe.loc)
    }
  }

  /**
    * Returns the unboxing method (e.g., `intValue`) for a Flix primitive type.
    * This is the same mechanism javac uses to implement auto-unboxing.
    */
  private def javaUnboxMethod(tpe: Type): JMethod = {
    import java.lang.constant.ConstantDescs.*
    def unbox(box: ClassDesc, name: String, prim: ClassDesc): JMethod =
      JMethod(box, name, MethodTypeDesc.of(prim), isInterface = false)
    tpe match {
      case Type.Bool => unbox(CD_Boolean, "booleanValue", CD_boolean)
      case Type.Char => unbox(CD_Character, "charValue", CD_char)
      case Type.Int8 => unbox(CD_Byte, "byteValue", CD_byte)
      case Type.Int16 => unbox(CD_Short, "shortValue", CD_short)
      case Type.Int32 => unbox(CD_Integer, "intValue", CD_int)
      case Type.Int64 => unbox(CD_Long, "longValue", CD_long)
      case Type.Float32 => unbox(CD_Float, "floatValue", CD_float)
      case Type.Float64 => unbox(CD_Double, "doubleValue", CD_double)
      case _ => throw InternalCompilerException(s"Unexpected non-primitive type '$tpe'", tpe.loc)
    }
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

  /**
    * Returns the Java method on `clazz` matching `name` and `arity` (excluding the receiver), if any.
    *
    * The resolved method's erased signature is carried on the [[MonoAst.JvmMethod]] so the
    * backend can emit matching descriptors without reflection.
    */
  private def overriddenJavaMethod(clazz: Class[?], name: String, arity: Int): Option[java.lang.reflect.Method] =
    JvmUtils.getOverridableInstanceMethods(clazz).collectFirst {
      case m if m.getName == name && m.getParameterCount == arity => m
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
  private def isAutoUnboxMethod(method: JMethod): Boolean = {
    method.descriptor.parameterCount() == 0 && (method.name match {
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

  /*
   * Methods for renaming
   */

  /**
    * Renames the given expression `exp0` per `env`.
    */
  private def renameExp(exp0: MonoAst.Expr, env: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Expr = exp0 match {
    case MonoAst.Expr.Cst(_, _, _) => exp0

    case MonoAst.Expr.Var(sym, tpe, loc) =>
      val s = env.getOrElse(sym, sym)
      MonoAst.Expr.Var(s, tpe, loc)

    case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = renameFormalParam(fparam, env)
      val e = renameExp(exp, env)
      MonoAst.Expr.Lambda(p, e, tpe, loc)

    case MonoAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = renameExp(exp1, env)
      val e2 = renameExp(exp2, env)
      MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(renameExp(_, env))
      MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(renameExp(_, env))
      MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case MonoAst.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(renameExp(_, env))
      MonoAst.Expr.ApplyOp(sym, es, tpe, eff, loc)

    case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(renameExp(_, env))
      MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case MonoAst.Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      val s = env.getOrElse(sym, sym)
      val e1 = renameExp(exp1, env)
      val e2 = renameExp(exp2, env)
      MonoAst.Expr.Let(s, e1, e2, tpe, eff, occur, loc)

    case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      val s = env.getOrElse(sym, sym)
      val fps = fparams.map(renameFormalParam(_, env))
      val e1 = renameExp(exp1, env)
      val e2 = renameExp(exp2, env)
      MonoAst.Expr.LocalDef(s, fps, e1, e2, tpe, eff, occur, loc)

    case MonoAst.Expr.Region(sym, regionVar, exp, tpe, eff, loc) =>
      val s = env.getOrElse(sym, sym)
      val e = renameExp(exp, env)
      MonoAst.Expr.Region(s, regionVar, e, tpe, eff, loc)

    case MonoAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = renameExp(exp1, env)
      val e2 = renameExp(exp2, env)
      val e3 = renameExp(exp3, env)
      MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case MonoAst.Expr.Stm(exps, exp, tpe, eff, loc) =>
      val es = exps.map(renameExp(_, env))
      val e = renameExp(exp, env)
      MonoAst.Expr.Stm(es, e, tpe, eff, loc)

    case MonoAst.Expr.Discard(exp, eff, loc) =>
      val e = renameExp(exp, env)
      MonoAst.Expr.Discard(e, eff, loc)

    case MonoAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = renameExp(exp, env)
      val rs = rules.map {
        case MonoAst.MatchRule(pat, guard, exp1) =>
          val p = renamePattern(pat, env)
          val g = guard.map(renameExp(_, env))
          val e1 = renameExp(exp1, env)
          MonoAst.MatchRule(p, g, e1)
      }
      MonoAst.Expr.Match(e, rs, tpe, eff, loc)

    case MonoAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = renameExp(exp, env)
      val rs = rules.map {
        case MonoAst.ExtMatchRule(pat, exp1, loc1) =>
          val p = renameExtPattern(pat, env)
          val e1 = renameExp(exp1, env)
          MonoAst.ExtMatchRule(p, e1, loc1)
      }
      MonoAst.Expr.ExtMatch(e, rs, tpe, eff, loc)

    case MonoAst.Expr.Cast(exp, tpe, eff, loc) =>
      val e = renameExp(exp, env)
      MonoAst.Expr.Cast(e, tpe, eff, loc)

    case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = renameExp(exp, env)
      val rs = rules.map {
        case MonoAst.CatchRule(sym, clazz, exp1) =>
          val s = env.getOrElse(sym, sym)
          val e1 = renameExp(exp1, env)
          MonoAst.CatchRule(s, clazz, e1)
      }
      MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case MonoAst.Expr.RunWith(exp, effSymUse, rules, tpe, eff, loc) =>
      val e = renameExp(exp, env)
      val rs = rules.map {
        case MonoAst.HandlerRule(opSymUse, fparams, hexp) =>
          val fps = fparams.map(renameFormalParam(_, env))
          val he = renameExp(hexp, env)
          MonoAst.HandlerRule(opSymUse, fps, he)
      }
      MonoAst.Expr.RunWith(e, effSymUse, rs, tpe, eff, loc)

    case MonoAst.Expr.NewObject(_, _, _, _, _, _, _) => exp0

  }

  /**
    * Renames the given formal param `fparam0` per `env`.
    */
  private def renameFormalParam(fparam0: MonoAst.FormalParam, env: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.FormalParam = fparam0 match {
    case MonoAst.FormalParam(sym, tpe, occur, loc) =>
      val s = env.getOrElse(sym, sym)
      MonoAst.FormalParam(s, tpe, occur, loc)
  }

  /**
    * Renames the given pattern `pattern0` per `env`.
    */
  private def renamePattern(pattern0: MonoAst.Pattern, env: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Pattern = pattern0 match {
    case MonoAst.Pattern.Wild(tpe, loc) =>
      MonoAst.Pattern.Wild(tpe, loc)

    case MonoAst.Pattern.Var(sym, tpe, occur, loc) =>
      val s = env.getOrElse(sym, sym)
      MonoAst.Pattern.Var(s, tpe, occur, loc)

    case MonoAst.Pattern.Cst(cst, tpe, loc) =>
      MonoAst.Pattern.Cst(cst, tpe, loc)

    case MonoAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(renamePattern(_, env))
      MonoAst.Pattern.Tag(symUse, ps, tpe, loc)

    case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
      val ps = pats.map(renamePattern(_, env))
      MonoAst.Pattern.Tuple(ps, tpe, loc)

    case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map(renameRecordLabelPattern(_, env))
      val p = renamePattern(pat, env)
      MonoAst.Pattern.Record(ps, p, tpe, loc)
  }

  /**
    * Renames the given record label pattern `pattern0` per `env`.
    */
  private def renameRecordLabelPattern(pattern0: MonoAst.Pattern.Record.RecordLabelPattern, env: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Pattern.Record.RecordLabelPattern = pattern0 match {
    case MonoAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val p = renamePattern(pat, env)
      MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
  }

  /**
    * Renames the given ext pattern `pattern0` per `env`.
    */
  private def renameExtPattern(pattern0: MonoAst.ExtPattern, env: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.ExtPattern = pattern0 match {
    case MonoAst.ExtPattern.Default(loc) =>
      MonoAst.ExtPattern.Default(loc)

    case MonoAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(renameVarOrWild(_, env))
      MonoAst.ExtPattern.Tag(label, ps, loc)
  }

  /**
    * Renames the given ext tag pattern `pattern0` per `env`.
    */
  private def renameVarOrWild(pattern0: MonoAst.ExtTagPattern, env: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.ExtTagPattern = pattern0 match {
    case MonoAst.ExtTagPattern.Wild(tpe, loc) =>
      MonoAst.ExtTagPattern.Wild(tpe, loc)

    case MonoAst.ExtTagPattern.Var(sym, tpe, occur, loc) =>
      val s = env.getOrElse(sym, sym)
      MonoAst.ExtTagPattern.Var(s, tpe, occur, loc)

    case MonoAst.ExtTagPattern.Unit(tpe, loc) =>
      MonoAst.ExtTagPattern.Unit(tpe, loc)
  }

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
    * Returns an expression merging `exps` using `Defs.Fixpoint.Solver.Merge`.
    */
  private def mergeExps(exps: List[MonoAst.Expr], loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr =
    exps.reduceRight {
      (exp, acc) =>
        val resultType = Types.Fixpoint.Ast.Datalog.Datalog
        val defn = lookupSym(Defs.Fixpoint.Solver.Union, resultType)
        val argExps = exp :: acc :: Nil
        val groundArrowTpe = Types.Fixpoint.Solver.MergeType
        MonoAst.Expr.ApplyDef(defn, argExps, groundArrowTpe, resultType, exp.eff, loc)
    }

  /**
    * Returns a new `Datalog` from `datalogExp` containing only facts from the predicate given by the `PredSym` `predSymExp`
    * using `Defs.Fixpoint.Solver.Filter`.
    */
  private def projectSym(predSymExp: MonoAst.Expr, datalogExp: MonoAst.Expr, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val resultType = Types.Fixpoint.Ast.Datalog.Datalog
    val defn = lookupSym(Defs.Fixpoint.Solver.ProjectSym, resultType)
    val argExps = predSymExp :: datalogExp :: Nil
    val groundArrowTpe = Types.Fixpoint.Solver.FilterType
    MonoAst.Expr.ApplyDef(defn, argExps, groundArrowTpe, resultType, datalogExp.eff, loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: MonoAst.Expr, argTypes: Nel[Type], resultType: Type)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    //
    // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Boxed`.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> `resultType`.
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Boxed.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Fixpoint.Boxed), Types.Fixpoint.Boxed, exp0.loc)

    // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> Boxed).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Compute the liftXb symbol.
    val sym = lookupSym(Defs.Fixpoint.Boxable.Lift(argTypes.length), liftType)

    // Construct a call to the liftX function.
    MonoAst.Expr.ApplyDef(sym, List(exp0), Specialize.rewriteEnumStructType(liftType), returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    */
  private def liftXb(exp0: MonoAst.Expr, argTypes: Nel[Type])(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    //
    // The liftX family of functions are of the form: a -> b -> c -> Bool and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Bool.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> Bool.
    val argType = Type.mkPureCurriedArrow(argTypes, Type.Bool, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Bool.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Fixpoint.Boxed), Type.Bool, exp0.loc)

    // The type of the overall liftXb function, i.e. (a -> b -> c -> Bool) -> (Boxed -> Boxed -> Boxed -> Bool).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Compute the liftXb symbol.
    val sym = lookupSym(Defs.Fixpoint.Boxable.LiftB(argTypes.length), liftType)

    // Construct a call to the liftXb function.
    MonoAst.Expr.ApplyDef(sym, List(exp0), Specialize.rewriteEnumStructType(liftType), returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
    */
  private def liftXY(outVars: List[Symbol.VarSym], exp0: MonoAst.Expr, argTypes: List[Type], resultType: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    //
    // The liftXY family of functions are of the form: i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)] and
    // returns a function of the form Vector[Boxed] -> Vector[Vector[Boxed]].
    // That is, the function accepts a *curried* function and an uncurried function that takes
    // its input as a boxed Vector and return its output as a vector of vectors.
    //

    // The type of the function argument, i.e. i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)].
    // With no in variables the `lift0XY` functions take the vector directly, rather than a function.
    val argType = argTypes match {
      case Nil => resultType
      case t :: ts => Type.mkPureCurriedArrow(Nel(t, ts), resultType, loc)
    }

    // The type of the returned function, i.e. Vector[Boxed] -> Vector[Vector[Boxed]].
    val returnType = Type.mkPureArrow(Type.mkVector(Types.Fixpoint.Boxed, loc), Type.mkVector(Type.mkVector(Types.Fixpoint.Boxed, loc), loc), loc)

    // The type of the overall liftXY function, i.e. (i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)]) -> (Vector[Boxed] -> Vector[Vector[Boxed]]).
    val liftType = Type.mkPureArrow(argType, returnType, loc)

    // Compute the number of bound ("output") and free ("input") variables.
    val numberOfInVars = argTypes.length
    val numberOfOutVars = outVars.length

    // Compute the liftXY symbol.
    // For example, lift3X2 is a function from three arguments to a Vector of pairs.
    val sym = lookupSym(Defs.Fixpoint.Boxable.LiftXM(numberOfInVars, numberOfOutVars), liftType)

    // Construct a call to the liftXY function.
    MonoAst.Expr.ApplyDef(sym, List(exp0), Specialize.rewriteEnumStructType(liftType), returnType, Type.Pure, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.Fixpoint.Ast.Datalog.HeadTerm, "Var", innerExp, Types.Fixpoint.Ast.Datalog.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: MonoAst.Expr)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    mkTag(Enums.Fixpoint.Ast.Datalog.HeadTerm, "Lit", List(exp), Types.Fixpoint.Ast.Datalog.HeadTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    mkTag(Enums.Fixpoint.Ast.Datalog.BodyTerm, "Wild", Nil, Types.Fixpoint.Ast.Datalog.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.Fixpoint.Ast.Datalog.BodyTerm, "Var", innerExp, Types.Fixpoint.Ast.Datalog.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: MonoAst.Expr)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    mkTag(Enums.Fixpoint.Ast.Datalog.BodyTerm, "Lit", List(exp), Types.Fixpoint.Ast.Datalog.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val nameExp = MonoAst.Expr.Cst(Constant.Str(sym.text), Type.Str, sym.loc)
    mkTag(Enums.Fixpoint.Ast.Datalog.VarSym, "VarSym", List(nameExp), Types.Fixpoint.Ast.Datalog.VarSym, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = d match {
    case Denotation.Relational =>
      mkTag(Enums.Fixpoint.Ast.Shared.Denotation, "Relational", Nil, Types.Fixpoint.Ast.Shared.Denotation, loc)

    case Denotation.Latticenal =>
      tpeOpt match {
        case None => throw InternalCompilerException("Unexpected nullary lattice predicate.", loc)
        case Some(tpe) =>
          val innerType = lowerType(tpe)
          // The type `Denotation[tpe]`.
          val unboxedDenotationType = Type.mkEnum(Enums.Fixpoint.Ast.Shared.Denotation, innerType :: Nil, loc)

          // The type `Denotation[Boxed]`.
          val boxedDenotationType = Types.Fixpoint.Ast.Shared.Denotation

          val latticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)
          val latticeSym: Symbol.DefnSym = lookupSym(Symbol.mkDefnSym(s"Fixpoint${Symbols.fixpointVersion}.Ast.Shared.lattice"), latticeType)

          val boxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)
          val boxSym: Symbol.DefnSym = lookupSym(Symbol.mkDefnSym(s"Fixpoint${Symbols.fixpointVersion}.Ast.Shared.box"), boxType)

          val innerApply = MonoAst.Expr.ApplyDef(latticeSym, List(MonoAst.Expr.Cst(Constant.Unit, Type.Unit, loc)), Specialize.rewriteEnumStructType(latticeType), Specialize.rewriteEnumStructType(unboxedDenotationType), Type.Pure, loc)
          MonoAst.Expr.ApplyDef(boxSym, List(innerApply), Specialize.rewriteEnumStructType(boxType), Specialize.rewriteEnumStructType(boxedDenotationType), Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = p match {
    case Polarity.Positive =>
      mkTag(Enums.Fixpoint.Ast.Datalog.Polarity, "Positive", Nil, Types.Fixpoint.Ast.Datalog.Polarity, loc)

    case Polarity.Negative =>
      mkTag(Enums.Fixpoint.Ast.Datalog.Polarity, "Negative", Nil, Types.Fixpoint.Ast.Datalog.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Fixity` from the given fixity `f`.
    */
  private def mkFixity(f: Fixity, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = f match {
    case Fixity.Loose =>
      mkTag(Enums.Fixpoint.Ast.Datalog.Fixity, "Loose", Nil, Types.Fixpoint.Ast.Datalog.Fixity, loc)

    case Fixity.Fixed =>
      mkTag(Enums.Fixpoint.Ast.Datalog.Fixity, "Fixed", Nil, Types.Fixpoint.Ast.Datalog.Fixity, loc)
  }

  /**
    * Freshens every symbol in `vars`, renames them in `exp`, and curries the result into a
    * lambda per var (outermost var first). Shared by [[mkGuard]]/[[mkFunctional]]/[[mkAppTerm]],
    * which each then lift the result to operate on boxed values.
    */
  private def curryFreshLambda(vars: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, loc: SourceLocation)(implicit tables: SpecializationTables, flix: Flix): MonoAst.Expr = {
    // Introduce a fresh variable for each free variable.
    val freshVars = vars.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }
    // Rename every symbol in `exp` for its fresh equivalent.
    val freshExp = renameExp(exp, freshVars)
    // Curry `freshExp` in a lambda expression for each free variable.
    vars.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val rewrittenTpe = Specialize.rewriteEnumStructType(tpe)
        val fparam = MonoAst.FormalParam(freshSym, rewrittenTpe, Occur.Unknown, loc)
        val lambdaType = Type.mkPureArrow(rewrittenTpe, acc.tpe, loc)
        MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.GuardX`. At most 5 free variables are supported.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      val sym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, loc)(RegionScope.Top, flix)
      // Construct a lambda that takes the unit argument.
      val fparam = MonoAst.FormalParam(sym, Type.Unit, Occur.Unknown, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = MonoAst.Expr.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.Fixpoint.Ast.Datalog.BodyPredicate, s"Guard0", List(lambdaExp), Types.Fixpoint.Ast.Datalog.BodyPredicate, loc)
    }

    val lambdaExp = curryFreshLambda(fvs, exp, loc)

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXb(lambdaExp, Nel.unsafeFrom(fvs.map(_._2)))

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.Fixpoint.Ast.Datalog.BodyPredicate, s"Guard$arity", innerExp, Types.Fixpoint.Ast.Datalog.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.Functional`.
    */
  private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, rawResultTpe: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    if (inVars.length > 5) {
      throw InternalCompilerException("Does not support more than 5 in variables.", loc)
    }
    if (outVars.isEmpty) {
      throw InternalCompilerException("Requires at least one out variable.", loc)
    }
    if (outVars.length > 5) {
      throw InternalCompilerException("Does not support more than 5 out variables.", loc)
    }

    val lambdaExp = curryFreshLambda(inVars, exp, loc)

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXY(outVars, lambdaExp, inVars.map(_._2), rawResultTpe, exp.loc)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val boundVarVector = mkVector(outVars.map(mkVarSym), Types.Fixpoint.Ast.Datalog.VarSym, loc)
    val freeVarVector = mkVector(inVars.map(kv => mkVarSym(kv._1)), Types.Fixpoint.Ast.Datalog.VarSym, loc)
    val innerExp = List(boundVarVector, liftedExp, freeVarVector)
    mkTag(Enums.Fixpoint.Ast.Datalog.BodyPredicate, s"Functional", innerExp, Types.Fixpoint.Ast.Datalog.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.HeadTerm.AppX`.
    */
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, rawResultTpe: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    val lambdaExp = curryFreshLambda(fvs, exp, loc)

    // Lift the lambda expression to operate on boxed values.
    // `fvs` is non-empty since the caller falls back to a literal head term when there are no free variables.
    val liftedExp = liftX(lambdaExp, Nel.unsafeFrom(fvs.map(_._2)), rawResultTpe)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.Fixpoint.Ast.Datalog.HeadTerm, s"App$arity", innerExp, Types.Fixpoint.Ast.Datalog.HeadTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = MonoAst.Expr.Cst(Constant.Str(sym), Type.Str, loc)
      val idExp = MonoAst.Expr.Cst(Constant.Int64(0), Type.Int64, loc)
      val inner = List(nameExp, idExp)
      mkTag(Enums.Fixpoint.Ast.Shared.PredSym, "PredSym", inner, Types.Fixpoint.Ast.Shared.PredSym, loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: MonoAst.Expr, rawTpe: Type)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(rawTpe, Types.Fixpoint.Boxed, loc)
    MonoAst.Expr.ApplyDef(lookupSym(Defs.Fixpoint.Boxable.Box, tpe), List(exp), Specialize.rewriteEnumStructType(tpe), Types.Fixpoint.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkVector(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation): MonoAst.Expr = {
    MonoAst.Expr.ApplyAtomic(AtomicOp.VectorLit, exps, Type.mkVector(elmType, loc), Type.Pure, loc)
  }

  /*
   * Datalog lowering
   */

  /**
    * Rewrites
    * {{{
    *     pquery e1, e2, e3 select Head(t1, ..., tn) with {W1, ..., Wm}
    * }}}
    * to
    * {{{
    *     provenanceOf(PredSym("Head"), Vector#{t1, ..., tn}, Vector#{W1, ..., Wm}, mkExtVar, e1 <+> e2 <+> e3)
    * }}}
    * where `mkExtVar` is the mapping this function builds from `PredSym` and terms to an
    * extensible variant.
    */
  private def lowerQueryWithProvenance(exps: List[TypedAst.Expr], select: Predicate.Head, withh: List[Name.Pred], tpe0: Type, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val tpe = lowerType(tpe0)
    val mergedExp = mergeExps(exps.map(visitExp(_, env0, subst)), loc)
    val (goalPredSym, goalTerms) = select match {
      case TypedAst.Predicate.Head.Atom(pred, _, terms, _, loc1) =>
        val boxedTerms = terms.map(t => box(visitExp(t, env0, subst), subst(t.tpe)))
        (mkPredSym(pred), mkVector(boxedTerms, Types.Fixpoint.Boxed, loc1))
    }
    val withPredSyms = mkVector(withh.map(mkPredSym), Types.Fixpoint.Ast.Shared.PredSym, loc)
    val extVarType = unwrapVectorType(tpe, loc)
    val preds = predicatesOfExtVar(extVarType, loc)
    val lambdaExp = mkExtVarLambda(preds, extVarType, loc)
    val argExps = goalPredSym :: goalTerms :: withPredSyms :: lambdaExp :: mergedExp :: Nil
    val groundArrowTpe = Types.Fixpoint.Solver.mkProvenanceOf(extVarType, loc)
    val defn = lookupSym(Defs.Fixpoint.Solver.ProvenanceOf, groundArrowTpe)
    MonoAst.Expr.ApplyDef(defn, argExps, Specialize.rewriteEnumStructType(groundArrowTpe), Specialize.rewriteEnumStructType(tpe), eff, loc)
  }

  /**
    * Rewrites
    * {{{
    *     query e_db, e_pr select (v1, v2) from P(v1, v2)
    * }}}
    * to
    * {{{
    *     facts2(PredSym("P"), solve(e_db <+> e_pr))
    * }}}
    */
  private def lowerQueryWithSelect(exps: List[TypedAst.Expr], queryExp: TypedAst.Expr, predArity: Int, pred: Name.Pred, tpe: Type, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val loweredExps = exps.map(visitExp(_, env0, subst))
    val loweredQueryExp = visitExp(queryExp, env0, subst)

    // Define the name and type of the appropriate factsX function in Solver.flix
    val defTpe = Type.mkPureUncurriedArrow(Nel.of(Types.Fixpoint.Ast.Shared.PredSym, Types.Fixpoint.Ast.Datalog.Datalog), tpe, loc)
    val sym = lookupSym(Defs.Fixpoint.Solver.Facts(predArity), defTpe)

    // Merge and solve exps
    val mergedExp = mergeExps(loweredQueryExp :: loweredExps, loc)
    val solveDefn = lookupSym(Defs.Fixpoint.Solver.RunSolver, Types.Fixpoint.Solver.SolveType)
    val solvedExp = MonoAst.Expr.ApplyDef(solveDefn, mergedExp :: Nil, Types.Fixpoint.Solver.SolveType, Types.Fixpoint.Ast.Datalog.Datalog, eff, loc)

    // Put everything together
    val argExps = mkPredSym(pred) :: solvedExp :: Nil
    MonoAst.Expr.ApplyDef(sym, argExps, Specialize.rewriteEnumStructType(defTpe), Specialize.rewriteEnumStructType(tpe), eff, loc)

  }

  /**
    * Rewrites
    * {{{
    *     solve e₁, e₂, e₃ project P₁, P₂, P₃
    * }}}
    * to
    * {{{
    *     let tmp% = solve e₁ <+> e₂ <+> e₃;
    *     merge (project P₁ tmp%, project P₂ tmp%, project P₃ tmp%)
    * }}}
    */
  private def lowerSolveWithProject(exps0: List[TypedAst.Expr], optPreds: Option[List[Name.Pred]], mode: SolveMode, eff: Type, loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val defn = mode match {
      case SolveMode.Default => lookupSym(Defs.Fixpoint.Solver.RunSolver, Types.Fixpoint.Ast.Datalog.Datalog)
      case SolveMode.WithProvenance => lookupSym(Defs.Fixpoint.Solver.RunSolverWithProvenance, Types.Fixpoint.Ast.Datalog.Datalog)
    }
    val exps = exps0.map(visitExp(_, env0, subst))
    val mergedExp = mergeExps(exps, loc)
    val argExps = mergedExp :: Nil
    val solvedExp = MonoAst.Expr.ApplyDef(defn, argExps, Types.Fixpoint.Solver.SolveType, Types.Fixpoint.Ast.Datalog.Datalog, eff, loc)
    val tmpVarSym = Symbol.freshVarSym("tmp%", BoundBy.Let, loc)(RegionScope.Top, flix)
    val letBodyExp = optPreds match {
      case Some(preds) =>
        mergeExps(preds.map(pred => {
          val varExp = MonoAst.Expr.Var(tmpVarSym, Types.Fixpoint.Ast.Datalog.Datalog, loc)
          projectSym(mkPredSym(pred), varExp, loc)
        }), loc)
      case None => MonoAst.Expr.Var(tmpVarSym, Types.Fixpoint.Ast.Datalog.Datalog, loc)
    }
    MonoAst.Expr.Let(tmpVarSym, solvedExp, letBodyExp, Types.Fixpoint.Ast.Datalog.Datalog, eff, Occur.Unknown, loc)

  }

  /**
    * Rewrites
    * {{{
    *     inject e1, e2 into P1/1, P2/2
    * }}}
    * to
    * {{{
    *     injectInto1(PredSym("P1"), e1) <+> injectInto2(PredSym("P2"), e2)
    * }}}
    */
  private def lowerInjectInto(exps: List[TypedAst.Expr], predsAndArities: List[PredicateAndArity], loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val loweredExps = ListOps.zip(exps, predsAndArities).map {
      case (exp, PredicateAndArity(pred, arity)) =>
        val expTpe = subst(exp.tpe)

        // The type of the function.
        val defTpe = Type.mkPureUncurriedArrow(Nel.of(Types.Fixpoint.Ast.Shared.PredSym, lowerType(expTpe)), Types.Fixpoint.Ast.Datalog.Datalog, loc)

        // Compute the symbol of the function.
        val sym = lookupSym(Defs.Fixpoint.Solver.InjectInto(arity), defTpe)

        // Put everything together.
        val argExps = mkPredSym(pred) :: visitExp(exp, env0, subst) :: Nil
        MonoAst.Expr.ApplyDef(sym, argExps, Specialize.rewriteEnumStructType(defTpe), Types.Fixpoint.Ast.Datalog.Datalog, subst(exp.eff), loc)
    }
    mergeExps(loweredExps, loc)

  }

  /*
   * Methods for lowering provenance datalog expressions.
   */

  /**
    * Returns `t` from the Flix type `Vector[t]`.
    */
  private def unwrapVectorType(tpe: Type, loc: SourceLocation): Type = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), extType, _) => extType
    case t => throw InternalCompilerException(
      s"Expected Type.Apply(Type.Cst(TypeConstructor.Vector, _), _, _), but got $t",
      loc
    )
  }

  /**
    * Returns the pairs consisting of predicates and their term types from the extensible variant
    * type `tpe`.
    */
  private def predicatesOfExtVar(tpe: Type, loc: SourceLocation): List[(Name.Pred, List[Type])] = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.Extensible, _), tpe1, loc1) =>
      predicatesOfSchemaRow(tpe1, loc1)
    case t => throw InternalCompilerException(
      s"Expected Type.Apply(Type.Cst(TypeConstructor.Extensible, _), _, _), but got $t",
      loc
    )
  }

  /**
    * Returns the pairs consisting of predicates and their term types from the SchemaRow `row`.
    */
  private def predicatesOfSchemaRow(row: Type, loc: SourceLocation): List[(Name.Pred, List[Type])] = row match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), rel, loc2), tpe2, loc1) =>
      (pred, termTypesOfRelation(rel, loc2)) :: predicatesOfSchemaRow(tpe2, loc1)
    case Type.Var(_, _) => Nil
    case Type.SchemaRowEmpty => Nil
    case t => throw InternalCompilerException(s"Got unexpected $t", loc)
  }

  /**
    * Returns the types constituting a `Type.Relation`.
    */
  private def termTypesOfRelation(rel: Type, loc: SourceLocation): List[Type] = {
    def flattenApply(rel0: Type, loc0: SourceLocation): List[Type] = rel0 match {
      case Type.Cst(TypeConstructor.Relation(_), _) => Nil
      case Type.Apply(rest, t, loc1) => t :: flattenApply(rest, loc1)
      case _ if rel0.typeConstructor.contains(TypeConstructor.AnyType) => Nil
      // The type of the relation is undetermined, i.e. it is a free type variable that has been replaced by AnyType.
      // Since we have an AnyType we are free to treat it however we want. Here we decide to treat the relation as being nullary.
      case t => throw InternalCompilerException(s"Expected Type.Apply(_, _, _), but got $t", loc0)
    }

    flattenApply(rel, loc).reverse
  }

  /**
    * Returns the `MonoAst` lambda expression
    * {{{
    *   predSym: PredSym -> terms: Vector[Boxed] -> match predSym {
    *     case PredSym.PredSym(name, _) => match name {
    *       case "P1" => xvar P1(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       case "P2" => xvar P2(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       ...
    *     }
    *   }
    * }}}
    * where `P1, P2, ...` are in `preds` with their respective term types.
    */
  private def mkExtVarLambda(preds: List[(Name.Pred, List[Type])], tpe: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val predSymVar = Symbol.freshVarSym("predSym", BoundBy.FormalParam, loc)(RegionScope.Top, flix)
    val termsVar = Symbol.freshVarSym("terms", BoundBy.FormalParam, loc)(RegionScope.Top, flix)
    mkLambdaExp(predSymVar, Types.Fixpoint.Ast.Shared.PredSym,
      mkLambdaExp(termsVar, Types.Fixpoint.VectorOfBoxed,
        mkExtVarBody(preds, predSymVar, termsVar, tpe, loc),
        tpe, Type.Pure, loc
      ),
      Type.mkPureArrow(Types.Fixpoint.VectorOfBoxed, tpe, loc), Type.Pure, loc
    )
  }

  /**
    * Returns the `MonoAst` lambda expression
    * {{{
    *   paramName -> exp
    * }}}
    * where `"paramName" == param.text` and `exp` has type `expType` and effect `eff`.
    */
  private def mkLambdaExp(param: Symbol.VarSym, paramTpe: Type, exp: MonoAst.Expr, expTpe: Type, eff: Type, loc: SourceLocation): MonoAst.Expr =
    MonoAst.Expr.Lambda(
      MonoAst.FormalParam(param, paramTpe, Occur.Unknown, loc),
      exp,
      Type.mkArrowWithEffect(paramTpe, eff, expTpe, loc),
      loc
    )

  /**
    * Returns the `MonoAst` match expression
    * {{{
    *   match predSym {
    *     case PredSym.PredSym(name, _) => match name {
    *       case "P1" => xvar P1(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       case "P2" => xvar P2(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    *       ...
    *     }
    *   }
    * }}}
    * where `P1, P2, ...` are in `preds` with their respective term types, `"predSym" == predSymVar.text`
    * and `"terms" == termsVar.text`.
    */
  private def mkExtVarBody(preds: List[(Name.Pred, List[Type])], predSymVar: Symbol.VarSym, termsVar: Symbol.VarSym, tpe: Type, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val nameVar = Symbol.freshVarSym(Name.Ident("name", loc), BoundBy.Pattern)(RegionScope.Top, flix)
    MonoAst.Expr.Match(
      exp = MonoAst.Expr.Var(predSymVar, Types.Fixpoint.Ast.Shared.PredSym, loc),
      rules = List(
        MonoAst.MatchRule(
          pat = MonoAst.Pattern.Tag(
            symUse = SymUse.CaseSymUse(findCaseSym(Enums.Fixpoint.Ast.Shared.PredSym, "PredSym"), loc),
            pats = List(
              MonoAst.Pattern.Var(nameVar, Type.Str, Occur.Unknown, loc),
              MonoAst.Pattern.Wild(Type.Int64, loc)
            ),
            tpe = Types.Fixpoint.Ast.Shared.PredSym, loc = loc
          ),
          guard = None,
          exp = MonoAst.Expr.Match(
            exp = MonoAst.Expr.Var(nameVar, Type.Str, loc),
            rules = preds.map {
              case (p, types) => mkProvenanceMatchRule(termsVar, tpe, p, types, loc)
            },
            tpe = tpe, eff = Type.Pure, loc = loc
          ),
        )
      ),
      tpe = tpe, eff = Type.Pure, loc
    )
  }

  /**
    * Returns the pattern match rule
    * {{{
    *   case "P" => xvar P(unbox(Vector.get(0, terms)), unbox(Vector.get(1, terms)), ...)
    * }}}
    * where `"P" == p.name`
    */
  private def mkProvenanceMatchRule(termsVar: Symbol.VarSym, tpe: Type, p: Name.Pred, types: List[Type], loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.MatchRule = {
    val termsExps = types.zipWithIndex.map {
      case (tpe1, i) => mkUnboxedTerm(termsVar, tpe1, i, loc)
    }
    MonoAst.MatchRule(
      pat = MonoAst.Pattern.Cst(Constant.Str(p.name), Type.Str, loc),
      guard = None,
      exp = MonoAst.Expr.ApplyAtomic(
        op = AtomicOp.ExtTag(Name.Label(p.name, loc)),
        exps = termsExps,
        tpe = tpe, eff = Type.Pure, loc = loc
      )
    )
  }

  /**
    * Returns the `MonoAst` expression
    * {{{
    *   unbox(Vector.get(i, terms))
    * }}}
    * where `"terms" == termsVar.text`.
    */
  private def mkUnboxedTerm(termsVar: Symbol.VarSym, tpe: Type, i: Int, loc: SourceLocation)(implicit tables: SpecializationTables, root: TypedAst.Root): MonoAst.Expr = {
    val outerGroundArrowTpe = Type.mkPureUncurriedArrow(Nel.of(Types.Fixpoint.Boxed), tpe, loc)
    val innerGroundArrowTpe = Type.mkPureUncurriedArrow(Nel.of(Type.Int32, Types.Fixpoint.VectorOfBoxed), Types.Fixpoint.Boxed, loc)
    MonoAst.Expr.ApplyDef(
      sym = lookupSym(Defs.Fixpoint.Boxable.Unbox, outerGroundArrowTpe),
      exps = List(
        MonoAst.Expr.ApplyDef(
          sym = lookupSym(Symbol.mkDefnSym(s"Vector.get"), innerGroundArrowTpe),
          exps = List(
            MonoAst.Expr.Cst(Constant.Int32(i), Type.Int32, loc),
            MonoAst.Expr.Var(termsVar, Types.Fixpoint.VectorOfBoxed, loc)
          ),
          itpe = innerGroundArrowTpe,
          tpe = Types.Fixpoint.Boxed, eff = Type.Pure, loc = loc
        )
      ),
      itpe = Specialize.rewriteEnumStructType(outerGroundArrowTpe),
      tpe = Specialize.rewriteEnumStructType(tpe), eff = Type.Pure, loc = loc
    )
  }

  /** Constructs a `Fixpoint/Ast/Datalog.Datalog` value from the Datalog constraints `cs`. */
  private def lowerConstraintSet(cs: List[TypedAst.Constraint], loc: SourceLocation, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val factExps = cs.filter(c => c.body.isEmpty).map(lowerConstraint(_, env0, subst))
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(lowerConstraint(_, env0, subst))

    val factListExp = mkVector(factExps, Types.Fixpoint.Ast.Datalog.Constraint, loc)
    val ruleListExp = mkVector(ruleExps, Types.Fixpoint.Ast.Datalog.Constraint, loc)

    val innerExp = List(factListExp, ruleListExp)
    mkTag(Enums.Fixpoint.Ast.Datalog.Datalog, "Datalog", innerExp, Types.Fixpoint.Ast.Datalog.Datalog, loc)
  }

  /**
    * Specializes and lowers the given constraint `c0`.
    */
  private def lowerConstraint(c0: TypedAst.Constraint, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = c0 match {
    case TypedAst.Constraint(cparams, head, body, loc) =>
      // Freshen the constraint params (the quantified vars) up front.
      val env = cparams.foldLeft(env0) {
        case (env1, TypedAst.ConstraintParam(bnd, _, _)) =>
          if (env1.contains(bnd.sym)) env1
          else { val freshSym = Symbol.freshVarSym(bnd.sym); env1 + (bnd.sym -> freshSym) }
      }
      val headExp = lowerHeadPred(cparams, head, env, subst)
      val bodyExp = mkVector(body.map(lowerBodyPred(cparams, _, env, subst)), Types.Fixpoint.Ast.Datalog.BodyPredicate, loc)
      val innerExp = List(headExp, bodyExp)
      mkTag(Enums.Fixpoint.Ast.Datalog.Constraint, "Constraint", innerExp, Types.Fixpoint.Ast.Datalog.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def lowerHeadPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Head, env: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = p0 match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(t => subst(t.tpe)), loc)
      val termsExp = mkVector(terms.map(lowerHeadTerm(cparams0, _, env, subst)), Types.Fixpoint.Ast.Datalog.HeadTerm, loc)
      val innerExp = List(predSymExp, denotationExp, termsExp)
      mkTag(Enums.Fixpoint.Ast.Datalog.HeadPredicate, "HeadAtom", innerExp, Types.Fixpoint.Ast.Datalog.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def lowerBodyPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Body, env: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = p0 match {
    case TypedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(t => subst(t.tpe)), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val fixityExp = mkFixity(fixity, loc)
      val termsExp = mkVector(terms.map(lowerBodyTerm(cparams0, _, env, subst)), Types.Fixpoint.Ast.Datalog.BodyTerm, loc)
      val innerExp = List(predSymExp, denotationExp, polarityExp, fixityExp, termsExp)
      mkTag(Enums.Fixpoint.Ast.Datalog.BodyPredicate, "BodyAtom", innerExp, Types.Fixpoint.Ast.Datalog.BodyPredicate, loc)

    case TypedAst.Predicate.Body.Functional(outVars0, exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val inVars = MonomorphHelpers.quantifiedVars(cparams0, exp0).map { case (sym, tpe) => (env(sym), subst(tpe)) }
      val exp = visitExp(exp0, env, subst)
      val outVars = outVars0.map(b => env(b.sym))
      mkFunctional(outVars, inVars, exp, subst(exp0.tpe), loc)

    case TypedAst.Predicate.Body.Guard(exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val quantifiedFreeVars = MonomorphHelpers.quantifiedVars(cparams0, exp0).map { case (sym, tpe) => (env(sym), subst(tpe)) }
      val exp = visitExp(exp0, env, subst)
      mkGuard(quantifiedFreeVars, exp, loc)

  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def lowerHeadTerm(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr, env: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    //
    // We need to consider four cases:
    //
    // Case 1.1: The expression is quantified variable. We translate it to a Var.
    // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case TypedAst.Expr.Var(sym, _, _) =>
        // Case 1: Variable term.
        if (MonomorphHelpers.isQuantifiedVar(sym, cparams0)) {
          // Case 1.1: Quantified variable.
          mkHeadTermVar(env(sym))
        } else {
          // Case 1.2: Lexically bound variable.
          mkHeadTermLit(box(visitExp(exp0, env, subst), subst(exp0.tpe)))
        }

      case _ =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val quantifiedFreeVars = MonomorphHelpers.quantifiedVars(cparams0, exp0)

        if (quantifiedFreeVars.isEmpty) {
          // Case 2: No quantified variables. The expression can be reduced to a value.
          mkHeadTermLit(box(visitExp(exp0, env, subst), subst(exp0.tpe)))
        } else {
          // Case 3: Quantified variables. The expression is translated to an application term.
          val fvs = quantifiedFreeVars.map { case (sym, tpe) => (env(sym), subst(tpe)) }
          mkAppTerm(fvs, visitExp(exp0, env, subst), subst(exp0.tpe), exp0.loc)
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def lowerBodyTerm(cparams0: List[TypedAst.ConstraintParam], pat0: TypedAst.Pattern, env: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, root: TypedAst.Root, flix: Flix): MonoAst.Expr = pat0 match {
    case TypedAst.Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case TypedAst.Pattern.Var(bnd, tpe, loc) =>
      if (MonomorphHelpers.isQuantifiedVar(bnd.sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(env(bnd.sym))
      } else {
        // Case 2: Lexically bound variable *expression*.
        val rawTpe = subst(tpe)
        mkBodyTermLit(box(MonoAst.Expr.Var(env(bnd.sym), visitTypeSubstituted(rawTpe), loc), rawTpe))
      }

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      val rawTpe = subst(tpe)
      mkBodyTermLit(box(MonoAst.Expr.Cst(cst, visitTypeSubstituted(rawTpe), loc), rawTpe))

    case TypedAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Error(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

  }

  /**
    * Lowers `sym` from a restrictable enum sym into a regular enum sym.
    */
  private[monomorph2] def lowerRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): Symbol.EnumSym =
    new Symbol.EnumSym(None, sym.namespace, sym.name, sym.loc)
}
