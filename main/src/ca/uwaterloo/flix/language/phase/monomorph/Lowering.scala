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

package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.{DefaultHandler, Predicate}
import ca.uwaterloo.flix.language.ast.MonoAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Denotation, Fixity, Mutability, Polarity, PredicateAndArity, Scope, SolveMode, SymUse, TypeSource}
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, Name, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.monomorph.Specialization.Context
import ca.uwaterloo.flix.language.phase.monomorph.Symbols.{Defs, Enums, Types}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.collection.{CofiniteSet, ListOps, Nel}

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint.Ast.Datalog` values (which are ordinary Flix values).
  * This allows the Datalog engine to be implemented as an ordinary Flix program.
  *
  * In addition to translating expressions, types must also be translated from
  * Schema types to enum types.
  *
  * Finally, values must be boxed using the Boxable.
  *
  * It also translates channels to the lowered types.
  */
object Lowering {

  /**
    * Lowers the given type `tpe0`.
    *
    * Replaces schema types with the Datalog enum type and channel-related types with the channel enum type.
    */
  private def lowerType(tpe0: Type): Type = tpe0.typeConstructor match {
    case Some(TypeConstructor.Schema) =>
      // We replace any Schema type, no matter the number of polymorphic type applications, with the erased Datalog type.
      Types.Datalog
    case _ => lowerTypeNonSchema(tpe0)
  }

  private def lowerTypeNonSchema(tpe0: Type): Type = tpe0 match {
    case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

    case Type.Var(_, _) => tpe0

    // Rewrite Sender[t] to Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe, _) =>
      val t = lowerType(tpe)
      mkChannelTpe(t, loc)

    // Rewrite Receiver[t] to Concurrent.Channel.Mpmc[t, IO]
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

    case Type.Alias(sym, args, t, loc) =>
      Type.Alias(sym, args.map(lowerType), lowerType(t), loc)

    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException("unexpected associated type", loc)

    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)

    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

  }

  /**
    * Lowers the given def `defn`.
    */
  protected[monomorph] def lowerDef(defn0: TypedAst.Def)(implicit ctx: Context, root: TypedAst.Root, flix: Flix): MonoAst.Def = {
    implicit val lctx: LocalContext = LocalContext.empty
    /*
     If the definition is an entry point it is wrapped with the required default handlers before the rest of lowering.
       For example:
          {{{ def myEntryPoint(): Unit \ Assert = e }}}
       Will be transformed into:
          {{{ def myEntryPoint(): Unit \ IO = Assert.runWithIO(_ -> e) }}}
     */
    val defn = if (TypedAstOps.isEntryPoint(defn0)) {
      wrapDefWithDefaultHandlers(defn0)
    } else {
      defn0
    }
    defn match {
      case TypedAst.Def(sym, spec0, exp, loc) =>
        val spec = lowerSpec(spec0)
        val e = lowerExp(exp)
        MonoAst.Def(sym, spec, e, loc)
    }
  }

  /**
    * Lowers the given enum `enum0`.
    */
  protected[monomorph] def lowerEnum(enum0: TypedAst.Enum): MonoAst.Enum = enum0 match {
    case TypedAst.Enum(doc, ann, mod, sym, tparams0, _, cases0, loc) =>
      val tparams = tparams0.map(lowerTypeParam)
      val cases = cases0.map {
        case (_, TypedAst.Case(caseSym, tpes0, _, caseLoc)) =>
          val tpes = tpes0.map(lowerType)
          (caseSym, MonoAst.Case(caseSym, tpes, caseLoc))
      }
      MonoAst.Enum(doc, ann, mod, sym, tparams, cases, loc)
  }

  /**
    * Lowers the given enum `enum0` from a restrictable enum into a regular enum.
    */
  protected[monomorph] def lowerRestrictableEnum(enum0: TypedAst.RestrictableEnum): MonoAst.Enum = enum0 match {
    case TypedAst.RestrictableEnum(doc, ann, mod, sym0, index0, tparams0, _, cases0, loc) =>
      // index is erased since related checking has concluded.
      // Restrictable tag is lowered into a regular tag
      val index = lowerTypeParam(index0)
      val tparams = tparams0.map(lowerTypeParam)
      val cases = cases0.map {
        case (_, TypedAst.RestrictableCase(caseSym0, tpes0, _, caseLoc)) =>
          val tpes = tpes0.map(lowerType)
          val caseSym = lowerRestrictableCaseSym(caseSym0)
          (caseSym, MonoAst.Case(caseSym, tpes, caseLoc))
      }
      val sym = lowerRestrictableEnumSym(sym0)
      MonoAst.Enum(doc, ann, mod, sym, index :: tparams, cases, loc)
  }

  /**
    * Lowers the given `effect`.
    */
  protected[monomorph] def lowerEffect(effect: TypedAst.Effect): MonoAst.Effect = effect match {
    case TypedAst.Effect(doc, ann, mod, sym, _, ops0, loc) =>
      // TODO EFFECT-TPARAMS use tparams
      val ops = ops0.map(lowerOp)
      MonoAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Lowers the given struct `struct0`.
    */
  protected[monomorph] def lowerStruct(struct0: TypedAst.Struct): MonoAst.Struct = struct0 match {
    case TypedAst.Struct(doc, ann, mod, sym, tparams0, _, fields0, loc) =>
      val tparams = tparams0.map(lowerTypeParam)
      val fields = fields0.map {
        case (fieldSym, field) => MonoAst.StructField(fieldSym, lowerType(field.tpe), loc)
      }
      MonoAst.Struct(doc, ann, mod, sym, tparams, fields.toList, loc)
  }

  /**
    * Lowers the given `op`.
    */
  private def lowerOp(op: TypedAst.Op): MonoAst.Op = op match {
    case TypedAst.Op(sym, spec0, loc) =>
      val spec = lowerSpec(spec0)
      MonoAst.Op(sym, spec, loc)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def lowerSpec(spec0: TypedAst.Spec): MonoAst.Spec = spec0 match {
    case TypedAst.Spec(doc, ann, mod, _, fparams0, declaredScheme, retTpe, eff, _, _) =>
      val fs = fparams0.map(lowerFormalParam)
      val fType = lowerType(declaredScheme.base)
      val rType = lowerType(retTpe)
      MonoAst.Spec(doc, ann, mod, fs, fType, rType, eff, DefContext.Unknown)
  }

  /**
    * Lowers `exp0` replacing all types with the lowered types and lowering channels and fixpoint to the primitives.
    */
  private def lowerExp(exp0: TypedAst.Expr)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case TypedAst.Expr.Cst(cst, tpe, loc) => MonoAst.Expr.Cst(cst, lowerType(tpe), loc)

    case TypedAst.Expr.Var(sym, tpe, loc) => MonoAst.Expr.Var(sym, lowerType(tpe), loc)

    case TypedAst.Expr.Hole(sym, _, tpe, eff, loc) =>
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, eff, loc)

    case TypedAst.Expr.HoleWithExp(_, _, tpe, _, loc) =>
      val sym = Symbol.freshHoleSym(loc)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.OpenAs(_, exp, _, _) =>
      lowerExp(exp) // TODO RESTR-VARS maybe add to monoAST

    case TypedAst.Expr.Use(_, _, exp, _) =>
      lowerExp(exp)

    case TypedAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = lowerFormalParam(fparam)
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.Lambda(p, e, t, loc)

    case TypedAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyClo(e1, e2, t, eff, loc)

    case TypedAst.Expr.ApplyDef(sym, exps, _, itpe, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val it = lowerType(itpe)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyDef(sym.sym, es, it, t, eff, loc)

    case TypedAst.Expr.ApplyLocalDef(bnd, exps, _, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyLocalDef(bnd.sym, es, t, eff, loc)

    case TypedAst.Expr.ApplyOp(bnd, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyOp(bnd.sym, es, t, eff, loc)

    case TypedAst.Expr.Unary(sop, exp, tpe, eff, loc) => sop match {
      case _: SemanticOp.ReflectOp =>
        throw InternalCompilerException("ReflectOp should have been resolved in Specialization", loc)
      case _ =>
        val e = lowerExp(exp)
        val t = lowerType(tpe)
        MonoAst.Expr.ApplyAtomic(AtomicOp.Unary(sop), List(e), t, eff, loc)
    }

    case TypedAst.Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.Let(bnd.sym, e1, e2, t, eff, Occur.Unknown, loc)

    case TypedAst.Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, loc) =>
      val fps = fparams.map(lowerFormalParam)
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.LocalDef(bnd.sym, fps, e1, e2, t, eff, Occur.Unknown, loc)

    case TypedAst.Expr.Region(bnd, regSym, exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.Region(bnd.sym, regSym, e, t, eff, loc)

    case TypedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val e3 = lowerExp(exp3)
      val t = lowerType(tpe)
      MonoAst.Expr.IfThenElse(e1, e2, e3, t, eff, loc)

    case TypedAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.Stm(e1, e2, t, eff, loc)

    case TypedAst.Expr.Discard(exp, eff, loc) =>
      val e = lowerExp(exp)
      MonoAst.Expr.Discard(e, eff, loc)

    case TypedAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      val rs = rules.map(visitMatchRule)
      MonoAst.Expr.Match(e, rs, t, eff, loc)

    case TypedAst.Expr.RestrictableChoose(_, exp, rules, tpe, eff, loc) =>
      // lower into an ordinary match
      val e = lowerExp(exp)
      val rs = rules.map(lowerRestrictableChooseRule)
      val t = lowerType(tpe)
      MonoAst.Expr.Match(e, rs, t, eff, loc)

    case TypedAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val rs = rules.map(lowerExtMatch)
      val t = lowerType(tpe)
      MonoAst.Expr.ExtMatch(e, rs, t, eff, loc)

    case TypedAst.Expr.Tag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(symUse.sym), es, t, eff, loc)

    case TypedAst.Expr.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      // Lower a restrictable tag into a normal tag.
      val caseSym = lowerRestrictableCaseSym(symUse.sym)
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), es, t, eff, loc)

    case TypedAst.Expr.ExtTag(label, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ExtTag(label), es, t, eff, loc)

    case TypedAst.Expr.Tuple(exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Tuple, es, t, eff, loc)

    case TypedAst.Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.RecordSelect(label), List(e), t, eff, loc)

    case TypedAst.Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.RecordExtend(label), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.RecordRestrict(label), List(e), t, eff, loc)

    case TypedAst.Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayLit, e :: es, t, eff, loc)

    case TypedAst.Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val e3 = lowerExp(exp3)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayNew, List(e1, e2, e3), t, eff, loc)

    case TypedAst.Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayLoad, List(e1, e2), t, eff, loc)

    case TypedAst.Expr.ArrayLength(exp, eff, loc) =>
      val e = lowerExp(exp)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayLength, List(e), Type.Int32, eff, loc)

    case TypedAst.Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val e3 = lowerExp(exp3)
      MonoAst.Expr.ApplyAtomic(AtomicOp.ArrayStore, List(e1, e2, e3), Type.Unit, eff, loc)

    case TypedAst.Expr.StructNew(sym, fields0, region0, tpe, eff, loc) =>
      val fields = fields0.map { case (k, v) => (k, lowerExp(v)) }
      val (names0, es) = fields.unzip
      val names = names0.map(_.sym)
      val t = lowerType(tpe)
      region0.map(lowerExp) match {
        case Some(region) =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.StructNew(sym, Mutability.Mutable, names), region :: es, t, eff, loc)
        case None =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.StructNew(sym, Mutability.Immutable, names), es, t, eff, loc)
      }

    case TypedAst.Expr.StructGet(exp, field, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.StructGet(field.sym), List(e), t, eff, loc)

    case TypedAst.Expr.StructPut(exp, field, exp1, tpe, eff, loc) =>
      val struct = lowerExp(exp)
      val rhs = lowerExp(exp1)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.StructPut(field.sym), List(struct, rhs), t, eff, loc)

    case TypedAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.VectorLit(es, t, eff, loc)

    case TypedAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.VectorLoad(e1, e2, t, eff, loc)

    case TypedAst.Expr.VectorLength(exp, loc) =>
      MonoAst.Expr.VectorLength(lowerExp(exp), loc)

    case TypedAst.Expr.Ascribe(exp, _, _, _, _, _) =>
      lowerExp(exp)

    case TypedAst.Expr.InstanceOf(exp, clazz, loc) =>
      val e = lowerExp(exp)
      if (isPrimType(e.tpe)) {
        // If it's a primitive type, evaluate the expression but return false
        MonoAst.Expr.Stm(e, MonoAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc), Type.Bool, e.eff, loc)
      } else {
        // If it's a reference type, then do the instanceof check
        MonoAst.Expr.ApplyAtomic(AtomicOp.InstanceOf(clazz), List(e), Type.Bool, e.eff, loc)
      }

    case TypedAst.Expr.CheckedCast(_, exp, tpe, eff, loc) =>
      // Note: We do *NOT* erase checked (i.e. safe) casts.
      // In Java, `String` is a subtype of `Object`, but the Flix IR makes this upcast _explicit_.
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      mkCast(e, t, eff, loc)

    case TypedAst.Expr.UncheckedCast(exp, _, _, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      mkCast(e, t, eff, loc)

    case TypedAst.Expr.Unsafe(exp, _, _, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      mkCast(e, t, eff, loc)

    case TypedAst.Expr.Without(exp, _, _, _, _) =>
      lowerExp(exp)

    case TypedAst.Expr.Throw(exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Throw, List(e), t, eff, loc)

    case TypedAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      val rs = rules.map(lowerCatchRule)
      MonoAst.Expr.TryCatch(e, rs, t, eff, loc)

    case TypedAst.Expr.Handler(symUse, rules, bodyTpe, bodyEff, handledEff, tpe, loc) =>
      // handler sym { rules }
      // is lowered to
      // handlerBody -> try handlerBody() with sym { rules }
      val bodySym = Symbol.freshVarSym("handlerBody", BoundBy.FormalParam, loc.asSynthetic)(Scope.Top, flix)
      val rs = rules.map(lowerHandlerRule)
      val bt = lowerType(bodyTpe)
      val t = lowerType(tpe)
      val bodyThunkType = Type.mkArrowWithEffect(Type.Unit, bodyEff, bt, loc.asSynthetic)
      val bodyVar = MonoAst.Expr.Var(bodySym, bodyThunkType, loc.asSynthetic)
      val body = MonoAst.Expr.ApplyClo(bodyVar, MonoAst.Expr.Cst(Constant.Unit, Type.Unit, loc.asSynthetic), bt, bodyEff, loc.asSynthetic)
      val RunWith = MonoAst.Expr.RunWith(body, symUse, rs, bt, handledEff, loc)
      val param = MonoAst.FormalParam(bodySym, bodyThunkType, Occur.Unknown, loc.asSynthetic)
      MonoAst.Expr.Lambda(param, RunWith, t, loc)

    case TypedAst.Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      // run exp1 with exp2
      // is lowered to
      // exp2(_runWith -> exp1)
      val e1 = lowerExp(exp1)
      val unitParam = MonoAst.FormalParam(Symbol.freshVarSym("_runWith", BoundBy.FormalParam, loc.asSynthetic)(Scope.Top, flix), Type.Unit, Occur.Unknown, loc.asSynthetic)
      val thunkType = Type.mkArrowWithEffect(Type.Unit, e1.eff, e1.tpe, loc.asSynthetic)
      val thunk = MonoAst.Expr.Lambda(unitParam, e1, thunkType, loc.asSynthetic)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyClo(lowerExp(exp2), thunk, t, eff, loc)

    case TypedAst.Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeConstructor(constructor), es, t, eff, loc)

    case TypedAst.Expr.InvokeSuperConstructor(constructor, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeSuperConstructor(constructor), es, t, eff, loc)

    case TypedAst.Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeMethod(method), e :: es, t, eff, loc)

    case TypedAst.Expr.InvokeSuperMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      (lctx.className, lctx.thisRef) match {
        case (Some(cn), Some(thisRef)) =>
          MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeSuperMethod(method, cn), thisRef :: es, t, eff, loc)
        case _ =>
          throw InternalCompilerException("InvokeSuperMethod outside NewObject context", loc)
      }

    case TypedAst.Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.InvokeStaticMethod(method), es, t, eff, loc)

    case TypedAst.Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.GetField(field), List(e), t, eff, loc)

    case TypedAst.Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.PutField(field), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.GetStaticField(field, tpe, eff, loc) =>
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.GetStaticField(field), List.empty, t, eff, loc)

    case TypedAst.Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.PutStaticField(field), List(e), t, eff, loc)

    case TypedAst.Expr.NewObject(name, clazz, tpe, eff, constructors, methods, loc) =>
      val cs = constructors.map(lowerJvmConstructor)
      val ms = methods.map { m =>
        val thisParam = m.fparams.head
        val thisTpe = lowerType(thisParam.tpe)
        val thisRef = MonoAst.Expr.Var(thisParam.bnd.sym, thisTpe, loc)
        implicit val lctx: LocalContext = LocalContext(Some(name), Some(thisRef))
        lowerJvmMethod(m)
      }
      val t = lowerType(tpe)
      MonoAst.Expr.NewObject(name, clazz, t, eff, cs, ms, loc)

    case TypedAst.Expr.NewChannel(exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      lowerNewChannel(e, tpe, eff, loc)

    case TypedAst.Expr.GetChannel(innerExp, tpe, eff, loc) =>
      val e = lowerExp(innerExp)
      val t = lowerType(tpe)
      mkGetChannel(e, t, eff, loc)

    case TypedAst.Expr.PutChannel(innerExp1, innerExp2, _, eff, loc) =>
      val exp1 = lowerExp(innerExp1)
      val exp2 = lowerExp(innerExp2)
      Lowering.mkPutChannel(exp1, exp2, eff, loc)

    case TypedAst.Expr.SelectChannel(rules0, default0, tpe, eff, loc) =>
      val rules = rules0.map {
        case TypedAst.SelectChannelRule(bnd, chan, exp, _) =>
          (bnd.sym, lowerExp(chan), lowerExp(exp))
      }
      val default = default0.map(lowerExp)
      val t = lowerType(tpe)
      Lowering.mkSelectChannel(rules, default, t, eff, loc)

    case TypedAst.Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e1, e2), t, eff, loc)

    case TypedAst.Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val fs = frags.map {
        case TypedAst.ParYieldFragment(pat, fragExp, fragLoc) =>
          val p = lowerPat(pat)
          (p, lowerExp(fragExp), fragLoc)
      }
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      Lowering.mkParYield(fs, e, t, eff, loc)

    case TypedAst.Expr.Lazy(exp, tpe, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Lazy, List(e), t, Type.Pure, loc)

    case TypedAst.Expr.Force(exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(AtomicOp.Force, List(e), t, eff, loc)

    case TypedAst.Expr.FixpointConstraintSet(cs, _, loc) =>
      lowerConstraintSet(cs, loc)

    case TypedAst.Expr.FixpointLambda(pparams, exp, _, eff, loc) =>
      val resultType = Types.Datalog
      val defn = lookup(Defs.Rename, resultType)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.PredSym, loc)
      val argExps = predExps :: lowerExp(exp) :: Nil
      MonoAst.Expr.ApplyDef(defn, argExps, Types.RenameType, resultType, eff, loc)

    case TypedAst.Expr.FixpointMerge(exp1, exp2, _, eff, loc) =>
      val resultType = Types.Datalog
      val defn = lookup(Defs.Merge, resultType)
      val argExps = lowerExp(exp1) :: lowerExp(exp2) :: Nil
      MonoAst.Expr.ApplyDef(defn, argExps, Types.MergeType, resultType, eff, loc)

    case TypedAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tpe0, eff, loc) =>
      lowerQueryWithProvenance(exps, select, withh, tpe0, eff, loc)

    case TypedAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, _, _, pred, tpe, eff, loc) =>
      lowerQueryWithSelect(exps, queryExp, selects, pred, tpe, eff, loc)

    case TypedAst.Expr.FixpointSolveWithProject(exps, optPreds, mode, _, eff, loc) =>
      lowerSolveWithProject(exps, optPreds, mode, eff, loc)

    case TypedAst.Expr.FixpointInjectInto(exps, predsAndArities, _, _, loc) =>
      lowerInjectInto(exps, predsAndArities, loc)

    case TypedAst.Expr.ApplySig(_, _, _, _, _, _, _, _) =>
      throw InternalCompilerException(s"Unexpected ApplySig", exp0.loc)

    case TypedAst.Expr.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)

  }

  /**
    * Lowers the given JvmConstructor `constructor`.
    */
  private def lowerJvmConstructor(constructor: TypedAst.JvmConstructor)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.JvmConstructor = constructor match {
    case TypedAst.JvmConstructor(exp, retTpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(retTpe)
      MonoAst.JvmConstructor(e, t, eff, loc)
  }

  /**
    * Lowers the given JvmMethod `method`.
    */
  private def lowerJvmMethod(method: TypedAst.JvmMethod)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, retTyp, eff, loc) =>
      val fs = fparams.map(lowerFormalParam)
      val e = lowerExp(exp)
      val t = lowerType(retTyp)
      MonoAst.JvmMethod(ident, fs, e, t, eff, loc)
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def lowerCatchRule(rule: TypedAst.CatchRule)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.CatchRule = rule match {
    case TypedAst.CatchRule(bnd, clazz, exp, _) =>
      val e = lowerExp(exp)
      MonoAst.CatchRule(bnd.sym, clazz, e)
  }

  /**
    * Lowers the given handler rule `rule0`.
    */
  private def lowerHandlerRule(rule0: TypedAst.HandlerRule)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.HandlerRule = rule0 match {
    case TypedAst.HandlerRule(opSymUse, fparams0, body0, _) =>
      val fparams = fparams0.map(lowerFormalParam)
      val body = lowerExp(body0)
      MonoAst.HandlerRule(opSymUse, fparams, body)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: TypedAst.MatchRule)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.MatchRule = rule0 match {
    case TypedAst.MatchRule(pat, guard, body, _) =>
      val p = lowerPat(pat)
      val g = guard.map(lowerExp)
      val b = lowerExp(body)
      MonoAst.MatchRule(p, g, b)
  }

  /**
    * Lowers the given pattern `pat0`.
    */
  private def lowerPat(pat0: TypedAst.Pattern): MonoAst.Pattern = pat0 match {
    case TypedAst.Pattern.Wild(tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.Pattern.Wild(t, loc)

    case TypedAst.Pattern.Var(bnd, tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.Pattern.Var(bnd.sym, t, Occur.Unknown, loc)

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.Pattern.Cst(cst, t, loc)

    case TypedAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(lowerPat)
      val t = lowerType(tpe)
      MonoAst.Pattern.Tag(symUse, ps, t, loc)

    case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(lowerPat)
      val t = lowerType(tpe)
      MonoAst.Pattern.Tuple(es, t, loc)

    case TypedAst.Pattern.Record(pats, pat, tpe, loc) =>
      val patsVal = pats.map {
        case TypedAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val p1 = lowerPat(pat1)
          val t1 = lowerType(tpe1)
          MonoAst.Pattern.Record.RecordLabelPattern(label, p1, t1, loc1)
      }
      val patVal = lowerPat(pat)
      val t = lowerType(tpe)
      MonoAst.Pattern.Record(patsVal, patVal, t, loc)

    case TypedAst.Pattern.Error(_, loc) =>
      throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)
  }

  /**
    * Lowers the given restrictable choice rule `rule0` to a match rule.
    */
  private def lowerRestrictableChooseRule(rule0: TypedAst.RestrictableChooseRule)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.MatchRule = rule0 match {
    case TypedAst.RestrictableChooseRule(pat, exp) =>
      val e = lowerExp(exp)
      pat match {
        case TypedAst.RestrictableChoosePattern.Tag(symUse, pat0, tpe, loc) =>
          val termPatterns = pat0.map {
            case TypedAst.RestrictableChoosePattern.Var(TypedAst.Binder(varSym, _), varTpe, varLoc) => MonoAst.Pattern.Var(varSym, varTpe, Occur.Unknown, varLoc)
            case TypedAst.RestrictableChoosePattern.Wild(wildTpe, wildLoc) => MonoAst.Pattern.Wild(wildTpe, wildLoc)
            case TypedAst.RestrictableChoosePattern.Error(_, errLoc) => throw InternalCompilerException("unexpected restrictable choose variable", errLoc)
          }
          val tagSymUse = lowerRestrictableCaseSymUse(symUse)
          val p = MonoAst.Pattern.Tag(tagSymUse, termPatterns, lowerType(tpe), loc)
          MonoAst.MatchRule(p, None, e)
        case TypedAst.RestrictableChoosePattern.Error(_, loc) => throw InternalCompilerException("unexpected error restrictable choose pattern", loc)
      }
  }

  /**
    * Lowers `sym` from a restrictable case sym use into a regular case sym use.
    */
  private def lowerRestrictableCaseSymUse(symUse: SymUse.RestrictableCaseSymUse): SymUse.CaseSymUse = {
    SymUse.CaseSymUse(lowerRestrictableCaseSym(symUse.sym), symUse.sym.loc)
  }

  private def lowerExtPat(pat0: TypedAst.ExtPattern): MonoAst.ExtPattern = pat0 match {
    case TypedAst.ExtPattern.Default(loc) =>
      MonoAst.ExtPattern.Default(loc)

    case TypedAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(lowerExtTagPat)
      MonoAst.ExtPattern.Tag(label, ps, loc)

    case TypedAst.ExtPattern.Error(loc) =>
      throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  private def lowerExtTagPat(pat0: TypedAst.ExtTagPattern): MonoAst.ExtTagPattern = pat0 match {
    case TypedAst.ExtTagPattern.Wild(tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.ExtTagPattern.Wild(t, loc)

    case TypedAst.ExtTagPattern.Var(bnd, tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.ExtTagPattern.Var(bnd.sym, t, Occur.Unknown, loc)

    case TypedAst.ExtTagPattern.Unit(tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.ExtTagPattern.Unit(t, loc)

    case TypedAst.ExtTagPattern.Error(_, loc) =>
      throw InternalCompilerException("unexpected error ext pattern", loc)
  }

  private def lowerExtMatch(rule0: TypedAst.ExtMatchRule)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.ExtMatchRule = rule0 match {
    case TypedAst.ExtMatchRule(pat, exp, loc) =>
      val p = lowerExtPat(pat)
      val e = lowerExp(exp)
      MonoAst.ExtMatchRule(p, e, loc)
  }

  private def lowerFormalParam(fparam: TypedAst.FormalParam): MonoAst.FormalParam = fparam match {
    case TypedAst.FormalParam(bnd, tpe, _, loc0) => MonoAst.FormalParam(bnd.sym, lowerType(tpe), Occur.Unknown, loc0)
  }

  private def lowerTypeParam(tparam: TypedAst.TypeParam): MonoAst.TypeParam = tparam match {
    case TypedAst.TypeParam(name, sym, loc) =>
      MonoAst.TypeParam(name, sym, loc)
  }

  /**
    * Wraps an entry point function with calls to the default handlers of each of the effects appearing in
    * its signature. The order in which the handlers are applied is not defined and should not be relied upon.
    *
    * For example, if we had default handlers for some effects A, B and C:
    *
    * Transforms a function:
    * {{{
    *     def f(arg1: tpe1, ...): tpe \ A + B = exp
    * }}}
    * Into:
    * {{{
    *     def f(arg1: tpe1, ...): tpe \ (((ef - A) + IO) - B) + IO =
    *         handlerB(_ -> handlerA(_ ->exp))
    * }}}
    *
    * Each of the wrappers:
    *   - Removes the handled effect from the function's effect set and adds IO
    *   - Creates a lambda (_ -> originalBody) and passes it to each handler
    *   - Updates the function's type signature accordingly
    *
    * @param currentDef The entry point function definition to wrap
    * @param root       The typed AST root
    * @return The wrapped function definition with all necessary default effect handlers
    */
  private def wrapDefWithDefaultHandlers(currentDef: TypedAst.Def)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): TypedAst.Def = {
    // Obtain the concrete effect set of the definition that is going to be wrapped.
    // We are expecting entry points, and all entry points should have a concrete effect set
    // Obtain the concrete effect set of the definition that is going to be wrapped.
    // We are expecting entry points, and all entry points should have a concrete effect set
    val defEffects: CofiniteSet[Symbol.EffSym] = Type.eval(currentDef.spec.eff) match {
      case Result.Ok(s) => s
      // This means eff is either not well-formed or it has type variables.
      // Either way, in this case we will wrap with all default handlers
      // to make sure that the effects present in the signature that have default handlers are handled
      case Result.Err(_) => throw InternalCompilerException("Unexpected illegal effect set on entry point", currentDef.spec.eff.loc)
    }
    // Gather only the default handlers for the effects appearing in the signature of the definition.
    val requiredHandlers = root.defaultHandlers.filter(h => defEffects.contains(h.handledSym))
    // Wrap the expression in each of the required default handlers.
    // Right now, the order depends on the order of defaultHandlers.
    requiredHandlers.foldLeft(currentDef)((defn, handler) => wrapInHandler(defn, handler))
  }

  /**
    * Wraps an entry point function with a default effect handler.
    *
    * Transforms a function:
    * {{{
    *     def f(arg1: tpe1, ...): tpe \ ef = exp
    * }}}
    *
    * Into:
    * {{{
    *     def f(arg1: tpe1, ...): tpe \ (ef - handledEffect) + IO =
    *         handler(_ -> exp)
    * }}}
    *
    * The wrapper:
    *   - Removes the handled effect from the function's effect set and adds IO
    *   - Creates a lambda (_ -> originalBody) and passes it to each handler
    *   - Updates the function's type signature accordingly
    *
    * @param defn           The entry point function definition to wrap
    * @param defaultHandler Information about the default handler to apply
    * @param root           The typed AST root
    * @return The wrapped function definition with updated effect signature
    */
  private def wrapInHandler(defn: TypedAst.Def, defaultHandler: DefaultHandler)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): TypedAst.Def = {
    // Create synthetic locations
    val effLoc = defn.spec.eff.loc.asSynthetic
    val baseTypeLoc = defn.spec.declaredScheme.base.loc.asSynthetic
    val expLoc = defn.exp.loc.asSynthetic
    // The new type is the same as the wrapped def with an effect set of
    // `(ef - handledEffect) + IO` where `ef` is the effect set of the previous definition.
    val effDif = Type.mkDifference(defn.spec.eff, defaultHandler.handledEff, effLoc)
    // Technically we could perform this outside at the wrapInHandlers level
    // by just checking the length of the handlers and if it is greater than 0
    // just adding IO. However, that would only work while default handlers can only generate IO.
    val eff = Type.mkUnion(effDif, Type.IO, effLoc)
    val tpe = Type.mkCurriedArrowWithEffect(defn.spec.fparams.map(_.tpe), eff, defn.spec.retTpe, baseTypeLoc)
    val spec = defn.spec.copy(
      declaredScheme = defn.spec.declaredScheme.copy(base = tpe),
      eff = eff,
    )
    // Create _ -> exp
    val innerLambda =
      TypedAst.Expr.Lambda(
        TypedAst.FormalParam(
          TypedAst.Binder(Symbol.freshVarSym("_", BoundBy.FormalParam, expLoc)(Scope.Top, flix), Type.Unit),
          Type.Unit,
          TypeSource.Inferred,
          expLoc
        ),
        defn.exp,
        Type.mkArrowWithEffect(Type.Unit, defn.spec.eff, defn.spec.retTpe, expLoc),
        expLoc
      )
    // Create the instantiated type of the handler
    val handlerArrowType = Type.mkArrowWithEffect(innerLambda.tpe, eff, defn.spec.retTpe, expLoc)
    // Create HandledEff.handle(_ -> exp)
    val defnSym = lookup(defaultHandler.handlerSym, handlerArrowType)
    val handlerDefSymUse = SymUse.DefSymUse(defnSym, expLoc)
    val handlerCall = TypedAst.Expr.ApplyDef(handlerDefSymUse, List(innerLambda), List(innerLambda.tpe), handlerArrowType, defn.spec.retTpe, eff, expLoc)
    defn.copy(spec = spec, exp = handlerCall)
  }

  /**
    * Lowers `sym` from a restrictable enum sym into a regular enum sym.
    */
  private def lowerRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): Symbol.EnumSym =
    new Symbol.EnumSym(None, sym.namespace, sym.name, sym.loc)

  /**
    * Returns the definition associated with the given symbol `sym`.
    *
    * @param tpe must be specialized. Can be visited, if the underlying function can handle that
    */
  private def lookup(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): Symbol.DefnSym =
    Specialization.specializeDefnSym(sym, tpe)

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
        MonoAst.Expr.Stm(exp, crash, tpe, eff, loc)
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
    * Lowers `sym` from a restrictable case sym into a regular case sym.
    */
  private def lowerRestrictableCaseSym(sym: Symbol.RestrictableCaseSym): Symbol.CaseSym = {
    val enumSym = lowerRestrictableEnumSym(sym.enumSym)
    new Symbol.CaseSym(enumSym, sym.name, sym.loc)
  }

  /**
    * Make a new channel tuple (sender, receiver) expression
    *
    * New channel expressions are rewritten as follows:
    * {{{ %%CHANNEL_NEW%%(m) }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.newChannel(10) }}}
    *
    * @param tpe The specialized type of the result
    */
  private def lowerNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookup(Defs.ChannelNewTuple, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, itpe, lowerType(tpe), eff, loc)
  }

  /**
    * Make a channel get expression
    *
    * Channel get expressions are rewritten as follows:
    * {{{ <- c }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.get(c) }}}
    */
  private def mkGetChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookup(Defs.ChannelGet, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, itpe, lowerType(tpe), eff, loc)
  }

  /**
    * Make a channel put expression
    *
    * Channel put expressions are rewritten as follows:
    * {{{ c <- 42 }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.put(42, c) }}}
    */
  private def mkPutChannel(exp1: MonoAst.Expr, exp2: MonoAst.Expr, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoUncurriedArrow(List(exp2.tpe, exp1.tpe), Type.Unit, loc))
    val defnSym = lookup(Defs.ChannelPut, itpe)
    MonoAst.Expr.ApplyDef(defnSym, List(exp2, exp1), itpe, Type.Unit, eff, loc)
  }

  /**
    * Make a channel select expression
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
  private def mkSelectChannel(rules: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], default: Option[MonoAst.Expr], tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val t = lowerType(tpe)

    val channels = rules.map { case (_, c, _) => (mkLetSym("chan", loc), c) }
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
  private def mkChannelAdminList(rs: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], channels: List[(Symbol.VarSym, MonoAst.Expr)], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val admins = ListOps.zip(rs, channels) map {
      case ((_, c, _), (chanSym, _)) =>
        val itpe = lowerType(Type.mkPureArrow(c.tpe, Types.ChannelMpmcAdmin, loc))
        val defnSym = lookup(Defs.ChannelMpmcAdmin, itpe)
        MonoAst.Expr.ApplyDef(defnSym, List(MonoAst.Expr.Var(chanSym, lowerType(c.tpe), loc)), itpe, Types.ChannelMpmcAdmin, Type.Pure, loc)
    }
    mkList(admins, Types.ChannelMpmcAdmin, loc)
  }

  /**
    * Construct a call to `selectFrom` given a list of MpmcAdmin objects and optional default.
    *
    * Transforms
    * {{{ mpmcAdmin(ch1), mpmcAdmin(ch1), ... }}}
    * Into
    * {{{ selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: ... :: Nil, false) }}}
    *
    * When `default` is `Some` the second parameter will be `true`.
    */
  private def mkChannelSelect(admins: MonoAst.Expr, default: Option[MonoAst.Expr], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    val selectRetTpe = Type.mkTuple(List(Type.Int32, locksType), loc)
    val itpe = Type.mkIoUncurriedArrow(List(admins.tpe, Type.Bool), selectRetTpe, loc)
    val blocking = default match {
      case Some(_) => MonoAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc)
      case None => MonoAst.Expr.Cst(Constant.Bool(true), Type.Bool, loc)
    }
    val defnSym = lookup(Defs.ChannelSelectFrom, itpe)
    MonoAst.Expr.ApplyDef(defnSym, List(admins, blocking), lowerType(itpe), selectRetTpe, Type.IO, loc)
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
  private def mkChannelCases(rs: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], channels: List[(Symbol.VarSym, MonoAst.Expr)], eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): List[MonoAst.MatchRule] = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    ListOps.zip(rs, channels).zipWithIndex map {
      case (((sym, chan, exp), (chSym, _)), i) =>
        val locksSym = mkLetSym("locks", loc)
        val pat = mkTuplePattern(Nel(MonoAst.Pattern.Cst(Constant.Int32(i), Type.Int32, loc), List(MonoAst.Pattern.Var(locksSym, locksType, Occur.Unknown, loc))), loc)
        val getTpe = extractChannelTpe(chan.tpe)
        val itpe = lowerType(Type.mkIoUncurriedArrow(List(chan.tpe, locksType), getTpe, loc))
        val args = List(MonoAst.Expr.Var(chSym, lowerType(chan.tpe), loc), MonoAst.Expr.Var(locksSym, locksType, loc))
        val defnSym = lookup(Defs.ChannelUnsafeGetAndUnlock, itpe)
        val getExp = MonoAst.Expr.ApplyDef(defnSym, args, lowerType(itpe), lowerType(getTpe), eff, loc)
        val e = MonoAst.Expr.Let(sym, getExp, exp, lowerType(exp.tpe), eff, Occur.Unknown, loc)
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
  private def mkSelectDefaultCase(default: Option[MonoAst.Expr], loc: SourceLocation): List[MonoAst.MatchRule] = {
    default match {
      case Some(defaultExp) =>
        val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)
        val pat = mkTuplePattern(Nel(MonoAst.Pattern.Cst(Constant.Int32(-1), Type.Int32, loc), List(MonoAst.Pattern.Wild(locksType, loc))), loc)
        val defaultMatch = MonoAst.MatchRule(pat, None, defaultExp)
        List(defaultMatch)
      case _ =>
        List()
    }
  }

  /**
    * Returns a desugared [[TypedAst.Expr.ParYield]] expression as a nested match-expression.
    */
  private def mkParYield(frags: List[(MonoAst.Pattern, MonoAst.Expr, SourceLocation)], exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Only generate channels for n-1 fragments. We use the current thread for the last fragment.
    val fs = frags.init
    val last = frags.last

    // Generate symbols for each channel.
    val chanSymsWithPatAndExp = fs.map { case (p, e, l) => (p, mkLetSym("channel", l.asSynthetic), e) }

    // Make `GetChannel` exps for the spawnable exps.
    val waitExps = mkBoundParWaits(chanSymsWithPatAndExp, exp)

    // Evaluate the last expression in the current thread (so just make let-binding)
    val desugaredYieldExp = mkLetMatch(last._1, last._2, waitExps)

    // Generate channels and spawn exps.
    val chanSymsWithExp = chanSymsWithPatAndExp.map { case (_, s, e) => (s, e) }
    val blockExp = mkParChannels(desugaredYieldExp, chanSymsWithExp)

    // Wrap everything in a purity cast,
    MonoAst.Expr.Cast(blockExp, lowerType(tpe), eff, loc.asSynthetic)
  }

  /**
    * Returns a full `par yield` expression.
    */
  private def mkParChannels(exp: MonoAst.Expr, chanSymsWithExps: List[(Symbol.VarSym, MonoAst.Expr)])(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Make spawn expressions `spawn ch <- exp`.
    val spawns = chanSymsWithExps.foldRight(exp: MonoAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val e1 = mkChannelExp(sym, e.tpe, loc) // The channel `ch`
        val e2 = mkPutChannel(e1, e, Type.IO, loc) // The put exp: `ch <- exp0`.
        val e3 = MonoAst.Expr.Cst(Constant.Static, Type.mkRegionToStar(Type.IO, loc), loc)
        val e4 = MonoAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e2, e3), Type.Unit, Type.IO, loc) // Spawn the put expression from above i.e. `spawn ch <- exp0`.
        MonoAst.Expr.Stm(e4, acc, acc.tpe, Type.mkUnion(e4.eff, acc.eff, loc), loc) // Return a statement expression containing the other spawn expressions along with this one.
    }

    // Make let bindings `let ch = chan 1;`.
    chanSymsWithExps.foldRight(spawns: MonoAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val chan = mkNewChannel(MonoAst.Expr.Cst(Constant.Int32(1), Type.Int32, loc), mkChannelTpe(e.tpe, loc), Type.IO, loc) // The channel exp `chan 1`
        MonoAst.Expr.Let(sym, chan, acc, acc.tpe, Type.mkUnion(e.eff, acc.eff, loc), Occur.Unknown, loc) // The let-binding `let ch = chan 1`
    }
  }

  /**
    * Make a new channel expression
    */
  private def mkNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookup(Defs.ChannelNew, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, itpe, tpe, eff, loc)
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
  private def mkBoundParWaits(patSymExps: List[(MonoAst.Pattern, Symbol.VarSym, MonoAst.Expr)], exp: MonoAst.Expr)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr =
    patSymExps.map {
      case (p, sym, e) =>
        val loc = e.loc.asSynthetic
        val chExp = mkChannelExp(sym, e.tpe, loc)
        (p, mkGetChannel(chExp, e.tpe, Type.IO, loc))
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
    * An expression for a channel variable called `sym`
    */
  private def mkChannelExp(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation): MonoAst.Expr = {
    MonoAst.Expr.Var(sym, mkChannelTpe(tpe, loc), loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    *
    * @param elmType is assumed to be specialized and lowered.
    */
  private def mkList(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation): MonoAst.Expr = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, loc)
    }
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    *
    * @param elmType is assumed to be specialized and lowered.
    */
  private def mkNil(elmType: Type, loc: SourceLocation): MonoAst.Expr = {
    mkTag(Enums.FList, "Nil", Nil, Types.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
    */
  private def mkCons(hd: MonoAst.Expr, tail: MonoAst.Expr, loc: SourceLocation): MonoAst.Expr = {
    mkTag(Enums.FList, "Cons", List(hd, tail), lowerType(tail.tpe), loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    *
    * @param tpe is assumed to be specialized and lowered.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exps: List[MonoAst.Expr], tpe: Type, loc: SourceLocation): MonoAst.Expr = {
    val caseSym = new Symbol.CaseSym(sym, tag, loc.asSynthetic)
    MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, tpe, Type.Pure, loc)
  }

  /**
    * Returns `(t1, t2)` where `tpe = Concurrent.Channel.Mpmc[t1, t2]`.
    *
    * @param tpe is assumed to be specialized, but not lowered.
    */
  private def extractChannelTpe(tpe: Type): Type = tpe match {
    case Type.Apply(Type.Apply(Types.ChannelMpmc, elmType, _), _, _) => elmType
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
    * Returns a new `VarSym` for use in a let-binding.
    *
    * This function is called `mkLetSym` to avoid confusion with [[mkVarSym]].
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)(Scope.Top, flix)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe`.
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    mkChannelTpe(tpe, Type.IO, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe1` in region `tpe2`.
    */
  private def mkChannelTpe(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Apply(Types.ChannelMpmc, tpe1, loc), tpe2, loc)
  }

  /*
    * Datalog lowering
    */

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Datalog` value from the given list of Datalog constraints `cs`.
    */
  private def lowerConstraintSet(cs: List[TypedAst.Constraint], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val factExps = cs.filter(c => c.body.isEmpty).map(lowerConstraint)
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(lowerConstraint)

    val factListExp = mkVector(factExps, Types.Constraint, loc)
    val ruleListExp = mkVector(ruleExps, Types.Constraint, loc)

    val innerExp = List(factListExp, ruleListExp)
    mkTag(Enums.Datalog, "Datalog", innerExp, Types.Datalog, loc)
  }

  /**
    *
    * Create appropriate call to Fixpoint.Solver.provenanceOf. This requires creating a mapping, mkExtVar, from
    * PredSym and terms to an extensible variant.
    */
  private def lowerQueryWithProvenance(exps: List[TypedAst.Expr], select: Predicate.Head, withh: List[Name.Pred], tpe0: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val tpe = lowerType(tpe0)
    val mergedExp = mergeExps(exps.map(lowerExp), loc)
    val (goalPredSym, goalTerms) = select match {
      case TypedAst.Predicate.Head.Atom(pred, _, terms, _, loc1) =>
        val boxedTerms = terms.map(t => box(lowerExp(t)))
        (mkPredSym(pred), mkVector(boxedTerms, Types.Boxed, loc1))
    }
    val withPredSyms = mkVector(withh.map(mkPredSym), Types.PredSym, loc)
    val extVarType = unwrapVectorType(tpe, loc)
    val preds = predicatesOfExtVar(extVarType, loc)
    val lambdaExp = mkExtVarLambda(preds, extVarType, loc)
    val argExps = goalPredSym :: goalTerms :: withPredSyms :: lambdaExp :: mergedExp :: Nil
    val itpe = Types.mkProvenanceOf(extVarType, loc)
    val defn = lookup(Defs.ProvenanceOf, itpe)
    MonoAst.Expr.ApplyDef(defn, argExps, itpe, tpe, eff, loc)
  }

  private def lowerQueryWithSelect(exps: List[TypedAst.Expr], queryExp: TypedAst.Expr, selects: List[TypedAst.Expr], pred: Name.Pred, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val loweredExps = exps.map(lowerExp)
    val loweredQueryExp = lowerExp(queryExp)

    val predArity = selects.length

    // Define the name and type of the appropriate factsX function in Solver.flix
    val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), tpe, loc)
    val sym = lookup(Defs.Facts(predArity), defTpe)

    // Merge and solve exps
    val mergedExp = mergeExps(loweredQueryExp :: loweredExps, loc)
    val solveDefn = lookup(Defs.Solve, Types.SolveType)
    val solvedExp = MonoAst.Expr.ApplyDef(solveDefn, mergedExp :: Nil, Types.SolveType, Types.Datalog, eff, loc)

    // Put everything together
    val argExps = mkPredSym(pred) :: solvedExp :: Nil
    MonoAst.Expr.ApplyDef(sym, argExps, defTpe, tpe, eff, loc)

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
  private def lowerSolveWithProject(exps0: List[TypedAst.Expr], optPreds: Option[List[Name.Pred]], mode: SolveMode, eff: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val defn = mode match {
      case SolveMode.Default => lookup(Defs.Solve, Types.Datalog)
      case SolveMode.WithProvenance => lookup(Defs.SolveWithProvenance, Types.Datalog)
    }
    val exps = exps0.map(lowerExp)
    val mergedExp = mergeExps(exps, loc)
    val argExps = mergedExp :: Nil
    val solvedExp = MonoAst.Expr.ApplyDef(defn, argExps, Types.SolveType, Types.Datalog, eff, loc)
    val tmpVarSym = Symbol.freshVarSym("tmp%", BoundBy.Let, loc)(Scope.Top, flix)
    val letBodyExp = optPreds match {
      case Some(preds) =>
        mergeExps(preds.map(pred => {
          val varExp = MonoAst.Expr.Var(tmpVarSym, Types.Datalog, loc)
          projectSym(mkPredSym(pred), varExp, loc)
        }), loc)
      case None => MonoAst.Expr.Var(tmpVarSym, Types.Datalog, loc)
    }
    MonoAst.Expr.Let(tmpVarSym, solvedExp, letBodyExp, Types.Datalog, eff, Occur.Unknown, loc)

  }

  private def lowerInjectInto(exps: List[TypedAst.Expr], predsAndArities: List[PredicateAndArity], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val loweredExps = exps.zip(predsAndArities).map {
      case (exp, PredicateAndArity(pred, _)) =>
        // Compute the types arguments of the functor F[(a, b, c)] or F[a].
        val (_, targsLength) = exp.tpe match {
          case Type.Apply(tycon, innerType, _) => innerType.typeConstructor match {
            case Some(TypeConstructor.Tuple(_)) => (tycon, innerType.typeArguments.length)
            case Some(TypeConstructor.Unit) => (tycon, 0)
            case _ => (tycon, 1)
          }
          case _ => throw InternalCompilerException(s"Unexpected non-foldable type: '${exp.tpe}'.", loc)
        }

        // The type of the function.
        val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, lowerType(exp.tpe)), Types.Datalog, loc)

        // Compute the symbol of the function.
        val sym = lookup(Defs.ProjectInto(targsLength), defTpe)

        // Put everything together.
        val argExps = mkPredSym(pred) :: lowerExp(exp) :: Nil
        MonoAst.Expr.ApplyDef(sym, argExps, defTpe, Types.Datalog, exp.eff, loc)
    }
    mergeExps(loweredExps, loc)

  }

  /**
    * Lowers the given constraint `c0`.
    */
  private def lowerConstraint(c0: TypedAst.Constraint)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = c0 match {
    case TypedAst.Constraint(cparams, head, body, loc) =>
      val headExp = lowerHeadPred(cparams, head)
      val bodyExp = mkVector(body.map(lowerBodyPred(cparams, _)), Types.BodyPredicate, loc)
      val innerExp = List(headExp, bodyExp)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def lowerHeadPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Head)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = p0 match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val termsExp = mkVector(terms.map(lowerHeadTerm(cparams0, _)), Types.HeadTerm, loc)
      val innerExp = List(predSymExp, denotationExp, termsExp)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def lowerBodyPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Body)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = p0 match {
    case TypedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val fixityExp = mkFixity(fixity, loc)
      val termsExp = mkVector(terms.map(lowerBodyTerm(cparams0, _)), Types.BodyTerm, loc)
      val innerExp = List(predSymExp, denotationExp, polarityExp, fixityExp, termsExp)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case TypedAst.Predicate.Body.Functional(outVars0, exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val inVars = quantifiedVars(cparams0, exp0)
      val exp = lowerExp(exp0)
      val outVars = outVars0.map(_.sym)
      mkFunctional(outVars, inVars, exp, loc)

    case TypedAst.Predicate.Body.Guard(exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val quantifiedFreeVars = quantifiedVars(cparams0, exp0)
      val exp = lowerExp(exp0)
      mkGuard(quantifiedFreeVars, exp, loc)

  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def lowerHeadTerm(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
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
        if (isQuantifiedVar(sym, cparams0)) {
          // Case 1.1: Quantified variable.
          mkHeadTermVar(sym)
        } else {
          // Case 1.2: Lexically bound variable.
          mkHeadTermLit(box(lowerExp(exp0)))
        }

      case _ =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val quantifiedFreeVars = quantifiedVars(cparams0, exp0)

        if (quantifiedFreeVars.isEmpty) {
          // Case 2: No quantified variables. The expression can be reduced to a value.
          mkHeadTermLit(box(lowerExp(exp0)))
        } else {
          // Case 3: Quantified variables. The expression is translated to an application term.
          mkAppTerm(quantifiedFreeVars, lowerExp(exp0), exp0.loc)
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def lowerBodyTerm(cparams0: List[TypedAst.ConstraintParam], pat0: TypedAst.Pattern)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = pat0 match {
    case TypedAst.Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case TypedAst.Pattern.Var(bnd, tpe, loc) =>
      if (isQuantifiedVar(bnd.sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(bnd.sym)
      } else {
        // Case 2: Lexically bound variable *expression*.
        mkBodyTermLit(box(MonoAst.Expr.Var(bnd.sym, tpe, loc)))
      }

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      mkBodyTermLit(box(MonoAst.Expr.Cst(cst, tpe, loc)))

    case TypedAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Error(_, loc) =>
      throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

  }

  /**
    * Returns an expression merging `exps` using `Defs.Merge`.
    */
  private def mergeExps(exps: List[MonoAst.Expr], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr =
    exps.reduceRight {
      (exp, acc) =>
        val resultType = Types.Datalog
        val defn = lookup(Defs.Merge, resultType)
        val argExps = exp :: acc :: Nil
        val itpe = Types.MergeType
        MonoAst.Expr.ApplyDef(defn, argExps, itpe, resultType, exp.eff, loc)
    }

  /**
    * Returns a new `Datalog` from `datalogExp` containing only facts from the predicate given by the `PredSym` `predSymExp`
    * using `Defs.Filter`.
    */
  private def projectSym(predSymExp: MonoAst.Expr, datalogExp: MonoAst.Expr, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val resultType = Types.Datalog
    val defn = lookup(Defs.Filter, resultType)
    val argExps = predSymExp :: datalogExp :: Nil
    val itpe = Types.FilterType
    MonoAst.Expr.ApplyDef(defn, argExps, itpe, resultType, datalogExp.eff, loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: MonoAst.Expr, argTypes: List[Type], resultType: Type)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    //
    // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Boxed`.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> `resultType`.
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Boxed.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Types.Boxed, exp0.loc)

    // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> Boxed).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Compute the liftXb symbol.
    val sym = lookup(Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Boxable.lift${argTypes.length}"), liftType)

    // Construct a call to the liftX function.
    MonoAst.Expr.ApplyDef(sym, List(exp0), liftType, returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    */
  private def liftXb(exp0: MonoAst.Expr, argTypes: List[Type])(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    //
    // The liftX family of functions are of the form: a -> b -> c -> Bool and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Bool.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> Bool.
    val argType = Type.mkPureCurriedArrow(argTypes, Type.Bool, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Bool.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Type.Bool, exp0.loc)

    // The type of the overall liftXb function, i.e. (a -> b -> c -> Bool) -> (Boxed -> Boxed -> Boxed -> Bool).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Compute the liftXb symbol.
    val sym = lookup(Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Boxable.lift${argTypes.length}b"), liftType)

    // Construct a call to the liftXb function.
    MonoAst.Expr.ApplyDef(sym, List(exp0), liftType, returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
    */
  private def liftXY(outVars: List[Symbol.VarSym], exp0: MonoAst.Expr, argTypes: List[Type], resultType: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    //
    // The liftXY family of functions are of the form: i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)] and
    // returns a function of the form Vector[Boxed] -> Vector[Vector[Boxed]].
    // That is, the function accepts a *curried* function and an uncurried function that takes
    // its input as a boxed Vector and return its output as a vector of vectors.
    //

    // The type of the function argument, i.e. i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)].
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, loc)

    // The type of the returned function, i.e. Vector[Boxed] -> Vector[Vector[Boxed]].
    val returnType = Type.mkPureArrow(Type.mkVector(Types.Boxed, loc), Type.mkVector(Type.mkVector(Types.Boxed, loc), loc), loc)

    // The type of the overall liftXY function, i.e. (i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)]) -> (Vector[Boxed] -> Vector[Vector[Boxed]]).
    val liftType = Type.mkPureArrow(argType, returnType, loc)

    // Compute the number of bound ("output") and free ("input") variables.
    val numberOfInVars = argTypes.length
    val numberOfOutVars = outVars.length

    // Compute the liftXY symbol.
    // For example, lift3X2 is a function from three arguments to a Vector of pairs.
    val sym = lookup(Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Boxable.lift${numberOfInVars}X$numberOfOutVars"), liftType)

    // Construct a call to the liftXY function.
    MonoAst.Expr.ApplyDef(sym, List(exp0), liftType, returnType, Type.Pure, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym): MonoAst.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: MonoAst.Expr): MonoAst.Expr = {
    mkTag(Enums.HeadTerm, "Lit", List(exp), Types.HeadTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation): MonoAst.Expr = {
    mkTag(Enums.BodyTerm, "Wild", Nil, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym): MonoAst.Expr = {
    val innerExp = List(mkVarSym(sym))
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: MonoAst.Expr): MonoAst.Expr = {
    mkTag(Enums.BodyTerm, "Lit", List(exp), Types.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): MonoAst.Expr = {
    val nameExp = MonoAst.Expr.Cst(Constant.Str(sym.text), Type.Str, sym.loc)
    mkTag(Enums.VarSym, "VarSym", List(nameExp), Types.VarSym, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = d match {
    case Denotation.Relational =>
      mkTag(Enums.Denotation, "Relational", Nil, Types.Denotation, loc)

    case Denotation.Latticenal =>
      tpeOpt match {
        case None => throw InternalCompilerException("Unexpected nullary lattice predicate.", loc)
        case Some(tpe) =>
          val innerType = lowerType(tpe)
          // The type `Denotation[tpe]`.
          val unboxedDenotationType = Type.mkEnum(Enums.Denotation, innerType :: Nil, loc)

          // The type `Denotation[Boxed]`.
          val boxedDenotationType = Types.Denotation

          val latticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)
          val latticeSym: Symbol.DefnSym = lookup(Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Ast.Shared.lattice"), latticeType)

          val boxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)
          val boxSym: Symbol.DefnSym = lookup(Symbol.mkDefnSym(s"Fixpoint${Defs.version}.Ast.Shared.box"), boxType)

          val innerApply = MonoAst.Expr.ApplyDef(latticeSym, List(MonoAst.Expr.Cst(Constant.Unit, Type.Unit, loc)), latticeType, unboxedDenotationType, Type.Pure, loc)
          MonoAst.Expr.ApplyDef(boxSym, List(innerApply), boxType, boxedDenotationType, Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation): MonoAst.Expr = p match {
    case Polarity.Positive =>
      mkTag(Enums.Polarity, "Positive", Nil, Types.Polarity, loc)

    case Polarity.Negative =>
      mkTag(Enums.Polarity, "Negative", Nil, Types.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Fixity` from the given fixity `f`.
    */
  private def mkFixity(f: Fixity, loc: SourceLocation): MonoAst.Expr = f match {
    case Fixity.Loose =>
      mkTag(Enums.Fixity, "Loose", Nil, Types.Fixity, loc)

    case Fixity.Fixed =>
      mkTag(Enums.Fixity, "Fixed", Nil, Types.Fixity, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.GuardX`.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      val sym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, loc)(Scope.Top, flix)
      // Construct a lambda that takes the unit argument.
      val fparam = MonoAst.FormalParam(sym, Type.Unit, Occur.Unknown, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = MonoAst.Expr.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.BodyPredicate, s"Guard0", List(lambdaExp), Types.BodyPredicate, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = MonoAst.FormalParam(freshSym, tpe, Occur.Unknown, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXb(lambdaExp, fvs.map(_._2))

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.Functional`.
    */
  private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Compute the number of in and out variables.
    val numberOfInVars = inVars.length
    val numberOfOutVars = outVars.length

    if (numberOfInVars > 5) {
      throw InternalCompilerException("Does not support more than 5 in variables.", loc)
    }
    if (numberOfOutVars == 0) {
      throw InternalCompilerException("Requires at least one out variable.", loc)
    }
    if (numberOfOutVars > 5) {
      throw InternalCompilerException("Does not support more than 5 out variables.", loc)
    }

    // Introduce a fresh variable for each in variable.
    val freshVars = inVars.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = inVars.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = MonoAst.FormalParam(freshSym, tpe, Occur.Unknown, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXY(outVars, lambdaExp, inVars.map(_._2), exp.tpe, exp.loc)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val boundVarVector = mkVector(outVars.map(mkVarSym), Types.VarSym, loc)
    val freeVarVector = mkVector(inVars.map(kv => mkVarSym(kv._1)), Types.VarSym, loc)
    val innerExp = List(boundVarVector, liftedExp, freeVarVector)
    mkTag(Enums.BodyPredicate, s"Functional", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.HeadTerm.AppX`.
    */
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: MonoAst.Expr, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = MonoAst.FormalParam(freshSym, tpe, Occur.Unknown, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        MonoAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs.map(_._2), exp.tpe)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = liftedExp :: varExps
    mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred): MonoAst.Expr = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = MonoAst.Expr.Cst(Constant.Str(sym), Type.Str, loc)
      val idExp = MonoAst.Expr.Cst(Constant.Int64(0), Type.Int64, loc)
      val inner = List(nameExp, idExp)
      mkTag(Enums.PredSym, "PredSym", inner, Types.PredSym, loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: MonoAst.Expr)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed, loc)
    MonoAst.Expr.ApplyDef(lookup(Defs.Box, tpe), List(exp), tpe, Types.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkVector(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation): MonoAst.Expr = {
    MonoAst.Expr.VectorLit(exps, Type.mkVector(elmType, loc), Type.Pure, loc)
  }

  /**
    * Return a list of quantified variables in the given expression `exp0`.
    *
    * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
    * but not in the constraint params `cparams0` of the constraint.
    */
  private def quantifiedVars(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr): List[(Symbol.VarSym, Type)] = {
    TypedAstOps.freeVars(exp0).toList.filter {
      case (sym, _) => isQuantifiedVar(sym, cparams0)
    }
  }

  /**
    * Returns `true` if the given variable symbol `sym` is a quantified variable according to the given constraint params `cparams0`.
    *
    * That is, the variable symbol is *NOT* lexically bound.
    */
  private def isQuantifiedVar(sym: Symbol.VarSym, cparams0: List[TypedAst.ConstraintParam]): Boolean =
    cparams0.exists(p => p.bnd.sym == sym)

  /*
   * Methods for substitution
   */

  /**
    * Applies the given substitution `subst` to the given expression `exp0`.
    */
  private def substExp(exp0: MonoAst.Expr, subst: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Expr = exp0 match {
    case MonoAst.Expr.Cst(_, _, _) => exp0

    case MonoAst.Expr.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      MonoAst.Expr.Var(s, tpe, loc)

    case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      MonoAst.Expr.Lambda(p, e, tpe, loc)

    case MonoAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case MonoAst.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      MonoAst.Expr.ApplyOp(sym, es, tpe, eff, loc)

    case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case MonoAst.Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      MonoAst.Expr.Let(s, e1, e2, tpe, eff, occur, loc)

    case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      val s = subst.getOrElse(sym, sym)
      val fps = fparams.map(substFormalParam(_, subst))
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      MonoAst.Expr.LocalDef(s, fps, e1, e2, tpe, eff, occur, loc)

    case MonoAst.Expr.Region(sym, regionVar, exp, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e = substExp(exp, subst)
      MonoAst.Expr.Region(s, regionVar, e, tpe, eff, loc)

    case MonoAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case MonoAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.Discard(exp, eff, loc) =>
      val e = substExp(exp, subst)
      MonoAst.Expr.Discard(e, eff, loc)

    case MonoAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case MonoAst.MatchRule(pat, guard, exp1) =>
          val p = substPattern(pat, subst)
          val g = guard.map(substExp(_, subst))
          val e1 = substExp(exp1, subst)
          MonoAst.MatchRule(p, g, e1)
      }
      MonoAst.Expr.Match(e, rs, tpe, eff, loc)

    case MonoAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case MonoAst.ExtMatchRule(pat, exp1, loc1) =>
          val p = substExtPattern(pat, subst)
          val e1 = substExp(exp1, subst)
          MonoAst.ExtMatchRule(p, e1, loc1)
      }
      MonoAst.Expr.ExtMatch(e, rs, tpe, eff, loc)

    case MonoAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      MonoAst.Expr.VectorLit(es, tpe, eff, loc)

    case MonoAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.VectorLength(exp, loc) =>
      val e = substExp(exp, subst)
      MonoAst.Expr.VectorLength(e, loc)

    case MonoAst.Expr.Cast(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      MonoAst.Expr.Cast(e, tpe, eff, loc)

    case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case MonoAst.CatchRule(sym, clazz, exp1) =>
          val s = subst.getOrElse(sym, sym)
          val e1 = substExp(exp1, subst)
          MonoAst.CatchRule(s, clazz, e1)
      }
      MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case MonoAst.Expr.RunWith(exp, effSymUse, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case MonoAst.HandlerRule(opSymUse, fparams, hexp) =>
          val fps = fparams.map(substFormalParam(_, subst))
          val he = substExp(hexp, subst)
          MonoAst.HandlerRule(opSymUse, fps, he)
      }
      MonoAst.Expr.RunWith(e, effSymUse, rs, tpe, eff, loc)

    case MonoAst.Expr.NewObject(_, _, _, _, _, _, _) => exp0

  }

  /**
    * Applies the given substitution `subst` to the given formal param `fparam0`.
    */
  private def substFormalParam(fparam0: MonoAst.FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.FormalParam = fparam0 match {
    case MonoAst.FormalParam(sym, tpe, occur, loc) =>
      val s = subst.getOrElse(sym, sym)
      MonoAst.FormalParam(s, tpe, occur, loc)
  }

  /**
    * Applies the given substitution `subst` to the given pattern `pattern0`.
    */
  private def substPattern(pattern0: MonoAst.Pattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Pattern = pattern0 match {
    case MonoAst.Pattern.Wild(tpe, loc) =>
      MonoAst.Pattern.Wild(tpe, loc)

    case MonoAst.Pattern.Var(sym, tpe, occur, loc) =>
      val s = subst.getOrElse(sym, sym)
      MonoAst.Pattern.Var(s, tpe, occur, loc)

    case MonoAst.Pattern.Cst(cst, tpe, loc) =>
      MonoAst.Pattern.Cst(cst, tpe, loc)

    case MonoAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(substPattern(_, subst))
      MonoAst.Pattern.Tag(symUse, ps, tpe, loc)

    case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
      val ps = pats.map(substPattern(_, subst))
      MonoAst.Pattern.Tuple(ps, tpe, loc)

    case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map(substRecordLabelPattern(_, subst))
      val p = substPattern(pat, subst)
      MonoAst.Pattern.Record(ps, p, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given record label pattern `pattern0`.
    */
  private def substRecordLabelPattern(pattern0: MonoAst.Pattern.Record.RecordLabelPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Pattern.Record.RecordLabelPattern = pattern0 match {
    case MonoAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val p = substPattern(pat, subst)
      MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
  }

  /**
    * Applies the given substitution `subst` to the given ext pattern `pattern0`.
    */
  private def substExtPattern(pattern0: MonoAst.ExtPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.ExtPattern = pattern0 match {
    case MonoAst.ExtPattern.Default(loc) =>
      MonoAst.ExtPattern.Default(loc)

    case MonoAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(substVarOrWild(_, subst))
      MonoAst.ExtPattern.Tag(label, ps, loc)
  }

  /**
    * Applies the given substitution `subst` to the given ext tag pattern `pattern0`.
    */
  private def substVarOrWild(pattern0: MonoAst.ExtTagPattern, subst: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.ExtTagPattern = pattern0 match {
    case MonoAst.ExtTagPattern.Wild(tpe, loc) =>
      MonoAst.ExtTagPattern.Wild(tpe, loc)

    case MonoAst.ExtTagPattern.Var(sym, tpe, occur, loc) =>
      val s = subst.getOrElse(sym, sym)
      MonoAst.ExtTagPattern.Var(s, tpe, occur, loc)

    case MonoAst.ExtTagPattern.Unit(tpe, loc) =>
      MonoAst.ExtTagPattern.Unit(tpe, loc)
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
  private def mkExtVarLambda(preds: List[(Name.Pred, List[Type])], tpe: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val predSymVar = Symbol.freshVarSym("predSym", BoundBy.FormalParam, loc)(Scope.Top, flix)
    val termsVar = Symbol.freshVarSym("terms", BoundBy.FormalParam, loc)(Scope.Top, flix)
    mkLambdaExp(predSymVar, Types.PredSym,
      mkLambdaExp(termsVar, Types.VectorOfBoxed,
        mkExtVarBody(preds, predSymVar, termsVar, tpe, loc),
        tpe, Type.Pure, loc
      ),
      Type.mkPureArrow(Types.VectorOfBoxed, tpe, loc), Type.Pure, loc
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
  private def mkExtVarBody(preds: List[(Name.Pred, List[Type])], predSymVar: Symbol.VarSym, termsVar: Symbol.VarSym, tpe: Type, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val nameVar = Symbol.freshVarSym(Name.Ident("name", loc), BoundBy.Pattern)(Scope.Top, flix)
    MonoAst.Expr.Match(
      exp = MonoAst.Expr.Var(predSymVar, Types.PredSym, loc),
      rules = List(
        MonoAst.MatchRule(
          pat = MonoAst.Pattern.Tag(
            symUse = SymUse.CaseSymUse(Symbol.mkCaseSym(Enums.PredSym, Name.Ident("PredSym", loc)), loc),
            pats = List(
              MonoAst.Pattern.Var(nameVar, Type.Str, Occur.Unknown, loc),
              MonoAst.Pattern.Wild(Type.Int64, loc)
            ),
            tpe = Types.PredSym, loc = loc
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
  private def mkProvenanceMatchRule(termsVar: Symbol.VarSym, tpe: Type, p: Name.Pred, types: List[Type], loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.MatchRule = {
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
  private def mkUnboxedTerm(termsVar: Symbol.VarSym, tpe: Type, i: Int, loc: SourceLocation)(implicit ctx: Context, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = {
    val outerItpe = Type.mkPureUncurriedArrow(List(Types.Boxed), tpe, loc)
    val innerItpe = Type.mkPureUncurriedArrow(List(Type.Int32, Types.VectorOfBoxed), Types.Boxed, loc)
    MonoAst.Expr.ApplyDef(
      sym = lookup(Defs.Unbox, outerItpe),
      exps = List(
        MonoAst.Expr.ApplyDef(
          sym = lookup(Symbol.mkDefnSym(s"Vector.get"), innerItpe),
          exps = List(
            MonoAst.Expr.Cst(Constant.Int32(i), Type.Int32, loc),
            MonoAst.Expr.Var(termsVar, Types.VectorOfBoxed, loc)
          ),
          itpe = innerItpe,
          tpe = Types.Boxed, eff = Type.Pure, loc = loc
        )
      ),
      itpe = outerItpe,
      tpe = tpe, eff = Type.Pure, loc = loc
    )
  }

  /**
    * A local context threaded through `lowerExp` to carry information from an
    * enclosing `NewObject` to nested `InvokeSuperMethod` expressions.
    *
    * @param className The internal name of the enclosing anonymous class (e.g. `"pkg.Anon$1"`).
    *                  Set to `Some` when lowering a `NewObject` method body; `None` otherwise.
    *                  Injected into `AtomicOp.InvokeSuperMethod` so the backend can generate
    *                  the `CHECKCAST` and `INVOKEVIRTUAL super$methodName` instructions.
    * @param thisRef   A `Var` expression referencing the `_this` parameter (the first formal
    *                  parameter of the JvmMethod). Prepended to `InvokeSuperMethod` arguments
    *                  so the backend receives the receiver object as the first expression.
    */
  private case class LocalContext(className: Option[String], thisRef: Option[MonoAst.Expr])

  private object LocalContext {
    val empty: LocalContext = LocalContext(None, None)
  }

}
