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
import ca.uwaterloo.flix.language.ast.{MonoAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.phase.monomorph2.Specialize.{SpecializationTables, StrictSubstitution, specializeFormalParams}
import ca.uwaterloo.flix.language.phase.monomorph2.Symbols.Types
import ca.uwaterloo.flix.util.InternalCompilerException

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
  // TODO: THIS WILL BE FILLED IN PROPERLY LATER.
  private def visitExp(exp0: TypedAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit tables: SpecializationTables, lctx: LocalContext, root: TypedAst.Root, flix: Flix): MonoAst.Expr = ???

  /**
    * Wraps an entry point function with calls to the default handlers of each of the effects appearing in
    * its signature. The order in which the handlers are applied is not defined and should not be relied upon.
    */
  // TODO: THIS WILL BE FILLED IN PROPERLY LATER.
  private def wrapDefWithDefaultHandlers(currentDef: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Def = ???

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
    * Lowers `sym` from a restrictable enum sym into a regular enum sym.
    */
  private[monomorph2] def lowerRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): Symbol.EnumSym =
    new Symbol.EnumSym(None, sym.namespace, sym.name, sym.loc)
}
