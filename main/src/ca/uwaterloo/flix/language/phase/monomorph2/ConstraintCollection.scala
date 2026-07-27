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
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.FormalParam
import ca.uwaterloo.flix.util.ParOps

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
  private class Context {
    private val flows: mutable.ArrayBuffer[Flow] = mutable.ArrayBuffer.empty

    /** Emits `flow` as one of the generated constraints. */
    def addFlow(flow: Flow): Unit = synchronized { flows.addOne(flow) }

    /** Returns every flow emitted so far. */
    def result: Set[Flow] = synchronized { flows.toSet }
  }

  /**
    * Generates specialization constraints for every top-level declaration in `root`.
    */
  def generate(root0: TypedAst.Root)(implicit flix: Flix): Set[Flow] = {
    implicit val ctx: Context = new Context()
    implicit val root: TypedAst.Root = root0

    val fromDefs: Set[Flow] = ???

    ParOps.parMap(root.enums.values) { enm =>
      val mvar = MonoVar.Enum(enm.sym)
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = enm.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      visitEnum(enm)
    }

    val fromInstances: Set[Flow] = ???

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

    val fromSigs: Set[Flow] = ???

    ParOps.parMap(root.effects.values.flatMap(_.ops)) { op =>
      // We need a Synthetic DefnSym to tie the tparams to
      val defnSym = new Symbol.DefnSym(None, op.sym.namespace, op.sym.name, op.sym.loc)
      val mvar = MonoVar.Def(defnSym)
      implicit val tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg] = op.spec.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      op.spec.fparams.foreach { case FormalParam(_, tpe, _, _, _) => visitType(tpe) }
      visitType(op.spec.retTpe)
    }

    ctx.result ++ fromDefs ++ fromInstances ++ fromSigs
  }

  /**
    * Emits flow constraints for enum type applications occurring in `tpe`.
    */
  private def visitType(tpe0: Type)(implicit ctx: Context, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit = {
    def dealiasedVisitType(tpe: Type): Unit = tpe match {
      case at @ Type.AssocType(_, arg, _, _) =>
        if (at.typeVars.isEmpty)
          visitType(MonomorphCanon.reduceAssocType(at)(root, flix))
        else
          dealiasedVisitType(arg)
      case _: Type.BaseType
           | Type.Var(_, _)
           | Type.Cst(_, _) => ()
      case app @ Type.Apply(_, _, _) =>
        val args = app.typeArguments
        args.foreach(dealiasedVisitType)
        val mvarOpt = app.baseType match {
          case Type.Cst(TypeConstructor.Enum(sym, _), _)             => Some(MonoVar.Enum(sym))
          case Type.Cst(TypeConstructor.RestrictableEnum(sym, _), _) => Some(MonoVar.RestrictableEnum(sym))
          case Type.Cst(TypeConstructor.Struct(sym, _), _)           => Some(MonoVar.Struct(sym))
          case _                                                     => None
        }
        mvarOpt.foreach(mvar => ctx.addFlow(Flow(args.map(t => dealiasedTypeToMonoArg(t)), mvar)))
    }
    dealiasedVisitType(Type.eraseAliases(tpe0))
  }

  /**
    * Emits flow constraints for all case field types in `enumDecl`.
    */
  private def visitEnum(enumDecl: TypedAst.Enum)(implicit ctx: Context, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    enumDecl.cases.values.foreach(cas => cas.tpes.foreach(visitType(_)))

  /**
    * Emits flow constraints for all case field types in `restrictableEnumDecl`.
    */
  private def visitRestrictableEnum(restrictableEnumDecl: TypedAst.RestrictableEnum)(implicit ctx: Context, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    restrictableEnumDecl.cases.values.foreach(cas => cas.tpes.foreach(visitType(_)))

  /**
    * Emits flow constraints for all field types in `structDecl`.
    */
  private def visitStruct(structDecl: TypedAst.Struct)(implicit ctx: Context, tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg], root: TypedAst.Root, flix: Flix): Unit =
    structDecl.fields.values.foreach(field => visitType(field.tpe))

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
      case Type.Cst(_, _) | _: Type.BaseType =>
        MonoArg.Const(tpe)
      case Type.Apply(_, _, _) =>
        if (tpe.kind == Kind.Eff && tpe.typeVars.isEmpty)
          MonoArg.Const(MonomorphCanon.simplify(tpe, isGround = true)(root, flix))
        else {
          MonoArg.App(dealiasedTypeToMonoArg(tpe.baseType), tpe.typeArguments.map(arg => dealiasedTypeToMonoArg(arg)))
        }
      case other =>
        MonoArg.Const(other)
    }

}
