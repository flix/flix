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

/**
  * Constraint generation for constraint-based monomorphization: emits `Flow` constraints
  * describing how concrete types propagate through the program, for [[ConstraintSolver]] to solve.
  */
object ConstraintCollection {

  /**
    * Generation context.
    *
    * @param tparamEnv maps the current declaration's type parameters to `MonoArg.Param` — always
    *                  `Param`, not statically enforced, so a future scope could use more than one
    *                  `MonoVar`.
    * @param root the typed AST root (needed to resolve ground associated types)
    * @param flix the Flix context (needed by TypeReduction2)
    */
  case class Context(
    tparamEnv: Map[Symbol.KindedTypeVarSym, MonoArg],
    root: TypedAst.Root,
    flix: Flix
  )

  /**
    * Generates specialization constraints for every top-level declaration in `root`.
    */
  def generate(root: TypedAst.Root)(implicit flix: Flix): Set[Flow] = {
    val fromDefs: Set[Flow] = ???

    val fromEnums = ParOps.parMap(root.enums.values) { enm =>
      val mvar = MonoVar.Enum(enm.sym)
      val tparamEnv = enm.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      implicit val ctx: Context = Context(tparamEnv, root, flix)
      visitEnum(enm, Nil)
    }.flatten.toSet

    val fromInstances: Set[Flow] = ???

    val fromRestrictableEnums = ParOps.parMap(root.restrictableEnums.values) { enm =>
      val mvar = MonoVar.RestrictableEnum(enm.sym)
      val tparamEnv = (enm.index :: enm.tparams).zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      implicit val ctx: Context = Context(tparamEnv, root, flix)
      visitRestrictableEnum(enm, Nil)
    }.flatten.toSet

    val fromStructs = ParOps.parMap(root.structs.values) { struct =>
      val mvar = MonoVar.Struct(struct.sym)
      val tparamEnv = struct.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      implicit val ctx: Context = Context(tparamEnv, root, flix)
      visitStruct(struct, Nil)
    }.flatten.toSet

    val fromSigs: Set[Flow] = ???

    val fromEffects = ParOps.parMap(root.effects.values.flatMap(_.ops)) { op =>
      // We need a Synthetic DefnSym to tie the tparams to
      val defnSym = new Symbol.DefnSym(None, op.sym.namespace, op.sym.name, op.sym.loc)
      val mvar = MonoVar.Def(defnSym)
      val tparamEnv = op.spec.tparams.zipWithIndex.map { case (tp, i) => tp.sym -> MonoArg.Param(mvar, i) }.toMap
      implicit val ctx: Context = Context(tparamEnv, root, flix)
      val acc1 = op.spec.fparams.foldLeft(List.empty[Flow]) { case (a, FormalParam(_, tpe, _, _, _)) => visitType(tpe, a) }
      visitType(op.spec.retTpe, acc1)
    }.flatten.toSet

    fromDefs ++ fromEnums ++ fromInstances ++ fromRestrictableEnums ++ fromStructs ++ fromSigs ++ fromEffects
  }

  /**
    * Emits flow constraints for enum type applications occurring in `tpe`.
    */
  private def visitType(tpe0: Type, acc: List[Flow])(implicit ctx: Context): List[Flow] = Type.eraseAliases(tpe0) match {
    case at @ Type.AssocType(_, arg, _, _) =>
      if (at.typeVars.isEmpty)
        visitType(MonomorphCanon.reduceAssocType(at)(ctx.root, ctx.flix), acc)
      else
        visitType(arg, acc)
    case _: Type.BaseType
         | Type.Var(_, _)
         | Type.Cst(_, _) => acc
    case app @ Type.Apply(_, _, _) =>
      val args = app.typeArguments
      val acc1 = args.foldLeft(acc)((a, t) => visitType(t, a))
      val mvarOpt = app.baseType match {
        case Type.Cst(TypeConstructor.Enum(sym, _), _)             => Some(MonoVar.Enum(sym))
        case Type.Cst(TypeConstructor.RestrictableEnum(sym, _), _) => Some(MonoVar.RestrictableEnum(sym))
        case Type.Cst(TypeConstructor.Struct(sym, _), _)           => Some(MonoVar.Struct(sym))
        case _                                                     => None
      }
      mvarOpt match {
        case Some(mvar) => Flow(args.map(t => typeToMonoArg(t)), mvar) :: acc1
        case None       => acc1
      }
  }

  /**
    * Emits flow constraints for all case field types in `enumDecl`.
    */
  private def visitEnum(enumDecl: TypedAst.Enum, acc: List[Flow])(implicit ctx: Context): List[Flow] =
    enumDecl.cases.values.foldLeft(acc) { (a, cas) => cas.tpes.foldLeft(a)((a2, t) => visitType(t, a2)) }

  /**
    * Emits flow constraints for all case field types in `restrictableEnumDecl`.
    */
  private def visitRestrictableEnum(restrictableEnumDecl: TypedAst.RestrictableEnum, acc: List[Flow])(implicit ctx: Context): List[Flow] =
    restrictableEnumDecl.cases.values.foldLeft(acc) { (a, cas) => cas.tpes.foldLeft(a)((a2, t) => visitType(t, a2)) }

  /**
    * Emits flow constraints for all field types in `structDecl`.
    */
  private def visitStruct(structDecl: TypedAst.Struct, acc: List[Flow])(implicit ctx: Context): List[Flow] =
    structDecl.fields.values.foldLeft(acc) { (a, field) => visitType(field.tpe, a) }

  /** Converts `tpe0` to a `MonoArg` relative to the current declaration context. */
  private def typeToMonoArg(tpe0: Type)(implicit ctx: Context): MonoArg = {
    val tpe = Type.eraseAliases(tpe0)
    tpe match {
      case Type.Var(sym, _) =>
        // A type variable that is not a tparam of the current decl (absent from tparamEnv) — e.g.
        // a region var introduced by `region r { ... }`. We record it as an opaque constant so the
        // flow is still emitted but the solver does not propagate it.
        ctx.tparamEnv.getOrElse(sym, MonoArg.Const(tpe))
      case at @ Type.AssocType(symUse, arg, kind, assocLoc) =>
        if (tpe.typeVars.isEmpty)
          MonoArg.Const(MonomorphCanon.reduceAssocType(at)(ctx.root, ctx.flix))
        else
          MonoArg.Assoc(symUse.sym, typeToMonoArg(arg), kind, assocLoc)
      case Type.Cst(_, _) | _: Type.BaseType =>
        MonoArg.Const(tpe)
      case Type.Apply(_, _, _) =>
        if (tpe.kind == Kind.Eff && tpe.typeVars.isEmpty)
          MonoArg.Const(MonomorphCanon.simplify(tpe, isGround = true)(ctx.root, ctx.flix))
        else {
          MonoArg.App(typeToMonoArg(tpe.baseType), tpe.typeArguments.map(arg => typeToMonoArg(arg)))
        }
      case other =>
        MonoArg.Const(other)
    }
  }

}
