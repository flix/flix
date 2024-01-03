/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Validation.{flatMapN, mapN, traverse}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListMap

object Typer2 {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.phase("Typer") {
    flatMapN(TypeInference.run(root, oldRoot, changeSet)) {
      case (defSubsts, sigSubsts) =>
        TypeReconstruction.run(root, defSubsts, sigSubsts, oldRoot, changeSet)
    }
  }

  /**
    * Reconstructs types in the given defs.
    */
  private def visitDefs(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] = {
    flix.subphase("Defs") {
      val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)
      ParOps.parTraverseValues(staleDefs) {
        case defn => visitDef(defn, assumedTconstrs = Nil, root, classEnv, eqEnv)
      }.map(_ ++ freshDefs)
    }
  }

  /**
    * Reconstructs types in the given def.
    */
  private def visitDef(defn: KindedAst.Def, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = {
    val substVal = TypeInference.visitDefn(defn, assumedTconstrs, root, classEnv, eqEnv)
    mapN(substVal) {
      case subst => TypeReconstruction.visitDef(defn, root, subst)
    }
  }

  /**
    * Performs type inference and reassembly on all classes in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitClasses(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.ClassSym, TypedAst.Class], TypeError] =
    flix.subphase("Classes") {
      val (staleClasses, freshClasses) = changeSet.partition(root.classes, oldRoot.classes)
      ParOps.parTraverseValues(staleClasses)(visitClass(_, root, classEnv, eqEnv)).map(_ ++ freshClasses)
    }

  /**
    * Reassembles a single class.
    */
  private def visitClass(clazz: KindedAst.Class, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Class, TypeError] = clazz match {
    case KindedAst.Class(doc, ann, mod, sym, tparam0, superClasses0, assocs0, sigs0, laws0, loc) =>
      val tparam = TypeReconstruction.visitTypeParam(tparam0, root) // TODO ASSOC-TYPES redundant?
      val superClasses = superClasses0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeSig(doc, mod, sym, tp0, kind, loc) =>
          val tp = TypeReconstruction.visitTypeParam(tp0, root) // TODO ASSOC-TYPES redundant?
          TypedAst.AssocTypeSig(doc, mod, sym, tp, kind, loc) // TODO ASSOC-TYPES trivial
      }
      val tconstr = Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), Type.Var(tparam.sym, tparam.loc), sym.loc)
      val sigsVal = traverse(sigs0.values)(visitSig(_, List(tconstr), root, classEnv, eqEnv))
      val lawsVal = traverse(laws0)(visitDef(_, List(tconstr), root, classEnv, eqEnv))
      mapN(sigsVal, lawsVal) {
        case (sigs, laws) => TypedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, laws, loc)
      }
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: KindedAst.Sig, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Sig, TypeError] = {
    val substVal = TypeInference.visitSig(sig, assumedTconstrs, root, classEnv, eqEnv)
    mapN(substVal) {
      case subst => TypeReconstruction.visitSig(sig, root, subst)
    }
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[ListMap[Symbol.ClassSym, TypedAst.Instance], TypeError] =
    flix.subphase("Instances") {

      val instances0 = for {
        (_, insts) <- root.instances
        inst <- insts
      } yield inst

      val instancesVal = ParOps.parTraverse(instances0)(visitInstance(_, root, classEnv, eqEnv))

      mapN(instancesVal) {
        case instances =>
          val map = instances.map {
            case instance => instance.clazz.sym -> instance
          }
          ListMap.from(map)
      }
    }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Instance, TypeError] = inst match {
    case KindedAst.Instance(doc, ann, mod, sym, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tpe = tpe0 // TODO ASSOC-TYPES redundant?
      val tconstrs = tconstrs0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) =>
          TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) // TODO ASSOC-TYPES trivial
      }
      val defsVal = Validation.traverse(defs0)(visitDef(_, tconstrs, root, classEnv, eqEnv))
      mapN(defsVal) {
        case defs => TypedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs, defs, ns, loc)
      }
  }

}
