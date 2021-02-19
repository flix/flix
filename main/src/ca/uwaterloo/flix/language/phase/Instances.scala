/*
 * Copyright 2020 Matthew Lutze
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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object Instances extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Validates instances and classes in the given AST root.
    */
  override def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Instances") {
    Validation.sequenceX(List(
      visitInstances(root),
      visitClasses(root)
    )).map(_ => root)
  }

  /**
    * Validates all instances in the given AST root.
    */
  private def visitClasses(root: TypedAst.Root)(implicit flix: Flix): Validation[Unit, InstanceError] = {

    /**
      * Checks that every super class of `class0` is lawful, unless `class0` is marked `lawless`.
      */
    def checkLawfulSuperClasses(class0: TypedAst.Class): Validation[Unit, InstanceError] = class0 match {
      case TypedAst.Class(_, mod, _, _, _, _, _, _) if mod.isLawless => ().toSuccess
      case TypedAst.Class(_, _, sym, _, superClasses, _, _, loc) =>
        val lawlessSuperClasses = superClasses.filter {
          superSym => root.classes(superSym).mod.isLawless
        }
        Validation.traverseX(lawlessSuperClasses) {
          superSym => InstanceError.LawlessSuperClass(sym, superSym, loc).toFailure
        }
    }

    /**
      * Checks that all signatures in `class0` are used in laws, unless `class0` is marked `lawless`.
      */
    def checkLawApplication(class0: TypedAst.Class): Validation[Unit, InstanceError] = class0 match {
      case TypedAst.Class(_, mod, _, _, _, _, _, _) if mod.isLawless => ().toSuccess
      case TypedAst.Class(_, _, _, _, _, sigs, laws, _) =>
        val usedSigs = laws.foldLeft(Set.empty[Symbol.SigSym]) {
          case (acc, TypedAst.Def(_, _, _, _, _, _, exp, _, _, _, _)) => acc ++ TypedAstOps.sigSymsOf(exp)
        }
        val unusedSigs = sigs.map(_.sym).toSet.removedAll(usedSigs)
        Validation.traverseX(unusedSigs) {
          sig => InstanceError.UnlawfulSignature(sig, sig.loc).toFailure
        }
    }

    /**
      * Performs validations on a single class.
      */
    def visitClass(class0: TypedAst.Class): Validation[Unit, InstanceError] = {
      Validation.sequenceX(List(
        checkLawfulSuperClasses(class0),
        checkLawApplication(class0)
      ))
    }

    val results = ParOps.parMap(root.classes.values, visitClass)
    Validation.sequenceX(results)
  }

  /**
    * Validates all instances in the given AST root.
    */
  private def visitInstances(root: TypedAst.Root)(implicit flix: Flix): Validation[Unit, InstanceError] = {

    /**
      * Checks that an instance is not an orphan.
      * It is declared in either:
      * * The class's companion namespace.
      * * The same namespace as its type.
      */
    def checkOrphan(inst: TypedAst.Instance): Validation[Unit, InstanceError] = inst match {
      case TypedAst.Instance(_, _, classSym, tpe, _, _, ns, loc) => tpe.typeConstructor match {
        // Case 1: Enum type in the same namespace as the instance: not an orphan
        case Some(TypeConstructor.Enum(enumSym, _)) if enumSym.namespace == ns.idents.map(_.name) => ().toSuccess
        // Case 2: Any type in the class namespace: not an orphan
        case _ if (classSym.namespace) == ns.idents.map(_.name) => ().toSuccess
        // Case 3: Any type outside the class companion namespace and enum declaration namespace: orphan
        case _ => InstanceError.OrphanInstance(tpe, classSym, loc).toFailure
      }
    }

    /**
      * Checks that the instance type is simple:
      * * all type variables are unique
      * * all type arguments are variables
      */
    def checkSimple(inst: TypedAst.Instance): Validation[Unit, InstanceError] = inst match {
      case TypedAst.Instance(_, _, sym, tpe, _, _, _, loc) => tpe match {
        case _: Type.Cst => ().toSuccess
        case _: Type.Var => InstanceError.ComplexInstanceType(tpe, sym, loc).toFailure
        case _: Type.Lambda => throw InternalCompilerException("Unexpected lambda type.")
        case _: Type.Apply =>
          Validation.fold(tpe.typeArguments, List.empty[Type.Var]) {
            // Case 1: Type variable
            case (seen, tvar: Type.Var) =>
              // Case 1.1 We've seen it already. Error.
              if (seen.contains(tvar))
                InstanceError.DuplicateTypeVariableOccurrence(tvar, sym, loc).toFailure
              // Case 1.2 We haven't seen it before. Add it to the list.
              else
                (tvar :: seen).toSuccess
            // Case 2: Non-type variable. Error.
            case (_, _) => InstanceError.ComplexInstanceType(tpe, sym, loc).toFailure
          }.map(_ => ())
      }
    }

    /**
      * Checks for overlap of instance types, assuming the instances are of the same class.
      */
    def checkOverlap(inst1: TypedAst.Instance, inst2: TypedAst.Instance)(implicit flix: Flix): Validation[Unit, InstanceError] = {
      Unification.unifyTypes(inst1.tpe, inst2.tpe) match {
        case Ok(_) =>
          Validation.Failure(LazyList(
            InstanceError.OverlappingInstances(inst1.loc, inst2.loc),
            InstanceError.OverlappingInstances(inst2.loc, inst1.loc)
          ))
        case Err(_) => ().toSuccess
      }
    }

    /**
      * Checks that every signature in `clazz` is implemented in `inst`, and that `inst` does not have any extraneous definitions.
      */
    def checkSigMatch(inst: TypedAst.Instance)(implicit flix: Flix): Validation[Unit, InstanceError] = {
      val clazz = root.classes(inst.sym)

      // Step 1: check that each signature has an implementation.
      val sigMatchVal = Validation.traverseX(clazz.signatures) {
        sig =>
          (inst.defs.find(_.sym.name == sig.sym.name), sig.exp) match {
            // Case 1: there is no definition with the same name, and no default implementation
            case (None, None) => InstanceError.MissingImplementation(sig.sym, inst.loc).toFailure
            // Case 2: there is no definition with the same name, but there is a default implementation
            case (None, Some(_)) => ().toSuccess
            // Case 3: there is an implementation marked override, but no default implementation
            case (Some(defn), None) if defn.mod.isOverride => InstanceError.IllegalOverride(defn.sym, defn.loc).toFailure
            // Case 4: there is an overriding implementation, but no override modifier
            case (Some(defn), Some(_)) if !defn.mod.isOverride => InstanceError.UnmarkedOverride(defn.sym, defn.loc).toFailure
            // Case 5: there is an implementation with the right modifier
            case (Some(defn), _) =>
              val expectedScheme = Scheme.partiallyInstantiate(sig.sc, clazz.tparam.tpe, inst.tpe)
              if (Scheme.equal(expectedScheme, defn.declaredScheme, root.classEnv)) {
                // Case 5.1: the schemes match. Success!
                ().toSuccess
              } else {
                // Case 5.2: the schemes do not match
                InstanceError.MismatchedSignatures(sig.sym, defn.loc, expectedScheme, defn.declaredScheme).toFailure
              }
          }
      }
      // Step 2: check that there are no extra definitions
      val extraDefVal = Validation.traverseX(inst.defs) {
        defn =>
          clazz.signatures.find(_.sym.name == defn.sym.name) match {
            case None => InstanceError.ExtraneousDefinition(defn.sym, defn.loc).toFailure
            case _ => ().toSuccess
          }
      }

      Validation.sequenceX(List(sigMatchVal, extraDefVal))
    }

    /**
      * Checks that there is an instance for each super class of the class of `inst`.
      */
    def checkSuperInstances(inst: TypedAst.Instance): Validation[Unit, InstanceError] = inst match {
      case TypedAst.Instance(_, _, sym, tpe, _, _, _, loc) =>
        val superClasses = root.classEnv(sym).superClasses
        Validation.traverseX(superClasses) {
          superClass =>
            val superInsts = root.classEnv.get(superClass).map(_.instances).getOrElse(Nil)
            // Check each instance of the super class
            if (superInsts.exists(superInst => Unification.unifiesWith(tpe, superInst.tpe))) {
              // Case 1: An instance matches. Success.
              ().toSuccess
            } else {
              // Case 2: No instance matches. Error.
              InstanceError.MissingSuperClassInstance(tpe, sym, superClass, loc).toFailure
            }
        }
    }

    /**
      * Reassembles a set of instances of the same class.
      */
    def checkInstancesOfClass(insts0: List[TypedAst.Instance]): Validation[Unit, InstanceError] = {
      val insts = insts0
      // Check each instance against each instance that hasn't been checked yet
      val checks = insts.tails.toSeq
      Validation.traverseX(checks) {
        case inst :: unchecked =>
          Validation.sequenceX(List(
            checkSimple(inst),
            Validation.traverse(unchecked)(checkOverlap(_, inst)),
            checkSigMatch(inst),
            checkOrphan(inst),
            checkSuperInstances(inst),
          ))

        case Nil => ().toSuccess
      }
    }

    // Check the instances of each class in parallel.
    val results = ParOps.parMap(root.instances.values, checkInstancesOfClass)
    Validation.traverseX(results)(identity)
  }
}
