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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.unification.{ClassEnvironment, Substitution, Unification, UnificationError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object Instances {

  /**
    * Validates instances and classes in the given AST root.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationMessage] = flix.phase("Instances") {
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
      * Checks that all signatures in `class0` are used in laws, unless `class0` is marked `lawless`.
      */
    def checkLawApplication(class0: TypedAst.Class): Validation[Unit, InstanceError] = class0 match {
      case TypedAst.Class(_, _, mod, _, _, _, _, _, _) if mod.isLawless => ().toSuccess
      case TypedAst.Class(_, _, _, _, _, _, sigs, laws, _) =>
        val usedSigs = laws.foldLeft(Set.empty[Symbol.SigSym]) {
          case (acc, TypedAst.Def(_, _, TypedAst.Impl(exp, _))) => acc ++ TypedAstOps.sigSymsOf(exp)
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
      checkLawApplication(class0)
    }

    val results = ParOps.parMap(root.classes.values)(visitClass)
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
      case TypedAst.Instance(_, _, sym, tpe, _, _, ns, _) => tpe.typeConstructor match {
        // Case 1: Enum type in the same namespace as the instance: not an orphan
        case Some(TypeConstructor.KindedEnum(enumSym, _)) if enumSym.namespace == ns.idents.map(_.name) => ().toSuccess
        // Case 2: Any type in the class namespace: not an orphan
        case _ if (sym.clazz.namespace) == ns.idents.map(_.name) => ().toSuccess
        // Case 3: Any type outside the class companion namespace and enum declaration namespace: orphan
        case _ => InstanceError.OrphanInstance(tpe, sym, sym.loc).toFailure
      }
    }

    /**
      * Checks that the instance type is simple:
      * * all type variables are unique
      * * all type arguments are variables or booleans
      */
    def checkSimple(inst: TypedAst.Instance): Validation[Unit, InstanceError] = inst match {
      case TypedAst.Instance(_, _, sym, tpe, _, _, _, _) => tpe match {
        case _: Type.Cst => ().toSuccess
        case _: Type.KindedVar => InstanceError.ComplexInstanceType(tpe, sym, sym.loc).toFailure
        case _: Type.Apply =>
          Validation.fold(tpe.typeArguments, List.empty[Type.KindedVar]) {
            // Case 1: Type variable
            case (seen, tvar: Type.KindedVar) =>
              // Case 1.1 We've seen it already. Error.
              if (seen.contains(tvar))
                InstanceError.DuplicateTypeVariableOccurrence(tvar, sym, sym.loc).toFailure
              // Case 1.2 We haven't seen it before. Add it to the list.
              else
                (tvar :: seen).toSuccess
            // Case 2: True. Continue.
            case (seen, Type.Cst(TypeConstructor.True, _)) => seen.toSuccess
            // Case 3: False. Continue.
            case (seen, Type.Cst(TypeConstructor.False, _)) => seen.toSuccess
            // Case 4: Some other type. Error.
            case (_, _) => InstanceError.ComplexInstanceType(tpe, sym, sym.loc).toFailure
          }.map(_ => ())
        case Type.Alias(alias, _, _, _) => InstanceError.IllegalTypeAliasInstance(alias.sym, sym, sym.loc).toFailure
        case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type.")
        case _: Type.Ascribe => throw InternalCompilerException("Unexpected ascribe type.")
      }
    }

    /**
      * Checks for overlap of instance types, assuming the instances are of the same class.
      */
    def checkOverlap(inst1: TypedAst.Instance, inst2: TypedAst.Instance)(implicit flix: Flix): Validation[Unit, InstanceError] = {
      Unification.unifyTypes(generifyBools(inst1.tpe), inst2.tpe) match {
        case Ok(_) =>
          Validation.Failure(LazyList(
            InstanceError.OverlappingInstances(inst1.sym.loc, inst2.sym.loc),
            InstanceError.OverlappingInstances(inst2.sym.loc, inst1.sym.loc)
          ))
        case Err(_) => ().toSuccess
      }
    }

    /**
      * Converts `true` and `false` in the given type into type variables.
      */
    def generifyBools(tpe0: Type)(implicit flix: Flix): Type = tpe0 match {
      case Type.Cst(TypeConstructor.True, loc) => Type.freshVar(Kind.Bool, loc)
      case Type.Cst(TypeConstructor.False, loc) => Type.freshVar(Kind.Bool, loc)
      case t: Type.KindedVar => t
      case t: Type.Cst => t
      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(generifyBools(tpe1), generifyBools(tpe2), loc)
      case Type.Alias(cst, args, tpe, loc) => Type.Alias(cst, args.map(generifyBools), generifyBools(tpe), loc)
      case _: Type.UnkindedVar => throw InternalCompilerException("unexpected unkinded type")
      case _: Type.Ascribe => throw InternalCompilerException("unexpected unkinded type")
    }

    /**
      * Checks that every signature in `clazz` is implemented in `inst`, and that `inst` does not have any extraneous definitions.
      */
    def checkSigMatch(inst: TypedAst.Instance)(implicit flix: Flix): Validation[Unit, InstanceError] = {
      val clazz = root.classes(inst.sym.clazz)

      // Step 1: check that each signature has an implementation.
      val sigMatchVal = Validation.traverseX(clazz.signatures) {
        sig =>
          (inst.defs.find(_.sym.name == sig.sym.name), sig.impl) match {
            // Case 1: there is no definition with the same name, and no default implementation
            case (None, None) => InstanceError.MissingImplementation(sig.sym, inst.sym.loc).toFailure
            // Case 2: there is no definition with the same name, but there is a default implementation
            case (None, Some(_)) => ().toSuccess
            // Case 3: there is an implementation marked override, but no default implementation
            case (Some(defn), None) if defn.spec.mod.isOverride => InstanceError.IllegalOverride(defn.sym, defn.sym.loc).toFailure
            // Case 4: there is an overriding implementation, but no override modifier
            case (Some(defn), Some(_)) if !defn.spec.mod.isOverride => InstanceError.UnmarkedOverride(defn.sym, defn.sym.loc).toFailure
            // Case 5: there is an implementation with the right modifier
            case (Some(defn), _) =>
              val expectedScheme = Scheme.partiallyInstantiate(sig.spec.declaredScheme, clazz.tparam.sym, inst.tpe)
              if (Scheme.equal(expectedScheme, defn.spec.declaredScheme, root.classEnv)) {
                // Case 5.1: the schemes match. Success!
                ().toSuccess
              } else {
                // Case 5.2: the schemes do not match
                InstanceError.MismatchedSignatures(sig.sym, defn.sym.loc, expectedScheme, defn.spec.declaredScheme).toFailure
              }
          }
      }
      // Step 2: check that there are no extra definitions
      val extraDefVal = Validation.traverseX(inst.defs) {
        defn =>
          clazz.signatures.find(_.sym.name == defn.sym.name) match {
            case None => InstanceError.ExtraneousDefinition(defn.sym, defn.sym.loc).toFailure
            case _ => ().toSuccess
          }
      }

      Validation.sequenceX(List(sigMatchVal, extraDefVal))
    }

    /**
      * Finds an instance of the class for a given type.
      */
    def findInstanceForType(tpe: Type, clazz: Symbol.ClassSym): Option[(Ast.Instance, Substitution)] = {
        val superInsts = root.classEnv.get(clazz).map(_.instances).getOrElse(Nil)
        // lazily find the instance whose type unifies and save the substitution
        superInsts.iterator.flatMap {
          superInst => Unification.unifyTypes(tpe, superInst.tpe).toOption.map((superInst, _))
        }.nextOption()
    }

    /**
      * Checks that there is an instance for each super class of the class of `inst`,
      * and that the constraints on `inst` entail the constraints on the super instance.
      */
    def checkSuperInstances(inst: TypedAst.Instance): Validation[Unit, InstanceError] = inst match {
      case TypedAst.Instance(_, _, sym, tpe, tconstrs, _, _, _) =>
        val superClasses = root.classEnv(sym.clazz).superClasses
        Validation.traverseX(superClasses) {
          superClass =>
            // Find the instance of the superclass matching the type of this instance.
            findInstanceForType(tpe, superClass) match {
              case Some((superInst, subst)) =>
                // Case 1: An instance matches. Check that its constraints are entailed by this instance.
                Validation.traverseX(superInst.tconstrs) {
                  tconstr =>
                    ClassEnvironment.entail(tconstrs.map(subst.apply), subst(tconstr), root.classEnv) match {
                      case Validation.Failure(errors) => Validation.Failure(errors.map {
                        case UnificationError.NoMatchingInstance(missingTconstr) => InstanceError.MissingConstraint(missingTconstr, superClass, sym.loc)
                        case _ => throw InternalCompilerException("Unexpected unification error")
                      })
                      case Validation.Success(_) => ().toSuccess
                    }
                }
              case None =>
                // Case 2: No instance matches. Error.
                InstanceError.MissingSuperClassInstance(tpe, sym, superClass, sym.loc).toFailure
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
          // check that the instance is on a valid type, suppressing other errors if not
          checkSimple(inst) andThen {
            _ =>
              Validation.sequenceX(List(
                Validation.traverse(unchecked)(checkOverlap(_, inst)),
                checkSigMatch(inst),
                checkOrphan(inst),
                checkSuperInstances(inst),
              ))
          }

        case Nil => ().toSuccess
      }
    }

    // Check the instances of each class in parallel.
    val results = ParOps.parMap(root.instances.values)(checkInstancesOfClass)
    Validation.traverseX(results)(identity)
  }
}
