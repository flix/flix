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
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, Kind, RigidityEnv, Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.unification.{ClassEnvironment, Substitution, Unification, UnificationError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object Instances {

  /**
    * Validates instances and classes in the given AST root.
    */
  def run(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Unit, CompilationMessage] = flix.phase("Instances") {
    val errs = visitInstances(root, oldRoot, changeSet) ::: visitClasses(root)
    errs match {
      case Nil => ().toSuccess
      case es => Validation.SoftFailure((), LazyList.from(es))
    }
  }

  /**
    * Validates all instances in the given AST root.
    */
  private def visitClasses(root: TypedAst.Root)(implicit flix: Flix): List[InstanceError] = {

    /**
      * Checks that all signatures in `class0` are used in laws if `class0` is marked `lawful`.
      */
    def checkLawApplication(class0: TypedAst.Class): List[InstanceError] = class0 match {
      // Case 1: lawful class
      case TypedAst.Class(_, _, mod, _, _, _, _, sigs, laws, _) if mod.isLawful =>
        val usedSigs = laws.foldLeft(Set.empty[Symbol.SigSym]) {
          case (acc, TypedAst.Def(_, _, TypedAst.Impl(exp, _))) => acc ++ TypedAstOps.sigSymsOf(exp)
        }
        val unusedSigs = sigs.map(_.sym).toSet.removedAll(usedSigs)
        unusedSigs.toList.map {
          sig => InstanceError.UnlawfulSignature(sig, sig.loc)
        }
      // Case 2: non-lawful class
      case TypedAst.Class(_, _, _, _, _, _, _, _, _, _) => Nil
    }

    /**
      * Performs validations on a single class.
      */
    def visitClass(class0: TypedAst.Class): List[InstanceError] = {
      checkLawApplication(class0)
    }

    val results = ParOps.parMap(root.classes.values)(visitClass)
    results.flatten.toList
  }

  /**
    * Validates all instances in the given AST root.
    */
  private def visitInstances(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): List[InstanceError] = {

    /**
      * Checks that an instance is not an orphan.
      * It is declared in either:
      * * The class's companion namespace.
      * * The same namespace as its type.
      */
    def checkOrphan(inst: TypedAst.Instance): List[InstanceError] = inst match {
      case TypedAst.Instance(_, _, _, clazz, tpe, _, _, _, ns, _) => tpe.typeConstructor match {
        // Case 1: Enum type in the same namespace as the instance: not an orphan
        case Some(TypeConstructor.Enum(enumSym, _)) if enumSym.namespace == ns.idents.map(_.name) => Nil
        // Case 2: Any type in the class namespace: not an orphan
        case _ if (clazz.sym.namespace) == ns.idents.map(_.name) => Nil
        // Case 3: Any type outside the class companion namespace and enum declaration namespace: orphan
        case _ => List(InstanceError.OrphanInstance(tpe, clazz.sym, clazz.loc))
      }
    }

    /**
      * Checks that the instance type is simple:
      * * all type variables are unique
      * * all type arguments are variables or booleans
      */
    def checkSimple(inst: TypedAst.Instance): List[InstanceError] = inst match {
      case TypedAst.Instance(_, _, _, clazz, tpe, _, _, _, _, _) => tpe match {
        case _: Type.Cst => Nil
        case _: Type.Var => List(InstanceError.ComplexInstanceType(tpe, clazz.sym, clazz.loc))
        case _: Type.Apply =>
          val (_, errs0) = tpe.typeArguments.foldLeft((List.empty[Type.Var], List.empty[InstanceError])) {
            // Case 1: Type variable
            case ((seen, errs), tvar: Type.Var) =>
              // Case 1.1 We've seen it already. Error.
              if (seen.contains(tvar))
                (seen, List(InstanceError.DuplicateTypeVariableOccurrence(tvar, clazz.sym, clazz.loc)))
              // Case 1.2 We haven't seen it before. Add it to the list.
              else
                (tvar :: seen, errs)
            // Case 2: True. Continue.
            case (acc, Type.Cst(TypeConstructor.True, _)) => acc
            // Case 3: False. Continue.
            case (acc, Type.Cst(TypeConstructor.False, _)) => acc
            // Case 4: Some other type. Error.
            case ((seen, errs), _) => (seen, InstanceError.ComplexInstanceType(tpe, clazz.sym, clazz.loc) :: errs)
          }
          errs0
        case Type.Alias(alias, _, _, _) => List(InstanceError.IllegalTypeAliasInstance(alias.sym, clazz.sym, clazz.loc))
        case Type.AssocType(assoc, _, _, loc) => throw InternalCompilerException("unexpected associated type", loc) // TODO ASSOC-TYPES real error
      }
    }

    /**
      * Checks for overlap of instance types, assuming the instances are of the same class.
      */
    def checkOverlap(inst1: TypedAst.Instance, inst2: TypedAst.Instance)(implicit flix: Flix): List[InstanceError] = {
      Unification.unifyTypes(generifyBools(inst1.tpe), inst2.tpe, RigidityEnv.empty) match {
        case Ok(_) =>
          List(
            InstanceError.OverlappingInstances(inst1.clazz.loc, inst2.clazz.loc),
            InstanceError.OverlappingInstances(inst2.clazz.loc, inst1.clazz.loc)
          )
        case Err(_) => Nil
      }
    }

    /**
      * Converts `true` and `false` in the given type into type variables.
      */
    def generifyBools(tpe0: Type)(implicit flix: Flix): Type = tpe0 match {
      case Type.Cst(TypeConstructor.True, loc) => Type.freshVar(Kind.Bool, loc)
      case Type.Cst(TypeConstructor.False, loc) => Type.freshVar(Kind.Bool, loc)
      case t: Type.Var => t
      case t: Type.Cst => t
      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(generifyBools(tpe1), generifyBools(tpe2), loc)
      case Type.Alias(cst, args, tpe, loc) => Type.Alias(cst, args.map(generifyBools), generifyBools(tpe), loc)
      case Type.AssocType(cst, args, kind, loc) => Type.AssocType(cst, args.map(generifyBools), kind, loc)
    }

    /**
      * Checks that every signature in `clazz` is implemented in `inst`, and that `inst` does not have any extraneous definitions.
      */
    def checkSigMatch(inst: TypedAst.Instance)(implicit flix: Flix): List[InstanceError] = {
      val clazz = root.classes(inst.clazz.sym)

      // Step 1: check that each signature has an implementation.
      val sigMatchVal = clazz.signatures.flatMap {
        sig =>
          (inst.defs.find(_.sym.name == sig.sym.name), sig.impl) match {
            // Case 1: there is no definition with the same name, and no default implementation
            case (None, None) => List(InstanceError.MissingImplementation(sig.sym, inst.clazz.loc))
            // Case 2: there is no definition with the same name, but there is a default implementation
            case (None, Some(_)) => Nil
            // Case 3: there is an implementation marked override, but no default implementation
            case (Some(defn), None) if defn.spec.mod.isOverride => List(InstanceError.IllegalOverride(defn.sym, defn.sym.loc))
            // Case 4: there is an overriding implementation, but no override modifier
            case (Some(defn), Some(_)) if !defn.spec.mod.isOverride => List(InstanceError.UnmarkedOverride(defn.sym, defn.sym.loc))
            // Case 5: there is an implementation with the right modifier
            case (Some(defn), _) =>
              val expectedScheme = Scheme.partiallyInstantiate(sig.spec.declaredScheme, clazz.tparam.sym, inst.tpe, defn.sym.loc)
              if (Scheme.equal(expectedScheme, defn.spec.declaredScheme, root.classEnv, root.eqEnv)) {
                // Case 5.1: the schemes match. Success!
                Nil
              } else {
                // Case 5.2: the schemes do not match
                List(InstanceError.MismatchedSignatures(sig.sym, defn.sym.loc, expectedScheme, defn.spec.declaredScheme))
              }
          }
      }
      // Step 2: check that there are no extra definitions
      val extraDefVal = inst.defs.flatMap {
        defn =>
          clazz.signatures.find(_.sym.name == defn.sym.name) match {
            case None => List(InstanceError.ExtraneousDefinition(defn.sym, defn.sym.loc))
            case _ => Nil
          }
      }

      sigMatchVal ::: extraDefVal
    }

    /**
      * Finds an instance of the class for a given type.
      */
    def findInstanceForType(tpe: Type, clazz: Symbol.ClassSym): Option[(Ast.Instance, Substitution)] = {
      val superInsts = root.classEnv.get(clazz).map(_.instances).getOrElse(Nil)
      // lazily find the instance whose type unifies and save the substitution
      superInsts.iterator.flatMap {
        superInst => Unification.unifyTypes(tpe, superInst.tpe, RigidityEnv.empty).toOption.map {
          case (subst, econstrs) => (superInst, subst) // TODO ASSOC-TYPES consider econstrs
        }
      }.nextOption()
    }

    /**
      * Checks that there is an instance for each super class of the class of `inst`,
      * and that the constraints on `inst` entail the constraints on the super instance.
      */
    def checkSuperInstances(inst: TypedAst.Instance): List[InstanceError] = inst match {
      case TypedAst.Instance(_, _, _, clazz, tpe, tconstrs, _, _, _, _) =>
        val superClasses = root.classEnv(clazz.sym).superClasses
        superClasses flatMap {
          superClass =>
            // Find the instance of the superclass matching the type of this instance.
            findInstanceForType(tpe, superClass) match {
              case Some((superInst, subst)) =>
                // Case 1: An instance matches. Check that its constraints are entailed by this instance.
                superInst.tconstrs flatMap {
                  tconstr =>
                    ClassEnvironment.entail(tconstrs.map(subst.apply), subst(tconstr), root.classEnv) match {
                      case Validation.Success(_) => Nil
                      case failure => failure.errors.map {
                        case UnificationError.NoMatchingInstance(missingTconstr) => InstanceError.MissingConstraint(missingTconstr, superClass, clazz.loc)
                        case _ => throw InternalCompilerException("Unexpected unification error", inst.loc)
                      }
                    }
                }
              case None =>
                // Case 2: No instance matches. Error.
                List(InstanceError.MissingSuperClassInstance(tpe, clazz.sym, superClass, clazz.loc))
            }
        }
    }

    def checkInstance(inst: TypedAst.Instance): List[InstanceError] = {
      val isClassStable = inst.clazz.loc.source.stable
      val isInstanceStable = inst.loc.source.stable
      val isIncremental = changeSet.isInstanceOf[ChangeSet.Changes]
      if (isIncremental && isClassStable && isInstanceStable) {
        return Nil
      }

      checkSigMatch(inst) ::: checkOrphan(inst) ::: checkSuperInstances(inst)
    }

    /**
      * Reassembles a set of instances of the same class.
      */
    def checkInstancesOfClass(insts0: List[TypedAst.Instance]): List[InstanceError] = {
      val insts = insts0
      // Check each instance against each instance that hasn't been checked yet
      val checks = insts.tails.toList
      checks flatMap {
        case inst :: unchecked =>
          // check that the instance is on a valid type, suppressing other errors if not
          checkSimple(inst) match {
            case Nil => unchecked.flatMap(checkOverlap(_, inst)) ::: checkInstance(inst)
            case errs => errs
          }
        case Nil => Nil
      }
    }

    // Check the instances of each class in parallel.
    val results = ParOps.parMap(root.instances.values)(checkInstancesOfClass)
    results.flatten.toList
  }
}
