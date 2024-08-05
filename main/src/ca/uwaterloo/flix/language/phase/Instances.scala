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
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, RigidityEnv, Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.unification.{Substitution, TraitEnvironment, Unification, UnificationError}
import ca.uwaterloo.flix.util.collection.ListOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result, Validation}

object Instances {

  /**
    * Validates instances and traits in the given AST root.
    */
  def run(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, InstanceError] =
    flix.phase("Instances") {
      val errors = visitInstances(root, oldRoot, changeSet) ::: visitTraits(root)

      Validation.toSuccessOrSoftFailure(root, errors)
    }(DebugValidation())

  /**
    * Validates all instances in the given AST root.
    */
  private def visitTraits(root: TypedAst.Root)(implicit flix: Flix): List[InstanceError] = {
    val results = ParOps.parMap(root.traits.values)(visitTrait)
    results.flatten.toList
  }

  /**
    * Checks that all signatures in `trait0` are used in laws if `trait0` is marked `lawful`.
    */
  private def checkLawApplication(trait0: TypedAst.Trait): List[InstanceError] = trait0 match {
    // Case 1: lawful trait
    case TypedAst.Trait(_, _, mod, _, _, _, _, sigs, laws, _) if mod.isLawful =>
      val usedSigs = laws.foldLeft(Set.empty[Symbol.SigSym]) {
        case (acc, TypedAst.Def(_, _, exp)) => acc ++ TypedAstOps.sigSymsOf(exp)
      }
      val unusedSigs = sigs.map(_.sym).toSet -- usedSigs
      unusedSigs.toList.map {
        sig => InstanceError.UnlawfulSignature(sig, sig.loc)
      }
    // Case 2: non-lawful trait
    case TypedAst.Trait(_, _, _, _, _, _, _, _, _, _) => Nil
  }

  /**
    * Performs validations on a single trait.
    */
  private def visitTrait(trait0: TypedAst.Trait): List[InstanceError] = {
    checkLawApplication(trait0)
  }

  /**
    * Validates all instances in the given AST root.
    */
  private def visitInstances(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): List[InstanceError] = {
    // Check the instances of each trait in parallel.
    val results = ParOps.parMap(root.instances.values)(checkInstancesOfTrait(_, root, changeSet))
    results.flatten.toList
  }

  /**
    * Checks that an instance is not an orphan.
    * It is declared in either:
    * * The trait's companion namespace.
    * * The same namespace as its type.
    */
  private def checkOrphan(inst: TypedAst.Instance)(implicit flix: Flix): List[InstanceError] = inst match {
    case TypedAst.Instance(_, _, _, trt, tpe, _, _, _, ns, _) => tpe.typeConstructor match {
      // Case 1: Enum type in the same namespace as the instance: not an orphan
      case Some(TypeConstructor.Enum(enumSym, _)) if enumSym.namespace == ns.idents.map(_.name) => Nil
      // Case 2: Any type in the trait namespace: not an orphan
      case _ if trt.sym.namespace == ns.idents.map(_.name) => Nil
      // Case 3: Any type outside the trait companion namespace and enum declaration namespace: orphan
      case _ => List(InstanceError.OrphanInstance(trt.sym, tpe, trt.loc))
    }
  }

  /**
    * Checks that the instance type is simple:
    * * all type variables are unique
    * * all type arguments are variables
    */
  private def checkSimple(inst: TypedAst.Instance)(implicit flix: Flix): List[InstanceError] = inst match {
    case TypedAst.Instance(_, _, _, trt, tpe, _, _, _, _, _) => tpe match {
      case _: Type.Cst => Nil
      case _: Type.Var => List(InstanceError.ComplexInstance(tpe, trt.sym, trt.loc))
      case _: Type.Apply =>
        // ensure that the head is a concrete type
        val headErrs = tpe.typeConstructor match {
          case None => List(InstanceError.ComplexInstance(tpe, trt.sym, trt.loc))
          case Some(_) => Nil
        }
        val (_, errs0) = tpe.typeArguments.foldLeft((List.empty[Type.Var], List.empty[InstanceError])) {
          // Case 1: Type variable
          case ((seen, errs), tvar: Type.Var) =>
            // Case 1.1 We've seen it already. Error.
            if (seen.contains(tvar))
              (seen, List(InstanceError.DuplicateTypeVar(tvar, trt.sym, trt.loc)))
            // Case 1.2 We haven't seen it before. Add it to the list.
            else
              (tvar :: seen, errs)
          // Case 2: Non-variable. Error.
          case ((seen, errs), _) => (seen, InstanceError.ComplexInstance(tpe, trt.sym, trt.loc) :: errs)
        }
        headErrs ::: errs0

      case Type.Alias(alias, _, _, _) => List(InstanceError.IllegalTypeAliasInstance(alias.sym, trt.sym, trt.loc))
      case Type.AssocType(assoc, _, _, loc) => List(InstanceError.IllegalAssocTypeInstance(assoc.sym, trt.sym, loc))
    }
  }

  /**
    * Checks for overlap of instance types, assuming the instances are of the same trait.
    */
  private def checkOverlap(inst1: TypedAst.Instance, heads: Map[TypeConstructor, TypedAst.Instance])(implicit flix: Flix): List[InstanceError] = {
    // Note: We have that Type.Error unifies with any other type, hence we filter such instances here.
    unsafeGetHead(inst1) match {
      case TypeConstructor.Error(_) =>
        // Suppress error for Type.Error.
        Nil
      case tc => heads.get(tc) match {
        // Case 1: No match. No Error.
        case None =>
          Nil
        // Case 2: An instance matching this type exists. Error.
        case Some(inst2) =>
          List(
            InstanceError.OverlappingInstances(inst1.trt.sym, inst1.trt.loc, inst2.trt.loc),
            InstanceError.OverlappingInstances(inst1.trt.sym, inst2.trt.loc, inst1.trt.loc)
          )
      }
    }
  }

  /**
    * Checks that every signature in `trt` is implemented in `inst`, and that `inst` does not have any extraneous definitions.
    */
  private def checkSigMatch(inst: TypedAst.Instance, root: TypedAst.Root)(implicit flix: Flix): List[InstanceError] = {
    val trt = root.traits(inst.trt.sym)

    // Step 1: check that each signature has an implementation.
    val sigMatchVal = trt.sigs.flatMap {
      sig =>
        (inst.defs.find(_.sym.text == sig.sym.name), sig.exp) match {
          // Case 1: there is no definition with the same name, and no default implementation
          case (None, None) => List(InstanceError.MissingImplementation(sig.sym, inst.trt.loc))
          // Case 2: there is no definition with the same name, but there is a default implementation
          case (None, Some(_)) => Nil
          // Case 3: there is an implementation marked override, but no default implementation
          case (Some(defn), None) if defn.spec.mod.isOverride => List(InstanceError.IllegalOverride(defn.sym, defn.sym.loc))
          // Case 4: there is an overriding implementation, but no override modifier
          case (Some(defn), Some(_)) if !defn.spec.mod.isOverride => List(InstanceError.UnmarkedOverride(defn.sym, defn.sym.loc))
          // Case 5: there is an implementation with the right modifier
          case (Some(defn), _) =>
            val expectedScheme = Scheme.partiallyInstantiate(sig.spec.declaredScheme, trt.tparam.sym, inst.tpe, defn.sym.loc)
            if (Scheme.equal(expectedScheme, defn.spec.declaredScheme, root.traitEnv, root.eqEnv)) {
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
        trt.sigs.find(_.sym.name == defn.sym.text) match {
          case None => List(InstanceError.ExtraneousDef(defn.sym, inst.trt.sym, defn.sym.loc))
          case _ => Nil
        }
    }

    sigMatchVal ::: extraDefVal
  }

  /**
    * Finds an instance of the trait for a given type.
    */
  def findInstanceForType(tpe: Type, trt: Symbol.TraitSym, root: TypedAst.Root)(implicit flix: Flix): Option[(Ast.Instance, Substitution)] = {
    val superInsts = root.traitEnv.get(trt).map(_.instances).getOrElse(Nil)
    // lazily find the instance whose type unifies and save the substitution
    ListOps.findMap(superInsts) {
      superInst =>
        Unification.unifyTypes(tpe, superInst.tpe, RigidityEnv.empty).toOption.map {
          case (subst, _) => (superInst, subst) // TODO ASSOC-TYPES consider econstrs
        }
    }
  }

  /**
    * Checks that there is an instance for each super trait of the trait of `inst`,
    * and that the constraints on `inst` entail the constraints on the super instance.
    */
  private def checkSuperInstances(inst: TypedAst.Instance, root: TypedAst.Root)(implicit flix: Flix): List[InstanceError] = inst match {
    case TypedAst.Instance(_, _, _, trt, tpe, tconstrs, _, _, _, _) =>
      val superTraits = root.traitEnv(trt.sym).superTraits
      superTraits flatMap {
        superTrait =>
          // Find the instance of the super trait matching the type of this instance.
          findInstanceForType(tpe, superTrait, root) match {
            case Some((superInst, subst)) =>
              // Case 1: An instance matches. Check that its constraints are entailed by this instance.
              superInst.tconstrs flatMap {
                tconstr =>
                  TraitEnvironment.entail(tconstrs.map(subst.apply), subst(tconstr), root.traitEnv).toHardResult match {
                    case Result.Ok(_) => Nil
                    case Result.Err(errors) => errors.map {
                      case UnificationError.NoMatchingInstance(missingTconstr) => InstanceError.MissingTraitConstraint(missingTconstr, superTrait, trt.loc)
                      case _ => throw InternalCompilerException("Unexpected unification error", inst.loc)
                    }.toList
                  }
              }
            case None =>
              // Case 2: No instance matches. Error.
              List(InstanceError.MissingSuperTraitInstance(tpe, trt.sym, superTrait, trt.loc))
          }
      }
  }

  /**
    * Reassembles an instance
    */
  private def checkInstance(inst: TypedAst.Instance, root: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): List[InstanceError] = {
    val isTraitStable = inst.trt.loc.source.stable
    val isInstanceStable = inst.loc.source.stable
    val isIncremental = changeSet.isInstanceOf[ChangeSet.Changes]
    if (isIncremental && isTraitStable && isInstanceStable) {
      return Nil
    }

    checkSigMatch(inst, root) ::: checkOrphan(inst) ::: checkSuperInstances(inst, root)
  }

  /**
    * Reassembles a set of instances of the same trait.
    */
  private def checkInstancesOfTrait(insts0: List[TypedAst.Instance], root: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): List[InstanceError] = {

    // Instances can be uniquely identified by their heads,
    // due to the non-complexity rule and non-overlap rule.
    // This maps each instance head to its corresponding instance.
    var heads = Map.empty[TypeConstructor, TypedAst.Instance]

    insts0.flatMap {
      // check that the instance is on a valid type, suppressing other errors if not
      case inst => checkSimple(inst) match {
        // Case 1: No Errors. Run other checks.
        case Nil =>
          val res = checkOverlap(inst, heads) ::: checkInstance(inst, root, changeSet)
          // add the head to the set
          heads += (unsafeGetHead(inst) -> inst)
          res
        // Case 2: Errors. Skip other checks.
        case errs => errs
      }
    }
  }

  /**
    * Retrieves the head of a simple instance.
    *
    * The head of an instance `Trait[T[a, b, c]]` is T
    */
  private def unsafeGetHead(inst: TypedAst.Instance): TypeConstructor = {
    inst.tpe.typeConstructor match {
      case Some(tc) => tc
      case None => throw InternalCompilerException("unexpected non-simple type", inst.trt.loc)
    }
  }
}
