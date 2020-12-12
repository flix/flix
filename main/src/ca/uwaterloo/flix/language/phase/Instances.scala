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
import ca.uwaterloo.flix.language.ast.{Scheme, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{ParOps, Validation}

object Instances extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Validates instances in the given AST root.
    */
  override def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Instances") {
    for {
      _ <- visitInstances(root)
    } yield root
  }


  /**
    * Validates all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: TypedAst.Root)(implicit flix: Flix): Validation[Unit, InstanceError] = {

    // MATT docs
    def checkSimple(inst: TypedAst.Instance): Validation[Unit, InstanceError] = inst match {
      case TypedAst.Instance(_, _, _, tpe, _, _, loc) =>
        Validation.fold(tpe.typeArguments, List.empty[Type.Var]) {
          case (seen, tvar: Type.Var) =>
            if (seen.contains(tvar))
              InstanceError.DuplicateTypeParameter(tvar, loc).toFailure // MATT better loc
            else
              (tvar :: seen).toSuccess
          case (_, _) => InstanceError.ComplexInstanceType(tpe, loc).toFailure // MATT better loc
        }.map(_ => ())
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
      val sigMatchVal = checkEach(clazz.signatures) {
        sig =>
          inst.defs.find(_.sym.name == sig.sym.name) match {
            // Case 1: there is no definition with the same name
            case None => InstanceError.MissingImplementation(sig.sym, inst.loc).toFailure
            case Some(defn) =>
              val expectedScheme = Scheme.partiallyInstantiate(sig.sc, clazz.tparam.tpe, inst.tpe)
              if (Scheme.equal(expectedScheme, defn.declaredScheme, root.classEnv)) {
                // Case 2.1: the schemes match. Success!
                ().toSuccess
              } else {
                // Case 2.2: the schemes do not match
                InstanceError.MismatchedSignatures(defn.loc, expectedScheme, defn.declaredScheme).toFailure
              }
          }
      }
      // Step 2: check that there are no extra definitions
      sigMatchVal.flatMap {
        _ =>
          checkEach(inst.defs) {
            defn =>
              clazz.signatures.find(_.sym.name == defn.sym.name) match {
                case None => InstanceError.ExtraneousDefinition(defn.sym, defn.loc).toFailure
                case _ => ().toSuccess
              }
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
      checkEach(checks) {
        case inst :: unchecked =>
          for {
            _ <- checkSimple(inst)
            _ <- Validation.traverse(unchecked)(checkOverlap(_, inst))
            _ <- checkSigMatch(inst)
          } yield ()
        case Nil => ().toSuccess
      }
    }

    // Check the instances of each class in parallel.
    val results = ParOps.parMap(root.instances.values, checkInstancesOfClass)
    checkEach(results)(identity)
  }

  /**
    * Apply a check to each element in the sequence `xs`.
    */
  private def checkEach[In, Error](xs: Iterable[In])(f: In => Validation[Unit, Error]): Validation[Unit, Error] = {
    Validation.fold(xs, ()) {
      case ((), x) => f(x)
    }
  }
}
