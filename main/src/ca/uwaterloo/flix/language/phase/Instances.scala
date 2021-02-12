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
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object Instances extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Validates instances in the given AST root.
    */
  override def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Instances") {
    for {
      _ <- visitInstances(root)
    } yield root
  }

  // MATT use this
  private def visitClasses(root: TypedAst.Root)(implicit flix: Flix): Validation[Unit, InstanceError] = {

    def checkLawfulSuperClasses(class0: TypedAst.Class): Validation[Unit, InstanceError] = class0 match {
      case TypedAst.Class(doc, mod, sym, tparam, superClasses, signatures, laws, loc) => // MATT make _
        if (mod.isLawless) {
          ().toSuccess
        } else {
          Validation.traverseX(superClasses) {
            superSym =>
              val superClass = root.classes(superSym)
              if (superClass.mod.isLawless) {
                ??? // MATT error
              } else {
                ().toSuccess
              }
          }
        }
    }

    // MATT docs
    def findUsedSigs(defn0: TypedAst.Def): List[Symbol.SigSym] = {
      def visit(exp0: TypedAst.Expression): List[Symbol.SigSym] = exp0 match {
        case Expression.Unit(loc) => Nil
        case Expression.Null(tpe, loc) => Nil
        case Expression.True(loc) => Nil
        case Expression.False(loc) => Nil
        case Expression.Char(lit, loc) => Nil
        case Expression.Float32(lit, loc) => Nil
        case Expression.Float64(lit, loc) => Nil
        case Expression.Int8(lit, loc) => Nil
        case Expression.Int16(lit, loc) => Nil
        case Expression.Int32(lit, loc) => Nil
        case Expression.Int64(lit, loc) => Nil
        case Expression.BigInt(lit, loc) => Nil
        case Expression.Str(lit, loc) => Nil
        case Expression.Default(tpe, loc) => Nil
        case Expression.Wild(tpe, loc) => Nil
        case Expression.Var(sym, tpe, loc) => Nil
        case Expression.Def(sym, tpe, loc) => Nil
        case Expression.Sig(sym, tpe, loc) => List(sym)
        case Expression.Hole(sym, tpe, eff, loc) => Nil
        case Expression.Lambda(fparam, exp, tpe, loc) => visit(exp)
        case Expression.Apply(exp, exps, tpe, eff, loc) => visit(exp) ++ exps.flatMap(visit)
        case Expression.Unary(sop, exp, tpe, eff, loc) => visit(exp)
        case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visit(exp1) ++ visit(exp2) ++ visit(exp3)
        case Expression.Stm(exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.Match(exp, rules, tpe, eff, loc) => visit(exp) ++ rules.flatMap(rule => visit(rule.exp) ++ visit(rule.guard))
        case Expression.Choose(exps, rules, tpe, eff, loc) => exps.flatMap(visit) ++ rules.flatMap(rule => visit(rule.exp))
        case Expression.Tag(sym, tag, exp, tpe, eff, loc) => visit(exp)
        case Expression.Tuple(elms, tpe, eff, loc) => elms.flatMap(visit)
        case Expression.RecordEmpty(tpe, loc) => Nil
        case Expression.RecordSelect(exp, field, tpe, eff, loc) => visit(exp)
        case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => visit(value) ++ visit(rest)
        case Expression.RecordRestrict(field, rest, tpe, eff, loc) => visit(rest)
        case Expression.ArrayLit(elms, tpe, eff, loc) => elms.flatMap(visit)
        case Expression.ArrayNew(elm, len, tpe, eff, loc) => visit(elm) ++ visit(len)
        case Expression.ArrayLoad(base, index, tpe, eff, loc) => visit(base) ++ visit(index)
        case Expression.ArrayLength(base, eff, loc) => visit(base)
        case Expression.ArrayStore(base, index, elm, loc) => visit(base) ++ visit(index)
        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => visit(base) ++ visit(beginIndex) ++ visit(endIndex)
        case Expression.Ref(exp, tpe, eff, loc) => visit(exp)
        case Expression.Deref(exp, tpe, eff, loc) => visit(exp)
        case Expression.Assign(exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.Existential(fparam, exp, loc) => visit(exp)
        case Expression.Universal(fparam, exp, loc) => visit(exp)
        case Expression.Ascribe(exp, tpe, eff, loc) => visit(exp)
        case Expression.Cast(exp, tpe, eff, loc) => visit(exp)
        case Expression.TryCatch(exp, rules, tpe, eff, loc) => visit(exp) ++ rules.flatMap(rule => visit(rule.exp))
        case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => args.flatMap(visit)
        case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => visit(exp) ++ args.flatMap(visit)
        case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => args.flatMap(visit)
        case Expression.GetField(field, exp, tpe, eff, loc) => visit(exp)
        case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.GetStaticField(field, tpe, eff, loc) => Nil
        case Expression.PutStaticField(field, exp, tpe, eff, loc) => visit(exp)
        case Expression.NewChannel(exp, tpe, eff, loc) => visit(exp)
        case Expression.GetChannel(exp, tpe, eff, loc) => visit(exp)
        case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.SelectChannel(rules, default, tpe, eff, loc) => rules.flatMap(rule => visit(rule.chan) ++ visit(rule.exp)) ++ default.toList.flatMap(visit)
        case Expression.Spawn(exp, tpe, eff, loc) => visit(exp)
        case Expression.Lazy(exp, tpe, loc) => visit(exp)
        case Expression.Force(exp, tpe, eff, loc) => visit(exp)
        case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => Nil
        case Expression.FixpointCompose(exp1, exp2, stf, tpe, eff, loc) => visit(exp1) ++ (visit(exp2))
        case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => visit(exp)
        case Expression.FixpointProject(pred, exp, tpe, eff, loc) => visit(exp)
        case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => visit(exp1) ++ visit(exp2)
        case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) => visit(exp1) ++ visit(exp2) ++ visit(exp3)
      }

      visit(defn0.exp)
    }

    // MATT docs
    def checkLawApplication(class0: TypedAst.Class): Validation[Unit, InstanceError] = class0 match {
      case TypedAst.Class(doc, mod, sym, tparam, superClasses, signatures, laws, loc) => // MATT make _
        if (mod.isLawless) {
          ().toSuccess
        } else {
          val usedSigs = laws.flatMap(findUsedSigs)
          Validation.traverseX(signatures) {
            sig =>
              if (usedSigs.contains(sig.sym)) {
                ().toSuccess
              } else {
                ??? // MATT error
              }
          }
        }
    }

    def visitClass(class0: TypedAst.Class): Validation[Unit, InstanceError] = {
      ??? // MATT
    }

    ??? // MATT
  }

  /**
    * Validates all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
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
          Validation.traverseX(inst.defs) {
            defn =>
              clazz.signatures.find(_.sym.name == defn.sym.name) match {
                case None => InstanceError.ExtraneousDefinition(defn.sym, defn.loc).toFailure
                case _ => ().toSuccess
              }
          }
      }
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
          for {
            _ <- checkSimple(inst)
            _ <- Validation.traverse(unchecked)(checkOverlap(_, inst))
            _ <- checkSigMatch(inst)
            _ <- checkOrphan(inst)
            _ <- checkSuperInstances(inst)
          } yield ()
        case Nil => ().toSuccess
      }
    }

    // Check the instances of each class in parallel.
    val results = ParOps.parMap(root.instances.values, checkInstancesOfClass)
    Validation.traverseX(results)(identity)
  }
}
