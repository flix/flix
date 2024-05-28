/*
 * Copyright 2024 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}

object TypeReduction {

  /**
    * Simplifies the given type by reducing associated type applications.
    *
    * Θ ⊩ τ ⤳ τ'
    *
    * Returns the simplified type and a Boolean flag to indicate whether progress was made.
    *
    * Applications that cannot be resolved are left as they are.
    * These are applications to variables and applications to other unresolvable types.
    *
    * Applications that are illegal result in an Err.
    * These are applications to types for which the eqEnv has no corresponding instance.
    *
    * For example:
    * {{{
    *   Int           ~> Int
    *   Elm[List[a]]  ~> a
    *   Elm[Int]      ~> <ERROR>
    *   Elm[Elm[a]]   ~> Elm[Elm[a]]
    * }}}
    */
  def simplify(tpe: Type, renv0: RigidityEnv, loc: SourceLocation)(implicit eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[(Type, Boolean), TypeError] = tpe match {
    // A var is already simple.
    case t: Type.Var => Result.Ok((t, false))

    // A constant is already simple
    case t: Type.Cst => Result.Ok((t, false))

    // lapp_L and lapp_R
    case Type.Apply(tpe1, tpe2, loc) =>
      for {
        (t1, p1) <- simplify(tpe1, renv0, loc)
        (t2, p2) <- simplify(tpe2, renv0, loc)
        (t3, p3) <- simplifyJava(Type.Apply(t1, t2, loc), loc)
      } yield {
        (t3, p1 || p2 || p3)
      }

    // arg_R and syn_R
    case Type.AssocType(cst, arg, kind, _) =>
      simplify(arg, renv0, loc).flatMap {
        case (t, p) =>
          // we mark t's tvars as rigid so we get the substitution in the right direction
          val renv = t.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
          val insts = eenv(cst.sym)

          // find the first (and only) instance that matches
          val simplifiedOpt = ListOps.findMap(insts) {
            inst =>
              Unification.unifyTypes(t, inst.arg, renv).toOption.flatMap {
                case (subst, Nil) => Some(subst(inst.ret))
                case (_, _ :: _) => None // if we have leftover constraints then it didn't actually unify
              }
          }
          simplifiedOpt match {
            // Can't reduce. Check what the original type was.
            case None =>
              t.baseType match {
                // If it's a var, it's ok. It may be substituted later to a type we can reduce.
                // Or it might be part of the signature as an associated type.
                case Type.Var(sym, loc) => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // If it's an associated type, it's ok. It may be reduced later to a concrete type.
                case _: Type.AssocType => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // Otherwise it's a problem.
                case baseTpe => Result.Err(ConstraintSolver.mkMissingInstance(cst.sym.clazz, baseTpe, renv, loc))
              }
            // We could reduce! Simplify further if possible.
            case Some(t) => simplify(t, renv0, loc).map { case (res, _) => (res, true) }
          }
      }

    case Type.Alias(cst, args, t, _) => simplify(t, renv0, loc)
  }

  /**
   * Simplifies the type of the java method.
   *
   * @param tpe the type of the java method
   * @param loc the location where the java method has been called
   * @return
   */
  private def simplifyJava(tpe: Type, loc: SourceLocation)(implicit flix: Flix): Result[(Type, Boolean), TypeError] =
    tpe.typeConstructor match {
      case Some(TypeConstructor.MethodReturnType(m, n)) =>
        val targs = tpe.typeArguments
        resolveMethodReturnType(targs.head, m, targs.tail, loc) match {
          case ResolutionResult.Resolved(t) => Result.Ok((t, true))
          case ResolutionResult.MethodNotFound() => Result.Err(TypeError.MethodNotFound(m, tpe, RigidityEnv.empty, loc))
          case ResolutionResult.NoProgress => Result.Ok((tpe, false))
        }
      case _ => Result.Ok((tpe, false))
    }

  /**
   * This is the resolution process of the java method method, member of the class of the java object thisObj.
   * Returns the return type of the java method according to the type of thisObj and the arguments of the method,
   * if there exists such a java method.
   * Otherwise, either the java method could not be found with the given method signature, or, the return type
   * could not be simplified more at this state of the process.
   *
   * @param thisObj the java object
   * @param method  the java method, supposedly member of the class of the java object
   * @param ts      the list containing the type of thisObj and the arguments of the method
   * @param loc     the location where the java method has been called
   * @return        A ResolutionResult object that indicates the status of the resolution progress
   */
  private def resolveMethodReturnType(thisObj: Type, method: String, ts: List[Type], loc: SourceLocation)(implicit flix: Flix): ResolutionResult =
    thisObj match {
      case Type.Cst(TypeConstructor.Str, loc2) =>
        val clazz = classOf[String]
        // Filter out candidate methods by method name
        // NB: this considers also static methods
        val candidateMethods = clazz.getMethods.filter(m => m.getName == method)
          .filter(m => m.getParameterCount == 0) // Type.Cst case, no parameter???

        candidateMethods.length match {
          case 1 =>
            val tpe = Type.getFlixType(candidateMethods.head.getReturnType)
            ResolutionResult.Resolved(tpe)
          case _ => ResolutionResult.MethodNotFound() // For now, method not found either if there is an ambiguity or no method found
        }
      case _ => ResolutionResult.NoProgress
    }


  /**
   * Represents the result of a resolution process of a java method.
   *
   * There are three possible outcomes:
   *
   * 1. Resolved(tpe): Indicates that there was some progress in the resolution and returns a simplified type of the java method.
   * 2. MethodNotFound(): The resolution failed to find a corresponding java method.
   * 3. NoProgress: The resolution did not make any progress in the type simplification.
   */
  sealed trait ResolutionResult
  object ResolutionResult {
    case class Resolved(tpe: Type) extends ResolutionResult
    case class MethodNotFound() extends ResolutionResult
    case object NoProgress extends ResolutionResult
  }

}
