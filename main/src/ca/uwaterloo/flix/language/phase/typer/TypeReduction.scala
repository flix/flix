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
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver.ResolutionResult
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}

import java.lang.reflect.{Constructor, Field, Method}
import java.math.BigInteger
import scala.annotation.tailrec

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
  def simplify(tpe: Type, renv0: RigidityEnv, loc: SourceLocation)(implicit scope: Scope, eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[(Type, Boolean), TypeError] = tpe match {
    // A var is already simple.
    case t: Type.Var => Result.Ok((t, false))

    // A constant is already simple
    case t: Type.Cst => Result.Ok((t, false))

    // lapp_L and lapp_R
    case Type.Apply(tpe1, tpe2, loc) =>
      for {
        (t1, p1) <- simplify(tpe1, renv0, loc)
        (t2, p2) <- simplify(tpe2, renv0, loc)
      } yield {
        (Type.Apply(t1, t2, loc), p1 || p2)
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
                case baseTpe => Result.Err(ConstraintSolver.mkMissingInstance(cst.sym.trt, baseTpe, renv, loc))
              }
            // We could reduce! Simplify further if possible.
            case Some(t) => simplify(t, renv0, loc).map { case (res, _) => (res, true) }
          }
      }

    case Type.Alias(cst, args, t, _) => simplify(t, renv0, loc)

    case Type.JvmToType(j0, _) =>
      simplify(j0, renv0, loc).map {
        case (Type.Cst(TypeConstructor.JvmConstructor(constructor), _), _) => (Type.getFlixType(constructor.getDeclaringClass), true)
        case (Type.Cst(TypeConstructor.JvmField(field), _), _) => (Type.getFlixType(field.getType), true)
        case (Type.Cst(TypeConstructor.JvmMethod(method), _), _) => (Type.getFlixType(method.getReturnType), true)
        case (j, p) => (Type.JvmToType(j, loc), p)
      }

    case cons@Type.UnresolvedJvmType(Type.JvmMember.JvmConstructor(clazz, tpes), _) =>
      lookupConstructor(clazz, tpes, loc) match {
        // Case 1: We resolved the type.
        case JavaConstructorResolutionResult.Resolved(tpe) => Result.Ok((tpe, true))
        // Case 2: Ambiguous constructor. Error.
        case JavaConstructorResolutionResult.AmbiguousConstructor(constructors) => Result.Err(TypeError.AmbiguousConstructor(clazz, tpes, constructors, renv0, loc))
        // Case 3: No such constructor. Error.
        case JavaConstructorResolutionResult.ConstructorNotFound => Result.Err(TypeError.ConstructorNotFound(clazz, tpes, List(), renv0, loc)) // TODO INTEROP: fill in candidate methods
        // Case 4: Not ready to reduce. Return the constructor.
        case JavaConstructorResolutionResult.UnresolvedTypes => Result.Ok(cons, false)
      }

    case meth@Type.UnresolvedJvmType(Type.JvmMember.JvmMethod(tpe, name, tpes), _) =>
      lookupMethod(tpe, name.name, tpes, loc) match {
        // Case 1: We resolved the type.
        case JavaMethodResolutionResult.Resolved(tpe) => Result.Ok((tpe, true))
        // Case 2: Ambiguous method. Error.
        case JavaMethodResolutionResult.AmbiguousMethod(methods) => Result.Err(TypeError.AmbiguousMethod(name, tpe, tpes, methods, renv0, loc))
        // Case 3: No such method. Error.
        case JavaMethodResolutionResult.MethodNotFound => Result.Err(TypeError.MethodNotFound(name, tpe, tpes, loc))
        // Case 4: Not ready to reduce. Return the method.
        case JavaMethodResolutionResult.UnresolvedTypes => Result.Ok((meth, false))
      }

    case meth@Type.UnresolvedJvmType(Type.JvmMember.JvmStaticMethod(clazz, name, tpes), _) =>
      lookupStaticMethod(clazz, name.name, tpes, loc) match {
        // Case 1: We resolved the type.
        case JavaMethodResolutionResult.Resolved(tpe) => Result.Ok((tpe, true))
        // Case 2: Ambiguous method. Error.
        case JavaMethodResolutionResult.AmbiguousMethod(methods) => Result.Err(TypeError.AmbiguousStaticMethod(clazz, name, tpes, methods, renv0, loc))
        // Case 3: No such method. Error.
        case JavaMethodResolutionResult.MethodNotFound => Result.Err(TypeError.StaticMethodNotFound(clazz, name, tpes, List(), renv0, loc)) // TODO INTEROP: fill in candidate methods
        // Case 4: Not ready to reduce. Return the method.
        case JavaMethodResolutionResult.UnresolvedTypes => Result.Ok((meth, false))
      }

    case field@Type.UnresolvedJvmType(Type.JvmMember.JvmField(tpe, name), _) =>
      lookupField(tpe, name.name, loc) match {
        case JavaFieldResolutionResult.Resolved(tpe) => Result.Ok((tpe, true))
        case JavaFieldResolutionResult.FieldNotFound => Result.Err(TypeError.FieldNotFound(name, tpe, loc))
        case JavaFieldResolutionResult.UnresolvedTypes => Result.Ok((field, false))
      }
  }

  /**
    * Returns `true` if the given type contains [[Type.JvmToType]] or [[Type.UnresolvedJvmType]].
    */
  def containsJvmTypes(tpe: Type): Boolean = tpe match {
    case Type.Var(_, _) => false
    case Type.Cst(_, _) => false
    case Type.Apply(tpe1, tpe2, _) => containsJvmTypes(tpe1) || containsJvmTypes(tpe2)
    case Type.Alias(_, _, tpe, _) => containsJvmTypes(tpe)
    case Type.AssocType(_, arg, _, _) => containsJvmTypes(arg)
    case Type.JvmToType(_, _) => true
    case Type.UnresolvedJvmType(_, _) => true
  }

  /**
   * This is the resolution process of the Java constructor designated by a class and the parameter types.
   * Returns the return type of the Java constructor according, if there exists such a Java constructor.
   * Otherwise, either the Java constructor could not be found with the given signature or there was an ambiguity.
   *
   * @param clazz   the constructor's Java class
   * @param ts      the list containing the parameter types of the constructor
   * @param loc     the location where the java constructor has been invoked
   * @return        A JavaConstructorResolutionResult object that indicates the status of the resolution progress
   */
  def lookupConstructor(clazz: Class[_], ts: List[Type], loc: SourceLocation): JavaConstructorResolutionResult = {
    if (ts.forall(isKnown)) retrieveConstructor(clazz, ts, loc)
    else JavaConstructorResolutionResult.UnresolvedTypes
  }

  /**
   * This is the resolution process of the Java method method, member of the class of the Java object thisObj.
   * Returns the return type of the Java method according to the type of thisObj and the arguments of the method,
   * if there exists such a Java method.
   * Otherwise, either the Java method could not be found with the given method signature, or, there was an ambiguity.
   *
   * @param thisObj     the Java object
   * @param methodName  the Java method, supposedly member of the class of the Java object
   * @param ts          the list containing the type of thisObj and the arguments of the method
   * @param loc         the location where the Java method has been called
   * @return            A JavaMethodResolutionResult object that indicates the status of the resolution progress
   */
  def lookupMethod(thisObj: Type, methodName: String, ts: List[Type], loc: SourceLocation): JavaMethodResolutionResult = {
    if (isKnown(thisObj) && ts.forall(isKnown)) {
      Type.classFromFlixType(thisObj) match {
        case Some(clazz) =>
          retrieveMethod(clazz, methodName, ts, loc = loc)
        case None =>
          JavaMethodResolutionResult.MethodNotFound
      }
    } else JavaMethodResolutionResult.UnresolvedTypes
  }

  /**
    * This is the resolution process of the Java static method method, member of the class of the Java class clazz.
    * Returns the return type of the Java static method according to the class of clazz and the arguments of the method,
    * if there exists such a Java method.
    * Otherwise, either the Java method could not be found with the given method signature, or, there was an ambiguity.
    *
    * @param clazz       the Java class
    * @param methodName  the Java method, supposedly member of the class of the Java object
    * @param ts          the list containing the type of thisObj and the arguments of the method
    * @param loc         the location where the Java method has been called
    * @return            A JavaMethodResolutionResult object that indicates the status of the resolution progress
    */
  def lookupStaticMethod(clazz: Class[_], methodName: String, ts: List[Type], loc: SourceLocation): JavaMethodResolutionResult = {
    if (ts.forall(isKnown)) retrieveMethod(clazz, methodName, ts, isStatic = true, loc = loc)
    else JavaMethodResolutionResult.UnresolvedTypes
  }

  /**
    * This is the resolution process of the Java field, member of the class of the Java object thisObj.
    * Returns the type of the Java field according to the type of thisObj if there exists such a Java method.
    * Otherwise, either the Java field could not be found with the given name.
    *
    * @param thisObj    the Java object
    * @param fieldName  the Java field, supposedly member of the class of the Java object
    * @param loc        the location where the Java field has been accessed
    * @return           A JavaFieldResolutionResult object that indicates the status of the resolution progress
    */
  def lookupField(thisObj: Type, fieldName: String, loc: SourceLocation): JavaFieldResolutionResult = {
    if (isKnown(thisObj)) {
      Type.classFromFlixType(thisObj) match {
        case Some(clazz) =>
          retrieveField(clazz, fieldName, loc)
        case None =>
          JavaFieldResolutionResult.FieldNotFound
      }
    }
    else JavaFieldResolutionResult.UnresolvedTypes
  }

  /**
   * Helper method to retrieve a constructor given its parameter types and the class.
   * Returns a JavaConstructorResolutionResult containing either the constructor, a list of candidate constructors or
   * a ConstructorNotFound object. The working process is similar to retrieveMethod and differs in the selection of candidate constructors.
   */
  private def retrieveConstructor(clazz: Class[_], ts: List[Type], loc: SourceLocation): JavaConstructorResolutionResult = {
    val candidateConstructors = clazz.getConstructors.filter(c => isCandidateConstructor(c, ts)).toList

    candidateConstructors match {
      // Case 1: No such constructor. Error.
      case Nil => JavaConstructorResolutionResult.ConstructorNotFound

      // Case 2: Exactly one matching constructor. Success!
      case constructor :: Nil =>
        val tpe = Type.Cst(TypeConstructor.JvmConstructor(constructor), loc)
        JavaConstructorResolutionResult.Resolved(tpe)

      // Case 3: Multiple matching methods. We need to refine our search.
      case cs@(_ :: _ :: _) =>

        // Among candidate constructors if there already exists the one with the exact signature,
        // we should ignore the rest.
        val exactConstructors = cs
          .filter(c => // Parameter types correspondence with subtyping
            (c.getParameterTypes zip ts).forall {
              case (clazz, tpe) => Type.getFlixType(clazz) == tpe
            })

        exactConstructors match {
          // Case 3.1: No exact matches. We have ambiguity among the candidates.
          case Nil => JavaConstructorResolutionResult.AmbiguousConstructor(candidateConstructors)

          // Case 3.2: One exact match. Success!
          case exact :: Nil => JavaConstructorResolutionResult.Resolved(Type.Cst(TypeConstructor.JvmConstructor(exact), loc))

          // Case 3.3: Multiple exact matches. Impossible.
          case _ :: _ :: _ => JavaConstructorResolutionResult.AmbiguousConstructor(candidateConstructors.toList) // 0 corresponds to no exact constructor, 2 or higher should be impossible in Java
        }
    }
  }

  /**
   * Helper method to retrieve a method given its name and parameter types.
   * Returns a JavaMethodResolutionResult either containing the Java method or a MethodNotFound object.
   */
  private def retrieveMethod(clazz: Class[_], methodName: String, ts: List[Type], isStatic: Boolean = false, loc: SourceLocation): JavaMethodResolutionResult = {
    // NB: this considers also static methods
    val candidateMethods = getMethods(clazz).filter(m => isCandidateMethod(m, methodName, isStatic, ts))

    candidateMethods match {
      // Case 1: No such method. Error.
      case Nil => JavaMethodResolutionResult.MethodNotFound

      // Case 2: Exactly one matching method. Success!
      case method :: Nil =>
        val tpe = Type.Cst(TypeConstructor.JvmMethod(method), loc)
        JavaMethodResolutionResult.Resolved(tpe)

      // Case 3: Multiple matching methods. We need to refine our search.
      case ms@(_ :: _ :: _) =>

        // Among candidate methods if there already exists the method with the exact signature,
        // we should ignore the rest. E.g.: append(String), append(Object) in SB for the call append("a") should already know to use append(String)
        val exactMethods = ms
          .filter(m => // Parameter types correspondence with subtyping
            (m.getParameterTypes zip ts).forall {
              case (clazz, tpe) => Type.getFlixType(clazz) == tpe
            })

        exactMethods match {
          // Case 3.1: No exact matches. We have ambiguity among the candidates.
          case Nil => JavaMethodResolutionResult.AmbiguousMethod(candidateMethods)

          // Case 3.2: One exact match. Success!
          case exact :: Nil => JavaMethodResolutionResult.Resolved(Type.Cst(TypeConstructor.JvmMethod(exact), loc))

          // Case 3.3: Multiple exact matches. Impossible.
          case _ :: _ :: _ => throw InternalCompilerException("Unexpected multiple exact matches for Java method", loc)
        }
    }
  }

  /**
    * Helper method to retrieve a field given its class and name.
    * Returns a JavaFieldResolutionResult either containing the Java field or a FieldNotFound object.
    */
  private def retrieveField(clazz: Class[_], fieldName: String, loc: SourceLocation): JavaFieldResolutionResult = {
      getField(clazz, fieldName) match {
        case Some(field) => JavaFieldResolutionResult.Resolved(Type.Cst(TypeConstructor.JvmField(field), loc))
        case None => JavaFieldResolutionResult.FieldNotFound
      }
  }

  /**
    * Returns the `fieldName` field of `clazz` if it exists.
    *
    * Field name "length" of array classes always return `None` (see Class.getField).
    */
  private def getField(clazz: Class[_], fieldName: String): Option[Field] = {
    try {
      Some(clazz.getField(fieldName))
    } catch {
      case _: NoSuchFieldException => None
    }
  }

  /**
    * Returns the methods of the class INCLUDING implicit interface inheritance from Object.
    */
  def getMethods(clazz: Class[_]): List[Method] = {
    if (clazz.isInterface) {
      // Case 1: Interface. We have to add the methods from Object.
      val declaredMethods = clazz.getMethods.toList

      // Find all the methods in Object that are not declared in the interface.
      val undeclaredObjectMethods = classOf[Object].getMethods.toList.filter {
        case objectMethod => !declaredMethods.exists {
          case declaredMethod => methodsMatch(objectMethod, declaredMethod)
        }
      }

      // Add the undeclared object methods to the declared methods.
      declaredMethods ::: undeclaredObjectMethods

    } else {
      // Case 2: Class. Just return the methods.
      clazz.getMethods.toList
    }
  }

  /**
    * Returns true if the methods are the same, modulo their declaring class.
    */
  private def methodsMatch(m1: Method, m2: Method): Boolean = {
    m1.getName == m2.getName &&
      isStatic(m1) == isStatic(m2) &&
      m1.getParameterTypes.sameElements(m2.getParameterTypes)
  }

  /**
   * Helper method that returns if the given constructor is a candidate constructor given a signature.
   */
  private def isCandidateConstructor(cand: Constructor[_], ts: List[Type]): Boolean =
    (cand.getParameterCount == ts.length) &&
      (cand.getParameterTypes zip ts).forall {
        case (clazz, tpe) => isSubtype(tpe, Type.getFlixType(clazz))
      }

  /**
   * Helper method that returns if the given method is a candidate method given a signature.
   *
   * @param cand       a candidate method
   * @param methodName the potential candidate method's name
   * @param ts         the list of parameter types of the potential candidate method
   */
  private def isCandidateMethod(cand: Method, methodName: String, static: Boolean = false, ts: List[Type]): Boolean = {
    val candIsStatic = isStatic(cand)
    (candIsStatic == static) &&
      (cand.getName == methodName) &&
      (cand.getParameterCount == ts.length) &&
      // Parameter types correspondence with subtyping
      (cand.getParameterTypes zip ts).forall {
        case (clazz, tpe) => isSubtype(tpe, Type.getFlixType(clazz))
      } &&
      // NB: once methods with same signatures have been filtered out, we should remove super-methods duplicates
      // if the superclass is abstract or ignore if it is a primitive type or void
      (cand.getReturnType.equals(Void.TYPE) || cand.getReturnType.isPrimitive || cand.getReturnType.isArray || // for all arrays return types?
        java.lang.reflect.Modifier.isInterface(cand.getReturnType.getModifiers) || // interfaces are considered primitives
        !(java.lang.reflect.Modifier.isAbstract(cand.getReturnType.getModifiers) && !static))
  } // temporary to avoid superclass abstract duplicate, except for static methods

  private def isStatic(method: Method): Boolean = {
    java.lang.reflect.Modifier.isStatic(method.getModifiers)
  }

  /**
   * Helper method to define a sub-typing relation between two given Flix types.
   * Returns true if tpe1 is a sub-type of type tpe2, false otherwise.
   */
  @tailrec
  private def isSubtype(tpe1: Type, tpe2: Type): Boolean = {
    (tpe1, tpe2) match {
      case (t1, t2) if t1 == t2 => true
      // Base types
      case (Type.Cst(TypeConstructor.Native(clazz1), _), Type.Cst(TypeConstructor.Native(clazz2), _)) => clazz2.isAssignableFrom(clazz1)
      case (Type.Cst(TypeConstructor.Unit, _), Type.Cst(TypeConstructor.Native(clazz), _)) if clazz == classOf[java.lang.Object] => true
      case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(clazz), _)) => clazz.isAssignableFrom(classOf[java.lang.String])
      case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(clazz), _)) => clazz.isAssignableFrom(classOf[java.math.BigInteger])
      case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(clazz), _)) => clazz.isAssignableFrom(classOf[java.math.BigDecimal])
      case (Type.Cst(TypeConstructor.Regex, _), Type.Cst(TypeConstructor.Native(clazz), _)) => clazz.isAssignableFrom(classOf[java.util.regex.Pattern])
      // Arrays
      case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType1, _), rcVar1, _),
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType2, _), rcVar2, _)) =>
        isSubtype(elmType1, elmType2)
      // Arrow to Java function interface
      case (Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), eff, _), varArg, _), varRet, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        (varArg, varRet) match {
          case (Type.Cst(tc1, _), Type.Cst(tc2, _)) =>
            (tc1, tc2) match {
              case (TypeConstructor.Int32, TypeConstructor.Unit) =>
                clazz == classOf[java.util.function.IntConsumer]
              case (TypeConstructor.Int32, TypeConstructor.Bool) =>
                clazz == classOf[java.util.function.IntPredicate]
              case (TypeConstructor.Int32, TypeConstructor.Int32) =>
                clazz == classOf[java.util.function.IntUnaryOperator]
              case (TypeConstructor.Int32, TypeConstructor.Native(obj)) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.IntFunction[Object]]
              case (TypeConstructor.Float64, TypeConstructor.Unit) =>
                clazz == classOf[java.util.function.DoubleConsumer]
              case (TypeConstructor.Float64, TypeConstructor.Bool) =>
                clazz == classOf[java.util.function.DoublePredicate]
              case (TypeConstructor.Float64, TypeConstructor.Float64) =>
                clazz == classOf[java.util.function.DoubleUnaryOperator]
              case (TypeConstructor.Float64, TypeConstructor.Native(obj)) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.DoubleFunction[Object]]
              case (TypeConstructor.Int64, TypeConstructor.Unit) =>
                clazz == classOf[java.util.function.LongConsumer]
              case (TypeConstructor.Int64, TypeConstructor.Bool) =>
                clazz == classOf[java.util.function.LongPredicate]
              case (TypeConstructor.Int64, TypeConstructor.Int64) =>
                clazz == classOf[java.util.function.LongUnaryOperator]
              case (TypeConstructor.Int64, TypeConstructor.Native(obj)) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.LongFunction[Object]]
              case (TypeConstructor.Native(obj), TypeConstructor.Unit) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.Consumer[Object]]
              case (TypeConstructor.Native(obj), TypeConstructor.Bool) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.Predicate[Object]]
              case _ => false
            }
          case _ => false
        }
      // Null is a sub-type of every Java object and non-primitive Flix type
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Native(_), _)) => true
      case (Type.Cst(TypeConstructor.Null, _), tpe) if !isPrimitive(tpe) => true
      case _ => false
    }
  }

  /**
   * Returns true iff the given type tpe is a Flix primitive.
   */
  private def isPrimitive(tpe: Type): Boolean = {
    tpe match {
      case Type.Cst(TypeConstructor.Bool, _) => true
      case Type.Cst(TypeConstructor.Char, _) => true
      case Type.Cst(TypeConstructor.Float32, _) => true
      case Type.Cst(TypeConstructor.Float64, _) => true
      case Type.Cst(TypeConstructor.Int8, _) => true
      case Type.Cst(TypeConstructor.Int16, _) => true
      case Type.Cst(TypeConstructor.Int32, _) => true
      case Type.Cst(TypeConstructor.Int64, _) => true
      case _ => false
    }
  }

  /**
    * Returns `true` if the type is resolved enough for Java resolution.
    */
  private def isKnown(t0: Type): Boolean = t0 match {
    case Type.Var(_, _) if t0.kind == Kind.Eff => true
    case Type.Var(_, _) => false
    case Type.Cst(_, _) => true
    case Type.JvmToType(_, _) => false
    case Type.UnresolvedJvmType(_, _) => false
    case Type.Apply(tpe1, tpe2, _) => isKnown(tpe1) && isKnown(tpe2)
    case Type.Alias(_, _, tpe, _) => isKnown(tpe)
    case Type.AssocType(_, _, _, _) => false
  }

  /**
   * Represents the result of a resolution process of a java method.
   *
   * There are three possible outcomes:
   *
   *   1. Resolved(tpe): Indicates that there was some progress in the resolution and returns a
   *      simplified type of the java method.
   *   1. AmbiguousMethod: The resolution lacked some elements to find a java method among a set of
   *      methods.
   *   1. MethodNotFound: The resolution failed to find a corresponding java method.
   *   1. UnresolvedTyped: The types involved are not reduced enough to decide the java method
   */
  sealed trait JavaMethodResolutionResult
  object JavaMethodResolutionResult {
    case class Resolved(tpe: Type) extends JavaMethodResolutionResult
    case class AmbiguousMethod(methods: List[Method]) extends JavaMethodResolutionResult
    case object MethodNotFound extends JavaMethodResolutionResult
    case object UnresolvedTypes extends JavaMethodResolutionResult
  }

  /**
    * Represents the result of a resolution process of a java field.
    *
    * There are two possible outcomes:
    *
    *   1. FieldNotFound: The resolution failed to find a corresponding java field.
    *   1. UnresolvedTypes: The types involved are not reduced enough to decide the java method
    */
  sealed trait JavaFieldResolutionResult
  object JavaFieldResolutionResult {
    case class Resolved(tpe: Type) extends JavaFieldResolutionResult
    case object FieldNotFound extends JavaFieldResolutionResult
    case object UnresolvedTypes extends JavaFieldResolutionResult
  }

  /**
   * Represents the result of a resolution process of a java constructor.
   *
   * There are three possible outcomes:
   *   1. Resolved(tpe): Indicates that there was some progress in the resolution and returns a simplified type of the java constructor.
   *   1. AmbiguousConstructor: The resolution lacked some elements to find a java constructor among a set of constructors.
   *   1. ConstructorNotFound: The resolution failed to find a corresponding java constructor.
   *   1. UnresolvedTypes: The types involved are not reduced enough to decide the java method
   */
  sealed trait JavaConstructorResolutionResult
  object JavaConstructorResolutionResult {
    case class Resolved(tpe: Type) extends JavaConstructorResolutionResult
    case class AmbiguousConstructor(constructors: List[Constructor[_]]) extends JavaConstructorResolutionResult
    case object ConstructorNotFound extends JavaConstructorResolutionResult
    case object UnresolvedTypes extends JavaConstructorResolutionResult
  }
}
