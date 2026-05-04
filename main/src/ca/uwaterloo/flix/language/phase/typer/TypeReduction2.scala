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
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, RegionScope}
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Substitution}
import ca.uwaterloo.flix.util.JvmUtils
import org.apache.commons.lang3.reflect.{ConstructorUtils, MethodUtils}

import java.lang.reflect.{Constructor, Field, GenericArrayType, Method, ParameterizedType, TypeVariable, WildcardType}
import scala.annotation.tailrec

object TypeReduction2 {

  /**
    * Performs various reduction rules on the given type.
    */
  def reduce(tpe0: Type)(implicit scope: RegionScope, renv: RigidityEnv, progress: Progress, eqenv: EqualityEnv, flix: Flix): (Type, List[TypeConstraint]) = tpe0 match {
    case t: Type.Var => (t, Nil)

    case t: Type.Cst => (t, Nil)

    case Type.Apply(tpe1, tpe2, loc) =>
      val (t1, cs1) = reduce(tpe1)
      val (t2, cs2) = reduce(tpe2)
      // Performance: Reuse this, if possible.
      val tpe = if ((t1 eq tpe1) && (t2 eq tpe2))
        tpe0
      else
        Type.Apply(t1, t2, loc)
      (tpe, cs1 ::: cs2)

    case Type.Alias(_, _, tpe, _) => (tpe, Nil)

    case Type.AssocType(AssocTypeSymUse(sym, _), tpe, _, _) =>
      val (t, cs) = reduce(tpe)

      // Get all the associated types from the context
      val assocOpt = eqenv.getAssocDef(sym, t)

      // Find the instance that matches
      val matches = assocOpt.flatMap {
        case AssocTypeDef(tparams, assocTpe0, ret0) =>


          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val assocRenv = t.typeVars.map(_.sym).foldLeft(renv)(_.markRigid(_))


          // Refresh the flexible variables in the instance
          // (variables may be rigid if the instance comes from a constraint on the definition)
          val assocVarMap = tparams.map {
            case fromSym => fromSym -> Type.freshVar(fromSym.kind, fromSym.loc)(scope, flix)
          }.toMap
          val assocSubst = Substitution(assocVarMap)
          val assocTpe = assocSubst(assocTpe0)
          val ret = assocSubst(ret0)

          // Instantiate all the instance constraints according to the substitution.
          ConstraintSolver2.fullyUnify(t, assocTpe, scope, assocRenv).map {
            case subst => subst(ret)
          }
      }

      matches match {
        // Case 1: No match. Can't reduce the type.
        case None => (tpe0, cs)

        // Case 2: One match. Use it.
        case Some(newTpe) =>
          progress.markProgress()
          (newTpe, cs)
      }

    case Type.JvmToType(tpe, loc) =>
      val (t, cs) = reduce(tpe)
      t match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          progress.markProgress()
          (instantiateJavaTypeWithFreshVars(constructor.getDeclaringClass, scope, loc), cs)

        case Type.Cst(TypeConstructor.JvmField(field), _) =>
          progress.markProgress()
          (instantiateJavaTypeWithFreshVars(field.getType, scope, loc), cs)

        case t1 => t1.typeConstructor match {
          case Some(TypeConstructor.JvmMethod(method)) =>
            progress.markProgress()
            (resolveMethodReturnType(method, t1.typeArguments, scope, loc), cs)

          case _ => (Type.JvmToType(t1, loc), cs)
        }
      }

    case Type.JvmToEff(tpe, loc) =>
      val (t, cs) = reduce(tpe)
      t match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          progress.markProgress()
          (PrimitiveEffects.getConstructorEffs(constructor, loc), cs)

        case t1 => t1.typeConstructor match {
          case Some(TypeConstructor.JvmMethod(method)) =>
            progress.markProgress()
            (PrimitiveEffects.getMethodEffs(method, loc), cs)

          case _ => (Type.JvmToEff(t1, loc), cs)
        }
      }

    case unresolved@Type.UnresolvedJvmType(member, loc) =>
      member match {
        case JvmMember.JvmConstructor(clazz, tpes) =>
          val (reducedTpes, css) = tpes.map(reduce(_)).unzip
          val cs = css.flatten
          lookupConstructor(clazz, reducedTpes) match {
            case JavaConstructorResolution.Resolved(constructor) =>
              progress.markProgress()
              (Type.Cst(TypeConstructor.JvmConstructor(constructor), loc), cs)
            case _ => (unresolved, cs)
          }

        case JvmMember.JvmField(_, tpe, name) =>
          val (reducedTpe, cs) = reduce(tpe)
          lookupField(reducedTpe, name.name) match {
            case JavaFieldResolution.Resolved(field) =>
              progress.markProgress()
              (Type.Cst(TypeConstructor.JvmField(field), loc), cs)
            case _ => (unresolved, cs)
          }

        case JvmMember.JvmMethod(tpe, name, tpes) =>
          val (reducedTpe, cs0) = reduce(tpe)
          val (reducedTpes, css) = tpes.map(reduce(_)).unzip
          val cs = cs0 ::: css.flatten
          lookupMethod(reducedTpe, name.name, reducedTpes) match {
            case JavaMethodResolution.Resolved(method) =>
              val classTypeArgs = extractClassTypeArgs(method, reducedTpe, scope, loc)
              val (tpe, cs0) = instantiateMethod(method, classTypeArgs, reducedTpes, scope, loc)
              progress.markProgress()
              (tpe, cs ::: cs0)
            case _ => (unresolved, cs)
          }

        case JvmMember.JvmStaticMethod(clazz, name, tpes) =>
          val (reducedTpes, css) = tpes.map(reduce(_)).unzip
          val cs = css.flatten
          lookupStaticMethod(clazz, name.name, reducedTpes) match {
            case JavaMethodResolution.Resolved(method) =>
              val (tpe, cs0) = instantiateMethod(method, Nil, reducedTpes, scope, loc)
              progress.markProgress()
              (tpe, cs ::: cs0)
            case _ => (unresolved, cs)
          }
      }
  }

  /** Tries to find a constructor of `clazz` that takes arguments of type `ts`. */
  private def lookupConstructor(clazz: Class[?], ts: List[Type])(implicit scope: RegionScope, renv: RigidityEnv): JavaConstructorResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaConstructorResolution.UnresolvedTypes

    val tparams = ts.map(getJavaType)
    val c = ConstructorUtils.getMatchingAccessibleConstructor(clazz, tparams *)

    // Check if we found a matching constructor.
    if (c != null && !usesBoxing(tparams, c.getParameterTypes)) {
      JavaConstructorResolution.Resolved(c)
    } else {
      JavaConstructorResolution.NotFound
    }
  }

  /** Tries to find a method of `thisObj` that takes arguments of type `ts`. */
  private def lookupMethod(thisObj: Type, methodName: String, ts: List[Type])(implicit scope: RegionScope, renv: RigidityEnv): JavaMethodResolution = {
    val typesAreKnown = isKnown(thisObj) && ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    // For type variables (rigid), fall back to java.lang.Object (the erased type).
    val clazz = Type.classFromFlixType(thisObj).getOrElse(classOf[Object])
    retrieveMethod(clazz, methodName, ts, static = false)
  }

  /** Tries to find a static method of `clazz` that takes arguments of type `ts`. */
  private def lookupStaticMethod(clazz: Class[?], methodName: String, ts: List[Type])(implicit scope: RegionScope, renv: RigidityEnv): JavaMethodResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    retrieveMethod(clazz, methodName, ts, static = true)
  }

  /** Tries to find a static/dynamic method of `clazz` that takes arguments of type `ts`. */
  private def retrieveMethod(clazz: Class[?], methodName: String, ts: List[Type], static: Boolean): JavaMethodResolution = {
    val tparams = ts.map(getJavaType)
    val m = MethodUtils.getMatchingAccessibleMethod(clazz, methodName, tparams *)
    // We check if we found a method and if its static flag matches.
    if (m != null && JvmUtils.isStatic(m) == static && !usesBoxing(tparams, m.getParameterTypes)) {
      // Case 1: We found the method on the clazz.
      JavaMethodResolution.Resolved(m)
    } else {
      // Case 2: We failed to find the method on the clazz.
      // We make one attempt on java.lang.Object.
      val classObj = classOf[java.lang.Object]
      val m = MethodUtils.getMatchingAccessibleMethod(classObj, methodName, tparams *)
      if (m != null && JvmUtils.isStatic(m) == static && !usesBoxing(tparams, m.getParameterTypes)) {
        // Case 2.1: We found the method on java.lang.Object.
        JavaMethodResolution.Resolved(m)
      } else {
        // Case 2.2: We failed to find the method, so we report an error on the original clazz.
        JavaMethodResolution.NotFound
      }
    }
  }

  /**
    * Returns `true` if `args` and `params` have indices that require unsupported unboxing
    * (wrapper arg to primitive param). Primitive-to-Object boxing is allowed and handled
    * by automatic boxing in Lowering.
    *
    * This function is used to check [[MethodUtils.getMatchingAccessibleMethod]] and
    * [[ConstructorUtils.getMatchingAccessibleConstructor]] matches.
    */
  private def usesBoxing(args: List[Class[?]], params: Array[Class[?]]): Boolean = {
    // This method is checking an existing match, so zip is fine.
    args.zip(params).exists {
      // Wrapper arg to primitive param (unboxing): not supported.
      case (clazz, java.lang.Boolean.TYPE) if clazz != java.lang.Boolean.TYPE => true
      case (clazz, java.lang.Byte.TYPE) if clazz != java.lang.Byte.TYPE => true
      case (clazz, java.lang.Short.TYPE) if clazz != java.lang.Short.TYPE => true
      case (clazz, java.lang.Integer.TYPE) if clazz != java.lang.Integer.TYPE => true
      case (clazz, java.lang.Long.TYPE) if clazz != java.lang.Long.TYPE => true
      case (clazz, java.lang.Character.TYPE) if clazz != java.lang.Character.TYPE => true
      case (clazz, java.lang.Float.TYPE) if clazz != java.lang.Float.TYPE => true
      case (clazz, java.lang.Double.TYPE) if clazz != java.lang.Double.TYPE => true
      // Primitive arg to non-primitive param (boxing): supported via automatic boxing.
      // These cases are intentionally NOT rejected.
      // Otherwise it is not boxing.
      case _ => false
    }
  }


  /**
    * Returns the Java reflective class object corresponding to the given Flix `tpe`.
    */
  private def getJavaType(tpe: Type): Class[?] = tpe match {
    case Type.Bool => java.lang.Boolean.TYPE
    case Type.Int8 => java.lang.Byte.TYPE
    case Type.Int16 => java.lang.Short.TYPE
    case Type.Int32 => java.lang.Integer.TYPE
    case Type.Int64 => java.lang.Long.TYPE
    case Type.Char => java.lang.Character.TYPE
    case Type.Float32 => java.lang.Float.TYPE
    case Type.Float64 => java.lang.Double.TYPE
    case Type.Cst(TypeConstructor.BigDecimal, _) => classOf[java.math.BigDecimal]
    case Type.Cst(TypeConstructor.BigInt, _) => classOf[java.math.BigInteger]
    case Type.Cst(TypeConstructor.Str, _) => classOf[String]
    case Type.Cst(TypeConstructor.Regex, _) => classOf[java.util.regex.Pattern]
    case Type.Cst(TypeConstructor.Native(clazz), _) => clazz

    // Parameterized Java types (e.g. ArrayList[String])
    case Type.Apply(_, _, _) if isNativeBase(tpe) =>
      tpe.baseType match {
        case Type.Cst(TypeConstructor.Native(clazz), _) => clazz
        case _ => classOf[Object]
      }

    // Arrays
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType, _), _, _) =>
      val t = getJavaType(elmType)
      t.arrayType()

    // Vectors
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), elmType, _) =>
      val t = getJavaType(elmType)
      t.arrayType()

    // Functions: map Flix Arrow types to Java functional interfaces.
    case Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), varArg, _), varRet, _) =>
      lookupFunIF(varArg, varRet) match {
        case Some(mapping) => mapping.javaClass
        case None => classOf[Object]
      }
    case _ => classOf[Object] // default
  }

  /** Tries to find a field of `thisObj` with the name `fieldName`. */
  private def lookupField(thisObj: Type, fieldName: String)(implicit scope: RegionScope, renv: RigidityEnv): JavaFieldResolution = {
    val typeIsKnown = isKnown(thisObj)
    if (!typeIsKnown) return JavaFieldResolution.UnresolvedTypes
    val opt = for {
      clazz <- Type.classFromFlixType(thisObj)
      field <- JvmUtils.getField(clazz, fieldName, static = false)
    } yield JavaFieldResolution.Resolved(field)
    opt.getOrElse(JavaFieldResolution.NotFound)
  }

  /**
    * Returns `true` if type is resolved enough for Java resolution.
    *
    * Rigid type variables are considered known — they erase to `Object` at runtime
    * (Java generics are erased). Flexible type variables are unknown because they
    * may later resolve to a specific type that selects a different method overload.
    */
  private def isKnown(tpe: Type)(implicit scope: RegionScope, renv: RigidityEnv): Boolean = tpe match {
    case Type.Var(sym, _) if tpe.kind == Kind.Eff => true
    case Type.Var(sym, _) => renv.isRigid(sym)
    case Type.Cst(_, _) => true
    case Type.JvmToType(_, _) => false
    case Type.JvmToEff(_, _) => false
    case Type.UnresolvedJvmType(_, _) => false
    case Type.Apply(t1, t2, _) =>
      // Native type applications are always known because Java erases generics at runtime,
      // so the type arguments do not affect method/field resolution.
      isNativeBase(tpe) || (isKnown(t1) && isKnown(t2))
    case Type.Alias(_, _, t, _) => isKnown(t)
    case Type.AssocType(_, _, _, _) => false
  }

  /** A lookup result of a Java field. */
  private sealed trait JavaFieldResolution

  private object JavaFieldResolution {

    /** One matching field. */
    case class Resolved(field: Field) extends JavaFieldResolution

    /** No matching field. */
    case object NotFound extends JavaFieldResolution

    /**
      * The types used for the lookup are not resolved enough to decide on a field.
      *
      * This happens if they contain e.g., type variables or associated types.
      */
    case object UnresolvedTypes extends JavaFieldResolution

  }

  /** A lookup result of a Java constructor. */
  private sealed trait JavaConstructorResolution

  private object JavaConstructorResolution {

    /** One matching constructor. */
    case class Resolved(constructor: Constructor[?]) extends JavaConstructorResolution

    /** No matching constructor. */
    case object NotFound extends JavaConstructorResolution

    /**
      * The types used for the lookup are not resolved enough to decide on a constructor.
      *
      * This happens if they contain e.g., type variables or associated types.
      */
    case object UnresolvedTypes extends JavaConstructorResolution

  }

  /** A lookup result of a Java method. */
  private sealed trait JavaMethodResolution

  private object JavaMethodResolution {

    /** One matching method. */
    case class Resolved(method: Method) extends JavaMethodResolution

    /** No matching method. */
    case object NotFound extends JavaMethodResolution

    /**
      * The types used for the lookup are not resolved enough to decide on a method.
      *
      * This happens if they contain e.g., type variables or associated types.
      */
    case object UnresolvedTypes extends JavaMethodResolution

  }

  /** Returns `true` if the base type of a chain of applications is a `Native` constructor. */
  @tailrec
  private def isNativeBase(tpe: Type): Boolean = tpe match {
    case Type.Cst(TypeConstructor.Native(_), _) => true
    case Type.Apply(t1, _, _) => isNativeBase(t1)
    case _ => false
  }

  /** Like `instantiateJavaTypeWithObjectArgs` but uses fresh type variables instead of `Object`. */
  private def instantiateJavaTypeWithFreshVars(clazz: Class[?], scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): Type = {
    val base = Type.getFlixType(clazz)
    val n = clazz.getTypeParameters.length
    if (n > 0) Type.mkApply(base, List.fill(n)(Type.freshVar(Kind.Star, loc)(scope, flix)), loc)
    else base
  }

  /**
    * Resolves the return type of `method`, using generic type information when available.
    *
    * Delegates to `resolveGenericType` which handles all `java.lang.reflect.Type` subtypes
    * including TypeVariable, ParameterizedType, WildcardType, GenericArrayType, and Class.
    *
    * Example 1: `ArrayList[String].get(int)` -- the generic return type is `E`.
    * The receiver `ArrayList[String]` maps `E -> String`, so the result is `String`.
    *
    * Example 2: `HashMap[String, Int32].get(Object)` -- the generic return type is `V`.
    * The receiver `HashMap[String, Int32]` maps `K -> String, V -> Int32`, so the result is `Int32`.
    *
    * Example 3: `HashMap[String, Int32].keySet()` -- the generic return type is `Set<K>`.
    * The receiver maps `K -> String`, so the result is `Set[String]`.
    *
    * Example 4: `String.length()` -- the return type is `int` (not a type variable).
    * Falls back to `instantiateJavaTypeWithFreshVars(int)` which yields `Int32`.
    */
  private def resolveMethodReturnType(method: Method, typeArgs: List[Type],
    scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): Type = {
    val substMap = buildTypeVarSubstitution(method, typeArgs)
    method.getGenericReturnType match {
      case tv: TypeVariable[_] =>
        // Bare type variable return (e.g., E from ArrayList<E>.get()).
        substMap.getOrElse(tv.getName, instantiateJavaTypeWithFreshVars(method.getReturnType, scope, loc))
      case _: ParameterizedType =>
        // Parameterized return type (e.g., Set<K> from HashMap.keySet()).
        // Resolve type arguments using the substitution map.
        resolveGenericType(method.getGenericReturnType, substMap, method.getReturnType, scope, loc)
      case _ =>
        // Other return types (Class, GenericArrayType, etc.).
        // Use erased return type with fresh vars for backward compatibility.
        instantiateJavaTypeWithFreshVars(method.getReturnType, scope, loc)
    }
  }

  /**
    * Resolves a `java.lang.reflect.Type` to a Flix `Type` using the given type variable
    * substitution map.
    *
    * Handles all subtypes of `java.lang.reflect.Type`:
    *   - `TypeVariable`: look up in substMap; fall back to erased class with fresh vars
    *   - `ParameterizedType`: resolve raw class + recursively resolve type arguments
    *   - `WildcardType`: resolve upper bound (defaults to Object)
    *   - `GenericArrayType`: recursively resolve component type, wrap in Array
    *   - `Class[_]`: use `instantiateJavaTypeWithFreshVars`
    *
    * @param genericType    the generic type from Java reflection
    * @param substMap       mapping from type variable names to Flix types
    * @param erasedFallback the erased class to use when a type variable is not in the map
    */
  private def resolveGenericType(genericType: java.lang.reflect.Type,
    substMap: Map[String, Type], erasedFallback: Class[?],
    scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): Type = {
    genericType match {
      case tv: TypeVariable[_] =>
        // Look up the type variable in the substitution map.
        // If not found (e.g., receiver has type variable args that were filtered out),
        // use a fresh variable so the result can unify with the expected type.
        substMap.getOrElse(tv.getName, Type.freshVar(Kind.Star, loc)(scope, flix))

      case pt: ParameterizedType =>
        // Resolve parameterized types like Set<K>, Map.Entry<K,V>, Iterator<E>, etc.
        val rawClass = pt.getRawType.asInstanceOf[Class[?]]
        val base = Type.getFlixType(rawClass)
        val typeArgs = pt.getActualTypeArguments.toList.map { arg =>
          resolveGenericType(arg, substMap, classOf[Object], scope, loc)
        }
        if (typeArgs.nonEmpty)
          Type.mkApply(base, typeArgs, loc)
        else
          base

      case wt: WildcardType =>
        // Resolve wildcard types like "? extends K" or "? super V".
        // Use the upper bound if it references a type variable (e.g., "? extends R"),
        // otherwise use a fresh type variable to avoid premature erasure to Object.
        wt.getUpperBounds.toList match {
          case (tv: TypeVariable[_]) :: _ =>
            resolveGenericType(tv, substMap, classOf[Object], scope, loc)
          case _ =>
            Type.freshVar(Kind.Star, loc)(scope, flix)
        }

      case gat: GenericArrayType =>
        // Resolve generic array types like "T[]".
        val componentType = resolveGenericType(gat.getGenericComponentType, substMap, classOf[Object], scope, loc)
        Type.mkArray(componentType, Type.IO, loc)

      case c: Class[?] =>
        // Plain class (non-generic or raw). Convert directly.
        instantiateJavaTypeWithFreshVars(c, scope, loc)

      case _ =>
        // Unknown type variant. Fall back to erased type.
        instantiateJavaTypeWithFreshVars(erasedFallback, scope, loc)
    }
  }

  /**
    * Builds a mapping from Java type variable names (e.g., `"E"`, `"K"`, `"V"`) to Flix types,
    * using the provided type arguments (class-level first, then method-level).
    *
    * Example: For `JvmMethod(HashMap.get)[String][Int32]`, where `HashMap<K, V>` declares
    * type parameters `K` and `V`, the typeArgs are `[String, Int32]` and the result is
    * `{"K" -> String, "V" -> Int32}`.
    *
    */
  private def buildTypeVarSubstitution(method: Method, typeArgs: List[Type]): Map[String, Type] = {
    val classParamNames: Array[String] =
      if (java.lang.reflect.Modifier.isStatic(method.getModifiers)) Array.empty
      else method.getDeclaringClass.getTypeParameters.map(_.getName)
    val methodParamNames: Array[String] = method.getTypeParameters.map(_.getName)
    val allParamNames = classParamNames ++ methodParamNames
    if (allParamNames.length == typeArgs.length)
      allParamNames.zip(typeArgs).toMap
    else
      Map.empty
  }

  /**
    * Instantiates a resolved Java method: creates fresh type variables for method-level
    * type parameters, builds the applied method type, and emits generic argument constraints.
    *
    * Example 1: `ArrayList[String].add("hello")` with classTypeArgs = [String]
    *   - methodTypeArgs = [] (add has no method-level type params)
    *   - emits Equality(String, String) for the `E` parameter
    *
    * Example 2: `Collections.singletonList("hello")` with classTypeArgs = [] (static)
    *   - methodTypeArgs = [?t] (fresh var for `T`)
    *   - emits Equality(?t, String) for the `T` parameter
    */
  private def instantiateMethod(method: Method, classTypeArgs: List[Type], argTypes: List[Type], scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): (Type, List[TypeConstraint]) = {
    val methodTypeArgs = method.getTypeParameters.toList.map(_ => Type.freshVar(Kind.Star, loc)(scope, flix))
    val allTypeArgs = classTypeArgs ++ methodTypeArgs
    val base = Type.Cst(TypeConstructor.JvmMethod(method), loc)
    val tpe = Type.mkApply(base, allTypeArgs, loc)
    val cs = mkArgConstraints(method, allTypeArgs, argTypes, scope, loc)
    (tpe, cs)
  }

  /**
    * Builds equality constraints linking actual argument types to the expected generic
    * parameter types of the resolved Java method.
    *
    * For each parameter whose generic type is a `TypeVariable` (e.g., `E` in
    * `add(E element)`), a `ParameterizedType` (e.g., `BodyHandler<T>` in
    * `send(req, handler)`), or a `GenericArrayType` (e.g., `T[]` in
    * `Stream.of(T...)`), resolves the expected type via the substitution map
    * and emits an equality constraint between the expected type and the actual
    * argument type.
    */
  private def mkArgConstraints(method: Method, typeArgs: List[Type],
    argTypes: List[Type], scope: RegionScope, loc: SourceLocation)
    (implicit flix: Flix): List[TypeConstraint] = {
    val substMap = buildTypeVarSubstitution(method, typeArgs)
    val genericParamTypes = method.getGenericParameterTypes
    argTypes.zip(genericParamTypes).flatMap { case (argType, genericParamType) =>
      genericParamType match {
        case tv: TypeVariable[_] =>
          substMap.get(tv.getName).map { expectedType =>
            TypeConstraint.Equality(expectedType, argType,
              TypeConstraint.Provenance.Match(expectedType, argType, loc))
          }
        case pt: ParameterizedType =>
          mkParamTypeConstraints(pt, argType, substMap, loc)
        case gat: GenericArrayType =>
          // For varargs/array params (e.g., T[] in Stream.of(T...)), emit a constraint
          // linking the component type variable to the Flix array/vector element type.
          (gat.getGenericComponentType, argType.typeArguments) match {
            case (tv: TypeVariable[_], elmType :: _) =>
              substMap.get(tv.getName).map { expectedType =>
                TypeConstraint.Equality(expectedType, elmType,
                  TypeConstraint.Provenance.Match(expectedType, elmType, loc))
              }.toList
            case _ => Nil
          }
        case _ => None
      }
    }
  }

  /**
    * Extracts class-level type arguments from the receiver type for a method,
    * mapped to the declaring class's type parameters.
    *
    * When the method is declared on a supertype (e.g., `Function.apply` found
    * on a `UnaryOperator[String]` receiver), uses the type hierarchy to
    * correctly map the receiver's type args to the declaring class's params.
    *
    * Falls back to fresh type variables for any params that cannot be resolved.
    */
  private def extractClassTypeArgs(method: Method, receiverType: Type,
    scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): List[Type] = {
    val declaringClass = method.getDeclaringClass
    val n = declaringClass.getTypeParameters.length
    if (n == 0) return Nil

    val receiverArgs = receiverType.typeArguments
    // Try to get the instantiated class from the receiver type's base.
    val instantiatedClassOpt = Type.classFromFlixType(receiverType)
    instantiatedClassOpt match {
      case Some(instClass) if instClass == declaringClass =>
        // Direct: receiver type args are for the declaring class.
        if (receiverArgs.length == n) receiverArgs
        else List.fill(n)(Type.freshVar(Kind.Star, loc)(scope, flix))
      case Some(instClass) =>
        // Inherited method: map through the type hierarchy.
        val indexMapping = JvmUtils.resolveTypeParamMapping(method, instClass)
        val declaringParamNames: Array[String] = declaringClass.getTypeParameters.map(_.getName)
        declaringParamNames.toList.map { name =>
          indexMapping.get(name) match {
            case Some(idx) if idx < receiverArgs.length => receiverArgs(idx)
            case _ => Type.freshVar(Kind.Star, loc)(scope, flix)
          }
        }
      case None =>
        List.fill(n)(Type.freshVar(Kind.Star, loc)(scope, flix))
    }
  }

  /**
    * Emits equality constraints for a `ParameterizedType` Java method parameter
    * by linking its type arguments to the corresponding Flix type arguments.
    *
    * Handles two cases:
    *
    * 1. **Arrow types** (Flix functions passed as Java functional interfaces):
    *    Uses `lookupFunIF` to determine which Arrow component (arg/ret) maps to
    *    which interface type param, then constrains the method's type variable
    *    against that component.
    *
    *    Example: `IntStream.mapToObj(IntFunction<? extends R>)` with arg `Int32 -> Object \ IO`:
    *    - `lookupFunIF` maps `IntFunction` to `FunIFMapping(retParam = Some("R"))`
    *    - The interface param `R` corresponds to the Arrow return type `Object`
    *    - The wildcard `? extends R` resolves to method type variable `R`
    *    - Emits constraint: `?r ~ Object`
    *
    * 2. **Native types** (Java objects like `BodyHandler[byte[]]`, `Class[A]`):
    *    Zips the Java type arguments with the Flix type arguments directly.
    */
  private def mkParamTypeConstraints(
    pt: ParameterizedType, argType: Type,
    substMap: Map[String, Type], loc: SourceLocation
  ): List[TypeConstraint] = {
    argType match {
      case Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), flixArg, _), flixRet, _) =>
        lookupFunIF(flixArg, flixRet) match {
          case Some(mapping) =>
            val fiTypeArgs: Map[String, Type] =
              mapping.argParam.map(_ -> flixArg).toMap ++
              mapping.retParam.map(_ -> flixRet).toMap
            val rawClass = pt.getRawType.asInstanceOf[Class[?]]
            val interfaceParamNames = rawClass.getTypeParameters.map(_.getName)
            interfaceParamNames.zip(pt.getActualTypeArguments).flatMap {
              case (ifParamName, javaTypeArg) =>
                resolveToTypeVariable(javaTypeArg).flatMap { methodTvName =>
                  for {
                    expectedType <- substMap.get(methodTvName)
                    flixType <- fiTypeArgs.get(ifParamName)
                  } yield TypeConstraint.Equality(expectedType, flixType,
                    TypeConstraint.Provenance.Match(expectedType, flixType, loc))
                }
            }.toList
          case None => Nil
        }
      case _ =>
        val javaTypeArgs = pt.getActualTypeArguments.toList
        val flixTypeArgs = argType.typeArguments
        javaTypeArgs.zip(flixTypeArgs).flatMap {
          case (tv: TypeVariable[_], flixArg) =>
            substMap.get(tv.getName).map { expectedType =>
              TypeConstraint.Equality(expectedType, flixArg,
                TypeConstraint.Provenance.Match(expectedType, flixArg, loc))
            }
          case _ => None
        }
    }
  }

  /** Extracts the underlying TypeVariable name from a Java generic type, resolving through wildcards. */
  private def resolveToTypeVariable(javaType: java.lang.reflect.Type): Option[String] = javaType match {
    case tv: TypeVariable[_] => Some(tv.getName)
    case wt: WildcardType =>
      wt.getUpperBounds.toList.collectFirst { case tv: TypeVariable[_] => tv.getName }
        .orElse(wt.getLowerBounds.toList.collectFirst { case tv: TypeVariable[_] => tv.getName })
    case _ => None
  }

  /**
    * Maps a Flix Arrow type to its Java functional interface.
    * `argParam`/`retParam` name the interface type param that corresponds
    * to the Arrow's argument/return type (None for primitive-specialized
    * interfaces like IntConsumer that have no type params).
    */
  private case class FunIFMapping(
    javaClass: Class[?],
    argParam: Option[String],
    retParam: Option[String]
  )

  /** Looks up the Java functional interface for a Flix Arrow with the given arg and ret types. */
  private def lookupFunIF(argType: Type, retType: Type): Option[FunIFMapping] = {
    import TypeConstructor.*
    (argType, retType) match {
      case (Type.Cst(Int32, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(classOf[java.util.function.IntConsumer], None, None))
      case (Type.Cst(Int32, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(classOf[java.util.function.IntPredicate], None, None))
      case (Type.Cst(Int32, _), Type.Cst(Int32, _)) =>
        Some(FunIFMapping(classOf[java.util.function.IntUnaryOperator], None, None))
      case (Type.Cst(Int32, _), _) =>
        Some(FunIFMapping(classOf[java.util.function.IntFunction[Object]], None, Some("R")))
      case (Type.Cst(Int64, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(classOf[java.util.function.LongConsumer], None, None))
      case (Type.Cst(Int64, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(classOf[java.util.function.LongPredicate], None, None))
      case (Type.Cst(Int64, _), Type.Cst(Int64, _)) =>
        Some(FunIFMapping(classOf[java.util.function.LongUnaryOperator], None, None))
      case (Type.Cst(Int64, _), _) =>
        Some(FunIFMapping(classOf[java.util.function.LongFunction[Object]], None, Some("R")))
      case (Type.Cst(Float64, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(classOf[java.util.function.DoubleConsumer], None, None))
      case (Type.Cst(Float64, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(classOf[java.util.function.DoublePredicate], None, None))
      case (Type.Cst(Float64, _), Type.Cst(Float64, _)) =>
        Some(FunIFMapping(classOf[java.util.function.DoubleUnaryOperator], None, None))
      case (Type.Cst(Float64, _), _) =>
        Some(FunIFMapping(classOf[java.util.function.DoubleFunction[Object]], None, Some("R")))
      case (_, Type.Cst(Unit, _)) =>
        Some(FunIFMapping(classOf[java.util.function.Consumer[Object]], Some("T"), None))
      case (_, Type.Cst(Bool, _)) =>
        Some(FunIFMapping(classOf[java.util.function.Predicate[Object]], Some("T"), None))
      case (_, _) =>
        Some(FunIFMapping(classOf[java.util.function.Function[Object, Object]], Some("T"), Some("R")))
    }
  }
}
