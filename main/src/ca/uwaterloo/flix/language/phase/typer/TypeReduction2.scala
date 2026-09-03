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
import ca.uwaterloo.flix.language.ast.jvm.{JavaField, JavaMethod, JavaType, JavaTypeParameter, JavaTypeVariable}
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, RegionScope}
import ca.uwaterloo.flix.language.jvm.{ClassDescs, JavaArgument, JavaClasses, JavaMemberResolver}
import ca.uwaterloo.flix.language.phase.typer.jvm.{JavaTypes, PrimitiveEffects}
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Substitution}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ConstantDescs.*
import java.lang.constant.ClassDesc
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

    case Type.AssocType(symUse, tpe, kind, loc) =>
      val (t, cs) = reduce(tpe)

      // Get all the associated types from the context
      val assocOpt = eqenv.getAssocDef(symUse.sym, t)

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
        // Case 1: No match. We cannot reduce the head, but the argument may have
        // been reduced. We must reflect that in the returned type: reducing the
        // argument calls `progress.markProgress()`, so returning the original
        // `tpe0` here would signal progress without changing the type, causing
        // the constraint solver to loop forever (see issue #11213).
        // Performance: Reuse `tpe0` if the argument was unchanged.
        case None =>
          if (t eq tpe)
            (tpe0, cs)
          else
            (Type.AssocType(symUse, t, kind, loc), cs)

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
          (JavaTypes.instantiateWithFreshVars(constructor.ref.owner, scope, loc), cs)

        case Type.Cst(TypeConstructor.JvmField(field), _) =>
          progress.markProgress()
          (JavaTypes.instantiateWithFreshVars(field.ref.descriptor, scope, loc), cs)

        case t1 => t1.typeConstructor match {
          case Some(TypeConstructor.JvmMethod(method, classTypeParameters)) =>
            progress.markProgress()
            (resolveMethodReturnType(method, classTypeParameters, t1.typeArguments, scope, loc), cs)

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
          case Some(TypeConstructor.JvmMethod(method, _)) =>
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
          lookupConstructor(clazz, reducedTpes, loc) match {
            case JavaConstructorResolution.Resolved(constructor) =>
              progress.markProgress()
              (Type.Cst(TypeConstructor.JvmConstructor(constructor), loc), cs)
            case _ => (unresolved, cs)
          }

        case JvmMember.JvmField(_, tpe, name) =>
          val (reducedTpe, cs) = reduce(tpe)
          lookupField(reducedTpe, name.name, loc) match {
            case JavaFieldResolution.Resolved(field) =>
              progress.markProgress()
              (Type.Cst(TypeConstructor.JvmField(field), loc), cs)
            case _ => (unresolved, cs)
          }

        case JvmMember.JvmMethod(tpe, name, tpes) =>
          val (reducedTpe, cs0) = reduce(tpe)
          val (reducedTpes, css) = tpes.map(reduce(_)).unzip
          val cs = cs0 ::: css.flatten
          lookupMethod(reducedTpe, name.name, reducedTpes, loc) match {
            case JavaMethodResolution.Resolved(method) =>
              val classTypeParameters = classTypeParametersOf(method, getJavaTypeDesc(reducedTpe), loc)
              val classTypeArgs = extractClassTypeArgs(classTypeParameters, reducedTpe, scope, loc)
              val (tpe, cs0) = instantiateMethod(method, classTypeParameters, classTypeArgs, reducedTpes, scope, loc)
              progress.markProgress()
              (tpe, cs ::: cs0)
            case _ => (unresolved, cs)
          }

        case JvmMember.JvmStaticMethod(clazz, name, tpes) =>
          val (reducedTpes, css) = tpes.map(reduce(_)).unzip
          val cs = css.flatten
          lookupStaticMethod(clazz, name.name, reducedTpes, loc) match {
            case JavaMethodResolution.Resolved(method) =>
              // Class type parameters are not in scope for static methods.
              val (tpe, cs0) = instantiateMethod(method, Nil, Nil, reducedTpes, scope, loc)
              progress.markProgress()
              (tpe, cs ::: cs0)
            case _ => (unresolved, cs)
          }
      }
  }

  /** Tries to find a constructor of `owner` that takes arguments of type `ts`. */
  private def lookupConstructor(owner: ClassDesc, ts: List[Type], loc: SourceLocation)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): JavaConstructorResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaConstructorResolution.UnresolvedTypes

    val arguments = ts.map(getJavaArgument)
    JavaMemberResolver.constructors(owner, arguments) match {
      // The resolver returns every constructor tied for the best match, in class-file declaration order.
      // We deterministically pick the first one.
      case Ok(constructor :: _) => JavaConstructorResolution.Resolved(constructor)
      case Ok(Nil) => JavaConstructorResolution.NotFound
      case Err(error) =>
        val query = s"${ClassDescs.binaryNameOf(owner)}(${arguments.mkString(", ")})"
        throw InternalCompilerException(s"Java constructor lookup failed for '$query': $error", loc)
    }
  }

  /** Tries to find a method of `thisObj` that takes arguments of type `ts`. */
  private def lookupMethod(thisObj: Type, methodName: String, ts: List[Type], loc: SourceLocation)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): JavaMethodResolution = {
    val typesAreKnown = isKnown(thisObj) && ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    // Rigid type variables and other non-Java types fall back to Object.
    retrieveMethod(getJavaTypeDesc(thisObj), methodName, ts, static = false, loc)
  }

  /** Tries to find a static method of `owner` that takes arguments of type `ts`. */
  private def lookupStaticMethod(owner: ClassDesc, methodName: String, ts: List[Type], loc: SourceLocation)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): JavaMethodResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    retrieveMethod(owner, methodName, ts, static = true, loc)
  }

  /** Tries to find a static/dynamic method of `owner` that takes arguments of type `ts`. */
  private def retrieveMethod(owner: ClassDesc, methodName: String, ts: List[Type], static: Boolean, loc: SourceLocation)(implicit flix: Flix): JavaMethodResolution = {
    val arguments = ts.map(getJavaArgument)
    JavaMemberResolver.methods(owner, methodName, arguments, static) match {
      // The resolver returns every method tied for the best match in a deterministic order.
      // We pick the first one.
      case Ok(method :: _) => JavaMethodResolution.Resolved(method)
      case Ok(Nil) => JavaMethodResolution.NotFound
      case Err(error) =>
        val kind = if (static) "static" else "instance"
        val query = s"$kind ${ClassDescs.binaryNameOf(owner)}.$methodName(${arguments.mkString(", ")})"
        throw InternalCompilerException(s"Java method lookup failed for '$query': $error", loc)
    }
  }

  /** Returns the descriptor-based Java argument corresponding to the given Flix `tpe`. */
  private def getJavaArgument(tpe: Type): JavaArgument = tpe match {
    case Type.Cst(TypeConstructor.Null, _) => JavaArgument.Null
    case _ => JavaArgument.Typed(getJavaTypeDesc(tpe))
  }

  /** Returns the Java class descriptor corresponding to the given non-null Flix `tpe`. */
  private def getJavaTypeDesc(tpe: Type): ClassDesc = tpe match {
    case Type.Bool => CD_boolean
    case Type.Int8 => CD_byte
    case Type.Int16 => CD_short
    case Type.Int32 => CD_int
    case Type.Int64 => CD_long
    case Type.Char => CD_char
    case Type.Float32 => CD_float
    case Type.Float64 => CD_double
    case Type.Cst(TypeConstructor.BigDecimal, _) => JavaClasses.BigDecimal
    case Type.Cst(TypeConstructor.BigInt, _) => JavaClasses.BigInteger
    case Type.Cst(TypeConstructor.Str, _) => CD_String
    case Type.Cst(TypeConstructor.Regex, _) => JavaClasses.Regex
    case Type.Cst(TypeConstructor.Native(desc, _), _) => desc

    // Parameterized Java types erase to their native base type.
    case Type.Apply(_, _, _) if isNativeBase(tpe) =>
      tpe.baseType match {
        case Type.Cst(TypeConstructor.Native(desc, _), _) => desc
        case _ => CD_Object
      }

    // Arrays and vectors erase to Java arrays. A null element type falls back to Object.
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType, _), _, _) =>
      getJavaArrayTypeDesc(elmType)
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), elmType, _) =>
      getJavaArrayTypeDesc(elmType)

    // Functions map to the same Java functional interfaces as the reflective path.
    case Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), varArg, _), varRet, _) =>
      lookupFunIF(varArg, varRet).map(_.desc).getOrElse(CD_Object)
    case _ => CD_Object
  }

  /** Returns the Java array descriptor for an array or vector element type. */
  private def getJavaArrayTypeDesc(elmType: Type): ClassDesc = elmType match {
    case Type.Cst(TypeConstructor.Null, _) => CD_Object.arrayType()
    case _ => getJavaTypeDesc(elmType).arrayType()
  }

  /** Tries to find a field of `thisObj` with the name `fieldName`. */
  private def lookupField(thisObj: Type, fieldName: String, loc: SourceLocation)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): JavaFieldResolution = {
    val typeIsKnown = isKnown(thisObj)
    if (!typeIsKnown) return JavaFieldResolution.UnresolvedTypes

    val owner = getJavaTypeDesc(thisObj)
    JavaMemberResolver.field(owner, fieldName, static = false) match {
      case Ok(Some(field)) => JavaFieldResolution.Resolved(field)
      case Ok(None) => JavaFieldResolution.NotFound
      case Err(error) =>
        val query = s"${ClassDescs.binaryNameOf(owner)}.$fieldName"
        throw InternalCompilerException(s"Java field lookup failed for '$query': $error", loc)
    }
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
    case class Resolved(field: JavaField) extends JavaFieldResolution

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
    case class Resolved(constructor: JavaMethod) extends JavaConstructorResolution

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
    case class Resolved(method: JavaMethod) extends JavaMethodResolution

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
    case Type.Cst(TypeConstructor.Native(_, _), _) => true
    case Type.Apply(t1, _, _) => isNativeBase(t1)
    case _ => false
  }

  /**
    * Resolves the return type of `method`, using generic type information when available.
    *
    * Delegates to `resolveGenericType` which handles every kind of [[JavaType]].
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
    * Falls back to `JavaTypes.instantiateWithFreshVars(int)` which yields `Int32`.
    */
  private def resolveMethodReturnType(method: JavaMethod, classTypeParameters: List[JavaTypeParameter], typeArgs: List[Type],
    scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): Type = {
    val substMap = buildTypeVarSubstitution(method, classTypeParameters, typeArgs)
    method.returnType match {
      case JavaType.Variable(variable, erasure) =>
        // Bare type variable return (e.g., E from ArrayList<E>.get()).
        substMap.getOrElse(variable, JavaTypes.instantiateWithFreshVars(erasure, scope, loc))
      case returnType: JavaType.Parameterized =>
        // Parameterized return type (e.g., Set<K> from HashMap.keySet()).
        // Resolve type arguments using the substitution map.
        resolveGenericType(returnType, substMap, scope, loc)
      case returnType =>
        // Other return types (non-generic, generic arrays, etc.).
        // Use erased return type with fresh vars for backward compatibility.
        JavaTypes.instantiateWithFreshVars(returnType.erasure, scope, loc)
    }
  }

  /**
    * Resolves the Java `genericType` to a Flix [[Type]] using the given type variable substitution map.
    *
    * Handles every kind of [[JavaType]]:
    *   - `Variable`: look up in substMap; fall back to a fresh type variable
    *   - `Parameterized`: resolve the erased class + recursively resolve the type arguments
    *   - `Wildcard`: resolve the upper bound if it is a type variable; otherwise a fresh type variable
    *   - `GenericArray`: recursively resolve the component type, wrap in Array
    *   - `NonGeneric`: use `JavaTypes.instantiateWithFreshVars`
    */
  private def resolveGenericType(genericType: JavaType, substMap: Map[JavaTypeVariable, Type],
    scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): Type = genericType match {
    case JavaType.Variable(variable, _) =>
      // Look up the type variable in the substitution map.
      // If not found (e.g., receiver has type variable args that were filtered out),
      // use a fresh variable so the result can unify with the expected type.
      substMap.getOrElse(variable, Type.freshVar(Kind.Star, loc)(scope, flix))

    case JavaType.Parameterized(erasure, arguments) =>
      // Resolve parameterized types like Set<K>, Map.Entry<K,V>, Iterator<E>, etc.
      val base = JavaTypes.flixTypeOf(erasure, loc)
      val typeArgs = arguments.map(resolveGenericType(_, substMap, scope, loc))
      if (typeArgs.nonEmpty)
        Type.mkApply(base, typeArgs, loc)
      else
        base

    case JavaType.Wildcard(upperBounds, _, _) =>
      // Resolve wildcard types like "? extends K" or "? super V".
      // Use the upper bound if it references a type variable (e.g., "? extends R"),
      // otherwise use a fresh type variable to avoid premature erasure to Object.
      upperBounds match {
        case (variable: JavaType.Variable) :: _ =>
          resolveGenericType(variable, substMap, scope, loc)
        case _ =>
          Type.freshVar(Kind.Star, loc)(scope, flix)
      }

    case JavaType.GenericArray(component, _) =>
      // Resolve generic array types like "T[]".
      val componentType = resolveGenericType(component, substMap, scope, loc)
      Type.mkArray(componentType, Type.IO, loc)

    case JavaType.NonGeneric(erasure) =>
      // Plain class (non-generic or raw). Convert directly.
      JavaTypes.instantiateWithFreshVars(erasure, scope, loc)
  }

  /**
    * Builds a mapping from the Java type variables in scope for `method` to Flix types,
    * using the provided type arguments (class-level first, then method-level).
    *
    * `classTypeParameters` are the type parameters of the class that the method was looked up on
    * that are in scope (see [[TypeConstructor.JvmMethod]]).
    *
    * Example: For `JvmMethod(HashMap.get)[String][Int32]`, where `HashMap<K, V>` declares
    * type parameters `K` and `V`, the typeArgs are `[String, Int32]` and the result is
    * `{K -> String, V -> Int32}`.
    */
  private def buildTypeVarSubstitution(method: JavaMethod, classTypeParameters: List[JavaTypeParameter], typeArgs: List[Type]): Map[JavaTypeVariable, Type] = {
    val allParams = (classTypeParameters ::: method.typeParameters).map(_.variable)
    if (allParams.length == typeArgs.length)
      allParams.zip(typeArgs).toMap
    else
      Map.empty
  }

  /**
    * Returns the type parameters of `owner`, the class or interface that `method` was looked up on,
    * that are in scope for `method`: all of them for instance methods and none for static methods.
    *
    * The parameter and return types of a method found through the virtual method graph of `owner`
    * refer to the type parameters of `owner`, even if the method is declared by a supertype.
    */
  private def classTypeParametersOf(method: JavaMethod, owner: ClassDesc, loc: SourceLocation)(implicit flix: Flix): List[JavaTypeParameter] =
    if (method.isStatic || !owner.isClassOrInterface) Nil
    else JavaTypes.lookupClass(owner, loc).typeParameters

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
  private def instantiateMethod(method: JavaMethod, classTypeParameters: List[JavaTypeParameter], classTypeArgs: List[Type], argTypes: List[Type], scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): (Type, List[TypeConstraint]) = {
    val methodTypeArgs = method.typeParameters.map(_ => Type.freshVar(Kind.Star, loc)(scope, flix))
    val allTypeArgs = classTypeArgs ++ methodTypeArgs
    val base = Type.Cst(TypeConstructor.JvmMethod(method, classTypeParameters), loc)
    val tpe = Type.mkApply(base, allTypeArgs, loc)
    val cs = mkArgConstraints(method, classTypeParameters, allTypeArgs, argTypes, loc)
    (tpe, cs)
  }

  /**
    * Builds equality constraints linking actual argument types to the expected generic
    * parameter types of the resolved Java method.
    *
    * For each parameter whose generic type is a `Variable` (e.g., `E` in
    * `add(E element)`), a `Parameterized` type (e.g., `BodyHandler<T>` in
    * `send(req, handler)`), or a `GenericArray` (e.g., `T[]` in
    * `Stream.of(T...)`), resolves the expected type via the substitution map
    * and emits an equality constraint between the expected type and the actual
    * argument type.
    */
  private def mkArgConstraints(method: JavaMethod, classTypeParameters: List[JavaTypeParameter], typeArgs: List[Type],
    argTypes: List[Type], loc: SourceLocation)(implicit flix: Flix): List[TypeConstraint] = {
    val substMap = buildTypeVarSubstitution(method, classTypeParameters, typeArgs)
    argTypes.zip(method.parameterTypes).flatMap { case (argType, paramType) =>
      argType match {
        // `null` is a valid value of any reference type, so it must not constrain
        // the parameter's type variable. Emit no constraint and let the receiver
        // or surrounding context determine the type variable.
        case Type.Cst(TypeConstructor.Null, _) => None
        case _ => paramType match {
          case JavaType.Variable(variable, _) =>
            substMap.get(variable).map { expectedType =>
              TypeConstraint.Equality(expectedType, argType,
                TypeConstraint.Provenance.Match(expectedType, argType, loc))
            }
          case pt: JavaType.Parameterized =>
            mkParamTypeConstraints(pt, argType, substMap, loc)
          case JavaType.GenericArray(component, _) =>
            // For varargs/array params (e.g., T[] in Stream.of(T...)), emit a constraint
            // linking the component type variable to the Flix array/vector element type.
            (component, argType.typeArguments) match {
              case (JavaType.Variable(variable, _), elmType :: _) =>
                substMap.get(variable).map { expectedType =>
                  TypeConstraint.Equality(expectedType, elmType,
                    TypeConstraint.Provenance.Match(expectedType, elmType, loc))
                }.toList
              case _ => Nil
            }
          case _ => None
        }
      }
    }
  }

  /**
    * Extracts the class-level type arguments of the receiver type for the type parameters
    * `classTypeParameters` of the class that a method was looked up on.
    *
    * For example, `Function.apply` found on a `UnaryOperator[String]` receiver refers to
    * `UnaryOperator.T`, which is bound to `String`.
    *
    * Falls back to fresh type variables if the receiver does not carry one type argument per parameter.
    */
  private def extractClassTypeArgs(classTypeParameters: List[JavaTypeParameter], receiverType: Type,
    scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): List[Type] = {
    val receiverArgs = receiverType.typeArguments
    if (receiverArgs.length == classTypeParameters.length) receiverArgs
    else classTypeParameters.map(_ => Type.freshVar(Kind.Star, loc)(scope, flix))
  }

  /**
    * Emits equality constraints for a `Parameterized` Java method parameter
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
  private def mkParamTypeConstraints(pt: JavaType.Parameterized, argType: Type,
    substMap: Map[JavaTypeVariable, Type], loc: SourceLocation)(implicit flix: Flix): List[TypeConstraint] = {
    argType match {
      case Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), flixArg, _), flixRet, _) =>
        lookupFunIF(flixArg, flixRet) match {
          case Some(mapping) =>
            val fiTypeArgs: Map[String, Type] =
              mapping.argParam.map(_ -> flixArg).toMap ++
              mapping.retParam.map(_ -> flixRet).toMap
            val interfaceParamNames = JavaTypes.lookupClass(pt.erasure, loc).typeParameters.map(_.variable.name)
            interfaceParamNames.zip(pt.arguments).flatMap {
              case (ifParamName, javaTypeArg) =>
                resolveToTypeVariable(javaTypeArg).flatMap { methodTv =>
                  for {
                    expectedType <- substMap.get(methodTv)
                    flixType <- fiTypeArgs.get(ifParamName)
                  } yield TypeConstraint.Equality(expectedType, flixType,
                    TypeConstraint.Provenance.Match(expectedType, flixType, loc))
                }
            }
          case None => Nil
        }
      case _ =>
        pt.arguments.zip(argType.typeArguments).flatMap {
          case (JavaType.Variable(variable, _), flixArg) =>
            substMap.get(variable).map { expectedType =>
              TypeConstraint.Equality(expectedType, flixArg,
                TypeConstraint.Provenance.Match(expectedType, flixArg, loc))
            }
          case _ => None
        }
    }
  }

  /** Extracts the underlying type variable from a Java generic type, resolving through wildcards. */
  private def resolveToTypeVariable(javaType: JavaType): Option[JavaTypeVariable] = javaType match {
    case JavaType.Variable(variable, _) => Some(variable)
    case JavaType.Wildcard(upperBounds, lowerBounds, _) =>
      upperBounds.collectFirst { case JavaType.Variable(variable, _) => variable }
        .orElse(lowerBounds.collectFirst { case JavaType.Variable(variable, _) => variable })
    case _ => None
  }

  /**
    * Maps a Flix Arrow type to its Java functional interface.
    * `argParam`/`retParam` name the interface type param that corresponds
    * to the Arrow's argument/return type (None for primitive-specialized
    * interfaces like IntConsumer that have no type params).
    */
  private case class FunIFMapping(
    desc: ClassDesc,
    argParam: Option[String],
    retParam: Option[String]
  )

  /** Looks up the Java functional interface for a Flix Arrow with the given arg and ret types. */
  private def lookupFunIF(argType: Type, retType: Type): Option[FunIFMapping] = {
    import TypeConstructor.*
    (argType, retType) match {
      case (Type.Cst(Int32, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.IntConsumer, None, None))
      case (Type.Cst(Int32, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.IntPredicate, None, None))
      case (Type.Cst(Int32, _), Type.Cst(Int32, _)) =>
        Some(FunIFMapping(JavaClasses.IntUnaryOperator, None, None))
      case (Type.Cst(Int32, _), _) =>
        Some(FunIFMapping(JavaClasses.IntFunction, None, Some("R")))
      case (Type.Cst(Int64, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.LongConsumer, None, None))
      case (Type.Cst(Int64, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.LongPredicate, None, None))
      case (Type.Cst(Int64, _), Type.Cst(Int64, _)) =>
        Some(FunIFMapping(JavaClasses.LongUnaryOperator, None, None))
      case (Type.Cst(Int64, _), _) =>
        Some(FunIFMapping(JavaClasses.LongFunction, None, Some("R")))
      case (Type.Cst(Float64, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.DoubleConsumer, None, None))
      case (Type.Cst(Float64, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.DoublePredicate, None, None))
      case (Type.Cst(Float64, _), Type.Cst(Float64, _)) =>
        Some(FunIFMapping(JavaClasses.DoubleUnaryOperator, None, None))
      case (Type.Cst(Float64, _), _) =>
        Some(FunIFMapping(JavaClasses.DoubleFunction, None, Some("R")))
      case (_, Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.ObjConsumer, Some("T"), None))
      case (_, Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.ObjPredicate, Some("T"), None))
      case (_, _) =>
        Some(FunIFMapping(JavaClasses.ObjFunction, Some("T"), Some("R")))
    }
  }
}
