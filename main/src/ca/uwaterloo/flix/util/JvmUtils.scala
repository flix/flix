/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.util

import java.lang.reflect.{Field, Member, Method, Modifier, ParameterizedType, TypeVariable}

object JvmUtils {

  /**
    * Returns the (static/dynamic) `fieldName` field of `clazz` if it exists.
    *
    * Field name "length" of array classes always return `None` (see Class.getField).
    *
    * @param clazz the class to search
    * @param static whether to find a static field or an instance field
    */
  def getField(clazz: Class[?], fieldName: String, static: Boolean): Option[Field] = {
    try {
      val field = clazz.getField(fieldName)
      if (isStatic(field) == static) Some(field) else None
    } catch {
      case _: NoSuchFieldException => None
    }
  }

  /**
    * Returns the static fields of the class.
    */
  def getStaticFields(clazz: Class[?]): List[Field] =
    clazz.getFields.toList.filter(isStatic)

  /**
    * Returns `true` if `member` is a static member ([[Field]] and [[Method]] extends [[Member]]).
    */
  def isStatic(member: Member): Boolean =
    java.lang.reflect.Modifier.isStatic(member.getModifiers)

  /**
    * Returns `true` if `member` is an instance member ([[Field]] and [[Method]] extends [[Member]]).
    */
  def isInstance(member: Member): Boolean =
    !isStatic(member)

  /**
    * Returns the methods of the class.
    *
    * If the given class is an array, the method `clone` is not included (see Class.getMethods).
    */
  def getMethods(clazz: Class[?]): List[Method] =
    getMethods(clazz, static = true, instance = true)

  /**
    * Returns the static methods of the class.
    */
  def getStaticMethods(clazz: Class[?]): List[Method] =
    getMethods(clazz, static = true, instance = false)

  /**
    * Returns the instance methods of the class.
    *
    * If the given class is an array, the method `clone` is not included (see Class.getMethods).
    */
  def getInstanceMethods(clazz: Class[?]): List[Method] =
    getMethods(clazz, static = false, instance = true)

  /**
    * Returns the methods of the class.
    *
    * If the given class is an array, the method `clone` is not included (see Class.getMethods).
    *
    * Returns `Nil` if both `static` and `instance` is `false`.
    *
    * @param clazz the class to search
    * @param static whether to include static methods
    * @param instance whether to include instance methods
    */
  private def getMethods(clazz: Class[?], static: Boolean, instance: Boolean): List[Method] = {
    val allMethods = clazz.getMethods.toList
    var methods = allMethods

    // We have to add the methods from Object manually for interfaces (see Class.getMethods).
    // Object only has instance methods, so skip if those are not relevant.
    if (instance && clazz.isInterface) {
      // If interface has `boolean eq(that: Object)` we should omit `Object.eq`
      // If interface I has `boolean eq(that: I)` we should include `Object.eq`
      val objectMethods = classOf[Object].getMethods.toList
      val undeclaredObjectMethods = objectMethods.filter {
        objectMethod => !allMethods.exists(methodsMatch(objectMethod, _))
      }
      methods = methods ::: undeclaredObjectMethods
    }

    // Remove static methods if not wanted.
    if (!static) methods = methods.filterNot(isStatic(_))
    // Remove instance methods if not wanted.
    if (!instance) methods = methods.filterNot(isInstance(_))
    methods
  }

  /**
    * Returns true if the methods are the same in regards to overloading.
    */
  private def methodsMatch(m1: Method, m2: Method): Boolean = {
    m1.getName == m2.getName &&
      isStatic(m1) == isStatic(m2) &&
      m1.getParameterTypes.sameElements(m2.getParameterTypes)
  }

  /**
    * Builds a mapping from the declaring class's type parameter names to
    * indices into `instantiatedClass.getTypeParameters`, walking the Java type
    * hierarchy from `instantiatedClass` up to `declaringClass`.
    *
    * For example, `UnaryOperator<T>` extends `Function<T, T>`.
    * If `method.getDeclaringClass` is `Function` (with params `[T, R]`)
    * and `instantiatedClass` is `UnaryOperator` (with param `[T]`):
    *   - `Function.T` maps to index 0 (UnaryOperator.T)
    *   - `Function.R` maps to index 0 (UnaryOperator.T)
    *
    * Returns `None` for type parameters that cannot be traced back to
    * the instantiated class (e.g., wildcards or concrete types in the hierarchy).
    *
    * @param method             the resolved Java method (may be declared on a supertype)
    * @param instantiatedClass  the class being instantiated (e.g., `UnaryOperator`)
    * @return a map from declaring-class type parameter name to index in `instantiatedClass.getTypeParameters`
    */
  def resolveTypeParamMapping(method: Method, instantiatedClass: Class[?]): Map[String, Int] = {
    val declaringClass = method.getDeclaringClass
    if (Modifier.isStatic(method.getModifiers)) return Map.empty
    if (declaringClass == instantiatedClass) {
      // Method declared directly on the class — identity mapping.
      instantiatedClass.getTypeParameters.map(_.getName).zipWithIndex.toMap
    } else {
      // Walk the hierarchy to find how declaringClass's type params map
      // to instantiatedClass's type params.
      resolveHierarchy(instantiatedClass, declaringClass) match {
        case Some(mapping) => mapping
        case None => Map.empty
      }
    }
  }

  /**
    * Walks the type hierarchy from `from` to `target`, returning a mapping from
    * `target`'s type parameter names to indices in `from`'s type parameters.
    *
    * At each level, `getGenericInterfaces` / `getGenericSuperclass` gives us a
    * `ParameterizedType` whose actual type arguments are either `TypeVariable`s
    * (referring to `from`'s params) or concrete types.
    */
  private def resolveHierarchy(from: Class[?], target: Class[?]): Option[Map[String, Int]] = {
    // Build an index: from's type param name -> position
    val fromIndex: Map[String, Int] =
      from.getTypeParameters.map(_.getName).zipWithIndex.toMap

    // Check each generic supertype
    val superclass: List[java.lang.reflect.Type] =
      Option(from.getGenericSuperclass).toList
    val genericSupertypes: List[java.lang.reflect.Type] =
      from.getGenericInterfaces.toList ::: superclass

    val results: Iterator[Option[Map[String, Int]]] = genericSupertypes.iterator.map { supertype =>
      getRawClass(supertype) match {
        case None => None
        case Some(rawClass) =>
          supertype match {
            case pt: ParameterizedType =>
              if (rawClass == target) {
                Some(buildMapping(target, pt, fromIndex))
              } else if (target.isAssignableFrom(rawClass)) {
                val intermediateIndex = buildIndexMapping(rawClass, pt, fromIndex)
                resolveHierarchy(rawClass, target).map { innerMap =>
                  innerMap.flatMap { case (name, rawIdx) =>
                    intermediateIndex.get(rawIdx).map(name -> _)
                  }
                }
              } else {
                None
              }
            case _ =>
              // Raw supertype (no generic info). Can't trace type params.
              if (rawClass == target) Some(Map.empty)
              else if (target.isAssignableFrom(rawClass)) resolveHierarchy(rawClass, target)
              else None
          }
      }
    }
    results.collectFirst { case Some(m) => m }
  }

  /**
    * Extracts the raw `Class[?]` from a `java.lang.reflect.Type`.
    */
  @SuppressWarnings(Array("unchecked"))
  private def getRawClass(tpe: java.lang.reflect.Type): Option[Class[?]] = tpe match {
    case pt: ParameterizedType => getRawClass(pt.getRawType)
    case c: Class[?] => Some(c)
    case _ => None
  }

  /**
    * Given a `ParameterizedType` (e.g., `Function<T, T>` where T refers to
    * UnaryOperator.T), maps the raw class's type parameter names to
    * indices in the `fromIndex` map.
    */
  private def buildMapping(targetClass: Class[?], pt: ParameterizedType, fromIndex: Map[String, Int]): Map[String, Int] = {
    val targetParamNames: Array[String] = targetClass.getTypeParameters.map(_.getName)
    val args = pt.getActualTypeArguments
    targetParamNames.zip(args).flatMap {
      case (name, tv: TypeVariable[_]) =>
        fromIndex.get(tv.getName).map(name -> _)
      case _ => None
    }.toMap
  }

  /**
    * Like `buildMapping`, but returns a map from the raw class's type parameter
    * *index* to the `fromIndex` index. Used for composing through intermediate classes.
    */
  private def buildIndexMapping(rawClass: Class[?], pt: ParameterizedType, fromIndex: Map[String, Int]): Map[Int, Int] = {
    val args = pt.getActualTypeArguments
    args.zipWithIndex.flatMap {
      case (tv: TypeVariable[_], i) =>
        fromIndex.get(tv.getName).map(i -> _)
      case _ => None
    }.toMap
  }

}
