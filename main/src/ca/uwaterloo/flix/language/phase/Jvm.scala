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

package ca.uwaterloo.flix.language.phase

import java.lang.reflect.{Field, Member, Method}

object Jvm {

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
    * Returns `Nil` if both static` and `instance` is `false`.
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

}
