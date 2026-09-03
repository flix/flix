/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.jvm.{JavaType, JavaTypeVariable, JavaTypeVariableOwner}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ConstantDescs.*
import java.lang.constant.ClassDesc

class TestJavaTypes extends AnyFunSuite {

  private val loc = SourceLocation.Unknown

  test("isSubtype.ReferenceTypes") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaTypes.isSubtype(CD_String, CD_Object, loc))
      assert(JavaTypes.isSubtype(CD_String, ClassDesc.of("java.lang.CharSequence"), loc))
      assert(!JavaTypes.isSubtype(CD_Object, CD_String, loc))
      assert(!JavaTypes.isSubtype(CD_String, CD_Integer, loc))
    } finally flix.javaTypeProvider.close()
  }

  test("isSubtype.PrimitiveAndArrayTypes") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaTypes.isSubtype(CD_int, CD_int, loc))
      assert(!JavaTypes.isSubtype(CD_int, CD_long, loc))
      assert(!JavaTypes.isSubtype(CD_int, CD_Object, loc))
      assert(JavaTypes.isSubtype(CD_String.arrayType(), CD_Object, loc))
      assert(JavaTypes.isSubtype(CD_String.arrayType(), CD_Object.arrayType(), loc))
      assert(!JavaTypes.isSubtype(CD_int.arrayType(), CD_long.arrayType(), loc))
    } finally flix.javaTypeProvider.close()
  }

  test("isThrowable") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaTypes.isThrowable(CD_Throwable, loc))
      assert(JavaTypes.isThrowable(ClassDesc.of("java.lang.RuntimeException"), loc))
      assert(!JavaTypes.isThrowable(CD_String, loc))
      assert(!JavaTypes.isThrowable(CD_int, loc))
    } finally flix.javaTypeProvider.close()
  }

  test("isObjectMethod") {
    implicit val flix: Flix = new Flix
    try {
      // Comparator redeclares equals(Object), which Object also declares, but compare is Comparator's own.
      val methods = JavaTypes.overridableMethods(ClassDesc.of("java.util.Comparator"), loc)
      val equals = methods.find(_.ref.name == "equals").get
      val compare = methods.find(_.ref.name == "compare").get
      assert(JavaTypes.isObjectMethod(equals, loc))
      assert(!JavaTypes.isObjectMethod(compare, loc))
    } finally flix.javaTypeProvider.close()
  }

  test("flixTypeOf.JavaType") {
    implicit val flix: Flix = new Flix
    try {
      val list = ClassDesc.of("java.util.List")
      val variable = JavaTypeVariable(JavaTypeVariableOwner.Class(list), "E")
      val subst: Map[JavaTypeVariable, Type] = Map(variable -> Type.Str)
      // A bound variable takes its substitution; an unbound one, a generic array, and a wildcard fall back to Object.
      assert(JavaTypes.flixTypeOf(JavaType.Variable(variable, CD_Object), subst, loc) == Type.Str)
      assert(JavaTypes.flixTypeOf(JavaType.Variable(variable, CD_Object), Map.empty, loc) == Type.mkObject(loc))
      assert(JavaTypes.flixTypeOf(JavaType.GenericArray(JavaType.Variable(variable, CD_Object), CD_Object.arrayType()), subst, loc) == Type.mkObject(loc))
      assert(JavaTypes.flixTypeOf(JavaType.Wildcard(Nil, Nil, CD_Object), subst, loc) == Type.mkObject(loc))
      // Non-generic and parameterized types map to their Flix counterparts.
      assert(JavaTypes.flixTypeOf(JavaType.NonGeneric(CD_String), subst, loc) == Type.Str)
      assert(JavaTypes.flixTypeOf(JavaType.NonGeneric(CD_int), subst, loc) == Type.Int32)
      val listOfStr = JavaTypes.flixTypeOf(JavaType.Parameterized(list, List(JavaType.Variable(variable, CD_Object))), subst, loc)
      assert(listOfStr == Type.mkApply(Type.mkNative(list, 1, loc), List(Type.Str), loc))
      listOfStr.typeConstructor match {
        case Some(TypeConstructor.Native(desc, arity)) => assert(desc == list && arity == 1)
        case other => fail(s"Unexpected type constructor: $other")
      }
    } finally flix.javaTypeProvider.close()
  }

}
