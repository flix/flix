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
import ca.uwaterloo.flix.language.jvm.JavaClasses
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ConstantDescs.*
import java.lang.constant.ClassDesc

class TestJavaTypes extends AnyFunSuite {

  private val loc = SourceLocation.Unknown

  test("flixTypeOf.JavaType") {
    implicit val flix: Flix = new Flix
    try {
      val list = ClassDesc.of("java.util.List")
      val variable = JavaTypeVariable(JavaTypeVariableOwner.Class(list), "E")
      val subst: Map[JavaTypeVariable, Type] = Map(variable -> Type.Str)
      val obj = Type.mkObject(loc)
      // A bound variable takes its substitution; an unbound one and an unbounded wildcard take the fallback.
      assert(JavaTypes.flixTypeOf(JavaType.Variable(variable, CD_Object), subst, loc)(obj) == Type.Str)
      assert(JavaTypes.flixTypeOf(JavaType.Variable(variable, CD_Object), Map.empty, loc)(obj) == obj)
      assert(JavaTypes.flixTypeOf(JavaType.Wildcard(Nil, Nil, CD_Object), subst, loc)(obj) == obj)
      // A wildcard bounded by a variable and a generic array resolve through the variable.
      assert(JavaTypes.flixTypeOf(JavaType.Wildcard(List(JavaType.Variable(variable, CD_Object)), Nil, CD_Object), subst, loc)(obj) == Type.Str)
      assert(JavaTypes.flixTypeOf(JavaType.GenericArray(JavaType.Variable(variable, CD_Object), CD_Object.arrayType()), subst, loc)(obj) == Type.mkArray(Type.Str, Type.IO, loc))
      // Non-generic and parameterized types map to their Flix counterparts.
      assert(JavaTypes.flixTypeOf(JavaType.NonGeneric(CD_String), subst, loc)(obj) == Type.Str)
      assert(JavaTypes.flixTypeOf(JavaType.NonGeneric(CD_int), subst, loc)(obj) == Type.Int32)
      val listOfStr = JavaTypes.flixTypeOf(JavaType.Parameterized(list, List(JavaType.Variable(variable, CD_Object))), subst, loc)(obj)
      assert(listOfStr == Type.mkApply(Type.mkNative(list, 1, loc), List(Type.Str), loc))
      // The fallback is evaluated once per unbound position, so each can receive its own fresh variable.
      var evaluations = 0
      val rawList = JavaTypes.flixTypeOf(JavaType.NonGeneric(list), subst, loc)({ evaluations += 1; obj })
      val map = ClassDesc.of("java.util.Map")
      val k = JavaType.Variable(JavaTypeVariable(JavaTypeVariableOwner.Class(map), "K"), CD_Object)
      val v = JavaType.Variable(JavaTypeVariable(JavaTypeVariableOwner.Class(map), "V"), CD_Object)
      val mapOfUnbound = JavaTypes.flixTypeOf(JavaType.Parameterized(map, List(k, v)), subst, loc)({ evaluations += 1; obj })
      assert(rawList == Type.mkApply(Type.mkNative(list, 1, loc), List(obj), loc))
      assert(mapOfUnbound == Type.mkApply(Type.mkNative(map, 2, loc), List(obj, obj), loc))
      assert(evaluations == 3)
      listOfStr.typeConstructor match {
        case Some(TypeConstructor.Native(desc, arity)) => assert(desc == list && arity == 1)
        case other => fail(s"Unexpected type constructor: $other")
      }
    } finally flix.javaTypeProvider.close()
  }

  test("flixTypeOf.Descriptor") {
    val list = ClassDesc.of("java.util.List")
    assert(JavaTypes.flixTypeOf(CD_int, 0) == Type.Int32)
    assert(JavaTypes.flixTypeOf(CD_void, 0) == Type.Unit)
    assert(JavaTypes.flixTypeOf(CD_String, 0) == Type.Str)
    assert(JavaTypes.flixTypeOf(ClassDesc.of("java.math.BigInteger"), 0) == Type.BigInt)
    assert(JavaTypes.flixTypeOf(list, 1) == Type.mkNative(list, 1, loc))
    // The arity is only evaluated for a native type, so a special class never triggers it.
    var evaluated = false
    assert(JavaTypes.flixTypeOf(CD_String, { evaluated = true; 0 }) == Type.Str)
    assert(!evaluated)
    assert(JavaTypes.flixTypeOf(list, { evaluated = true; 1 }) == Type.mkNative(list, 1, loc))
    assert(evaluated)
    // An array of a generic class carries the arity of its element class.
    assert(JavaTypes.flixTypeOf(list.arrayType(), 1) == Type.mkArray(Type.mkNative(list, 1, loc), Type.IO, loc))
  }

  test("descriptorOf") {
    val list = ClassDesc.of("java.util.List")
    assert(JavaTypes.descriptorOf(Type.Int32) == Some(CD_int))
    assert(JavaTypes.descriptorOf(Type.Str) == Some(CD_String))
    assert(JavaTypes.descriptorOf(Type.mkNative(list, 1, loc)) == Some(list))
    assert(JavaTypes.descriptorOf(Type.mkApply(Type.mkNative(list, 1, loc), List(Type.Str), loc)) == Some(list))
    assert(JavaTypes.descriptorOf(Type.Unit) == None)
    assert(JavaTypes.descriptorOf(Type.mkArray(Type.Int32, Type.IO, loc)) == None)
  }

  test("formatType") {
    assert(JavaTypes.formatType(CD_int) == "Int32")
    assert(JavaTypes.formatType(CD_String) == "String")
    assert(JavaTypes.formatType(JavaClasses.BigInteger) == "BigInt")
    assert(JavaTypes.formatType(ClassDesc.of("java.util.Map$Entry")) == "java.util.Map$Entry")
    assert(JavaTypes.formatType(CD_int.arrayType()) == Type.mkArray(Type.Int32, Type.IO, loc).toString)
  }

}
