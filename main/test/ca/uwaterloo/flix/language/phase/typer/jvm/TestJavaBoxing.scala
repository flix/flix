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

import ca.uwaterloo.flix.language.ast.shared.JMethod
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ConstantDescs.*
import java.lang.constant.MethodTypeDesc

class TestJavaBoxing extends AnyFunSuite {

  private val loc = SourceLocation.Unknown

  test("isPrimitive") {
    assert(JavaBoxing.isPrimitive(Type.Bool))
    assert(JavaBoxing.isPrimitive(Type.Int32))
    assert(JavaBoxing.isPrimitive(Type.Float64))
    assert(!JavaBoxing.isPrimitive(Type.Str))
    assert(!JavaBoxing.isPrimitive(Type.Unit))
    assert(!JavaBoxing.isPrimitive(Type.mkObject(loc)))
    assert(!JavaBoxing.isPrimitive(Type.mkArray(Type.Int32, Type.IO, loc)))
  }

  test("boxMethod") {
    assert(JavaBoxing.boxMethod(Type.Int32) == JMethod(CD_Integer, "valueOf", MethodTypeDesc.of(CD_Integer, CD_int), isInterface = false))
    assert(JavaBoxing.boxMethod(Type.Bool) == JMethod(CD_Boolean, "valueOf", MethodTypeDesc.of(CD_Boolean, CD_boolean), isInterface = false))
    assert(JavaBoxing.boxMethod(Type.Char) == JMethod(CD_Character, "valueOf", MethodTypeDesc.of(CD_Character, CD_char), isInterface = false))
    assert(JavaBoxing.boxMethod(Type.Float64) == JMethod(CD_Double, "valueOf", MethodTypeDesc.of(CD_Double, CD_double), isInterface = false))
    assertThrows[InternalCompilerException](JavaBoxing.boxMethod(Type.Str))
  }

  test("unboxMethod") {
    assert(JavaBoxing.unboxMethod(Type.Int32) == JMethod(CD_Integer, "intValue", MethodTypeDesc.of(CD_int), isInterface = false))
    assert(JavaBoxing.unboxMethod(Type.Bool) == JMethod(CD_Boolean, "booleanValue", MethodTypeDesc.of(CD_boolean), isInterface = false))
    assert(JavaBoxing.unboxMethod(Type.Int64) == JMethod(CD_Long, "longValue", MethodTypeDesc.of(CD_long), isInterface = false))
    assert(JavaBoxing.unboxMethod(Type.Float32) == JMethod(CD_Float, "floatValue", MethodTypeDesc.of(CD_float), isInterface = false))
    assertThrows[InternalCompilerException](JavaBoxing.unboxMethod(Type.Unit))
  }

  test("boxedType") {
    assert(JavaBoxing.boxedType(Type.Int32, loc) == Type.mkNative(CD_Integer, 0, loc))
    assert(JavaBoxing.boxedType(Type.Int8, loc) == Type.mkNative(CD_Byte, 0, loc))
    assert(JavaBoxing.boxedType(Type.Int16, loc) == Type.mkNative(CD_Short, 0, loc))
    assertThrows[InternalCompilerException](JavaBoxing.boxedType(Type.Str, loc))
  }

}
