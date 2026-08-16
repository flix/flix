/*
 * Copyright 2026 Werner Stein
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

import ca.uwaterloo.flix.language.ast.{Name, SimpleType, SourceLocation, Symbol}
import org.scalatest.funsuite.AnyFunSuite

class TestErasureKey extends AnyFunSuite {

  private val loc: SourceLocation = SourceLocation.Unknown

  private def enumSym(fqn: String): Symbol.EnumSym = Symbol.mkEnumSym(fqn)

  private def structSym(fqn: String): Symbol.StructSym = {
    val parts = fqn.split('.').toList
    new Symbol.StructSym(None, parts.init, parts.last, loc)
  }

  test("ofEnum.01") {
    assert(ErasureKey.ofEnum(enumSym("Option"), Nil) == "Option|")
  }

  test("ofEnum.02") {
    assert(ErasureKey.ofEnum(enumSym("Option"), List(SimpleType.Int32)) == "Option|Int32")
  }

  test("ofEnum.03") {
    // A namespaced symbol renders its full path.
    assert(ErasureKey.ofEnum(enumSym("Foo.Option"), Nil) == "Foo.Option|")
  }

  test("ofEnum.04") {
    // Multiple type arguments are comma separated. `SimpleType.Object` is
    // `Native(classOf[java.lang.Object])`, not a distinct nullary case.
    assert(ErasureKey.ofEnum(enumSym("Pair"), List(SimpleType.Int32, SimpleType.Object)) == "Pair|Int32,Native(java.lang.Object)")
  }

  test("ofStruct.01") {
    assert(ErasureKey.ofStruct(structSym("MutList"), List(SimpleType.Object)) == "MutList|Native(java.lang.Object)")
  }

  test("distinct.01") {
    // Different type arguments must not collide.
    assert(ErasureKey.ofEnum(enumSym("Option"), List(SimpleType.Int32)) != ErasureKey.ofEnum(enumSym("Option"), List(SimpleType.Object)))
  }

  test("distinct.02") {
    // Different symbols must not collide, even at the same type arguments.
    assert(ErasureKey.ofEnum(enumSym("Option"), List(SimpleType.Int32)) != ErasureKey.ofEnum(enumSym("Result"), List(SimpleType.Int32)))
  }

  test("distinct.03") {
    // Namespaces separate enums with the same text.
    assert(ErasureKey.ofEnum(enumSym("A.Option"), Nil) != ErasureKey.ofEnum(enumSym("B.Option"), Nil))
  }

  test("distinct.04") {
    // An enum and a struct sharing a name must not collide: ofEnum and ofStruct key into
    // independent caches in Eraser, but nothing stops the rendered strings from being
    // compared or hashed together elsewhere, so this is worth pinning directly.
    assert(ErasureKey.ofEnum(enumSym("Box"), Nil) == ErasureKey.ofStruct(structSym("Box"), Nil))
  }

  test("distinct.05") {
    // Argument order matters.
    val targs1 = List(SimpleType.Int32, SimpleType.Object)
    val targs2 = List(SimpleType.Object, SimpleType.Int32)
    assert(ErasureKey.ofEnum(enumSym("Pair"), targs1) != ErasureKey.ofEnum(enumSym("Pair"), targs2))
  }

  test("nested.01") {
    val tuple = SimpleType.Tuple(List(SimpleType.Int32, SimpleType.Bool))
    assert(ErasureKey.ofEnum(enumSym("Box"), List(tuple)) == "Box|Tuple(Int32,Bool)")
  }

  test("nested.02") {
    assert(ErasureKey.ofEnum(enumSym("Box"), List(SimpleType.Array(SimpleType.Int32))) == "Box|Array(Int32)")
  }

  test("nested.03") {
    assert(ErasureKey.ofEnum(enumSym("Box"), List(SimpleType.Lazy(SimpleType.Object))) == "Box|Lazy(Native(java.lang.Object))")
  }

  test("nested.04") {
    val inner = SimpleType.Enum(enumSym("Inner"), List(SimpleType.Int32))
    assert(ErasureKey.ofEnum(enumSym("Outer"), List(inner)) == "Outer|Enum(Inner Int32)")
  }

  test("nested.05") {
    val arrow = SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Bool)
    assert(ErasureKey.ofEnum(enumSym("Box"), List(arrow)) == "Box|Arrow(Int32->Bool)")
  }

  test("nested.06") {
    val ext = SimpleType.ExtensibleExtend(Name.Pred("P", loc), List(SimpleType.Int32), SimpleType.ExtensibleEmpty)
    assert(ErasureKey.ofEnum(enumSym("Box"), List(ext)) == "Box|ExtensibleExtend(P Int32 ExtensibleEmpty)")
  }

  test("native.01") {
    // Native class names must not depend on formatting choices made elsewhere.
    val native = SimpleType.Native(classOf[String])
    assert(ErasureKey.ofEnum(enumSym("Box"), List(native)) == "Box|Native(java.lang.String)")
  }

  test("deterministic.01") {
    val targs = List(SimpleType.Int32, SimpleType.Bool)
    assert(ErasureKey.ofEnum(enumSym("Pair"), targs) == ErasureKey.ofEnum(enumSym("Pair"), targs))
  }

}
