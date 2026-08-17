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
package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.language.ast.shared.{RegionScope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, SymId, Type, TypeConstructor}
import org.scalatest.funsuite.AnyFunSuite

class TestSpecializationKey extends AnyFunSuite {

  private val loc: SourceLocation = SourceLocation.Unknown

  private def defnSym(fqn: String, id: Option[String] = None): Symbol.DefnSym =
    Symbol.mkDefnSym(fqn, id.map(SymId.Hash.apply))

  private def cst(tc: TypeConstructor): Type = Type.Cst(tc, loc)

  private val Int32: Type = cst(TypeConstructor.Int32)
  private val Str: Type = cst(TypeConstructor.Str)
  private val Bool: Type = cst(TypeConstructor.Bool)

  test("of.01") {
    assert(SpecializationKey.of(defnSym("List.map"), Int32) == "List.map|Int32")
  }

  test("of.02") {
    // A def in the root namespace has no leading dot.
    assert(SpecializationKey.of(defnSym("main"), Int32) == "main|Int32")
  }

  test("of.03") {
    val tpe = Type.Apply(Type.Apply(cst(TypeConstructor.Tuple(2)), Int32, loc), Str, loc)
    assert(SpecializationKey.of(defnSym("A.f"), tpe) == "A.f|((Tuple(2) Int32) Str)")
  }

  test("distinct.01") {
    assert(SpecializationKey.of(defnSym("f"), Int32) != SpecializationKey.of(defnSym("f"), Str))
  }

  test("distinct.02") {
    assert(SpecializationKey.of(defnSym("f"), Int32) != SpecializationKey.of(defnSym("g"), Int32))
  }

  test("distinct.03") {
    // Namespaces separate defs with the same text.
    assert(SpecializationKey.of(defnSym("A.f"), Int32) != SpecializationKey.of(defnSym("B.f"), Int32))
  }

  test("includesDefnId.01") {
    // A trait's default implementation and an instance's share a qualified name and can be
    // specialized at the identical type, so the id is the only thing separating them.
    val instance = defnSym("Option.point", Some("41712"))
    val default = defnSym("Option.point")
    assert(SpecializationKey.of(instance, Int32) != SpecializationKey.of(default, Int32))
  }

  test("includesDefnId.02") {
    val a = defnSym("Eq.eq", Some("1"))
    val b = defnSym("Eq.eq", Some("2"))
    assert(SpecializationKey.of(a, Int32) != SpecializationKey.of(b, Int32))
  }

  test("includesDefnId.03") {
    assert(SpecializationKey.of(defnSym("f", Some("7")), Int32) == "f$7|Int32")
  }

  test("ignoresRegion.01") {
    // Regions carry a counter and are erased before code generation.
    val r1 = cst(TypeConstructor.Region(new Symbol.RegionSym(1, "r", loc)))
    val r2 = cst(TypeConstructor.Region(new Symbol.RegionSym(2, "r", loc)))
    assert(SpecializationKey.of(defnSym("f"), r1) == SpecializationKey.of(defnSym("f"), r2))
  }

  test("ignoresError.01") {
    // Error types carry a counter, and such a program never reaches code generation.
    val e1 = cst(TypeConstructor.Error(1, Kind.Star))
    val e2 = cst(TypeConstructor.Error(2, Kind.Star))
    assert(SpecializationKey.of(defnSym("f"), e1) == SpecializationKey.of(defnSym("f"), e2))
  }

  test("ignoresTypeVarIds.01") {
    // Variables are numbered by first appearance, so two types that differ only in which
    // variables inference happened to allocate render identically.
    val a = Type.Var(mkVar(100), loc)
    val b = Type.Var(mkVar(200), loc)
    assert(SpecializationKey.of(defnSym("f"), a) == SpecializationKey.of(defnSym("f"), b))
  }

  test("ignoresTypeVarIds.02") {
    // But the *shape* is preserved: (a, a) and (a, b) stay distinct.
    val x = Type.Var(mkVar(1), loc)
    val y = Type.Var(mkVar(2), loc)
    val same = Type.Apply(Type.Apply(cst(TypeConstructor.Tuple(2)), x, loc), x, loc)
    val diff = Type.Apply(Type.Apply(cst(TypeConstructor.Tuple(2)), x, loc), y, loc)
    assert(SpecializationKey.of(defnSym("f"), same) != SpecializationKey.of(defnSym("f"), diff))
  }

  test("ignoresLocation.01") {
    // Source positions must not reach a generated name.
    val here = Type.Cst(TypeConstructor.Int32, SourceLocation.Unknown)
    val there = Type.Cst(TypeConstructor.Int32, loc.asSynthetic)
    assert(SpecializationKey.of(defnSym("f"), here) == SpecializationKey.of(defnSym("f"), there))
  }

  test("followsAlias.01") {
    // Normalization removes aliases; if one survives, the key names what it stands for.
    val aliasSym = new Symbol.TypeAliasSym(Nil, "Celsius", loc)
    val alias = Type.Alias(ca.uwaterloo.flix.language.ast.shared.SymUse.TypeAliasSymUse(aliasSym, loc), Nil, Int32, loc)
    assert(SpecializationKey.of(defnSym("f"), alias) == SpecializationKey.of(defnSym("f"), Int32))
  }

  test("effectsMatter.01") {
    // An effect is part of the specialization, so it is part of the key.
    val pure = cst(TypeConstructor.Pure)
    val univ = cst(TypeConstructor.Univ)
    assert(SpecializationKey.of(defnSym("f"), pure) != SpecializationKey.of(defnSym("f"), univ))
  }

  test("deterministic.01") {
    val tpe = Type.Apply(Type.Apply(cst(TypeConstructor.Tuple(2)), Int32, loc), Bool, loc)
    assert(SpecializationKey.of(defnSym("f"), tpe) == SpecializationKey.of(defnSym("f"), tpe))
  }

  test("jvmMethod.01") {
    // A JVM descriptor, not reflection's own toString: declaring class + length-prefixed
    // name + parameter types + return type.
    val method = classOf[String].getMethod("length")
    val tpe = cst(TypeConstructor.JvmMethod(method))
    assert(SpecializationKey.of(defnSym("f"), tpe) == "f|JvmMethod(Ljava/lang/String;6:length()I)")
  }

  test("jvmMethod.02") {
    // Two overloads distinguished by parameter types alone.
    val indexOf1 = classOf[String].getMethod("indexOf", classOf[String])
    val indexOf2 = classOf[String].getMethod("indexOf", classOf[String], classOf[Int])
    val tpe1 = cst(TypeConstructor.JvmMethod(indexOf1))
    val tpe2 = cst(TypeConstructor.JvmMethod(indexOf2))
    assert(SpecializationKey.of(defnSym("f"), tpe1) != SpecializationKey.of(defnSym("f"), tpe2))
  }

  test("jvmMethod.03") {
    // The return type must be part of the key: a JVM class can contain two real, distinct
    // methods that share a declaring class, name, and parameter types but differ only in
    // return type -- a Java *source* compiler never emits this for a hand-written overload,
    // but it emits exactly this for a covariant-return override, as a synthetic bridge
    // method carrying the overridden (non-covariant) signature alongside the real one.
    // Verified against a real JDK example rather than a constructed one: CharBuffer.mark()
    // (added in JDK 9 to return CharBuffer instead of Buffer, for fluent chaining) compiles
    // to both the real, non-bridge `CharBuffer mark()` and a synthetic bridge `Buffer
    // mark()`, both declared on CharBuffer, both taking no arguments.
    val methods = classOf[java.nio.CharBuffer].getMethods.filter(m => m.getName == "mark" && m.getParameterCount == 0)
    val real = methods.find(!_.isBridge).getOrElse(fail("expected a non-bridge mark() on CharBuffer"))
    val bridge = methods.find(_.isBridge).getOrElse(fail("expected a bridge mark() on CharBuffer -- has the JDK changed CharBuffer's hierarchy?"))
    assert(real.getReturnType == classOf[java.nio.CharBuffer])
    assert(bridge.getReturnType == classOf[java.nio.Buffer])
    val realKey = SpecializationKey.of(defnSym("f"), cst(TypeConstructor.JvmMethod(real)))
    val bridgeKey = SpecializationKey.of(defnSym("f"), cst(TypeConstructor.JvmMethod(bridge)))
    assert(realKey != bridgeKey)
  }

  test("jvmConstructor.01") {
    val ctor = classOf[java.lang.Object].getConstructor()
    val tpe = cst(TypeConstructor.JvmConstructor(ctor))
    assert(SpecializationKey.of(defnSym("f"), tpe) == "f|JvmConstructor(Ljava/lang/Object;())")
  }

  test("jvmField.01") {
    val field = classOf[Integer].getField("MAX_VALUE")
    val tpe = cst(TypeConstructor.JvmField(field))
    assert(SpecializationKey.of(defnSym("f"), tpe) == "f|JvmField(Ljava/lang/Integer;9:MAX_VALUEI)")
  }

  /**
    * Returns a kinded type variable symbol with the given id.
    */
  private def mkVar(id: Int): Symbol.KindedTypeVarSym =
    new Symbol.KindedTypeVarSym(id, VarText.Absent, Kind.Star, isSlack = false, RegionScope.Top, loc)

}
