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
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import org.scalatest.funsuite.AnyFunSuite

class TestSpecializationKey extends AnyFunSuite {

  private val loc: SourceLocation = SourceLocation.Unknown

  private def defnSym(fqn: String, id: Option[String] = None): Symbol.DefnSym =
    Symbol.mkDefnSym(fqn, id)

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

  test("ignoresDefnId.01") {
    // The def's own id is excluded: it is a counter for instance defs, and the type the
    // instance is specialized at already separates them.
    val withId = defnSym("Eq.eq", Some("229042"))
    val withOther = defnSym("Eq.eq", Some("991"))
    assert(SpecializationKey.of(withId, Int32) == SpecializationKey.of(withOther, Int32))
    assert(SpecializationKey.of(withId, Int32) == SpecializationKey.of(defnSym("Eq.eq"), Int32))
  }

  test("ignoresDefnId.02") {
    // Two instances of one trait method still differ, because their types do.
    val eq = defnSym("Eq.eq", Some("1"))
    assert(SpecializationKey.of(eq, Int32) != SpecializationKey.of(eq, Str))
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

  /**
    * Returns a kinded type variable symbol with the given id.
    */
  private def mkVar(id: Int): Symbol.KindedTypeVarSym =
    new Symbol.KindedTypeVarSym(id, VarText.Absent, Kind.Star, isSlack = false, RegionScope.Top, loc)

}
