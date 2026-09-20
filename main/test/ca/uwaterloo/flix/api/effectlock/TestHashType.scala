/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.{RegionScope, TraitConstraint, VarText}
import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.{Kind, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{Options, Sha256}
import org.scalatest.funsuite.AnyFunSuite

class TestHashType extends AnyFunSuite with TestUtils {

  private val loc: SourceLocation = SourceLocation.Unknown

  private val scope: RegionScope = RegionScope.Top

  /**
    * The options the programs of this suite are checked with.
    *
    * The chaos monkey is off. It randomly permutes the trait and equality constraints of a
    * declaration in [[ca.uwaterloo.flix.language.phase.Namer]], to expose code that depends on
    * the order of a collection it should not depend on. The hash of a scheme does depend on that
    * order, deliberately: the hash is structural, and the compiler itself does not permute the
    * constraints of a declaration. Leaving the chaos monkey on would therefore make the tests
    * below fail at random, and what they would be reporting is the chaos monkey rather than
    * anything about the program being hashed.
    */
  private val TestOptions: Options = Options.TestWithLibNix.copy(xchaosMonkey = false)

  /** As [[TestOptions]], but with the standard library. */
  private val TestOptionsWithLib: Options = Options.TestWithLibAll.copy(xchaosMonkey = false)

  //
  // Determinism.
  //

  test("determinism.01") {
    // The same program, compiled twice, hashes the same.
    val input = "pub def f(x: a, y: b): (a, b) = (x, y)"
    assert(hashDef("f", input) == hashDef("f", input))
  }

  test("determinism.02") {
    // The identifiers of the type variables of `f` differ between the two programs, since the
    // declarations before it are handed identifiers first.
    val prefix =
      """
        |pub def g(x: c): c = x
        |pub def h(x: d, y: e, z: i): (d, e, i) = (x, y, z)
        |""".stripMargin
    val f = "pub def f(x: a, y: b): (a, b) = (x, y)"
    assert(hashDef("f", f) == hashDef("f", prefix + f))
  }

  test("determinism.03") {
    // The whole standard library hashes the same twice over. This also checks that no declaration
    // of the standard library contains a type that cannot be hashed.
    val hashes1 = hashLibrary()
    val hashes2 = hashLibrary()
    val differing = hashes1.filter { case (sym, hash) => !hashes2.get(sym).contains(hash) }.keys.toList.sorted
    assert(differing.isEmpty, s"the hash of ${differing.length} declaration(s) differs, e.g. ${differing.take(5)}")
  }

  //
  // Alpha renaming.
  //

  test("alpha.01") {
    assert(hashDef("f", "pub def f(x: a): a = x") == hashDef("f", "pub def f(x: b): b = x"))
  }

  test("alpha.02") {
    assert(hashDef("f", "pub def f(x: a, y: b): (a, b) = (x, y)") ==
      hashDef("f", "pub def f(x: c, y: d): (c, d) = (x, y)"))
  }

  test("alpha.03") {
    // Effect variables are renamed like any other variable.
    assert(hashDef("f", "pub def f(g: a -> b \\ ef): b \\ ef = ???") ==
      hashDef("f", "pub def f(g: x -> y \\ z): y \\ z = ???"))
  }

  test("alpha.04") {
    // The hash is blind to the names of the variables, but not to which variable is which.
    assert(hashDef("f", "pub def f(x: a, _y: b): a = x") !=
      hashDef("f", "pub def f(_x: a, y: b): b = y"))
  }

  test("alpha.05") {
    // Neither the name nor the identifier of a variable is hashed.
    val sym1 = new Symbol.KindedTypeVarSym(1, VarText.SourceText("a"), Kind.Star, isSlack = false, scope, loc)
    val sym2 = new Symbol.KindedTypeVarSym(7, VarText.SourceText("b"), Kind.Star, isSlack = false, scope, loc)
    assert(HashType.hashType(Type.Var(sym1, loc)) == HashType.hashType(Type.Var(sym2, loc)))
  }

  test("alpha.06") {
    // The kind of a variable is hashed, so two variables of different kinds are kept apart.
    val sym1 = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val sym2 = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Eff, isSlack = false, scope, loc)
    assert(HashType.hashType(Type.Var(sym1, loc)) != HashType.hashType(Type.Var(sym2, loc)))
  }

  //
  // Type aliases.
  //

  test("alias.01") {
    val withAlias =
      """
        |type alias A = Int32
        |pub def f(x: A): A = x
        |""".stripMargin
    assert(hashDef("f", withAlias) == hashDef("f", "pub def f(x: Int32): Int32 = x"))
  }

  //
  // Distinctness.
  //

  test("distinct.01") {
    val input =
      """
        |import java.lang.Object
        |
        |pub def f01(x: Int8): Int8 = x
        |pub def f02(x: Int16): Int16 = x
        |pub def f03(x: Int32): Int32 = x
        |pub def f04(x: Int64): Int64 = x
        |pub def f05(x: BigInt): BigInt = x
        |pub def f06(x: Float32): Float32 = x
        |pub def f07(x: Float64): Float64 = x
        |pub def f08(x: BigDecimal): BigDecimal = x
        |pub def f09(x: Char): Char = x
        |pub def f10(x: Bool): Bool = x
        |pub def f11(x: String): String = x
        |pub def f12(x: Unit): Unit = x
        |pub def f13(): (Int32, Int32) = ???
        |pub def f14(): (Int32, Int32, Int32) = ???
        |pub def f15(): { a = Int32 } = ???
        |pub def f16(): { b = Int32 } = ???
        |pub def f17(): { a = Int32, b = Int32 } = ???
        |pub def f18(): { a = Int32 | r } = ???
        |pub def f19(_x: Array[Int32, r]): Int32 \ r = ???
        |pub def f20(_x: Vector[Int32]): Int32 = ???
        |pub def f21(x: a): a = x
        |pub def f22(x: a, y: b): (a, b) = (x, y)
        |pub def f23(g: a -> b): a -> b = g
        |pub def f24(g: a -> b \ ef): a -> b \ ef = g
        |pub def f25(): E = E.C
        |pub def f26(): S[a] = ???
        |pub def f27(): Void \ Ef = Ef.op()
        |pub def f28(_g: a -> b \ (ef1 + ef2)): Void \ (ef1 + ef2) - Ef = ???
        |pub def f29(_x: Lazy[Int32]): Int32 = ???
        |pub def f30(_x: Object): Bool = ???
        |
        |enum E { case C }
        |enum S[a] { case D(a) }
        |eff Ef { def op(): Void }
        |""".stripMargin

    val (root, errors) = check(input, TestOptions)
    expectSuccess((root, errors))
    val hashes = root.get.defs.values.toList.map(d => HashType.hashScheme(d.spec.declaredScheme))
    assert(hashes.length == 30)
    assert(hashes.distinct.length == hashes.length)
  }

  //
  // Tags. Nodes that carry the same children are kept apart by the tag that names them.
  //

  test("tag.01") {
    val hashes = List(
      TypeConstructor.Arrow(2),
      TypeConstructor.Tuple(2),
      TypeConstructor.Relation(2),
      TypeConstructor.Lattice(2)
    ).map(tc => HashType.hashType(Type.Cst(tc, loc)))
    assert(hashes.distinct.length == hashes.length)
  }

  test("tag.02") {
    // A label and a predicate of the same name.
    assert(HashType.hashType(Type.Cst(TypeConstructor.RecordRowExtend(Name.Label("a", loc)), loc)) !=
      HashType.hashType(Type.Cst(TypeConstructor.SchemaRowExtend(Name.Pred("a", loc)), loc)))
  }

  test("tag.03") {
    // An enum and a struct of the same name.
    val enumSym = new Symbol.EnumSym(None, List("Foo"), "Bar", loc)
    val structSym = new Symbol.StructSym(None, List("Foo"), "Bar", loc)
    assert(HashType.hashType(Type.Cst(TypeConstructor.Enum(enumSym, Kind.Star), loc)) !=
      HashType.hashType(Type.Cst(TypeConstructor.Struct(structSym, Kind.Star), loc)))
  }

  test("tag.04") {
    // A scheme and the type it holds.
    val tpe = Type.Cst(TypeConstructor.Int32, loc)
    assert(HashType.hashScheme(Scheme(Nil, Nil, Nil, tpe)) != HashType.hashType(tpe))
  }

  test("namespace.01") {
    // The parts of a namespace are hashed one by one, so 'Foo.Bar' and 'Foob.Ar' differ.
    val sym1 = new Symbol.EnumSym(None, List("Foo"), "Bar", loc)
    val sym2 = new Symbol.EnumSym(None, List("Foob"), "Ar", loc)
    assert(HashType.hashType(Type.Cst(TypeConstructor.Enum(sym1, Kind.Star), loc)) !=
      HashType.hashType(Type.Cst(TypeConstructor.Enum(sym2, Kind.Star), loc)))
  }

  test("namespace.02") {
    // The parts of a namespace are counted, so 'Foo.Bar.Baz' and 'Foo.BarBaz' differ.
    val sym1 = new Symbol.EnumSym(None, List("Foo", "Bar"), "Baz", loc)
    val sym2 = new Symbol.EnumSym(None, List("Foo"), "BarBaz", loc)
    assert(HashType.hashType(Type.Cst(TypeConstructor.Enum(sym1, Kind.Star), loc)) !=
      HashType.hashType(Type.Cst(TypeConstructor.Enum(sym2, Kind.Star), loc)))
  }

  //
  // Kinds.
  //

  test("kind.01") {
    val hashes = List(
      Kind.Wild,
      Kind.WildCaseSet,
      Kind.Star,
      Kind.Eff,
      Kind.Bool,
      Kind.RecordRow,
      Kind.SchemaRow,
      Kind.Predicate,
      Kind.Arrow(Kind.Star, Kind.Star),
      Kind.Arrow(Kind.Star, Kind.Eff),
      Kind.Arrow(Kind.Eff, Kind.Star),
      Kind.Arrow(Kind.Arrow(Kind.Star, Kind.Star), Kind.Star),
      Kind.Arrow(Kind.Star, Kind.Arrow(Kind.Star, Kind.Star))
    ).map(HashType.hashKind)
    assert(hashes.distinct.length == hashes.length)
  }

  test("kind.02") {
    // The kind of an enum is hashed, so `E` and `E[a]` are kept apart.
    val sym = new Symbol.EnumSym(None, Nil, "E", loc)
    assert(HashType.hashType(Type.Cst(TypeConstructor.Enum(sym, Kind.Star), loc)) !=
      HashType.hashType(Type.Cst(TypeConstructor.Enum(sym, Kind.Arrow(Kind.Star, Kind.Star)), loc)))
  }

  //
  // Schemes.
  //

  test("scheme.01") {
    // A quantified variable and a variable that is free.
    val sym = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val tpe = Type.Var(sym, loc)
    assert(HashType.hashScheme(Scheme(List(sym), Nil, Nil, tpe)) !=
      HashType.hashScheme(Scheme(Nil, Nil, Nil, tpe)))
  }

  test("scheme.02") {
    // A quantifier that the scheme does not use does not affect the hash.
    val sym1 = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val sym2 = new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val tpe = Type.Var(sym1, loc)
    assert(HashType.hashScheme(Scheme(List(sym1), Nil, Nil, tpe)) ==
      HashType.hashScheme(Scheme(List(sym1, sym2), Nil, Nil, tpe)))
  }

  test("scheme.03") {
    // The order of the quantifiers is not hashed.
    val sym1 = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val sym2 = new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val tpe = Type.mkTuple(List(Type.Var(sym1, loc), Type.Var(sym2, loc)), loc)
    assert(HashType.hashScheme(Scheme(List(sym1, sym2), Nil, Nil, tpe)) ==
      HashType.hashScheme(Scheme(List(sym2, sym1), Nil, Nil, tpe)))
  }

  test("scheme.04") {
    // The order of the trait constraints is hashed: the hash is structural, so equal hashes mean
    // that two schemes are the same, whereas different hashes mean only that they may differ.
    val sym = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val tpe = Type.Var(sym, loc)
    val tconstr1 = TraitConstraint(TraitSymUse(new Symbol.TraitSym(Nil, "Eq", loc), loc), tpe, loc)
    val tconstr2 = TraitConstraint(TraitSymUse(new Symbol.TraitSym(Nil, "Order", loc), loc), tpe, loc)
    assert(HashType.hashScheme(Scheme(List(sym), List(tconstr1, tconstr2), Nil, tpe)) !=
      HashType.hashScheme(Scheme(List(sym), List(tconstr2, tconstr1), Nil, tpe)))
  }

  test("scheme.05") {
    // Which trait constraints a scheme has is hashed.
    val sym = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val tpe = Type.Var(sym, loc)
    val tconstr1 = TraitConstraint(TraitSymUse(new Symbol.TraitSym(Nil, "Eq", loc), loc), tpe, loc)
    val tconstr2 = TraitConstraint(TraitSymUse(new Symbol.TraitSym(Nil, "Order", loc), loc), tpe, loc)
    assert(HashType.hashScheme(Scheme(List(sym), List(tconstr1, tconstr2), Nil, tpe)) !=
      HashType.hashScheme(Scheme(List(sym), List(tconstr1), Nil, tpe)))
  }

  test("scheme.06") {
    // Two constraints on different variables are kept apart when the two schemes differ only in
    // which variable each constraint is on.
    val sym1 = new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val sym2 = new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Star, isSlack = false, scope, loc)
    val tpe1 = Type.Var(sym1, loc)
    val tpe2 = Type.Var(sym2, loc)
    val tpe = Type.mkTuple(List(tpe1, tpe2), loc)
    val eq = new Symbol.TraitSym(Nil, "Eq", loc)
    val order = new Symbol.TraitSym(Nil, "Order", loc)
    val scheme1 = Scheme(List(sym1, sym2), List(
      TraitConstraint(TraitSymUse(eq, loc), tpe1, loc),
      TraitConstraint(TraitSymUse(order, loc), tpe2, loc)
    ), Nil, tpe)
    val scheme2 = Scheme(List(sym1, sym2), List(
      TraitConstraint(TraitSymUse(eq, loc), tpe2, loc),
      TraitConstraint(TraitSymUse(order, loc), tpe1, loc)
    ), Nil, tpe)
    assert(HashType.hashScheme(scheme1) != HashType.hashScheme(scheme2))
  }

  //
  // Golden hashes. These pin the hashing scheme itself: a change to any of the rules, or to the
  // version of the scheme, changes them. Update them only when the change is deliberate.
  //

  test("golden.01") {
    assert(HashType.hashType(Type.Cst(TypeConstructor.Int32, loc)).toString ==
      "sha256:7dad1579ec59d87919c1fecc880115ed78f96e44f297b46a841c6989facc6128")
  }

  test("golden.02") {
    assert(HashType.hashKind(Kind.Star).toString ==
      "sha256:24067c6b1ee9d26b702a8857bc3c20384f3592b8e5013b5052f292823cdc3020")
  }

  test("golden.03") {
    val tpe = Type.mkPureArrow(Type.Cst(TypeConstructor.Int32, loc), Type.Cst(TypeConstructor.Bool, loc), loc)
    assert(HashType.hashScheme(Scheme(Nil, Nil, Nil, tpe)).toString ==
      "sha256:ed1223de424e1f84a12845f463ff9592a8c1a8f17f82c4afd841e80f5958f82e")
  }

  /**
    * Returns the hash of the declared scheme of the def named `name` in `input`.
    */
  private def hashDef(name: String, input: String): Sha256 = {
    val (root, errors) = check(input, TestOptions)
    expectSuccess((root, errors))
    val defn = root.get.defs.collectFirst { case (sym, defn) if sym.text == name => defn }.get
    HashType.hashScheme(defn.spec.declaredScheme)
  }

  /**
    * Returns the hash of the declared scheme of every def and sig of the standard library.
    */
  private def hashLibrary(): Map[String, Sha256] = {
    val (root, errors) = check("", TestOptionsWithLib)
    expectSuccess((root, errors))
    val defs = root.get.defs.map { case (sym, defn) => sym.toString -> HashType.hashScheme(defn.spec.declaredScheme) }
    val sigs = root.get.sigs.map { case (sym, sig) => sym.toString -> HashType.hashScheme(sig.spec.declaredScheme) }
    defs ++ sigs
  }

}
