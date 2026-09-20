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
import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.language.ast.{Scheme, TypedAst}
import ca.uwaterloo.flix.util.{Options, Sha256}
import org.scalatest.funsuite.AnyFunSuite

class TestEffectLock extends AnyFunSuite with TestUtils {

  /**
    * The options the programs of this suite are checked with.
    *
    * The chaos monkey is off, for the reason given in [[TestHashType]].
    */
  private val TestOptions: Options = Options.TestWithLibNix.copy(xchaosMonkey = false)

  private val clerk: PackageId = PackageId.mkPackageId("github:flix/museum-clerk").get

  private val other: Sha256 = Sha256.parse("sha256:" + "1" * 64).get

  /** A program whose `f` is pure. */
  private val Pure: String = "pub def f(): Unit = ()"

  /** A program whose `f` has an effect, and is otherwise [[Pure]]. */
  private val Effectful: String =
    """
      |eff Ef { def op(): Unit }
      |pub def f(): Unit \ Ef = Ef.op()
      |""".stripMargin

  //
  // Lock.
  //

  test("lock.01") {
    // What the user wrote belongs to no package, so there is nothing to lock.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    assert(EffectLock.lock(root, Set(clerk)) == EffectLockfile(Map.empty))
  }

  test("lock.02") {
    // Locking no package locks nothing, whatever the program declares.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    assert(EffectLock.lock(root, Set.empty) == EffectLockfile(Map.empty))
  }

  //
  // Check.
  //

  test("check.01") {
    // A signature that is the one that was locked is not reported.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", root)))
    assert(EffectLock.check(lockfile, root).isEmpty)
  }

  test("check.02") {
    // A signature that is not the one that was locked is reported.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("f" -> other))
    assert(EffectLock.check(lockfile, root).map { case (_, sym, _) => sym } == List("f"))
  }

  test("check.03") {
    // A locked declaration that the program no longer has is not reported: it cannot be called,
    // so it cannot do anything the lock did not allow.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("g" -> other))
    assert(EffectLock.check(lockfile, root).isEmpty)
  }

  test("check.04") {
    // A declaration the lock does not mention is not reported.
    val root = rootOf("pub def f(x: Int32): Int32 = x\npub def g(x: Bool): Bool = x")
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", root)))
    assert(EffectLock.check(lockfile, root).isEmpty)
  }

  test("check.05") {
    // Changes are reported in order of symbol.
    val root = rootOf("pub def f(x: Int32): Int32 = x\npub def g(x: Bool): Bool = x")
    val lockfile = lockOf(defs = Map("g" -> other, "f" -> other))
    assert(EffectLock.check(lockfile, root).map { case (_, sym, _) => sym } == List("f", "g"))
  }

  test("check.06") {
    // A sig is checked like a def.
    val input =
      """
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |""".stripMargin
    val root = rootOf(input)
    val lockfile = lockOf(sigs = Map("Sellable.price" -> other))
    assert(EffectLock.check(lockfile, root).map { case (_, sym, _) => sym } == List("Sellable.price"))
  }

  test("check.07") {
    // A widening of the effects of a declaration is reported.
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", rootOf(Pure))))
    assert(EffectLock.check(lockfile, rootOf(Effectful)).map { case (_, sym, _) => sym } == List("f"))
  }

  test("check.08") {
    // A narrowing of the effects of a declaration is reported too. The lock says whether a
    // signature is the one that was locked, not whether the change to it was a safe one.
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", rootOf(Effectful))))
    assert(EffectLock.check(lockfile, rootOf(Pure)).map { case (_, sym, _) => sym } == List("f"))
  }

  test("check.09") {
    // Renaming a type parameter is not a change.
    val locked = rootOf("pub def f(x: a): a = x")
    val upgrade = rootOf("pub def f(x: b): b = x")
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", locked)))
    assert(EffectLock.check(lockfile, upgrade).isEmpty)
  }

  /**
    * Returns the root of `input`, which must compile.
    */
  private def rootOf(input: String): TypedAst.Root = {
    val (root, errors) = check(input, TestOptions)
    expectSuccess((root, errors))
    root.get
  }

  /**
    * Returns the hash of the declared scheme of the def named `name` in `root`.
    */
  private def hashOf(name: String, root: TypedAst.Root): Sha256 = {
    HashType.hashScheme(schemeOf(name, root))
  }

  /**
    * Returns the declared scheme of the def named `name` in `root`.
    */
  private def schemeOf(name: String, root: TypedAst.Root): Scheme = {
    root.defs.collectFirst { case (sym, defn) if sym.text == name => defn.spec.declaredScheme }.get
  }

  /**
    * Returns a lock file that locks `defs` and `sigs` for one package.
    */
  private def lockOf(defs: Map[String, Sha256] = Map.empty, sigs: Map[String, Sha256] = Map.empty): EffectLockfile = {
    EffectLockfile(Map(clerk -> LockedPackage(defs, sigs)))
  }

}
