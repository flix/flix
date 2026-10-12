/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.effectlock.EffectLock.CheckResult
import ca.uwaterloo.flix.api.{Bootstrap, CompilerConstants, InstalledPackage}
import ca.uwaterloo.flix.language.ast.shared.{PackageId, SecurityContext}
import ca.uwaterloo.flix.language.ast.{Scheme, TypedAst}
import ca.uwaterloo.flix.tools.pkg.PkgTestUtils
import ca.uwaterloo.flix.util.{FileOps, Formatter, Options, Sha256}
import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

/**
  * The tests of effect locking.
  *
  * Not discovered, and run as part of [[ca.uwaterloo.flix.tools.pkg.PackageManagerSuite]]: the
  * end to end test downloads a package from GitHub, which is the same reason the tests of the
  * package manager are not run with the rest.
  */
@DoNotDiscover
class TestEffectLock extends AnyFunSuite with TestUtils {

  /**
    * The options the programs of this suite are checked with.
    *
    * The chaos monkey is off, for the reason given in [[TestHashType]].
    */
  private val TestOptions: Options = Options.TestWithLibNix.copy(xchaosMonkey = false)

  private val clerk: PackageId = PackageId.mkPackageId("github:flix/museum-clerk").get

  private val museum: PackageId = PackageId.mkPackageId("github:flix/museum").get

  private val other: Sha256 = Sha256.parse("sha256:" + "1" * 64).get

  /** A package whose `f` is pure. */
  private val Pure: String = "pub def f(): Unit = ()"

  /** A package whose `f` has an effect, and is otherwise [[Pure]]. */
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
    val root = rootOf(Map.empty, user = "pub def f(x: Int32): Int32 = x")
    assert(EffectLock.lock(root, Set(clerk)) == EffectLockfile(Map.empty))
  }

  test("lock.02") {
    // Locking no package locks nothing, whatever the packages declare.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    assert(EffectLock.lock(root, Set.empty) == EffectLockfile(Map.empty))
  }

  test("lock.03") {
    // A declaration is locked by its name inside its package, which does not name the package,
    // and only if it is public.
    val files = Map(
      "Main.flix" -> "pub def f(x: Int32): Int32 = hidden(x)\ndef hidden(x: Int32): Int32 = x",
      "Museum.flix" -> "pub mod Museum { }",
      "Museum/Clerk.flix" ->
        """
          |pub mod Museum.Clerk {
          |    pub def buy(): Unit = ()
          |    pub trait Sellable[a] {
          |        pub def price(x: a): Int32
          |    }
          |}
          |""".stripMargin
    )
    val lockfile = EffectLock.lock(rootOfFiles(Map(clerk -> files), user = ""), Set(clerk))
    assert(lockfile.packages.keySet == Set(clerk))
    assert(lockfile.packages(clerk).defs.keySet == Set("f", "Museum.Clerk.buy"))
    assert(lockfile.packages(clerk).sigs.keySet == Set("Museum.Clerk.Sellable.price"))
  }

  test("lock.04") {
    // Only a package that is named is locked, under its own identifier.
    val root = rootOf(Map(clerk -> "pub def f(x: Int32): Int32 = x", museum -> "pub def g(x: Bool): Bool = x"), user = "")
    val lockfile = EffectLock.lock(root, Set(museum))
    assert(lockfile.packages.keySet == Set(museum))
    assert(lockfile.packages(museum).defs.keySet == Set("g"))
  }

  //
  // Check.
  //

  test("check.01") {
    // A signature that is the one that was locked is compared and not reported.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", root)))
    assert(EffectLock.check(lockfile, root, Set(clerk)) == CheckResult(1, Nil))
  }

  test("check.02") {
    // A signature that is not the one that was locked is reported.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("f" -> other))
    val result = EffectLock.check(lockfile, root, Set(clerk))
    assert(result.compared == 1)
    assert(changedNames(result) == List("f"))
  }

  test("check.03") {
    // A locked declaration that the package no longer has is neither compared nor reported: it
    // cannot be called, so it cannot do anything the lock did not allow.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("g" -> other))
    assert(EffectLock.check(lockfile, root, Set(clerk)) == CheckResult(0, Nil))
  }

  test("check.04") {
    // A declaration the lock does not mention is not compared.
    val root = rootOf("pub def f(x: Int32): Int32 = x\npub def g(x: Bool): Bool = x")
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", root)))
    assert(EffectLock.check(lockfile, root, Set(clerk)) == CheckResult(1, Nil))
  }

  test("check.05") {
    // Changes are reported in order of name.
    val root = rootOf("pub def f(x: Int32): Int32 = x\npub def g(x: Bool): Bool = x")
    val lockfile = lockOf(defs = Map("g" -> other, "f" -> other))
    assert(changedNames(EffectLock.check(lockfile, root, Set(clerk))) == List("f", "g"))
  }

  test("check.06") {
    // A sig is checked like a def, under the trait it belongs to.
    val input =
      """
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |""".stripMargin
    val root = rootOf(input)
    val lockfile = lockOf(sigs = Map("Sellable.price" -> other))
    val result = EffectLock.check(lockfile, root, Set(clerk))
    assert(result.compared == 1)
    assert(changedNames(result) == List("Sellable.price"))
  }

  test("check.07") {
    // A widening of the effects of a declaration is reported.
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", rootOf(Pure))))
    assert(changedNames(EffectLock.check(lockfile, rootOf(Effectful), Set(clerk))) == List("f"))
  }

  test("check.08") {
    // A narrowing of the effects of a declaration is reported too. The lock says whether a
    // signature is the one that was locked, not whether the change to it was a safe one.
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", rootOf(Effectful))))
    assert(changedNames(EffectLock.check(lockfile, rootOf(Pure), Set(clerk))) == List("f"))
  }

  test("check.09") {
    // Renaming a type parameter is not a change.
    val locked = rootOf("pub def f(x: a): a = x")
    val upgrade = rootOf("pub def f(x: b): b = x")
    val lockfile = lockOf(defs = Map("f" -> hashOf("f", locked)))
    assert(EffectLock.check(lockfile, upgrade, Set(clerk)) == CheckResult(1, Nil))
  }

  test("check.10") {
    // A package that the targets do not name is not checked.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("f" -> other))
    assert(EffectLock.check(lockfile, root, Set(museum)) == CheckResult(0, Nil))
  }

  test("check.11") {
    // Naming one package checks that one and no other, so that a package whose signatures have
    // drifted does not stand in the way of checking another.
    val root = rootOf(Map(clerk -> "pub def f(x: Int32): Int32 = x", museum -> "pub def g(x: Bool): Bool = x"), user = "")
    val lockfile = EffectLockfile(Map(
      clerk -> LockedPackage(Map("f" -> other), Map.empty),
      museum -> LockedPackage(Map("g" -> other), Map.empty)
    ))
    assert(EffectLock.check(lockfile, root, Set(clerk)).changed.map { case (id, name, _) => (id, name) } == List((clerk, "f")))
    assert(EffectLock.check(lockfile, root, Set(museum)).changed.map { case (id, name, _) => (id, name) } == List((museum, "g")))
    assert(changedNames(EffectLock.check(lockfile, root, Set(clerk, museum))) == List("g", "f"))
  }

  test("check.12") {
    // Naming no package checks nothing, since nothing is a target.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map("f" -> other))
    assert(EffectLock.check(lockfile, root, Set.empty) == CheckResult(0, Nil))
  }

  test("check.13") {
    // A locked declaration is looked for in the package it is locked under, and nowhere else:
    // neither the user's `f` nor the clerk's `f` is the museum's.
    val root = rootOf(Map(clerk -> "pub def f(x: Int32): Int32 = x", museum -> ""), user = "pub def f(x: Int32): Int32 = x")
    val lockfile = EffectLockfile(Map(museum -> LockedPackage(Map("f" -> other), Map.empty)))
    assert(EffectLock.check(lockfile, root, Set(clerk, museum)) == CheckResult(0, Nil))
  }

  test("check.14") {
    // A locked name that carries the package, which is not how a declaration is named, matches
    // nothing, and the count says that nothing was compared.
    val root = rootOf("pub def f(x: Int32): Int32 = x")
    val lockfile = lockOf(defs = Map(s"$clerk.f" -> other))
    assert(EffectLock.check(lockfile, root, Set(clerk)) == CheckResult(0, Nil))
  }

  test("check.15") {
    // A locked declaration that is no longer public is not compared: it cannot be called.
    val root = rootOf("def f(x: Int32): Int32 = x\npub def g(): Int32 = f(1)")
    val lockfile = lockOf(defs = Map("f" -> other))
    assert(EffectLock.check(lockfile, root, Set(clerk)) == CheckResult(0, Nil))
  }

  test("check.16") {
    // What was compared is counted over defs and sigs alike, leaving out what is no longer
    // declared.
    val input =
      """
        |pub def f(x: Int32): Int32 = x
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |""".stripMargin
    val root = rootOf(input)
    val lockfile = lockOf(
      defs = Map("f" -> hashOf("f", root), "g" -> other),
      sigs = Map("Sellable.price" -> other, "Sellable.gone" -> other)
    )
    val result = EffectLock.check(lockfile, root, Set(clerk))
    assert(result.compared == 2)
    assert(changedNames(result) == List("Sellable.price"))
  }

  //
  // End to end.
  //

  test("effects.lock.01") {
    // Lock the signatures of a package that is really downloaded, and check them.
    val p = Files.createTempDirectory("flix-project-")
    Bootstrap.init(p)(System.out)
    // N.B.: `extras` calls Java, which the default security context forbids.
    FileOps.writeString(p.resolve(Bootstrap.FLIX_TOML), PkgTestUtils.mkTomlWithDeps(
      """"github:flix/extras" = { version = "0.3.0", security = "unrestricted" }"""))

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.NoFormatter, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap), None)(System.out).unsafeGet

    val lockfile = EffectLockfileParser.parse(p.resolve(Bootstrap.EFFECTS_LOCK)).unsafeGet
    val id = PackageId.mkPackageId("github:flix/extras").get
    val extras = lockfile.packages(id)
    // A declaration is locked by its name inside the package, under the entry of the package.
    assert(extras.defs.contains("Extras.Graph.closure"))
    assert(extras.sigs.isEmpty)

    // Nothing has changed since the signatures were locked, so the check passes, whether every
    // package is checked or only the one.
    bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), None)(System.out).unsafeGet
    bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), Some("flix/extras"))(System.out).unsafeGet
  }

  test("effects.lock.02") {
    // A package the project has not installed can be neither locked nor checked, and both
    // commands say so in the same way.
    val p = Files.createTempDirectory("flix-project-")
    Bootstrap.init(p)(System.out)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.NoFormatter, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap), None)(System.out).unsafeGet

    assert(bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap), Some("flix/extras"))(System.out).toOption.isEmpty)
    assert(bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), Some("flix/extras"))(System.out).toOption.isEmpty)
  }

  /**
    * Returns the root of a program in which the clerk package has the source `input` and the
    * user wrote nothing. The program must compile.
    */
  private def rootOf(input: String): TypedAst.Root = rootOf(Map(clerk -> input), user = "")

  /**
    * Returns the root of a program in which each of `packages` is installed with the given
    * source as its one file, and `user` is what the user wrote. The program must compile.
    */
  private def rootOf(packages: Map[PackageId, String], user: String): TypedAst.Root =
    rootOfFiles(packages.map { case (id, source) => id -> Map("Main.flix" -> source) }, user)

  /**
    * Returns the root of a program in which each of `packages` is installed with the given
    * files, each file name to its source, and `user` is what the user wrote. The program must
    * compile.
    *
    * Nothing mounts anything: a lock is about what a package declares, not what reaches it.
    */
  private def rootOfFiles(packages: Map[PackageId, Map[String, String]], user: String): TypedAst.Root = {
    val pkgs = packages.toList.map {
      case (id, files) => InstalledPackage(PkgTestUtils.mkFpkg(id.name, files), id, SecurityContext.Unrestricted, Map.empty)
    }
    val flix = PkgTestUtils.mkFlix(pkgs, Map.empty)
    flix.setOptions(TestOptions)
    flix.addSource(CompilerConstants.VirtualTestFile, user, sctx)
    val (root, errors) = flix.check()
    expectSuccess((root, errors))
    root.get
  }

  /**
    * Returns the names of the declarations that `result` reports as changed, in the order they
    * are reported.
    */
  private def changedNames(result: CheckResult): List[String] = result.changed.map { case (_, name, _) => name }

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
    * Returns a lock file that locks `defs` and `sigs` for the clerk package.
    */
  private def lockOf(defs: Map[String, Sha256] = Map.empty, sigs: Map[String, Sha256] = Map.empty): EffectLockfile = {
    EffectLockfile(Map(clerk -> LockedPackage(defs, sigs)))
  }

}
