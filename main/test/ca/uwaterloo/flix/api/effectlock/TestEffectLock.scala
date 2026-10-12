/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.effectlock.EffectLock.CheckResult
import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, CompilerConstants, InstalledPackage}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{PackageId, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.PkgTestUtils
import ca.uwaterloo.flix.util.Result.{Err, Ok}
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

  test("lock") {
    // The clerk declares a def at its root, a private def, a def in a nested module, and a sig.
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
    val root = rootOf(Map(clerk -> files, museum -> Map("Main.flix" -> "pub def g(x: Bool): Bool = x")), user = "pub def h(x: Int32): Int32 = x")

    // A declaration is locked by its name inside its package, which does not name the package,
    // and only if it is public. Only a package that is named is locked, under its own
    // identifier, and what the user wrote belongs to no package.
    val lockfile = EffectLock.lock(root, Set(clerk))
    assert(lockfile.packages.keySet == Set(clerk))
    assert(lockfile.packages(clerk).defs.keySet == Set("f", "Museum.Clerk.buy"))
    assert(lockfile.packages(clerk).sigs.keySet == Set("Museum.Clerk.Sellable.price"))

    assert(EffectLock.lock(root, Set(museum)).packages(museum).defs.keySet == Set("g"))
    assert(EffectLock.lock(root, Set(clerk, museum)).packages.keySet == Set(clerk, museum))
    assert(EffectLock.lock(root, Set.empty) == EffectLockfile(Map.empty))
  }

  test("check") {
    // The clerk as it was locked, and after an upgrade. Each declaration of the upgrade stands
    // for one rule of the check, as its comment says.
    val before =
      """
        |eff Ef { def op(): Unit }
        |pub def same(): Unit = ()
        |pub def wider(): Unit = ()
        |pub def narrower(): Unit \ Ef = Ef.op()
        |pub def renamed(x: a): a = x
        |pub def gone(): Unit = ()
        |pub def hidden(): Unit = ()
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |""".stripMargin
    val after =
      """
        |eff Ef { def op(): Unit }
        |pub def same(): Unit = ()             // compared, and not reported.
        |pub def wider(): Unit \ Ef = Ef.op()  // reported: an effect was added.
        |pub def narrower(): Unit = ()         // reported: a narrowing is a change too.
        |pub def renamed(x: b): b = x          // compared, and not reported: a type parameter is a binder.
        |                                      // gone: not compared, since it is no longer declared.
        |def hidden(): Unit = ()               // not compared: no longer public, so it cannot be called.
        |pub def added(): Unit = hidden()      // not compared: not locked.
        |pub def shared(): Unit = ()           // not compared: locked for the museum, not for the clerk.
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32 \ Ef   // reported: a sig is checked like a def, under its trait.
        |}
        |""".stripMargin
    // The museum drifts too, and loses the `shared` that the clerk and the user then declare.
    val museumBefore = "pub def m(): Unit = ()\npub def shared(): Unit = ()"
    val museumAfter = "eff Ef { def op(): Unit }\npub def m(): Unit \\ Ef = Ef.op()"
    val locked = rootOf(Map(clerk -> Map("Main.flix" -> before), museum -> Map("Main.flix" -> museumBefore)), user = "")
    val upgrade = rootOf(Map(clerk -> Map("Main.flix" -> after), museum -> Map("Main.flix" -> museumAfter)), user = "pub def shared(): Unit = ()")
    val lockfile = EffectLock.lock(locked, Set(clerk, museum))

    // What the clerk still declares publicly is compared, and the changes are reported as the
    // defs in order of name, then the sigs.
    val result = EffectLock.check(lockfile, upgrade, Set(clerk))
    assert(result.compared == 5)
    assert(changedNames(result) == List("narrower", "wider", "Sellable.price"))

    // Naming one package checks that one and no other, so that a package whose signatures have
    // drifted does not stand in the way of checking another. A locked declaration is looked for
    // in the package it is locked under, and nowhere else: neither the clerk's `shared` nor the
    // user's is the museum's.
    val one = EffectLock.check(lockfile, upgrade, Set(museum))
    assert(one.compared == 1)
    assert(one.changed.map { case (id, sym, _) => (id, EffectLock.nameOf(sym)) } == List((museum, "m")))
    val both = EffectLock.check(lockfile, upgrade, Set(clerk, museum))
    assert(both.compared == 6)
    assert(changedNames(both) == List("m", "narrower", "wider", "Sellable.price"))

    // Naming no package checks nothing, since nothing is a target.
    assert(EffectLock.check(lockfile, upgrade, Set.empty) == CheckResult(0, Nil))
  }

  test("effects.lock") {
    // Lock the signatures of a package that is really downloaded, check them, and see a change to
    // one of them reported.
    val p = Files.createTempDirectory("flix-project-")
    Bootstrap.init(p)(System.out)
    // N.B.: `extras` calls Java, which the default security context forbids.
    FileOps.writeString(p.resolve(Bootstrap.FLIX_TOML), PkgTestUtils.mkTomlWithDeps(
      """"github:flix/extras" = { version = "0.3.0", security = "unrestricted" }"""))

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.NoFormatter, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap), None)(System.out).unsafeGet

    // A declaration is locked by its name inside the package, under the entry of the package.
    val lockfile = EffectLockfileParser.parse(p.resolve(Bootstrap.EFFECTS_LOCK)).unsafeGet
    val extras = PackageId.mkPackageId("github:flix/extras").get
    assert(lockfile.packages.keySet == Set(extras))
    assert(lockfile.packages(extras).defs.contains("Extras.Graph.closure"))
    assert(lockfile.packages(extras).sigs.isEmpty)

    // Nothing has changed since the signatures were locked, so the check passes, whether every
    // package is checked or only the one.
    bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), None)(System.out).unsafeGet
    bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), Some("flix/extras"))(System.out).unsafeGet

    // A package the project does not declare can be neither locked nor checked, and both
    // commands say so in the same way.
    assert(bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap), Some("flix/museum"))(System.out) == Err(BootstrapError.DependencyNotDeclared(museum)))
    assert(bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), Some("flix/museum"))(System.out) == Err(BootstrapError.DependencyNotDeclared(museum)))

    // Lock `closure` at a signature it does not have, as an upgrade of `extras` might leave it,
    // and the check reports it, and it alone.
    val other = Sha256.parse("sha256:" + "1" * 64).get
    val drifted = EffectLockfile(Map(extras -> LockedPackage(lockfile.packages(extras).defs + ("Extras.Graph.closure" -> other), Map.empty)))
    FileOps.writeString(p.resolve(Bootstrap.EFFECTS_LOCK), EffectLockfile.format(drifted))
    bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap), None)(System.out) match {
      case Err(BootstrapError.SignaturesChangedError(changes)) => assert(changes.map { case (_, sym, _) => EffectLock.nameOf(sym) } == List("Extras.Graph.closure"))
      case Err(e) => fail(s"Expected a changed signature, but got: ${e.message(Formatter.NoFormatter)}")
      case Ok(()) => fail("Expected the changed signature to be reported.")
    }
  }

  /**
    * Returns the root of a program in which each of `packages` is installed with the given
    * files, each file name to its source, and `user` is what the user wrote. The program must
    * compile.
    *
    * Nothing mounts anything: a lock is about what a package declares, not what reaches it.
    */
  private def rootOf(packages: Map[PackageId, Map[String, String]], user: String): TypedAst.Root = {
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
  private def changedNames(result: CheckResult): List[String] = result.changed.map { case (_, sym, _) => EffectLock.nameOf(sym) }

}
