package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Version}
import ca.uwaterloo.flix.language.ast.shared.{PackageId, Repository}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{FileOps, Formatter, Result, Sha256}
import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala

@DoNotDiscover
class TestBootstrap extends AnyFunSuite {

  private val ProjectPrefix: String = "flix-project-"

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
  }

  test("check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.check(PkgTestUtils.mkFlix(b))
  }

  test("packages.lock.01") {
    val p = mkProjectWithDependency()
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    val entry = lockfile.packages((ClerkIdentifier, SemVer(1, 1, 0)))

    assert(entry.toml == Sha256.ofFile(clerkFile(p, Bootstrap.EXT_TOML)))
    assert(entry.fpkg.contains(Sha256.ofFile(clerkFile(p, Bootstrap.EXT_FPKG))))
  }

  test("packages.lock.02") {
    // The second bootstrap finds every dependency cached, and must digest the cached files to
    // arrive at the same lock file rather than leaving the entries out.
    val p = mkProjectWithDependency()
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    val first = Files.readString(p.resolve(Bootstrap.PACKAGES_LOCK))

    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    val second = Files.readString(p.resolve(Bootstrap.PACKAGES_LOCK))

    assert(first == second)
  }

  test("packages.lock.03") {
    // A project with no Flix dependencies still locks, and locks nothing.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    assert(lockfile.packages.isEmpty)
  }

  test("packages.lock.04") {
    // A cached file that no longer matches the lock file is refused.
    val p = mkProjectWithDependency()
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    Files.writeString(clerkFile(p, Bootstrap.EXT_FPKG), "not the package you are looking for")

    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out) match {
      case Ok(_) => fail("Expected the tampered package to be refused.")
      case Err(BootstrapError.FlixPackageError(e: PackageError.MismatchedCachedDigest)) =>
        assert(e.identifier == ClerkIdentifier)
        assert(e.extension == Bootstrap.EXT_FPKG)
      case Err(e) => fail(s"Expected a mismatched digest, but got: ${e.message(Formatter.getDefault)}")
    }
  }

  test("packages.lock.05") {
    // A tampered file leaves the lock file alone, so the digest it recorded is not overwritten
    // by the digest of whatever is there now.
    val p = mkProjectWithDependency()
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    val before = Files.readString(p.resolve(Bootstrap.PACKAGES_LOCK))

    Files.writeString(clerkFile(p, Bootstrap.EXT_FPKG), "not the package you are looking for")
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out)

    assert(Files.readString(p.resolve(Bootstrap.PACKAGES_LOCK)) == before)
  }

  test("packages.lock.06") {
    // An entry for a package the project does not depend on is dropped, not reported.
    val p = mkProjectWithDependency()
    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    val stale = Lockfile(LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet.packages
      + ((PackageId(Repository.GitHub, "flix", "gone"), SemVer(9, 9, 9)) -> LockEntry(Sha256("a" * 64), Some(Sha256("b" * 64)))))
    Files.writeString(p.resolve(Bootstrap.PACKAGES_LOCK), Lockfile.format(stale))

    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    assert(lockfile.packages.keySet == Set((ClerkIdentifier, SemVer(1, 1, 0))))
  }

  test("packages.lock.07") {
    // A lock file that is not a lock file is reported rather than ignored.
    val p = mkProjectWithDependency()
    Files.writeString(p.resolve(Bootstrap.PACKAGES_LOCK), "[lock]\nversion = 99\n")

    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out) match {
      case Ok(_) => fail("Expected the unreadable lock file to be refused.")
      case Err(BootstrapError.LockParseError(_: LockError.UnsupportedLockVersion)) => ()
      case Err(e) => fail(s"Expected an unsupported lock version, but got: ${e.message(Formatter.getDefault)}")
    }
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(PkgTestUtils.mkFlix(b))

    // The build command does not write anything to disk.
    val buildDir = p.resolve("./build/").normalize()
    assert(!Files.exists(buildDir))
  }

  test("build-classes") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildClasses(PkgTestUtils.mkFlix(b))

    val classDir = p.resolve("./build/class/").normalize()
    val classFiles = FileOps.getFilesIn(classDir, Int.MaxValue)
    assert(classFiles.nonEmpty)
    assert(classFiles.forall(FileOps.isClassFile))
    assert(Files.exists(classDir.resolve("Main.class")))
  }

  test("build-jar") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix(b)
    b.build(flix)
    b.buildJar(flix)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    assert(Files.exists(jarPath))
    assert(jarPath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-jar generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix(b)
    b.build(flix)
    b.buildJar(flix)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(jarPath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted == "2014-06-27 00:00:00")
    }
  }

  test("build-jar always generates package that is byte-for-byte exactly the same modulo concurrency") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")

    val b1 = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix1 = PkgTestUtils.mkFlix(b1)
    // Use 1 thread for deterministic symbols
    flix1.setOptions(flix1.options.copy(threads = 1))
    b1.buildJar(flix1)
    val hash1 = Sha256.ofFile(jarPath)

    val b2 = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix2 = PkgTestUtils.mkFlix(b2)
    // Use 1 thread for deterministic symbols
    flix2.setOptions(flix2.options.copy(threads = 1))
    b2.buildJar(flix2)
    val hash2 = Sha256.ofFile(jarPath)

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    assert(Files.exists(packagePath))
    assert(packagePath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-pkg generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(packagePath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted == "2014-06-27 00:00:00")
    }
  }

  test("build-pkg always generates package that is byte-for-byte exactly the same") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix(b)
    b.build(flix)

    b.buildPkg(flix)(Formatter.getDefault)

    val hash1 = Sha256.ofFile(packagePath)

    b.buildPkg(flix)(Formatter.getDefault)

    val hash2 = Sha256.ofFile(packagePath)

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg refuses a project that does not check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    // A public module in a file whose path does not match its name.
    Files.writeString(p.resolve("src").resolve("Bar.flix"), "pub mod Foo { pub def f(): Int32 = 1 }")

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val result = b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault)
    assert(result.toOption.isEmpty)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    assert(!Files.exists(packagePath))
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.run(PkgTestUtils.mkFlix(b), Array("arg0", "arg1"))
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.test(PkgTestUtils.mkFlix(b))
  }

  test("clean-command-should-remove-class-files-and-directories-if-compiled-previously") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildClasses(PkgTestUtils.mkFlix(b))
    val buildDir = p.resolve("./build/").normalize()
    val buildFiles = FileOps.getFilesIn(buildDir, Int.MaxValue)
    if (buildFiles.isEmpty || buildFiles.exists(!FileOps.checkExt(_, "class"))) {
      fail(
        s"""build output is not as expected:
           |${buildFiles.mkString(System.lineSeparator())}
           |""".stripMargin)
    }
    b.clean()
    val newBuildFiles = FileOps.getFilesIn(buildDir, Int.MaxValue)
    if (newBuildFiles.nonEmpty || Files.exists(buildDir)) {
      fail(
        s"""at least one file was not cleaned from build dir:
           |${newBuildFiles.mkString(System.lineSeparator())}
           |""".stripMargin)
    }
  }

  test("clean-should-error-on-unexpected-file") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildClasses(PkgTestUtils.mkFlix(b))
    val buildDir = p.resolve("./build/").normalize()
    FileOps.writeString(buildDir.resolve("./other.txt").normalize(), "hello")
    b.clean() match {
      case Result.Ok(_) => fail("expected clean to abort")
      case Result.Err(_) => succeed
    }
  }

  test("clean-should-succeed-on-non-existent-build-dir") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val buildDir = p.resolve("./build/").normalize()
    if (Files.exists(buildDir)) {
      fail("did not expected build directory to exist")
    }
    b.clean() match {
      case Result.Ok(_) => succeed
      case Result.Err(_) => fail("expected success")
    }
  }

  test("clean-should-do-nothing-in-directory-mode") {
    val p = Files.createTempDirectory(ProjectPrefix)
    FileOps.writeString(p.resolve("./Main.flix").normalize(),
      """
        |def main(): Unit = ()
        |""".stripMargin)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val buildDir = p.resolve("./build/").normalize()
    if (Files.exists(buildDir)) {
      fail("did not expected build directory to exist")
    }
    b.clean() match {
      case Result.Ok(_) => fail("expected failure in directory mode")
      case Result.Err(_) => succeed
    }
  }

  test("eff-lock should write effect lock file") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "unrestricted" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "unrestricted" }
        |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    TestPkgTrustTransitive.entry()
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    // Assert effects.lock does not exist
    val effectLockFile = p.resolve("effects.lock").normalize()
    if (Files.exists(effectLockFile)) {
      fail("Unexpected 'effects.lock' file. File is not supposed to exist")
    }

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix(bootstrap)
    bootstrap.lockEffects(flix).unsafeGet

    // Assert that effects.lock exists now
    if (Files.exists(effectLockFile)) {
      succeed
    } else {
      fail("File 'effects.lock' does not exist")
    }
  }

  test("eff-check on same version as before is ok") {
    // Version 0.1.0 of the dependency has signature `Int32 -> Int32`.
    // There is no upgrade done, but we assert that
    // performing eff-check after eff-lock succeeds.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-eff-upgrade" = "0.1.0"
        |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    println(Upgr.entrypoint(42))
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap)).unsafeGet

    assert(bootstrap.checkEffects(PkgTestUtils.mkFlix(bootstrap)) == Result.Ok(()))
  }

  test("eff-check on effect unsafe upgrade reports error") {
    // Version 0.1.0 of the dependency has signature `Int32 -> Int32`.
    // Version 0.1.1 of the dependency has signature `Int32 -> Int32 \ IO`.
    // We upgrade from `Int32 -> Int32` to `Int32 -> Int32 \ IO`
    // and assert that it does NOT succeed.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    val pkgAuthor = "jaschdoc"
    val pkgName = "flix-test-pkg-eff-upgrade"
    val vOld = "0.1.0"
    val vNew = "0.1.1"

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vOld"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    println(Upgr.entrypoint(42))
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap)).unsafeGet

    // Perform upgrade by overriding manifest
    val tomlUpgr = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vNew"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), tomlUpgr)
    // Delete old files
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vOld/$pkgName-$vOld.toml")).unsafeGet
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vOld/$pkgName-$vOld.fpkg")).unsafeGet

    val bootstrapUpgr = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    bootstrapUpgr.checkEffects(PkgTestUtils.mkFlix(bootstrapUpgr)) match {
      case Result.Err(BootstrapError.EffectUpgradeError(_)) => succeed
      case Result.Err(e) => fail(e.message(Formatter.getDefault))
      case Result.Ok(()) => fail("expected effect upgrade error")
    }
  }

  test("eff-check on effect downgrade is ok") {
    // Version 0.1.0 of the dependency has signature `Int32 -> Int32`.
    // Version 0.1.1 of the dependency has signature `Int32 -> Int32 \ IO`.
    // We downgrade from `Int32 -> Int32 \ IO` to `Int32 -> Int32`
    // and assert that it succeeds.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    val pkgAuthor = "jaschdoc"
    val pkgName = "flix-test-pkg-eff-upgrade"
    val vSafe = "0.1.0"
    val vUnsafe = "0.1.1"

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vUnsafe"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    println(Upgr.entrypoint(42))
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix(bootstrap)).unsafeGet

    // Perform upgrade by overriding manifest
    val tomlUpgr = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vSafe"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), tomlUpgr)
    // Delete old files
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vUnsafe/$pkgName-$vUnsafe.toml")).unsafeGet
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vUnsafe/$pkgName-$vUnsafe.fpkg")).unsafeGet

    val bootstrapUpgr = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    assert(bootstrapUpgr.checkEffects(PkgTestUtils.mkFlix(bootstrapUpgr)) == Result.Ok(()))
  }

  test("flix-version.current") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    // N.B.: `init` writes the current version of Flix to `flix.toml`.
    Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out) match {
      case Result.Ok(_) => // Expected.
      case Result.Err(e) => fail(s"Expected success, but got: ${e.message(Formatter.NoFormatter)}")
    }
  }

  test("flix-version.older") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    FileOps.writeString(p.resolve("flix.toml").normalize(), mkTomlWithFlixVersion("0.1.0"))
    Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out) match {
      case Result.Ok(_) => // Expected: an older required version is fine.
      case Result.Err(e) => fail(s"Expected success, but got: ${e.message(Formatter.NoFormatter)}")
    }
  }

  test("flix-version.newer") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    FileOps.writeString(p.resolve("flix.toml").normalize(), mkTomlWithFlixVersion("999.0.0"))
    Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out) match {
      case Result.Ok(_) => fail("Expected BootstrapError.FlixVersionTooOld, but bootstrap succeeded.")
      case Result.Err(e: BootstrapError.FlixVersionTooOld) =>
        assert(e.required == SemVer(999, 0, 0))
        assert(e.current == SemVer.ofVersion(Version.CurrentVersion))
      case Result.Err(e) => fail(s"Expected BootstrapError.FlixVersionTooOld, but got: ${e.message(Formatter.NoFormatter)}")
    }
  }

  test("flix-version.examples") {
    val current = SemVer.ofVersion(Version.CurrentVersion)
    val manifests = FileOps.getFilesIn(Path.of("examples"), Int.MaxValue).filter(_.getFileName.toString == "flix.toml")
    assert(manifests.nonEmpty, "Expected to find at least one 'flix.toml' under 'examples'.")
    for (manifest <- manifests) {
      val required = ManifestParser.parse(manifest).unsafeGet.flix
      assert(required <= current, s"'$manifest' requires Flix $required, but the current version is $current.")
    }
  }

  /**
    * Returns a `flix.toml` without dependencies that requires the given version `v` of Flix.
    */
  private def mkTomlWithFlixVersion(v: String): String = {
    s"""
       |[package]
       |name = "test"
       |description = "test"
       |version = "0.1.0"
       |flix = "$v"
       |authors = ["flix"]
       |""".stripMargin
  }

  /**
    * The identifier of the package the lock file tests depend on.
    */
  private val ClerkIdentifier: PackageId = PackageId(Repository.GitHub, "flix", "museum-clerk")

  /**
    * Returns a new project directory whose manifest declares a single Flix dependency.
    */
  private def mkProjectWithDependency(): Path = {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML),
      s"""
         |[package]
         |name = "test"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |"$ClerkIdentifier" = { version = "1.1.0", mount = "Clerk" }
         |""".stripMargin)
    p
  }

  /**
    * Returns the path that the dependency of [[mkProjectWithDependency]] is installed at in the
    * project at `p`, with the given extension.
    */
  private def clerkFile(p: Path, ext: String): Path =
    Bootstrap.getLibraryDirectory(p)
      .resolve("github").resolve("flix").resolve("museum-clerk").resolve("1.1.0")
      .resolve(s"museum-clerk-1.1.0.$ext")

}
