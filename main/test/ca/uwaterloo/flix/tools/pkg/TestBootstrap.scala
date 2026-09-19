package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Version}
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{FileOps, Formatter, Result, Sha256}
import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala

@DoNotDiscover
class TestBootstrap extends AnyFunSuite {

  private val ProjectPrefix: String = "flix-project-"

  /** Returns the path of the package that `build-pkg` builds at `p`. */
  private def packagePath(p: Path): Path =
    p.resolve("artifact").resolve(Bootstrap.PACKAGE_FPKG)

  /** Creates a project that can be packaged. Returns the project directory. */
  private def mkPackageProject(): Path = {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    p
  }

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

  test("outdated.01") {
    // The project declares museum-clerk 1.0.0, and museum-entrance 1.1.0 requires museum-clerk
    // 1.1.0, so museum-clerk is built at 1.1.0. It is compared by the version it is built at.
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
         |"github:flix/museum-clerk" = "1.0.0"
         |"github:flix/museum-entrance" = "1.1.0"
         |""".stripMargin)
    val b = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    val bytes = new ByteArrayOutputStream()
    b.outdated(PkgTestUtils.mkFlix(b))(new PrintStream(bytes)).unsafeGet
    val lines = bytes.toString.linesIterator.map(_.trim.split("\\s+").toList).toList

    // The table says what is declared and what is built.
    assert(lines.exists(_.take(3) == List("package", "declared", "built")))

    // museum-entrance is built at the version that is declared, and has a newer release.
    assert(lines.exists(_.take(3) == List("flix/museum-entrance", "1.1.0", "1.1.0")))

    // museum-clerk is built at 1.1.0. If it is listed at all, which it is once it has a newer
    // release, it is listed as built at 1.1.0, and 1.1.0 is not offered as an update.
    for (clerk <- lines.filter(_.headOption.contains("flix/museum-clerk"))) {
      assert(clerk.take(3) == List("flix/museum-clerk", "1.0.0", "1.1.0"))
      assert(!clerk.drop(3).contains("1.1.0"))
    }
  }

  test("install.01") {
    // A package that is asked for at a version is declared at that version, under a mount
    // derived from its name, and is installed. The dependencies that are already declared are
    // still declared afterwards, with the versions and the mounts they were declared with.
    val p = mkProjectWithDependency()
    val added = PackageId(Repository.GitHub, "jaschdoc", "flix-test-pkg-eff-upgrade")
    install(p, s"jaschdoc/${added.name}@0.1.1").unsafeGet

    val dep = flixDependency(p, added)
    assert(dep.version == SemVer(0, 1, 1))
    assert(dep.mount.contains(Mountpoint("FlixTestPkgEffUpgrade")))

    val clerk = flixDependency(p, ClerkIdentifier)
    assert(clerk.version == SemVer(1, 1, 0))
    assert(clerk.mount.contains(Mountpoint("Clerk")))

    // The package is installed, and the lock file records it.
    assert(Files.exists(libFile(p, added, SemVer(0, 1, 1), Bootstrap.EXT_FPKG)))
    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    assert(lockfile.packages.contains((added, SemVer(0, 1, 1))))

    // The manifest is rewritten as a whole, so the keys it does not model do not survive. The
    // 'name' of a package is one of them, and is dead: nothing reads it.
    assert(!Files.readString(p.resolve(Bootstrap.FLIX_TOML)).contains("name"))
  }

  test("install.02") {
    // A package that is asked for at no version is declared at its newest release.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    install(p, "jaschdoc/flix-test-pkg-eff-upgrade").unsafeGet

    val releases = GitHub.getReleases(GitHub.Project("jaschdoc", "flix-test-pkg-eff-upgrade"), PkgTestUtils.gitHubToken).unsafeGet
    val dep = flixDependency(p, PackageId(Repository.GitHub, "jaschdoc", "flix-test-pkg-eff-upgrade"))
    assert(dep.version == releases.map(r => r.version).max)
  }

  test("install.03") {
    // A package that is already declared is not declared twice.
    val p = mkProjectWithDependency()
    val before = Files.readString(p.resolve(Bootstrap.FLIX_TOML))

    install(p, "flix/museum-clerk@2.1.0") match {
      case Ok(_) => fail("Expected the declared dependency to be refused.")
      case Err(BootstrapError.DependencyAlreadyDeclared(id, version)) =>
        assert(id == ClerkIdentifier)
        assert(version == SemVer(1, 1, 0))
      case Err(e) => fail(s"Expected a declared dependency, but got: ${e.message(Formatter.getDefault)}")
    }

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == before)
  }

  test("install.04") {
    // A mount that another dependency already has is not one to take, and a run that assumes
    // yes has no one to ask for another.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML),
      s"""
         |[package]
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |"github:jaschdoc/flix-test-pkg-eff-upgrade" = { version = "0.1.1", mount = "MuseumClerk" }
         |""".stripMargin)
    val before = Files.readString(p.resolve(Bootstrap.FLIX_TOML))

    install(p, "flix/museum-clerk@1.1.0") match {
      case Ok(_) => fail("Expected the taken mount to be refused.")
      case Err(BootstrapError.NoMount(id)) => assert(id == ClerkIdentifier)
      case Err(e) => fail(s"Expected a taken mount, but got: ${e.message(Formatter.getDefault)}")
    }

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == before)
  }

  test("install.05") {
    // A version that was never released is not one to declare. It is the resolution that says
    // so, since a version that is asked for is taken as it is asked for, so the dependency is
    // written before it is refused, and the manifest that was there is put back.
    val p = mkProjectWithDependency()
    val before = Files.readString(p.resolve(Bootstrap.FLIX_TOML))

    install(p, "flix/museum-entrance@9.9.9") match {
      case Ok(_) => fail("Expected the missing release to be refused.")
      case Err(BootstrapError.FlixPackageError(e: PackageError.VersionDoesNotExist)) =>
        assert(e.version == SemVer(9, 9, 9))
      case Err(e) => fail(s"Expected a missing release, but got: ${e.message(Formatter.getDefault)}")
    }

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == before)
  }


  test("remove.01") {
    // A package that is declared is no longer declared, and the lock file no longer records it.
    // The dependencies that are left are still declared, with their versions and their mounts.
    val p = mkProjectWithDependency()
    val other = PackageId(Repository.GitHub, "jaschdoc", "flix-test-pkg-eff-upgrade")
    install(p, s"jaschdoc/${other.name}@0.1.1").unsafeGet

    remove(p, "flix/museum-clerk").unsafeGet

    val manifest = ManifestParser.parse(p.resolve(Bootstrap.FLIX_TOML)).unsafeGet
    assert(!manifest.flixDependencies.exists(dep => dep.id == ClerkIdentifier))

    val dep = flixDependency(p, other)
    assert(dep.version == SemVer(0, 1, 1))
    assert(dep.mount.contains(Mountpoint("FlixTestPkgEffUpgrade")))

    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    assert(lockfile.packages.keySet == Set((other, SemVer(0, 1, 1))))
  }

  test("remove.02") {
    // The only dependency of a project can be removed, which leaves a manifest that declares
    // none, and that the project still bootstraps from.
    val p = mkProjectWithDependency()
    remove(p, "flix/museum-clerk").unsafeGet

    val toml = Files.readString(p.resolve(Bootstrap.FLIX_TOML))
    assert(!toml.contains("[dependencies]"))
    assert(ManifestParser.parse(toml, Path.of(Bootstrap.FLIX_TOML)).unsafeGet.dependencies.isEmpty)

    Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    assert(LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet.packages.isEmpty)
  }

  test("remove.03") {
    // A package that the project does not declare is not one it can drop.
    val p = mkProjectWithDependency()
    val before = Files.readString(p.resolve(Bootstrap.FLIX_TOML))

    remove(p, "flix/museum-entrance") match {
      case Ok(_) => fail("Expected the undeclared package to be refused.")
      case Err(BootstrapError.DependencyNotDeclared(id)) =>
        assert(id == PackageId(Repository.GitHub, "flix", "museum-entrance"))
      case Err(e) => fail(s"Expected an undeclared dependency, but got: ${e.message(Formatter.getDefault)}")
    }

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == before)
  }

  test("remove.04") {
    // A package is declared at one version, so a version is not part of a removal.
    val p = mkProjectWithDependency()
    val before = Files.readString(p.resolve(Bootstrap.FLIX_TOML))

    remove(p, "flix/museum-clerk@1.1.0") match {
      case Ok(_) => fail("Expected the version to be refused.")
      case Err(BootstrapError.UnexpectedVersion(spec)) => assert(spec == "flix/museum-clerk@1.1.0")
      case Err(e) => fail(s"Expected an unexpected version, but got: ${e.message(Formatter.getDefault)}")
    }

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == before)
  }

  test("remove.05") {
    // What another dependency requires stays: museum-entrance requires museum-clerk, so dropping
    // the declaration of museum-clerk leaves the package itself in the resolution.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML),
      s"""
         |[package]
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |"$ClerkIdentifier" = "1.1.0"
         |"github:flix/museum-entrance" = "1.1.0"
         |""".stripMargin)

    remove(p, "flix/museum-clerk").unsafeGet

    val manifest = ManifestParser.parse(p.resolve(Bootstrap.FLIX_TOML)).unsafeGet
    assert(manifest.flixDependencies.map(dep => dep.id) == List(PackageId(Repository.GitHub, "flix", "museum-entrance")))

    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    assert(lockfile.packages.contains((ClerkIdentifier, SemVer(1, 1, 0))))
  }

  test("upgrade.01") {
    // Only the version changes: what else the declaration says, and where it says it, is what
    // it said before.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML),
      s"""
         |[package]
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |"$ClerkIdentifier" = { version = "1.0.0", mount = "Clerk", security = "paranoid" }
         |"github:jaschdoc/flix-test-pkg-eff-upgrade" = { version = "0.1.1", mount = "Eff" }
         |""".stripMargin)

    upgrade(p, "flix/museum-clerk@1.1.0").unsafeGet

    val dep = flixDependency(p, ClerkIdentifier)
    assert(dep.version == SemVer(1, 1, 0))
    assert(dep.mount.contains(Mountpoint("Clerk")))
    assert(dep.sctx == SecurityContext.Paranoid)

    // The declaration is replaced where it is, and not dropped and added.
    val manifest = ManifestParser.parse(p.resolve(Bootstrap.FLIX_TOML)).unsafeGet
    assert(manifest.flixDependencies.map(d => d.id) == List(ClerkIdentifier, PackageId(Repository.GitHub, "jaschdoc", "flix-test-pkg-eff-upgrade")))

    // The new version is installed, and the lock file records it.
    assert(Files.exists(libFile(p, ClerkIdentifier, SemVer(1, 1, 0), Bootstrap.EXT_FPKG)))
    val lockfile = LockfileParser.parse(p.resolve(Bootstrap.PACKAGES_LOCK)).unsafeGet
    assert(lockfile.packages.contains((ClerkIdentifier, SemVer(1, 1, 0))))
    assert(!lockfile.packages.contains((ClerkIdentifier, SemVer(1, 0, 0))))
  }

  test("upgrade.02") {
    // A package that is asked for at no version is declared at its newest release.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val pkg = PackageId(Repository.GitHub, "jaschdoc", "flix-test-pkg-eff-upgrade")
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML),
      s"""
         |[package]
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |"$pkg" = { version = "0.1.0", mount = "Eff" }
         |""".stripMargin)

    upgrade(p, s"jaschdoc/${pkg.name}").unsafeGet

    val releases = GitHub.getReleases(GitHub.Project(pkg.owner, pkg.name), PkgTestUtils.gitHubToken).unsafeGet
    assert(flixDependency(p, pkg).version == releases.map(r => r.version).max)
  }

  test("upgrade.03") {
    // A version below the one that is declared is taken as it is asked for: a declaration is a
    // version to pin as well as a version to raise.
    val p = mkProjectWithDependency()
    upgrade(p, "flix/museum-clerk@1.0.0").unsafeGet

    val dep = flixDependency(p, ClerkIdentifier)
    assert(dep.version == SemVer(1, 0, 0))
    assert(dep.mount.contains(Mountpoint("Clerk")))
  }

  test("upgrade.04") {
    // A package that already declares the version it would be given is left alone, so a command
    // that changes nothing rewrites nothing, comments included.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val toml =
      s"""
         |[package]
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |# The clerk of the museum.
         |"$ClerkIdentifier" = { version = "1.1.0", mount = "Clerk" }
         |""".stripMargin
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML), toml)

    upgrade(p, "flix/museum-clerk@1.1.0").unsafeGet

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == toml)
  }

  test("upgrade.05") {
    // An upgrade that the project cannot be built with is refused, and the manifest that was
    // there is put back. museum-entrance requires museum-clerk 1.1.0, and a major is a
    // compatibility boundary, so museum-clerk 2.1.0 is not a version both can be given.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    Files.writeString(p.resolve(Bootstrap.FLIX_TOML),
      s"""
         |[package]
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |
         |[dependencies]
         |"$ClerkIdentifier" = "1.0.0"
         |"github:flix/museum-entrance" = "1.1.0"
         |""".stripMargin)
    val before = Files.readString(p.resolve(Bootstrap.FLIX_TOML))

    upgrade(p, "flix/museum-clerk@2.1.0") match {
      case Ok(_) => fail("Expected the incompatible version to be refused.")
      case Err(_) => // Expected.
    }

    assert(Files.readString(p.resolve(Bootstrap.FLIX_TOML)) == before)
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
    val p = mkPackageProject()

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault).unsafeGet

    // The package has a fixed name, and is not named after the directory it is built in: a
    // release asset has to be found again from the repository and the version alone.
    assert(Files.exists(packagePath(p)))
    assert(!packagePath(p).getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-pkg generates ZIP entries with fixed time") {
    val p = mkPackageProject()

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault).unsafeGet

    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(packagePath(p).toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted == "2014-06-27 00:00:00")
    }
  }

  test("build-pkg always generates package that is byte-for-byte exactly the same") {
    val p = mkPackageProject()

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix(b)
    b.build(flix)

    b.buildPkg(flix)(Formatter.getDefault).unsafeGet

    val hash1 = Sha256.ofFile(packagePath(p))

    b.buildPkg(flix)(Formatter.getDefault).unsafeGet

    val hash2 = Sha256.ofFile(packagePath(p))

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg refuses a project that does not check") {
    val p = mkPackageProject()
    // A public module in a file whose path does not match its name.
    Files.writeString(p.resolve("src").resolve("Bar.flix"), "pub mod Foo { pub def f(): Int32 = 1 }")

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val result = b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault)
    assert(result.toOption.isEmpty)

    assert(!Files.exists(packagePath(p)))
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
    * Installs `spec` into the project at `p`, without asking anything of whoever runs the tests.
    */
  private def install(p: Path, spec: String): Result[Unit, BootstrapError] =
    Bootstrap.install(p, spec, PkgTestUtils.gitHubToken, assumeYes = true)(Formatter.getDefault, System.out)

  /**
    * Removes `spec` from the project at `p`.
    */
  private def remove(p: Path, spec: String): Result[Unit, BootstrapError] =
    Bootstrap.remove(p, spec, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out)

  /**
    * Declares `spec` at another version in the project at `p`.
    */
  private def upgrade(p: Path, spec: String): Result[Unit, BootstrapError] =
    Bootstrap.upgrade(p, spec, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out)

  /**
    * Returns the dependency on `id` that the manifest of the project at `p` declares.
    */
  private def flixDependency(p: Path, id: PackageId): Dependency.FlixDependency = {
    val manifest = ManifestParser.parse(p.resolve(Bootstrap.FLIX_TOML)).unsafeGet
    manifest.flixDependencies.find(dep => dep.id == id) match {
      case Some(dep) => dep
      case None => fail(s"Expected '$id' to be a dependency of the project.")
    }
  }

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
    libFile(p, ClerkIdentifier, SemVer(1, 1, 0), ext)

  /**
    * Returns the path that `id` is installed at in the project at `p`, at `version` and with the
    * given extension.
    */
  private def libFile(p: Path, id: PackageId, version: SemVer, ext: String): Path =
    Bootstrap.getLibraryDirectory(p)
      .resolve("github").resolve(id.owner).resolve(id.name).resolve(version.toString)
      .resolve(s"${id.name}-$version.$ext")

}
