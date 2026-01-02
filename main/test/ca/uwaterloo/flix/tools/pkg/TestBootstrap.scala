package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.util.{FileOps, Formatter, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import java.security.{DigestInputStream, MessageDigest}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.{CollectionHasAsScala, EnumerationHasAsScala}
import scala.util.Using

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
    b.check(PkgTestUtils.mkFlix)
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(PkgTestUtils.mkFlix)
  }

  test("build-jar") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    b.build(flix)
    b.buildJar(flix)(Formatter.getDefault)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    assert(Files.exists(jarPath))
    assert(jarPath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-jar generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    b.build(flix)
    b.buildJar(flix)(Formatter.getDefault)

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

    val flix1 = PkgTestUtils.mkFlix
    // Use 1 thread for deterministic symbols
    flix1.setOptions(flix1.options.copy(threads = 1))

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildJar(flix1)(Formatter.getDefault)
    val hash1 = calcHash(jarPath)

    // Use new flix instance to reset symbol generation
    val flix2 = PkgTestUtils.mkFlix
    // Use 1 thread for deterministic symbols
    flix2.setOptions(flix2.options.copy(threads = 1))
    b.buildJar(flix2)(Formatter.getDefault)
    val hash2 = calcHash(jarPath)

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg()(Formatter.getDefault)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    assert(Files.exists(packagePath))
    assert(packagePath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-pkg generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg()(Formatter.getDefault)

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

    val flix = PkgTestUtils.mkFlix

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(flix)

    b.buildPkg()(Formatter.getDefault)

    val hash1 = calcHash(packagePath)

    b.buildPkg()(Formatter.getDefault)

    val hash2 = calcHash(packagePath)

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.run(PkgTestUtils.mkFlix, Array("arg0", "arg1"))
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.test(PkgTestUtils.mkFlix)
  }

  test("clean-command-should-remove-class-files-and-directories-if-compiled-previously") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(PkgTestUtils.mkFlix)
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
    b.build(PkgTestUtils.mkFlix)
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
    val flix = PkgTestUtils.mkFlix
    bootstrap.lockEffects(flix).unsafeGet

    // Assert that effects.lock exists now
    if (Files.exists(effectLockFile)) {
      succeed
    } else {
      fail("File 'effects.lock' does not exist")
    }
  }

  test("eff-check on same version as before is ok") {
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
    bootstrap.lockEffects(PkgTestUtils.mkFlix).unsafeGet

    assert(bootstrap.checkEffects(PkgTestUtils.mkFlix) == Result.Ok(()))
  }

  test("eff-check on effect unsafe upgrade reports error") {
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
    bootstrap.lockEffects(PkgTestUtils.mkFlix).unsafeGet

    // Perform upgrade by overriding manifest
    val tomlUpgr = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-eff-upgrade" = "0.1.1"
        |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), tomlUpgr)
    if (Files.exists(p.resolve("lib/jaschdoc/flix-test-pkg-eff-upgrade/"))) {
      println("flix prefix is valid")
    }
    // FileOps.delete(p.resolve("lib/jaschdoc/flix-test-pkg-eff-upgrade/")).unsafeGet

    //val bootstrapUpgr = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    fail("wip")


    assert(bootstrap.checkEffects(PkgTestUtils.mkFlix) == Result.Ok(()))
  }

  private def calcHash(p: Path): String = {
    val sha = MessageDigest.getInstance("SHA-256")
    Using(new DigestInputStream(Files.newInputStream(p), sha)) { input =>
      input.readNBytes(8192)
      sha.digest.map("%02x".format(_)).mkString
    }.get
  }

}
