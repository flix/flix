package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{Bootstrap, Flix}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import java.security.{DigestInputStream, MessageDigest}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Using

class TestBootstrap extends AnyFunSuite {

  private val ProjectPrefix: String = "flix-project-"

  private val DefaultOptions: Options = Options.Default

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
  }

  test("check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.check(DefaultOptions)
  }

  test("build") {
    implicit val flix: Flix = new Flix()
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.build()
  }

  test("build-jar") {
    implicit val flix: Flix = new Flix()
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.build()
    b.buildJar(DefaultOptions)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    assert(Files.exists(jarPath))
    assert(jarPath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-jar generates ZIP entries with fixed time") {
    implicit val flix: Flix = new Flix()
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.build()
    b.buildJar(DefaultOptions)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(jarPath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted.equals("2014-06-27 00:00:00"))
    }
  }

  test("build-jar always generates package that is byte-for-byte exactly the same") {
    implicit val flix: Flix = new Flix()
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")

    val b = Bootstrap.bootstrap(p)(System.out).get
    b.build()
    b.buildJar(DefaultOptions)

    def hash1 = calcHash(jarPath)

    b.build()
    b.buildJar(DefaultOptions)

    def hash2 = calcHash(jarPath)

    assert(
      hash1.equals(hash2),
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)

    val b = Bootstrap.bootstrap(p)(System.out).get
    b.buildPkg(DefaultOptions)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    assert(Files.exists(packagePath))
    assert(packagePath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-pkg generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)

    val b = Bootstrap.bootstrap(p)(System.out).get
    b.buildPkg(DefaultOptions)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(packagePath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted.equals("2014-06-27 00:00:00"))
    }
  }

  test("build-pkg always generates package that is byte-for-byte exactly the same") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")

    val b = Bootstrap.bootstrap(p)(System.out).get
    b.buildPkg(DefaultOptions)

    def hash1 = calcHash(packagePath)

    b.buildPkg(DefaultOptions)

    def hash2 = calcHash(packagePath)

    assert(
      hash1.equals(hash2),
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("benchmark") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.benchmark(DefaultOptions)
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.run(DefaultOptions)
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p, DefaultOptions)(System.out)
    val b = Bootstrap.bootstrap(p)(System.out).get
    b.test(DefaultOptions)
  }

  def calcHash(p: Path): String = {
    val buffer = new Array[Byte](8192)
    val sha = MessageDigest.getInstance("SHA-256")
    Using(new DigestInputStream(Files.newInputStream(p), sha)) { input =>
      while (input.read(buffer) != -1) {}
      sha.digest.map("%02x".format(_)).mkString
    }.get
  }

}
