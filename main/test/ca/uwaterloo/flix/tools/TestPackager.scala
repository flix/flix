package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

import java.nio.file.{Files, Path}
import java.security.{DigestInputStream, MessageDigest}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Using

class TestPackager extends FunSuite {

  private val ProjectPrefix: String = "flix-project-"

  private val DefaultOptions: Options = Options.Default

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
  }

  test("check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.check(p, DefaultOptions)
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.build(p, DefaultOptions)
  }

  test("build-jar") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.build(p, DefaultOptions)
    Packager.buildJar(p, DefaultOptions)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve(packageName + ".jar")
    assert(Files.exists(jarPath))
    assert(jarPath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-jar generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.buildJar(p, DefaultOptions)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve(packageName + ".jar")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(packagePath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted.equals("2014-06-27 00:00:00"))
    }
  }

  test("build-jar always generates package that is byte-for-byte exactly the same") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    val packageName = p.getFileName.toString
    val packagePath = p.resolve(packageName + ".jar")

    Packager.buildJar(p, DefaultOptions)
    def hash1 = calcHash(packagePath)

    Files.delete(packagePath)
    Packager.buildJar(p, DefaultOptions)
    def hash2 = calcHash(packagePath)

    assert(
      hash1.equals(hash2),
      s"Two file hashes are not same: ${hash1} and ${hash2}")
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.buildPkg(p, DefaultOptions)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve(packageName + ".fpkg")
    assert(Files.exists(packagePath))
    assert(packagePath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-pkg generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.buildPkg(p, DefaultOptions)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve(packageName + ".fpkg")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(packagePath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted.equals("2014-06-27 00:00:00"))
    }
  }

  test("build-pkg always generates package that is byte-for-byte exactly the same") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    val packageName = p.getFileName.toString
    val packagePath = p.resolve(packageName + ".fpkg")

    Packager.buildPkg(p, DefaultOptions)
    def hash1 = calcHash(packagePath)

    Files.delete(packagePath)
    Packager.buildPkg(p, DefaultOptions)
    def hash2 = calcHash(packagePath)

    assert(
      hash1.equals(hash2),
      s"Two file hashes are not same: ${hash1} and ${hash2}")
  }

  test("benchmark") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.benchmark(p, DefaultOptions)
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.run(p, DefaultOptions)
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.test(p, DefaultOptions)
  }

  def calcHash(p: Path): String = {
    val buffer = new Array[Byte](8192)
    val sha = MessageDigest.getInstance("SHA-256")
    Using(new DigestInputStream(Files.newInputStream(p), sha)) { input =>
      while (input.read(buffer) != -1) { }
      sha.digest.map("%02x".format(_)).mkString
    }.get
  }

}
