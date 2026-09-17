package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, InstalledPackage}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.{FileOps, Formatter}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class TestMounts extends AnyFunSuite {

  private val Id: String = "github:test/dep"

  /** Builds a package that declares `pub mod Board` and a non-public `mod Secret`. */
  private def mkPkg(): Path = {
    val p = Files.createTempDirectory("flix-mount-dep-")
    Bootstrap.init(p)(System.out)
    Files.delete(p.resolve("src").resolve("Main.flix"))
    Files.delete(p.resolve("test").resolve("TestMain.flix"))
    FileOps.writeString(p.resolve("src").resolve("Board.flix"), "pub mod Board { pub def place(): Int32 = 42 }")
    FileOps.writeString(p.resolve("src").resolve("Secret.flix"), "mod Secret { pub def hidden(): Int32 = 1 }")
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault).unsafeGet
    p.resolve("artifact").resolve(p.getFileName.toString + ".fpkg")
  }

  private def check(pkgPath: Path, mounts: Map[String, String], main: String): List[String] = {
    val pkg = InstalledPackage(pkgPath, Id, SecurityContext.Unrestricted, Map.empty)
    val flix = PkgTestUtils.mkFlix(List(pkg), mounts)
    flix.addSource(Path.of("Main.flix"), main, SecurityContext.Unrestricted)
    val (_, errors) = flix.check()
    errors.map(_.getClass.getSimpleName)
  }

  test("mounted.reachable") {
    val pkg = mkPkg()
    val mounts = Map("Game" -> Id)
    assertResult(Nil)(check(pkg, mounts, "def main(): Unit \\ IO = println(Game.Board.place())"))
  }

  test("mounted.not-reachable-unqualified") {
    val pkg = mkPkg()
    val mounts = Map("Game" -> Id)
    val errors = check(pkg, mounts, "def main(): Unit \\ IO = println(Board.place())")
    assert(errors.exists(_.contains("Undefined")), errors)
  }

  test("flat.reachable") {
    val pkg = mkPkg()
    val mounts = Map.empty[String, String]
    assertResult(Nil)(check(pkg, mounts, "def main(): Unit \\ IO = println(Board.place())"))
  }

  test("flat.private-reachable") {
    // A package nothing mounts keeps sharing the root namespace, so its non-public modules are
    // reachable exactly as they were before mounts existed.
    val pkg = mkPkg()
    val mounts = Map.empty[String, String]
    assertResult(Nil)(check(pkg, mounts, "def main(): Unit \\ IO = println(Secret.hidden())"))
  }

  test("mounted.private-not-reachable") {
    val pkg = mkPkg()
    val mounts = Map("Game" -> Id)
    val errors = check(pkg, mounts, "def main(): Unit \\ IO = println(Game.Secret.hidden())")
    assert(errors.nonEmpty, "expected the non-public module of a mounted package to be inaccessible")
  }

}
