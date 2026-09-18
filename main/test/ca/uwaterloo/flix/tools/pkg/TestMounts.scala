package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, InstalledPackage}
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.{FileOps, Formatter}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class TestMounts extends AnyFunSuite {

  private val Id: PackageId = PackageId(Repository.GitHub, "test", "dep")

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

  private def check(pkgPath: Path, mounts: Map[Mountpoint, PackageId], main: String): List[String] = {
    val pkg = InstalledPackage(pkgPath, Id, SecurityContext.Unrestricted, Map.empty)
    val flix = PkgTestUtils.mkFlix(List(pkg), mounts)
    flix.addSource(Path.of("Main.flix"), main, SecurityContext.Unrestricted)
    val (root, errors) = flix.check()
    messages = CompilationMessage.formatAll(errors)(Formatter.NoFormatter, root)
    errors.map(_.getClass.getSimpleName)
  }

  /** The rendered messages of the errors the last [[check]] reported. */
  private var messages: String = ""

  test("mounted.reachable") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Game") -> Id)
    assertResult(Nil)(check(pkg, mounts, "def main(): Unit \\ IO = println(Game.Board.place())"))
  }

  test("mounted.not-reachable-unqualified") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Game") -> Id)
    val errors = check(pkg, mounts, "def main(): Unit \\ IO = println(Board.place())")
    assert(errors.exists(_.contains("Undefined")), errors)
  }

  test("collision.library") {
    // A mount that shadows a library module is rejected: 'List.map' inside the mounting code
    // would resolve into the dependency.
    val pkg = mkPkg()
    val errors = check(pkg, Map(Mountpoint("List") -> Id), "def main(): Unit \\ IO = println(1)")
    assert(errors.contains("MountShadowsLibrary"), errors)
  }

  test("collision.own-declaration") {
    // A mount that shadows a declaration of the mounting code is rejected: the name would mean
    // the declaration at the top level and the dependency inside a nested module.
    val pkg = mkPkg()
    val main =
      """
        |pub mod Game { pub def size(): Int32 = 1 }
        |def main(): Unit \\ IO = println(Game.size())
        |""".stripMargin
    val errors = check(pkg, Map(Mountpoint("Game") -> Id), main)
    assert(errors.contains("MountShadowsDeclaration"), errors)
  }

  test("collision.other-package-allowed") {
    // A mount that shadows a module of another package is allowed: the author asked for the name.
    val pkg = mkPkg()
    assertResult(Nil)(check(pkg, Map(Mountpoint("Game") -> Id), "def main(): Unit \\ IO = println(Game.Board.place())"))
  }

  test("mounted.private-not-reachable") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Game") -> Id)
    val errors = check(pkg, mounts, "def main(): Unit \\ IO = println(Game.Secret.hidden())")
    assert(errors.nonEmpty, "expected the non-public module of a mounted package to be inaccessible")
    // The root a package is named under cannot be written in source, so it is shown as the
    // identifier of the package rather than as the name the compiler gives it.
    assert(messages.contains(s"${Id}.Secret"), messages)
    assert(!messages.contains("$pkg$"), messages)
  }

}
