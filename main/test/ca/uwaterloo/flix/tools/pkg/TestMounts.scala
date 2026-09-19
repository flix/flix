package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.{Bootstrap, InstalledPackage}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.{FileOps, Formatter}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class TestMounts extends AnyFunSuite with TestUtils {

  private val Id: PackageId = PackageId(Repository.GitHub, "test", "dep")

  /** Builds a package that declares `pub mod Board`, `pub mod Game`, `pub mod Game.Rules` and a non-public `mod Secret`. */
  private def mkPkg(): Path = {
    val p = Files.createTempDirectory("flix-mount-dep-")
    Bootstrap.init(p)(System.out)
    Files.delete(p.resolve("src").resolve("Main.flix"))
    Files.delete(p.resolve("test").resolve("TestMain.flix"))
    FileOps.writeString(p.resolve("src").resolve("Board.flix"), "pub mod Board { pub def place(): Int32 = 42 }")
    FileOps.writeString(p.resolve("src").resolve("Secret.flix"), "mod Secret { pub def hidden(): Int32 = 1 }")
    FileOps.writeString(p.resolve("src").resolve("Game.flix"), "pub mod Game { pub def name(): String = \"flixball\" }")
    Files.createDirectories(p.resolve("src").resolve("Game"))
    FileOps.writeString(p.resolve("src").resolve("Game").resolve("Rules.flix"), "pub mod Game.Rules { pub def players(): Int32 = 2 }")
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg(PkgTestUtils.mkFlix(b))(Formatter.getDefault).unsafeGet
    p.resolve("artifact").resolve(p.getFileName.toString + ".fpkg")
  }

  /** Checks `main` against the package at `pkgPath`, installed under `mounts`. */
  private def check(pkgPath: Path, mounts: Map[Mountpoint, PackageId], main: String): (Option[TypedAst.Root], List[CompilationMessage]) = {
    val pkg = InstalledPackage(pkgPath, Id, SecurityContext.Unrestricted, Map.empty)
    val flix = PkgTestUtils.mkFlix(List(pkg), mounts)
    flix.addSource(Path.of("Main.flix"), main, SecurityContext.Unrestricted)
    flix.check()
  }

  test("mounted.not-reachable-bare") {
    // A mount is not a module: it is only read before `::` in a use.
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val result = check(pkg, mounts, "def main(): Unit \\ IO = println(Flixball.Board.place())")
    expectError[ResolutionError.UndefinedName](result)
  }

  test("mounted.not-reachable-bare-use") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball.Board
        |def main(): Unit \ IO = println(Board.place())
        |""".stripMargin
    expectError[ResolutionError.UndefinedUse](check(pkg, mounts, main))
  }

  test("mounted.not-reachable-unqualified") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Game") -> Id)
    val result = check(pkg, mounts, "def main(): Unit \\ IO = println(Board.place())")
    expectError[ResolutionError.UndefinedName](result)
  }

  test("unmounted.not-reachable") {
    // A package is named under its own root, so one that the project does not mount is not
    // reachable, qualified or not. A package that is a dependency of a dependency is like that.
    val pkg = mkPkg()
    val mounts = Map.empty[Mountpoint, PackageId]
    val result = check(pkg, mounts, "def main(): Unit \\ IO = println(Board.place())")
    expectError[ResolutionError.UndefinedName](result)
  }

  test("mount.named-like-library-module") {
    // A mount is only read before `::`, so it shadows nothing: `List` is still the library's.
    val pkg = mkPkg()
    val main =
      """
        |use List::Board
        |def main(): Unit \ IO = println(Board.place() + List.length(1 :: Nil))
        |""".stripMargin
    expectSuccess(check(pkg, Map(Mountpoint("List") -> Id), main))
  }

  test("mount.named-like-own-module") {
    // Nor does it shadow a module of the mounting code: `Game.size` is the project's own.
    val pkg = mkPkg()
    val main =
      """
        |use Game::Board
        |mod Game { pub def size(): Int32 = 1 }
        |def main(): Unit \ IO = println(Board.place() + Game.size())
        |""".stripMargin
    expectSuccess(check(pkg, Map(Mountpoint("Game") -> Id), main))
  }

  test("package-use.module") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Board
        |def main(): Unit \ IO = println(Board.place())
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.lowercase-mount") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("flixball") -> Id)
    val main =
      """
        |use flixball::Board
        |def main(): Unit \ IO = println(Board.place())
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.nested-module") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Game.Rules
        |def main(): Unit \ IO = println(Rules.players())
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.def") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Game.Rules.players
        |def main(): Unit \ IO = println(players())
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.many") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::{Game, Board}
        |def main(): Unit \ IO = println(Board.place() + Game.Rules.players())
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.many-after-path") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Game.Rules.{players => count}
        |def main(): Unit \ IO = println(count())
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.expression") {
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |def main(): Unit \ IO = {
        |    use Flixball::Board.place;
        |    println(place())
        |}
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.not-shadowed") {
    // A package-qualified use is looked up in the package, not through what is in scope: the
    // project's own 'Board.place' is a String, so this only type checks against the package's.
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Board
        |mod Board { pub def place(): String = "own" }
        |def main(): Unit \ IO = println(Board.place() + 1)
        |""".stripMargin
    expectSuccess(check(pkg, mounts, main))
  }

  test("package-use.undefined-package") {
    // A mount absent from the table.
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flyball::Board
        |def main(): Unit \ IO = println(1)
        |""".stripMargin
    expectError[ResolutionError.UndefinedPackage](check(pkg, mounts, main))
  }

  test("package-use.undefined-use") {
    // A good mount with a bad path.
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Game.Rulez
        |def main(): Unit \ IO = println(1)
        |""".stripMargin
    expectError[ResolutionError.UndefinedUse](check(pkg, mounts, main))
  }

  test("package-use.private-not-reachable") {
    // Accessibility is enforced where a name is used, so the test has to call into the module.
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Secret
        |def main(): Unit \ IO = println(Secret.hidden())
        |""".stripMargin
    val result = check(pkg, mounts, main)
    expectError[ResolutionError.InaccessibleModule](result)
    // The root a package is named under cannot be written in source, so it must not be shown.
    val messages = CompilationMessage.formatAll(result._2)(Formatter.NoFormatter, result._1)
    assert(!messages.contains("$pkg$"), messages)
  }

  test("package-use.private-def-not-reachable") {
    // A def of a non-public module, used directly rather than through the module.
    val pkg = mkPkg()
    val mounts = Map(Mountpoint("Flixball") -> Id)
    val main =
      """
        |use Flixball::Secret.hidden
        |def main(): Unit \ IO = println(hidden())
        |""".stripMargin
    expectError[ResolutionError.InaccessibleModule](check(pkg, mounts, main))
  }

}
