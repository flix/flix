package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.{Bootstrap, InstalledPackage}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.{FileOps, Formatter, LibLevel}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.util.Using

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
    val flix = PkgTestUtils.mkFlix(b)
    flix.setOptions(flix.options.copy(lib = LibLevel.Min))
    b.buildPkg(flix)(Formatter.getDefault).unsafeGet
    p.resolve("artifact").resolve(Bootstrap.PACKAGE_FPKG)
  }

  /** Checks `main` against the package at `pkgPath`, installed under `mounts`, with the library level `lib`. */
  private def check(pkgPath: Path, mounts: Map[Mountpoint, PackageId], main: String, lib: LibLevel = LibLevel.Min): (Option[TypedAst.Root], List[CompilationMessage]) = {
    val pkg = InstalledPackage(pkgPath, Id, SecurityContext.Unrestricted, Map.empty)
    val flix = PkgTestUtils.mkFlix(List(pkg), mounts)
    flix.setOptions(flix.options.copy(lib = lib))
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
    expectSuccess(check(pkg, Map(Mountpoint("List") -> Id), main, lib = LibLevel.All))
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

  //
  // Mounts of a dependency graph.
  //
  // A mount is relative to the package that declares it: a use in a package is looked up in that
  // package's own mount table, and nowhere else. So two dependencies may mount different packages
  // at the same name, and the root project may reuse the name for a third, without conflict.
  //

  private def mkId(name: String): PackageId = PackageId(Repository.GitHub, "test", name)

  /** Writes an `.fpkg` named `name` whose `.flix` sources are `files`, each file name to its text. */
  private def mkFpkg(name: String, files: Map[String, String]): Path = {
    val p = Files.createTempDirectory("flix-mount-graph-").resolve(s"$name.fpkg")
    Using(new ZipOutputStream(Files.newOutputStream(p))) { zip =>
      for ((fileName, text) <- files) {
        zip.putNextEntry(new ZipEntry(fileName))
        zip.write(text.getBytes(StandardCharsets.UTF_8))
        zip.closeEntry()
      }
    }.get
    p
  }

  /** The packages of the graph: three leaves, and two that reach a leaf through a mount. */
  private val D1: PackageId = mkId("d1")
  private val D2: PackageId = mkId("d2")
  private val D3: PackageId = mkId("d3")
  private val B: PackageId = mkId("b")
  private val C: PackageId = mkId("c")

  /** A package that declares `pub mod Lib { pub def who(): <body> }` and mounts nothing. */
  private def leaf(id: PackageId, body: String): InstalledPackage =
    InstalledPackage(mkFpkg(id.name, Map("Lib.flix" -> s"pub mod Lib { pub def who(): $body }")), id, SecurityContext.Unrestricted, Map.empty)

  /** A package whose module `mod` reaches `Lib.who` through its own mount `foo`, which names `dep`. */
  private def viaFoo(id: PackageId, mod: String, dep: PackageId): InstalledPackage = {
    // The use is inside the module: a use at the top of a file does not reach a nested module.
    val src = s"pub mod $mod {\n    use foo::Lib\n    pub def who(): String = Lib.who()\n}"
    InstalledPackage(mkFpkg(id.name, Map(s"$mod.flix" -> src)), id, SecurityContext.Unrestricted, Map(Mountpoint("foo") -> dep))
  }

  /** Checks `main` against the installed `pkgs`, with `mounts` as the mount table of the root project. */
  private def checkGraph(pkgs: List[InstalledPackage], mounts: Map[Mountpoint, PackageId], main: String): (Option[TypedAst.Root], List[CompilationMessage]) = {
    val flix = PkgTestUtils.mkFlix(pkgs, mounts)
    flix.setOptions(flix.options.copy(lib = LibLevel.Min))
    flix.addSource(Path.of("Main.flix"), main, SecurityContext.Unrestricted)
    flix.check()
  }

  test("graph.same-mount-different-packages") {
    // A depends on B and C. B mounts D1 at `foo`, C mounts D2 at `foo`. A mounts neither.
    val pkgs = List(
      leaf(D1, "String = \"d1\""),
      leaf(D2, "String = \"d2\""),
      viaFoo(B, "B", D1),
      viaFoo(C, "C", D2),
    )
    val mounts = Map(Mountpoint("b") -> B, Mountpoint("c") -> C)
    val main =
      """
        |use b::B
        |use c::C
        |def main(): Unit \ IO = println(B.who() + C.who())
        |""".stripMargin
    expectSuccess(checkGraph(pkgs, mounts, main))
  }

  test("graph.same-mount-same-package") {
    // A diamond: B and C both mount D1 at `foo`.
    val pkgs = List(
      leaf(D1, "String = \"d1\""),
      viaFoo(B, "B", D1),
      viaFoo(C, "C", D1),
    )
    val mounts = Map(Mountpoint("b") -> B, Mountpoint("c") -> C)
    val main =
      """
        |use b::B
        |use c::C
        |def main(): Unit \ IO = println(B.who() + C.who())
        |""".stripMargin
    expectSuccess(checkGraph(pkgs, mounts, main))
  }

  test("graph.same-mount-in-root-and-dependency") {
    // A mounts D3 at `foo` too. B's `foo` is still D1: B.who() is a String only if B's own table
    // is consulted, since D3's `Lib.who` is an Int32.
    val pkgs = List(
      leaf(D1, "String = \"d1\""),
      leaf(D3, "Int32 = 3"),
      viaFoo(B, "B", D1),
    )
    val mounts = Map(Mountpoint("b") -> B, Mountpoint("foo") -> D3)
    val main =
      """
        |use b::B
        |use foo::Lib
        |def main(): Unit \ IO = println(B.who())
        |def n(): Int32 = Lib.who()
        |""".stripMargin
    expectSuccess(checkGraph(pkgs, mounts, main))
  }

  test("graph.mount-not-inherited") {
    // B mounts D1 at `foo`, but A does not: the mount of a dependency does not reach its dependent.
    val pkgs = List(
      leaf(D1, "String = \"d1\""),
      viaFoo(B, "B", D1),
    )
    val mounts = Map(Mountpoint("b") -> B)
    val main =
      """
        |use foo::Lib
        |def main(): Unit \ IO = println(Lib.who())
        |""".stripMargin
    expectError[ResolutionError.UndefinedPackage](checkGraph(pkgs, mounts, main))
  }

}
