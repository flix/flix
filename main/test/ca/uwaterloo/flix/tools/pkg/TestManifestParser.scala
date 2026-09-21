package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{Formatter, Result}
import org.scalatest.DoNotDiscover
import ca.uwaterloo.flix.tools.pkg.PkgTestUtils.ManifestPath
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Paths
import scala.reflect.ClassTag

@DoNotDiscover
class TestManifestParser extends AnyFunSuite {

  def expectError[T](result: Result[Manifest, ManifestError])(implicit classTag: ClassTag[T]): Unit = {
    result match {
      case Ok(_) => fail(s"Expected failure, but got success.")
      case Err(error) =>
        val expected = classTag.runtimeClass
        val actual = error.getClass
        if (!expected.isAssignableFrom(actual)) {
          fail(s"Expected an error of type ${expected.getSimpleName}, but found:\n\n${actual.getName}")
        }
    }
  }

  /**
    * Asserts that `actual` is `expected` up to the order of their dependencies, which
    * [[Manifest.format]] sorts.
    */
  def assertSameUpToOrder(expected: Manifest, actual: Manifest): Unit = {
    assertResult(expected.copy(dependencies = Nil))(actual.copy(dependencies = Nil))
    assertResult(expected.dependencies.toSet)(actual.dependencies.toSet)
  }

  val f: Formatter = Formatter.NoFormatter
  val s: String = File.separator
  val tomlCorrect: String = {
    """
      |[package]
      |version = "0.1.0"
      |repository = "github:johnDoe/hello-world"
      |flix = "0.33.0"
      |
      |[dependencies]
      |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3.4"
      |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
      |
      |[jar-dependencies]
      |"myJar.jar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
      |
      |""".stripMargin
  }

  test("Ok.named-by-its-repository") {
    // A package is named by the repository it is published as, which is what a dependent
    // addresses it by.
    val toml =
      """
        |[package]
        |version = "0.1.0"
        |repository = "github:johnDoe/hello-world"
        |flix = "0.33.0"
        |""".stripMargin
    assertResult(expected = "johnDoe/hello-world")(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.displayName
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.unnamed") {
    // A package that declares no repository cannot be addressed, and so has no name.
    val toml =
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |""".stripMargin
    assertResult(expected = Manifest.Unnamed)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.displayName
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.minimal") {
    // A manifest declares only what something reads.
    val toml =
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |""".stripMargin
    assertResult(expected = SemVer(0, 1, 0))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.version
        case Err(e) => fail(e.message(f))
      }
    )
  }

  test("Ok.version") {
    assertResult(expected = SemVer(0, 1, 0))(actual = {
      ManifestParser.parse(tomlCorrect, ManifestPath) match {
        case Ok(manifest) => manifest.version
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.repository.Some") {
    assertResult(expected = Some(GitHub.Project("johnDoe", "hello-world")))(actual = {
      ManifestParser.parse(tomlCorrect, ManifestPath) match {
        case Ok(manifest) => manifest.repository
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.repository.None") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    assertResult(expected = None)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.repository
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix") {
    assertResult(expected = SemVer(0, 33, 0))(actual = {
      ManifestParser.parse(tomlCorrect, ManifestPath) match {
        case Ok(manifest) => manifest.flix
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.dependencies") {
    assertResult(expected = List(Dependency.FlixDependency(PackageId(Repository.GitHub, "jls", "tic-tac-toe"), SemVer(1, 2, 3), Mountpoint("ticTacToe"), SecurityContext.Plain, DependencyStyle.Table),
      Dependency.FlixDependency(PackageId(Repository.GitHub, "mlutze", "flixball"), SemVer(3, 2, 1), Mountpoint("flixball"), SecurityContext.Plain, DependencyStyle.VersionOnly),
      Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3.4"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.0-M1"),
      Dependency.JarDependency("https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar", "myJar.jar")))(actual = {
      ManifestParser.parse(tomlCorrect, ManifestPath) match {
        case Ok(manifest) => manifest.dependencies
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "470"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "470")))(ManifestParser.parse(toml, ManifestPath) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "47"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "47")))(ManifestParser.parse(toml, ManifestPath) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "a.7.0"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "a.7.0")))(ManifestParser.parse(toml, ManifestPath) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.b.0"
        |
        |""".stripMargin
    }
    assertResult(expected = Set(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.b.0")))(ManifestParser.parse(toml, ManifestPath) match {
      case Ok(manifest) => manifest.dependencies.toSet
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.c"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.c")))(ManifestParser.parse(toml, ManifestPath) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.flix-dependency-permission.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |""".stripMargin
    }
    assertResult(expected = SecurityContext.Plain)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .sctx
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |""".stripMargin
    }
    assertResult(expected = SecurityContext.Plain)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .sctx
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "paranoid" }
        |""".stripMargin
    }
    assertResult(expected = SecurityContext.Paranoid)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .sctx
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "plain" }
        |""".stripMargin
    }
    assertResult(expected = SecurityContext.Plain)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .sctx
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.05") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "unrestricted" }
        |""".stripMargin
    }
    assertResult(expected = SecurityContext.Unrestricted)(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .sctx
        case Err(e) => e.message(f)
      }
    )
  }

  // Identity tests / parse-render-parse
  test("Manifest.Identity.01") {
    val toml = tomlCorrect
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "470"
        |
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "47"
        |
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.05") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "a.7.0"
        |
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.06") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.b.0"
        |
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.07") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.c"
        |
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.08") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.09") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.10") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "paranoid" }
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.11") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "plain" }
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.12") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "unrestricted" }
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.13") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "unrestricted" }
        |""".stripMargin
    }
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  /////////////
  // Errors  //
  /////////////
  // File does not exist
  test("ManifestError.IOError.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/missing.toml"
    val path = Paths.get(pathString)
    val result = ManifestParser.parse(path)
    expectError[ManifestError.IOError](result)
  }

  test("ManifestError.IllegalPackageKeyFound.01") {
    // A key the package table does not have at all.
    val toml = {
      """
        |[package]
        |mane = "hello-world"
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Description

  test("ManifestError.IllegalPackageKeyFound.02") {
    // A manifest declares only what is read. This key is read by nothing, so it says
    // nothing, and is no longer one a manifest may declare.
    val toml = {
      """
        |[package]
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Version
  test("ManifestError.MissingRequiredProperty.03") {
    val toml = {
      """
        |[package]
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.03") {
    val toml = {
      """
        |[package]
        |varsion = "0.1.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.03") {
    val toml = {
      """
        |[package]
        |version = ["0.1.0"]
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.01") {
    val toml = {
      """
        |[package]
        |version = "010"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0.1"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.VersionNumberWrong.01") {
    val toml = {
      """
        |[package]
        |version = "a.1.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.02") {
    val toml = {
      """
        |[package]
        |version = "0.b.0"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.c"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  // Repository
  test("ManifestError.IllegalPackageKeyFound.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repsository = "github:johnDoe/hello-world"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RepositoryFormatError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repository = "hello-world"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repository = "johnDoe/hello-world"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repository = "github:github/johnDoe/hello-world"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repository = "github:johnDoe/"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.05") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repository = "github:/hello-world"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.06") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |repository = "github:/"
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  // Modules
  test("ManifestError.IllegalPackageKeyFound.05") {
    // A manifest declares only what is read. This key is read by nothing, so it says
    // nothing, and is no longer one a manifest may declare.
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |modules = ["FirstMod", "SecondMod"]
        |flix = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Flix
  test("ManifestError.MissingRequiredProperty.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.06") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flux = "0.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.05") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = 330
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0330"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0,33,0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.VersionNumberWrong.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "?.33.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.05") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.?.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.06") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.?"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  //License

  test("ManifestError.IllegalPackageKeyFound.07") {
    // A manifest declares only what is read. This key is read by nothing, so it says
    // nothing, and is no longer one a manifest may declare.
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Authors

  test("ManifestError.IllegalPackageKeyFound.08") {
    // A manifest declares only what is read. This key is read by nothing, so it says
    // nothing, and is no longer one a manifest may declare.
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Name
  test("ManifestError.IllegalPackageKeyFound.09") {
    // `name` named nothing -- a package is named by the repository it is published as -- and
    // was free to disagree with the repository beside it.
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |version = "0.1.0"
        |repository = "github:johnDoe/hello-world"
        |flix = "0.33.0"
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Dependencies
  test("ManifestError.DependencyFormatError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = 123
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.VersionTypeError](result)
  }

  test("ManifestError.IllegalTableFound.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[depandencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalTableFound](result)
  }
  test("ManifestError.IllegalName.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |"github:ml&tze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.IllegalName.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic#tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.FlixVersionFormatError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "123"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.23"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixDependencyFormatError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls:tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyFormatError](result)
  }

  test("ManifestError.FlixDependencyFormatError.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github/jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |"github:mlutze/flixball" = "a.2.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |"github:mlutze/flixball" = "3.b.1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.05") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |"github:mlutze/flixball" = "3.2.c"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  //Mvn-dependencies
  test("ManifestError.DependencyFormatError.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = 470
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.DependencyFormatError](result)
  }

  test("ManifestError.IllegalTableFound.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mwn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalTableFound](result)
  }

  test("ManifestError.IllegalName.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.po)tgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.IllegalName.04") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:post¤resql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.MavenDependencyFormatError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty.jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.MavenDependencyFormatError](result)
  }

  test("ManifestError.MavenDependencyFormatError.02") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org:eclipse:jetty:jetty-server" = "4.7.0.M1"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.MavenDependencyFormatError](result)
  }

  //Jar-dependencies
  test("ManifestError.JarUrlTypeError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[jar-dependencies]
        |"myJar.jar" = ["url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.JarUrlTypeError](result)
  }

  test("ManifestError.IllegalTableFound.03") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[jar-dependences]
        |"myJar.jar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.IllegalTableFound](result)
  }

  test("ManifestError.JarUrlFileNameError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[jar-dependencies]
        |"myJar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.JarUrlFileNameError](result)
  }

  test("ManifestError.JarUrlExtensionError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[jar-dependencies]
        |"myJar.jsr" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.JarUrlExtensionError](result)
  }

  test("ManifestError.JarUrlFormatError.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[jar-dependencies]
        |"myJar.jar" = "https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.JarUrlFormatError](result)
  }

  test("ManifestError.WrongUrlFormat.01") {
    val toml = {
      """
        |[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[jar-dependencies]
        |"myJar.jar" = "url:repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.WrongUrlFormat](result)
  }

  test("ManifestError.FlixUnknownSecurityValue.01") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixUnknownSecurityValue](result)
  }

  test("ManifestError.FlixUnknownSecurityValue.02") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = "abc" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixUnknownSecurityValue](result)
  }

  test("ManifestError.FlixDependencySecurityType.01") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = [] }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencySecurityType](result)
  }

  test("ManifestError.FlixDependencySecurityType.02") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = ["plain"] }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencySecurityType](result)
  }

  test("ManifestError.FlixDependencySecurityType.03") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = true }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencySecurityType](result)
  }

  test("ManifestError.FlixDependencySecurityType.04") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe", security = 42 }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencySecurityType](result)
  }

  test("ManifestError.UnsupportedRepository.01") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"hubgit:jls/tic-tac-toe" = "1.2.3"
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.UnsupportedRepository](result)
  }

  test("Ok.mount") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "Game" }
        |"github:mlutze/flixball" = "3.2.1"
        |""".stripMargin
    assertResult(expected = List(Mountpoint("Game"), Mountpoint("flixball")))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.flixDependencies.map(_.mount)
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.mount.lowercase") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "game" }
        |""".stripMargin
    assertResult(expected = List(Mountpoint("game")))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.flixDependencies.map(_.mount)
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.mount.derived.01") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:mlutze/flixball" = "3.2.1"
        |""".stripMargin
    assertResult(expected = List(Mountpoint("flixball")))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.flixDependencies.map(_.mount)
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.mount.derived.02") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:mlutze/flixball" = { version = "3.2.1", security = "paranoid" }
        |""".stripMargin
    assertResult(expected = List(Mountpoint("flixball")))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.flixDependencies.map(_.mount)
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.mount.derived.03") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:flix/Flix" = "1.0.0"
        |""".stripMargin
    assertResult(expected = List(Mountpoint("Flix")))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.flixDependencies.map(_.mount)
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.mount.derived.04") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "ticTacToe" }
        |""".stripMargin
    assertResult(expected = List(Mountpoint("ticTacToe")))(actual =
      ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m.flixDependencies.map(_.mount)
        case Err(e) => e.message(f)
      }
    )
  }

  test("ManifestError.FlixDependencyUnderivableMount.Hyphen.01") {
    // The name of the repository is never folded into a mount.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyUnderivableMount](result)
  }

  test("ManifestError.FlixDependencyUnderivableMount.Hyphen.02") {
    // Nor in the table form.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", security = "plain" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyUnderivableMount](result)
  }

  test("ManifestError.FlixDependencyUnderivableMount.Digit.01") {
    // A mount begins with a letter.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:gabrielecirulli/2048" = "1.0.0"
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyUnderivableMount](result)
  }

  test("ManifestError.FlixDependencyUnderivableMount.Keyword.01") {
    // A keyword is read before a name, so it cannot be written before `::`.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:someone/type" = "1.0.0"
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyUnderivableMount](result)
  }

  test("Manifest.Identity.Mount") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "Game", security = "paranoid" }
        |"github:mlutze/flixball" = "3.2.1"
        |""".stripMargin
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val manifest2 = ManifestParser.parse(Manifest.format(manifest1), ManifestPath).unsafeGet
    assertSameUpToOrder(manifest1, manifest2)
  }

  test("Manifest.Identity.Mount.Derived") {
    // A derived mount is not rendered, so the shorthand survives a round trip.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:mlutze/flixball" = "3.2.1"
        |""".stripMargin
    val manifest1 = ManifestParser.parse(toml, ManifestPath).unsafeGet
    val rendered = Manifest.format(manifest1)
    assert(!rendered.contains("mount"), rendered)
    assertResult(manifest1)(ManifestParser.parse(rendered, ManifestPath).unsafeGet)
  }

  test("ManifestError.FlixDependencyDuplicateMount.Derived") {
    // Two repositories with one name derive one mount.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:alice/json" = "1.0.0"
        |"github:bob/json" = "2.0.0"
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyDuplicateMount](result)
  }

  test("ManifestError.FlixDependencyDuplicateMount") {
    // Two dependencies under one mount. Without the error one of them would silently win.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "game" }
        |"github:mlutze/flixball" = { version = "3.2.1", mount = "game" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyDuplicateMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount") {
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "Foo.Bar" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Keyword.01") {
    // A keyword is read before a name, so it cannot be written before `::`.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "type" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Keyword.02") {
    // Only the first group matters: the lexer reads `type`, `-`, `level`.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "type-level" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Digit.01") {
    // A mount begins with a letter.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "2048" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Digit.02") {
    // Every group begins with a letter.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "utf-8" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Hyphen.01") {
    // A trailing hyphen.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "json-" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Hyphen.02") {
    // A repeated hyphen.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "flix--json" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

  test("ManifestError.FlixDependencyIllegalMount.Hyphen.03") {
    // A hyphen anywhere in a mount.
    val toml =
      """[package]
        |version = "0.1.0"
        |flix = "0.33.0"
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", mount = "tic-tac-toe" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, ManifestPath)
    expectError[ManifestError.FlixDependencyIllegalMount](result)
  }

}
