package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{Dependency, ManifestError, ManifestParser, PackageModules, Repository, SemVer}
import ca.uwaterloo.flix.util.{Formatter, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.funsuite.AnyFunSuite
import ca.uwaterloo.flix.tools.pkg.Manifest

import java.io.File
import java.net.URI
import java.nio.file.Paths
import scala.reflect.ClassTag

class TestManifestParser extends AnyFunSuite {

  def expectError[T](result: Result[Manifest, ManifestError])(implicit classTag: ClassTag[T]): Unit =
    result match {
      case Ok(_) => fail(s"Expected failure, but got success.")
      case Err(error) =>
        val expected = classTag.runtimeClass
        val actual = error.getClass
        if (!expected.isAssignableFrom(actual)) {
          fail(s"Expected an error of type ${expected.getSimpleName}, but found:\n\n${actual.getName}")
        }
    }

  val f: Formatter = Formatter.NoFormatter
  val s: String = File.separator
  val tomlCorrect: String = {
    """
      |[package]
      |name = "hello-world"
      |description = "A simple program"
      |version = "0.1.0"
      |repository = "github:johnDoe/hello-world"
      |modules = ["FirstMod", "SecondMod.Foo"]
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |[dependencies]
      |"github:jls/tic-tac-toe" = "1.2.3"
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

  test("Ok.name") {
    assertResult(expected = "hello-world")(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.name
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.description") {
    assertResult(expected = "A simple program")(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.description
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.version") {
    assertResult(expected = SemVer(0, 1, Some(0), None, None))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.version
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.repository.Some") {
    assertResult(expected = Some(GitHub.Project("johnDoe", "hello-world")))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.repository
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.repository.None") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(expected = None)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.repository
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.modules.Some") {
    assertResult(expected = PackageModules.Selected(Set(Symbol.mkModuleSym(List("FirstMod")), Symbol.mkModuleSym(List("SecondMod", "Foo")))))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.modules
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.modules.None") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(expected = PackageModules.All)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.modules
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix") {
    assertResult(expected = SemVer(0, 33, Some(0), None, None))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.flix
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.license.Some") {
    assertResult(expected = Some("Apache-2.0"))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.license
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.license.None") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(expected = None)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.license
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.authors") {
    assertResult(expected = List("John Doe <john@example.com>"))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.authors
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.dependencies") {
    assertResult(expected = List(Dependency.FlixDependency(Repository.GitHub, "jls", "tic-tac-toe", SemVer(1, 2, Some(3), None, None)),
                                 Dependency.FlixDependency(Repository.GitHub, "mlutze", "flixball", SemVer(3, 2, Some(1), None, None)),
                                 Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3.4"),
                                 Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.0-M1"),
                                 Dependency.JarDependency(new URI("https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar").toURL, "myJar.jar")))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.dependencies
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "470"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "470")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "47"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "47")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "a.7.0"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "a.7.0")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.b.0"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.b.0")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.c"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.c")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  /*
  * Errors
  * */
  //File does not exist
  test("ManifestError.IOError.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/missing.toml"
    val path = Paths.get(pathString)
    val result = ManifestParser.parse(path)
    expectError[ManifestError.IOError](result)
  }

  //Name
  test("ManifestError.MissingRequiredProperty.01") {
    val toml = {
      """
        |[package]
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.01") {
    val toml = {
      """
        |[package]
        |mane = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.01") {
    val toml = {
      """
        |[package]
        |name = 1
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  //Description
  test("ManifestError.MissingRequiredProperty.02") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |version = "0.1.0"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |desciption = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.02") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |description = 2
      |version = "0.1.0"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
  }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  //Version
  test("ManifestError.MissingRequiredProperty.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |varsion = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.03") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |description = "A simple program"
      |version = ["0.1.0"]
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("Err.version.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "010"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "010").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0.1"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "0.1.0.1").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "a.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "a.1.0", "For input string: \"a\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.b.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.b.0", "For input string: \"b\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.c"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.1.c", "For input string: \"c\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  // Repository
  test("ManifestError.IllegalPackageKeyFound.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repsository = "github:johnDoe/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("Err.repository.wrongFormat.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "johnDoe/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "johnDoe/hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:github/johnDoe/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:github/johnDoe/hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:johnDoe/"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:johnDoe/").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.05") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:/hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.06") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:/"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:/").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  // Modules
  test("ManifestError.IllegalPackageKeyFound.05") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |modjules = ["FirsMod", "SecondMod"]
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |modules = 123
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  //Flix
  test("ManifestError.MissingRequiredProperty.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.06") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flux = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.05") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = 330
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("Err.flix.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0330"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "0330").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0,33,0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "0,33,0").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "?.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "?.33.0", "For input string: \"?\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.?.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.?.0", "For input string: \"?\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.?"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.33.?", "For input string: \"?\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //License
  test("ManifestError.RequiredPropertyHasWrongType.06") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = 123
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.IllegalPackageKeyFound.07") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |licence = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Authors
  test("Err.authors.missing") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MissingRequiredProperty(null, "package.authors", None).message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("ManifestError.IllegalPackageKeyFound.08") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authars = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.07") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = "John Doe <john@example.com>"
        |
        |""".stripMargin
    }
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("Err.authors.type.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = [12345678]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.AuthorNameError(null, "key at index 0 is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.authors.type.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>", 159]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.AuthorNameError(null, "key at index 1 is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Dependencies
  test("Err.dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = 123
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.DependencyFormatError(null, "class java.lang.Long cannot be cast to class java.lang.String (java.lang.Long and java.lang.String are in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[depandencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "depandencies").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.name.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:ml&tze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "ml&tze").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.name.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic#tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "tic#tac-toe").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "123"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "123").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.23"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "1.23").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls:tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixDependencyFormatError(null, "github:jls:tic-tac-toe").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github/jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixDependencyFormatError(null, "github/jls/tic-tac-toe").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "a.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "a.2.1", "For input string: \"a\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.b.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "3.b.1", "For input string: \"b\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.c"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "3.2.c", "For input string: \"c\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Mvn-dependencies
  test("Err.mvn-dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = 470
        |
        |""".stripMargin
    }
    assertResult(ManifestError.DependencyFormatError(null, "class java.lang.Long cannot be cast to class java.lang.String (java.lang.Long and java.lang.String are in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mwn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "mwn-dependencies").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.name.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.po)tgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "org.po)tgresql").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.name.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:post¤resql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "post¤resql").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }


  test("Err.mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty.jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MavenDependencyFormatError(null, "org.eclipse.jetty.jetty-server").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org:eclipse:jetty:jetty-server" = "4.7.0.M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MavenDependencyFormatError(null, "org:eclipse:jetty:jetty-server").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Jar-dependencies
  test("Err.jar-dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jar" = ["url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlTypeError(null, "class org.tomlj.MutableTomlArray cannot be cast to class java.lang.String (org.tomlj.MutableTomlArray is in unnamed module of loader 'app'; java.lang.String is in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => println(e.message(f)); e.message(f)
    })
  }

  test("Err.jar-dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependences]
        |"myJar.jar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "jar-dependences").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.filename.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlFileNameError(null, "myJar").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.filename.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jsr" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlExtensionError(null, "myJar.jsr", "jsr").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.url.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jar" = "https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlFormatError(null, "https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.url.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jar" = "url:repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.WrongUrlFormat(null, "url:repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar", "URI is not absolute").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

}
