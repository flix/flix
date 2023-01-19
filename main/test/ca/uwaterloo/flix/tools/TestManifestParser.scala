package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{Dependency, DependencyKind, Manifest, ManifestError, ManifestParser, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

import java.nio.file.Paths

class TestManifestParser extends FunSuite {

  val tomlCorrect = {
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
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[dev-dependencies]
      |"github:fuzzer/fuzzer" = "1.2.3"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3"
      |"org.eclipse.jetty:jetty-server" = "4.7.0"
      |
      |[dev-mvn-dependencies]
      |"org.junit:junit" = "1.2.3"
      |
      |""".stripMargin
  }

  test("Ok.name") {
    assertResult(expected = "hello-world")(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.name
        case Err(e) => Err(e)
      }
    })
  }

  test("Ok.description") {
    assertResult(expected = "A simple program")(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.description
        case Err(e) => Err(e)
      }
    })
  }

  test("Ok.version") {
    assertResult(expected = SemVer(0,1,0))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.version
        case Err(e) => Err(e)
      }
    })
  }

  test("Ok.flix") {
    assertResult(expected = SemVer(0, 33, 0))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.flix
        case Err(e) => Err(e)
      }
    })
  }

  test("Ok.license.Some") {
    assertResult(expected = Some("Apache-2.0"))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.license
        case Err(e) => Err(e)
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(expected = None)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.license
        case Err(e) => Err(e)
      }
    )
  }

  test("Ok.authors") {
    assertResult(expected = List("John Doe <john@example.com>"))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.authors
        case Err(e) => Err(e)
      }
    })
  }

  test("Ok.dependencies") {
    assertResult(expected = List(Dependency.FlixDependency("github:mlutze/flixball", SemVer(3,2,1), DependencyKind.Production),
                                 Dependency.FlixDependency("github:jls/tic-tac-toe", SemVer(1,2,3), DependencyKind.Production),
                                 Dependency.FlixDependency("github:fuzzer/fuzzer", SemVer(1,2,3), DependencyKind.Development),
                                 Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", SemVer(4,7,0), DependencyKind.Production),
                                 Dependency.MavenDependency("org.postgresql", "postgresql", SemVer(1,2,3), DependencyKind.Production),
                                 Dependency.MavenDependency("org.junit", "junit", SemVer(1,2,3), DependencyKind.Development)))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.dependencies
        case Err(e) => Err(e)
      }
    })
  }

  /*
  * Errors
  * */
  //File does not exist
  test("Err.file.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/missing.toml"
    val path = Paths.get(pathString)
    assertResult(Err(ManifestError.IOError(path)))(ManifestParser.parse(path))
  }

  //Name
  test("Err.name.missing") {
    val toml = {
      """
        |[package]
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'package.name' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.name.type") {
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(null, "'package.name' should have type String")))(ManifestParser.parse(toml, null))
  }

  //Description
  test("Err.description.missing") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |version = "0.1.0"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |[dependencies]
      |"github:jls/tic-tac-toe" = "1.2.3"
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[dev-dependencies]
      |"github:fuzzer/fuzzer" = "1.2.3"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3"
      |"org.eclipse.jetty:jetty-server" = "4.7.0"
      |
      |[dev-mvn-dependencies]
      |"org.junit:junit" = "1.2.3"
      |
      |""".stripMargin
  }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'package.description' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.description.type") {
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
      |[dependencies]
      |"github:jls/tic-tac-toe" = "1.2.3"
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[dev-dependencies]
      |"github:fuzzer/fuzzer" = "1.2.3"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3"
      |"org.eclipse.jetty:jetty-server" = "4.7.0"
      |
      |[dev-mvn-dependencies]
      |"org.junit:junit" = "1.2.3"
      |
      |""".stripMargin
  }
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(null, "'package.description' should have type String")))(ManifestParser.parse(toml, null))
  }

  //Version
  test("Err.version.missing") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |description = "A simple program"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |[dependencies]
      |"github:jls/tic-tac-toe" = "1.2.3"
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[dev-dependencies]
      |"github:fuzzer/fuzzer" = "1.2.3"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3"
      |"org.eclipse.jetty:jetty-server" = "4.7.0"
      |
      |[dev-mvn-dependencies]
      |"org.junit:junit" = "1.2.3"
      |
      |""".stripMargin
  }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'package.version' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.version.type") {
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
      |[dependencies]
      |"github:jls/tic-tac-toe" = "1.2.3"
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[dev-dependencies]
      |"github:fuzzer/fuzzer" = "1.2.3"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3"
      |"org.eclipse.jetty:jetty-server" = "4.7.0"
      |
      |[dev-mvn-dependencies]
      |"org.junit:junit" = "1.2.3"
      |
      |""".stripMargin
  }
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(null, "'package.version' should have type String")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  //Flix
  test("Err.flix.missing") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'package.flix' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.flix.type") {
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(null, "'package.flix' should have type String")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  //License
  test("Err.license.type") {
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(null, "'package.license' should have type String")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'package.authors' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.authors.type.01") {
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(null, "'package.authors' should have type Array")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.AuthorNameError(null, "All author names should be of type String")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.AuthorNameError(null, "All author names should be of type String")))(ManifestParser.parse(toml, null))
  }

  //Dependencies
  test("Err.dependencies.missing") {
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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'dependencies' is missing")))(ManifestParser.parse(toml, null))
  }

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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.DependencyFormatError(null, "A value in a dependency table should be of type String")))(ManifestParser.parse(toml, null))
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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
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
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  //Dev-dependencies
  test("Err.dev-dependencies.missing") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'dev-dependencies' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-dependencies.type") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = ["1.2.3"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.DependencyFormatError(null, "A value in a dependency table should be of type String")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-dependencies.format.01") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "123"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-dependencies.format.02") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3.4.5"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-dependencies.numbers.01") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "a.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-dependencies.numbers.02") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.b.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-dependencies.numbers.03") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.c"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  //Mvn-dependencies
  test("Err.mvn-dependencies.missing") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'mvn-dependencies' is missing")))(ManifestParser.parse(toml, null))
  }

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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = 470
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.DependencyFormatError(null, "A value in a dependency table should be of type String")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "470"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
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
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "47.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
  }

  test("Err.mvn-dependencies.format.03") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty.jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MavenDependencyFormatError(null, "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(toml, null))
  }

  test("Err.mvn-dependencies.format.04") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org:eclipse:jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MavenDependencyFormatError(null, "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(toml, null))
  }

  test("Err.mvn-dependencies.numbers.01") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "a.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  test("Err.mvn-dependencies.numbers.02") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.b.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  test("Err.mvn-dependencies.numbers.03") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.c"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  //Dev-mvn-dependencies
  test("Err.dev-mvn-dependencies.missing") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MissingRequiredProperty(null, "'dev-mvn-dependencies' is missing")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.type") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = 123456
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.DependencyFormatError(null, "A value in a dependency table should be of type String")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.format.01") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "123.456"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.format.02") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3.4.5.6"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionHasWrongLength(null, "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.format.03") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org;junit;junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MavenDependencyFormatError(null, "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.format.04") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org:junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.MavenDependencyFormatError(null, "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.numbers.01") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "a.2.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.numbers.02") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.b.3"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

  test("Err.dev-mvn-dependencies.numbers.03") {
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
        |"github:mlutze/flixball" = "3.2.1"
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3"
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.c"
        |
        |""".stripMargin
    }
    assertResult(Err(ManifestError.VersionNumberWrong(null, "Could not parse version as three numbers")))(ManifestParser.parse(toml, null))
  }

}
