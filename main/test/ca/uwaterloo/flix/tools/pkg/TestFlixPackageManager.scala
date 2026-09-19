package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap
import org.scalatest.{BeforeAndAfter, DoNotDiscover}
import ca.uwaterloo.flix.tools.pkg.PkgTestUtils.ManifestPath
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintStream}
import java.nio.file.Files

@DoNotDiscover
class TestFlixPackageManager extends AnyFunSuite with BeforeAndAfter {
  private val s: String = File.separator
  private implicit val formatter: Formatter = Formatter.NoFormatter
  private implicit val out: PrintStream = System.out

  test("Install missing dependency.01") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-clerk" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val resolution = FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }

      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
        case Ok(l) =>
          l.packages.head.path.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg")
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Install missing dependency.02") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val manifests =
        FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
          case Ok(resolution) => FlixPackageManager.resolveSecurityLevels(resolution)
          case Err(e) => fail(e.message(formatter))
        }
      FlixPackageManager.installAll(manifests, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
        case Ok(l) => l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")) &&
          l.packages.exists(_.path.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg"))
        case Err(e) => e
      }
    })
  }

  test("Install missing dependencies from list of manifests") {
    assertResult(expected = true)(actual = {
      val toml1 = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-clerk" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val toml2 = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest1 = ManifestParser.parse(toml1, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }
      val manifest2 = ManifestParser.parse(toml2, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val resolution1 = FlixPackageManager.resolve(manifest1, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }
      val resolution2 = FlixPackageManager.resolve(manifest2, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }
      val resolution = FlixPackageManager.SecureResolution(origin = manifest1, security = resolution1.security ++ resolution2.security, manifestToFlixDeps = resolution1.manifestToFlixDeps ++ resolution2.manifestToFlixDeps, tomlDigests = resolution1.tomlDigests ++ resolution2.tomlDigests)


      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
        case Ok(l) => l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")) &&
          l.packages.exists(_.path.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg"))
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Do not install existing dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val resolution = FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }
      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) // installs the dependency
      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match { // does nothing
        case Ok(l) =>
          l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg"))
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Find transitive dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-entrance" = "1.2.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
        case Ok(resolution) => resolution.manifests.contains(manifest) && resolution.manifests.exists(m => m.name == "museum-clerk")
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Give error for missing dependency") {
    val toml = {
      """
        |[package]
        |name = "test"
        |description = "test"
        |version = "0.0.0"
        |flix = "0.0.0"
        |authors = ["Anna Blume"]
        |
        |[dependencies]
        |"github:flix/does-not-exist" = "1.0.0"
        |
        |[mvn-dependencies]
        |
        |""".stripMargin
    }

    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter)) //should not happen
    }

    val path = Files.createTempDirectory("")
    FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
      case Ok(res) => fail(res.toString)
      case Err(e) =>
        e.message(formatter)
        succeed
    }
  }

  test("Give error for missing version") {
    assertResult(expected = PackageError.VersionDoesNotExist(SemVer(0, 0, 1), Project("flix", "museum")).message(formatter))(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum" = "0.0.1"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter))
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Install transitive dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum" = "1.4.0"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val manifests = FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
        case Ok(resolution) => FlixPackageManager.resolveSecurityLevels(resolution)
        case Err(e) => fail(e.message(formatter))
      }
      FlixPackageManager.installAll(manifests, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
        case Ok(l) =>
          l.packages.exists(_.path.endsWith(s"flix${s}museum${s}1.4.0${s}museum-1.4.0.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-entrance${s}1.2.0${s}museum-entrance-1.2.0.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-restaurant${s}1.1.0${s}museum-restaurant-1.1.0.fpkg"))
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("resolveSecurityLevels.strictest.01") {
    // A package that one dependent declares paranoid is paranoid, whatever another declares.
    assertResult(expected = SecurityContext.Paranoid)(actual = levelOfShared("paranoid", "unrestricted"))
  }

  test("resolveSecurityLevels.strictest.02") {
    // The order in which the dependents are met makes no difference.
    assertResult(expected = SecurityContext.Paranoid)(actual = levelOfShared("unrestricted", "paranoid"))
  }

  test("resolveSecurityLevels.strictest.03") {
    assertResult(expected = SecurityContext.Plain)(actual = levelOfShared("unrestricted", "plain"))
  }

  test("resolveSecurityLevels.strictest.04") {
    assertResult(expected = SecurityContext.Unrestricted)(actual = levelOfShared("unrestricted", "unrestricted"))
  }

  /**
    * Returns the security context of a package that two dependents declare, the first with the
    * security context `left` and the second with `right`. The project allows both dependents to
    * be unrestricted.
    */
  private def levelOfShared(left: String, right: String): SecurityContext = {
    val origin = mkManifest("origin",
      """"github:flix/left" = { version = "1.0.0", security = "unrestricted" }
        |"github:flix/right" = { version = "1.0.0", security = "unrestricted" }""".stripMargin)
    val l = mkManifest("left", s""""github:flix/shared" = { version = "1.0.0", security = "$left" }""")
    val r = mkManifest("right", s""""github:flix/shared" = { version = "1.0.0", security = "$right" }""")
    val shared = mkManifest("shared", "")

    val List(toLeft, toRight) = FlixPackageManager.findFlixDependencies(origin)
    val resolution = FlixPackageManager.Resolution(
      origin = origin,
      manifests = List(origin, l, r, shared),
      immediateDependents = Map(origin -> Nil, l -> List(origin), r -> List(origin), shared -> List(l, r)),
      manifestToFlixDeps = ListMap(Map(
        l -> List(toLeft),
        r -> List(toRight),
        shared -> (FlixPackageManager.findFlixDependencies(l) ::: FlixPackageManager.findFlixDependencies(r))
      )),
      tomlDigests = Map.empty
    )
    FlixPackageManager.resolveSecurityLevels(resolution).security(shared)
  }

  test("checkFlixVersions.01") {
    // A package that requires a newer Flix than the one that is running.
    val resolution = mkResolutionOf(origin = "0.33.0", a = "0.80.0", b = "0.33.0")
    assertResult(expected = List(PackageError.FlixVersionTooOld(PackageId(Repository.GitHub, "flix", "a"), SemVer(0, 1, 0), SemVer(0, 80, 0), SemVer(0, 76, 0))))(
      actual = FlixPackageManager.checkFlixVersions(resolution, SemVer(0, 76, 0))
    )
  }

  test("checkFlixVersions.02") {
    // A package that requires the Flix that is running, or an older one, can be built.
    val resolution = mkResolutionOf(origin = "0.33.0", a = "0.76.0", b = "0.10.0")
    assertResult(expected = Nil)(actual = FlixPackageManager.checkFlixVersions(resolution, SemVer(0, 76, 0)))
  }

  test("checkFlixVersions.03") {
    // The project is not checked here: whoever read its manifest knows where it came from.
    val resolution = mkResolutionOf(origin = "0.80.0", a = "0.33.0", b = "0.33.0")
    assertResult(expected = Nil)(actual = FlixPackageManager.checkFlixVersions(resolution, SemVer(0, 76, 0)))
  }

  test("checkFlixVersions.04") {
    // Every package that cannot be built is reported, in order of identifier.
    val resolution = mkResolutionOf(origin = "0.33.0", a = "0.80.0", b = "0.90.0")
    assertResult(expected = List("github:flix/a", "github:flix/b"))(
      actual = FlixPackageManager.checkFlixVersions(resolution, SemVer(0, 76, 0)).collect {
        case e: PackageError.FlixVersionTooOld => e.identifier.toString
      }
    )
  }

  /**
    * Returns the resolution of a project that requires the packages `a` and `b`, where the
    * project and the two packages each require the given version of Flix.
    */
  private def mkResolutionOf(origin: String, a: String, b: String): FlixPackageManager.Resolution = {
    val project = mkManifest("origin",
      """"github:flix/b" = "0.1.0"
        |"github:flix/a" = "0.1.0"""".stripMargin, flix = origin)
    val manifestA = mkManifest("a", "", flix = a)
    val manifestB = mkManifest("b", "", flix = b)
    val List(toB, toA) = FlixPackageManager.findFlixDependencies(project)
    FlixPackageManager.Resolution(
      origin = project,
      manifests = List(project, manifestB, manifestA),
      immediateDependents = Map(project -> Nil, manifestA -> List(project), manifestB -> List(project)),
      manifestToFlixDeps = ListMap(Map(manifestA -> List(toA), manifestB -> List(toB))),
      tomlDigests = Map.empty
    )
  }

  test("mismatched-versions") {
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-mismatched-versions" = "0.1.0"
        |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val path = Files.createTempDirectory("")
    FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
      case Ok(_) => fail("expected error, got success")
      case Err(_: PackageError.MismatchedVersions) => succeed
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("mkIncompatibleVersions.01") {
    // The requirements are ordered by version, and then by dependent.
    val beta = mkManifest("beta", """"github:flix/museum-clerk" = "2.0.0"""")
    val alpha = mkManifest("alpha", """"github:flix/museum-clerk" = "2.0.0"""")
    val gamma = mkManifest("gamma", """"github:flix/museum-clerk" = "1.1.0"""")
    val id = PackageId(Repository.GitHub, "flix", "museum-clerk")
    val requirements = List(beta, alpha, gamma).map(m => (m, FlixPackageManager.findFlixDependencies(m).head))
    val error = FlixPackageManager.mkIncompatibleVersions(id, requirements)
    assertResult(expected = id)(actual = error.identifier)
    assertResult(expected = List(("gamma", SemVer(1, 1, 0)), ("alpha", SemVer(2, 0, 0)), ("beta", SemVer(2, 0, 0))))(
      actual = error.requirements.map { case (dependent, dep) => (dependent.name, dep.version) }
    )
  }

  test("resolve.raise.01") {
    // museum-giftshop 1.0.0 requires museum-clerk 1.0.0 and museum-entrance 1.2.0 requires
    // museum-clerk 1.1.0, so museum-clerk is built at 1.1.0, and at no other version.
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:flix/museum-giftshop" = "1.0.0"
        |"github:flix/museum-entrance" = "1.2.0"
        |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val path = Files.createTempDirectory("")
    FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
      case Ok(resolution) =>
        assertResult(expected = List(SemVer(1, 1, 0)))(
          actual = resolution.manifests.filter(_.name == "museum-clerk").map(_.version)
        )
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("resolve.raise.02") {
    // What is installed and locked is the version that was selected, not a version that was declared.
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:flix/museum-giftshop" = "1.0.0"
        |"github:flix/museum-entrance" = "1.2.0"
        |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val path = Files.createTempDirectory("")
    val resolution = FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
      case Ok(r) => FlixPackageManager.resolveSecurityLevels(r)
      case Err(e) => fail(e.message(formatter))
    }
    FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
      case Ok(installation) =>
        val clerk = PackageId(Repository.GitHub, "flix", "museum-clerk")
        // Both versions are locked, since the manifest of each was read to resolve the graph. Only
        // the package that was downloaded records an fpkg.
        assertResult(expected = Some(true))(actual = installation.lockfile.packages.get((clerk, SemVer(1, 1, 0))).map(_.fpkg.isDefined))
        assertResult(expected = Some(false))(actual = installation.lockfile.packages.get((clerk, SemVer(1, 0, 0))).map(_.fpkg.isDefined))
        assertResult(expected = List(s"museum-clerk-1.1.0.fpkg"))(
          actual = installation.packages.filter(_.id == clerk).map(_.path.getFileName.toString).distinct
        )
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("selectVersion.01") {
    // The greatest of the required versions is the least one that satisfies them all.
    assertResult(expected = Some(SemVer(1, 5, 1)))(
      actual = FlixPackageManager.selectVersion(List(SemVer(1, 2, 0), SemVer(1, 5, 1), SemVer(1, 3, 9)))
    )
  }

  test("selectVersion.02") {
    // Versions that do not share a major have nothing to select.
    assertResult(expected = None)(
      actual = FlixPackageManager.selectVersion(List(SemVer(1, 2, 0), SemVer(2, 0, 0)))
    )
  }

  test("selectVersion.03") {
    // A pre-1.0 package is selected across a minor, since both versions have major 0.
    assertResult(expected = Some(SemVer(0, 4, 2)))(
      actual = FlixPackageManager.selectVersion(List(SemVer(0, 3, 0), SemVer(0, 4, 2)))
    )
  }

  // The example in the documentation of `resolve`:
  //
  //   project  requires  A 1.0.0  and  B 1.0.0
  //   A 1.0.0  requires  C 1.1.1
  //   B 1.0.0  requires  C 1.1.2
  //   C 1.1.1  requires  X 1.0.0
  //   C 1.1.2  requires  nothing
  private val A = PackageId(Repository.GitHub, "flix", "a")
  private val B = PackageId(Repository.GitHub, "flix", "b")
  private val C = PackageId(Repository.GitHub, "flix", "c")
  private val X = PackageId(Repository.GitHub, "flix", "x")

  private val ExampleNodes = List(
    (A, SemVer(1, 0, 0)), (B, SemVer(1, 0, 0)), (C, SemVer(1, 1, 1)), (C, SemVer(1, 1, 2)), (X, SemVer(1, 0, 0))
  )

  private val ExampleRequires = Map(
    (A, SemVer(1, 0, 0)) -> List(C),
    (B, SemVer(1, 0, 0)) -> List(C),
    (C, SemVer(1, 1, 1)) -> List(X)
  )

  test("select.01") {
    // Every package is given the greatest version it is required at.
    assertResult(expected = Ok(Map(A -> SemVer(1, 0, 0), B -> SemVer(1, 0, 0), C -> SemVer(1, 1, 2), X -> SemVer(1, 0, 0))))(
      actual = FlixPackageManager.select(ExampleNodes)
    )
  }

  test("select.02") {
    // A package whose versions do not share a major has no version to be given.
    assertResult(expected = Err(C))(
      actual = FlixPackageManager.select((C, SemVer(2, 0, 0)) :: ExampleNodes)
    )
  }

  test("live.01") {
    // X is required only by C 1.1.1, which is not the selected version of C.
    val selected = Map(A -> SemVer(1, 0, 0), B -> SemVer(1, 0, 0), C -> SemVer(1, 1, 2), X -> SemVer(1, 0, 0))
    assertResult(expected = Set(A, B, C))(
      actual = FlixPackageManager.live(List(A, B), selected, ExampleRequires)
    )
  }

  test("live.02") {
    // X is live if the selected version of C requires it too.
    val selected = Map(A -> SemVer(1, 0, 0), B -> SemVer(1, 0, 0), C -> SemVer(1, 1, 2), X -> SemVer(1, 0, 0))
    val requires = ExampleRequires + ((C, SemVer(1, 1, 2)) -> List(X))
    assertResult(expected = Set(A, B, C, X))(
      actual = FlixPackageManager.live(List(A, B), selected, requires)
    )
  }

  test("live.03") {
    // A cycle among the selected versions ends the walk.
    val selected = Map(A -> SemVer(1, 0, 0), B -> SemVer(1, 0, 0))
    val requires = Map((A, SemVer(1, 0, 0)) -> List(B), (B, SemVer(1, 0, 0)) -> List(A))
    assertResult(expected = Set(A, B))(
      actual = FlixPackageManager.live(List(A), selected, requires)
    )
  }

  /**
    * Returns a manifest named `name` with the given Flix dependency declarations `deps`, that
    * requires the version `flix` of Flix.
    */
  private def mkManifest(name: String, deps: String, flix: String = "0.33.0"): Manifest = {
    val toml =
      s"""
         |[package]
         |name = "$name"
         |description = "test"
         |version = "0.1.0"
         |flix = "$flix"
         |authors = ["flix"]
         |
         |[dependencies]
         |$deps
         |""".stripMargin
    ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }
  }

}
