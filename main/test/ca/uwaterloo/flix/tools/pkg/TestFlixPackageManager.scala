package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, Version}
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
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum-clerk" = { version = "2.1.2", mount = "clerk" }
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
          l.packages.head.path.endsWith(s"flix${s}museum-clerk${s}2.1.2${s}museum-clerk-2.1.2.fpkg")
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Install missing dependency.02") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = { version = "2.0.2", mount = "giftshop" }
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
        case Ok(l) => l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}2.0.2${s}museum-giftshop-2.0.2.fpkg")) &&
          l.packages.exists(_.path.endsWith(s"flix${s}museum-clerk${s}2.1.2${s}museum-clerk-2.1.2.fpkg"))
        case Err(e) => e
      }
    })
  }

  test("Install falls back to the release listing") {
    // The address of a release asset is guessed before the listing is read, which costs a
    // request against the API rate limit. `jaschdoc/flix-test-pkg-eff-upgrade` publishes its
    // package as `test-pkg-eff-upgrade.fpkg`, which is neither the fixed name nor the name of
    // the repository, so it is found only by reading the listing.
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-eff-upgrade" = { version = "0.1.1", mount = "effUpgrade" }
        |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val path = Files.createTempDirectory("")
    val resolution = FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
      case Ok(r) => r
      case Err(e) => fail(e.message(formatter))
    }

    FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
      case Ok(installation) =>
        assert(installation.packages.exists(_.path.endsWith(s"flix-test-pkg-eff-upgrade-0.1.1.${Bootstrap.EXT_FPKG}")))
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("Install missing dependencies from list of manifests") {
    assertResult(expected = true)(actual = {
      val toml1 = {
        """
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum-clerk" = { version = "2.1.2", mount = "clerk" }
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val toml2 = {
        """
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = { version = "2.0.2", mount = "giftshop" }
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
        case Ok(l) => l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}2.0.2${s}museum-giftshop-2.0.2.fpkg")) &&
          l.packages.exists(_.path.endsWith(s"flix${s}museum-clerk${s}2.1.2${s}museum-clerk-2.1.2.fpkg"))
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Do not install existing dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = { version = "2.0.2", mount = "giftshop" }
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
          l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}2.0.2${s}museum-giftshop-2.0.2.fpkg"))
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Find transitive dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum-entrance" = { version = "2.0.2", mount = "entrance" }
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
        case Ok(resolution) =>
          val clerk = PackageId(Repository.GitHub, "flix", "museum-clerk")
          resolution.manifests.contains(manifest) && resolution.tomlDigests.keys.exists { case (id, _) => id == clerk }
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Give error for missing dependency") {
    val toml = {
      """
        |[package]
        |version = "0.0.0"
        |flix = "0.0.0"
        |
        |[dependencies]
        |"github:flix/does-not-exist" = { version = "1.0.0", mount = "missing" }
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
          |version = "0.0.0"
          |flix = "0.0.0"
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
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[dependencies]
          |"github:flix/museum" = "3.0.1"
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
          l.packages.exists(_.path.endsWith(s"flix${s}museum${s}3.0.1${s}museum-3.0.1.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-clerk${s}2.1.2${s}museum-clerk-2.1.2.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-entrance${s}2.0.2${s}museum-entrance-2.0.2.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-giftshop${s}2.0.2${s}museum-giftshop-2.0.2.fpkg")) &&
            l.packages.exists(_.path.endsWith(s"flix${s}museum-restaurant${s}2.0.2${s}museum-restaurant-2.0.2.fpkg"))
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

  test("resolveSecurityLevels.cycle.01") {
    // Two packages that each require the other. The level the project declares `left` with
    // reaches `right` around the cycle, whatever `left` declares `right` with.
    val origin = mkManifest("origin", """"github:flix/left" = { version = "1.0.0", security = "plain" }""")
    val l = mkManifest("left", """"github:flix/right" = { version = "1.0.0", security = "unrestricted" }""")
    val r = mkManifest("right", """"github:flix/left" = { version = "1.0.0", security = "unrestricted" }""")

    val List(toLeft) = FlixPackageManager.findFlixDependencies(origin)
    val List(toRight) = FlixPackageManager.findFlixDependencies(l)
    val List(backToLeft) = FlixPackageManager.findFlixDependencies(r)
    val resolution = FlixPackageManager.Resolution(
      origin = origin,
      manifests = List(origin, l, r),
      immediateDependents = Map(origin -> Nil, l -> List(origin, r), r -> List(l)),
      manifestToFlixDeps = ListMap(Map(l -> List(toLeft, backToLeft), r -> List(toRight))),
      tomlDigests = Map.empty
    )

    val security = FlixPackageManager.resolveSecurityLevels(resolution).security
    assertResult(expected = SecurityContext.Plain)(actual = security(l))
    assertResult(expected = SecurityContext.Plain)(actual = security(r))
  }

  test("resolveSecurityLevels.cycle.02") {
    // A package that requires an older version of itself is its own dependent, since a
    // declaration leads to the version of the package that is built.
    val origin = mkManifest("origin", """"github:flix/older" = { version = "1.1.0", security = "paranoid" }""")
    val older = mkManifest("older", """"github:flix/older" = { version = "1.0.0", security = "unrestricted" }""")

    val List(toOlder) = FlixPackageManager.findFlixDependencies(origin)
    val List(toItself) = FlixPackageManager.findFlixDependencies(older)
    val resolution = FlixPackageManager.Resolution(
      origin = origin,
      manifests = List(origin, older),
      immediateDependents = Map(origin -> Nil, older -> List(older, origin)),
      manifestToFlixDeps = ListMap(Map(older -> List(toOlder, toItself))),
      tomlDigests = Map.empty
    )

    assertResult(expected = SecurityContext.Paranoid)(actual = FlixPackageManager.resolveSecurityLevels(resolution).security(older))
  }

  test("resolveSecurityLevels.cycle.03") {
    // A cycle that nothing restricts stays unrestricted: a level is lowered only where a
    // dependent or a declaration asks for it.
    val origin = mkManifest("origin", """"github:flix/left" = { version = "1.0.0", security = "unrestricted" }""")
    val l = mkManifest("left", """"github:flix/right" = { version = "1.0.0", security = "unrestricted" }""")
    val r = mkManifest("right", """"github:flix/left" = { version = "1.0.0", security = "unrestricted" }""")

    val List(toLeft) = FlixPackageManager.findFlixDependencies(origin)
    val List(toRight) = FlixPackageManager.findFlixDependencies(l)
    val List(backToLeft) = FlixPackageManager.findFlixDependencies(r)
    val resolution = FlixPackageManager.Resolution(
      origin = origin,
      manifests = List(origin, l, r),
      immediateDependents = Map(origin -> Nil, l -> List(origin, r), r -> List(l)),
      manifestToFlixDeps = ListMap(Map(l -> List(toLeft, backToLeft), r -> List(toRight))),
      tomlDigests = Map.empty
    )

    val security = FlixPackageManager.resolveSecurityLevels(resolution).security
    assertResult(expected = SecurityContext.Unrestricted)(actual = security(l))
    assertResult(expected = SecurityContext.Unrestricted)(actual = security(r))
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

  test("builtVersions.01") {
    // A package is built at the version of the manifest its declarations resolve to, which here
    // is greater than the version that is declared.
    val origin = mkManifest("origin", """"github:flix/museum-clerk" = { version = "1.0.0", mount = "clerk" }""")
    val clerk = mkManifest("museum-clerk", "").copy(version = SemVer(1, 1, 0))
    val resolution = FlixPackageManager.SecureResolution(
      origin = origin,
      security = Map(origin -> SecurityContext.Unrestricted, clerk -> SecurityContext.Plain),
      manifestToFlixDeps = ListMap(Map(clerk -> FlixPackageManager.findFlixDependencies(origin))),
      tomlDigests = Map.empty
    )
    assertResult(expected = Map(PackageId(Repository.GitHub, "flix", "museum-clerk") -> SemVer(1, 1, 0)))(
      actual = FlixPackageManager.builtVersions(resolution)
    )
  }

  test("findAvailableUpdates.01") {
    // museum-clerk has a release newer than 1.0.0 in the same major version.
    val clerk = PackageId(Repository.GitHub, "flix", "museum-clerk")
    FlixPackageManager.findAvailableUpdates(clerk, SemVer(1, 0, 0), PkgTestUtils.gitHubToken) match {
      case Ok(updates) => assert(updates.minor.exists(_ >= SemVer(1, 1, 0)))
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("findAvailableUpdates.02") {
    // Nothing is newer than a version that is greater than every release.
    val clerk = PackageId(Repository.GitHub, "flix", "museum-clerk")
    FlixPackageManager.findAvailableUpdates(clerk, SemVer(999, 0, 0), PkgTestUtils.gitHubToken) match {
      case Ok(updates) => assert(updates.isEmpty)
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("mismatched-versions") {
    // A release that declares a version other than the one it is published as is refused, and is
    // named by the version it is published as and the one it declares.
    //
    // The manifest is put in the cache rather than downloaded. A manifest is validated as it is
    // installed, and a file that is already there is installed by being found, so the mistake is
    // reached the same way whether it was fetched now or before -- and no release has to be
    // published carrying it.
    val id = PackageId(Repository.GitHub, "flix", "museum-clerk")
    val released = SemVer(2, 1, 2)
    val declared = SemVer(2, 1, 3)

    val path = Files.createTempDirectory("")
    val dir = Bootstrap.getLibraryDirectory(path)
      .resolve("github").resolve(id.owner).resolve(id.name).resolve(released.toString)
    Files.createDirectories(dir)
    Files.writeString(dir.resolve(s"${id.name}-$released.${Bootstrap.EXT_TOML}"),
      s"""
         |[package]
         |version = "$declared"
         |repository = "$id"
         |flix = "${Version.CurrentVersion}"
         |""".stripMargin)

    val toml = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"$id" = { version = "$released", mount = "clerk" }
         |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock) match {
      case Ok(_) => fail("expected error, got success")
      case Err(e: PackageError.MismatchedVersions) =>
        assert(e.identifier == id)
        assert(e.release == released)
        assert(e.declared == declared)
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("mkIncompatibleVersions.01") {
    // The requirements are ordered by version, and then by dependent.
    val beta = mkManifest("beta", """"github:flix/museum-clerk" = { version = "2.0.0", mount = "clerk" }""")
    val alpha = mkManifest("alpha", """"github:flix/museum-clerk" = { version = "2.0.0", mount = "clerk" }""")
    val gamma = mkManifest("gamma", """"github:flix/museum-clerk" = { version = "1.1.0", mount = "clerk" }""")
    val id = PackageId(Repository.GitHub, "flix", "museum-clerk")
    val requirements = List(beta, alpha, gamma).map(m => (m, FlixPackageManager.findFlixDependencies(m).head))
    val error = FlixPackageManager.mkIncompatibleVersions(id, requirements)
    assertResult(expected = id)(actual = error.identifier)
    assertResult(expected = List(("flix/gamma", SemVer(1, 1, 0)), ("flix/alpha", SemVer(2, 0, 0)), ("flix/beta", SemVer(2, 0, 0))))(
      actual = error.requirements.map { case (dependent, dep) => (dependent.displayName, dep.version) }
    )
  }

  test("resolve.raise.01") {
    // museum 3.0.2 requires museum-clerk 2.1.3, and its museum-entrance and museum-giftshop
    // require museum-clerk 2.1.2, so museum-clerk is built at 2.1.3, and at no other version.
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:flix/museum" = "3.0.2"
        |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, ManifestPath) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val path = Files.createTempDirectory("")
    FlixPackageManager.resolve(manifest, path, PkgTestUtils.gitHubToken, PkgTestUtils.NoLock).map(FlixPackageManager.resolveSecurityLevels) match {
      case Ok(resolution) =>
        val clerk = PackageId(Repository.GitHub, "flix", "museum-clerk")
        assertResult(expected = Some(SemVer(2, 1, 3)))(
          actual = FlixPackageManager.builtVersions(resolution).get(clerk)
        )
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("resolve.raise.02") {
    // What is installed and locked is the version that was selected, not a version that was declared.
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:flix/museum" = "3.0.2"
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
        assertResult(expected = Some(true))(actual = installation.lockfile.packages.get((clerk, SemVer(2, 1, 3))).map(_.fpkg.isDefined))
        assertResult(expected = Some(false))(actual = installation.lockfile.packages.get((clerk, SemVer(2, 1, 2))).map(_.fpkg.isDefined))
        assertResult(expected = List(s"museum-clerk-2.1.3.fpkg"))(
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
         |version = "0.1.0"
         |repository = "github:flix/$name"
         |flix = "$flix"
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
