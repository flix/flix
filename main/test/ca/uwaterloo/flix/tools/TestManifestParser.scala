package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{Dependency, DependencyKind, Manifest, ManifestParser, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

class TestManifestParser extends FunSuite {

  val manifest: Manifest = ManifestParser.parse("examples/projects/project-with-deps/flix.toml")(System.out) match {
    case Ok(m) => m
    case Err(_) => /*Should never happen*/ Manifest("", "", SemVer(0,0,0), SemVer(0,0,0), None, List.empty, List.empty)
  }

  test("Ok.name") {
    assertResult(expected = "hello-world")(actual = manifest.name)
  }

  test("Ok.description") {
    assertResult(expected = "A simple program")(actual = manifest.description)
  }

  test("Ok.version") {
    assertResult(expected = SemVer(0,1,0))(actual = manifest.version)
  }

  test("Ok.flix") {
    assertResult(expected = SemVer(0, 33, 0))(actual = manifest.flix)
  }

  test("Ok.license") {
    assertResult(expected = Some("Apache-2.0"))(actual = manifest.license)
  }

  test("Ok.authors") {
    assertResult(expected = List("John Doe <john@example.com"))(actual = manifest.authors)
  }

  test("Ok.dependencies") {
    assertResult(expected = List(Dependency.FlixDependency("github:mlutze/flixball", SemVer(3,2,1), DependencyKind.Production),
                                 Dependency.FlixDependency("github:jls/tic-tac-toe", SemVer(1,2,3), DependencyKind.Production),
                                 Dependency.FlixDependency("github:fuzzer/fuzzer", SemVer(1,2,3), DependencyKind.Development),
                                 Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", SemVer(4,7,0), DependencyKind.Production),
                                 Dependency.MavenDependency("org.postgresql", "postgresql", SemVer(1,2,3), DependencyKind.Production),
                                 Dependency.MavenDependency("org.junit", "junit", SemVer(1,2,3), DependencyKind.Development)))(actual = manifest.dependencies)
  }

}
