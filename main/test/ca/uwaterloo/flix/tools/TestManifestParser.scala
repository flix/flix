package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{Dependency, DependencyKind, Manifest, ManifestError, ManifestParser, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

import java.nio.file.Paths

class TestManifestParser extends FunSuite {

  val manifest: Manifest = ManifestParser.parse("main/test/ca/uwaterloo/flix/tools/manifests/ok.toml")(System.out) match {
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

  test("Ok.license.Some") {
    assertResult(expected = Some("Apache-2.0"))(actual = manifest.license)
  }

  test("Ok.license.None") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/ok_license_none.toml"
    assertResult(expected = None)(actual =
      ManifestParser.parse(pathString)(System.out) match {
        case Ok(m) => m.license
        case Err(e) => Err(e)
      }
    )
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

  /*
  * Errors
  * */
  //File does not exist
  test("Err.file.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/missing.toml"
    assertResult(Err(ManifestError.IOError(Paths.get(pathString))))(ManifestParser.parse(pathString)(System.out))
  }

  //Name
  test("Err.name.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/name_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'package.name' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.name.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/name_type.toml"
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(Paths.get(pathString), "'package.name' should have type String")))(ManifestParser.parse(pathString)(System.out))
  }

  //Description
  test("Err.description.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/description_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'package.description' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.description.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/description_type.toml"
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(Paths.get(pathString), "'package.description' should have type String")))(ManifestParser.parse(pathString)(System.out))
  }

  //Version
  test("Err.version.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'package.version' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.version.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_type.toml"
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(Paths.get(pathString), "'package.version' should have type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.version.format.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_format_01.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.version.format.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_format_02.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.version.numbers.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_numbers_01.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.version.numbers.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_numbers_02.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.version.numbers.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/version_numbers_03.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  //Flix
  test("Err.flix.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'package.flix' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.flix.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_type.toml"
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(Paths.get(pathString), "'package.flix' should have type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.flix.format.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_format_01.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.flix.format.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_format_02.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.flix.numbers.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_numbers_01.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.flix.numbers.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_numbers_02.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.flix.numbers.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/flix_numbers_03.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  //License
  test("Err.license.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/license_type.toml"
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(Paths.get(pathString), "'package.license' should have type String")))(ManifestParser.parse(pathString)(System.out))
  }

  //Authors
  //TODO: fix authors tests
  test("Err.authors.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/authors_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'package.authors' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.authors.type.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/authors_type_01.toml"
    assertResult(Err(ManifestError.RequiredPropertyHasWrongType(Paths.get(pathString), "'package.authors' should have type Array")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.authors.type.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/authors_type_02.toml"
    assertResult(Err(ManifestError.AuthorNameError(Paths.get(pathString), "All author names should be of type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.authors.type.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/authors_type_03.toml"
    assertResult(Err(ManifestError.AuthorNameError(Paths.get(pathString), "All author names should be of type String")))(ManifestParser.parse(pathString)(System.out))
  }

  //Dependencies
  test("Err.dependencies.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'dependencies' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dependencies.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_type.toml"
    assertResult(Err(ManifestError.DependencyFormatError(Paths.get(pathString), "A value in a dependency table should be of type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dependencies.format.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_format_01.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dependencies.format.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_format_02.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dependencies.numbers.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_numbers_01.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dependencies.numbers.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_numbers_02.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dependencies.numbers.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dependencies_numbers_03.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  //Dev-dependencies
  test("Err.dev-dependencies.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'dev-dependencies' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-dependencies.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_type.toml"
    assertResult(Err(ManifestError.DependencyFormatError(Paths.get(pathString), "A value in a dependency table should be of type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-dependencies.format.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_format_01.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-dependencies.format.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_format_02.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-dependencies.numbers.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_numbers_01.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-dependencies.numbers.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_numbers_02.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-dependencies.numbers.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_dependencies_numbers_03.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  //Mvn-dependencies
  test("Err.mvn-dependencies.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'mvn-dependencies' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_type.toml"
    assertResult(Err(ManifestError.DependencyFormatError(Paths.get(pathString), "A value in a dependency table should be of type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.format.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_format_01.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.format.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_format_02.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.format.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_format_03.toml"
    assertResult(Err(ManifestError.MavenDependencyFormatError(Paths.get(pathString), "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.format.04") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_format_04.toml"
    assertResult(Err(ManifestError.MavenDependencyFormatError(Paths.get(pathString), "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.numbers.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_numbers_01.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.numbers.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_numbers_02.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.mvn-dependencies.numbers.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/mvn_dependencies_numbers_03.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  //Dev-mvn-dependencies
  test("Err.dev-mvn-dependencies.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_missing.toml"
    assertResult(Err(ManifestError.MissingRequiredProperty(Paths.get(pathString), "'dev-mvn-dependencies' is missing")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.type") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_type.toml"
    assertResult(Err(ManifestError.DependencyFormatError(Paths.get(pathString), "A value in a dependency table should be of type String")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.format.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_format_01.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.format.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_format_02.toml"
    assertResult(Err(ManifestError.VersionHasWrongLength(Paths.get(pathString), "A version should be formatted like so: 'x.x.x'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.format.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_format_03.toml"
    assertResult(Err(ManifestError.MavenDependencyFormatError(Paths.get(pathString), "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.format.04") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_format_04.toml"
    assertResult(Err(ManifestError.MavenDependencyFormatError(Paths.get(pathString), "A Maven dependency should be formatted like so: 'group:artifact'")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.numbers.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_numbers_01.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.numbers.02") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_numbers_02.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

  test("Err.dev-mvn-dependencies.numbers.03") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/manifests/dev_mvn_dependencies_numbers_03.toml"
    assertResult(Err(ManifestError.VersionNumberWrong(Paths.get(pathString), "Could not parse version as three numbers")))(ManifestParser.parse(pathString)(System.out))
  }

}
