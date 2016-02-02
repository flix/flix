package ca.uwaterloo.flix.util

object Version {
  val currentVersion = Version(major = 0, minor = 1, revision = 0)
}

case class Version(major: Int, minor: Int, revision: Int) {
  override val toString: String = s"v$major.$minor.$revision"
}
