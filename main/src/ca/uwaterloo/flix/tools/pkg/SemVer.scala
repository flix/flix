/*
 * Copyright 2023 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.tools.pkg

object SemVer {

  /**
    * An ordering on Semantic Versions
    */
  implicit def semVerOrdering: Ordering[SemVer] =
    Ordering.by((_: SemVer).major)
      .orElseBy(_.minor)
      .orElseBy(_.patch)

}

/**
  * A semantic version number.
  */
case class SemVer(major: Int, minor: Int, patch: Option[Int], buildDot: Option[Int], buildDash: Option[String]) {
  override def toString: String = patch match {
    case None => s"$major.$minor"
    case Some(patch) => buildDot match {
      case None => buildDash match {
        case Some(build) => s"$major.$minor.$patch-$build"
        case None => s"$major.$minor.$patch"
      }
      case Some(build) => s"$major.$minor.$patch.$build"
    }

  }


}
