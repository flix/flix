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

import java.net.URL

sealed trait FDependency

object FDependency {

  case class FlixDependency(repo: Repository, username: String, projectName: String, version: SemVer, permissions: List[Permission]) extends FDependency

  case class MavenDependency(groupId: String, artifactId: String, versionTag: String) extends FDependency

  case class JarDependency(url: URL, fileName: String) extends FDependency

}
