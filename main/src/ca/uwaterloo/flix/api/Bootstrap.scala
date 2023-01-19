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
package ca.uwaterloo.flix.api

object Bootstrap {

  // Given a working dir p.
  // 1. Does `p/flix.toml` exist?
  // 1.1 If yes, the enter project mode.
  // 1.2 Otherwise enter folder mode.


  sealed trait Mode

  object Mode {
    case object Project extends Mode
    case object Directory extends Mode
  }


}
