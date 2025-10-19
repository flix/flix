/*
 * Copyright 2024 Andreas Stenbæk Larsen
 * Copyright 2025 Jakob Schneider Villumsen
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

sealed trait Trust

/**
  * Trust for dependencies.
  */
object Trust {

  /**
    * Plain Flix must not have any unsafe features.
    *   1. No unsafe casts
    *   1. No Java interop
    */
  case object Plain extends Trust {
    override def toString: String = "plain"
  }

  /**
    * May use unchecked casts and Java interop.
    */
  case object Unrestricted extends Trust {
    override def toString: String = "unrestricted"
  }

  def fromString(s: String): Option[Trust] = s match {
    case "plain" => Some(Plain)
    case "unrestricted" => Some(Unrestricted)
    case _ => None
  }
}
