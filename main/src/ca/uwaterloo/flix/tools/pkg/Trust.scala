/*
 * Copyright 2024 Andreas Stenbæk Larsen
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
  * Permissions for dependencies.
  */
object Trust {

  case object PlainFlix extends Trust {
    override def toString: String = "plain"
  }

  case object TrustJavaClass extends Trust {
    override def toString: String = "trust-javaclass"
  }

  case object Unrestricted extends Trust {
    override def toString: String = "unrestricted"
  }

  def fromString(s: String): Option[Trust] = s.toLowerCase match {
    case "plain" => Some(PlainFlix)
    case "trust-javaclass" => Some(TrustJavaClass)
    case "unrestricted" => Some(Unrestricted)
    case _ => None
  }
}
