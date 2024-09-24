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

sealed trait Permission

/** Permissions for dependencies. */
object Permission {

  /** Permission to have an effect. */
  case object Effect extends Permission {
    override def toString: String = "effect"
  }

  /** Permission to call Java libraries. */
  case object JavaInterop extends Permission {
    override def toString: String = "java-interop"
  }

  /** Permission to use unchecked casts. */
  case object UncheckedCast extends Permission {
    override def toString: String = "unchecked-cast"
  }

  def mkPermission(s: String): Option[Permission] = s match {
    case "java-interop" => Some(JavaInterop)
    case "unchecked-cast" => Some(UncheckedCast)
    case "effect" => Some(Effect)
    case _ => None
  }
}
