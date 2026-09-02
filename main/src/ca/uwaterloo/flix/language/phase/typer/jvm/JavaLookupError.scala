/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.language.phase.typer.jvm

import java.lang.constant.ClassDesc

/** An error encountered while looking up Java class-file metadata. */
sealed trait JavaLookupError {
  def desc: ClassDesc

  /** Returns a human-readable explanation of this lookup failure. */
  def explanation: String
}

object JavaLookupError {

  /** The class file was found, but its metadata could not be read. */
  case class InvalidClass(desc: ClassDesc, message: String) extends JavaLookupError {
    def explanation: String = s"The class file for '${desc.displayName()}' could not be read: $message"
  }

  /** The class file for `desc` was not present in the configured class path. */
  case class MissingClass(desc: ClassDesc) extends JavaLookupError {
    def explanation: String = s"The class file for '${desc.displayName()}' was not found on the configured class path."
  }

  /** `desc` does not denote a nominal reference type. */
  case class UnsupportedDescriptor(desc: ClassDesc) extends JavaLookupError {
    def explanation: String = s"'${desc.displayName()}' is not a Java class or interface descriptor."
  }

}
