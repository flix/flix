/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.jvm

import java.lang.constant.ClassDesc

/** An error encountered while looking up Java class-file metadata. */
sealed trait JavaLookupError {
  def desc: ClassDesc

  /** Returns a sentence that explains this error to the user. */
  def explanation: String = this match {
    case JavaLookupError.InvalidClass(desc, message) => s"The class file of '${ClassDescs.binaryNameOf(desc)}' could not be read: $message"
    case JavaLookupError.MissingClass(desc) => s"The class '${ClassDescs.binaryNameOf(desc)}' was not found on the class path."
    case JavaLookupError.UnsupportedDescriptor(desc) => s"'${ClassDescs.binaryNameOf(desc)}' does not denote a class or interface."
  }
}

object JavaLookupError {

  /** The class file was found, but its metadata could not be read. */
  case class InvalidClass(desc: ClassDesc, message: String) extends JavaLookupError

  /** The class file for `desc` was not present in the configured class path. */
  case class MissingClass(desc: ClassDesc) extends JavaLookupError

  /** `desc` does not denote a nominal reference type. */
  case class UnsupportedDescriptor(desc: ClassDesc) extends JavaLookupError

}
