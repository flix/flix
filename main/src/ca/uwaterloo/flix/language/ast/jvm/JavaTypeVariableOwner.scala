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
package ca.uwaterloo.flix.language.ast.jvm

import java.lang.constant.ClassDesc

/** Identifies the declaration that owns a Java type variable. */
sealed trait JavaTypeVariableOwner

object JavaTypeVariableOwner {
  case class Class(owner: ClassDesc) extends JavaTypeVariableOwner

  case class Method(owner: JavaMethodRef) extends JavaTypeVariableOwner

  /**
    * An owner that could not be recovered from the class-file metadata.
    *
    * This is a fallback for class-file parsers that expose a symbolic type variable without its declaring class or
    * method. It should not be used when a declaring source is available because same-named variables with unknown
    * owners compare equal.
    */
  case object Unknown extends JavaTypeVariableOwner
}
