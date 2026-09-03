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
package ca.uwaterloo.flix.language.jvm

import ca.uwaterloo.flix.language.ast.jvm.{JavaClass, JavaMethod}
import ca.uwaterloo.flix.util.Result

import java.lang.constant.ClassDesc

/**
  * Provides the Java type metadata needed by the frontend without loading classes.
  *
  * The interface deliberately contains no Byte Buddy types. A future TypeReduction adapter can therefore depend on
  * this interface and the descriptor-based data without depending on a particular class-file parser.
  */
trait JavaTypeProvider extends AutoCloseable {

  /** Returns `Ok` with metadata for `desc`, or `Err` if the descriptor cannot be looked up. */
  def lookupClass(desc: ClassDesc): Result[JavaClass, JavaLookupError]

  /** Returns `Ok` with the virtual method graph for `desc`, or `Err` if the descriptor cannot be looked up. */
  def virtualMethods(desc: ClassDesc): Result[List[JavaMethod], JavaLookupError]

  /** Returns `Ok` with the subtype result, or `Err` if either descriptor cannot be looked up. */
  def isSubtype(subtype: ClassDesc, supertype: ClassDesc): Result[Boolean, JavaLookupError]

}
