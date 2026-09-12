/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
