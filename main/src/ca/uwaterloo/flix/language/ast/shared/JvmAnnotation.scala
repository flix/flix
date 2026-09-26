/*
 * Copyright 2025 Flix authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.SourceLocation

import java.lang.constant.ClassDesc

/**
  * Represents a resolved JVM annotation (after name resolution).
  * Used from ResolvedAst through JvmAst.
  *
  * `isRuntimeVisible` holds whether the annotation has runtime retention. It is computed
  * during resolution, where the annotation class is loaded, so the backend needs no reflection.
  */
case class JvmAnnotation(clazz: ClassDesc, isRuntimeVisible: Boolean, loc: SourceLocation)
