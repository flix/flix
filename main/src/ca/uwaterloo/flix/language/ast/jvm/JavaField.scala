/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

/** Class-file metadata for a Java field. */
case class JavaField(ref: JavaFieldRef, modifiers: Int, fieldType: JavaType) extends JavaMember
