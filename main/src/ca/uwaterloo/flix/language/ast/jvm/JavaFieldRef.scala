/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

import java.lang.constant.ClassDesc

/** A nominal, descriptor-based reference to a Java field. */
case class JavaFieldRef(owner: ClassDesc, name: String, descriptor: ClassDesc)
