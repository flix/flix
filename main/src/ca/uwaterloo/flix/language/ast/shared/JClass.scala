/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import java.lang.constant.ClassDesc

/**
  * A nominal reference to the Java class or interface `desc`.
  *
  * `isInterface` holds whether `desc` is an interface; the JVM needs the distinction to
  * decide between extending a superclass and implementing an interface.
  *
  * Unlike [[Class]], a [[JClass]] does not retain a loaded class.
  */
case class JClass(desc: ClassDesc, isInterface: Boolean)
