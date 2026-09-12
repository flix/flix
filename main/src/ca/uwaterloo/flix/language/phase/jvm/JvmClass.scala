/*
 * Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm

import java.lang.constant.ClassDesc

/**
  * Represents a Java class (or interface).
  *
  * @param name     the descriptor of the class (or interface).
  * @param bytecode the bytecode of the class (or interface).
  */
case class JvmClass(name: ClassDesc, bytecode: Array[Byte]) {
  /**
    * Returns the hashCode of `this` JvmClass.
    */
  override def hashCode(): Int = name.hashCode()

  /**
    * Returns `true` if `obj` is a JvmClass with the same name.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: JvmClass => this.name == that.name
    case _ => false
  }

  /**
    * Returns a string representation of `this` JvmClass.
    */
  override def toString: String = s"JvmClass($name, ${bytecode.length} bytes)"
}
