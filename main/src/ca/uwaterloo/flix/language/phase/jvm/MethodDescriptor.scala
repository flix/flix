/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm

import org.objectweb.asm

/**
  * Companion object for the [[MethodDescriptor]] class.
  */
object MethodDescriptor {
  val NothingToVoid: MethodDescriptor = MethodDescriptor(Nil, VoidableType.Void)

  def mkDescriptor(argument: BackendType*)(result: VoidableType): MethodDescriptor =
    MethodDescriptor(argument.toList, result)
}

/**
  * Represents the type descriptor of a JVM method.
  */
// TODO: Would be nice to allow BackendObjType here to avoid conversions
case class MethodDescriptor(arguments: List[BackendType], result: VoidableType) {
  /**
    * Returns the type descriptor of this method.
    */
  val toDescriptor: String = {
    // Descriptor of result
    val resultDescriptor = result.toDescriptor

    // Descriptor of arguments
    val argumentDescriptor = arguments.map(_.toDescriptor).mkString

    // Descriptor of the method
    s"($argumentDescriptor)$resultDescriptor"
  }

  def toAsmType: asm.Type = asm.Type.getType(toDescriptor)
}
