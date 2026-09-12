/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm

import java.lang.constant.{ClassDesc, ConstantDescs, MethodTypeDesc}

/**
  * Helpers for constructing [[MethodTypeDesc]] from [[ClassDesc]].
  */
object MethodTypeDescs {

  /** The descriptor `()V` of a method that takes no arguments and returns void. */
  val NothingToVoid: MethodTypeDesc = MethodTypeDesc.of(ConstantDescs.CD_void)

  /** Returns the [[MethodTypeDesc]] of a method that takes `argument`s and returns `result`. */
  def mkDescriptor(argument: ClassDesc*)(result: ClassDesc): MethodTypeDesc =
    MethodTypeDesc.of(result, argument *)

  /** Returns the [[MethodTypeDesc]] of a method that takes `argument`s and returns void. */
  def mkVoidDescriptor(argument: ClassDesc*): MethodTypeDesc =
    MethodTypeDesc.of(ConstantDescs.CD_void, argument *)

}
