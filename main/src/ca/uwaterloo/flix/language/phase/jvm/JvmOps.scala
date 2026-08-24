/*
 * Copyright 2017 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.ast.{JvmAst, SimpleType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.Mangle.mangle
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ClassDesc

object JvmOps {

  /** Returns the index of `varOffset` combined with the context offset. */
  def getIndex(varOffset: Int, contextOffset: Int): Int =
    varOffset + contextOffset

  /**
    * Returns the erased arrow type of `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, String) -> Int   =>  Fn3$Int$Obj$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getErasedFunctionInterfaceType(tpe: SimpleType)(implicit root: Root): BackendObjType.Arrow = tpe match {
    case SimpleType.Arrow(targs, tresult) =>
      BackendObjType.Arrow(targs.map(BackendType.toErasedBackendType), BackendType.toBackendType(tresult))
    case _ =>
      throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the erased closure abstract class type `CloX$Y$Z` for the given [[SimpleType]].
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getErasedClosureAbstractClassType(tpe: SimpleType): BackendObjType.AbstractArrow = tpe match {
    case SimpleType.Arrow(targs, tresult) =>
      BackendObjType.AbstractArrow(targs.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(tresult))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassName(sym: Symbol.DefnSym): ClassDesc = {
    // The JVM name is of the form Clo$sym.name
    val name = Mangle.mkClassName(s"Clo", sym.name)

    // The JVM package is the namespace of the symbol.
    val pkg = sym.namespace

    // The result type.
    ClassDesc.ofInternalName((pkg :+ name).mkString("/"))
  }

  /**
    * Returns the effect definition class for the given symbol.
    *
    * For example:
    *
    * Print       =>  Eff$Print
    * List.Crash  =>  List.Eff$Crash
    */
  def getEffectDefinitionClassName(sym: Symbol.EffSym): ClassDesc = {
    val pkg = sym.namespace
    val name = Mangle.mkClassName("Eff", sym.name)
    ClassDesc.ofInternalName((pkg :+ name).mkString("/"))
  }

  /**
    * Returns the op name of the given symbol.
    */
  def getEffectOpName(op: Symbol.OpSym): String =
    mangle(op.name)

  def getTagName(name: String): String =
    mangle(name)

  /** Returns the struct type of `struct`. */
  def getStructType(struct: JvmAst.Struct)(implicit root: Root): BackendObjType.Struct =
    BackendObjType.Struct(struct.fields.map(field => BackendType.toBackendType(field.tpe)))

}
