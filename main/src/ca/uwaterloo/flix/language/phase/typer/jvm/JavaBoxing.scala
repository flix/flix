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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.language.ast.shared.JMethod
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ConstantDescs.*
import java.lang.constant.{ClassDesc, MethodTypeDesc}

/**
  * Boxing and unboxing of the Flix primitive types to and from their Java wrapper classes.
  *
  * The lowering phases use the same mechanism as javac: `Integer.valueOf(int)` boxes an `Int32`
  * and `Integer.intValue()` unboxes it.
  */
object JavaBoxing {

  /**
    * Returns `true` if `tpe` is a primitive type.
    *
    * N.B.: `tpe` must be normalized.
    */
  def isPrimitive(tpe: Type): Boolean = tpe match {
    case Type.Char => true
    case Type.Bool => true
    case Type.Int8 => true
    case Type.Int16 => true
    case Type.Int32 => true
    case Type.Int64 => true
    case Type.Float32 => true
    case Type.Float64 => true
    case Type.Cst(_, _) => false
    case Type.Apply(_, _, _) => false
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)
  }

  /** Returns the `valueOf` method that boxes the primitive type `tpe`, e.g. `Integer.valueOf(int)` for `Int32`. */
  def boxMethod(tpe: Type): JMethod = {
    val wrapper = wrapperOf(tpe)
    JMethod(wrapper.box, "valueOf", MethodTypeDesc.of(wrapper.box, wrapper.prim), isInterface = false)
  }

  /** Returns the method that unboxes the wrapper of the primitive type `tpe`, e.g. `Integer.intValue()` for `Int32`. */
  def unboxMethod(tpe: Type): JMethod = {
    val wrapper = wrapperOf(tpe)
    JMethod(wrapper.box, wrapper.unboxName, MethodTypeDesc.of(wrapper.prim), isInterface = false)
  }

  /** Returns the native type of the wrapper class of the primitive type `tpe`, e.g. `java.lang.Integer` for `Int32`. */
  def boxedType(tpe: Type, loc: SourceLocation): Type =
    Type.mkNative(wrapperOf(tpe).box, 0, loc)

  /** The Java wrapper class `box` of a primitive type `prim` and the name of the method that unboxes it. */
  private case class Wrapper(box: ClassDesc, prim: ClassDesc, unboxName: String)

  /** Returns the wrapper of the primitive type `tpe`, or throws if `tpe` is not a primitive type. */
  private def wrapperOf(tpe: Type): Wrapper = tpe match {
    case Type.Bool => Wrapper(CD_Boolean, CD_boolean, "booleanValue")
    case Type.Char => Wrapper(CD_Character, CD_char, "charValue")
    case Type.Int8 => Wrapper(CD_Byte, CD_byte, "byteValue")
    case Type.Int16 => Wrapper(CD_Short, CD_short, "shortValue")
    case Type.Int32 => Wrapper(CD_Integer, CD_int, "intValue")
    case Type.Int64 => Wrapper(CD_Long, CD_long, "longValue")
    case Type.Float32 => Wrapper(CD_Float, CD_float, "floatValue")
    case Type.Float64 => Wrapper(CD_Double, CD_double, "doubleValue")
    case _ => throw InternalCompilerException(s"Unexpected non-primitive type '$tpe'", tpe.loc)
  }

}
