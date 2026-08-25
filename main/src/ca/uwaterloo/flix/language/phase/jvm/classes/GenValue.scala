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

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, StaticConstructorMethod, StaticField, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassConstants, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_Object, CD_boolean, CD_byte, CD_char, CD_double, CD_float, CD_int, CD_long, CD_short}

/**
  * The `Value` class, a [[GenResult]] holding a finished computation's value.
  *
  * It has one field per erased type, of which only the one matching the value's type is
  * used, plus cached singletons for `Unit`, `true`, and `false`.
  */
object GenValue {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Value"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.desc, IsFinal, interfaces = List(GenResult.desc))

    // The fields of all erased types, only one will be relevant
    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(BoolField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(CharField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(Int8Field, IsPublic, NotFinal, NotVolatile)
    cm.mkField(Int16Field, IsPublic, NotFinal, NotVolatile)
    cm.mkField(Int32Field, IsPublic, NotFinal, NotVolatile)
    cm.mkField(Int64Field, IsPublic, NotFinal, NotVolatile)
    cm.mkField(Float32Field, IsPublic, NotFinal, NotVolatile)
    cm.mkField(Float64Field, IsPublic, NotFinal, NotVolatile)
    cm.mkField(ObjectField, IsPublic, NotFinal, NotVolatile)

    // Cached singleton Value instances for Unit, true, and false
    cm.mkField(UnitField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(TrueField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(FalseField, IsPublic, IsFinal, NotVolatile)
    cm.mkStaticConstructor(StaticConstructorMethod(this.desc), staticConstructorIns(_))

    cm.closeClassMaker()
  }

  private def staticConstructorIns(implicit mv: MethodVisitor): Unit = {
    // Value.UNIT = new Value(); Value.UNIT.o = Unit.INSTANCE
    NEW(this.desc)
    DUP()
    INVOKESPECIAL(Constructor)
    DUP()
    GETSTATIC(GenUnit.SingletonField)
    PUTFIELD(ObjectField)
    PUTSTATIC(UnitField)
    // Value.TRUE = new Value(); Value.TRUE.b = true
    NEW(this.desc)
    DUP()
    INVOKESPECIAL(Constructor)
    DUP()
    ICONST_1()
    PUTFIELD(BoolField)
    PUTSTATIC(TrueField)
    // Value.FALSE = new Value(); Value.FALSE.b = false (default, but explicit)
    NEW(this.desc)
    DUP()
    INVOKESPECIAL(Constructor)
    PUTSTATIC(FalseField)
    RETURN()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  private def BoolField: InstanceField = InstanceField(this.desc, "b", CD_boolean)

  private def CharField: InstanceField = InstanceField(this.desc, "c", CD_char)

  private def Int8Field: InstanceField = InstanceField(this.desc, "i8", CD_byte)

  private def Int16Field: InstanceField = InstanceField(this.desc, "i16", CD_short)

  private def Int32Field: InstanceField = InstanceField(this.desc, "i32", CD_int)

  private def Int64Field: InstanceField = InstanceField(this.desc, "i64", CD_long)

  private def Float32Field: InstanceField = InstanceField(this.desc, "f32", CD_float)

  private def Float64Field: InstanceField = InstanceField(this.desc, "f64", CD_double)

  private def ObjectField: InstanceField = InstanceField(this.desc, "o", CD_Object)

  def UnitField: StaticField = StaticField(this.desc, "UNIT", this.desc)

  def TrueField: StaticField = StaticField(this.desc, "TRUE", this.desc)

  def FalseField: StaticField = StaticField(this.desc, "FALSE", this.desc)

  /**
    * Returns the field of Value that holds a value of type `tpe`.
    *
    * `tpe` need not be erased: every reference type is held in [[ObjectField]].
    */
  def fieldFromType(tpe: ClassDesc): InstanceField = tpe match {
    case CD_boolean => BoolField
    case CD_char => CharField
    case CD_byte => Int8Field
    case CD_short => Int16Field
    case CD_int => Int32Field
    case CD_long => Int64Field
    case CD_float => Float32Field
    case CD_double => Float64Field
    case _ => ObjectField
  }

}
