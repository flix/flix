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

import ca.uwaterloo.flix.language.ast.MonoType

import scala.annotation.tailrec

/**
  * Represents all Flix types that are not object on the JVM including Void.
  */
sealed trait VoidableType {
  /**
    * Returns a descriptor for the type. `Void` has descriptor `"V"`.
    */
  def toDescriptor: String
}

object VoidableType {
  case object Void extends VoidableType {
    override val toDescriptor: String = "V"

    /**
      * The erased string representation used in JVM names.
      */
    val toErasedString: String = "Void"
  }
}

/**
  * Represents all Flix types that are not objects on the JVM (array is an exception).
  */
sealed trait BackendType extends VoidableType {
  def toDescriptor: String = this match {
    case BackendType.Primitive(tpe) => tpe.toDescriptor
    case BackendType.Array(tpe) => s"[${tpe.toDescriptor}"
    case BackendType.Reference(ref) => ref.toDescriptor
  }

  /**
    * Returns the erased type, either itself if `this` is primitive or `java.lang.Object`
    * if `this` is an array or a reference.
    *
    * @return
    */
  def toErased: BackendType = this match {
    case BackendType.Primitive(_) => this
    case BackendType.Array(_) | BackendType.Reference(_) => BackendObjType.JavaObject.toTpe
  }

  /**
    * A string representing the erased type. This is used for parametrized class names.
    */
  val toErasedString: String = this match {
    case BackendType.Primitive(tpe) => tpe.toErasedString
    case BackendType.Array(_) | BackendType.Reference(_) => "Obj"
  }

  def is64BitWidth: Boolean = this match {
    case BackendType.Primitive(PrimitiveType.Int64) => true
    case BackendType.Primitive(PrimitiveType.Float64) => true
    case BackendType.Primitive(_) | BackendType.Array(_) |
         BackendType.Reference(_) => false
  }

  /**
    * Returns the Array fill type for the value of the type specified by `tpe`
    */
  def toArrayFillType: String = s"([${this.toErased.toDescriptor}${this.toErased.toDescriptor})V"

}

object BackendType {

  case class Primitive(tpe: PrimitiveType) extends BackendType

  case class Array(tpe: BackendType) extends BackendType {
    override def toDescriptor: String = {

      @tailrec
      def visit(t: BackendType, acc: String): String = t match {
        case Primitive(_) => acc + t.toDescriptor
        case Reference(_) => acc + t.toDescriptor
        case Array(tt) => visit(tt, acc + "[")
      }

      visit(tpe, "[")
    }
  }

  /**
    * Holds a reference to some object type.
    */
  case class Reference(ref: BackendObjType) extends BackendType {
    def name: JvmName = ref.jvmName
  }

  /**
    * Contains all the primitive types and `Reference(Native(JvmName.Object))`.
    */
  def erasedTypes: List[BackendType] =
    Primitive(PrimitiveType.Bool) :: Primitive(PrimitiveType.Char) :: Primitive(PrimitiveType.Float32) :: Primitive(PrimitiveType.Float64) :: Primitive(PrimitiveType.Int8) :: Primitive(PrimitiveType.Int16) :: Primitive(PrimitiveType.Int32) :: Primitive(PrimitiveType.Int64) :: BackendObjType.JavaObject.toTpe :: Nil

  /**
    * Computes the erased `BackendType` based on the given `MonoType`.
    */
  def toErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => Primitive(PrimitiveType.Bool)
    case MonoType.Char => Primitive(PrimitiveType.Char)
    case MonoType.Float32 => Primitive(PrimitiveType.Float32)
    case MonoType.Float64 => Primitive(PrimitiveType.Float64)
    case MonoType.Int8 => Primitive(PrimitiveType.Int8)
    case MonoType.Int16 => Primitive(PrimitiveType.Int16)
    case MonoType.Int32 => Primitive(PrimitiveType.Int32)
    case MonoType.Int64 => Primitive(PrimitiveType.Int64)
    case MonoType.Unit | MonoType.BigDecimal | MonoType.BigInt | MonoType.String | MonoType.Regex |
         MonoType.Array(_) | MonoType.Lazy(_) | MonoType.Ref(_) | MonoType.Tuple(_) |
         MonoType.Enum(_) | MonoType.Arrow(_, _) | MonoType.RecordEmpty | MonoType.RecordExtend(_, _, _) |
         MonoType.SchemaEmpty | MonoType.SchemaExtend(_, _, _) | MonoType.Native(_) |
         MonoType.Region => BackendObjType.JavaObject.toTpe
  }

  /**
    * Computes the `BackendType` based on the given `MonoType`.
    */
  def toFlixErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => Primitive(PrimitiveType.Bool)
    case MonoType.Char => Primitive(PrimitiveType.Char)
    case MonoType.Int8 => Primitive(PrimitiveType.Int8)
    case MonoType.Int16 => Primitive(PrimitiveType.Int16)
    case MonoType.Int32 => Primitive(PrimitiveType.Int32)
    case MonoType.Int64 => Primitive(PrimitiveType.Int64)
    case MonoType.Float32 => Primitive(PrimitiveType.Float32)
    case MonoType.Float64 => Primitive(PrimitiveType.Float64)
    case MonoType.Array(t) => Array(toFlixErasedBackendType(t))
    case MonoType.BigDecimal => BackendObjType.BigDecimal.toTpe
    case MonoType.BigInt => BackendObjType.BigInt.toTpe
    case MonoType.String => BackendObjType.String.toTpe
    case MonoType.Regex => BackendObjType.Regex.toTpe
    case MonoType.Native(clazz) =>
      // Maybe use clazz.getPackage and clazz.getSimpleName
      // TODO: Ugly hack.
      val fqn = clazz.getName.replace('.', '/')
      BackendObjType.Native(JvmName.mk(fqn)).toTpe
    case MonoType.Unit | MonoType.Lazy(_) | MonoType.Ref(_) |
         MonoType.Tuple(_) | MonoType.Arrow(_, _) | MonoType.RecordEmpty |
         MonoType.RecordExtend(_, _, _) | MonoType.Region | MonoType.Enum(_) |
         MonoType.SchemaEmpty | MonoType.SchemaExtend(_, _, _) => BackendObjType.JavaObject.toTpe
  }
}
