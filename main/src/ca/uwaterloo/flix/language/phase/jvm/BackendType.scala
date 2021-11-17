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

/**
  * Represents all Flix types that are not object on the JVM including Void.
  */
sealed trait VoidableType {
  def toDescriptor: String
}

object VoidableType {
  case object Void extends VoidableType {
    override val toDescriptor: String = "V"

    override def toString: String = "Void"
  }
}

/**
  * Represents all Flix types that are not objects on the JVM (array is an exception).
  */
sealed trait BackendType extends VoidableType {
  def toDescriptor: String

  def toErased: BackendType

  /**
    * A string representing the erased type. This is used for parametrized class names.
    */
  override def toString: String = "BackendType:MissingImpl"
}

object BackendType {
  case object Bool extends BackendType {
    override val toDescriptor: String = "Z"

    override def toErased: BackendType = this

    override def toString: String = "Bool"
  }

  case object Char extends BackendType {
    override val toDescriptor: String = "C"

    override def toErased: BackendType = this

    override def toString: String = "Char"
  }

  case object Int8 extends BackendType {
    override val toDescriptor: String = "B"

    override def toErased: BackendType = this

    override def toString: String = "Int8"
  }

  case object Int16 extends BackendType {
    override val toDescriptor: String = "S"

    override def toErased: BackendType = this

    override def toString: String = "Int16"
  }

  case object Int32 extends BackendType {
    override val toDescriptor: String = "I"

    override def toErased: BackendType = this

    override def toString: String = "Int32"
  }

  case object Int64 extends BackendType {
    override val toDescriptor: String = "J"

    override def toErased: BackendType = this

    override def toString: String = "Int64"
  }

  case object Float32 extends BackendType {
    override val toDescriptor: String = "F"

    override def toErased: BackendType = this

    override def toString: String = "Float32"
  }

  case object Float64 extends BackendType {
    override val toDescriptor: String = "D"

    override def toErased: BackendType = this

    override def toString: String = "Float64"
  }

  case class Array(tpe: BackendType) extends BackendType {
    override def toDescriptor: String = s"[${tpe.toDescriptor}"

    override def toErased: BackendType = JvmName.Object.toObjTpe.toTpe

    override def toString: String = "Obj"
  }

  /**
    * Holds a reference to some object type.
    */
  case class Reference(ref: BackendObjType) extends BackendType {
    override val toDescriptor: String = ref.toDescriptor

    def name: JvmName = ref.jvmName

    override def toErased: BackendType = JvmName.Object.toObjTpe.toTpe

    override def toString: String = "Obj"
  }

  /**
    * Contains all the primitive types and `Reference(Native(JvmName.Object))`.
    */
  def erasedTypes: List[BackendType] =
    Bool :: Char :: Float32 :: Float64 :: Int8 :: Int16 :: Int32 :: Int64 :: JvmName.Object.toObjTpe.toTpe :: Nil
}
