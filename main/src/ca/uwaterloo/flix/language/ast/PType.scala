/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.ast

sealed trait PType

// only exist at scala compile time, to help compiler writers
object PType {

  sealed trait PInt8 extends PType

  sealed trait PInt16 extends PType

  sealed trait PInt32 extends PType

  sealed trait PInt64 extends PType

  sealed trait PChar extends PType

  sealed trait PFloat32 extends PType

  sealed trait PFloat64 extends PType

  sealed trait PReference[T <: PRefType] extends PType

}

sealed trait PRefType

object PRefType {

  sealed trait PBoxedBool extends PRefType

  sealed trait PBoxedInt8 extends PRefType

  sealed trait PBoxedInt16 extends PRefType

  sealed trait PBoxedInt32 extends PRefType

  sealed trait PBoxedInt64 extends PRefType

  sealed trait PBoxedChar extends PRefType

  sealed trait PBoxedFloat32 extends PRefType

  sealed trait PBoxedFloat64 extends PRefType

  sealed trait PUnit extends PRefType

  sealed trait PRef[T <: PType] extends PRefType

  sealed trait PArray[T <: PType] extends PRefType

  sealed trait PChan[T <: PRefType] extends PRefType

  sealed trait PLazy[T <: PType] extends PRefType

  sealed trait PStr extends PRefType

  sealed trait PBigInt extends PRefType

  sealed trait PTuple extends PRefType

  sealed trait PAnyObject extends PRefType

  sealed trait PFunction[T <: PType] extends PRefType

}
