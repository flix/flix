/*
 * Copyright 2019 Miguel Fialho
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

package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.classes.RecordExtend

/**
  * Generates bytecode for the extended record class.
  */
object GenRecordExtend {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(
      new RecordExtend[JvmType.PrimBool].genClass,
      new RecordExtend[JvmType.PrimChar].genClass,
      new RecordExtend[JvmType.PrimFloat].genClass,
      new RecordExtend[JvmType.PrimDouble].genClass,
      new RecordExtend[JvmType.PrimByte].genClass,
      new RecordExtend[JvmType.PrimShort].genClass,
      new RecordExtend[JvmType.PrimInt].genClass,
      new RecordExtend[JvmType.PrimLong].genClass,
      new RecordExtend[JvmType.Object.type].genClass)
  }
}
