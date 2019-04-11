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
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.JvmName
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsClass, MnemonicsGenerator}
import ca.uwaterloo.flix.language.phase.njvm.classes.RecordExtend

/**
  * Generates bytecode for the extended record class.
  */
object GenRecordExtend extends MnemonicsGenerator {

  def gen(map: Map[JvmName, MnemonicsClass], ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {

    map + (
      new RecordExtend[NJvmType.PrimBool](map).getClassMapping,
      new RecordExtend[NJvmType.PrimChar](map).getClassMapping,
      new RecordExtend[NJvmType.PrimFloat](map).getClassMapping,
      new RecordExtend[NJvmType.PrimDouble](map).getClassMapping,
      new RecordExtend[NJvmType.PrimByte](map).getClassMapping,
      new RecordExtend[NJvmType.PrimShort](map).getClassMapping,
      new RecordExtend[NJvmType.PrimInt](map).getClassMapping,
      new RecordExtend[NJvmType.PrimLong](map).getClassMapping,
      new RecordExtend[NJvmType.Object.type](map).getClassMapping)
  }
}
