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
import ca.uwaterloo.flix.language.phase.njvm.classes.RefClass

/**
  * Generates bytecode for the ref classes.
  */

object GenRefClasses extends MnemonicsGenerator {

  def gen(map: Map[JvmName, MnemonicsClass], ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
    map + (
      new RefClass[NJvmType.PrimBool].getClassMapping,
      new RefClass[NJvmType.PrimChar].getClassMapping,
      new RefClass[NJvmType.PrimFloat].getClassMapping,
      new RefClass[NJvmType.PrimDouble].getClassMapping,
      new RefClass[NJvmType.PrimByte].getClassMapping,
      new RefClass[NJvmType.PrimShort].getClassMapping,
      new RefClass[NJvmType.PrimInt].getClassMapping,
      new RefClass[NJvmType.PrimLong].getClassMapping,
      new RefClass[NJvmType.Object.type].getClassMapping)
  }

}

