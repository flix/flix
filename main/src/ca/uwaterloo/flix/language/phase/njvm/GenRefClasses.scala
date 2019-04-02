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
import ca.uwaterloo.flix.language.phase.njvm.classes.RefClass

/**
  * Generates bytecode for the ref classes.
  */

object GenRefClasses {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    Map(
      new RefClass[JvmType.PrimBool.type].genClass,
      new RefClass[JvmType.PrimChar.type].genClass,
      new RefClass[JvmType.PrimFloat.type].genClass,
      new RefClass[JvmType.PrimDouble.type].genClass,
      new RefClass[JvmType.PrimByte.type].genClass,
      new RefClass[JvmType.PrimShort.type].genClass,
      new RefClass[JvmType.PrimInt.type].genClass,
      new RefClass[JvmType.PrimLong.type].genClass,
      new RefClass[JvmType.Object.type].genClass)
  }
}

