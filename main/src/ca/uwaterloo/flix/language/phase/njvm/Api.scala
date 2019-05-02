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

import ca.uwaterloo.flix.language.phase.jvm.JvmName
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

/**
  * This singleton will contain a bunch of capabilities to invoke methods which we
  * don't generate bytcode for. Such as the Object construtor, the string equals, etc.
  * This is essentially filled in an ad-hoc manner
  */
object Api {

  object Java {

    object Lang {

      object Object {
        val constructor: VoidMethod0 = new VoidMethod0(JvmModifier.InvokeSpecial, NJvmType.Object, "<init>")
      }

      object String {

        object equals {
          def INVOKE[S <: Stack]: F[S ** JString.type ** JString.type] => F[S ** NJvmType.PrimBool] =
            t => t.emitInvoke(JvmModifier.InvokeVirtual, JString.name.toInternalName, "equals", List(NJvmType.Object), NJvmType.PrimBool())
        }

      }

      object UnsupportedOperationException {
        val constructor: VoidMethod1[JString.type] =
          new VoidMethod1(JvmModifier.InvokeSpecial, Reference(JvmName.UnsupportedOperationException), "<init>")

      }

    }

  }

}



