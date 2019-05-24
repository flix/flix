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
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType.Reference

/**
  * This singleton will contain a bunch of capabilities to invoke methods which we
  * don't generate bytcode for. Such as the Object construtor, the string equals, etc.
  * This is essentially filled in an ad-hoc manner
  */
object Api {

  object Java {
    object Math{
      object BigIntger {
        val constructor : VoidMethod2[Ref[MBigInt], Ref[MString]] = new VoidMethod2(JvmModifier.InvokeSpecial, NJvmType.BigInteger, "<init>")
      }
    }
    object Runtime {
      object ProxyObject {
        val of : Method4[Ref[MObject], Ref[MFunction], Ref[MFunction], Ref[MFunction] , Ref[MProxyObject]] = new Method4(JvmModifier.InvokeStatic, Reference(JvmName.ProxyObject), "of")
      }
      object Value {
        object Unit {
          val getInstance: Method0[Ref[MUnit]] = new Method0(JvmModifier.InvokeStatic, NJvmType.Unit, "getInstance")
        }
      }
    }
    object Lang {
      object Boolean {
        val valueOf : Method1[MBool, Ref[MBool]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Boolean), "valueOf")
        val booleanValue : Method1[Ref[MBool],MBool] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Boolean), "booleanValue")

      }
      object Character {
        val valueOf : Method1[MChar, Ref[MChar]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Character), "valueOf")
        val charValue : Method1[Ref[MChar],MChar] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Character), "charValue")

      }
      object Float {
        val valueOf : Method1[MFloat, Ref[MFloat]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Float), "valueOf")
        val floatValue : Method1[Ref[MFloat], MFloat] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Float), "floatValue")
      }
      object Double {
        val valueOf : Method1[MDouble, Ref[MDouble]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Double), "valueOf")
        val doubleValue : Method1[Ref[MDouble],MDouble] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Double), "doubleValue")

      }
      object Byte {
        val valueOf : Method1[MByte, Ref[MByte]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Byte), "valueOf")
        val byteValue : Method1[Ref[MByte], MByte] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Byte), "byteValue")

      }
      object Short {
        val valueOf : Method1[MShort, Ref[MShort]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Short), "valueOf")
        val shortValue : Method1[Ref[MShort],MShort] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Short), "shortValue")

      }
      object Integer {
        val valueOf : Method1[MInt, Ref[MInt]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Integer), "valueOf")
        val intValue : Method1[Ref[MInt],MInt] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Integer), "intValue")

      }
      object Long {
        val valueOf : Method1[MLong, Ref[MLong]] = new Method1(JvmModifier.InvokeStatic, Reference(JvmName.Long), "valueOf")
        val longValue : Method1[Ref[MLong], MLong] = new Method1(JvmModifier.InvokeVirtual, Reference(JvmName.Long), "longValue")

      }

      object Object {
        val constructor: VoidMethod1[Ref[MObject]] = new VoidMethod1(JvmModifier.InvokeSpecial, NJvmType.Object, "<init>")
      }
      object String {
        object equals {
          def INVOKE[S <: Stack]: F[S ** Ref[MString] ** Ref[MString]] => F[S ** MBool] =
            t => t.emitInvoke(JvmModifier.InvokeVirtual, NJvmType.String.name.toInternalName, "equals", List(NJvmType.Object), NJvmType.PrimBool)
        }
      }
      object Exception {
        val constructor: VoidMethod2[Ref[MObject], Ref[MString]] =
          new VoidMethod2(JvmModifier.InvokeSpecial, Reference(JvmName.Exception), "<init>")
      }
      object UnsupportedOperationException {
        val constructor: VoidMethod2[Ref[MObject], Ref[MString]] =
          new VoidMethod2(JvmModifier.InvokeSpecial, Reference(JvmName.UnsupportedOperationException), "<init>")
      }
    }
  }
}



