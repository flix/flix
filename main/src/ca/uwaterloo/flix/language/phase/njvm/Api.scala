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



