package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.{JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{**, F, JvmModifier, Stack}

/**
  * This singleton will contain a bunch of capabilities to invoke methods which we
  * don't generate bytcode for. Such as the Object construtor, the string equals, etc.
  * This is essentially filled in an ad-hoc manner
  */
object Api {
  object Java {
    object Lang {
      object Object {
        object Constructor {
          def INVOKE[S <: Stack]: F[S ** NJvmType.Reference] => F[S] =
            t => t.emitInvoke(JvmModifier.InvokeSpecial, JvmName.Object.toInternalName, "<init>", Nil, NJvmType.Void())
        }
      }
      object String {
        object Equals {
          def INVOKE[S <: Stack]: F[S ** NJvmType.String.type ** NJvmType.String.type] => F[S ** NJvmType.PrimBool] =
            t => t.emitInvoke(JvmModifier.InvokeVirtual, NJvmType.String.name.toInternalName, "equals", List(NJvmType.Object), NJvmType.PrimBool())
        }
      }
      object Exception {
        object Constructor {
          def INVOKE[S <: Stack]: F[S ** NJvmType.Reference ** NJvmType.String.type] => F[S] =
            t => t.emitInvoke(JvmModifier.InvokeSpecial, JvmName.UnsupportedOperationException.toInternalName, "<init>", List(NJvmType.String), NJvmType.Void())
        }
      }
    }
  }
}



