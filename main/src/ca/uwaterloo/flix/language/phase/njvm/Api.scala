package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.{JvmName, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{**, F, JvmModifier, MnemonicsType, Stack}

object Api {

  object JavaRuntimeFunctions {

    object ObjectConstructor {
      def INVOKE[S <: Stack]: F[S ** JvmType.Reference] => F[S] =
        t => t.invoke(JvmModifier.InvokeSpecial, JvmName.Object.toInternalName, "<init>", Nil, JvmType.Void)
    }


    object ExceptionConstructor {
      def INVOKE[S <: Stack] : F[S ** MnemonicsType.UnsupportedOperationException.type ** JvmType.String.type] => F[S] =
        t => t.invoke(JvmModifier.InvokeSpecial, JvmName.UnsupportedOperationException.toInternalName, "<init>", List(JvmType.String), JvmType.Void)
    }

  }
}



