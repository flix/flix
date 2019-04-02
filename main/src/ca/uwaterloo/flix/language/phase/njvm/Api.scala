package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.{JvmName, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{**, F, JvmModifier, MnemonicsType, Stack}

/**
  * This singleton will contain a bunch of capabilities to invoke methods which we
  * don't generate bytcode for. Such as the Object construtor, the string equals, etc.
  * This is essentially filled in an ad-hoc manner
  */
object Api {

  object JavaRuntimeFunctions {

    object ObjectConstructor {
      def INVOKE[S <: Stack]: F[S ** JvmType.Reference] => F[S] =
        t => t.invoke(JvmModifier.InvokeSpecial, JvmName.Object.toInternalName, "<init>", Nil, JvmType.Void)
    }

    object ExceptionConstructor {
      def INVOKE[S <: Stack]: F[S ** MnemonicsType.UnsupportedOperationException.type ** JvmType.String.type] => F[S] =
        t => t.invoke(JvmModifier.InvokeSpecial, JvmName.UnsupportedOperationException.toInternalName, "<init>", List(JvmType.String), JvmType.Void)
    }

    object StringEquals {
      def INVOKE[S <: Stack]: F[S ** JvmType.String.type ** JvmType.String.type] => F[S ** JvmType.PrimBool.type] =
        t => t.invoke(JvmModifier.InvokeVirtual, JvmType.String.name.toInternalName, "equals", List(JvmType.Object), JvmType.PrimBool)
    }

  }

}



