package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MethodHandler0

object Api {

  object JavaRuntimeFunction{
    val ObjectConstructor = new MethodHandler0[JvmType.Void.type]
  }
}



