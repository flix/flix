package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Method0

object Api {

  object JavaRuntimeFunction{
    val ObjectConstructor = new Method0[JvmType.Void.type]
  }
}



