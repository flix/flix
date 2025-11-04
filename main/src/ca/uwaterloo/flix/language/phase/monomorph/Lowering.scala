package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.language.ast.Type

object Lowering {

  /**
    * Lowers the type `t`. Currently implemented as a no-op.
    *
    * When implemented:
    * Replaces schema types with the Datalog enum type and channel-related types with the channel enum type.
    *
    * @param t the type to be lowered.
    * @return
    */
   def lowerType(t: Type): Type = t
}
