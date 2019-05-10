package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType}
import ca.uwaterloo.flix.language.phase.jvm.{JvmName, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.interfaces.{ContinuationInterface, EnumInterface}

object GenEnumInterfaces extends MnemonicsGenerator {
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param types  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo])(implicit root: FinalAst.Root, flix: Flix): Map[JvmName, Mnemonics.MnemonicsClass] = {
    types.foldLeft(map) {
      case (macc, MonoType.Enum(sym, elms)) =>
        // Case 1: The type constructor is an enum.
        // Construct enum interface.
        val args = elms.map(getErasedJvmType)

        macc + new EnumInterface(sym, args).getClassMapping
      case (macc, _) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }
}
