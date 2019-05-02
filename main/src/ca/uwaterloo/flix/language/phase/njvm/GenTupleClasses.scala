package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType}
import ca.uwaterloo.flix.language.phase.jvm.JvmName
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{ MnemonicsGenerator, getErasedJvmType}
import ca.uwaterloo.flix.language.phase.njvm.classes.TupleClass

object GenTupleClasses extends MnemonicsGenerator{
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param ts  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, Mnemonics.MnemonicsClass], ts: Set[MonoType])(implicit root: FinalAst.Root, flix: Flix): Map[JvmName, Mnemonics.MnemonicsClass] = {
    ts.foldLeft(map) {
      case (macc, tpe@MonoType.Tuple(elms)) =>
        val targs = elms.map(getErasedJvmType)

        macc + new TupleClass(macc, targs).getClassMapping
      case (macc, tpe) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }
}