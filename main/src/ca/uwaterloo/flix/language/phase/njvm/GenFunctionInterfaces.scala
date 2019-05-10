package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType}
import ca.uwaterloo.flix.language.phase.jvm.{JvmName, NamespaceInfo, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.interfaces.{FunctionInterface, TupleInterface}

object GenFunctionInterfaces extends MnemonicsGenerator{
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param types  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo], ns: Set[NamespaceInfo])(implicit root: FinalAst.Root, flix: Flix): Map[JvmName, Mnemonics.MnemonicsClass] = {

    //
    // Generate a function interface for each type and collect the results in a map.
    //
    types.foldLeft(map) {
      case (macc, MonoType.Arrow(targs, tresult)) =>
        val elms = targs.map(getErasedJvmType)

        val returnType = getErasedJvmType(tresult)
        macc + new FunctionInterface(elms, returnType).getClassMapping
      case (macc, _) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }
}
