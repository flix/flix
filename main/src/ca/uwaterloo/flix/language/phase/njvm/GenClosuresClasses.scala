package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.GenClosureClasses.genByteCode
import ca.uwaterloo.flix.language.phase.jvm.{ClosureInfo, JvmClass, JvmName, JvmOps, NamespaceInfo, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.classes.Closure
import ca.uwaterloo.flix.language.phase.njvm.interfaces.ContinuationInterface

object GenClosuresClasses extends MnemonicsGenerator{
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param types  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
          ns: Set[NamespaceInfo], closures: Set[ClosureInfo])
         (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass]  = {
    closures.foldLeft(map) {
      case (macc, closure) =>
        macc + new Closure(closure).getClassMapping
    }
  }
}
