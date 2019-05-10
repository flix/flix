package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType}
import ca.uwaterloo.flix.language.phase.jvm.{JvmName, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes.{MBool, MByte, MChar, MDouble, MFloat, MInt, MLong, MShort, Ref}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.classes.RecordExtend
import ca.uwaterloo.flix.language.phase.njvm.interfaces.{ContinuationInterface, TupleInterface}

object GenContinuationInterfaces extends MnemonicsGenerator{
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param ts  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo])(implicit root: FinalAst.Root, flix: Flix): Map[JvmName, Mnemonics.MnemonicsClass] = {
    map + (
      new ContinuationInterface[MBool].getClassMapping,
      new ContinuationInterface[MChar].getClassMapping,
      new ContinuationInterface[MFloat].getClassMapping,
      new ContinuationInterface[MDouble].getClassMapping,
      new ContinuationInterface[MByte].getClassMapping,
      new ContinuationInterface[MShort].getClassMapping,
      new ContinuationInterface[MInt].getClassMapping,
      new ContinuationInterface[MLong].getClassMapping,
      new ContinuationInterface[Ref[MObject]].getClassMapping)
  }
}
