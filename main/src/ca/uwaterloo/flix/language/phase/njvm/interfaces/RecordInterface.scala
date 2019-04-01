package ca.uwaterloo.flix.language.phase.njvm.interfaces

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmOps, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, Method1, MnemonicsType}

class RecordInterface(implicit root: Root, flix: Flix) {
  //Setup
  private val it : JvmType.Reference = JvmOps.getRecordInterfaceType()
  private val ig : InterfaceGenerator =  new InterfaceGenerator(it, List(Public, Abstract, Interface), JvmType.Object, null)

  val getRecordWithField : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genGetRecordWithFieldInterfaceMethod
  val restrictField : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genRestrictFieldInterfaceMethod

  private def genGetRecordWithFieldInterfaceMethod : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    ig.mkMethod1[JvmType.String.type, MnemonicsType.RecordInterface.type](List(Public, Abstract), "getRecordWithField")

  private def genRestrictFieldInterfaceMethod : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    ig.mkMethod1[JvmType.String.type, MnemonicsType.RecordInterface.type](List(Public, Abstract), "restrictField")


  def genInterface: (JvmName, JvmClass) = {
    it.name -> JvmClass(it.name, ig.compile())
  }
}
