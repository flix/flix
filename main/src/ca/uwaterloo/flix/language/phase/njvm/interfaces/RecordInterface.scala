package ca.uwaterloo.flix.language.phase.njvm.interfaces

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmOps, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, Method1, MnemonicsType}

class RecordInterface(implicit root: Root, flix: Flix) {
  //Setup
  private val it: JvmType.Reference = JvmOps.getRecordInterfaceType()
  private val ig: InterfaceGenerator = new InterfaceGenerator(it, List(Public, Abstract, Interface), JvmType.Object, Array())
  //Fields
  //Interface has no fields ig doesn't even allow to compile fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method
  val getRecordWithField: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genGetRecordWithFieldInterfaceMethod
  val restrictField: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genRestrictFieldInterfaceMethod

  /**
    * Generate the getRecordWithField interface method. Returns the capability to call the method
    */
  private def genGetRecordWithFieldInterfaceMethod: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    ig.mkMethod1(List(Public, Abstract), "getRecordWithField")

  /**
    * Generate the restrictField interface method. Returns the capability to call the method
    */
  private def genRestrictFieldInterfaceMethod: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    ig.mkMethod1(List(Public, Abstract), "restrictField")


  def genInterface: (JvmName, JvmClass) = {
    it.name -> JvmClass(it.name, ig.compile())
  }
}
