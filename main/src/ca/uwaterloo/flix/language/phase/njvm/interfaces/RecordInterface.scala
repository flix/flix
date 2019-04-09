package ca.uwaterloo.flix.language.phase.njvm.interfaces

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.JvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, Method1}

class RecordInterface(implicit root: Root, flix: Flix) {
  //Setup
  private val it: JvmType.Reference = getRecordInterfaceType()
  private val ig: InterfaceGenerator = new InterfaceGenerator(it, List(Public, Abstract, Interface), JvmType.Object, Array())
  //Fields
  //Interface has no fields ig doesn't even allow to compile fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generate the lookupField interface method. Returns the capability to call the method
    */
  val lookupFieldMethod: Method1[JvmType.String.type, JvmType.Reference] = ig.mkMethod1("lookupField")

  /**
    * Generate the restrictField interface method. Returns the capability to call the method
    */
  val restrictFieldMethod: Method1[JvmType.String.type, JvmType.Reference] = ig.mkMethod1("restrictField")

  def genInterface: (JvmName, JvmClass) = {
    it.name -> JvmClass(it.name, ig.compile())
  }
}
