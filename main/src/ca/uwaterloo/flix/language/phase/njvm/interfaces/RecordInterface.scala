package ca.uwaterloo.flix.language.phase.njvm.interfaces

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, Method1}

class RecordInterface(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val it: NJvmType.Reference = getRecordInterfaceType()
  private val ig: InterfaceGenerator = new InterfaceGenerator(it, List())

  //Fields
  //Interface has no fields ig doesn't even allow to compile fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generate the lookupField interface method. Stores the capability to call the method
    */
  val lookupFieldMethod: Method1[NJvmType.String.type, NJvmType.Reference] = ig.mkMethod1("lookupField")

  /**
    * Generate the restrictField interface method. Stores the capability to call the method
    */
  val restrictFieldMethod: Method1[NJvmType.String.type, NJvmType.Reference] = ig.mkMethod1("restrictField")

  private val jvmClass: JvmClass = JvmClass(it.name, ig.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    it.name -> this
}
