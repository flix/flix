package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Api
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._

import scala.reflect.runtime.universe._

class RefClass[T : TypeTag](implicit root: Root, flix: Flix) {

  //Setup
  private val ct : JvmType.Reference = JvmName.getCellClassType(getJvmType[T])
  private val cg : ClassGenerator =  new ClassGenerator(ct, List(Public,Final), JvmType.Object, null)

  //Declare Fields
  private val field0 : Field[T] = cg.compileField[T](List(Private),"field0")

  //Declare methods
  //Constructor
  val constructor: Method1[T, JvmType.Void.type] = genConstructor

  //getValue
  val getValue : Method0[T] = genGetValueMethod

  //setValue
  val setValue : Method1[T, JvmType.Void.type ] = getSetValueMethod

  //Constructor
  private def genConstructor: Method1[T, JvmType.Void.type] = {

    cg.mkMethod1[T, JvmType.Void.type](List(Public), "<init>",
      sig =>
        sig.getArg0.LOAD[StackNil]|>>
          Api.JavaRuntimeFunctions.ObjectConstructor.INVOKE |>>
          sig.getArg0.LOAD |>>
          sig.getArg1.LOAD |>>
          field0.PUT_FIELD |>>
          RETURN)
  }

  private def genGetValueMethod : Method0[T] =

    cg.mkMethod0[T](List(Public,Final), "getValue",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          RETURN[T])


  private def getSetValueMethod: Method1[T, JvmType.Void.type] =

      cg.mkMethod1[T, JvmType.Void.type](List(Public, Final), "setValue",
        sig =>
          sig.getArg0.LOAD[StackNil] |>>
            sig.getArg1.LOAD |>>
            field0.PUT_FIELD |>>
            RETURN
      )

  def genClass : (JvmName, JvmClass) =
    ct.name -> JvmClass(ct.name , cg.compile())

}
