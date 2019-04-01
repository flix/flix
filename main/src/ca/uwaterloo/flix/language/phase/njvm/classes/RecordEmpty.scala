package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmOps, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Api
import ca.uwaterloo.flix.language.phase.njvm.Api.JavaRuntimeFunctions
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier.{Abstract, Final, Public}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{ClassGenerator, Instructions, Method1, MnemonicsType, StackNil}

class RecordEmpty(implicit root: Root, flix: Flix) {

  //Setup
  private val ct : JvmType.Reference = JvmOps.getRecordEmptyClassType()
  private val cg : ClassGenerator =  new ClassGenerator(ct, List(Public,Final), JvmType.Object, Array(JvmOps.getRecordInterfaceType()))

  //Constructor
  val constructor: Method0[JvmType.Void.type] = genConstructor

  def genConstructor: Method0[JvmType.Void.type] = {

    cg.mkMethod0[JvmType.Void.type](List(Public), "<init>",
      sig =>
        sig.getArg0.LOAD[StackNil]|>>
          JavaRuntimeFunctions.ObjectConstructor.INVOKE |>>
          Instructions.RETURN)
  }

  val getRecordWithField : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genGetRecordWithFieldMethod
  val restrictField : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genRestrictFieldInterfaceMethod


  private def genGetRecordWithFieldMethod : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    cg.mkMethod1[JvmType.String.type, MnemonicsType.RecordInterface.type](List(Public, Final), "getRecordWithField",
      _ =>
        NEW[StackNil, MnemonicsType.UnsupportedOperationException.type] |>>
        DUP |>>
        LDC_STRING("getRecordWithField shouldn't be called") |>>
        Api.JavaRuntimeFunctions.ExceptionConstructor.INVOKE |>>
        THROW
    )

  private def genRestrictFieldInterfaceMethod : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    cg.mkMethod1[JvmType.String.type, MnemonicsType.RecordInterface.type](List(Public, Final), "restrictField",
      _ =>
        NEW[StackNil, MnemonicsType.UnsupportedOperationException.type] |>>
        DUP |>>
        LDC_STRING("restrictField shouldn't be called") |>>
        Api.JavaRuntimeFunctions.ExceptionConstructor.INVOKE |>>
        THROW
    )



  def genClass: (JvmName, JvmClass) = ct.name -> JvmClass(ct.name, cg.compile())


}
