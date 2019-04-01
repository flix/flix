package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
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

  val _toString : Method0[JvmType.String.type] = genToStringMethod

  val _hashCode : Method0[JvmType.PrimInt.type] = genHashCodeMethod

  val equals : Method1[JvmType.Object.type, JvmType.PrimBool.type] = genEqualsMethod


  private def genGetRecordWithFieldMethod : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    cg.mkMethod1[JvmType.String.type, MnemonicsType.RecordInterface.type](List(Public, Final), "getRecordWithField",
      _ =>
        newUnsupportedOperationExceptionInstructions("getRecordWithField shouldn't be called")
    )

  private def genRestrictFieldInterfaceMethod : Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    cg.mkMethod1[JvmType.String.type, MnemonicsType.RecordInterface.type](List(Public, Final), "restrictField",
      _ =>
        newUnsupportedOperationExceptionInstructions("restrictField shouldn't be called")
    )

  private def genToStringMethod: Method0[JvmType.String.type] =
    cg.mkMethod0[JvmType.String.type](List(Public, Final), "toString",
      _ =>
        newUnsupportedOperationExceptionInstructions("toString shouldn't be called")
    )

  private def genHashCodeMethod: Method0[JvmType.PrimInt.type] =
    cg.mkMethod0[JvmType.PrimInt.type](List(Public, Final), "hashCode",
      _ =>
        newUnsupportedOperationExceptionInstructions("hashCode shouldn't be called")
    )

  private def genEqualsMethod: Method1[JvmType.Object.type, JvmType.PrimBool.type] =
    cg.mkMethod1[JvmType.Object.type, JvmType.PrimBool.type](List(Public, Final), "equal",
      _ =>
        newUnsupportedOperationExceptionInstructions("equals shouldn't be called")
    )



  def genClass: (JvmName, JvmClass) = ct.name -> JvmClass(ct.name, cg.compile())

}
