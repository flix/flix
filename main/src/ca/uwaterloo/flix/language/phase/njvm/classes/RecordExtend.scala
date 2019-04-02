package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.JavaRuntimeFunctions
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import org.objectweb.asm.Label

import scala.reflect.runtime.universe._


class RecordExtend[T: TypeTag](implicit root: Root, flix: Flix) {

  //Setup
  private val ct: JvmType.Reference = JvmOps.getRecordEmptyClassType()
  private val cg: ClassGenerator = new ClassGenerator(ct, List(Public, Final), JvmType.Object, Array(JvmOps.getRecordInterfaceType()))

  //Fields each variable represents a field which can be accessed
  //while generating code for this class
  private val field0: Field[JvmType.String.type] = cg.compileField(List(Private), "field0")
  private val field1: Field[T] = cg.compileField(List(Private), "field1")
  private val field2: Field[MnemonicsType.RecordInterface.type] = cg.compileField(List(Private), "field2")

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method
  val constructor: Method3[JvmType.String.type, T, MnemonicsType.RecordInterface.type, JvmType.Void.type] = genConstructor

  val getRecordWithField: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genGetRecordWithFieldMethod

  val restrictField: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genRestrictFieldInterfaceMethod

  val _toString: Method0[JvmType.String.type] = genToStringMethod

  val _hashCode: Method0[JvmType.PrimInt.type] = genHashCodeMethod

  val equals: Method1[JvmType.Object.type, JvmType.PrimBool.type] = genEqualsMethod

  private def genConstructor: Method3[JvmType.String.type, T, MnemonicsType.RecordInterface.type, JvmType.Void.type] = {

    cg.mkMethod3(List(Public), "<init>",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          JavaRuntimeFunctions.ObjectConstructor.INVOKE |>>
          sig.getArg0.LOAD |>>
          sig.getArg1.LOAD |>>
          field0.PUT_FIELD |>>

          sig.getArg0.LOAD |>>
          sig.getArg2.LOAD |>>
          field1.PUT_FIELD |>>

          sig.getArg0.LOAD |>>
          sig.getArg3.LOAD |>>
          field2.PUT_FIELD |>>
          RETURN)
  }

  private def genGetRecordWithFieldMethod: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = {

//    val ret = new Label
//    val falseCase = new Label
//
//    cg.mkMethod1(List(Public, Final), "getRecordWithField",
//      sig =>
//        sig.getArg0.LOAD[StackNil] |>>
//          field0.GET_FIELD |>>
//          sig.getArg1.LOAD |>>
//          JavaRuntimeFunctions.StringEquals.INVOKE |>>
//          IFEQ(falseCase) |>>
//          sig.getArg0.LOAD |>>
//          GOTO(ret)|>>
//          EMIT(falseCase) |>>
//          EMIT(ret) |>>
//          RETURN[MnemonicsType.RecordInterface.type]
//
//    )
    cg.mkMethod1(List(Public, Final), "getRecordWithField",
      _ =>
        newUnsupportedOperationExceptionInstructions("getRecordWithField shouldn't be called")
    )

  }
  private def genRestrictFieldInterfaceMethod: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    cg.mkMethod1(List(Public, Final), "restrictField",
      _ =>
        newUnsupportedOperationExceptionInstructions("restrictField shouldn't be called")
    )

  /**
    * Method which generates the `toString()` method which will always throws an exception, since `toString` should not be called.
    * Despite in order to stay in line with our format we still return the capability to call the method
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    */
  private def genToStringMethod: Method0[JvmType.String.type] =
    cg.mkMethod0(List(Public, Final), "toString",
      _ =>
        newUnsupportedOperationExceptionInstructions("toString shouldn't be called")
    )

  /** Method which generates the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * Despite in order to stay in line with our format we still return the capability to call the method
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    */
  private def genHashCodeMethod: Method0[JvmType.PrimInt.type] =
    cg.mkMethod0(List(Public, Final), "hashCode",
      _ =>
        newUnsupportedOperationExceptionInstructions("hashCode shouldn't be called")
    )

  /**
    * Method which generates the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * Despite in order to stay in line with our format we still return the capability to call the method
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    * throw new Exception("equals method shouldn't be called");
    * }
    *
    */
  private def genEqualsMethod: Method1[JvmType.Object.type, JvmType.PrimBool.type] =
    cg.mkMethod1(List(Public, Final), "equal",
      _ =>
        newUnsupportedOperationExceptionInstructions("equals shouldn't be called")
    )

  /**
    *  Method which generates the mapping from the JvmName to JvmClass (which contains the class bytecode)
    */
  def genClass: (JvmName, JvmClass) = ct.name -> JvmClass(ct.name, cg.compile())

}
