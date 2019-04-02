package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.JavaRuntimeFunctions
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._

class RecordEmpty(implicit root: Root, flix: Flix) {

  //Setup
  private val ct: JvmType.Reference = JvmOps.getRecordEmptyClassType()
  private val cg: ClassGenerator = new ClassGenerator(ct, List(Public, Final), JvmType.Object, Array(JvmOps.getRecordInterfaceType()))

  //Fields
  //Class with no fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method
  val constructor: Method0[JvmType.Void.type] = genConstructor

  val getRecordWithField: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genGetRecordWithFieldMethod

  val restrictField: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] = genRestrictFieldInterfaceMethod

  val _toString: Method0[JvmType.String.type] = genToStringMethod

  val _hashCode: Method0[JvmType.PrimInt.type] = genHashCodeMethod

  val equals: Method1[JvmType.Object.type, JvmType.PrimBool.type] = genEqualsMethod


  /**
    * This method generates the constructor for the RecordEmpty class. This constructor doesn't receive any arguments.
    * The method returns the capability to call the constructor
    * For example for RecordEmpty() creates the following constructor:
    *
    * public RecordEmpty() {}
    */
  private def genConstructor: Method0[JvmType.Void.type] = {

    cg.mkMethod0(List(Public), "<init>",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          JavaRuntimeFunctions.ObjectConstructor.INVOKE |>>
          RETURN)
  }

  /**
    * Method which generates the `getRecordWithField(String)` method which will always throws an exception,
    * since `getRecordWithField` should not be called.
    * Despite in order to stay in line with our format we still return the capability to call the method
    * The `getRecordWithField` method is always the following:
    *
    * public IRecord getRecordWithField(String var1) throws Exception {
    * throw new Exception("getField method shouldn't be called");
    * }
    *
    */
  private def genGetRecordWithFieldMethod: Method1[JvmType.String.type, MnemonicsType.RecordInterface.type] =
    cg.mkMethod1(List(Public, Final), "getRecordWithField",
      _ =>
        newUnsupportedOperationExceptionInstructions("getRecordWithField shouldn't be called")
    )

  /**
    * Method which generates the `restrictField(String)` method which will always throws an exception, since `restrictField` should not be called.
    * Despite in order to stay in line with our format we still return the capability to call the method
    * The `restrictField` method is always the following:
    *
    * public string getField(String var1) throws Exception {
    * throw new Exception("restrictField method shouldn't be called");
    * }
    */
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
    * Method which generates the mapping from the JvmName to JvmClass (which contains the class bytecode)
    */
  def genClass: (JvmName, JvmClass) = ct.name -> JvmClass(ct.name, cg.compile())

}
