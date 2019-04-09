package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.Java
import ca.uwaterloo.flix.language.phase.njvm.JvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._

import scala.reflect.runtime.universe._


class RecordExtend[T: TypeTag](implicit root: Root, flix: Flix) {

  //Setup
  private val ct: JvmType.Reference = getRecordEmptyClassType()
  private val cg: ClassGenerator = new ClassGenerator(ct, List(Public, Final), JvmType.Object, Array(getRecordInterfaceType()))

  //Fields each variable represents a field which can be accessed
  //while generating code for this class
  private val field0: Field[JvmType.String.type] = cg.mkField("field0")
  private val field1: Field[T] = cg.mkField("field1")
  private val field2: Field[JvmType.Reference] = cg.mkField("field2")

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method
  val defaultConstructor: Method3[JvmType.String.type, T, JvmType.Reference, JvmType.Void] = {
    cg.mkMethod3("<init>",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          Java.Lang.Object.Constructor.INVOKE |>>
          sig.getArg0.LOAD |>>
          sig.getArg1.LOAD |>>
          field0.PUT_FIELD |>>

          sig.getArg0.LOAD |>>
          sig.getArg2.LOAD |>>
          field1.PUT_FIELD |>>

          sig.getArg0.LOAD |>>
          sig.getArg3.LOAD |>>
          field2.PUT_FIELD |>>
          RETURN,
      List(Public))
  }

  val lookupFieldMethod: Method1[JvmType.String.type, JvmType.Reference] =
    cg.mkMethod1("lookupField",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          sig.getArg1.LOAD |>>
          Java.Lang.String.Equals.INVOKE |>>
          IFEQ(
            sig.getArg0.LOAD[StackNil] |>>
              RETURN) |>>
          sig.getArg0.LOAD |>>
          //TODO:Invoke IRecord lookup field
          RETURN


    )

  val restrictFieldMethod: Method1[JvmType.String.type, JvmType.Reference] =
    cg.mkMethod1("restrictField",
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
  val toStringMethod: Method0[JvmType.String.type] =
    cg.mkMethod0("toString",
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
  val hashCodeMethod: Method0[JvmType.PrimInt] =
    cg.mkMethod0("hashCode",
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
  val equalsMethod: Method1[JvmType.Object.type, JvmType.PrimBool] =
    cg.mkMethod1("equal",
      _ =>
        newUnsupportedOperationExceptionInstructions("equals shouldn't be called")
    )

  /**
    * Method which generates the mapping from the JvmName to JvmClass (which contains the class bytecode)
    */
  def genClass: (JvmName, JvmClass) = ct.name -> JvmClass(ct.name, cg.compile())

}
