package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._

class RecordEmpty(map: Map[JvmName, MnemonicsClass])(implicit root: Root, flix: Flix) extends MnemonicsClass {

  //Setup
  private val ct: NJvmType.Reference = getRecordEmptyClassType()
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getRecordInterfaceType()))

  //Fields
  //Class with no fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generates the constructor for the RecordEmpty class. This constructor doesn't receive any arguments.
    * Variable contains the capability to call the constructor
    * For example for RecordEmpty() creates the following constructor:
    *
    * public RecordEmpty() {}
    */
  val defaultConstructor: VoidMethod0 = {

    cg.mkConstructor0(
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          cg.SUPER |>>
          RETURN_VOID)
  }

  /**
    * Generate the `lookupField(String)` method which will always throws an exception,
    * since `getRecordWithField` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `lookupField` method is always the following:
    *
    * public Object lookupField(String var1) throws Exception {
    * throw new Exception("lookupField method shouldn't be called");
    * }
    */
  val lookupFieldMethod: Method1[NJvmType.String.type, NJvmType.Reference] =
    cg.mkMethod1("lookupField",
      _ =>
        newUnsupportedOperationExceptionInstructions("lookupField shouldn't be called")
    )

  /**
    * Generate the `restrictField(String)` method which will always throws an exception, since `restrictField` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `restrictField` method is always the following:
    *
    * public string getField(String var1) throws Exception {
    * throw new Exception("restrictField method shouldn't be called");
    * }
    */
  val restrictFieldMethod: Method1[NJvmType.String.type, NJvmType.Reference] =
    cg.mkMethod1("restrictField",
      _ =>
        newUnsupportedOperationExceptionInstructions("restrictField shouldn't be called")
    )

  /**
    * Generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    */
  val toStringMethod: Method0[NJvmType.String.type] =
    cg.mkMethod0("toString",
      _ =>
        newUnsupportedOperationExceptionInstructions("toString shouldn't be called")
    )

  /** Generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    */
  val hashCodeMethod: Method0[NJvmType.PrimInt] =
    cg.mkMethod0("hashCode",
      _ =>
        newUnsupportedOperationExceptionInstructions("hashCode shouldn't be called")
    )

  /**
    * Generate the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    * throw new Exception("equals method shouldn't be called");
    * }
    *
    */
  val equalsMethod: Method1[NJvmType.Object.type, NJvmType.PrimBool] =
    cg.mkMethod1("equal",
      _ =>
        newUnsupportedOperationExceptionInstructions("equals shouldn't be called")
    )

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this

}
