package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.{Api, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Api.Java
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._

import scala.reflect.runtime.universe._

class RefClass[T: TypeTag](implicit root: Root, flix: Flix) {

  //Setup
  private val ct: JvmType.Reference = getCellClassType(getJvmType[T])
  private val cg: ClassGenerator = new ClassGenerator(ct, List(Public, Final), JvmType.Object, Array())

  //Fields each variable represents a field which can be acessed
  //while generating code for this class
  private val field0: Field[T] = cg.mkField("field0")

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generates the constructor for the current RefClass(depends on the type paramater)
    * we are generating.. Returning the capability to invoke the constructor
    *
    * For example When generating a constructor for a RefClass which encapsualtes an integer the construtor will be:
    *
    * public Ref$Int32(int var1) {
    *   this.field0 = var1;
    * }
    *
    */
  val defaultConstructor: Method1[T, JvmType.Void] = {

    cg.mkMethod1("<init>",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          Java.Lang.Object.Constructor.INVOKE |>>
          sig.getArg0.LOAD |>>
          sig.getArg1.LOAD |>>
          field0.PUT_FIELD |>>
          RETURN,
      List(Public))
  }

  /**
    * Generates the getValue method for the current RefClass(depends on the type paramater)
    * we are generating. Returning the capability to invoke the getValue method
    *
    * For example When generating a getValue for a RefClass which encapsualtes an integer getValue will be:
    *
    * public final int getValue() {
    * return this.field0;
    * }
    *
    */
  val getValueMethod: Method0[T] =
    cg.mkMethod0("getValue",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          RETURN[T]
    )

  /**
    * Generates the setValue method for the current RefClass(depends on the type paramater)
    * we are generating. Returning the capability to invoke the setValue method
    *
    * For example When generating a getValue for a RefClass which encapsualtes an integer setValue will be:
    *
    * public final void setValue(int var1) {
    *   this.field0 = var1;
    * }
    *
    */
  val setValueMethod: Method1[T, JvmType.Void] =
    cg.mkMethod1("setValue",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          sig.getArg1.LOAD |>>
          field0.PUT_FIELD |>>
          RETURN
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
  def genClass: (JvmName, JvmClass) =
    ct.name -> JvmClass(ct.name, cg.compile())

}
