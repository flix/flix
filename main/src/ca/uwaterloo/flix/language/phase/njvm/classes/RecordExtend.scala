package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.Java
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.interfaces.RecordInterface
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._


import scala.reflect.runtime.universe._


class RecordExtend[T: TypeTag](map: Map[JvmName, MnemonicsClass])(implicit root: Root, flix: Flix) extends MnemonicsClass {

  //Setup
  private val ct: Reference = getRecordExtendClassType(getJvmType[T])
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getRecordInterfaceType()))

  //Fields each variable represents a field which can be accessed
  //while generating code for this class
  private val field0: Field[JString.type] = cg.mkField("field0")
  private val field1: Field[T] = cg.mkField("field1")
  private val field2: Field[Reference] = cg.mkField("field2")

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generate the constructor for the RecordExtend class. This constructor receives three arguments, the field label,
    * value and the rest of the record.
    * Variable contains the capability to call the constructor
    * For example for RecordExtend$Obj(String, Object, Object) creates the following constructor:
    *
    * public RecordExtend$Obj(String var1, Object var2, Object var3) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    *   this.field2 = var3;
    * }
    *
    */
  val defaultConstructor: VoidMethod3[JString.type, T, Reference] = {
    cg.mkConstructor3(
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg0.LOAD |>>
          sig.getArg1.LOAD |>>
          field0.PUT_FIELD |>>

          sig.getArg0.LOAD |>>
          sig.getArg2.LOAD |>>
          field1.PUT_FIELD |>>

          sig.getArg0.LOAD |>>
          sig.getArg3.LOAD |>>
          field2.PUT_FIELD |>>
          RETURN_VOID
    )
  }

  /**
    * Gennerate the getField method in the RecordExtend class. The method receives no arguments.
    * It should simply return the record field (value) which is store in the variable field2.
    *
    * We store the capability to call getField in getFieldMethod
    */
  val getFieldMethod: Method0[T] = {
    cg.mkMethod0("getField",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          field1.GET_FIELD |>>
          RETURN
    )
  }


  /**
    * Gennerate the lookupField method in the RecordExtend class. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return this
    * (The record object which has the given label). In case the provided label is not equal we recursively call lookupField on the rest of the record,
    * and return the value provided by the recursive call.
    *
    * We store the capability to call lookupField in lookupFieldMethod
    */
  val lookupFieldMethod: Method1[JString.type, Reference] = {
    val recordInterface: RecordInterface =
      map.getOrElse(getRecordInterfaceType().name, null).asInstanceOf[RecordInterface]

    cg.mkMethod1("lookupField",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          sig.getArg1.LOAD |>>
          Java.Lang.String.equals.INVOKE |>>
          IFEQ(
            sig.getArg0.LOAD[StackNil] |>>
              RETURN) |>>
          sig.getArg0.LOAD |>>
          field2.GET_FIELD |>>
          CHECK_CAST(getRecordInterfaceType()) |>>
          sig.getArg1.LOAD |>>
          recordInterface.lookupFieldMethod.INVOKE |>>
          RETURN
    )
  }

  /**
    * Generate the restrictField method in the RecordExtend class. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return the rest of the record
    * (field2).In case the provided label is not equal we recursively call restrictField on the rest of the record.
    * Then we should set our 'rest' field(field2) to what was returned by the recursive call.
    * Because we might need to update our 'rest' pointer since if the provided label is equal to the next field label,
    * then this field should no longer be in the record. We then return 'this'.
    *
    * We store the capability to call restrictField in restrictFieldMethod
    */
  val restrictFieldMethod: Method1[JString.type, Reference] = {
    val recordInterface: RecordInterface =
      map.getOrElse(getRecordInterfaceType().name, null).asInstanceOf[RecordInterface]

    cg.mkMethod1("restrictField",
      sig =>
        sig.getArg0.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          sig.getArg1.LOAD |>>
          Java.Lang.String.equals.INVOKE |>>
          IFEQ(
            sig.getArg0.LOAD[StackNil] |>>
              field2.GET_FIELD |>>
              RETURN) |>>
          sig.getArg0.LOAD |>>
          DUP |>>
          field2.GET_FIELD |>>
          CHECK_CAST(getRecordInterfaceType()) |>>
          sig.getArg1.LOAD |>>
          recordInterface.restrictFieldMethod.INVOKE |>>
          field2.PUT_FIELD |>>
          sig.getArg0.LOAD |>>
          RETURN
    )
  }

  /**
    * Generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    */
  val toStringMethod: Method0[JString.type] =
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
  val hashCodeMethod: Method0[PrimInt] =
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
  val equalsMethod: Method1[Object.type, PrimBool] =
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
