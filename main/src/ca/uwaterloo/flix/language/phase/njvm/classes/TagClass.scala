package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier.{Public, Static}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.{Api, NJvmType}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

import scala.reflect.runtime.universe._


class TagClass[T <: MnemonicsTypes : TypeTag](tag : TagInfo)(implicit root: Root, flix : Flix) extends MnemonicsClass {


  //Setup
  private val ct: Reference = getTagClassType(tag)
  private val cg: ClassGenerator = tag.enumType match {
    case MonoType.Enum(sym, elms) =>
      // Case 1: The type constructor is an enum.
      // Construct enum interface.
    val args = elms.map(getErasedJvmType)
    new ClassGenerator(ct, List(getEnumInterfaceType(sym, args)))
    case _ => ???
  }

  private val field0  : Field[T] = cg.mkField("field0")

  val instance : Option[UncheckedStaticField] =
    if(tag.tagType == MonoType.Unit) {
      val unitInstance = cg.mkUncheckedStaticField("unitInstance", ct, List(Public, Static))
      cg.UncheckedStaticFieldInit(unitInstance,
        NEW[StackNil](ct) |>>
        DUP[StackNil, Ref[MObject]] |>>
        Api.Java.Runtime.Value.Unit.getInstance.INVOKE[StackNil** Ref[MObject] ** Ref[MObject]] |>>
        Api.Java.Lang.Object.constructor.INVOKE[StackNil ** Ref[MObject] ** Ref[MObject], Ref[MUnit]] |>>
        unitInstance.PUT_STATIC |>>
        RETURN_VOID
      )
      Some(unitInstance)
    }
    else
      None

  val defaultConstructor : VoidMethod2[Ref[TagClass[T]], T] =
    cg.mkConstructor2(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
        cg.SUPER |>>
        sig.getArg1.LOAD |>>
        sig.getArg2.LOAD |>>
        field0.PUT_FIELD |>>
        RETURN_VOID
    )


  val getTagMethod : Method1[Ref[TagClass[T]], Ref[MString]] =
    cg.mkMethod1("getTag",
      _ =>
        LDC_STRING[StackNil](tag.tag) |>>
        RETURN
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
  val toStringMethod: Method1[Ref[TagClass[T]],Ref[MString]] =
    cg.mkMethod1("toString",
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
  val hashCodeMethod: Method1[Ref[TagClass[T]], MInt] =
    cg.mkMethod1("hashCode",
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
  val equalsMethod: Method2[Ref[TagClass[T]], Ref[MObject], MBool] =
    cg.mkMethod2("equal",
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

