package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType.{PrimBool, PrimByte, PrimChar, PrimDouble, PrimFloat, PrimInt, PrimLong, PrimShort, Reference}

import scala.reflect.runtime.universe._


class TupleClass(map : Map[JvmName, MnemonicsClass], elms : List[NJvmType])(implicit root: Root, flix : Flix) extends MnemonicsClass {


  //Setup
  private val ct: Reference = getTupleClassType(elms)
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getTupleInterfaceType(elms)))

  //Fields
  for ((arg, ind) <- elms.zipWithIndex) {
      arg match {
        case PrimBool => new Field[MBool]("field" + ind)
        case PrimChar => new Field[MChar]("field" + ind)
        case PrimByte => new Field[MByte]("field" + ind)
        case PrimShort => new Field[MShort]("field" + ind)
        case PrimInt => new Field[MInt]("field" + ind)
        case PrimLong => new Field[MLong]("field" + ind)
        case PrimFloat => new Field[MFloat]("field" + ind)
        case PrimDouble => new Field[MDouble]("field" + ind)
        case Reference(_) => new Field[Ref[MObject]]("field" + ind)
        case _ => ???
      }
  }

  private def getField[T1 <: MnemonicsTypes : TypeTag](ind : Int) : Field[T1] =
    new Field[T1]("field" + ind)

  val defaultConstrutor : UncheckedVoidMethod = {

    def setFields(ins : F[StackNil] => F[StackNil], sig : UncheckedFunSig) : F[StackNil] => F[StackNil] = {
      var setFields = ins
      for ((arg, ind) <- elms.zipWithIndex) {
        setFields = setFields |>>
          sig.getArg[Ref[TupleClass]](0).LOAD |>>
          sig.getArg(ind + 1).LOAD |>>
          getField(ind).PUT_FIELD
      }
      setFields
    }

    cg.mkUncheckedConstructor(ct+:elms,
      sig =>
        setFields(
          sig.getArg(0).asInstanceOf[Local[Ref[TupleClass]]].LOAD[StackNil] |>>
          cg.SUPER, sig) |>>
        RETURN_VOID)
  }

  /**
    * Generate the getIndex interface method. Stores the capability to call the method
    */
   private def getIndexIns[R <: MnemonicsTypes : TypeTag](index : Int) : FunSig1[Ref[TupleClass], R] => F[StackNil] => F[StackNil] =
      sig  =>
        sig.getArg1.LOAD[StackNil] |>>
          getField(index).GET_FIELD |>>
          RETURN

    for ((arg, ind) <- elms.zipWithIndex) {
      arg match {
        case PrimBool => cg.mkMethod1[Ref[TupleClass], MBool]("getIndex" + ind, getIndexIns(ind))
        case PrimChar => cg.mkMethod1[Ref[TupleClass], MChar]("getIndex" + ind, getIndexIns(ind))
        case PrimByte => cg.mkMethod1[Ref[TupleClass], MByte]("getIndex" + ind, getIndexIns(ind))
        case PrimShort => cg.mkMethod1[Ref[TupleClass], MShort]("getIndex" + ind, getIndexIns(ind))
        case PrimInt => cg.mkMethod1[Ref[TupleClass], MInt]("getIndex" + ind, getIndexIns(ind))
        case PrimLong => cg.mkMethod1[Ref[TupleClass], MLong]("getIndex" + ind, getIndexIns(ind))
        case PrimFloat => cg.mkMethod1[Ref[TupleClass], MFloat]("getIndex" + ind, getIndexIns(ind))
        case PrimDouble => cg.mkMethod1[Ref[TupleClass], MDouble]("getIndex" + ind, getIndexIns(ind))
        case Reference(_) => cg.mkMethod1[Ref[TupleClass], Ref[MObject]]("getIndex" + ind, getIndexIns(ind))
        case _ => ???
      }
    }

  def getIndexMethod[T1 <: MnemonicsTypes : TypeTag](index : Int) : Method1[Ref[TupleClass], T1] =
    new Method1[Ref[TupleClass], T1](JvmModifier.InvokeVirtual, ct, "getIndex" + index)
  /**
    * Generate the setIndex interface method. Stores the capability to call the method
    */

    private def setIndexIns[T1 <: MnemonicsTypes : TypeTag](index: Int): FunSig2[Ref[TupleClass], T1, MVoid] => F[StackNil] => F[StackNil] =
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
        sig.getArg2.LOAD |>>
        getField(index).PUT_FIELD |>>
        RETURN_VOID

    for ((arg, ind) <- elms.zipWithIndex) {
      arg match {
        case PrimBool => cg.mkVoidMethod2[Ref[TupleClass], MBool]("setIndex" + ind, setIndexIns(ind))
        case PrimChar => cg.mkVoidMethod2[Ref[TupleClass], MChar]("setIndex" + ind, setIndexIns(ind))
        case PrimByte => cg.mkVoidMethod2[Ref[TupleClass], MByte]("setIndex" + ind, setIndexIns(ind))
        case PrimShort => cg.mkVoidMethod2[Ref[TupleClass], MShort]("setIndex" + ind, setIndexIns(ind))
        case PrimInt => cg.mkVoidMethod2[Ref[TupleClass], MInt]("setIndex" + ind, setIndexIns(ind))
        case PrimLong => cg.mkVoidMethod2[Ref[TupleClass], MLong]("setIndex" + ind, setIndexIns(ind))
        case PrimFloat => cg.mkVoidMethod2[Ref[TupleClass], MFloat]("setIndex" + ind, setIndexIns(ind))
        case PrimDouble => cg.mkVoidMethod2[Ref[TupleClass], MDouble]("setIndex" + ind, setIndexIns(ind))
        case Reference(_) => cg.mkVoidMethod2[Ref[TupleClass], Ref[MObject]]("setIndex" + ind, setIndexIns(ind))
        case _ => ???
      }
    }
    def setIndexMethod[T1 <: MnemonicsTypes : TypeTag](index : Int) : VoidMethod2[Ref[TupleClass], T1] =
      new VoidMethod2[Ref[TupleClass], T1](JvmModifier.InvokeVirtual, ct, "setIndex" + index)

  /**
    * Generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    */
  val toStringMethod: Method1[Ref[TupleClass],Ref[MString]] =
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
  val hashCodeMethod: Method1[Ref[TupleClass], MInt] =
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
  val equalsMethod: Method2[Ref[TupleClass], Ref[MObject], MBool] =
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

