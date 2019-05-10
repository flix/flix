package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes.{MArray, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
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
        case PrimBool => cg.mkPrimField[MBool]("field" + ind)
        case PrimChar => cg.mkPrimField[MChar]("field" + ind)
        case PrimByte => cg.mkPrimField[MByte]("field" + ind)
        case PrimShort => cg.mkPrimField[MShort]("field" + ind)
        case PrimInt => cg.mkPrimField[MInt]("field" + ind)
        case PrimLong => cg.mkPrimField[MLong]("field" + ind)
        case PrimFloat => cg.mkPrimField[MFloat]("field" + ind)
        case PrimDouble => cg.mkPrimField[MDouble]("field" + ind)
        case Reference(_) => cg.mkField[Ref[MObject]]("field" + ind)
        case _ => ???
      }
  }

  private def getField[T1 <: MnemonicsTypes : TypeTag](ind : Int) : Field[T1] =
    new Field[T1]("field" + ind)

  private def getPrimField[T1 <: MnemonicsPrimTypes : TypeTag](ind : Int) : PrimField[T1] =
    new PrimField[T1]("field" + ind)

  val defaultConstrutor : UncheckedVoidMethod = {


    val setFields =
      (sig : UncheckedFunSig)  =>{
        var ins = NO_OP[StackNil]
        var ind: Int = 1
        for (arg <- elms) {
          ins = ins |>>
          sig.getArg[Ref[TupleClass]](0).LOAD |>>
          (arg match {
            case PrimBool => sig.getArg[MBool](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD
            case PrimChar =>  sig.getArg[MChar](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case PrimByte =>   sig.getArg[MByte](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case PrimShort => sig.getArg[MShort](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case PrimInt =>   sig.getArg[MInt](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case PrimLong =>   sig.getArg[MLong](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case PrimFloat =>  sig.getArg[MFloat](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case PrimDouble =>   sig.getArg[MDouble](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getPrimField(ind).PUT_FIELD
            case Reference(_) =>   sig.getArg[Ref[MObject]](ind).LOAD[StackNil ** Ref[TupleClass]] |>>
              getField(ind).PUT_FIELD
            case _ => ???
          })
          arg match {
            case PrimLong | PrimDouble => ind += 2
            case _ => ind += 1
          }
        }
        ins
      }

    cg.mkUncheckedConstructor(ct+:elms,
      sig =>
          sig.getArg(0).asInstanceOf[Local[Ref[TupleClass]]].LOAD[StackNil] |>>
          cg.SUPER |>>
          setFields(sig) |>>
        RETURN_VOID
    )
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


  val getBoxedValueMethod : Method1[Ref[TupleClass], MArray[MObject]] = {

    cg.mkMethod1("getBoxedValue",
      sig =>
        {
          var ins  =  LDC_INT[StackNil](elms.length) |>> NEWARRAY[StackNil, MObject]
          for ((arg, ind) <- elms.zipWithIndex) {
            ins = ins |>>
              DUP |>>
              LDC_INT(ind) |>>
              (arg match {
                case PrimBool => getPrimField[MBool](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimChar => getPrimField[MChar](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimByte => getPrimField[MByte](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimShort => getPrimField[MShort](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimInt => getPrimField[MInt](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimLong => getPrimField[MLong](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimFloat => getPrimField[MFloat](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case PrimDouble => getPrimField[MDouble](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt]|>> AASTORE
                case Reference(_) =>  sig.getArg1.LOAD[StackNil ** MArray[Ref[MObject]]  ** MArray[Ref[MObject]] ** MInt] |>>
                  getField[Ref[MObject]](ind).GET_FIELD|>> AASTORE
                case _ => ???
              })
          }
          ins |>> RETURN
        }
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

