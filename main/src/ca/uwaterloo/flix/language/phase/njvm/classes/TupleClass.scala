/*
 * Copyright 2019 Miguel Fialho
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes.{MArray, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType.{PrimBool, PrimByte, PrimChar, PrimDouble, PrimFloat, PrimInt, PrimLong, PrimShort, Reference}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe._

/**
  * Generates a tupleClass
  */
class TupleClass(map: Map[JvmName, MnemonicsClass], elms: List[NJvmType])(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Initiliaze the reference and the class generator
  private val ct: Reference = getTupleClassType(elms)
  //The class implements the respective tupleInterface
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getTupleInterfaceType(elms)))

  //The tuple class has a variable number of fields
  //Therefore we lopp through all of the elements and for each of them we generate a field
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
      case _ => throw InternalCompilerException(s"Unexpected type $arg")
    }
  }

  /**
    * Method which generates the capability to LOAD/STORE the result field
    */
  private def getField[T1 <: MnemonicsTypes : TypeTag](ind: Int): Field[T1] =
    new Field[T1]("field" + ind)

  /**
    * Method which generates the capability to LOAD/STORE the result field,
    * since it is a prim field it also allow boxing
    */
  private def getPrimField[T1 <: MnemonicsPrimTypes : TypeTag](ind: Int): PrimField[T1] =
    new PrimField[T1]("field" + ind)

  /**
    * Generate the constructor for the tuple class. Number of arguments on this constructor is equal number
    * of elements in the tuple and each argument corresponds to an element of the tuple with the appropriate type.
    * For example for tuple (Char, Int8) we create the following constructor:
    *
    * public Tuple(char var1, byte var2) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    * }
    */
  val defaultConstrutor: UncheckedVoidMethod = {
    //This method is unchecked as we don't know at compile time the number of arguments
    //The constructor calls super and sets each of the fields
    val setFields =
    (sig: UncheckedFunSig) => {
      //We need initialize each of the fields in the class
      //Therefore we need to accumulate the instructions
      //To do this we use a foldleft.
      //Note we also acumulate on the offset this is because long and double take double the space
      //in the stack
      val (inst, _) = elms.zipWithIndex.foldLeft(NO_OP[StackNil], 1) {
        case ((ins, offset), (arg, ind)) =>
          //We pattern match on the arg type here to ensure the correctness of types
          arg match {
            //For each of the cases we first load this then the respective arg
            //And we put the value in the correct field.
            case PrimBool =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MBool](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 1)
            case PrimChar =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MChar](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 1)
            case PrimByte =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MByte](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 1)
            case PrimShort =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MShort](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 1)
            case PrimInt =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MInt](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 1)
            case PrimLong =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MLong](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 2)
            case PrimFloat =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MFloat](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 1)
            case PrimDouble =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[MDouble](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getPrimField(ind).PUT_FIELD, offset + 2)
            case Reference(_) =>
              (ins |>>
                sig.getArg[Ref[TupleClass]](0).LOAD |>>
                sig.getArg[Ref[MObject]](offset).LOAD[StackNil ** Ref[TupleClass]] |>>
                getField(ind).PUT_FIELD, offset + 1)
            case _ => throw InternalCompilerException(s"Unexpected type $arg")
          }
      }
      inst
    }

    //generate the constructor
    cg.mkUncheckedConstructor(ct +: elms,
      sig =>
        sig.getArg(0).asInstanceOf[Local[Ref[TupleClass]]].LOAD[StackNil] |>>
          cg.SUPER |>>
          setFields(sig) |>>
          RETURN_VOID
    )
  }

  /**
    * Generate the getIndex method instruction for a given index
    */
  private def getIndexIns[R <: MnemonicsTypes : TypeTag](index: Int): FunSig1[Ref[TupleClass], R] => F[StackNil] => F[StackNil] =
    sig =>
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
      case _ => throw InternalCompilerException(s"Unexpected type $arg")
    }
  }

  /**
    * Generate the capability to invoke the get index method
    */
  def getIndexMethod[T1 <: MnemonicsTypes : TypeTag](index: Int): Method1[Ref[TupleClass], T1] =
    new Method1[Ref[TupleClass], T1](JvmModifier.InvokeVirtual, ct, "getIndex" + index)

  /**
    * Generate the setIndex method instruction for a given index
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
      case _ => throw InternalCompilerException(s"Unexpected type $arg")
    }
  }

  /**
    * Generate the capability to invoke the set index method
    */
  def setIndexMethod[T1 <: MnemonicsTypes : TypeTag](index: Int): VoidMethod2[Ref[TupleClass], T1] =
    new VoidMethod2[Ref[TupleClass], T1](JvmModifier.InvokeVirtual, ct, "setIndex" + index)


  /**
    * Generate the code for `getBoxedValue()` method. The method should return an array of objects containing all the
    * elements of the tuple in the same order that they appear on the tuple but if the element is a primitive then it will
    * box the value.
    */
  val getBoxedValueMethod: Method1[Ref[TupleClass], MArray[MObject]] = {

    cg.mkMethod1("getBoxedValue",
      sig =>
        elms.zipWithIndex.foldLeft(
          LDC_INT[StackNil](elms.length) |>>
            NEWARRAY[StackNil, MObject]) {
          case (ins, (arg, ind)) =>
            ins |>>
              DUP |>>
              LDC_INT(ind) |>>
              (arg match {
                case PrimBool =>
                  getPrimField[MBool](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimChar =>
                  getPrimField[MChar](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimByte =>
                  getPrimField[MByte](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimShort =>
                  getPrimField[MShort](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimInt =>
                  getPrimField[MInt](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimLong =>
                  getPrimField[MLong](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimFloat =>
                  getPrimField[MFloat](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case PrimDouble =>
                  getPrimField[MDouble](ind).GET_BOXED_FIELD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    AASTORE
                case Reference(_) =>
                  sig.getArg1.LOAD[StackNil ** MArray[Ref[MObject]] ** MArray[Ref[MObject]] ** MInt] |>>
                    getField[Ref[MObject]](ind).GET_FIELD |>>
                    AASTORE
                case _ => throw InternalCompilerException(s"Unexpected type $arg")
              })
        } |>> RETURN
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
  val toStringMethod: Method1[Ref[TupleClass], Ref[MString]] =
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

