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
import ca.uwaterloo.flix.language.ast.FinalAst.{Def, FormalParam, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, SpecialOperator, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.{Api, GenExpression, NJvmType}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.interfaces.ContinuationInterface
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Label

import scala.reflect.runtime.universe._

class FunctionClass(map: Map[JvmName, MnemonicsClass], sym: Symbol.DefnSym, defn: Def)(implicit root: Root, flix: Flix) extends MnemonicsClass {


  private val MonoType.Arrow(targs, tresult) = defn.tpe
  private val argsTpe = targs.map(getErasedJvmType)
  private val retTpe = getErasedJvmType(tresult)
  private val iFaceTpe: Reference = getFunctionInterfaceType(argsTpe, retTpe)

  private val ct: Reference = getFunctionDefinitionClassType(sym)
  private val cg: ClassGenerator = new ClassGenerator(ct, List(iFaceTpe))

  for ((arg, offset) <- argsTpe.zipWithIndex) {
    //Pattern match on each of the possible arg types and generate the respective field and setArg method
    //We need to pattern match as we want to the field and the method signature to match the types.
    arg match {
      case PrimBool =>
        val field = cg.mkPrimField[MBool]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MBool]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimChar =>
        val field = cg.mkPrimField[MChar]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MChar]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimByte =>
        val field = cg.mkPrimField[MByte]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MByte]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimShort =>
        val field = cg.mkPrimField[MShort]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MShort]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimInt =>
        val field = cg.mkPrimField[MInt]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MInt]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimLong =>
        val field = cg.mkPrimField[MLong]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MLong]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimFloat =>
        val field = cg.mkPrimField[MFloat]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MFloat]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimDouble =>
        val field = cg.mkPrimField[MDouble]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], MDouble]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case Reference(_) =>
        val field = cg.mkField[Ref[MObject]]("arg" + offset)
        cg.mkVoidMethod2[Ref[FunctionClass], Ref[MObject]]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case _ => throw InternalCompilerException(s"Unexpected type $arg")
    }
  }

  /**
    * Method which generates the capability to LOAD/STORE an argument field
    *
    * @param offset of the field we want to obtain
    */
  private def getArgField[T1 <: MnemonicsTypes : TypeTag](offset: Int): Field[T1] =
    new Field[T1]("arg" + offset)

  /**
    * Method which generates the capability to LOAD/STORE an argument field,
    * since it is a prim field it also allows boxing
    *
    * @param offset of the field we want to obtain
    */
  private def getArgPrimField[T1 <: MnemonicsPrimTypes : TypeTag](offset: Int): PrimField[T1] =
    new PrimField[T1]("arg" + offset)

  /**
    * Method which generates the capability to invoke a setArg method,
    *
    * @param offset of the setArg method we want to invoke
    */
  def setArgMethod[T1 <: MnemonicsTypes : TypeTag](offset: Int): VoidMethod2[Ref[FunctionClass], T1] =
    new VoidMethod2(JvmModifier.InvokeVirtual, ct, "setArg" + offset)

    //Generate the result field and the getResult method.
    //Only one method and field are generated
    retTpe match {
    //Pattern match on each of the possible return type and generate the respective field and getResult method
    //We need to pattern match as we want the field and the method signature to match the type.
    case PrimBool =>
      val field = cg.mkPrimField[MBool]("result")
      cg.mkMethod1[Ref[FunctionClass], MBool]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimChar =>
      val field = cg.mkPrimField[MChar]("result")
      cg.mkMethod1[Ref[FunctionClass], MChar]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimByte =>
      val field = cg.mkPrimField[MByte]("result")
      cg.mkMethod1[Ref[FunctionClass], MByte]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimShort =>
      val field = cg.mkPrimField[MShort]("result")
      cg.mkMethod1[Ref[FunctionClass], MShort]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimInt =>
      val field = cg.mkPrimField[MInt]("result")
      cg.mkMethod1[Ref[FunctionClass], MInt]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimLong =>
      val field = cg.mkPrimField[MLong]("result")
      cg.mkMethod1[Ref[FunctionClass], MLong]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimFloat =>
      val field = cg.mkPrimField[MFloat]("result")
      cg.mkMethod1[Ref[FunctionClass], MFloat]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimDouble =>
      val field = cg.mkPrimField[MDouble]("result")
      cg.mkMethod1[Ref[FunctionClass], MDouble]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case Reference(_) =>
      val field = cg.mkField[Ref[MObject]]("result")
      cg.mkMethod1[Ref[FunctionClass], Ref[MObject]]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
  }

  /**
    * Method which generates the capability to LOAD/STORE the result field
    */
  private def getResultField[T1 <: MnemonicsTypes : TypeTag]: Field[T1] =
    new Field[T1]("result")

  /**
    * Method which generates the capability to LOAD/STORE the result field,
    * since it is a prim field it also allow boxing
    */
  private def getResultPrimField[T1 <: MnemonicsPrimTypes : TypeTag]: PrimField[T1] =
    new PrimField[T1]("result")

  /**
    * Method which generates the capability to invoke a getResult method,
    */
  private def getResultMethod[T1 <: MnemonicsPrimTypes : TypeTag]: Method1[Ref[FunctionClass], T1] =
    new Method1(JvmModifier.InvokeVirtual, ct, "getResult")


  val defaultConstructor : VoidMethod1[Ref[FunctionClass]] =
    cg.mkConstructor1(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
        cg.SUPER |>>
        RETURN_VOID
    )

  val invokeMethod : VoidMethod2[Ref[FunctionClass], Ref[Context]] = {
    val enterLabel = new Label
    val mLabel = new MLabel(enterLabel)
    val expTpe = getErasedJvmType(defn.exp.tpe)
    cg.mkVoidMethod2("invoke",
      sig =>
        mLabel.EMIT_LABEL[StackNil]|>>
        defn.formals.zipWithIndex.foldLeft(NO_OP[StackNil]) {
            case (ins, (FormalParam(symb, tpe), ind)) =>
              val erasedType = getErasedJvmType(tpe)
              erasedType match {
                case PrimBool =>
                  val stackLocal = new Local[MBool](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MBool](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimChar =>
                  val stackLocal = new Local[MChar](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MChar](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimByte =>
                  val stackLocal = new Local[MByte](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MByte](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimShort =>
                  val stackLocal = new Local[MShort](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MShort](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimInt =>
                  val stackLocal = new Local[MInt](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MInt](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimLong =>
                  val stackLocal = new Local[MLong](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MLong](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimFloat =>
                  val stackLocal = new Local[MFloat](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MFloat](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimDouble =>
                  val stackLocal = new Local[MDouble](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MDouble](ind).GET_FIELD |>>
                    stackLocal.STORE
                case Reference(_) =>
                  val stackLocal = new Local[Ref[MObject]](symb.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[Ref[MObject]](ind).GET_FIELD |>>
                    stackLocal.STORE
                case _ => throw InternalCompilerException(s"Unexpected type $erasedType")
              }
          } |>>
          (expTpe match {
            case PrimBool =>
              GenExpression.compileExpression[StackNil, MBool](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MBool].PUT_FIELD
            case PrimChar =>
              GenExpression.compileExpression[StackNil, MChar](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MChar].PUT_FIELD
            case PrimByte =>
              GenExpression.compileExpression[StackNil, MByte](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MByte].PUT_FIELD
            case PrimShort =>
              GenExpression.compileExpression[StackNil, MShort](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MShort].PUT_FIELD
            case PrimInt =>
              GenExpression.compileExpression[StackNil, MInt](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MInt].PUT_FIELD
            case PrimLong =>
              GenExpression.compileExpression[StackNil, MLong](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                Unchecked_DUP_X2[StackNil ** MLong ** Ref[FunctionClass], StackNil ** Ref[FunctionClass] ** MLong ** MLong] |>>
                POP |>>
                getResultField[MLong].PUT_FIELD
            case PrimFloat =>
              GenExpression.compileExpression[StackNil, MFloat](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MFloat].PUT_FIELD
            case PrimDouble =>
              GenExpression.compileExpression[StackNil, MDouble](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                Unchecked_DUP_X2[StackNil ** MDouble ** Ref[FunctionClass], StackNil ** Ref[FunctionClass] ** MDouble ** MDouble] |>>
                POP |>>
                getResultField[MDouble].PUT_FIELD
            case Reference(_) =>
              GenExpression.compileExpression[StackNil, Ref[MObject]](defn.exp, map, Map(), enterLabel) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[Ref[MObject]].PUT_FIELD
            case _ => throw InternalCompilerException(s"Unexpected type $expTpe")
          }) |>>
          RETURN_VOID
    )
  }

  val evalMethod : Method2[Ref[FunctionClass], Ref[Context], Ref[MObject]] = {
    val contextLocal = new Local[Ref[Context]](1)
    val continuationField = new UncheckedClassField("continuation", getContextClassType, NJvmType.Object)

    // Type of the function
    val fnType = root.defs(defn.sym).tpe

    // Type of the continuation interface
    val cont =
      retTpe match {
        case PrimBool =>
          getContinuationInterfaceType[MBool]
        case PrimChar =>
          getContinuationInterfaceType[MChar]
        case PrimByte =>
          getContinuationInterfaceType[MByte]
        case PrimShort =>
          getContinuationInterfaceType[MShort]
        case PrimInt =>
          getContinuationInterfaceType[MInt]
        case PrimLong =>
          getContinuationInterfaceType[MLong]
        case PrimFloat =>
          getContinuationInterfaceType[MFloat]
        case PrimDouble =>
          getContinuationInterfaceType[MDouble]
        case Reference(_) =>
          getContinuationInterfaceType[Ref[MObject]]
        case _ => throw InternalCompilerException(s"Unexpected type $fnType")
      }

    cg.mkMethod2("eval",
      sig =>
        sig.getArg2.LOAD[StackNil] |>>
          sig.getArg1.LOAD |>>
          continuationField.PUT_FIELD |>>
          IFNONNULL(
            contextLocal.LOAD[StackNil] |>>
              continuationField.GET_FIELD[StackNil,Ref[Context], Ref[MObject]] |>>

              contextLocal.LOAD[StackNil ** Ref[MObject]] |>>
              CONST_NULL[StackNil ** Ref[MObject] ** Ref[Context], Ref[MObject]] |>>
              continuationField.PUT_FIELD[StackNil ** Ref[MObject], Ref[Context], Ref[MObject]] |>>
              (retTpe match {

                case PrimBool =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MBool]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimChar =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MChar]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimByte =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].invokeMethod
                  CHECK_CAST2[StackNil, Ref[MObject], Ref[ContinuationInterface[MByte]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimShort =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MShort]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimInt =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MInt]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimLong =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MLong]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimFloat =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MFloat]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimDouble =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[MDouble]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case Reference(_) =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].invokeMethod
                  CHECK_CAST2[StackNil , Ref[MObject], Ref[ContinuationInterface[Ref[MObject]]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
              }) |>>

              contextLocal.LOAD |>>
              continuationField.GET_FIELD
          ) |>>

          (retTpe match {
            case PrimBool =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Boolean.valueOf.INVOKE |>>
                RETURN

            case PrimChar =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Character.valueOf.INVOKE |>>
                RETURN
            case PrimByte =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Byte.valueOf.INVOKE |>>
                RETURN
            case PrimShort =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Short.valueOf.INVOKE |>>
                RETURN
            case PrimInt =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Integer.valueOf.INVOKE |>>
                RETURN
            case PrimLong =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Long.valueOf.INVOKE |>>
                RETURN
            case PrimFloat =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Float.valueOf.INVOKE |>>
                RETURN

            case PrimDouble =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Double.valueOf.INVOKE |>>
                RETURN

            case Reference(_) =>
              val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].getResultMethod
              ifoLocal.LOAD[StackNil] |>>
                getResultMethod.INVOKE |>>
                RETURN
            case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
          })
    )
  }

  val applyMethod : Method2[Ref[FunctionClass], Ref[MObject], Ref[MObject]] = {

    val contextLocal = new Local[Ref[Context]](1)
    val arrayLocal = new Local[MArray[Ref[MObject]]](2)
    val contextClass = map(getContextClassType.name).asInstanceOf[Context]

    cg.mkMethod2("apply",
      sig =>
        sig.getArg2.LOAD[StackNil] |>>
        CHECK_CAST_ARRAY[StackNil, Ref[MObject], MArray[Ref[MObject]]](NJvmType.Object) |>>
        arrayLocal.STORE[StackNil] |>>
          defn.formals.zipWithIndex.foldLeft(NO_OP[StackNil]) {
            case (ins, (FormalParam(_, tpe), ind)) =>
              val erasedType = getErasedJvmType(tpe)
              erasedType match {
                case PrimBool =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MBool]](Reference(JvmName.Boolean)) |>>
                    Api.Java.Lang.Boolean.booleanValue.INVOKE |>>
                    setArgMethod[MBool](ind).INVOKE
                case PrimChar =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MChar]](Reference(JvmName.Character)) |>>
                    Api.Java.Lang.Character.charValue.INVOKE |>>
                    setArgMethod[MChar](ind).INVOKE
                case PrimByte =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MByte]](Reference(JvmName.Byte)) |>>
                    Api.Java.Lang.Byte.byteValue.INVOKE |>>
                    setArgMethod[MByte](ind).INVOKE
                case PrimShort =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MShort]](Reference(JvmName.Short)) |>>
                    Api.Java.Lang.Short.shortValue.INVOKE |>>
                    setArgMethod[MShort](ind).INVOKE
                case PrimInt =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MInt]](Reference(JvmName.Integer)) |>>
                    Api.Java.Lang.Integer.intValue.INVOKE |>>
                    setArgMethod[MInt](ind).INVOKE
                case PrimLong =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MLong]](Reference(JvmName.Long)) |>>
                    Api.Java.Lang.Long.longValue.INVOKE |>>
                    setArgMethod[MLong](ind).INVOKE
                case PrimFloat =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MFloat]](Reference(JvmName.Float)) |>>
                    Api.Java.Lang.Float.floatValue.INVOKE |>>
                    setArgMethod[MFloat](ind).INVOKE
                case PrimDouble =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MDouble]](Reference(JvmName.Boolean)) |>>
                    Api.Java.Lang.Double.doubleValue.INVOKE |>>
                    setArgMethod[MDouble](ind).INVOKE
                case Reference(name) =>
                  ins |>>
                    sig.getArg1.LOAD[StackNil] |>>
                    arrayLocal.LOAD |>>
                    LDC_INT(ind) |>>
                    AALOAD[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]] |>>
                    CHECK_CAST2[StackNil ** Ref[FunctionClass], Ref[MObject], Ref[MObject]](Reference(name)) |>>
                    setArgMethod[Ref[MObject]](ind).INVOKE
                case _ => throw InternalCompilerException(s"Unexpected type $erasedType")
              }
          } |>>
          sig.getArg1.LOAD |>>
          NEW[StackNil ** Ref[FunctionClass], Ref[Context]](NJvmType.Context) |>>
          DUP |>>
          contextClass.defaultConstrutor.INVOKE |>>
          DUP |>>
          contextLocal.STORE |>>
          evalMethod.INVOKE |>>
          // Construct a proxy object.
          (tresult match {
            case arrayType: MonoType.Array =>
              val jvmTpe = getErasedJvmType(arrayType.tpe)
              (jvmTpe match{
                case PrimBool => newProxyArray[StackNil, MBool](arrayType)
                case PrimChar => newProxyArray[StackNil, MChar](arrayType)
                case PrimByte => newProxyArray[StackNil, MByte](arrayType)
                case PrimShort => newProxyArray[StackNil, MShort](arrayType)
                case PrimInt => newProxyArray[StackNil, MInt](arrayType)
                case PrimLong => newProxyArray[StackNil, MLong](arrayType)
                case PrimFloat => newProxyArray[StackNil, MFloat](arrayType)
                case PrimDouble => newProxyArray[StackNil, MDouble](arrayType)
                case Reference(_) => newProxyArray[StackNil, Ref[MObject]](arrayType)
                case _ => throw InternalCompilerException(s"Unexpected type $tresult")
              }).asInstanceOf[F[StackNil ** Ref[MObject]] => F[StackNil ** Ref[MProxyObject]]]
            case _ =>
              newProxyObject[StackNil](tresult)
          }) |>>
        RETURN
    )
  }
  private def newProxyArray[S <: Stack, T <: MnemonicsTypes : TypeTag](tpe: MonoType.Array)(implicit root: Root, flix: Flix): F[S ** Ref[MObject]] => F[S ** MArray[Ref[MProxyObject]]] = {

    val elementType = tpe.tpe

    val jvmTpe = getJvmType[T]
    // The local variable index of the original array.

    // The local variable index of the new array.
    val resultArray = new ArrayLocal[Ref[MProxyObject]](3)

    // The local variable index of the loop counter.
    val loopCounter = new Local[MInt](4)
    jvmTpe match{
      case Reference(_) =>
        val arrayLocal = new ArrayLocal[Ref[MObject]](2)
        CHECK_CAST_ARRAY2[S, Ref[MObject], MArray[Ref[MObject]]](getArrayType(jvmTpe)) |>>
          arrayLocal.STORE_ARRAY |>>
          arrayLocal.LOAD_ARRAY |>>
          ARRAYLENGTH |>>
          NEWARRAY[S, MProxyObject] |>>
          resultArray.STORE_ARRAY |>>
          FOR_GE(
            //Loop init
            ICONST[S](0) |>>
              loopCounter.STORE,
            //Loop test
            loopCounter.LOAD[S] |>>
              resultArray.LOAD_ARRAY |>>
              ARRAYLENGTH,
            //Loop body
            resultArray.LOAD_ARRAY[S] |>>
              loopCounter.LOAD |>>
              arrayLocal.LOAD_ARRAY |>>
              loopCounter.LOAD |>>
              arrayLocal.LOAD_ARRAY_ELEMENT |>>
              newProxyObject(elementType) |>>
              resultArray.STORE_ARRAY_ELEMENT |>>
              loopCounter.LOAD |>>
              ICONST(1) |>>
              IADD |>>
              loopCounter.STORE
          ) |>>
          resultArray.LOAD_ARRAY
      case _ =>
        val arrayLocal = new ArrayLocal[T](2)
        CHECK_CAST_ARRAY2[S, Ref[MObject], MArray[T]](getArrayType(jvmTpe)) |>>
        arrayLocal.STORE_ARRAY |>>
        arrayLocal.LOAD_ARRAY |>>
        ARRAYLENGTH |>>
        NEWARRAY[S, MProxyObject] |>>
        resultArray.STORE_ARRAY |>>
        FOR_GE(
          //Loop init
          ICONST[S](0) |>>
            loopCounter.STORE,
          //Loop test
          loopCounter.LOAD[S] |>>
            resultArray.LOAD_ARRAY |>>
            ARRAYLENGTH,
          //Loop body
          resultArray.LOAD_ARRAY[S] |>>
            loopCounter.LOAD |>>
            arrayLocal.LOAD_ARRAY |>>
            loopCounter.LOAD |>>
            arrayLocal.LOAD_ARRAY_ELEMENT |>>
            box[S ** MArray[Ref[MProxyObject]] ** MInt, T, Ref[MObject]] |>>
            newProxyObject(elementType) |>>
            resultArray.STORE_ARRAY_ELEMENT |>>
            loopCounter.LOAD |>>
            ICONST(1) |>>
            IADD |>>
            loopCounter.STORE
        ) |>>
        resultArray.LOAD_ARRAY
    }

  }

  private def newProxyObject[S <: Stack](tpe: MonoType)(implicit root: Root, flix: Flix): F[S ** Ref[MObject]] => F[S ** Ref[MProxyObject]] = {
    // Construct the equal function object.
    (root.specialOps(SpecialOperator.Equality).get(tpe) match {
        case None => CONST_NULL[S ** Ref[MObject] ,Ref[MFunction]]
        case Some(hashSym) => compileDefSymbol[S**Ref[MObject]](hashSym)
    })|>>
    // Construct the hash function object.
    (root.specialOps(SpecialOperator.HashCode).get(tpe) match {
        case None => CONST_NULL[S ** Ref[MObject]**Ref[MFunction] ,Ref[MFunction]]
        case Some(hashSym) => compileDefSymbol[S ** Ref[MObject]**Ref[MFunction]](hashSym)
    })|>>
    // Construct the toStr function object.
    (root.specialOps(SpecialOperator.ToString).get(tpe) match {
      case None => CONST_NULL[S ** Ref[MObject]**Ref[MFunction] ** Ref[MFunction] ,Ref[MFunction]]
      case Some(hashSym) => compileDefSymbol[S ** Ref[MObject]**Ref[MFunction] ** Ref[MFunction]](hashSym)
    })|>>
    Api.Java.Runtime.ProxyObject.of.INVOKE
  }

  private def compileDefSymbol[S <: Stack](sym: Symbol.DefnSym)(implicit root: Root, flix: Flix):
  F[S] => F[S ** Ref[MFunction]] = {
    // Retrieve the namespace of the def symbol.
    val ns = getNamespace(sym)

    // Retrieve the JVM type of the namespace.
    val nsJvmType = getNamespaceClassType(ns)

    // Retrieve the name of the namespace field on the context object.
    val nsFieldName = getNamespaceFieldNameInContextClass(ns)

    // Retrieve the name of the def on the namespace object.
    val defFieldName = getDefFieldNameInNamespaceClass(sym)

    // Retrieve the type of the function def class.
    val defJvmType = getFunctionDefinitionClassType(sym)

    val contextLocal = new Local[Ref[Context]](1)
    val namespaceObjField = new UncheckedClassField(nsFieldName, NJvmType.Context, nsJvmType)
    val defObjField = new UncheckedClassField(defFieldName, nsJvmType, defJvmType)

    // Load the current context.
    contextLocal.LOAD[S] |>>
    namespaceObjField.GET_FIELD[S, Ref[Context], Ref[MObject]] |>>
    defObjField.GET_FIELD[S, Ref[MObject], Ref[MFunction]]

  }

  private def box[S <: Stack, T1 <: MnemonicsTypes : TypeTag, T2 <: Ref[_]](implicit root: Root, flix: Flix): F[S ** T1] => F[S ** T2] = {
    val jvmTpe = getJvmType[T1]
    (jvmTpe match {
      case PrimBool =>
        Api.Java.Lang.Boolean.valueOf.INVOKE[S]
      case PrimChar =>
        Api.Java.Lang.Character.valueOf.INVOKE[S]
      case PrimByte =>
        Api.Java.Lang.Byte.valueOf.INVOKE[S]
      case PrimShort =>
        Api.Java.Lang.Short.valueOf.INVOKE[S]
      case PrimInt =>
        Api.Java.Lang.Integer.valueOf.INVOKE[S]
      case PrimLong =>
        Api.Java.Lang.Long.valueOf.INVOKE[S]
      case PrimFloat =>
        Api.Java.Lang.Float.valueOf.INVOKE[S]
      case PrimDouble =>
        Api.Java.Lang.Double.valueOf.INVOKE[S]
      case _ => throw InternalCompilerException(s"Unexpected type $jvmTpe")
    }).asInstanceOf
  }

  private val jvmClass: JvmClass =
    JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

