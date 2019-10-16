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
import ca.uwaterloo.flix.language.ast.FinalAst.{FormalParam, FreeVar, Root}
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.{ClosureInfo, JvmClass, JvmName, JvmOps}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm._
import ca.uwaterloo.flix.language.phase.njvm.Api
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.interfaces.ContinuationInterface
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Label

import scala.reflect.runtime.universe._

/**
  * Class that generates the closure classes
  * It receives all the necessary informantion through the parameter closure
  *
  * @param closure contains all the information about the closure
  */
class Closure(map: Map[JvmName, MnemonicsClass], closure: ClosureInfo)(implicit root: Root, flix: Flix) extends MnemonicsClass {

  //Extract the arguments and the result type from the closure
  private val MonoType.Arrow(targs, tresult) = closure.tpe
  //Erase the arguments, result and the free vars types of the closure
  private val argsTpe = targs.map(getErasedJvmType)
  private val retTpe = getErasedJvmType(tresult)
  private val cloFreevarsTpes = closure.freeVars.map(freeVar => getErasedJvmType(freeVar.tpe))
  //Get the class reference by providing the closure information
  private val ct: Reference = getClosureClassType(closure)
  //Instantiate the class generator. Closures implment the spawnable interface and the respective function interface
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getFunctionInterfaceType(argsTpe, retTpe), NJvmType.Spawnable))

  //Generate the creation context field, the field is of type Object.
  private val creationContext: Field[Ref[MObject]] = cg.mkField("creationContext")

  //For each of the free vars generate a field. If it is a prim type generate a prim field as it can be boxed
  for ((arg, ind) <- cloFreevarsTpes.zipWithIndex) {
    arg match {
      case PrimBool => cg.mkPrimField[MBool]("clo" + ind)
      case PrimChar => cg.mkPrimField[MChar]("clo" + ind)
      case PrimByte => cg.mkPrimField[MByte]("clo" + ind)
      case PrimShort => cg.mkPrimField[MShort]("clo" + ind)
      case PrimInt => cg.mkPrimField[MInt]("clo" + ind)
      case PrimLong => cg.mkPrimField[MLong]("clo" + ind)
      case PrimFloat => cg.mkPrimField[MFloat]("clo" + ind)
      case PrimDouble => cg.mkPrimField[MDouble]("clo" + ind)
      case Reference(_) => cg.mkField[Ref[MObject]]("clo" + ind)
      case _ => throw InternalCompilerException(s"Unexpected type $arg")
    }
  }

  /**
    * Method which generates the capability to LOAD/STORE a free var field
    *
    * @param offset of the field we want to obtain
    */
  private def getCloField[T1 <: MnemonicsTypes : TypeTag](offset: Int): Field[T1] =
    new Field[T1]("clo" + offset)

  /**
    * Method which generates the capability to LOAD/STORE a free var field,
    * since it is a prim field it also allows boxing
    *
    * @param offset of the field we want to obtain
    */
  private def getCloPrimField[T1 <: MnemonicsPrimTypes : TypeTag](offset: Int): PrimField[T1] =
    new PrimField[T1]("clo" + offset)

  //For each of the closure arguments generate the field and a set argument method.
  for ((arg, offset) <- argsTpe.zipWithIndex) {
    //Pattern match on each of the possible arg types and generate the respective field and setArg method
    //We need to pattern match as we want to the field and the method signature to match the types.
    arg match {
      case PrimBool =>
        val field = cg.mkPrimField[MBool]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MBool]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimChar =>
        val field = cg.mkPrimField[MChar]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MChar]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimByte =>
        val field = cg.mkPrimField[MByte]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MByte]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimShort =>
        val field = cg.mkPrimField[MShort]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MShort]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimInt =>
        val field = cg.mkPrimField[MInt]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MInt]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimLong =>
        val field = cg.mkPrimField[MLong]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MLong]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimFloat =>
        val field = cg.mkPrimField[MFloat]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MFloat]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case PrimDouble =>
        val field = cg.mkPrimField[MDouble]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], MDouble]("setArg" + offset,
          sig =>
            sig.getArg1.LOAD[StackNil] |>>
              sig.getArg2.LOAD |>>
              field.PUT_FIELD |>>
              RETURN_VOID
        )
      case Reference(_) =>
        val field = cg.mkField[Ref[MObject]]("arg" + offset)
        cg.mkVoidMethod2[Ref[Closure], Ref[MObject]]("setArg" + offset,
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
  def setArgMethod[T1 <: MnemonicsTypes : TypeTag](offset: Int): VoidMethod2[Ref[Closure], T1] =
    new VoidMethod2(JvmModifier.InvokeVirtual, ct, "setArg" + offset)

  //Generate the result field and the getResult method.
  //Only one method and field are generated
  retTpe match {
    //Pattern match on each of the possible return type and generate the respective field and getResult method
    //We need to pattern match as we want the field and the method signature to match the type.
    case PrimBool =>
      val field = cg.mkPrimField[MBool]("result")
      cg.mkMethod1[Ref[Closure], MBool]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimChar =>
      val field = cg.mkPrimField[MChar]("result")
      cg.mkMethod1[Ref[Closure], MChar]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimByte =>
      val field = cg.mkPrimField[MByte]("result")
      cg.mkMethod1[Ref[Closure], MByte]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimShort =>
      val field = cg.mkPrimField[MShort]("result")
      cg.mkMethod1[Ref[Closure], MShort]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimInt =>
      val field = cg.mkPrimField[MInt]("result")
      cg.mkMethod1[Ref[Closure], MInt]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimLong =>
      val field = cg.mkPrimField[MLong]("result")
      cg.mkMethod1[Ref[Closure], MLong]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimFloat =>
      val field = cg.mkPrimField[MFloat]("result")
      cg.mkMethod1[Ref[Closure], MFloat]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case PrimDouble =>
      val field = cg.mkPrimField[MDouble]("result")
      cg.mkMethod1[Ref[Closure], MDouble]("getResult",
        sig =>
          sig.getArg1.LOAD[StackNil] |>>
            field.GET_FIELD |>>
            RETURN
      )
    case Reference(_) =>
      val field = cg.mkField[Ref[MObject]]("result")
      cg.mkMethod1[Ref[Closure], Ref[MObject]]("getResult",
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
  private def getResultMethod[T1 <: MnemonicsPrimTypes : TypeTag]: Method1[Ref[Closure], T1] =
    new Method1(JvmModifier.InvokeVirtual, ct, "getResult")


  val invokeMethod: VoidMethod2[Ref[Closure], Ref[Context]] = {

    val defn = root.defs(closure.sym)
    // Free variables
    val frees = defn.formals.take(closure.freeVars.length).map(x => FreeVar(x.sym, x.tpe))

    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - closure.freeVars.length)
    cg.mkVoidMethod2("invoke",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          DUP |>>
          IF_ACMPEQ(
            NEW[StackNil, Ref[MObject]](Reference(JvmName.Exception)) |>>
              DUP |>>
              LDC_STRING("Closure is called with a different Context") |>>
              Api.Java.Lang.Exception.constructor.INVOKE |>>
              THROW
          ) |>>
          frees.zipWithIndex.foldLeft(NO_OP[StackNil]) {
            case (ins, (FreeVar(sym, tpe), ind)) =>
              val erasedType = getErasedJvmType(tpe)
              erasedType match {
                case PrimBool =>
                  val stackLocal = new Local[MBool](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MBool](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimChar =>
                  val stackLocal = new Local[MChar](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MChar](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimByte =>
                  val stackLocal = new Local[MByte](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MByte](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimShort =>
                  val stackLocal = new Local[MShort](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MShort](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimInt =>
                  val stackLocal = new Local[MInt](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MInt](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimLong =>
                  val stackLocal = new Local[MLong](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MLong](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimFloat =>
                  val stackLocal = new Local[MFloat](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MFloat](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimDouble =>
                  val stackLocal = new Local[MDouble](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[MDouble](ind).GET_FIELD |>>
                    stackLocal.STORE
                case Reference(_) =>
                  val stackLocal = new Local[Ref[MObject]](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getCloField[Ref[MObject]](ind).GET_FIELD |>>
                    stackLocal.STORE
                case _ => throw InternalCompilerException(s"Unexpected type $erasedType")
              }
          } |>>
          params.zipWithIndex.foldLeft(NO_OP[StackNil]) {
            case (ins, (FormalParam(sym, tpe), ind)) =>
              val erasedType = getErasedJvmType(tpe)
              erasedType match {
                case PrimBool =>
                  val stackLocal = new Local[MBool](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MBool](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimChar =>
                  val stackLocal = new Local[MChar](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MChar](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimByte =>
                  val stackLocal = new Local[MByte](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MByte](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimShort =>
                  val stackLocal = new Local[MShort](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MShort](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimInt =>
                  val stackLocal = new Local[MInt](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MInt](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimLong =>
                  val stackLocal = new Local[MLong](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MLong](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimFloat =>
                  val stackLocal = new Local[MFloat](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MFloat](ind).GET_FIELD |>>
                    stackLocal.STORE
                case PrimDouble =>
                  val stackLocal = new Local[MDouble](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[MDouble](ind).GET_FIELD |>>
                    stackLocal.STORE
                case Reference(_) =>
                  val stackLocal = new Local[Ref[MObject]](sym.getStackOffset + 3)
                  ins |>>
                    sig.getArg1.LOAD |>>
                    getArgField[Ref[MObject]](ind).GET_FIELD |>>
                    stackLocal.STORE
                case _ => throw InternalCompilerException(s"Unexpected type $erasedType")
              }
          } |>>
          (retTpe match {
            case PrimBool =>
              GenExpression.compileExpression[StackNil, MBool](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MBool].PUT_FIELD
            case PrimChar =>
              GenExpression.compileExpression[StackNil, MChar](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MChar].PUT_FIELD
            case PrimByte =>
              GenExpression.compileExpression[StackNil, MByte](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MByte].PUT_FIELD
            case PrimShort =>
              GenExpression.compileExpression[StackNil, MShort](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MShort].PUT_FIELD
            case PrimInt =>
              GenExpression.compileExpression[StackNil, MInt](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MInt].PUT_FIELD
            case PrimLong =>
              GenExpression.compileExpression[StackNil, MLong](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                Unchecked_DUP_X2[StackNil ** MLong ** Ref[Closure], StackNil ** Ref[Closure] ** MLong ** MLong] |>>
                POP |>>
                getResultField[MLong].PUT_FIELD
            case PrimFloat =>
              GenExpression.compileExpression[StackNil, MFloat](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[MFloat].PUT_FIELD
            case PrimDouble =>
              GenExpression.compileExpression[StackNil, MDouble](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                Unchecked_DUP_X2[StackNil ** MDouble ** Ref[Closure], StackNil ** Ref[Closure] ** MDouble ** MDouble] |>>
                POP |>>
                getResultField[MDouble].PUT_FIELD
            case Reference(_) =>
              GenExpression.compileExpression[StackNil, Ref[MObject]](defn.exp, map, Map(), new Label()) |>>
                sig.getArg1.LOAD |>>
                SWAP |>>
                getResultField[Ref[MObject]].PUT_FIELD
            case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
          }) |>>
          RETURN_VOID
    )
  }

  val spawnMethod: VoidMethod1[Ref[MObject]] = {
    val contextClass = map(getContextClassType.name).asInstanceOf[Context]
    val contextLocal = new Local[Ref[Context]](1)
    val continuationField = new ClassField[Ref[MObject]]("continuation", getContextClassType)

    val defn = root.defs(closure.sym)
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

    cg.mkVoidMethod1("spawn",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          NEW[StackNil ** Ref[MObject], Ref[Context]](NJvmType.Context) |>>
          DUP |>>
          contextClass.defaultConstrutor.INVOKE |>>
          contextLocal.STORE |>>

          contextLocal.LOAD |>>
          sig.getArg1.LOAD |>>
          continuationField.PUT_FIELD |>>
          IFNONNULL(
            contextLocal.LOAD[StackNil ** Ref[MObject]] |>>
              continuationField.GET_FIELD |>>

              contextLocal.LOAD[StackNil ** Ref[MObject] ** Ref[MObject]] |>>
              CONST_NULL[StackNil ** Ref[MObject] ** Ref[MObject] ** Ref[Context], Ref[MObject]] |>>
              continuationField.PUT_FIELD |>>
              (retTpe match {

                case PrimBool =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MBool]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MBool]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MBool]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimChar =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MChar]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimByte =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MByte]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimShort =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MShort]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimInt =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MInt]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimLong =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MLong]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimFloat =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MFloat]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case PrimDouble =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[MDouble]]](cont) |>>
                    DUP |>>
                    ifoLocal.STORE |>>
                    contextLocal.LOAD |>>
                    invokeMethod.INVOKE
                case Reference(_) =>
                  val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
                  val invokeMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].invokeMethod
                  CHECK_CAST2[StackNil ** Ref[MObject], Ref[MObject], Ref[ContinuationInterface[Ref[MObject]]]](cont) |>>
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
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Boolean.valueOf.INVOKE |>>
                RETURN_VOID2

            case PrimChar =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MChar]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MChar]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Character.valueOf.INVOKE |>>
                RETURN_VOID2

            case PrimByte =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MByte]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MByte]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Byte.valueOf.INVOKE |>>
                RETURN_VOID2

            case PrimShort =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MShort]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MShort]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Short.valueOf.INVOKE |>>
                RETURN_VOID2
            case PrimInt =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MInt]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MInt]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Integer.valueOf.INVOKE |>>
                RETURN_VOID2
            case PrimLong =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MLong]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MLong]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Long.valueOf.INVOKE |>>
                RETURN_VOID2
            case PrimFloat =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MFloat]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MFloat]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Float.valueOf.INVOKE |>>
                RETURN_VOID2

            case PrimDouble =>
              val ifoLocal = new Local[Ref[ContinuationInterface[MDouble]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[MDouble]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                Api.Java.Lang.Double.valueOf.INVOKE |>>
                RETURN_VOID2

            case Reference(_) =>
              val ifoLocal = new Local[Ref[ContinuationInterface[Ref[MObject]]]](2)
              val getResultMethod = map(cont.name).asInstanceOf[ContinuationInterface[Ref[MObject]]].getResultMethod
              ifoLocal.LOAD[StackNil ** Ref[MObject]] |>>
                getResultMethod.INVOKE |>>
                RETURN_VOID2
            case _ => throw InternalCompilerException(s"Unexpected type $retTpe")
          })
    )
  }

  //Generate the class construtor
  //This method is unchecked as we can't know at the compile time the number of
  //arguments it takes. As it depends on the number of free variables the closure has.
  val defaultConstructor: UncheckedVoidMethod = {
    val setCloFreevars =
      (sig: UncheckedFunSig) => {
        //We need initialize each of the free vars in the closure
        //Therefore we need to accumulate the instructions
        //To do this we use a foldleft.
        //Note we also acumulate on the offset this is because long and double take double the space
        //in the stack
        val (setCloFields, _) = cloFreevarsTpes.zipWithIndex.foldLeft((NO_OP[StackNil], 2)) {
          case ((ins, offset), (arg, ind)) => {
            arg match {
              //We pattern on the arg type match as we want to match the free var type
              case PrimBool =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MBool](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 1)
              case PrimChar =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MChar](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 1)
              case PrimByte =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MByte](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 1)
              case PrimShort =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MShort](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 1)
              case PrimInt =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MInt](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 1)
              case PrimLong =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MLong](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 2)
              case PrimFloat =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MFloat](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 1)
              case PrimDouble =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[MDouble](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloPrimField(ind).PUT_FIELD, offset + 2)
              case Reference(_) =>
                (ins |>>
                  sig.getArg[Ref[Closure]](0).LOAD |>>
                  sig.getArg[Ref[MObject]](offset).LOAD[StackNil ** Ref[Closure]] |>>
                  getCloField(ind).PUT_FIELD, offset + 1)
              case _ => throw InternalCompilerException(s"Unexpected type $arg")
            }
          }
        }
        setCloFields
      }

    //Create an unchecked construstor, specify the argument list. The first agurment is the class type
    //As this is in scope, however this is not present in the final signature
    //The constructor calls super and initiliazes each of the classes fields
    cg.mkUncheckedConstructor(ct +: NJvmType.Object +: cloFreevarsTpes,
      sig =>
        sig.getArg[Ref[Closure]](0).LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg[Ref[Closure]](0).LOAD |>>
          sig.getArg[Ref[MObject]](1).LOAD |>>
          creationContext.PUT_FIELD |>>
          setCloFreevars(sig) |>>
          RETURN_VOID
    )
  }

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass =
    JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

