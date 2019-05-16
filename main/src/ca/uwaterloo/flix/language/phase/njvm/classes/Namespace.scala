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
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, NamespaceInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Class that generates a namespace class given the namespace information
  *
  * @ns namespace information
  */
class Namespace(ns: NamespaceInfo)(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getNamespaceClassType(ns)
  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  //for each of the definitons on the namespace generate a field
  //and the respective shim method
  for ((sym, defn) <- ns.defs) {
    // type of `defn`
    val fieldTpe = getFunctionDefinitionClassType(sym)

    // Name of the field on namespace
    val fieldName = getDefFieldNameInNamespaceClass(sym)

    //generate the field
    cg.mkUncheckedField(fieldName, fieldTpe, List(Public))

    //get the method name as well as its arguments and return type
    val methodName = getDefMethodNameInNamespaceClass(defn.sym)
    val MonoType.Arrow(targs, tresult) = defn.tpe

    // Erased argument and result type.
    val erasedArgs = targs map getErasedJvmType
    val erasedResult = getErasedJvmType(tresult)
    //generate the context constructor capability
    val contextConstructor = new VoidMethod1[Ref[Context]](JvmModifier.InvokeSpecial, NJvmType.Context, "<init>")
    // Length of args in local
    val stackSize = erasedArgs.map(getStackSize).sum
    val namespaceClassType = getNamespaceClassType(ns)

    // generate the context and ifo local capabilities
    val contextLocal = new Local[Ref[Context]](stackSize)
    val ifoLocal = new Local[Ref[MObject]](stackSize + 1)

    //Get the namespace field name in the Context class
    val nsFieldName = getNamespaceFieldNameInContextClass(ns)

    //Get the definition field name in the namespace class
    val defnFieldName = getDefFieldNameInNamespaceClass(defn.sym)

    //generate the capability to acess the namespace field in the context class
    val nsField = new UncheckedClassField(nsFieldName, NJvmType.Context, namespaceClassType)
    //generate the capability to acess the definition field in the namespace class
    val nsIfoField = new UncheckedClassField(defnFieldName, namespaceClassType, fieldTpe)

    //generate the capability to acess the continuation field in the context class
    val continuationField = new ClassField[Ref[MObject]]("continuation", NJvmType.Context)

    //get the function interface type
    val functionInterface = getFunctionInterfaceType(erasedArgs, getErasedJvmType(tresult))

    //get the continuation interface type
    val contInterfaceTpe = getErasedJvmType(tresult) match {
      case PrimBool => getContinuationInterfaceType[MBool]
      case PrimChar => getContinuationInterfaceType[MChar]
      case PrimByte => getContinuationInterfaceType[MByte]
      case PrimShort => getContinuationInterfaceType[MShort]
      case PrimInt => getContinuationInterfaceType[MInt]
      case PrimLong => getContinuationInterfaceType[MLong]
      case PrimFloat => getContinuationInterfaceType[MFloat]
      case PrimDouble => getContinuationInterfaceType[MDouble]
      case Reference(_) => getContinuationInterfaceType[Ref[MObject]]
      case _ => throw InternalCompilerException(s"Unexpected type " + erasedResult)
    }
    //Generate the invoke and getResult method capability in the continuation interface
    val invokeMethod = new UncheckedVoidMethod(InvokeInterface, contInterfaceTpe, "invoke", List(NJvmType.Context))
    val getResultMethod = new UncheckedMethod(InvokeInterface, contInterfaceTpe, "getResult", Nil, erasedResult)

    // Generate the shim method.
    cg.mkUncheckedMethod(methodName,
      sig =>
        // Creating a context object
        NEW[StackNil, Ref[Context]](NJvmType.Context) |>>
          DUP |>>
          // Calling the constructor of context object
          contextConstructor.INVOKE |>>
          // Putting another reference of context on top of the stack
          DUP |>>
          // Storing Context on the variable
          contextLocal.STORE |>>
          // Extracting the namespace field from the context object
          nsField.GET_FIELD[StackNil, Ref[Context], Ref[Context]] |>>
          // Extracting the ifo from namespace
          nsIfoField.GET_FIELD[StackNil, Ref[Context], Ref[MObject]] |>>
          // Strong the IFO on a local variable
          ifoLocal.STORE |>> {
          // Set arguments for the IFO
          val (inst, _) = erasedArgs.zipWithIndex.foldLeft((NO_OP[StackNil], 0)) {
            case ((ins, offset), (arg, index)) =>
              //First we generate the ifoSetter capability
              //Then we duplicate the IFO reference
              //Load the argument from the field
              //Finally we set the ifo argument
              arg match {
                //We pattern on the arg type match as we want to match the arg type
                case PrimBool =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MBool](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MBool](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case PrimChar =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MChar](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MChar](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case PrimByte =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MByte](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MByte](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case PrimShort =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MShort](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MShort](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case PrimInt =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MInt](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MInt](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case PrimLong =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MLong](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MLong](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 2)
                case PrimFloat =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MFloat](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MFloat](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case PrimDouble =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], MDouble](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[MDouble](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 2)
                case Reference(_) =>
                  val ifoSetter = new VoidMethod2[Ref[MObject], Ref[MObject]](JvmModifier.InvokeVirtual, fieldTpe, "setArg" + index)
                  (ins |>>
                    ifoLocal.LOAD[StackNil] |>>
                    sig.getArg[Ref[MObject]](offset).LOAD |>>
                    ifoSetter.INVOKE, offset + 1)
                case _ => throw InternalCompilerException(s"Unexpected type $arg")
              }
          }
          inst
        }
          |>>
          // Put the closure on `continuation` field of `Context`
          contextLocal.LOAD |>>
          ifoLocal.LOAD |>>
          // Casting to JvmType of FunctionInterface
          CHECK_CAST(functionInterface) |>>
          continuationField.PUT_FIELD |>>
          // This is necessary since the loop has to pop a value from the stack!
          CONST_NULL[StackNil, Ref[MObject]] |>>
          ifoLocal.STORE |>>
          IFNONNULL(
            // Getting `continuation` field on `Context`
            contextLocal.LOAD[StackNil] |>>
              continuationField.GET_FIELD[StackNil, Ref[Context]] |>>
              // Setting `continuation` field of global to `null`
              contextLocal.LOAD[StackNil ** Ref[MObject]] |>>
              CONST_NULL[StackNil ** Ref[MObject] ** Ref[Context], Ref[MObject]] |>>
              continuationField.PUT_FIELD[StackNil ** Ref[MObject], Ref[Context]] |>>
              // Cast to the continuation
              CHECK_CAST[StackNil](contInterfaceTpe) |>>
              DUP[StackNil, Ref[MObject]] |>>
              // Storing the continuation on a local variable
              ifoLocal.STORE[StackNil ** Ref[MObject]] |>>
              // Call invoke
              contextLocal.LOAD[StackNil ** Ref[MObject]] |>>
              invokeMethod.INVOKE[StackNil ** Ref[MObject] ** Ref[Context], StackNil] |>>
              // Getting `continuation` field on `Context`
              contextLocal.LOAD[StackNil] |>>
              continuationField.GET_FIELD[StackNil, Ref[Context]]
          ) |>>
          // Loading the IFO
          ifoLocal.LOAD[StackNil] |>>
          (erasedResult match {
            // Invoking the `getResult` method and returning the value
            //We pattern match on the erased result in order for the type to match
            case PrimBool =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MBool] |>>
                RETURN
            case PrimChar =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MChar] |>>
                RETURN
            case PrimByte =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MByte] |>>
                RETURN
            case PrimShort =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MShort] |>>
                RETURN
            case PrimInt =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MInt] |>>
                RETURN
            case PrimLong =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MLong] |>>
                RETURN
            case PrimFloat =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MFloat] |>>
                RETURN
            case PrimDouble =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** MDouble] |>>
                RETURN
            case Reference(_) =>
              getResultMethod.INVOKE[StackNil ** Ref[MObject], StackNil ** Ref[MObject]] |>>
                RETURN
            case _ => throw InternalCompilerException(s"Unexpected type " + getErasedJvmType(tresult))
          })
      , erasedArgs, erasedResult, List(Public, Static, Final), true)
  }

  /**
    * Method which generates the capability to LOAD/STORE the definition field
    */
  def getUncheckedField(sym: Symbol.DefnSym): UncheckedField = {
    // JvmType of `defn`
    val fieldType = getFunctionDefinitionClassType(sym)

    // Name of the field on namespace
    val fieldName = getDefFieldNameInNamespaceClass(sym)
    new UncheckedField(fieldName, fieldType)
  }

  //Namespace constructor
  //It call the super constructor and
  //initializes each of definitions in the name space
  val defaultConstructor: VoidMethod1[Ref[Namespace]] =
    cg.mkConstructor1(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          ns.defs.foldLeft(NO_OP[StackNil]) {
            case (ins, (sym, _)) =>
              val field = getUncheckedField(sym)
              val classType = getFunctionDefinitionClassType(sym)
              val fieldConstructor = new VoidMethod1[Ref[Namespace]](JvmModifier.InvokeSpecial, classType, "<init>")
              ins |>>
              sig.getArg1.LOAD |>>
              NEW(classType) |>>
              DUP |>>
              fieldConstructor.INVOKE |>>
              field.PUT_FIELD
          } |>>
          RETURN_VOID
    )

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

