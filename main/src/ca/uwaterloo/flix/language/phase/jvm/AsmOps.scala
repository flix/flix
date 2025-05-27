/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmTarget}
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.{ClassWriter, MethodVisitor}

object AsmOps {

  /**
    * Returns the target JVM version.
    */
  def JavaVersion(implicit flix: Flix): Int = flix.options.target match {
    case JvmTarget.Version21 => V21
  }

  /**
    * Returns a freshly created class writer object.
    *
    * The object is constructed to compute stack map frames automatically.
    */
  def mkClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      JvmType.Object.name.toInternalName
    }
  }

  /**
    * Returns the stack size of a variable of type `tpe` in jvm.
    */
  def getStackSize(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type: $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool => 1
    case JvmType.PrimChar => 1
    case JvmType.PrimFloat => 1
    case JvmType.PrimDouble => 2
    case JvmType.PrimByte => 1
    case JvmType.PrimShort => 1
    case JvmType.PrimInt => 1
    case JvmType.PrimLong => 2
    case JvmType.Reference(_) => 1
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    */
  def getLoadInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => ILOAD
    case JvmType.PrimLong => LLOAD
    case JvmType.PrimFloat => FLOAD
    case JvmType.PrimDouble => DLOAD
    case JvmType.Reference(_) => ALOAD
  }

  /**
    * Returns the store instruction for the value of the type specified by `tpe`
    */
  def getStoreInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => ISTORE
    case JvmType.PrimLong => LSTORE
    case JvmType.PrimFloat => FSTORE
    case JvmType.PrimDouble => DSTORE
    case JvmType.Reference(_) => ASTORE
  }

  /**
    * Returns the array load instruction for arrays of the given JvmType tpe
    */
  def getArrayLoadInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool => BALOAD
    case JvmType.PrimChar => CALOAD
    case JvmType.PrimByte => BALOAD
    case JvmType.PrimShort => SALOAD
    case JvmType.PrimInt => IALOAD
    case JvmType.PrimLong => LALOAD
    case JvmType.PrimFloat => FALOAD
    case JvmType.PrimDouble => DALOAD
    case JvmType.Reference(_) => AALOAD
  }

  /**
    * Returns the array store instruction for arrays of the given JvmType tpe
    */
  def getArrayStoreInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool => BASTORE
    case JvmType.PrimChar => CASTORE
    case JvmType.PrimByte => BASTORE
    case JvmType.PrimShort => SASTORE
    case JvmType.PrimInt => IASTORE
    case JvmType.PrimLong => LASTORE
    case JvmType.PrimFloat => FASTORE
    case JvmType.PrimDouble => DASTORE
    case JvmType.Reference(_) => AASTORE
  }

  /**
    * Returns the CheckCast type for the value of the type specified by `tpe`
    */
  def getArrayType(tpe: JvmType): String = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool => "[Z"
    case JvmType.PrimChar => "[C"
    case JvmType.PrimByte => "[B"
    case JvmType.PrimShort => "[S"
    case JvmType.PrimInt => "[I"
    case JvmType.PrimLong => "[J"
    case JvmType.PrimFloat => "[F"
    case JvmType.PrimDouble => "[D"
    case JvmType.String => "[Ljava/lang/String;"
    case JvmType.Reference(_) => "[Ljava/lang/Object;"
  }

  /**
    * Returns the load instruction corresponding to the given type `tpe`
    */
  def getReturnInstruction(tpe: JvmType): Int = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool | JvmType.PrimChar | JvmType.PrimByte | JvmType.PrimShort | JvmType.PrimInt => IRETURN
    case JvmType.PrimLong => LRETURN
    case JvmType.PrimFloat => FRETURN
    case JvmType.PrimDouble => DRETURN
    case JvmType.Reference(_) => ARETURN
  }

  /**
    * Returns the descriptor of a method take takes the given `argumentTypes` and returns the given `resultType`.
    */
  def getMethodDescriptor(argumentTypes: List[JvmType], resultType: JvmType): String = {
    // Descriptor of result
    val resultDescriptor = resultType.toDescriptor

    // Descriptor of arguments
    val argumentDescriptor = argumentTypes.map(_.toDescriptor).mkString

    // Descriptor of the method
    s"($argumentDescriptor)$resultDescriptor"
  }

  /**
    * `tpe` is jvm type of value on top of the stack. If the value is not primitive, then we cast it to it's specific type,
    * if the value is a primitive then since there is no boxing, then no casting is necessary.
    */
  def castIfNotPrim(visitor: MethodVisitor, tpe: JvmType): Unit = tpe match {
    case JvmType.Void => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    case JvmType.PrimBool => ()
    case JvmType.PrimChar => ()
    case JvmType.PrimFloat => ()
    case JvmType.PrimDouble => ()
    case JvmType.PrimByte => ()
    case JvmType.PrimShort => ()
    case JvmType.PrimInt => ()
    case JvmType.PrimLong => ()
    case JvmType.Reference(name) => visitor.visitTypeInsn(CHECKCAST, name.toInternalName)
  }

  /**
    * Emits code that puts the function object of the def symbol `def` on top of the stack.
    */
  def compileDefSymbol(sym: Symbol.DefnSym, mv: MethodVisitor): Unit = {
    // JvmType of Def
    val defJvmName = JvmOps.getFunctionDefinitionClassName(sym)

    mv.visitTypeInsn(NEW, defJvmName.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, defJvmName.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
  }

}
