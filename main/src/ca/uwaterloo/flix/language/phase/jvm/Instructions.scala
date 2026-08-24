/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException}
import org.objectweb.asm.{MethodVisitor, Opcodes}

import java.lang.constant.ClassDesc

/**
  * A companion of [[BytecodeInstructions]] where instructions are expressed in terms of
  * nominal JVM types ([[ClassDesc]]) instead of [[BackendType]].
  *
  * Functions are gradually moved here from [[BytecodeInstructions]].
  */
object Instructions {

  /** Emits a `CHECKCAST` of the top of the stack to `tpe`, unless `tpe` is a primitive type. */
  def castIfNotPrim(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (!tpe.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, ClassDescs.internalNameOf(tpe))
  }

  /** Emits a `?ALOAD` instruction that loads an element from an array with element type `elmTpe`. */
  def xArrayLoad(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (elmTpe == CD_boolean) mv.visitInsn(Opcodes.BALOAD)
    else if (elmTpe == CD_char) mv.visitInsn(Opcodes.CALOAD)
    else if (elmTpe == CD_byte) mv.visitInsn(Opcodes.BALOAD)
    else if (elmTpe == CD_short) mv.visitInsn(Opcodes.SALOAD)
    else if (elmTpe == CD_int) mv.visitInsn(Opcodes.IALOAD)
    else if (elmTpe == CD_long) mv.visitInsn(Opcodes.LALOAD)
    else if (elmTpe == CD_float) mv.visitInsn(Opcodes.FALOAD)
    else if (elmTpe == CD_double) mv.visitInsn(Opcodes.DALOAD)
    else mv.visitInsn(Opcodes.AALOAD)
  }

  /** Emits a `?ASTORE` instruction that stores an element into an array with element type `elmTpe`. */
  def xArrayStore(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (elmTpe == CD_boolean) mv.visitInsn(Opcodes.BASTORE)
    else if (elmTpe == CD_char) mv.visitInsn(Opcodes.CASTORE)
    else if (elmTpe == CD_byte) mv.visitInsn(Opcodes.BASTORE)
    else if (elmTpe == CD_short) mv.visitInsn(Opcodes.SASTORE)
    else if (elmTpe == CD_int) mv.visitInsn(Opcodes.IASTORE)
    else if (elmTpe == CD_long) mv.visitInsn(Opcodes.LASTORE)
    else if (elmTpe == CD_float) mv.visitInsn(Opcodes.FASTORE)
    else if (elmTpe == CD_double) mv.visitInsn(Opcodes.DASTORE)
    else mv.visitInsn(Opcodes.AASTORE)
  }

  /** Emits a `NEWARRAY` or `ANEWARRAY` instruction that creates an array with element type `elmTpe`. */
  def xNewArray(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (elmTpe.isPrimitive) mv.visitIntInsn(Opcodes.NEWARRAY, newArrayOperandOf(elmTpe))
    else mv.visitTypeInsn(Opcodes.ANEWARRAY, ClassDescs.internalNameOf(elmTpe))
  }

  /** Emits a `POP` or `POP2` instruction that pops a value of type `tpe` off the stack. */
  def xPop(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_long || tpe == CD_double) mv.visitInsn(Opcodes.POP2)
    else mv.visitInsn(Opcodes.POP)
  }

  /** Emits a `?RETURN` instruction that returns a value of type `tpe`. */
  def xReturn(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitInsn(Opcodes.IRETURN)
    else if (tpe == CD_long) mv.visitInsn(Opcodes.LRETURN)
    else if (tpe == CD_float) mv.visitInsn(Opcodes.FRETURN)
    else if (tpe == CD_double) mv.visitInsn(Opcodes.DRETURN)
    else mv.visitInsn(Opcodes.ARETURN)
  }

  /** Returns the `NEWARRAY` type operand (e.g. [[Opcodes.T_INT]]) of the primitive type `tpe`. */
  private def newArrayOperandOf(tpe: ClassDesc): Int = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean) Opcodes.T_BOOLEAN
    else if (tpe == CD_char) Opcodes.T_CHAR
    else if (tpe == CD_byte) Opcodes.T_BYTE
    else if (tpe == CD_short) Opcodes.T_SHORT
    else if (tpe == CD_int) Opcodes.T_INT
    else if (tpe == CD_long) Opcodes.T_LONG
    else if (tpe == CD_float) Opcodes.T_FLOAT
    else if (tpe == CD_double) Opcodes.T_DOUBLE
    else throw InternalCompilerException(s"Unexpected primitive array element type '${tpe.descriptorString()}'", SourceLocation.Unknown)
  }

}
