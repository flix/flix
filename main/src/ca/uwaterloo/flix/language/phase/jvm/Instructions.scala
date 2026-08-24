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

  /** A local variable of type `tpe` at index `index`. */
  class Variable(val tpe: ClassDesc, index: Int) {
    def load()(implicit mv: MethodVisitor): Unit = xLoad(tpe, index)

    def store()(implicit mv: MethodVisitor): Unit = xStore(tpe, index)
  }

  /** Emits a `CHECKCAST` of the top of the stack to `tpe`, unless `tpe` is a primitive type. */
  def castIfNotPrim(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (!tpe.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, ClassDescs.internalNameOf(tpe))
  }

  /** Emits an `xStore` of `tpe` at `index` and runs `body` with the corresponding [[Variable]]. */
  def storeWithName(index: Int, tpe: ClassDesc)(body: Variable => Unit)(implicit mv: MethodVisitor): Unit = {
    xStore(tpe, index)
    body(new Variable(tpe, index))
  }

  /** Runs `body` with the [[Variable]] of type `tpe` at `index`. */
  def withName(index: Int, tpe: ClassDesc)(body: Variable => Unit): Unit =
    body(new Variable(tpe, index))

  /** Runs `body` with the [[Variable]]s of types `tpes` starting at `index`, and the next free index. */
  def withNames(index: Int, tpes: List[ClassDesc])(body: (Int, List[Variable]) => Unit): Unit = {
    var runningIndex = index
    val variables = tpes.map(tpe => {
      val variable = new Variable(tpe, runningIndex)
      runningIndex = runningIndex + stackSlotsOf(tpe)
      variable
    })
    body(runningIndex, variables)
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

  /** Emits a `?LOAD` instruction that loads the local variable of type `tpe` at `index`. */
  def xLoad(tpe: ClassDesc, index: Int)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitVarInsn(Opcodes.ILOAD, index)
    else if (tpe == CD_long) mv.visitVarInsn(Opcodes.LLOAD, index)
    else if (tpe == CD_float) mv.visitVarInsn(Opcodes.FLOAD, index)
    else if (tpe == CD_double) mv.visitVarInsn(Opcodes.DLOAD, index)
    else mv.visitVarInsn(Opcodes.ALOAD, index)
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

  /** Emits a `?STORE` instruction that stores the top of the stack into the local variable of type `tpe` at `index`. */
  def xStore(tpe: ClassDesc, index: Int)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitVarInsn(Opcodes.ISTORE, index)
    else if (tpe == CD_long) mv.visitVarInsn(Opcodes.LSTORE, index)
    else if (tpe == CD_float) mv.visitVarInsn(Opcodes.FSTORE, index)
    else if (tpe == CD_double) mv.visitVarInsn(Opcodes.DSTORE, index)
    else mv.visitVarInsn(Opcodes.ASTORE, index)
  }

  /** Returns the number of stack slots (1 or 2) that a value of type `tpe` occupies. */
  private def stackSlotsOf(tpe: ClassDesc): Int = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_long || tpe == CD_double) 2 else 1
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
