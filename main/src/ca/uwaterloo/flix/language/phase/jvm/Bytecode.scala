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
object Bytecode {

  /** Emits a `NEWARRAY` or `ANEWARRAY` instruction that creates an array with element type `elmTpe`. */
  def xNewArray(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (elmTpe.isPrimitive) mv.visitIntInsn(Opcodes.NEWARRAY, newArrayOperandOf(elmTpe))
    else mv.visitTypeInsn(Opcodes.ANEWARRAY, ClassDescs.internalNameOf(elmTpe))
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
