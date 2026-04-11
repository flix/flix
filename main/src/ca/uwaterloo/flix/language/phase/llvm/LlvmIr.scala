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

package ca.uwaterloo.flix.language.phase.llvm

/**
  * A small typed LLVM IR surface (textual `.ll`).
  *
  * This is intentionally partial: it models only the LLVM constructs the current backend emits.
  * Extend it as we add language features to the LLVM backend.
  */
object LlvmIr {

  //
  // Types
  //
  sealed trait Type {
    def render: String
  }

  object Type {
    case object Void extends Type {
      def render: String = "void"
    }

    case object Ptr extends Type {
      def render: String = "ptr"
    }

    case object I1 extends Type {
      def render: String = "i1"
    }

    case object I8 extends Type {
      def render: String = "i8"
    }

    case object I16 extends Type {
      def render: String = "i16"
    }

    case object I32 extends Type {
      def render: String = "i32"
    }

    case object I64 extends Type {
      def render: String = "i64"
    }

    case object Float extends Type {
      def render: String = "float"
    }

    case object Double extends Type {
      def render: String = "double"
    }

    case class Named(name: String) extends Type {
      def render: String = s"%$name"
    }

    case class Struct(fields: List[Type]) extends Type {
      def render: String = s"{ ${fields.map(_.render).mkString(", ")} }"
    }

    case class Array(length: Int, elemType: Type) extends Type {
      def render: String = s"[$length x ${elemType.render}]"
    }
  }

  //
  // Values
  //
  sealed trait Value {
    def tpe: Type

    def render: String
  }

  object Value {
    case class Local(name: String, tpe: Type) extends Value {
      def render: String = s"%$name"
    }

    case class Global(name: String, tpe: Type) extends Value {
      def render: String = s"@$name"
    }

    case class IntConst(value: Long, tpe: Type) extends Value {
      def render: String = value.toString
    }

    /**
      * A float32 constant represented by its raw IEEE-754 bits.
      *
      * We render this as a `bitcast` constant expression to avoid the pitfalls
      * of decimal rendering and to preserve NaN payloads and signed zero.
      */
    case class Float32Const(bits: Int) extends Value {
      def tpe: Type = Type.Float
      def render: String = s"bitcast (i32 $bits to float)"
    }

    /**
      * A float64 constant represented by its raw IEEE-754 bits.
      *
      * We render this as a `bitcast` constant expression to preserve the exact
      * bit pattern (including NaN payloads and signed zero).
      */
    case class Float64Const(bits: Long) extends Value {
      def tpe: Type = Type.Double
      def render: String = s"bitcast (i64 $bits to double)"
    }

    case class Null(tpe: Type) extends Value {
      def render: String = "null"
    }

    case class Undef(tpe: Type) extends Value {
      def render: String = "undef"
    }

    case class Zero(tpe: Type) extends Value {
      def render: String = "zeroinitializer"
    }

    case class StructConst(elems: List[Value], tpe: Type) extends Value {
      def render: String = {
        val fields = elems.map(v => s"${v.tpe.render} ${v.render}").mkString(", ")
        s"{ $fields }"
      }
    }

    case class ArrayConst(elems: List[Value], tpe: Type) extends Value {
      def render: String = {
        val xs = elems.map(v => s"${v.tpe.render} ${v.render}").mkString(", ")
        s"[ $xs ]"
      }
    }
  }

  //
  // Module
  //

  case class TypeDef(name: String, body: Type)

  sealed trait Decl

  object Decl {
    case class DeclareFun(retType: Type, name: String, params: List[Type]) extends Decl
  }

  case class Param(name: String, tpe: Type)

  case class Function(name: String,
                      retType: Type,
                      params: List[Param],
                      blocks: List[Block])

  case class Block(label: String,
                   phis: List[Instr.Phi],
                   instrs: List[Instr],
                   term: Terminator)

  sealed trait GlobalDef

  object GlobalDef {
    sealed trait Linkage

    object Linkage {
      case object Private extends Linkage
      case object External extends Linkage
    }

    /**
      * A null-terminated string constant.
      *
      * The byte array should include the trailing `0` terminator.
      */
    case class CString(name: String, bytes: Array[Byte]) extends GlobalDef

    case class Constant(name: String, tpe: Type, init: Value, align: Int = 8, linkage: Linkage = Linkage.Private) extends GlobalDef
  }

  case class Module(sourceFilename: String,
                    typeDefs: List[TypeDef],
                    decls: List[Decl],
                    globals: List[GlobalDef],
                    functions: List[Function])

  //
  // Instructions / Terminators
  //

  sealed trait Instr

  object Instr {
    case class Assign(dest: Value.Local, op: Op) extends Instr

    case class CallVoid(name: String, args: List[Value]) extends Instr

    case class Store(value: Value, addr: Value) extends Instr

    case class Phi(dest: Value.Local, incomings: List[(Value, String)]) extends Instr
  }

  sealed trait Terminator

  object Terminator {
    case class Ret(retType: Type, value: Value) extends Terminator

    case class Br(label: String) extends Terminator

    case class CondBr(cond: Value, ifTrue: String, ifFalse: String) extends Terminator

    case object Unreachable extends Terminator
  }

  //
  // Operations
  //

  sealed trait Op {
    def tpe: Type
  }

  object Op {
    case class Bin(opcode: String, tpe: Type, a: Value, b: Value) extends Op

    case class ICmp(pred: String, a: Value, b: Value) extends Op {
      def tpe: Type = Type.I1
    }

    case class FCmp(pred: String, a: Value, b: Value) extends Op {
      def tpe: Type = Type.I1
    }

    case class Cast(opcode: String, tpe: Type, value: Value) extends Op

    case class Alloca(elemType: Type) extends Op {
      def tpe: Type = Type.Ptr
    }

    case class Call(tpe: Type, name: String, args: List[Value]) extends Op

    case class CallIndirect(tpe: Type, callee: Value, args: List[Value]) extends Op

    case class Load(tpe: Type, addr: Value) extends Op

    case class Gep(elemType: Type, base: Value, index: Value) extends Op {
      def tpe: Type = Type.Ptr
    }

    case class InsertValue(tpe: Type, agg: Value, elem: Value, index: Int) extends Op

    case class ExtractValue(tpe: Type, aggType: Type, agg: Value, index: Int) extends Op
  }

}
