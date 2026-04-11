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

import ca.uwaterloo.flix.language.phase.llvm.LlvmIr.*

/**
  * A minimal pretty printer for the [[LlvmIr]] surface.
  */
object LlvmPrinter {

  def printModule(m: Module): String = {
    val sb = new StringBuilder(64 * 1024)

    sb.append("; Flix LLVM IR (bring-up)\n")
    sb.append(s"""source_filename = "${escape(m.sourceFilename)}"\n\n""")

    m.typeDefs.foreach { td =>
      sb.append(s"%${td.name} = type ${td.body.render}\n")
    }
    if (m.typeDefs.nonEmpty) sb.append("\n")

    m.decls.foreach {
      case Decl.DeclareFun(retType, name, params) =>
        sb.append(s"declare ${retType.render} @$name(${params.map(_.render).mkString(", ")})\n")
    }
    if (m.decls.nonEmpty) sb.append("\n")

    m.globals.foreach {
      case GlobalDef.CString(name, bytes) =>
        val arrTpe = Type.Array(bytes.length, Type.I8)
        sb.append(s"@$name = private unnamed_addr constant ${arrTpe.render} c\"${renderCString(bytes)}\", align 1\n")

      case GlobalDef.Constant(name, tpe, init, align, linkage) =>
        val linkageText = linkage match {
          case GlobalDef.Linkage.Private => "private "
          case GlobalDef.Linkage.External => ""
        }
        sb.append(s"@$name = ${linkageText}constant ${tpe.render} ${init.render}, align $align\n")
    }
    if (m.globals.nonEmpty) sb.append("\n")

    m.functions.foreach { f =>
      printFunction(f, sb)
      sb.append("\n")
    }

    sb.toString()
  }

  private def printFunction(f: Function, sb: StringBuilder): Unit = {
    val params = f.params.map(p => s"${p.tpe.render} %${p.name}").mkString(", ")
    sb.append(s"define ${f.retType.render} @${f.name}($params) {\n")

    f.blocks.foreach { b =>
      sb.append(s"${b.label}:\n")

      b.phis.foreach { phi =>
        val incoming = phi.incomings.map {
          case (v, lbl) => s"[ ${v.render}, %$lbl ]"
        }.mkString(", ")
        sb.append(s"  ${phi.dest.render} = phi ${phi.dest.tpe.render} $incoming\n")
      }

      b.instrs.foreach(instr => printInstr(instr, sb))
      printTerminator(b.term, sb)
    }

    sb.append("}\n")
  }

  private def printInstr(instr: Instr, sb: StringBuilder): Unit = instr match {
    case Instr.Assign(dest, op) =>
      sb.append(s"  ${dest.render} = ${printOp(op)}\n")

    case Instr.CallVoid(name, args) =>
      val argText = args.map(v => s"${v.tpe.render} ${v.render}").mkString(", ")
      sb.append(s"  call void @$name($argText)\n")

    case Instr.Store(value, addr) =>
      sb.append(s"  store ${value.tpe.render} ${value.render}, ptr ${addr.render}\n")

    case _: Instr.Phi =>
      // Phi instructions are printed as part of the block header.
      ()
  }

  private def printOp(op: Op): String = op match {
    case Op.Bin(opcode, tpe, a, b) =>
      s"$opcode ${tpe.render} ${a.render}, ${b.render}"

    case Op.ICmp(pred, a, b) =>
      s"icmp $pred ${a.tpe.render} ${a.render}, ${b.render}"

    case Op.FCmp(pred, a, b) =>
      s"fcmp $pred ${a.tpe.render} ${a.render}, ${b.render}"

    case Op.Cast(opcode, tpe, value) =>
      s"$opcode ${value.tpe.render} ${value.render} to ${tpe.render}"

    case Op.Alloca(elemType) =>
      s"alloca ${elemType.render}"

    case Op.Call(tpe, name, args) =>
      val argText = args.map(v => s"${v.tpe.render} ${v.render}").mkString(", ")
      s"call ${tpe.render} @$name($argText)"

    case Op.CallIndirect(tpe, callee, args) =>
      val argText = args.map(v => s"${v.tpe.render} ${v.render}").mkString(", ")
      s"call ${tpe.render} ${callee.render}($argText)"

    case Op.Load(tpe, addr) =>
      s"load ${tpe.render}, ptr ${addr.render}"

    case Op.Gep(elemType, base, index) =>
      s"getelementptr ${elemType.render}, ptr ${base.render}, ${index.tpe.render} ${index.render}"

    case Op.InsertValue(tpe, agg, elem, index) =>
      s"insertvalue ${tpe.render} ${agg.render}, ${elem.tpe.render} ${elem.render}, $index"

    case Op.ExtractValue(tpe, aggType, agg, index) =>
      s"extractvalue ${aggType.render} ${agg.render}, $index"
  }

  private def printTerminator(term: Terminator, sb: StringBuilder): Unit = term match {
    case Terminator.Ret(Type.Void, _) =>
      sb.append("  ret void\n")

    case Terminator.Ret(retType, value) =>
      sb.append(s"  ret ${retType.render} ${value.render}\n")

    case Terminator.Br(label) =>
      sb.append(s"  br label %$label\n")

    case Terminator.CondBr(cond, ifTrue, ifFalse) =>
      sb.append(s"  br ${cond.tpe.render} ${cond.render}, label %$ifTrue, label %$ifFalse\n")

    case Terminator.Unreachable =>
      sb.append("  unreachable\n")
  }

  private def escape(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")

  private def renderCString(bytes: Array[Byte]): String = {
    val sb = new StringBuilder(bytes.length * 4)
    bytes.foreach { b =>
      val x = b & 0xff
      x match {
        case 34 => sb.append("\\22") // "
        case 92 => sb.append("\\5C") // \
        case c if c >= 32 && c <= 126 =>
          sb.append(c.toChar)
        case other =>
          sb.append(f"\\$other%02X")
      }
    }
    sb.toString()
  }

}
