package ca.uwaterloo.flix.language.phase.jvm2

import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, JvmName, JvmType}

import java.lang.classfile.{CodeBuilder, MethodModel, Opcode}
import java.lang.constant.{ClassDesc, ConstantDescs, MethodTypeDesc}
//def compileInt(i: Int)(implicit mv: MethodVisitor): Unit = i match {
//  case -1 => mv.visitInsn(ICONST_M1)
//  case 0 => mv.visitInsn(ICONST_0)
//  case 1 => mv.visitInsn(ICONST_1)
//  case 2 => mv.visitInsn(ICONST_2)
//  case 3 => mv.visitInsn(ICONST_3)
//  case 4 => mv.visitInsn(ICONST_4)
//  case 5 => mv.visitInsn(ICONST_5)
//  case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => mv.visitIntInsn(BIPUSH, i)
//  case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => mv.visitIntInsn(SIPUSH, i)
//  case _ => mv.visitLdcInsn(i)
//}
// ...
//case Constant.Null =>
//mv.visitInsn(ACONST_NULL)
//AsmOps.castIfNotPrim(mv, JvmOps.getJvmType(tpe))
//
//case Constant.Bool(true) =>
//mv.visitInsn(ICONST_1)
//
//case Constant.Bool(false) =>
//mv.visitInsn(ICONST_0)
//
//case Constant.Char(c) =>
//compileInt(c)
//
//case Constant.Float32(f) =>
//f match {
//  case 0f => mv.visitInsn(FCONST_0)
//  case 1f => mv.visitInsn(FCONST_1)
//  case 2f => mv.visitInsn(FCONST_2)
//  case _ => mv.visitLdcInsn(f)
//}
//
//case Constant.Float64(d) =>
//d match {
//  case 0d => mv.visitInsn(DCONST_0)
//  case 1d => mv.visitInsn(DCONST_1)
//  case _ => mv.visitLdcInsn(d)
//}
//
//case Constant.BigDecimal(dd) =>
//// Can fail with NumberFormatException
//addSourceLine(mv, loc)
//mv.visitTypeInsn(NEW, BackendObjType.BigDecimal.jvmName.toInternalName)
//mv.visitInsn(DUP)
//mv.visitLdcInsn(dd.toString)
//mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigDecimal.jvmName.toInternalName, "<init>",
//  AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
//
//case Constant.Int8(b) =>
//compileInt(b)
//
//case Constant.Int16(s) =>
//compileInt(s)
//
//case Constant.Int32(i) =>
//compileInt(i)
//
//case Constant.Int64(l) =>
//compileLong(l)
//
//case Constant.BigInt(ii) =>
//// Add source line number for debugging (can fail with NumberFormatException)
//addSourceLine(mv, loc)
//mv.visitTypeInsn(NEW, BackendObjType.BigInt.jvmName.toInternalName)
//mv.visitInsn(DUP)
//mv.visitLdcInsn(ii.toString)
//mv.visitMethodInsn(INVOKESPECIAL, BackendObjType.BigInt.jvmName.toInternalName, "<init>",
//  AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
//
//case Constant.Str(s) =>
//mv.visitLdcInsn(s)
//
//case Constant.Regex(patt) =>
//// Add source line number for debugging (can fail with PatternSyntaxException)
//addSourceLine(mv, loc)
//mv.visitLdcInsn(patt.pattern)
//mv.visitMethodInsn(INVOKESTATIC, JvmName.Regex.toInternalName, "compile",
//  AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Regex), false)
//
//case Constant.RecordEmpty =>
//// We get the JvmType of the class for the RecordEmpty
//val classType = BackendObjType.RecordEmpty
//// Instantiating a new object of tuple
//mv.visitFieldInsn(GETSTATIC, classType.jvmName.toInternalName, BackendObjType.RecordEmpty.SingletonField.name, classType.toDescriptor)

object GenCst {
  def gen(cst: Constant, cb: CodeBuilder): Unit = cst match {
    case Constant.Unit =>
      cb.fieldAccess(
        Opcode.GETSTATIC, // opcode
        ClassDesc.of(BackendObjType.Unit.jvmName.toString), // owner
        BackendObjType.Unit.SingletonField.name, // name
        ClassDesc.of(BackendObjType.Unit.jvmName.toString) // type
      )
    case Constant.Null =>
      cb.aconst_null()

    case Constant.Bool(b) => b match {
      case true => cb.iconst_1()
      case false => cb.iconst_0()
    }

    case Constant.Char(c) =>
      compileInt(c, cb)

    case Constant.Float32(f) => f match {
      case 0f => cb.fconst_0()
      case 1f => cb.fconst_1()
      case 2f => cb.fconst_2()
      case _ =>  cb.ldc(f)
    }

    case Constant.Float64(d) => d match {
      case 0d => cb.dconst_0()
      case 1d => cb.dconst_1()
      case _ => cb.ldc(d);
    }


    case Constant.BigDecimal(dd) =>
      cb.new_(ClassDesc.of("java.math.BigDecimal"))
      cb.dup()
      cb.ldc(dd.toString)
      cb.invokespecial(
        ClassDesc.of("java.math.BigDecimal"),
        "<init>",
        MethodTypeDesc.of(ConstantDescs.CD_String, ConstantDescs.CD_void),
        false
      )

    case Constant.Int8(b) =>
      compileInt(b, cb)

    case Constant.Int16(s) =>
      compileInt(s, cb)

    case Constant.Int32(i) =>
      compileInt(i, cb)

    case Constant.Int64(l) =>
      compileLong(l, cb)

    case Constant.BigInt(ii) =>
      cb.new_(ClassDesc.of("java.math.BigInteger"))
      cb.dup()
      cb.ldc(ii.toString)
      cb.invokespecial(
        ClassDesc.of("java.math.BigInteger"),
        "<init>",
        MethodTypeDesc.of(ConstantDescs.CD_String, ConstantDescs.CD_void),
        false
      )

    case Constant.Str(s) =>
      cb.ldc(s)

    case Constant.Regex(patt) =>
      cb.ldc(patt.pattern())
      cb.invokestatic(
        ClassDesc.of(JvmName.Regex.toString), // owner
        "compile", // method name
        MethodTypeDesc.of(ConstantDescs.CD_String, ClassDesc.of(JvmType.Regex.name.toString)), // method type
      )

    case Constant.RecordEmpty =>
      val classType = BackendObjType.RecordEmpty
      cb.fieldAccess(
        Opcode.GETSTATIC,
        ClassDesc.of(classType.jvmName.toString),
        classType.SingletonField.name,
        ClassDesc.of(classType.jvmName.toString),
      )

  }

  private def compileInt(i: Int, cb: CodeBuilder): Unit = i match {
    case -1 => cb.iconst_m1()
    case 0 => cb.iconst_0()
    case 1 => cb.iconst_1()
    case 2 => cb.iconst_2()
    case 3 => cb.iconst_3()
    case 4 => cb.iconst_4()
    case 5 => cb.iconst_5()
    case _ if Byte.MinValue <= i && i <= Byte.MaxValue => cb.bipush(i.toByte)
    case _ if Short.MinValue <= i && i <= Short.MaxValue => cb.sipush(i.toShort)
    case _ => cb.ldc(i)
  }

  private def compileLong(l: Long, cb: CodeBuilder): Unit = l match {
    case 0L => cb.lconst_0()
    case 1L => cb.lconst_1()
    case _ => cb.ldc(l)
  }
}
