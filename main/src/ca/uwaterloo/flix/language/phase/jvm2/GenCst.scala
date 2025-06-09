package ca.uwaterloo.flix.language.phase.jvm2

import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, JvmName, JvmType}

import java.lang.classfile.{CodeBuilder, MethodModel, Opcode}
import java.lang.constant.{ClassDesc, ConstantDescs, MethodTypeDesc}

object GenCst {
  def gen(cst: Constant, cb: CodeBuilder): Unit = cst match {
    case Constant.Unit =>
      cb.fieldAccess(
        Opcode.GETSTATIC, // opcode
        ClassDesc.of(BackendObjType.Unit.jvmName.toInternalName), // owner
        BackendObjType.Unit.SingletonField.name, // name
        ClassDesc.of(BackendObjType.Unit.jvmName.toInternalName) // type
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
        MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String),
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
        // return type, argument types...
        MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String),
        false
      )

    case Constant.Str(s) =>
      cb.ldc(s)

    case Constant.Regex(patt) =>
      cb.ldc(patt.pattern)
      cb.invokestatic(
        ClassDesc.of(JvmName.Regex.toInternalName), // owner
        "compile", // method name
        // return type, argument types...
        MethodTypeDesc.of(ClassDesc.of(JvmName.Regex.toInternalName), ConstantDescs.CD_String), // method type
      )

    case Constant.RecordEmpty =>
      val classType = BackendObjType.RecordEmpty
      cb.fieldAccess(
        Opcode.GETSTATIC,
        ClassDesc.of(classType.jvmName.toInternalName),
        classType.SingletonField.name,
        ClassDesc.of(classType.jvmName.toInternalName),
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
