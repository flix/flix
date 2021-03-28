package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{ERefType, EType, PRefType, PType}
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.EType._
import ca.uwaterloo.flix.language.ast.ERefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.phase.jvm.JvmType
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes

/**
 * Generates bytecode for the cell classes.
 */
object GenRefClasses {

  /**
   * Returns the bytecode for the cell classes built-in to the Flix language.
   */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    //     //Type that we need a cell class for
    //    val types = List(JvmType.PrimBool, JvmType.PrimChar, JvmType.PrimFloat, JvmType.PrimDouble,
    //      JvmType.PrimByte, JvmType.PrimShort, JvmType.PrimInt, JvmType.PrimLong, JvmType.Object)
    //
    //    val types = List(Bool(), Int32(), Char(), Float32, Float64(), Int8(), Int16(), Int32(), Int64(), Reference(ERefType.))
    //
    //    // Generating each cell class
    //    types.map { tpe =>
    //      val classType = JvmName.getCellClassType(tpe)
    //      classType.name -> JvmClass(classType.name, genRefClass(classType, tpe))
    //    }.toMap
    ???
  }

  //  /**
  //    * Generating class `classType` with value of type `tpe`
  //    */
  //  def genRefClass(classType: JvmType.Reference, tpe: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
  //    // Class visitor
  //    val visitor = AsmOps.mkClassWriter()
  //
  //    // Class visitor
  //    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
  //      JvmName.Object.toInternalName, null)
  //
  //
  //    // Generate the instance field
  //    AsmOps.compileField(visitor, "value", tpe, isStatic = false, isPrivate = true)
  //
  //    // Generate the constructor
  //    genConstructor(classType, tpe, visitor)
  //
  //    // Generate `getValue` method
  //    genGetValue(classType, tpe, visitor)
  //
  //    // Generate `setValue` method
  //    genSetValue(classType, tpe, visitor)
  //
  //    // Generate the `toString` method.
  //    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
  //      "toString method shouldn't be called")
  //
  //    // Generate the `hashCode` method.
  //    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
  //      "hashCode method shouldn't be called")
  //
  //    // Generate the `equals` method.
  //    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool),
  //      "equals method shouldn't be called")
  //
  //    // Complete the visitor and get the bytecode.
  //    visitor.visitEnd()
  //    visitor.toByteArray
  //  }
  //
  /**
   * Generating constructor for the class `classType` with value of type `cellType`
   */
  //  def genConstructor(classType: JvmType.Reference, cellType: JvmType, visitor: ClassWriter )(implicit root: Root, flix: Flix): ??? = {
  def genConstructor[T <: PType](refElement: EType[T]): F[StackNil] => F[StackNil] = {
    ALOAD[StackNil, PRefType.PRef[T]](0) ~
      INVOKESPECIAL("class string of Object", "temp constructor () => V)")
      ??? // RETURN
  }

//
//  /**
//    * Generating `getValue` method for the class `classType` with value of type `cellType`
//    */
//  def genGetValue(classType: JvmType.Reference, cellType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
//    val iRet = AsmOps.getReturnInstruction(cellType)
//    val getValue = visitor.visitMethod(ACC_PUBLIC, "getValue", AsmOps.getMethodDescriptor(Nil, cellType), null, null)
//    getValue.visitCode()
//    getValue.visitVarInsn(ALOAD, 0)
//    getValue.visitFieldInsn(GETFIELD, classType.name.toInternalName, "value", cellType.toDescriptor)
//    getValue.visitInsn(iRet)
//    getValue.visitMaxs(1, 1)
//    getValue.visitEnd()
//  }
//
//  /**
//    * Generating `setValue` method for the class `classType` with value of type `cellType`
//    */
//  def genSetValue(classType: JvmType.Reference, cellType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
//    val iLoad = AsmOps.getLoadInstruction(cellType)
//    val setValue = visitor.visitMethod(ACC_PUBLIC, "setValue", AsmOps.getMethodDescriptor(List(cellType), JvmType.Void), null, null)
//    setValue.visitCode()
//    setValue.visitVarInsn(ALOAD, 0)
//    setValue.visitVarInsn(iLoad, 1)
//    setValue.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "value", cellType.toDescriptor)
//    setValue.visitInsn(RETURN)
//    setValue.visitMaxs(2, 2)
//    setValue.visitEnd()
//  }

}
