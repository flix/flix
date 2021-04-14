package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.EType._
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.{ERefType, EType, PRefType, PType}
import ca.uwaterloo.flix.language.phase.jvm.AsmOps
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.{ClassWriter, Opcodes}

/**
  * Generates bytecode for the ref classes.
  */
object GenRefClasses {

  /**
    * Returns the bytecode for the ref classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[String, JvmClass] = {

    // Generating each cell class
    def genAUX[T <: PType](tpe: EType[T]): (String, JvmClass) = {
      val eeType = Reference(ERefType.Ref(tpe))
      val className: String = Instructions.getInternalName(eeType)
      className -> JvmClass(className, genRefClass(className, tpe))
    }

    //Type that we need a cell class for
    Map() +
      genAUX(Bool()) +
      genAUX(Int8()) +
      genAUX(Int16()) +
      genAUX(Int32()) +
      genAUX(Int64()) +
      genAUX(Char()) +
      genAUX(Float32()) +
      genAUX(Float64()) +
      genAUX(Reference(null))
  }

  /**
    * Generating class `className` with value of type `innerType`
    */
  def genRefClass[T <: PType](className: String, innerType: EType[T])(implicit root: Root, flix: Flix): scala.Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null,
      "class string of Object", null)


    // Generate the instance field
    val innerTypeString = Instructions.getDescriptor(innerType)
    visitor.visitField(Opcodes.ACC_PRIVATE, "value", innerTypeString, null, null).visitEnd()

    val constructorDescriptor = Instructions.getDescriptor(innerTypeString, "V")
    // Generate the constructor
    val initMethod = visitor.visitMethod(Opcodes.ACC_PUBLIC, "<init>", constructorDescriptor, null, null)
    initMethod.visitCode()
    genConstructor()(F(initMethod))
    initMethod.visitMaxs(2, 2)
    initMethod.visitEnd()

    // Complete the visitor and get the bytecode.
    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generating constructor for the class with value of type `innerType`
    */
  def genConstructor[T <: PType](): F[StackNil] => F[StackEnd] = {
    ALOAD[StackNil, PRefType.PRef[T]](0) ~
      INVOKESPECIAL("class string of Object", "()V)")
    RETURN
  }
}
