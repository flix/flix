package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.jvm.AsmOps
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

/**
  * Generates bytecode for the ref classes.
  */
object GenRefClasses {

  /**
    * Returns the bytecode for the ref classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[String, JvmClass] = {

    // Generating each cell class
    def genAUX[T <: PType](tpe: RType[T]): (String, JvmClass) = {
      val eeType = RReference(RRef(tpe))
      val className: String = Instructions.getInternalName(eeType)
      className -> JvmClass(className, genRefClass(className, tpe))
    }

    //Type that we need a cell class for
    Map() +
      genAUX(RBool()) +
      genAUX(RInt8()) +
      genAUX(RInt16()) +
      genAUX(RInt32()) +
      genAUX(RInt64()) +
      genAUX(RChar()) +
      genAUX(RFloat32()) +
      genAUX(RFloat64()) +
      genAUX(RReference(null))
  }

  /**
    * Generating class `className` with value of type `innerType`
    */
  def genRefClass[T <: PType](className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
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
    ALOAD[StackNil, PRef[T]](0) ~
      INVOKESPECIAL("class string of Object", "()V)")
    RETURN
  }
}
