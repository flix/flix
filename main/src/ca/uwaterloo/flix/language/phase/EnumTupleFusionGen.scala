package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.phase.CodegenHelper._
import ca.uwaterloo.flix.language.phase.EnumGen.{compileGetBoxedEnumFieldMethod, compileGetTagMethod}
import ca.uwaterloo.flix.language.phase.LoadBytecode.dump
import ca.uwaterloo.flix.language.phase.TupleGen.compileGetBoxedValueMethod
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import org.objectweb.asm
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._
/**
  * Created by ramin on 2017-08-02.
  */

/*
object EnumTupleFusionGen extends Phase[ExecutableAst.Root, ExecutableAst.Root] {
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    if (flix.options.evaluation == Evaluation.Interpreted) {
      return root.toSuccess
    }

    // 1. Find enums and group them by symbols.
    val allEnums: List[(Type, (String, Type))] = root.defs.values.flatMap(x => findEnumCases(x.exp)).toList

    val enumsGroupedBySymbol: Map[EnumSym, List[(Type, (String, Type))]] = allEnums.groupBy{ case (tpe, _) => tpe match {
      case Type.Apply(Type.Enum(s, _), _) => s
      case Type.Enum(s, _) => s
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }}

    val enumTupleFusions: Map[EnumSym, Map[String, List[List[WrappedType]]]] =
      enumsGroupedBySymbol.map{
        case (sym, li) =>
          val enumsWithTupleFields = li.filter(_._2._2.isTuple)
            .groupBy(x => x._2._1)
            .map(x => (x._1, groupedFieldsToWrappedFields(groupTuplesByFieldTypes(x._2.map(_._2._2))).toList))
          sym -> enumsWithTupleFields
      }.toMap // Despite IDE highlighting, this is actually necessary.

    val byteCodes: Map[EnumSym, Map[String, Map[List[WrappedType], Array[Byte]]]] = enumTupleFusions.map{ case (sym, cases) =>
      val underlying = cases.map{ case (tag, types) =>
          val tpeToByteCode = types.map{ tpe =>
            val qualName = ETFClassName(sym, tag, tpe)
            val tupleInterfaceName = TupleInterfaceName(tpe)
            val enumInterface = EnumCaseInterfaceName(sym, tag, WrappedNonPrimitives(Set()))
            tpe -> compileEnumTupleFusion(qualName, tupleInterfaceName, enumInterface, tpe)
          }.toMap
          tag -> tpeToByteCode
      }.toMap
      sym -> underlying
    }.toMap

    val e =  System.nanoTime() - t
    root.copy(byteCodes = root.byteCodes.copy(ETFusionByteCode = byteCodes), time = root.time.copy(tupleGen = e)).toSuccess
  }

  private def compileEnumTupleFusion(clazzName: ETFClassName,
                                     tupleInterfaceName: TupleInterfaceName,
                                     enumInterfaceName: EnumCaseInterfaceName,
                                     fieldTypes: List[WrappedType]): Array[Byte] =
  {
    /*
     * Initialize the class writer. We override `getCommonSuperClass` method because `asm` implementation of this
     * function requires types to loaded so that they can be compared to each other.
     */
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES){
      override def getCommonSuperClass(tpe1: String, tpe2: String) : String = {
        asm.Type.getInternalName(Constants.objectClass)
      }
    }

    // Super descriptor
    val superDescriptor = asm.Type.getInternalName(Constants.objectClass)

    // Descriptors of implemented interfaces
    val interfaceDescriptors = Array(decorate(tupleInterfaceName), decorate(enumInterfaceName))

    // Initialize the visitor to create a class.
    visitor.visit(JavaVersion, ACC_PUBLIC + ACC_FINAL, decorate(clazzName), null, superDescriptor, interfaceDescriptors)

    /**
      * Fields and Methods for Tuple part of the class
      */
    // Adding fields and methods required for Tuple
    fieldTypes.zipWithIndex.foreach{ case (field, ind) =>
      // Descriptor of the field
      val desc = getWrappedTypeDescriptor(field)

      // Name of the field
      val fieldName = s"field$ind"

      // Defining fields of the tuple
      compileField(visitor, fieldName, desc, isStatic = false, isPrivate = true)

      // Emitting getter for each field
      compileGetFieldMethod(visitor, clazzName, desc, fieldName, s"getIndex$ind", getReturnInsn(field))

      // Emitting setter for each field
      compileSetFieldMethod(visitor, clazzName, desc, fieldName, s"setIndex$ind", getLoadInstruction(field))
    }

    // Emit the code for `getBoxedValue()` method for Tuple
    compileGetBoxedValueMethod(visitor, clazzName, fieldTypes)

    /**
      * Fields and Methods for Enum part of the class
      */
    // type of the `value` field of enum
    val enumFieldType = WrappedNonPrimitives(Set())

    // Generate the `getValue` method
    compileGetEnumValueMethod(visitor, clazzName)

    // Generate `getTag` method
    compileGetTagMethod(visitor, clazzName)

    // Generate `getBoxedEnumField` method
    compileGetBoxedEnumFieldMethod(visitor, clazzName, enumFieldType)

    /**
      * Specialized methods
      */

    compileFusionConstructor(visitor, clazzName, fieldTypes, enumFieldType)

    //compileFusionEqualsMethod(visitor, clazzName, enumInterfaceName, tupleInterfaceName, fieldTypes, enumFieldType)
    compileEqualsWithException(visitor, clazzName)

    compileFusionHashCodeMethod(visitor, clazzName, fieldTypes, enumFieldType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  def compileGetEnumValueMethod(visitor: ClassWriter, qualName: QualName) : Unit = {
    val objectDescriptor = asm.Type.getDescriptor(Constants.objectClass)
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getValue", s"()$objectDescriptor", null, null)

    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  private def compileFusionConstructor(visitor: ClassWriter,
                                       className: ETFClassName,
                                       fields: List[WrappedType],
                                       enumField: WrappedType): Unit = {
    val desc = fields.map(getWrappedTypeDescriptor).mkString

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", s"($desc)V", null, null)
    val clazz = Constants.objectClass
    val ctor = clazz.getConstructor()

    constructor.visitCode()

    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, asm.Type.getInternalName(clazz), "<init>",
      asm.Type.getConstructorDescriptor(ctor), false)

    var offset : Int = 1
    fields.zipWithIndex.foreach{ case (field, ind) =>
      val desc = getWrappedTypeDescriptor(field)
      val iLoad = getLoadInstruction(field)

      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitVarInsn(iLoad, offset)
      constructor.visitFieldInsn(PUTFIELD, decorate(className), s"field$ind",desc)

      field match {
        case WrappedPrimitive(Type.Int64) | WrappedPrimitive(Type.Float64) => offset += 2
        case _ => offset += 1
      }
    }

    // Return
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  private def compileFusionEqualsMethod(visitor: ClassWriter,
                                        className: ETFClassName,
                                        enumCaseName: EnumCaseInterfaceName,
                                        tupleInterfaceName: TupleInterfaceName,
                                        fields: List[WrappedType],
                                        enumField: WrappedType): Unit = {
    val clazz = Constants.objectClass
    val neq = new Label() //label for when the object is not instanceof tag case
    val eq = new Label()
    val checkFields = new Label()

    // label for when the object is not instanceof tuple case
    val instNotEq = new Label()
    // label for when the result of comparing an element of a tuple is false
    val valueNotEq = new Label()
    // MethodWriter to emit the code for method body

    // Descriptor of the `value` field
    val descriptor = getWrappedTypeDescriptor(enumField)

    val method = visitor.visitMethod(ACC_PUBLIC, "equals", s"(${asm.Type.getDescriptor(clazz)})Z", null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(INSTANCEOF, decorate(enumCaseName)) // compare the types
    method.visitJumpInsn(IFEQ, neq) // if types don't match go to `neq`
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(CHECKCAST, decorate(enumCaseName)) // cast to the current class
    method.visitFieldInsn(GETFIELD, decorate(enumCaseName), "value", descriptor) // get the value field
    method.visitTypeInsn(CHECKCAST, decorate(tupleInterfaceName))

    method.visitJumpInsn(GOTO, checkFields)

    method.visitJumpInsn(GOTO, eq)

    method.visitLabel(neq)
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(INSTANCEOF, decorate(tupleInterfaceName)) // compare the types
    method.visitJumpInsn(IFEQ, instNotEq) // if types don't match go to `instNotEq`

    // We cast the argument to current type only once, then we just duplicate the top of the stack.
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(CHECKCAST, decorate(tupleInterfaceName)) // cast to the current class

    method.visitLabel(checkFields)
    // Now we chack that all elements of tuple have equal values
    fields.zipWithIndex.foreach { case (field, ind) =>
      val desc = getWrappedTypeDescriptor(field)

      method.visitInsn(DUP)
      method.visitMethodInsn(INVOKEINTERFACE, decorate(tupleInterfaceName), s"getField$ind", s"()$desc", true)
      method.visitVarInsn(ALOAD, 0)
      method.visitMethodInsn(INVOKEVIRTUAL, decorate(className), s"getField$ind", s"()$desc", false)
      branchIfNotEqual(method, field, valueNotEq)
    }
    method.visitInsn(POP) // Popping the casted version of the argument
    method.visitLabel(eq)
    method.visitInsn(ICONST_1)
    method.visitInsn(IRETURN)
    method.visitLabel(valueNotEq) // if the code reaches here, it means that underlying values were not equal
    method.visitInsn(POP) // Popping the casted version of the argument
    method.visitLabel(instNotEq) // if we jump to this label it means the `instanceof` has returned false.
    method.visitInsn(ICONST_0)

    // Return
    method.visitInsn(IRETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(65535, 65535)
    method.visitEnd()
  }

  private def compileFusionHashCodeMethod(visitor: ClassWriter,
                                          className: ETFClassName,
                                          fields: List[WrappedType],
                                          enumField: WrappedType): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "hashCode", "()I", null, null)

    method.visitCode()
    method.visitLdcInsn(className.hashCode())

    // Now we loop over fields to compute the hash value
    fields.zipWithIndex.foreach { case (field, ind) =>
      // Multiplying the current hash value by 7
      method.visitLdcInsn(7)
      method.visitInsn(IMUL)

      // descriptor of the current field
      val desc = getWrappedTypeDescriptor(field)

      // Fetching the field
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(className), s"field$ind", desc)

      // Getting the hashCode of the field
      getHashCodeOrConvertToInt(method, field)

      // Adding the hash code to the accumulator
      method.visitInsn(IADD)
    }

    // Returning the hash
    method.visitInsn(IRETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

}
*/