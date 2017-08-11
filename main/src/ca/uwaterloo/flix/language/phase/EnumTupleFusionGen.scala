/*
 * Copyright 2015-2017 Ramin Zarifi
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.phase.CodegenHelper._
import ca.uwaterloo.flix.language.phase.EnumGen.{compileGetBoxedEnumFieldMethod, compileGetTagMethod}
import ca.uwaterloo.flix.language.phase.TupleGen.compileGetBoxedValueMethod
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import org.objectweb.asm
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._

/**
  * At this phase, we create java classes representing a class which is a fusion of both tuples and
  * enum cases. Fusion classes include all methods in both tuple and enum case.
  * When we generate an enum which has a tuple as it's field, we will generate a fusion object instead of generating
  * one tuple object and one enum object.
  *
  * The steps that we take to generate enums is as follows:
  *
  * 1. Find enums and group them by symbols.
  * We will recursively traverse the AST to find all enum case types. We will also extract the type of the field of
  * the enum at this stage, so each element of the list is (EnumType, (caseName, fieldType)).
  * Then we will group enums based on their enum symbols.
  *
  * 2. Find the enums which have a tuple as their field.
  * We will find the enums which have a tuple as their field. After that, we group tuples that have the same field representation so we only generate one class for them.
  * If a field is a primitive, then it can be represented by it's primitive but if the field is not a primitive then it
  * has to be represented using an object. Then, we generate representation of  all the tuple classes that we have to create. If a field is a primitive
  * then we wrap the field inside `WrappedPrimitive` and if the field is not a primitive then we wrap all the types that
  * will be represented using `object` on this tuple inside `WrappedNonPrimitives`.
  *
  * 3. Generate bytecode for fusion object.
  * At this stage, we emit code for fusion objects which implements all methods implemented by tuple classes and classes
  * for enum cases.
  */
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

    // 2. Find the enums which have a tuple as their field.
    val enumTupleFusions: Map[EnumSym, Map[String, List[List[WrappedType]]]] =
      enumsGroupedBySymbol.map{
        case (sym, li) =>
          val enumsWithTupleFields = li.filter(_._2._2.isTuple)
            .groupBy(x => x._2._1)
            .map(x => (x._1, groupedFieldsToWrappedFields(groupTuplesByFieldTypes(x._2.map(_._2._2))).toList))
          sym -> enumsWithTupleFields
      }.toMap // Despite IDE highlighting, this is actually necessary.

    // 3. Generate bytecode for fusion object.
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

  /**
    *
    * @param clazzName
    * @param tupleInterfaceName
    * @param enumInterfaceName
    * @param fieldTypes
    * @return
    */
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

    // Generate `toString` method
    val stringDescriptor = asm.Type.getDescriptor(Constants.stringClass)
    exceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", s"()$stringDescriptor", "toString method shouldn't be called")


    //compileFusionEqualsMethod(visitor, clazzName, enumInterfaceName, tupleInterfaceName, fieldTypes, enumFieldType)
    val clazz = Constants.objectClass
    exceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", s"(${asm.Type.getDescriptor(clazz)})Z", "Equals method shouldn't be called")

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
