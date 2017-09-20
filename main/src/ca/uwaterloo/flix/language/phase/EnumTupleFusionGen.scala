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
import ca.uwaterloo.flix.language.phase.EnumGen.{compileGetBoxedTagValueMethod, compileGetTagMethod}
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
  * one tuple object and one enum object as an optimization.
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
            .map(x => (x._1, groupedFieldsToWrappedFields(x._2.map(_._2._2)).toList))
          sym -> enumsWithTupleFields
      }.toMap // Despite IDE highlighting, this is actually necessary.

    // 3. Generate bytecode for fusion object.
    val byteCodes: Map[EnumSym, Map[String, Map[List[WrappedType], Array[Byte]]]] = enumTupleFusions.map{ case (sym, cases) =>
      val underlying = cases.map{ case (tag, types) =>
        val tpeToByteCode = types.map{ tpe =>
          val qualName = ETFClassName(sym, tag, tpe) // Qualified name of the fusion class
          val tupleInterfaceName = TupleInterfaceName(tpe) // Qualified name of the interface of the tuple part of the fusion
          val enumInterface = EnumTypeInterfaceName(sym) // Qualified name of the interface of the enum part of the fusion
          tpe -> compileEnumTupleFusion(qualName, tupleInterfaceName, enumInterface, tpe) // bytecode of the fusion class
        }.toMap
        tag -> tpeToByteCode
      }.toMap // Despite IDE highlighting, this is actually necessary.
      sym -> underlying
    }.toMap // Despite IDE highlighting, this is actually necessary.

    val e =  System.nanoTime() - t
    root.copy(byteCodes = root.byteCodes.copy(ETFusionByteCode = byteCodes), time = root.time.copy(fusionGen = e)).toSuccess
  }

  /**
    * We generate bytecode of the class identified by `clazzName` QualName. This class implements both `tupleInterfaceName`
    * and `enumTypeInterfaceName`.
    *
    * First, we precede with creating a field for each element of the tuple on the class. We use `compileField` helper with
    * name = `field${ind}` with `ind` being the index of the element in tuple and with descriptor obtained from `getWrappedTypeDescriptor`.
    * For example, if the second element of type is of type `WrappedPrimitive(Bool)`, we create the following
    * field on the class:
    *
    * private boolean field1;
    *
    * and if the 5th element of the tuple if of type `WrappedNonPrimitives(Set(..))` we create the following field on the class:
    *
    * private Object field4;
    *
    * Each field has a getter and a setter which the first one returns the field. For example the following field:
    *
    * private Object field4;
    *
    * has the following getters and setters:
    *
    * public Object getIndex4() {
    *   return field4;
    * }
    *
    * public void setIndex4(Object obj) {
    *   field4 = obj;
    * }
    *
    *
    * Now we generate fields and methods for the enum part of the class. This means that we have to generate `getValue`,
    * `getTag`, `getBoxedEnumField`, `toString` and `equals`.
    * `getTag()` is the function which returns the name of the enum case. `getValue()` returns the value of enum part of the
    *  fusion object which is really the object itself so we just return `this`.
    * `getBoxedEnumField()` returns the `value` field but the result is boxed inside an object. As an example, `getValue()` and
    * `getBoxedEnumField()` of the class representing `Ok[(Int32, Int32)]` is as follows:
    *
    * public final Object getValue() {
    *   return this;
    * }
    *
    * public final Object getBoxedEnumField() {
    *   return this;
    * }
    *
    * Next, we will generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * The `toString` method is always the following:
    *
    * public String toString() throws Exception {
    *   throw new Exception("equals method shouldn't be called")
    * }
    *
    * Then, we will generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    *   throw new Exception("equals method shouldn't be called")
    * }
    *
    * Finally, we generate the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    *   throw new Exception("equals method shouldn't be called");
    * }
    *
    *
    * @param clazzName Qualified name of the fusion class to be generated
    * @param tupleInterfaceName Qualified name of the tuple interface of the fusion
    * @param enumInterfaceName Qualified name of the enum interface of the fusion
    * @param fieldTypes type of fields of the fusion
    * @return Bytecode of the generated class
    */
  private def compileEnumTupleFusion(clazzName: ETFClassName,
                                     tupleInterfaceName: TupleInterfaceName,
                                     enumInterfaceName: EnumTypeInterfaceName,
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

    // Emit code for the constructor
    TupleGen.compileTupleConstructor(visitor, clazzName, fieldTypes)

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
    compileGetBoxedTagValueMethod(visitor, clazzName, enumFieldType)

    // Generate `toString` method
    val stringDescriptor = asm.Type.getDescriptor(Constants.stringClass)
    exceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", s"()$stringDescriptor", "toString method shouldn't be called")

    // Generate `hashCode` method
    exceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", s"()I", "hashCode method shouldn't be called")

    // Generate `equals` method
    val clazz = Constants.objectClass
    exceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", s"(${asm.Type.getDescriptor(clazz)})Z", "Equals method shouldn't be called")

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * This will generate a method for class identified by `qualName` which return the `value` of the field of the enum
    * which is just `this`.
    * @param visitor ClassWriter to emit method to the class
    * @param qualName Qualified name of the tuple class
    */
  def compileGetEnumValueMethod(visitor: ClassWriter, qualName: QualName) : Unit = {
    val objectDescriptor = asm.Type.getDescriptor(Constants.objectClass)
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getValue", s"()$objectDescriptor", null, null)
    method.visitCode()

    // load `this`
    method.visitVarInsn(ALOAD, 0)

    // return this
    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }
}