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

import ca.uwaterloo.flix.api.{Flix, TagInterface}
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ExecutableAst.Definition
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import org.objectweb.asm
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import CodegenHelper._

object EnumGen extends Phase[ExecutableAst.Root, ExecutableAst.Root] {

  /**
    * At this phase, we create java classes and interfaces representing Enum types and Enum cases.
    * The steps that we take to generate enums is as follows:
    *
    * 1. Transform non-functions and generate interface names
    * We transform non functions to functions with no argument, then we use this list of definitions
    * to generate interface names which is required in further steps to emit the correct signature for enum functions
    *
    * 2. Find enums and group them by symbols.
    * We will recursively traverse the AST to find all enum case types. We will also extract the type of the field of
    * the enum at this stage, so each element of the list is (EnumType, (caseName, fieldType)).
    * Then we will group enums based on their enum symbols.
    *
    * 3. Generate byteCodes of Enum Interface.
    * At this stage, we generate an interface for each enum symbol. All cases of that symbol will implement this interface.
    * This interface extends `tagInterface` interface which has two methods: `getBoxedValue()` and `getTag()`. For example,
    * for enum result defined as follows:
    *
    * enum Result[t, e] {
    *   case Ok(t),
    *   case Err(e)
    * }
    *
    * we generate the bytecode corresponding to following class:
    *
    * package ca.waterloo.flix.enums.Result;
    * import ca.uwaterloo.flix.api.TagInterface;
    * public interface EnumInterface extends TagInterface {
    * }
    *
    * We put the result of the bytecodes for all enums inside a map from symbol to (fullyQualifiedName, bytecode)
    *
    * 4. Separate primitive enums and object enums
    * For each enum symbol, we separate enum cases with that symbol which their field is of primitive type from cases
    * which their fields requires to be represented as an object. For the ones that require an object field, we will just
    * keep the case name and for the ones that have a primitive field, we will keep both the case name and the type of
    * the field.
    *
    * 5. Generate bytecode for each enum case.
    * A class will be generated for each enum case. This class contains two fields: `tag` and `value`. `tag` is the name
    * of the enum case represented as a string and `value` contains the field of the case. If the field is primitive, then
    * `value` is of the type of that primitive, otherwise,`value` is just an object. For example, for the case `Ok[Int32]`
    * we generate:
    *
    * public int value;
    *
    * but for the case `Ok[List[Int32]]` we generate:
    *
    * public Object value;
    *
    * Classes generated at this step implements the interface corresponding the symbol of the enum case and they include
    * implementations of following methods: `getTag()`, `getValue()`, `getBoxedValue()`,`toString()`, `hashCode()` and
    * `equals(Object)`.
    * `getTag()` is the function which returns the value of the `tag` field. `getValue()` returns the value of `value` field.
    * `getBoxedValue()` returns the `value` field but the result is boxed inside an object. As an example, `getValue()` and
    * `getBoxedValue()` of the class representing `Ok[Int32]` is as follows:
    *
    * public final int getValue() {
    *   return this.value;
    * }
    *
    * public final Object getBoxedValue() {
    *   return new Integer(this.value);
    * }
    *
    * `toString()` returns a representation of the enum case with `CaseName(fieldRepresentation)` as its format. For example
    * for `Some[Int]` we generate the following method:
    *
    * public String toString() {
    *   return "Some(".concat(String.valueOf(this.value).concat(")"));
    * }
    *
    * `hashCode()` returns a hashCode corresponding to this enum case. hashCode is the the hash of a list containing namespace
    * of the enum symbol, enum name and case name multiplied by 7 added to the hashCode of the field of the enum multiplied by 11.
    * For example, for enum `Some[Bool]` we generate the following method:
    *
    * public int hashCode() {
    *   return 7 * -1721515276 + 11 * this.value;
    * }
    *
    * and for `Some[List[Int]]` we generate the following method:
    *
    * public int hashCode() {
    *   return 7 * -1721515276 + 11 * this.value;
    * }
    *
    * Finally, for `equals(object)`, we first check that `object` is of the type of the class, then we invoke `equals()`
    * method on the `value` field of `this` and `object`. If the field is primitive then we just use `==` sign to compare
    * fields. For example for `Result[Int32]` we generate:
    *
    * public boolean equals(Object var1) {
    *   return var1 instanceof Ok && this.value == ((Ok)var1).value;
    * }
    *
    * and for `Err[List[Int32]]` we generate:
    *
    * public boolean equals(Object var1) {
    *   return var1 instanceof Err?this.value.equals(((Err)var1).value):false;
    * }
    */
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    if (flix.options.evaluation == Evaluation.Interpreted) {
      return root.toSuccess
    }

    // 1. Transform non-functions and generate interface names.
    val constantsList: List[Definition.Constant] = root.definitions.values.map { f =>
      f.tpe match {
        case Type.Apply(Type.Arrow(l), _) => f
        case t => f.copy(tpe = Type.mkArrow(List(), t))
      }
    }.toList.filterNot(_.ann.isLaw)

    val interfaceNames: Map[Type, QualName] = generateInterfaceNames(constantsList)

    // 2. Find enums and group them by symbols.
    val allEnums: List[(Type, (String, Type))] = root.definitions.values.flatMap(x => findEnums(x.exp)).toList

    val enumsGroupedBySymbol: Map[EnumSym, List[(Type, (String, Type))]] = allEnums.groupBy{ case (tpe, _) => tpe match {
      case Type.Apply(Type.Enum(s, _), _) => s
      case Type.Enum(s, _) => s
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }}

    // 3. Generate byteCodes of Enum Interface.
    val enumInterfaces : Map[EnumSym, (QualName, Array[Byte])] = enumsGroupedBySymbol.map{ case (sym, _) =>
      val name = CodegenHelper.getEnumInterfaceName(sym)
      sym -> (name, compileEnumInterface(name)(flix))
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 4. Separate primitive enums and object enums.
    val enumsGroupedBySymbolAndFieldType : Map[EnumSym, (Set[(String, Type)], Set[String])] = enumsGroupedBySymbol.map{case (sym, li) =>
      val (primEnums, enumsWithObjField) = li.partition(_._2._2.isPrimitive)
      sym -> (primEnums.map(x => (x._2._1, x._2._2)).toSet, enumsWithObjField.map(x => x._2._1).toSet)
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 5. Generate bytecode for each enum case.
    val enumByteCodes = enumsGroupedBySymbolAndFieldType.map{ case (sym, (primEnums, objectEnums)) =>
      val generatedPrimEnums : Map[(String, Type), Array[Byte]] = primEnums.map{ case (name, tpe) =>
        val qualName = CodegenHelper.getEnumCaseName(sym, name, Some(tpe))
        val interface = enumInterfaces(sym)._1
        (name, tpe) -> EnumGen.compileEnumClass(qualName, interface, Some(tpe), interfaceNames, flix.javaVersion)
      }.toMap
      val generatedObjectEnums : Map[String, Array[Byte]] = objectEnums.map{ name =>
        val fullName = CodegenHelper.getEnumCaseName(sym, name, None)
        val interface = enumInterfaces(sym)._1
        name -> EnumGen.compileEnumClass(fullName, interface, None, interfaceNames, flix.javaVersion)
      }.toMap
      sym -> (generatedPrimEnums, generatedObjectEnums)
    }

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(enumGen = e), enumInterfaceByteCodes = enumInterfaces, enumClassByteCodes = enumByteCodes).toSuccess
  }

  /**
    * Generates an interface for each enum.
    * Each case of the enum implements this interface. This interface extends `TagInterface`.
    * For example, for enum `Result` we create:
    *
    * package ca.waterloo.flix.enums.Result;
    * import ca.uwaterloo.flix.api.TagInterface;
    * public interface EnumInterface extends TagInterface {
    * }
    *
    * @param qualName Qualified Name of the interface to be generated
    * @return byte code representation of the class
    */
  def compileEnumInterface(qualName: QualName)(flix: Flix): Array[Byte] = {
    val visitor = new ClassWriter(0)

    visitor.visit(flix.javaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, decorate(qualName), null,
      asm.Type.getInternalName(Constants.objectClass), Array(asm.Type.getInternalName(Constants.tagInterface)))

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generates the class for each enum case.
    * Each class implements the interface of enum.
    * @param qualName Qualified name of the generated class
    * @param interfaceName Qualfied name of the interface of the Enum
    * @param fType Type of the `value` field, None means that the type is not a primitive
    * @param interfaces Map of functional interfaces to their qualified name
    * @return byte code representation of the class
    */
  def compileEnumClass(qualName: QualName,
                       interfaceName: QualName,
                       fType: Option[Type],
                       interfaces: Map[Type, QualName],
                       javaVersion: Int): Array[Byte] = {
    // Initialize the class writer.
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES)

    // Initialize the visitor to create a class.
    visitor.visit(javaVersion, ACC_PUBLIC + ACC_FINAL, decorate(qualName), null, asm.Type.getInternalName(Constants.objectClass),
      Array(decorate(interfaceName.ref)))

    // Descriptor of the the `value` field of the class
    val fieldDescriptor = getFieldTypeDescriptor(fType, interfaces)

    // Generate tag and value fields
    compileEnumField(visitor, "value", fieldDescriptor)
    compileEnumField(visitor, "tag", asm.Type.getDescriptor(Constants.stringClass))

    // Generate the constructor of the class
    compileEnumConstructor(visitor, qualName, fieldDescriptor, fType)

    // Generate the `getValue` method
    // Picking the appropriate opcode for return
    val retCode = fType match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$fieldDescriptor'.")
      case Some(Type.Bool) | Some(Type.Char) | Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) => IRETURN
      case Some(Type.Int64) => LRETURN
      case Some(Type.Float32) => FRETURN
      case Some(Type.Float64) => DRETURN
      case _ => ARETURN
    }
    compileGetFieldMethod(visitor, qualName, fieldDescriptor, "value", "getValue", retCode)

    // Generate `getTag` method
    compileGetFieldMethod(visitor, qualName, asm.Type.getDescriptor(Constants.stringClass), "tag", "getTag", ARETURN)

    // Generate `hashCode` method
    compileHashCodeMethod(visitor, qualName, fieldDescriptor, fType)

    // Generate `toString` method
    compileToStringMethod(visitor, qualName, fieldDescriptor, fType)

    // Generate `equals` method
    compileEqualsMethod(visitor, qualName, fieldDescriptor, fType)

    // Generate `getBoxedValue` method
    compileGetBoxedValueMethod(visitor, qualName, fieldDescriptor, fType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Creates a field of name `name` with the type `descriptor` on the class generated by `visitor`
    * Currently the only field generated are `tag` and `value`.
    * For example for the case `Err[Bool]`, we generate:
    *
    * public boolean value;
    * public String tag;
    *
    * And for `Error[List[Int32]]` we generate:
    *
    * public boolean value;
    * public String tag;
    *
    * @param visitor class visitor
    * @param name name of the field
    * @param descriptor descriptor of field
    */
  private def compileEnumField(visitor: ClassWriter, name: String, descriptor: String) = {
    val field = visitor.visitField(ACC_PUBLIC, name, descriptor, null, null)
    field.visitEnd()
  }

  /**
    * Returns descriptor of the field.
    * if the field is of primitive type it returns it's descriptor, otherwise it returns descriptor of object class.
    * @param fType type of the `value` field of the class
    * @param interfaces generated functional interfaces
    */
  private def getFieldTypeDescriptor(fType: Option[Type], interfaces: Map[Type, QualName]): String = {
    fType match {
      case Some(t) if t.isPrimitive => CodegenHelper.descriptor(t, interfaces)
      case _ => asm.Type.getDescriptor(Constants.objectClass)
    }
  }

  /**
    * Creates the single argument constructor of the enum case class which is named `qualName`.
    * The only argument required to instantiate the class is the `value`.
    * The type of the field of the case is give by `descriptor`
    * @param visitor class visitor
    * @param qualName name of the class
    * @param descriptor descriptor of the `value` field
    * @param fType type of the `value` field
    */
  private def compileEnumConstructor(visitor: ClassWriter, qualName: QualName, descriptor: String, fType: Option[Type]) = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", s"($descriptor)V", null, null)
    val clazz = Constants.objectClass
    val ctor = clazz.getConstructor()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, asm.Type.getInternalName(clazz), "<init>",
      asm.Type.getConstructorDescriptor(ctor), false)

    // Load instruction for type of `value`
    val iLoad : Int = fType match {
      case Some(Type.Var(id, _)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$descriptor'.")
      case Some(Type.Bool) | Some(Type.Char) | Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) => ILOAD
      case Some(Type.Int64) => LLOAD
      case Some(Type.Float32) => FLOAD
      case Some(Type.Float64) => DLOAD
      case _ => ALOAD
    }

    // Put the object given to the construct on the `value` field
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitVarInsn(iLoad, 1)
    constructor.visitFieldInsn(PUTFIELD, decorate(qualName), "value", descriptor)

    // Put the name of the tag on the `tag` field
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitLdcInsn(qualName.ref.last)
    constructor.visitFieldInsn(PUTFIELD, decorate(qualName), "tag", asm.Type.getDescriptor(Constants.stringClass))

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  /**
    * Generate the `methodName` method for fetching the `fieldName` field of the class.
    * `name` is name of the class and `descriptor` is type of the `fieldName` field.
    * This method generates `getValue()` and `getTag()` methods of the class.
    * For example, `Val[Char]` has following `getValue()` and `getTag()` methods:
    *
    * public final char getValue() {
    *   return this.value;
    * }
    *
    *
    * public final String getTag() {
    *   return this.tag;
    * }
    *
    * @param visitor class visitor
    * @param qualName Qualified name of the class
    * @param descriptor descriptor of the `value` field of the class
    * @param fieldName name of the field
    * @param methodName method name of getter of `fieldName`
    * @param iReturn opcode for returning the value of the field
    */
  private def compileGetFieldMethod(visitor: ClassWriter, qualName: QualName, descriptor: String, fieldName: String,
                                    methodName: String, iReturn: Int) = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, methodName, s"()$descriptor", null, null)

    method.visitCode()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, decorate(qualName), fieldName, descriptor)
    method.visitInsn(iReturn)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `getBoxedValue` method which returns the boxed value of `value` field of the class.
    * The generated method will return the `value` field if the field is of object type, otherwise, it will
    * box the object using the appropriate type.
    * For example, we generate the following method for `Ok[Int32]`:
    *
    * public final Object getBoxedValue() {
    *   return new Integer(this.value);
    * }
    *
    * And we generate the following method for `Ok[List[Int32]]`
    *
    * public final Object getBoxedValue() {
    *   return this.value;
    * }
    *
    * @param visitor class visitor
    * @param qualName Qualified name of the class
    * @param descriptor descriptor of the `value` field of the class
    * @param fType type of the `value` field of the class
    */
  private def compileGetBoxedValueMethod(visitor: ClassWriter, qualName: QualName, descriptor: String, fType: Option[Type]) = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getBoxedValue", s"()Ljava/lang/Object;", null, null)

    method.visitCode()

    /**
      * This method will box the primitive on top of the stack
      * @param boxedObjectDescriptor descriptor of the boxed version of the primitive
      * @param signature signature of the constructor
      */
    def box(boxedObjectDescriptor: String, signature: String) = {
      method.visitTypeInsn(NEW, boxedObjectDescriptor)
      method.visitInsn(DUP)
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(qualName), "value", descriptor)
      method.visitMethodInsn(INVOKESPECIAL, boxedObjectDescriptor, "<init>", signature, false)
    }


    // based on the type of the field, we pick the appropriate class that boxes the primitive
    fType match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$descriptor'.")
      case Some(Type.Bool) => box("java/lang/Boolean", "(Z)V")
      case Some(Type.Char) => box("java/lang/Character", "(C)V")
      case Some(Type.Int8) => box("java/lang/Byte", "(B)V")
      case Some(Type.Int16) => box("java/lang/Short", "(S)V")
      case Some(Type.Int32) => box("java/lang/Integer", "(I)V")
      case Some(Type.Int64) => box("java/lang/Long", "(J)V")
      case Some(Type.Float32) => box("java/lang/Float", "(F)V")
      case Some(Type.Float64) => box("java/lang/Double", "(D)V")
      case _ =>
        method.visitVarInsn(ALOAD, 0)
        method.visitFieldInsn(GETFIELD, decorate(qualName), "value", descriptor)
    }

    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `hashCode` for the class.
    * The hashCode in this case is `7 * name.hashCode() + 11 * this.value.hashCode()` if `value` field is object
    * otherwise if the field is primitive, the hashCode is `7 * name.hashCode() + 11 * this.value`
    * 'descriptor' is type of `value` field.
    * @param visitor class visitor
    * @param qualName Qualified name of the class
    * @param descriptor descriptor of the `value` field of the class
    * @param fType type of the underlying `value`
    */
  private def compileHashCodeMethod(visitor: ClassWriter, qualName: QualName, descriptor: String, fType: Option[Type]) = {
    val clazz = Constants.objectClass
    val objectMethod = clazz.getMethod("hashCode")
    val method = visitor.visitMethod(ACC_PUBLIC, "hashCode", "()I", null, null)

    method.visitCode()
    method.visitLdcInsn(7)
    method.visitLdcInsn(qualName.hashCode())
    method.visitInsn(IMUL)
    method.visitLdcInsn(11)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, decorate(qualName), "value", descriptor)

    /*
     * If the field is of type Object, we get the hashCode of the object
     * Otherwise, we convert the type to integer and use in hash code computation
     */
    fType match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$descriptor'.")
      case Some(Type.Bool) | Some(Type.Char) | Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) =>
      case Some(Type.Int64) => method.visitInsn(L2I)
      case Some(Type.Float32) => method.visitInsn(F2I)
      case Some(Type.Float64) => method.visitInsn(D2I)
      case _ => method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), objectMethod.getName,
        asm.Type.getMethodDescriptor(objectMethod), false)
    }

    method.visitInsn(IMUL)
    method.visitInsn(IADD)

    method.visitInsn(IRETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `toString` method for the class. The method returns `tag(value.toString())`.
    * For example, for `Ok[Bool]` we generate the following method:
    *
    * public String toString() {
    *   return "Ok(".concat(String.valueOf(this.value).concat(")"));
    * }
    *
    * And for `Ok[List[Int32]]` we generate the following method:
    *
    * public String toString() {
    *   return "Ok(".concat(this.value.toString().concat(")"));
    * }
    *
    * @param visitor class visitor
    * @param qualName Qualified name of the class
    * @param tpe descriptor of the `value` field of the class
    * @param fType type of the `value` field
    */
  private def compileToStringMethod(visitor: ClassWriter, qualName: QualName, tpe: String, fType: Option[Type]) = {
    val objectInternalName = asm.Type.getInternalName(Constants.objectClass)
    val stringInternalName = asm.Type.getInternalName(Constants.stringClass)
    val stringConcatMethod = Constants.stringClass.getMethod("concat", Constants.stringClass)

    val method = visitor.visitMethod(ACC_PUBLIC, "toString", s"()${asm.Type.getDescriptor(Constants.stringClass)}", null, null)

    method.visitCode()
    method.visitLdcInsn(qualName.ref.last.concat("("))
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, decorate(qualName), "value", tpe)

    /*
     * Converting `value` to String.
     * If it's an object, we will call `toString` on the object.
     * Otherwise, we use `valueOf` static method on String with the appropriate type.
     */
    fType match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Some(Type.Bool) =>
        val boolToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Boolean])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, boolToStringMethod.getName,
          asm.Type.getMethodDescriptor(boolToStringMethod), false)
      case Some(Type.Char) =>
        val charToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Char])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, charToStringMethod.getName,
          asm.Type.getMethodDescriptor(charToStringMethod), false)
      case Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) =>
        val intToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Int])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, intToStringMethod.getName,
          asm.Type.getMethodDescriptor(intToStringMethod), false)
      case Some(Type.Int64) =>
        val longToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Long])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, longToStringMethod.getName,
          asm.Type.getMethodDescriptor(longToStringMethod), false)
      case Some(Type.Float32) =>
        val floatToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Float])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, floatToStringMethod.getName,
          asm.Type.getMethodDescriptor(floatToStringMethod), false)
      case Some(Type.Float64) =>
        val doubleToStringMethod = Constants.stringClass.getMethod("valueOf", classOf[Double])
        method.visitMethodInsn(INVOKESTATIC, stringInternalName, doubleToStringMethod.getName,
          asm.Type.getMethodDescriptor(doubleToStringMethod), false)
      case _ =>
        val objectToStringMethod = Constants.objectClass.getMethod("toString")
        method.visitMethodInsn(INVOKEVIRTUAL, objectInternalName, objectToStringMethod.getName,
          asm.Type.getMethodDescriptor(objectToStringMethod), false)
    }

    method.visitLdcInsn(")")


    // We concatenate twice since there is 3 strings on the stack that we want to concat them together
    for(_ <- 0 until 2) {
      method.visitMethodInsn(INVOKEVIRTUAL, stringInternalName, stringConcatMethod.getName,
        asm.Type.getMethodDescriptor(stringConcatMethod), false)
    }

    method.visitInsn(ARETURN)
    method.visitMaxs(1, 10)
    method.visitEnd()
  }

  /**
    * Generate the `equals` method for the class.
    * we first check that `object` is of the type of the class, then we invoke `equals()`
    * method on the `value` field of `this` and `object`. If the field is primitive then we just use `==` sign to compare
    * fields. For example for `Result[Int32]` we generate:
    *
    * public boolean equals(Object var1) {
    *   return var1 instanceof Ok && this.value == ((Ok)var1).value;
    * }
    *
    * and for `Err[List[Int32]]` we generate:
    *
    * public boolean equals(Object var1) {
    *   return var1 instanceof Err?this.value.equals(((Err)var1).value):false;
    * }
    *
    * @param visitor class visitor
    * @param qualName Qualified name of the class
    * @param descriptor descriptor of the `value` field
    * @param fType Type of the `value` field
    */
  private def compileEqualsMethod(visitor: ClassWriter, qualName: QualName, descriptor: String, fType: Option[Type]) = {
    val clazz = Constants.objectClass
    val objectEqualsMethod = clazz.getMethod("equals", clazz)
    val neq = new Label() //label for when the object is not instanceof tag case

    val method = visitor.visitMethod(ACC_PUBLIC, "equals", s"(${asm.Type.getDescriptor(clazz)})Z", null, null)
    method.visitCode()
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(INSTANCEOF, decorate(qualName)) // compare the types
    method.visitJumpInsn(IFEQ, neq) // if types don't match go to `neq`
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, decorate(qualName), "value", descriptor)
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(CHECKCAST, decorate(qualName)) // cast to the current class
    method.visitFieldInsn(GETFIELD, decorate(qualName), "value", descriptor) // get the value field

    // This will pick the appropriate comparison for the type of the `value`
    fType match {
      case Some(Type.Var(id, kind)) =>  throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$fType'.")
      case Some(Type.Bool) | Some(Type.Char) | Some(Type.Int8) | Some(Type.Int16) | Some(Type.Int32) =>
        method.visitJumpInsn(IF_ICMPNE, neq)
        method.visitInsn(ICONST_1)
      case Some(Type.Int64) =>
        method.visitInsn(LCMP)
        method.visitJumpInsn(IFNE, neq)
        method.visitInsn(ICONST_1)
      case Some(Type.Float32) =>
        method.visitInsn(FCMPG)
        method.visitJumpInsn(IFNE, neq)
        method.visitInsn(ICONST_1)
      case Some(Type.Float64) =>
        method.visitInsn(DCMPG)
        method.visitJumpInsn(IFNE, neq)
        method.visitInsn(ICONST_1)
      case _ =>
        method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(clazz), objectEqualsMethod.getName,
          asm.Type.getMethodDescriptor(objectEqualsMethod), false)
    }

    method.visitInsn(IRETURN)
    method.visitLabel(neq) // if the code reaches here, it means that `instanceof` has returned false
    method.visitInsn(ICONST_0)
    method.visitInsn(IRETURN)
    method.visitMaxs(10, 10)

    method.visitEnd()
  }
}
