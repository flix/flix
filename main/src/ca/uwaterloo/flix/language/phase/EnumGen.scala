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
    * 1. Find enums and group them by symbols.
    * We will recursively traverse the AST to find all enum case types. We will also extract the type of the field of
    * the enum at this stage, so each element of the list is (EnumType, (caseName, fieldType)).
    * Then we will group enums based on their enum symbols.
    *
    * 2. Generate byteCodes of Enum Interface.
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
    * 3. Separate primitive enums and object enums
    * For each enum symbol, we separate enum cases with that symbol which their field is of primitive type from cases
    * which their fields requires to be represented as an object. For the ones that require an object field, we will just
    * keep the case name and for the ones that have a primitive field, we will keep both the case name and the type of
    * the field.
    *
    * 4. Generate bytecode for each enum case.
    * A class will be generated for each enum case. This class contains one field: `value`. `value` contains the field of the case.
    * If the field is primitive, then `value` is of the type of that primitive, otherwise, `value` is just an object.
    * For example, for the case `Ok[Int32]` we generate:
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
    * `getTag()` is the function which returns the name of the enum case. `getValue()` returns the value of `value` field.
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
    * `toString()` has to special cases, first when the field of the enum is `Unit` we don't show the `Unit` on the result
    * of the representation of the case, second for case `Cons` of `List` enum we represent it using `::` symbol instead of
    * the traditional representation.
    *
    * hashCode() returns a hashCode corresponding to this enum case. hashCode is the the hash of a list containing namespace
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
    *   return 7 * -1721515276 + 11 * this.value.hashCode();
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

    // 1. Find enums and group them by symbols.
    val allEnums: List[(Type, (String, Type))] = root.definitions.values.flatMap(x => findEnumCases(x.exp)).toList

    val enumsGroupedBySymbol: Map[EnumSym, List[(Type, (String, Type))]] = allEnums.groupBy{ case (tpe, _) => tpe match {
      case Type.Apply(Type.Enum(s, _), _) => s
      case Type.Enum(s, _) => s
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }}

    // 2. Generate byteCodes of Enum Interface.
    val enumInterfaces : Map[EnumSym, (EnumInterfName, Array[Byte])] = root.enums.map{ case (sym, _) =>
      val name = EnumInterfName(sym)
      sym -> (name, compileEnumInterface(name)(flix))
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 3. Separate primitive enums and object enums.
    val enumsGroupedBySymbolAndFieldType : Map[EnumSym, (Set[(String, Type)], Map[String, Set[Type]])] =
      enumsGroupedBySymbol.map{
        case (sym, li) =>
          val (primEnums, enumsWithObjField) = li.partition(x => isPrimitive(x._2._2))
          sym -> (primEnums.map(x => (x._2._1, x._2._2)).toSet,
            enumsWithObjField.groupBy(x => x._2._1).map(x => (x._1, x._2.map(_._2._2).toSet)))
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 4. Generate bytecode for each enum case.
    val enumByteCodes = enumsGroupedBySymbolAndFieldType.map{ case (sym, (primEnums, objectEnums)) =>
      val generatedPrimEnums : Map[(String, Type), Array[Byte]] = primEnums.map{ case (name, tpe) =>
        val wrapped = WrappedPrimitive(tpe)
        val qualName = EnumClassName(sym, name, wrapped)
        val interface = enumInterfaces(sym)._1
        // A primitive enum cannot be a singleton
        val isSingleton = false
        (name, tpe) -> EnumGen.compileEnumClass(qualName, interface, wrapped, isSingleton = isSingleton)
      }.toMap
      val generatedObjectEnums : Map[String, Array[Byte]] = objectEnums.map{ case (name, tpes) =>
        val wrapped = WrappedNonPrimitives(tpes)
        val fullName = EnumClassName(sym, name, wrapped)
        val interface = enumInterfaces(sym)._1
        // If the type of the case field is `Unit` then this is a singleton
        val isSingleton = root.enums(sym).cases(name).tpe == Type.Unit
        name -> EnumGen.compileEnumClass(fullName, interface, wrapped, isSingleton = isSingleton)
      }.toMap
      sym -> (generatedPrimEnums, generatedObjectEnums)
    }

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(enumGen = e),
      byteCodes = root.byteCodes.copy(enumInterfaceByteCodes = enumInterfaces, enumClassByteCodes = enumByteCodes)).toSuccess
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
  def compileEnumInterface(qualName: EnumInterfName)(flix: Flix): Array[Byte] = {
    val visitor = new ClassWriter(0)

    // Super class of the class
    val superClass = asm.Type.getInternalName(Constants.objectClass)
    // Interfaces to be extended
    val extendedInterfaced = Array(asm.Type.getInternalName(Constants.tagInterface))
    visitor.visit(JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, decorate(qualName), null, superClass, extendedInterfaced)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generates the class for each enum case.
    * Each class implements the interface of enum.
    * @param className Qualified name of the generated class
    * @param superType Qualfied name of the interface of the Enum
    * @param fType Type of the `value` field, None means that the type is not a primitive
    * @param isSingleton is `true` if this is a singleton, `false` otherwise.
    * @return byte code representation of the class
    */
  def compileEnumClass(className: EnumClassName,
                       superType: EnumInterfName,
                       fType: WrappedType,
                       isSingleton : Boolean): Array[Byte] = {
    /*
     *  Initialize the class writer. We override `getCommonSuperClass` method because `asm` implementation of this
     * function requires types to loaded so that they can be compared to each other.
     */
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES){
      override def getCommonSuperClass(tpe1: String, tpe2: String) : String = {
        asm.Type.getInternalName(Constants.objectClass)
      }
    }

    // Initialize the visitor to create a class.
    // Super class of the class
    val superClass = asm.Type.getInternalName(Constants.objectClass)
    // Interfaces to be implemented
    val implementedInterfaces = Array(decorate(superType))
    visitor.visit(JavaVersion, ACC_PUBLIC + ACC_FINAL, decorate(className), null, superClass, implementedInterfaces)

    // Generate value field
    compileField(visitor, "value", getWrappedTypeDescriptor(fType), isStatic = false)

    // Generate static `INSTANCE` field if it is a singleton
    if(isSingleton) {
      compileField(visitor, "unitInstance", s"L${decorate(className)};", isStatic = true)
    }

    // Generate the constructor of the class
    compileEnumConstructor(visitor, className, fType, isSingleton = isSingleton)

    // Initialize the static field if it is a singleton
    if(isSingleton){
      initializeStaticField(visitor, className)
    }

    // Generate the `getValue` method
    compileGetFieldMethod(visitor, className, getWrappedTypeDescriptor(fType), "value", "getValue", getReturnInsn(fType))

    // Generate `getBoxedValue` method
    compileGetBoxedValueMethod(visitor, className, fType)

    // Generate `getTag` method
    compileGetTagMethod(visitor, className)

    // Generate `hashCode` method
    compileHashCodeMethod(visitor, className, fType)

    // Generate `toString` method
    compileToStringMethod(visitor, className, fType)

    // Generate `equals` method
    compileEqualsMethod(visitor, className, fType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Initializing `getInstance` static field if the `value` field can be `UnitClass`
    * @param visitor class visitor
    * @param className Qualified name of the class
    */
  private def initializeStaticField(visitor: ClassWriter, className: EnumClassName) = {
    val unitClazz = Constants.unitClass
    val getInstanceMethod = unitClazz.getMethod("getInstance")

    val method = visitor.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    method.visitCode()

    // Instantiating the object
    method.visitTypeInsn(NEW, decorate(className))
    method.visitInsn(DUP)

    // Getting instance of `UnitClass`
    method.visitMethodInsn(INVOKESTATIC, asm.Type.getInternalName(unitClazz), getInstanceMethod.getName,
      asm.Type.getMethodDescriptor(getInstanceMethod), false)

    // Calling constructor on the object
    method.visitMethodInsn(INVOKESPECIAL, decorate(className), "<init>", s"(${asm.Type.getDescriptor(Constants.objectClass)})V", false)

    // Initializing the static field
    method.visitFieldInsn(PUTSTATIC, decorate(className), "unitInstance", s"L${decorate(className)};")

    // Return
    method.visitInsn(RETURN)
    method.visitMaxs(2, 0)
    method.visitEnd()
  }

  /**
    * Creates the single argument constructor of the enum case class which is named `className`.
    * The only argument required to instantiate the class is the `value`.
    * The type of the field of the case is give by `descriptor`.
    * If the `fType` is only `Unit`, then we can make the constructor private since the `unitInstance` field
    * can be used to obtain an instance of the case.
    *
    * @param visitor class visitor
    * @param className name of the class
    * @param fType type of the `value` field
    * @param isSingleton if the class is a singleton this flag is set
    */
  private def compileEnumConstructor(visitor: ClassWriter, className: EnumClassName, fType: WrappedType, isSingleton: Boolean) = {
    val descriptor = getWrappedTypeDescriptor(fType)

    // If this is a singleton then we should make the constructor private
    val specifier =
      if(isSingleton) {
        ACC_PRIVATE
      } else {
        ACC_PUBLIC
      }

    val constructor = visitor.visitMethod(specifier, "<init>", s"($descriptor)V", null, null)
    val clazz = Constants.objectClass
    val ctor = clazz.getConstructor()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, asm.Type.getInternalName(clazz), "<init>",
      asm.Type.getConstructorDescriptor(ctor), false)

    // Load instruction for type of `value`
    val iLoad = getLoadInstruction(fType)

    // Put the object given to the constructor on the `value` field
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitVarInsn(iLoad, 1)
    constructor.visitFieldInsn(PUTFIELD, decorate(className), "value", descriptor)

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  /**
    * Returns the load instruction corresponding to the `fType`, if it is `None` then the type can be represented by an object
    * @param fType type
    * @return A load instruction
    */
  private def getReturnInsn(fType: WrappedType) : Int = fType match {
    case WrappedPrimitive(Type.Bool) | WrappedPrimitive(Type.Char) | WrappedPrimitive(Type.Int8) | WrappedPrimitive(Type.Int16) |
         WrappedPrimitive(Type.Int32) => IRETURN
    case WrappedPrimitive(Type.Int64) => LRETURN
    case WrappedPrimitive(Type.Float32) => FRETURN
    case WrappedPrimitive(Type.Float64) => DRETURN
    case _ => ARETURN
  }

  /**
    * Generates the `getTag()` method of the class which is the implementation of `getTag` method on `tagInterface`.
    * This methods returns an string containing the tag name.
    * For example, `Val[Char]` has following `getTag()`method:
    *
    * public final String getTag() {
    *   return "Var";
    * }
    *
    * @param visitor class visitor
    * @param className Qualified name of the class
    */
  private def compileGetTagMethod(visitor: ClassWriter, className: EnumClassName) = {
    val descriptor = asm.Type.getDescriptor(Constants.stringClass)
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getTag", s"()$descriptor", null, null)
    method.visitLdcInsn(className.tag)
    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Generate the `methodName` method for fetching the `fieldName` field of the class.
    * `name` is name of the class and `descriptor` is type of the `fieldName` field.
    * This method generates `getValue()` method of the class.
    * For example, `Val[Char]` has following `getValue()`method:
    *
    * public final char getValue() {
    *   return this.value;
    * }
    *
    * @param visitor class visitor
    * @param qualName Qualified name of the class
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
    * @param fType type of the `value` field of the class
    */
  private def compileGetBoxedValueMethod(visitor: ClassWriter, qualName: QualName, fType: WrappedType) = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getBoxedValue", s"()Ljava/lang/Object;", null, null)

    method.visitCode()

    boxField(method, fType, qualName, "value")

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
    * @param fType type of the underlying `value`
    */
  private def compileHashCodeMethod(visitor: ClassWriter, qualName: QualName, fType: WrappedType) = {
    val clazz = Constants.objectClass
    val method = visitor.visitMethod(ACC_PUBLIC, "hashCode", "()I", null, null)

    method.visitCode()
    method.visitLdcInsn(7)
    method.visitLdcInsn(qualName.hashCode())
    method.visitInsn(IMUL)
    method.visitLdcInsn(11)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, decorate(qualName), "value", getWrappedTypeDescriptor(fType))

    /*
     * If the field is of type Object, we get the hashCode of the object
     * Otherwise, we convert the type to integer and use in hash code computation
     */
    getHashCodeOrConvertToInt(method, fType)

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
    * Special cases are listed as follows:
    *
    * 1. When the enum case has a unit field, we just print the case name and ignore printing the unit as it's field
    * For example, for `case Red(Unit)` we generate the following method:
    *
    * public String toString() {
    *   return "Red";
    * }
    *
    * 2. When the enum case is `Cons` of `List`, we hack to represent it with `::` which is also hacked into the system.
    * We first get the `value` field of the case, then since we know it's a tuple we cast it and use `getBoxedValue` to get
    * an array of length two. Then we invoke `toString` on the first element of the array, append " :: " to the string and
    * put it on top of the stack then we invoke `toString` on the second element of the array and then concatenate this to
    * the string on top of the stack.
    *
    * The code we generate is equivalent to:
    *
    * public String toString() {
    *   Object[] var10000 = ((TupleInterface)this.value).getBoxedValue();
    *   return var10000[0].toString().concat(" :: ".concat(var10000[1].toString()));
    * }
    *
    * @param visitor class visitor
    * @param qualName Qualified name of the class
    * @param fType type of the `value` field
    */
  private def compileToStringMethod(visitor: ClassWriter, qualName: EnumClassName, fType: WrappedType) = {
    val stringInternalName = asm.Type.getInternalName(Constants.stringClass)
    val stringConcatMethod = Constants.stringClass.getMethod("concat", Constants.stringClass)

    val method = visitor.visitMethod(ACC_PUBLIC, "toString", s"()${asm.Type.getDescriptor(Constants.stringClass)}", null, null)

    method.visitCode()

    if(fType == WrappedNonPrimitives(Set(Type.Unit))) { // Special case for when the field is `Unit`
      method.visitLdcInsn(qualName.tag)
    } else if(qualName.tag == "Cons" && qualName.sym.namespace == Nil && qualName.sym.name == "List") { // Special case for cons
      val tupleClazz = Constants.tupleClass
      val getBoxedValueMethod = tupleClazz.getMethod("getBoxedValue")

      // Load value field
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(qualName), "value", getWrappedTypeDescriptor(fType))

      // Cast the field to tuple interface
      method.visitTypeInsn(CHECKCAST, asm.Type.getInternalName(Constants.tupleClass))

      // Call `getBoxedValue()` on the object
      method.visitMethodInsn(INVOKEVIRTUAL, asm.Type.getInternalName(tupleClazz), getBoxedValueMethod.getName,
        asm.Type.getMethodDescriptor(getBoxedValueMethod), false)

      // Duplicate the reference to array
      method.visitInsn(DUP)

      // Get the first element of the array
      method.visitLdcInsn(0)
      method.visitInsn(AALOAD)

      // Convert the first element of the array to string
      javaValueToString(method, WrappedNonPrimitives(Set()))

      // Bring reference to the array to the top of the stack
      method.visitInsn(SWAP)

      // Put " :: " on top of the stack
      method.visitLdcInsn(" :: ")

      // Again, bring reference to the array to the top of the stack
      method.visitInsn(SWAP)

      // Convert the second element of the array to string
      method.visitLdcInsn(1)
      method.visitInsn(AALOAD)

      // Convert the first element of the array to string
      javaValueToString(method, WrappedNonPrimitives(Set()))

      // Concatenate all of the string on top of the stack together
      for (_ <- 0 until 2) {
        method.visitMethodInsn(INVOKEVIRTUAL, stringInternalName, stringConcatMethod.getName,
          asm.Type.getMethodDescriptor(stringConcatMethod), false)
      }
    } else {
      // Normal version of `toString`
      method.visitLdcInsn(qualName.tag.concat("("))
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, decorate(qualName), "value", getWrappedTypeDescriptor(fType))

      /*
       * Converting `value` to String.
       * If it's an object, we will call `toString` on the object.
       * Otherwise, we use `valueOf` static method on String with the appropriate type.
       */
      javaValueToString(method, fType)

      method.visitLdcInsn(")")

      // We concatenate twice since there is 3 strings on the stack that we want to concat them together
      for (_ <- 0 until 2) {
        method.visitMethodInsn(INVOKEVIRTUAL, stringInternalName, stringConcatMethod.getName,
          asm.Type.getMethodDescriptor(stringConcatMethod), false)
      }
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
    * @param fType Type of the `value` field
    */
  private def compileEqualsMethod(visitor: ClassWriter, qualName: QualName, fType: WrappedType) = {
    val clazz = Constants.objectClass
    val neq = new Label() //label for when the object is not instanceof tag case

    // Descriptor of the `value` field
    val descriptor = getWrappedTypeDescriptor(fType)

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
    branchIfNotEqual(method, fType, neq)

    method.visitInsn(ICONST_1)
    method.visitInsn(IRETURN)
    method.visitLabel(neq) // if the code reaches here, it means that `instanceof` has returned false
    method.visitInsn(ICONST_0)
    method.visitInsn(IRETURN)
    method.visitMaxs(10, 10)

    method.visitEnd()
  }
}
